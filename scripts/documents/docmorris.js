// Downloads the invoices of DocMorris orders
// ("Rechnung & wichtige Dokumente" in "Konto" → "Bestellungen")
//
// Usage:
//   node docmorris.js <dir> [from <YYYY-MM-DD>] [<order number>…]
//
// Saves the invoices of all orders (or of the orders placed since the date
// and/or of the given order numbers, e.g. "1BZWBQ")
// which are not yet in <dir>
// as `<order date>_docmorris_<order number>.pdf`
// (e.g. "2025-07-27_docmorris_1bzwbq.pdf",
// orders with several invoices, e.g. from marketplace sellers,
// get a "_<index>" suffix)
// and prints their paths to stdout.
//
// DocMorris only allows logins from Germany.
// From other countries route the browser through a proxy in Germany,
// e.g. an SSH tunnel to a host there:
//   ssh -N -D 1081 <host>
//   TRANSITY_PROXY=socks5://127.0.0.1:1081 node docmorris.js …
// All API requests are therefore made from within the page
// (requests of Playwright's `page.request` don't use the proxy).
//
// The login has to be completed manually in the browser.
// It's remembered in the browser profile.

import path from "node:path"
import {pathToFileURL} from "node:url"

import fse from "fs-extra"

import {dumpDebugFiles, launchBrowser} from "../browser.js"

const log = console.warn
const baseUrl = "https://www.docmorris.de"
const ordersUrl = `${baseUrl}/konto/bestellungen`
// Online orders and "Post- und Telefonbestellungen"
const orderOrigins = [["web"], ["postal", "phone"]]


// Fetches a URL of the shop's API in the page (i.e. with its session
// and through the browser's proxy) and returns the parsed JSON
export async function fetchJson (page, url) {
  const {status, text} = await page.evaluate(async apiUrl => {
    const response = await fetch(apiUrl, {credentials: "include"})
    return {status: response.status, text: await response.text()}
  }, new URL(url, baseUrl).href)
  if (status !== 200) {
    throw new Error(`${url} returned status ${status}`)
  }
  return JSON.parse(text)
}


// Like `fetchJson`, but returns the body as a Buffer
export async function fetchFile (page, url) {
  const {status, base64} = await page.evaluate(async fileUrl => {
    const response = await fetch(fileUrl, {credentials: "include"})
    const bytes = new Uint8Array(await response.arrayBuffer())
    let binary = ""
    for (let index = 0; index < bytes.length; index += 0x8000) {
      binary += String.fromCharCode(...bytes.subarray(index, index + 0x8000))
    }
    return {status: response.status, base64: btoa(binary)}
  }, new URL(url, baseUrl).href)
  if (status !== 200) {
    throw new Error(`${url} returned status ${status}`)
  }
  return Buffer.from(base64, "base64")
}


async function isLoggedIn (page) {
  try {
    const session = await fetchJson(page, "/api/auth/session")
    return Boolean(session?.user?.userId)
  }
  catch {
    // E.g. during navigations of the login
    return false
  }
}


export async function login (page) {
  log(`Open ${ordersUrl}`)
  await page.goto(ordersUrl, {timeout: 30000})
  if (!await isLoggedIn(page)) {
    log("Please log in in the browser …")
  }
  for (let waited = 0; !await isLoggedIn(page); waited += 2000) {
    if (waited >= 600000) {
      throw new Error("Login timed out")
    }
    await page.waitForTimeout(2000)
  }
  if (!page.url()
    .startsWith(ordersUrl)) {
    await page.goto(ordersUrl, {timeout: 30000})
  }
}


// "2025-07-27T18:24:00+0000" → "2025-07-27" (in German time)
// ("2022-10-03" for orders of the old shop)
export function toIsoDate (createdAt) {
  if (/^\d{4}-\d{2}-\d{2}$/.test(createdAt)) {
    return createdAt
  }
  const date = new Date(createdAt.replace(/([+-]\d{2})(\d{2})$/, "$1:$2"))
  return Number.isNaN(date.getTime())
    ? null
    : date.toLocaleDateString("sv-SE", {timeZone: "Europe/Berlin"})
}


// Returns `[{id, orderCode, date, hasDocuments}]` of all orders
export async function getOrders (page) {
  const orders = []

  for (const origins of orderOrigins) {
    for (let pageNumber = 1, totalPages = 1; pageNumber <= totalPages;
      pageNumber++
    ) {
      const params = new URLSearchParams({
        page: pageNumber,
        filterByOrigin: JSON.stringify(origins),
        maxNewWebOrders: "null",
        maxOldWebOrders: "null",
      })
      const {data, metadata} = await fetchJson(page, `/api/orders?${params}`)
      totalPages = metadata?.totalPages ?? 1
      for (const order of data?.orders ?? []) {
        orders.push({
          id: order.id,
          orderCode: order.orderCode,
          date: toIsoDate(order.createdAt),
          hasDocuments: order.hasDocuments,
        })
      }
    }
  }
  return orders
}


// Returns the ids of the invoices of an order
export async function getInvoiceIds (page, {id, orderCode}) {
  const {baskets} = await fetchJson(page,
    `/api/orders/${id}/documents?orderCode=${orderCode}`)
  return (baskets ?? [])
    .flatMap(basket => basket.documents ?? [])
    .filter(document => document.type === "invoice")
    .map(document => document.id)
}


export async function downloadInvoices (
  page,
  {outputDir, fromDate = null, orderCodes = []},
) {
  const wantedCodes = orderCodes.map(code => code.replace(/^#/, "")
    .toLowerCase())
  const allOrders = await getOrders(page)
  const orders = allOrders
    .filter(({orderCode, date}) =>
      (wantedCodes.length === 0 ||
        wantedCodes.includes(orderCode.toLowerCase())) &&
      (!fromDate || !date || date >= fromDate))
  const missingCodes = wantedCodes.filter(code =>
    !allOrders.some(({orderCode}) => orderCode.toLowerCase() === code))
  if (missingCodes.length > 0) {
    log(`Orders not found: ${missingCodes.join(", ")}`)
  }
  log(`Found ${orders.length} orders`)
  await fse.ensureDir(outputDir)
  const savedPaths = []

  for (const order of orders) {
    const {id, orderCode, date, hasDocuments} = order
    if (!hasDocuments) {
      log(`Skip ${date} ${orderCode} (no documents)`)
      continue
    }
    const invoiceIds = await getInvoiceIds(page, order)
    if (invoiceIds.length === 0) {
      log(`Skip ${date} ${orderCode} (no invoice)`)
      continue
    }

    for (const [index, invoiceId] of invoiceIds.entries()) {
      const suffix = invoiceIds.length > 1 ? `_${index + 1}` : ""
      const filePath = path.join(outputDir,
        `${date ?? "unknown-date"}_docmorris_${orderCode.toLowerCase()}` +
        `${suffix}.pdf`)
      if (await fse.pathExists(filePath)) {
        log(`Skip ${date} ${orderCode}${suffix} (already downloaded)`)
        continue
      }
      const body = await fetchFile(page,
        `/api/orders/${id}/documents/${invoiceId}?orderCode=${orderCode}`)
      if (!body.subarray(0, 5)
        .toString()
        .startsWith("%PDF")) {
        throw new Error(`${orderCode}: Invoice ${invoiceId} isn't a PDF`)
      }
      await fse.writeFile(filePath, body)
      log(`Saved ${date} ${orderCode}${suffix}`)
      savedPaths.push(filePath)
    }
  }
  return savedPaths
}


async function main () {
  const [outputDir, ...args] = process.argv.slice(2)
  const fromIndex = args.indexOf("from")
  const fromDate = fromIndex >= 0 ? args[fromIndex + 1] : null
  const orderCodes = fromIndex >= 0
    ? args.filter((arg, index) =>
      index !== fromIndex && index !== fromIndex + 1)
    : args
  if (!outputDir ||
    (fromIndex >= 0 && !/^\d{4}-\d{2}-\d{2}$/.test(fromDate ?? ""))
  ) {
    console.error(
      "Usage: node docmorris.js <dir> [from <YYYY-MM-DD>] [<order number>…]")
    process.exitCode = 1
    return
  }

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: "docmorris",
    acceptDownloads: false,
  })

  try {
    await login(page)
    const savedPaths = await downloadInvoices(page,
      {outputDir, fromDate, orderCodes})
    savedPaths.forEach(filePath => console.info(filePath))
    log(`Downloaded ${savedPaths.length} new invoices`)
  }
  catch (error) {
    await dumpDebugFiles(page, "docmorris-debug")
    console.error(error)
    process.exitCode = 1
  }
  finally {
    await browser.close()
  }
}


// Allows importing the functions, e.g. to test them in a running browser
if (
  process.argv[1] &&
  import.meta.url === pathToFileURL(process.argv[1]).href
) {
  main()
}
