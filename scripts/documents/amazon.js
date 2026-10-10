// Downloads the invoices and credit notes of Amazon orders
//
// Usage:
//   node amazon.js <dir> <order-id>…
//   node amazon.js <dir> year <YYYY>
//
// Saves the documents of the given orders (or of all orders placed
// in the year) which are not yet in <dir>
// as `<order date>_amazon_<order id>_<document>.pdf`
// (e.g. "2025-07-09_amazon_306-8902853-4928364_invoice_2.pdf")
// and prints their paths to stdout.
// Orders without an invoice (e.g. Prime memberships)
// get their "Printable Order Summary" rendered to PDF instead.
//
// Environment: AMAZON_DOMAIN (default "amazon.de", e.g. "amazon.co.za"),
// AMAZON_PAYMENT_METHODS (space separated last digits of cards/accounts,
// e.g. "6510 8262": skip orders paid with anything else,
// e.g. by other people sharing the Amazon account).
// The login has to be completed manually in the browser.
// It's remembered in a browser profile per domain.

import path from "node:path"
import {pathToFileURL} from "node:url"

import fse from "fs-extra"
import {chromium} from "playwright"

import {dumpDebugFiles, launchBrowser} from "../browser.js"

const log = console.warn
const domain = process.env.AMAZON_DOMAIN || "amazon.de"
// "/-/en" switches the pages to English, which makes dates parseable
const baseUrl = `https://www.${domain}`
const ordersUrl = `${baseUrl}/-/en/your-orders/orders`


export async function login (page) {
  log(`Open ${ordersUrl}`)
  await page.goto(ordersUrl, {timeout: 30000})
  if (!page.url()
    .includes("/your-orders/")) {
    log("Please log in in the browser …")
  }
  await page.waitForURL(/\/your-orders\//, {timeout: 600000})
  await page.locator(".order-card")
    .first()
    .waitFor({timeout: 30000})
}


// "15 July 2025" → "2025-07-15"
export function toIsoDate (dateText) {
  const date = new Date(`${dateText.trim()} 12:00 UTC`)
  return Number.isNaN(date.getTime())
    ? null
    : date.toISOString()
      .slice(0, 10)
}


// "Invoice 2" → "invoice_2", "Credit note" → "credit_note"
export function toFileLabel (linkText) {
  return linkText.trim()
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "_")
    .replace(/^_|_$/g, "")
}


// Returns `[{orderId, date}]` of all orders placed in the year
export async function getOrdersOfYear (page, year) {
  const orders = []
  for (let startIndex = 0; ; startIndex += 10) {
    await page.goto(
      `${ordersUrl}?timeFilter=year-${year}&startIndex=${startIndex}`,
      {timeout: 30000},
    )
    const headers = await page.locator(".order-card .order-header")
      .allInnerTexts()
    for (const header of headers) {
      const orderId = header.match(/ORDER # ([A-Z0-9]{3}-\d{7}-\d{7})/)?.[1]
      const dateText = header.match(/ORDER PLACED\s+(.+)/)?.[1]
      if (orderId) {
        orders.push({orderId, date: dateText ? toIsoDate(dateText) : null})
      }
    }
    if (headers.length < 10) {
      break
    }
  }
  return orders
}


const paymentMethodDigits = (process.env.AMAZON_PAYMENT_METHODS || "")
  .split(/\s+/)
  .filter(Boolean)


// Returns `{date, paymentMethod, html}` of the "Printable Order Summary"
async function getOrderSummary (page, orderId) {
  const response = await page.request.get(
    `${baseUrl}/-/en/gp/css/summary/print.html?orderID=${orderId}`)
  const html = await response.text()
  const text = html
    .replace(/<script[\s\S]*?<\/script>|<style[\s\S]*?<\/style>/g, "")
    .replace(/<[^>]+>/g, " ")
    .replace(/\s+/g, " ")
  // E.g. "Order placed 8 July 2025", "Subscription charged on 27 October 2025"
  const dateText = text
    .match(/(?:order placed|charged on|digital order):? (\d{1,2} \w+ \d{4})/i)
    ?.[1]
  // E.g. "Payment method Amazon Gift Card Visa Debitkarte •••• 8262 Order …"
  const paymentMethod = text.match(/Payment method (.*?) Order Summary/)
    ?.[1] ?? ""
  return {date: dateText ? toIsoDate(dateText) : null, paymentMethod, html}
}


// Whether the order was (at least partly) paid
// with one of the AMAZON_PAYMENT_METHODS (all orders if none are set)
export function isOwnPaymentMethod (paymentMethod) {
  return paymentMethodDigits.length === 0 ||
    paymentMethodDigits.some(digits =>
      // E.g. "Visa Debitkarte •••• 8262", "Visa ending in 8262"
      new RegExp(`(?:•|ending in)\\s*${digits}\\b`)
        .test(paymentMethod))
}


// Returns `[{label, url}]` of the invoices and credit notes of the order
export async function getDocumentLinks (page, orderId) {
  const response = await page.request.get(
    `${baseUrl}/-/en/your-orders/invoice/popover?orderId=${orderId}`)
  const html = await response.text()
  const linkPattern =
    /<a[^>]+href="([^"]*\/documents\/download\/[^"]+)"[^>]*>([^<]+)</g
  return [...html.matchAll(linkPattern)]
    .map(([, href, text]) => ({
      label: toFileLabel(text),
      url: new URL(href.replaceAll("&amp;", "&"), baseUrl).href,
    }))
}


async function renderPdf (html, filePath) {
  const browser = await chromium.launch()
  try {
    const page = await browser.newPage()
    await page.setContent(
      html.replace(/<head[^>]*>/i, `$&<base href="${baseUrl}/">`),
      {waitUntil: "networkidle"},
    )
    await page.pdf({path: filePath, format: "A4", printBackground: true})
  }
  finally {
    await browser.close()
  }
}


export async function downloadOrderDocuments (
  page,
  {outputDir, orderId, date: knownDate = null},
) {
  const summary = await getOrderSummary(page, orderId)
  if (!isOwnPaymentMethod(summary.paymentMethod)) {
    log(`Skip ${orderId} (paid with ${summary.paymentMethod || "unknown"})`)
    return []
  }
  const date = knownDate ?? summary.date ?? "unknown-date"
  const prefix = `${date}_amazon_${orderId}`
  const links = await getDocumentLinks(page, orderId)
  const savedPaths = []

  if (links.length === 0) {
    const filePath = path.join(outputDir, `${prefix}_order_summary.pdf`)
    if (await fse.pathExists(filePath)) {
      log(`Skip ${orderId} order summary (already downloaded)`)
      return savedPaths
    }
    await renderPdf(summary.html, filePath)
    log(`Saved ${orderId} order summary (no invoice available)`)
    savedPaths.push(filePath)
    return savedPaths
  }

  for (const {label, url} of links) {
    const filePath = path.join(outputDir, `${prefix}_${label}.pdf`)
    if (await fse.pathExists(filePath)) {
      log(`Skip ${orderId} ${label} (already downloaded)`)
      continue
    }
    const response = await page.request.get(url)
    const body = await response.body()
    if (!body.subarray(0, 5)
      .toString()
      .startsWith("%PDF")) {
      throw new Error(`${orderId} ${label}: ${url} didn't return a PDF`)
    }
    await fse.writeFile(filePath, body)
    log(`Saved ${orderId} ${label}`)
    savedPaths.push(filePath)
  }
  return savedPaths
}


async function main () {
  const [outputDir, ...args] = process.argv.slice(2)
  if (!outputDir || args.length === 0 ||
    (args[0] === "year" && !/^\d{4}$/.test(args[1] ?? ""))
  ) {
    console.error(
      "Usage: node amazon.js <dir> (<order-id>… | year <YYYY>)")
    process.exitCode = 1
    return
  }

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: `amazon-${domain}`,
    acceptDownloads: false,
  })

  try {
    await login(page)
    await fse.ensureDir(outputDir)

    const orders = args[0] === "year"
      ? await getOrdersOfYear(page, args[1])
      : args.map(orderId => ({orderId}))
    log(`Get the documents of ${orders.length} orders`)

    let downloadCounter = 0
    for (const order of orders) {
      const savedPaths = await downloadOrderDocuments(page,
        {outputDir, ...order})
      savedPaths.forEach(filePath => console.info(filePath))
      downloadCounter += savedPaths.length
    }
    log(`Downloaded ${downloadCounter} new documents`)
  }
  catch (error) {
    await dumpDebugFiles(page, "amazon-debug")
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
