// Downloads the invoices ("Rechnung") of Deutsche Bahn orders
// from the "Meine Reisen" section of https://www.bahn.de
//
// Usage:
//   node bahn.js <dir> <order-number>…
//   node bahn.js <dir> from <YYYY-MM-DD>
//
// Saves the invoices of the given orders (Auftragsnummern)
// or of all orders placed since the date which are not yet in <dir>
// as `<order date>_bahn_<order number>.pdf`
// (e.g. "2026-03-27_bahn_343600045046.pdf")
// and prints their paths to stdout.
// The order list of bahn.de only reaches back about 13 months,
// older orders can still be downloaded by their number.
// Subscriptions (e.g. Deutschland-Ticket) are managed in the separate
// Aboportal (https://abo.bahn.de) and are not supported.
//
// The login has to be completed manually in the browser.
// It's remembered in the browser profile.

import path from "node:path"
import {pathToFileURL} from "node:url"

import fse from "fs-extra"

import {dumpDebugFiles, launchBrowser} from "../browser.js"

const log = console.warn
const baseUrl = "https://www.bahn.de"
const tripsUrl = `${baseUrl}/buchung/reiseuebersicht`
// The API answers with "429 Too Many Requests" after a few fast requests
const requestDelay = 2000


// Opens the trip overview and waits until the user is logged in.
// Returns the id of the customer profile (needed to list the orders),
// which the page requests the upcoming trips with.
export async function login (page) {
  const ordersRequest = page.waitForRequest(
    request => /\/web\/api\/buchung\/auftrag\/v2\?.*kundenprofilId=/
      .test(request.url()),
    {timeout: 600000},
  )
  log(`Open ${tripsUrl}`)
  await page.goto(tripsUrl, {timeout: 30000})
  if (!page.url()
    .startsWith(tripsUrl)) {
    log("Please log in in the browser …")
  }
  const request = await ordersRequest
  return new URL(request.url()).searchParams.get("kundenprofilId")
}


// The access token of the logged in user, which the website keeps
// in the session storage. It's only valid for 5 minutes,
// so the page is reloaded to let the website renew it when it expires.
async function getAccessToken (page) {
  function readToken () {
    return page.evaluate(() =>
      JSON.parse(sessionStorage.getItem("token") || "null"))
  }

  let token = await readToken()
  if (token && token.expiresAtClientTimeS * 1000 > Date.now() + 30000) {
    return token.accessToken
  }

  log("Renew the access token")
  await page.goto(tripsUrl, {timeout: 30000})
  for (let waited = 0; waited < 30000; waited += 500) {
    token = await readToken()
    if (token && token.expiresAtClientTimeS * 1000 > Date.now() + 30000) {
      return token.accessToken
    }
    await page.waitForTimeout(500)
  }
  throw new Error("Could not get an access token (logged out?)")
}


// Requests a JSON API of the website from within the page,
// as requests from outside the browser are blocked by the bot protection
export async function getJson (page, apiPath) {
  for (let attempt = 1; ; attempt++) {
    await page.waitForTimeout(requestDelay)
    const accessToken = await getAccessToken(page)
    const {status, body} = await page.evaluate(
      async ({url, token}) => {
        const response = await fetch(url, {
          headers: {
            accept: "application/json",
            authorization: `Bearer ${token}`,
          },
        })
        return {status: response.status, body: await response.text()}
      },
      {url: `/web/api/${apiPath}`, token: accessToken},
    )
    if (status === 429 && attempt < 6) {
      log(`Rate limited, wait ${attempt * 15} s`)
      await page.waitForTimeout(attempt * 15000)
      continue
    }
    if (status !== 200) {
      throw new Error(`${apiPath}: HTTP ${status} ${body.slice(0, 200)}`)
    }
    return JSON.parse(body)
  }
}


// "2026-03-27T07:19:36Z" → "2026-03-27" (in German time)
export function toIsoDate (timestamp) {
  return new Date(timestamp)
    .toLocaleDateString("sv-SE", {timeZone: "Europe/Berlin"})
}


// Returns `[{orderId, date}]` of all orders (past and upcoming trips)
// placed since the date (YYYY-MM-DD)
export async function getOrdersSince (page, {kundenprofilId, fromDate}) {
  const now = new Date()
    .toISOString()
  const queries = [
    `letzterGeltungszeitpunktVor=${now}&auftragSortOrder=DESCENDING`,
    `letzterGeltungszeitpunktNach=${now}&auftragSortOrder=ASCENDING`,
  ]
  const orders = []
  for (const query of queries) {
    for (let startIndex = 0; ; startIndex += 10) {
      const {auftraege, hasMoreAuftraege} = await getJson(page,
        `buchung/auftrag/v2?startIndex=${startIndex}` +
        `&auftraegeReturnSize=10&${query}&kundenprofilId=${kundenprofilId}`)
      for (const order of auftraege) {
        const date = toIsoDate(order.anlagedatum)
        if (date >= fromDate) {
          orders.push({orderId: order.auftragsnummer, date})
        }
      }
      if (!hasMoreAuftraege || auftraege.length === 0) {
        break
      }
    }
  }
  return orders.sort((orderA, orderB) => orderA.date.localeCompare(orderB.date))
}


// Returns `{date, amount}` of the order (amount e.g. "6.90 EUR")
export async function getOrder (page, orderId) {
  const order = await getJson(page, `buchung/auftrag/${orderId}`)
  return {
    date: toIsoDate(order.anlagedatum),
    amount: order.gesamtpreis
      ? `${order.gesamtpreis.betrag.toFixed(2)} ${order.gesamtpreis.waehrung}`
      : null,
  }
}


// Returns the path of the saved invoice (null if it already existed)
export async function downloadInvoice (
  page,
  {outputDir, orderId, date: knownDate = null},
) {
  let date = knownDate
  let amount = null
  if (!date) {
    ({date, amount} = await getOrder(page, orderId))
  }
  const filePath = path.join(outputDir, `${date}_bahn_${orderId}.pdf`)
  if (await fse.pathExists(filePath)) {
    log(`Skip ${orderId} (already downloaded)`)
    return null
  }

  // The invoice is created on the first request
  // (with the address of the customer account)
  const {data} = await getJson(page, `buchung/rechnungen/${orderId}`)
  const body = Buffer.from(data || "", "base64")
  if (!body.subarray(0, 5)
    .toString()
    .startsWith("%PDF")) {
    throw new Error(`${orderId}: The invoice is not a PDF`)
  }
  await fse.writeFile(filePath, body)
  log(`Saved ${orderId} from ${date}${amount ? ` (${amount})` : ""}`)
  return filePath
}


async function main () {
  const [outputDir, ...args] = process.argv.slice(2)
  if (!outputDir || args.length === 0 ||
    (args[0] === "from" && !/^\d{4}-\d{2}-\d{2}$/.test(args[1] ?? "")) ||
    (args[0] !== "from" && !args.every(arg => /^\d+$/.test(arg)))
  ) {
    console.error(
      "Usage: node bahn.js <dir> (<order-number>… | from <YYYY-MM-DD>)")
    process.exitCode = 1
    return
  }

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: "bahn",
    acceptDownloads: false,
  })

  try {
    const kundenprofilId = await login(page)
    await fse.ensureDir(outputDir)

    const orders = args[0] === "from"
      ? await getOrdersSince(page, {kundenprofilId, fromDate: args[1]})
      : args.map(orderId => ({orderId}))
    log(`Get the invoices of ${orders.length} orders`)

    let downloadCounter = 0
    for (const order of orders) {
      const filePath = await downloadInvoice(page, {outputDir, ...order})
      if (filePath) {
        console.info(filePath)
        downloadCounter += 1
      }
    }
    log(`Downloaded ${downloadCounter} new invoices`)
  }
  catch (error) {
    await dumpDebugFiles(page, "bahn-debug")
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
