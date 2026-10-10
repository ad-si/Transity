// Downloads the invoices of the Golem.de subscriptions (e.g. golem pur)
// from the payment overview ("Meine Zahlungen")
// at https://service.golem.de/payments/payments/my
//
// Usage:
//   node golem.js <dir> [from <YYYY-MM-DD>]
//
// Saves the invoices of all payments (or of the payments since the date)
// which are not yet in <dir>
// as `<payment date>_golem_<transaction id>.pdf`
// (e.g. "2026-05-25_golem_5950651401.pdf")
// and prints their paths to stdout.
//
// The login has to be completed manually in the browser.
// It's remembered in the browser profile.

import path from "node:path"
import {pathToFileURL} from "node:url"

import fse from "fs-extra"

import {dumpDebugFiles, launchBrowser} from "../browser.js"

const log = console.warn
const baseUrl = "https://service.golem.de"
const paymentsUrl = `${baseUrl}/payments/payments/my`

const monthNumbers = {
  Januar: "01",
  Februar: "02",
  März: "03",
  April: "04",
  Mai: "05",
  Juni: "06",
  Juli: "07",
  August: "08",
  September: "09",
  Oktober: "10",
  November: "11",
  Dezember: "12",
}


export async function login (page) {
  log(`Open ${paymentsUrl}`)
  await page.goto(paymentsUrl, {timeout: 30000})
  if (!page.url()
    .startsWith(paymentsUrl)) {
    log("Please log in in the browser …")
  }
  // Without a session the page redirects to the login at account.golem.de
  // and back to the payments after the login
  await page.waitForURL(url => url.href.startsWith(paymentsUrl),
    {timeout: 600000})
  await page.locator("#payment-list")
    .waitFor({timeout: 30000})
}


// "25. Mai 2026 um 23:45:06" → "2026-05-25"
export function toIsoDate (dateText) {
  const match = dateText.match(/(\d{1,2})\.\s*(\S+)\s+(\d{4})/)
  const month = match && monthNumbers[match[2]]
  return month
    ? `${match[3]}-${month}-${match[1].padStart(2, "0")}`
    : null
}


// Returns `[{date, transactionId, amount, url}]` of all payments
// with an invoice
export async function getPayments (page) {
  const rows = page.locator("#payment-list > .row")
    .filter({has: page.locator("a[href*='/download-invoice/']")})
  const payments = []

  for (let index = 0; index < await rows.count(); index++) {
    const row = rows.nth(index)
    const columns = row.locator(":scope > div")
    const dateText = await columns.nth(0)
      .innerText()
    const href = await row.locator("a[href*='/download-invoice/']")
      .getAttribute("href")
    payments.push({
      date: toIsoDate(dateText),
      transactionId: (await columns.nth(1)
        .innerText()).trim(),
      amount: (await columns.nth(2)
        .innerText()).trim(),
      url: new URL(href, baseUrl).href,
    })
  }
  return payments
}


export async function downloadInvoices (
  page,
  {outputDir, fromDate = null},
) {
  const payments = (await getPayments(page))
    .filter(({date}) => !fromDate || !date || date >= fromDate)
  log(`Found ${payments.length} payments with invoices`)
  await fse.ensureDir(outputDir)
  const savedPaths = []

  for (const {date, transactionId, amount, url} of payments) {
    const filePath = path.join(outputDir,
      `${date ?? "unknown-date"}_golem_${transactionId}.pdf`)
    if (await fse.pathExists(filePath)) {
      log(`Skip ${date} ${transactionId} (already downloaded)`)
      continue
    }
    const response = await page.request.get(url)
    const body = await response.body()
    if (!body.subarray(0, 5)
      .toString()
      .startsWith("%PDF")) {
      throw new Error(`${transactionId}: ${url} didn't return a PDF`)
    }
    await fse.writeFile(filePath, body)
    log(`Saved ${date} ${transactionId} (${amount})`)
    savedPaths.push(filePath)
  }
  return savedPaths
}


async function main () {
  const [outputDir, ...args] = process.argv.slice(2)
  if (!outputDir ||
    (args.length > 0 &&
      (args[0] !== "from" || !/^\d{4}-\d{2}-\d{2}$/.test(args[1] ?? "")))
  ) {
    console.error("Usage: node golem.js <dir> [from <YYYY-MM-DD>]")
    process.exitCode = 1
    return
  }

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: "golem",
    acceptDownloads: false,
  })

  try {
    await login(page)
    const savedPaths = await downloadInvoices(page, {
      outputDir,
      fromDate: args[1] ?? null,
    })
    savedPaths.forEach(filePath => console.info(filePath))
    log(`Downloaded ${savedPaths.length} new invoices`)
  }
  catch (error) {
    await dumpDebugFiles(page, "golem-debug")
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
