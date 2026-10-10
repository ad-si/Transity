// Downloads the invoices of the Audimee (AI vocals) subscription
// from its Stripe customer portal ("Payment history & billing info")
//
// Usage:
//   node audimee.js <dir> [from <YYYY-MM-DD>]
//
// Saves all invoices (or those issued since the date)
// which are not yet in <dir> as `<invoice date>_audimee.pdf`
// (`<invoice date>_audimee_<invoice number>.pdf` if there are several
// invoices on one day) and prints their paths to stdout.
//
// The login has to be completed manually in the browser.
// It's remembered in the browser profile.

import path from "node:path"
import {pathToFileURL} from "node:url"

import fse from "fs-extra"

import {dumpDebugFiles, launchBrowser} from "../browser.js"

const log = console.warn
const accountUrl = "https://audimee.com/account"


export async function login (page) {
  log(`Open ${accountUrl}`)
  await page.goto(accountUrl, {timeout: 30000})

  // The sidebar links to the profile with the email address of the user
  const profileLink = page.locator("a[href$='/account']", {hasText: "@"})
  try {
    await profileLink.first()
      .waitFor({timeout: 15000})
  }
  catch {
    log("Please log in manually in the browser …")
    await profileLink.first()
      .waitFor({timeout: 600000})
    // After the login the page may be another one than the account page
    await page.goto(accountUrl, {timeout: 30000})
  }
  log("Logged in")
}


// Opens the Stripe customer portal and returns the URL
// and the headers of its API requests for the invoices
// (the portal session's API key is only valid for this session)
export async function openBillingPortal (page) {
  log("Open the billing portal")
  const invoicesRequestPromise = page.waitForRequest(request =>
    /billing\.stripe\.com\/v1\/billing_portal\/sessions\/[^/]+\/invoices\?/
      .test(request.url()),
  {timeout: 60000})
  await page.getByRole("button", {name: /Payment history/})
    .click({timeout: 30000})
  const invoicesRequest = await invoicesRequestPromise
  const headers = invoicesRequest.headers()

  return {
    invoicesUrl: invoicesRequest.url()
      .split("?")[0],
    headers: {
      authorization: headers.authorization,
      "stripe-account": headers["stripe-account"],
      "stripe-version": headers["stripe-version"],
      "stripe-livemode": "true",
    },
  }
}


// Returns all invoices of the customer, newest first
export async function getInvoices (page, {invoicesUrl, headers}) {
  const invoices = []
  let lastId = null

  while (true) {
    const url = `${invoicesUrl}?limit=100` +
      (lastId ? `&starting_after=${lastId}` : "")
    const response = await page.request.get(url, {headers})
    if (!response.ok()) {
      throw new Error(
        `Loading the invoices failed: ${response.status()} ` +
        await response.text())
    }
    const result = await response.json()
    invoices.push(...result.data)
    if (!result.has_more || result.data.length === 0) {
      break
    }
    lastId = result.data.at(-1).id
  }

  return invoices
}


export function getInvoiceDate (invoice) {
  return new Date(invoice.created * 1000)
    .toISOString()
    .slice(0, 10)
}


export async function downloadInvoices (
  page,
  {outputDir, invoices, fromDate = null},
) {
  await fse.ensureDir(outputDir)
  const selectedInvoices = invoices
    .filter(invoice => invoice.invoice_pdf && invoice.status !== "draft")
    .filter(invoice => !fromDate || getInvoiceDate(invoice) >= fromDate)

  const invoicesPerDate = {}
  for (const invoice of selectedInvoices) {
    const date = getInvoiceDate(invoice)
    invoicesPerDate[date] = (invoicesPerDate[date] || 0) + 1
  }

  const savedPaths = []
  for (const invoice of selectedInvoices) {
    const date = getInvoiceDate(invoice)
    const amount = `${(invoice.total / 100).toFixed(2)} ` +
      invoice.currency.toUpperCase()
    const fileName = invoicesPerDate[date] > 1
      ? `${date}_audimee_${invoice.number}.pdf`
      : `${date}_audimee.pdf`
    const filePath = path.join(outputDir, fileName)
    if (await fse.pathExists(filePath)) {
      log(`Skip invoice ${invoice.number} of ${date} (already downloaded)`)
      continue
    }

    const response = await page.request.get(invoice.invoice_pdf)
    const body = await response.body()
    if (!response.ok() || body.subarray(0, 5)
      .toString() !== "%PDF-"
    ) {
      throw new Error(
        `Downloading invoice ${invoice.number} failed: ${response.status()}`)
    }
    await fse.writeFile(filePath, body)
    log(`Saved invoice ${invoice.number} of ${date} (${amount})`)
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
    console.error("Usage: node audimee.js <dir> [from <YYYY-MM-DD>]")
    process.exitCode = 1
    return
  }

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: "audimee",
    acceptDownloads: false,
  })

  try {
    await login(page)
    const portal = await openBillingPortal(page)
    const invoices = await getInvoices(page, portal)
    log(`Found ${invoices.length} invoices`)
    const savedPaths = await downloadInvoices(page, {
      outputDir,
      invoices,
      fromDate: args[1] ?? null,
    })
    savedPaths.forEach(filePath => console.info(filePath))
    log(`Downloaded ${savedPaths.length} new invoices`)
  }
  catch (error) {
    await dumpDebugFiles(page, "audimee-debug")
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
