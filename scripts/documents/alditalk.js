// Downloads the monthly invoices and the top-up confirmations
// of an ALDI TALK (E-Plus Service GmbH) prepaid contract
// from the customer portal at https://www.alditalk-kundenportal.de
//
// Usage:
//   node alditalk.js <dir> [from <YYYY-MM-DD>] [<invoice-id>…]
//   node alditalk.js <dir> top-ups [from <YYYY-MM-DD>]
//
// Saves the invoices ("Postfach" → "Rechnungen") issued since the date
// (or only the ones with the given ids, e.g. "2608B10012990933M001")
// which are not yet in <dir> as `<invoice date>_aldi_talk.pdf`
// and prints their paths to stdout.
// Top-ups ("Aufladungen", e.g. the automatic "Aufladung bei geringem
// Guthaben" by direct debit) aren't invoiced.
// With `top-ups` the "Kostenübersicht" of each day with a top-up
// is saved as `<top-up date>_aldi_talk_top_up.pdf` instead.
// The portal only provides the top-ups of the last 2 years.
//
// The login has to be completed manually in the browser.

import path from "node:path"
import {pathToFileURL} from "node:url"

import fse from "fs-extra"

import {dumpDebugFiles, launchBrowser} from "../browser.js"

const log = console.warn
const baseUrl = "https://www.alditalk-kundenportal.de"
const overviewUrl = `${baseUrl}/portal/auth/uebersicht/`
const masterDataApi =
  `${baseUrl}/scs/bff/scs-207-customer-master-data-bff/customer-master-data/v1`
const costOverviewApi =
  `${baseUrl}/scs/bff/scs-205-cost-overview-bff/cost-overview`


// Opens the overview page and waits for the user to log in.
// Returns the subscription (with its `msisdn`, `contractId`,
// and `billingAccountId`) from the portal's navigation data,
// which it only loads for logged in users.
export async function login (page) {
  const subscriptionsPromise = waitForSubscriptions(page)
  log(`Open ${overviewUrl}`)
  await page.goto(overviewUrl, {timeout: 30000})
  if (!page.url()
    .startsWith(overviewUrl)) {
    log("Please log in in the browser …")
  }
  const subscriptions = await subscriptionsPromise
  if (subscriptions.length > 1) {
    log(`Found ${subscriptions.length} subscriptions, ` +
      `use ${subscriptions[0].msisdn}`)
  }
  return subscriptions[0]
}


async function waitForSubscriptions (page) {
  while (true) {
    const response = await page.waitForResponse(
      candidate => candidate.url()
        .includes("/navigation-list") && candidate.ok(),
      {timeout: 600000},
    )
    const subscriptions = (await response.json()
      .catch(() => null))
      ?.userDetails
      ?.subscriptions
    if (subscriptions?.length > 0) {
      return subscriptions
    }
  }
}


// "15.09.2026" → "2026-09-15"
export function toIsoDate (germanDate) {
  const [day, month, year] = germanDate.split(".")
  return `${year}-${month}-${day}`
}


// Returns `[{billId, billDate, billAmount, startDate, endDate}]`
// of all invoices (newest first)
export async function getInvoices (page, subscription) {
  const query = new URLSearchParams({
    subscribeType: subscription.subscriberType.toLowerCase(),
    billingAccountId: subscription.billingAccountId,
    msisdn: subscription.msisdn,
    documentType: "fisinvoice",
  })
  const response = await page.request.get(
    `${masterDataApi}/onload/invoiceContent?${query}`)
  if (!response.ok()) {
    throw new Error(`Loading the invoices failed: ${await response.text()}`)
  }
  const content = await response.json()
  return [
    ...content.monthlyInvoices ?? [],
    ...content.oldMonthlyInvoices ?? [],
  ]
}


// The documents are served as base64 encoded JSON attachments
function decodePdf (content, description) {
  const body = Buffer.from(content ?? "", "base64")
  if (!body.subarray(0, 5)
    .toString()
    .startsWith("%PDF")) {
    throw new Error(`${description} is not a PDF`)
  }
  return body
}


export async function downloadInvoices (
  page,
  {subscription, outputDir, fromDate = null, invoiceIds = []},
) {
  const invoices = (await getInvoices(page, subscription))
    .map(invoice => ({...invoice, date: toIsoDate(invoice.billDate)}))
    .filter(invoice =>
      (!fromDate || invoice.date >= fromDate) &&
      (invoiceIds.length === 0 || invoiceIds.includes(invoice.billId)))
  log(`Found ${invoices.length} invoices`)

  const savedPaths = []
  for (const invoice of invoices) {
    const description = `invoice ${invoice.billId} from ${invoice.date} ` +
      `(${invoice.billAmount} €)`
    const filePath = path.join(outputDir, `${invoice.date}_aldi_talk.pdf`)
    if (await fse.pathExists(filePath)) {
      log(`Skip ${description} (already downloaded)`)
      continue
    }
    const response = await page.request.get(
      `${masterDataApi}/downloadDocument/PRINTSHOP:${invoice.billId}`)
    const attachment = (await response.json()).attachment?.[0]
    await fse.writeFile(filePath, decodePdf(attachment?.content, description))
    log(`Saved ${description}`)
    savedPaths.push(filePath)
  }
  return savedPaths
}


// Returns the "YYYY-MM-DD" first and last days
// of all months from `fromDate` until today
export function getMonths (fromDate, today = new Date()) {
  const months = []
  let [year, month] = fromDate.split("-")
    .map(Number)
  const todayIso = today.toISOString()
    .slice(0, 10)
  while (`${year}-${String(month)
    .padStart(2, "0")}` <= todayIso.slice(0, 7)) {
    const lastDay = new Date(Date.UTC(year, month, 0))
      .toISOString()
      .slice(0, 10)
    months.push({
      fromDate: `${year}-${String(month)
        .padStart(2, "0")}-01`,
      toDate: lastDay < todayIso ? lastDay : todayIso,
    })
    month += 1
    if (month > 12) {
      month = 1
      year += 1
    }
  }
  return months
}


// Returns `[{date, amount, name}]` of all top-ups since `fromDate`.
// The portal only answers queries for single months
// and reports an error for months older than its history.
export async function getTopUps (page, subscription, fromDate) {
  const topUps = []
  for (const month of getMonths(fromDate)) {
    const query = new URLSearchParams({
      ...month,
      contractId: subscription.contractId,
    })
    const response = await page.request.get(
      `${costOverviewApi}/v1/topUpAndDeduction?${query}`)
    if (!response.ok()) {
      log(`No top-ups available for ${month.fromDate.slice(0, 7)}`)
      continue
    }
    const items = (await response.json()).topUps?.itemised ?? []
    for (const item of items) {
      topUps.push({
        // E.g. "2026-02-20T07:09:37.103+01:00"
        date: item.date.slice(0, 10),
        amount: item.taxIncludedAmount?.value,
        name: item.name,
      })
    }
  }
  return topUps
}


// Returns the "Kostenübersicht" PDF (top-ups, deductions,
// and connections) of the period
export async function getCostOverviewPdf (
  page,
  {subscription, fromDate, toDate},
) {
  const response = await page.request.post(
    `${costOverviewApi}/v2/pdfGenerator`,
    {
      data: {
        subscriptionId: subscription.contractId,
        fromDate,
        toDate,
        evnStatus: "1",
        msisdn: subscription.msisdn,
        subscriberType: subscription.subscriberType,
        usageRatingTag: "PayGo Non-Zero Rated Paid CDR," +
          "Non-PayGo Non-Zero Rated Paid CDR",
        voiceCount: 0,
        smsCount: 0,
        mmsCount: 0,
        dataCount: 0,
        vasCount: 0,
      },
    },
  )
  if (!response.ok()) {
    throw new Error(`Generating the cost overview ${fromDate} - ${toDate} ` +
      `failed: ${await response.text()}`)
  }
  return decodePdf((await response.json()).content,
    `Cost overview ${fromDate} - ${toDate}`)
}


export async function downloadTopUps (
  page,
  {subscription, outputDir, fromDate},
) {
  const topUps = await getTopUps(page, subscription, fromDate)
  log(`Found ${topUps.length} top-ups`)

  const savedPaths = []
  for (const date of new Set(topUps.map(topUp => topUp.date))) {
    const description = `top-ups from ${date} (` +
      topUps.filter(topUp => topUp.date === date)
        .map(topUp => `${topUp.amount} €`)
        .join(", ") +
      ")"
    const filePath = path.join(outputDir, `${date}_aldi_talk_top_up.pdf`)
    if (await fse.pathExists(filePath)) {
      log(`Skip ${description} (already downloaded)`)
      continue
    }
    const body = await getCostOverviewPdf(page,
      {subscription, fromDate: date, toDate: date})
    await fse.writeFile(filePath, body)
    log(`Saved ${description}`)
    savedPaths.push(filePath)
  }
  return savedPaths
}


// Parses `[top-ups] [from <YYYY-MM-DD>] [<invoice-id>…]`
export function parseArgs (args) {
  const options = {shallGetTopUps: false, fromDate: null, invoiceIds: []}
  for (let index = 0; index < args.length; index++) {
    if (args[index] === "top-ups") {
      options.shallGetTopUps = true
    }
    else if (args[index] === "from") {
      options.fromDate = args[index + 1]
      index += 1
      if (!/^\d{4}-\d{2}-\d{2}$/.test(options.fromDate ?? "")) {
        return null
      }
    }
    else {
      options.invoiceIds.push(args[index])
    }
  }
  return options
}


async function main () {
  const [outputDir, ...args] = process.argv.slice(2)
  const options = parseArgs(args)
  if (!outputDir || !options) {
    console.error("Usage:\n" +
      "  node alditalk.js <dir> [from <YYYY-MM-DD>] [<invoice-id>…]\n" +
      "  node alditalk.js <dir> top-ups [from <YYYY-MM-DD>]")
    process.exitCode = 1
    return
  }

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: "alditalk",
    acceptDownloads: false,
  })

  try {
    const subscription = await login(page)
    await fse.ensureDir(outputDir)

    let savedPaths = []
    if (options.shallGetTopUps) {
      // The portal's history reaches back 2 years
      const twoYearsAgo = new Date()
      twoYearsAgo.setFullYear(twoYearsAgo.getFullYear() - 2)
      savedPaths = await downloadTopUps(page, {
        subscription,
        outputDir,
        fromDate: options.fromDate ?? twoYearsAgo.toISOString()
          .slice(0, 10),
      })
    }
    else {
      savedPaths = await downloadInvoices(page,
        {subscription, outputDir, ...options})
    }
    savedPaths.forEach(filePath => console.info(filePath))
    log(`Downloaded ${savedPaths.length} new documents`)
  }
  catch (error) {
    await dumpDebugFiles(page, "alditalk-debug")
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
