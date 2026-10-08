// Downloads the transactions CSV ("Umsatzliste") of a DKB account
// from https://banking.dkb.de and prints it to stdout.
//
// Usage:
//   node dkb.js [from <YYYY-MM-DD>] [to <YYYY-MM-DD>]
//   (default: the last 90 days until today)
//   node dkb.js statements <dir>
//   Download all account statements ("Kontoauszüge") from the mailbox
//   which are not yet in <dir>, named "<statement date>_<number>.pdf"
//
//   node dkb.js documents <dir> [from <YYYY-MM-DD>] [to <YYYY-MM-DD>]
//   (default: the last 90 days until today)
//   Download all documents of the period from the mailbox
//   which are not yet in <dir>, with the names the bank gives them,
//   and print their paths to stdout.
//
// Environment: DKB_USERNAME, DKB_PASSWORD (else manual login),
// DKB_IBAN (account to export, default: the first account)
//
// The login needs a captcha and a confirmation in the DKB app,
// so it is always completed in the visible browser window.

import path from "node:path"

import fse from "fs-extra"

import {
  dumpDebugFiles,
  getCredentials,
  launchBrowser,
} from "../browser.js"

const log = console.warn
const baseUrl = "https://banking.dkb.de"


function toDDdotMMdotYYYY (date) {
  return [
    String(date.getDate())
      .padStart(2, "0"),
    String(date.getMonth() + 1)
      .padStart(2, "0"),
    date.getFullYear(),
  ].join(".")
}


function getDateArg (name) {
  const index = process.argv.indexOf(name)
  return index > -1
    ? new Date(`${process.argv[index + 1]}T00:00:00`)
    : null
}


async function login (page, {username, password}) {
  log(`Open ${baseUrl}/login`)
  await page.goto(`${baseUrl}/login`, {timeout: 30000})

  try {
    if (!username || !password) {
      throw new Error("No credentials configured")
    }
    await page.getByLabel("Anmeldename")
      .fill(username, {timeout: 15000})
    await page.getByLabel("Passwort")
      .fill(password, {timeout: 15000})
    log("Solve the captcha and click \"Anmelden\" in the browser …")
  }
  catch (error) {
    log(`Automated login stopped (${error.message.split("\n")[0]})`)
    log("Please complete the login manually in the browser …")
  }

  log("Wait for the login (confirm it in the DKB app) …")
  await page.waitForSelector("a[href^='/account/']", {timeout: 600000})
}


async function getAccountPath (page, iban) {
  const accounts = await page.evaluate(() => Array
    .from(document.querySelectorAll("a[href^='/account/']"))
    .map(link => ({
      path: link.getAttribute("href")
        .split("/")
        .slice(0, 3)
        .join("/"),
      // The IBAN is shown with spaces in the account tile
      text: (link.closest("section, li, div")?.innerText ?? "")
        .replace(/\s+/g, ""),
    })),
  )

  const account = iban
    ? accounts.find(acc => acc.text.includes(iban.replace(/\s+/g, "")))
    : accounts[0]

  if (!account) {
    throw new Error(`Account ${iban ?? ""} not found`)
  }
  return account.path
}


async function exportCsv (page, {startDate, endDate}) {
  log(`Set period ${toDDdotMMdotYYYY(startDate)} - ${
    toDDdotMMdotYYYY(endDate)}`)
  await page.click("button[aria-label='Zeitraum festlegen']")
  for (const [selector, date] of [
    ["#dateFrom", startDate],
    ["#dateTo", endDate],
  ]) {
    await page.click(selector)
    await page.keyboard.press("Meta+A")
    await page.keyboard.type(toDDdotMMdotYYYY(date), {delay: 30})
    await page.keyboard.press("Tab")
    await page.waitForTimeout(400)
  }
  await page.getByRole("button", {name: "Umsätze anzeigen"})
    .click()
  await page.waitForTimeout(4000)

  // The CSV is generated in the page as a blob.
  // Capture its content and suppress the actual download,
  // as Chromium crashes (SIGSEGV) when Playwright handles downloads.
  await page.evaluate(() => {
    window.transityBlobs = []
    const createObjectURL = URL.createObjectURL
    URL.createObjectURL = function (object) {
      if (object instanceof Blob) {
        object.text()
          .then(text => window.transityBlobs.push(text))
      }
      return createObjectURL.call(this, object)
    }
    HTMLAnchorElement.prototype.click = () => {}
  })

  log("Export CSV")
  await page.locator("text=CSV")
    .last()
    .click()
  await page.waitForFunction(
    () => window.transityBlobs.length > 0,
    null,
    {timeout: 60000},
  )
  return page.evaluate(() => window.transityBlobs[0])
}


// Uses the JSON API of the mailbox ("Postfach") with the session cookies
function getDocuments (page) {
  return page.evaluate(async () => {
    const response = await fetch(
      "/api/documentstorage/documents?page%5Blimit%5D=1000",
      {headers: {accept: "application/vnd.api+json"}},
    )
    return (await response.json()).data
  })
}


async function fetchDocument (page, id) {
  const base64 = await page.evaluate(async documentId => {
    const response = await fetch(
      `/api/documentstorage/documents/${documentId}`,
      {headers: {accept: "application/pdf"}},
    )
    const bytes = new Uint8Array(await response.arrayBuffer())
    let binary = ""
    for (const byte of bytes) {
      binary += String.fromCharCode(byte)
    }
    return btoa(binary)
  }, id)
  return Buffer.from(base64, "base64")
}


// The date (YYYY-MM-DD) the document was put into the mailbox
function getDocumentDate ({attributes}) {
  const date = attributes.creationDate ??
    attributes.receivedDate ??
    attributes.documentDate ??
    attributes.metadata?.statementDate
  return date?.slice(0, 10) ?? null
}


async function downloadDocuments (page, {outputDir, startDate, endDate}) {
  const documents = await getDocuments(page)
  const toIsoDate = date => [
    date.getFullYear(),
    String(date.getMonth() + 1)
      .padStart(2, "0"),
    String(date.getDate())
      .padStart(2, "0"),
  ].join("-")
  const [start, end] = [toIsoDate(startDate), toIsoDate(endDate)]

  await fse.ensureDir(outputDir)
  let downloadCounter = 0

  for (const document of documents) {
    const {fileName} = document.attributes
    const date = getDocumentDate(document)
    if (!date) {
      log(`No date for "${fileName}": ${JSON.stringify(document.attributes)}`)
    }
    else if (date < start || date > end) {
      continue
    }

    const filePath = path.join(
      outputDir,
      /\.pdf$/i.test(fileName) ? fileName : `${fileName}.pdf`,
    )
    if (await fse.pathExists(filePath)) {
      log(`Skip ${date} ${fileName} (already downloaded)`)
      continue
    }

    await fse.writeFile(filePath, await fetchDocument(page, document.id))
    log(`Saved ${date} ${fileName}`)
    console.info(filePath)
    downloadCounter += 1
  }

  log(`Downloaded ${downloadCounter} new documents`)
}


async function downloadStatements (page, outputDir) {
  const documents = await getDocuments(page)

  let downloadCounter = 0
  for (const document of documents) {
    // e.g. "Kontoauszug_7_2026_vom_06.07.2026_zu_Konto_1016815407"
    const match = document.attributes.fileName
      .match(/^Kontoauszug_(\d+)_\d{4}_vom_/)
    const date = document.attributes.metadata?.statementDate
    if (!match || !date) {
      continue
    }
    const filePath = path.join(
      outputDir, `${date}_${match[1].padStart(2, "0")}.pdf`)
    if (await fse.pathExists(filePath)) {
      continue
    }

    log(`Download ${filePath}`)
    await fse.writeFile(filePath, await fetchDocument(page, document.id))
    downloadCounter += 1
  }

  log(`Downloaded ${downloadCounter} new statements`)
}


async function main () {
  const credentials = await getCredentials("DKB", "DKB")
  const endDate = getDateArg("to") ?? new Date()
  const startDate = getDateArg("from") ??
    new Date(endDate.getTime() - 90 * 24 * 60 * 60 * 1000)

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: "dkb",
    acceptDownloads: false,
  })

  try {
    await login(page, credentials)

    if (process.argv[2] === "statements") {
      await downloadStatements(page, process.argv[3] || ".")
      return
    }
    if (process.argv[2] === "documents") {
      await downloadDocuments(page, {
        outputDir: process.argv[3] || ".",
        startDate,
        endDate,
      })
      return
    }

    const accountPath = await getAccountPath(page, process.env.DKB_IBAN)
    log(`Open ${baseUrl}${accountPath}`)
    await page.goto(`${baseUrl}${accountPath}`, {timeout: 30000})
    await page.waitForSelector("button[aria-label='Zeitraum festlegen']")

    const csv = await exportCsv(page, {startDate, endDate})
    console.info(csv.trimEnd())
  }
  catch (error) {
    await dumpDebugFiles(page, "dkb-debug")
    console.error(error)
    process.exitCode = 1
  }
  finally {
    await browser.close()
  }
}


main()
