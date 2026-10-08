// Downloads the transactions CSV ("Umsatzliste") of the HypoVereinsbank
// giro account from https://my.hypovereinsbank.de and writes it to stdout
// unchanged (UTF-16LE, as exported by the bank).
//
// Usage:
//   node hypovereinsbank.js [from <YYYY-MM-DD>] [to <YYYY-MM-DD>]
//   (default: the last 90 days until today)
//
//   node hypovereinsbank.js documents <dir> \
//     [from <YYYY-MM-DD>] [to <YYYY-MM-DD>]
//   (default: the last 90 days until today)
//   Download all documents of the period from the mailbox ("Postfach")
//   which are not yet in <dir>, with the names the bank gives them
//   (e.g. "Kontoauszug_0386826510_(258663519).PDF"),
//   and print their paths to stdout.
//   Downloading marks the documents as read in the mailbox.
//
// Environment: HYPOVEREINSBANK_USERNAME (Direct Banking Nummer),
// HYPOVEREINSBANK_PASSWORD (else manual login)
//
// The login has to be confirmed in the HVB app (SCA).

import path from "node:path"
import {pathToFileURL} from "node:url"

import fse from "fs-extra"

import {
  captureAttachment,
  captureAttachmentResponse,
  dumpDebugFiles,
  getCredentials,
  launchBrowser,
} from "../browser.js"

const log = console.warn
const baseUrl = "https://my.hypovereinsbank.de"


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
  const url = `${baseUrl}/login?view=/de/login.jsp`
  log(`Open ${url}`)
  await page.goto(url, {timeout: 30000})

  try {
    if (username && password) {
      await page.fill("#username", username, {timeout: 15000})
      await page.fill("#px2", password, {timeout: 15000})
    }
    else {
      // The browser profile may have saved the credentials.
      // Chromium hides autofilled values from the page until the user
      // interacts with it, but they match `:autofill`.
      log("No credentials configured, wait for the browser to autofill them")
      await page.waitForSelector("#px2:autofill", {timeout: 10000})
    }
    await page.click("#loginCommandButton", {timeout: 15000})
  }
  catch (error) {
    log(`Automated login stopped (${error.message.split("\n")[0]})`)
    log("Please complete the login manually in the browser …")
  }

  log("Wait for the login (confirm it in the HVB app) …")
  await page.waitForURL(/finanzstatus\.jsp/, {timeout: 600000})
  // The portal still sets up the session after showing the overview.
  // Navigating away before it is done ends in a redirect loop.
  await page.waitForLoadState("networkidle")
}


async function exportCsv (page, {startDate, endDate}) {
  log("Go to transactions page")
  await page.goto(
    `${baseUrl}/portal?view=/de/banking/konto/kontofuehrung/umsaetze.jsp`,
    {timeout: 30000},
  )
  await page.waitForSelector("#dateFrom_input")

  log(`Set period ${toDDdotMMdotYYYY(startDate)} - ${
    toDDdotMMdotYYYY(endDate)}`)
  await page.fill("#dateFrom_input", toDDdotMMdotYYYY(startDate))
  await page.fill("#dayTo_input", toDDdotMMdotYYYY(endDate))
  await page.click("#showtransactions")
  await page.waitForTimeout(6000)

  log("Export CSV")
  return captureAttachment(page, () => page.click("a[title=CSV]"))
}


// The file name of a "Content-Disposition" header, without directories
function getFileName (disposition) {
  const encoded = disposition.match(/filename\*=(?:UTF-8|utf-8)''([^;]+)/)
  const plain = disposition.match(/filename="?([^";]+)"?/)
  const name = encoded
    ? decodeURIComponent(encoded[1])
    : plain?.[1]
  return name
    ? path.basename(name.trim())
    : null
}


// The documents in the table of the current page
function getListedDocuments (page) {
  return page.$$eval("#postboxDocumentTable_data tr[data-rk]", rows =>
    rows.map(row => {
      const cells = [...row.querySelectorAll("td")]
        .map(cell => cell.innerText.trim()
          .replace(/\s+/g, " "))
      return {id: row.dataset.rk, date: cells[1], subject: cells[2]}
    }))
}


export async function downloadDocuments (
  page,
  {outputDir, startDate, endDate},
) {
  log("Go to documents page")
  const documentsUrl =
    `${baseUrl}/portal?view=/de/banking/uebersicht/postfach/ihre-dokumente.jsp`
  try {
    await page.goto(documentsUrl, {timeout: 30000})
  }
  catch (error) {
    if (!error.message.includes("ERR_TOO_MANY_REDIRECTS")) {
      throw error
    }
    log("Redirect loop, retry in 5 s")
    await page.waitForTimeout(5000)
    await page.goto(documentsUrl, {timeout: 30000})
  }
  await page.waitForSelector("#dateFrom_input")

  log(`Set period ${toDDdotMMdotYYYY(startDate)} - ${
    toDDdotMMdotYYYY(endDate)}`)
  await page.fill("#dateFrom_input", toDDdotMMdotYYYY(startDate))
  await page.fill("#dateTo_input", toDDdotMMdotYYYY(endDate))
  await page.click("#refreshbutton")
  await page.waitForLoadState("networkidle")

  // Show as many documents per page as possible (the largest option is
  // the total count), as paging races with the refresh after each download
  const rowsPerPage = page.locator(
    "#postboxDocumentTable_paginator_bottom select.ui-paginator-rpp-options")
  if (await rowsPerPage.count() > 0) {
    const options = await rowsPerPage.locator("option")
      .evaluateAll(elements => elements.map(element => Number(element.value)))
    await rowsPerPage.selectOption(String(Math.max(...options)))
    await page.waitForLoadState("networkidle")
    await page.waitForTimeout(2000)
  }

  await fse.ensureDir(outputDir)
  const existingFiles = await fse.readdir(outputDir)
  const seenIds = new Set()
  let downloadCounter = 0

  while (true) {
    for (const document of await getListedDocuments(page)) {
      if (seenIds.has(document.id)) {
        continue
      }
      seenIds.add(document.id)
      // The bank's file names end with the document id, e.g. "(258663519).PDF"
      if (existingFiles.some(fileName => fileName.includes(document.id))) {
        log(`Skip ${document.date} ${document.subject} (already downloaded)`)
        continue
      }

      const {body, headers} = await captureAttachmentResponse(page, () =>
        page.click(`tr[data-rk="${document.id}"] a[id$=":doDownload"]`))
      const fileName =
        getFileName(headers["content-disposition"] || "") ??
        `${document.id}.pdf`
      const filePath = path.join(outputDir, fileName)
      await fse.writeFile(filePath, body)
      log(`Saved ${document.date} ${document.subject}`)
      console.info(filePath)
      downloadCounter += 1
      // Downloading refreshes the table (delayed) to mark the document as read
      await page.waitForTimeout(2000)
      await page.waitForLoadState("networkidle")
    }

    const nextLink = page.locator(
      "#postboxDocumentTable a.ui-paginator-next:not(.ui-state-disabled)")
    if (await nextLink.count() === 0) {
      break
    }
    await nextLink.first()
      .click()
    await page.waitForLoadState("networkidle")
  }

  log(`Downloaded ${downloadCounter} new documents`)
}


async function main () {
  const credentials = await getCredentials(
    "HYPOVEREINSBANK", "HypoVereinsbank")
  const endDate = getDateArg("to") ?? new Date()
  const startDate = getDateArg("from") ??
    new Date(endDate.getTime() - 90 * 24 * 60 * 60 * 1000)

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: "hypovereinsbank",
    acceptDownloads: false,
  })

  try {
    await login(page, credentials)

    if (process.argv[2] === "documents") {
      await downloadDocuments(page, {
        outputDir: process.argv[3] || ".",
        startDate,
        endDate,
      })
      return
    }

    const csv = await exportCsv(page, {startDate, endDate})
    process.stdout.write(csv)
  }
  catch (error) {
    await dumpDebugFiles(page, "hypovereinsbank-debug")
    console.error(error)
    process.exitCode = 1
  }
  finally {
    await browser.close()
  }
}


// Allows importing the functions, e.g. to test them in a running browser
if (import.meta.url === pathToFileURL(process.argv[1]).href) {
  main()
}
