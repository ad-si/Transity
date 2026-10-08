// Downloads documents (e.g. "Online Auszug") from the "Online-Postkorb"
// of the FNZ Bank (formerly ebase / Finvesto) online banking
// at https://portal.fnz.de
//
// Usage:
//   node fnz.js <dir> [all]
//
// Downloads the unread documents (or with `all` all documents
// of the last 12 months) which are not yet in <dir>,
// with the names the bank gives them
// (e.g. "OnlineAuszug-Nr111-2026-10-08.pdf"),
// and prints their paths to stdout.
// Downloading marks the documents as read in the Postkorb.
//
// Environment: FNZ_USERNAME (Zugangs-ID), FNZ_PASSWORD (PIN)
// (else manual login)

import path from "node:path"
import {pathToFileURL} from "node:url"

import fse from "fs-extra"

import {
  captureAttachmentResponse,
  dumpDebugFiles,
  getCredentials,
  launchBrowser,
} from "../browser.js"

const log = console.warn
const baseUrl = "https://portal.fnz.de/(e1)/eo"


export async function login (page, {username, password}) {
  log(`Open ${baseUrl}`)
  await page.goto(baseUrl, {timeout: 30000})

  try {
    if (username && password) {
      await page.fill("#eox_ContentPane_3_txtPbzZugangsId", username,
        {timeout: 15000})
      await page.fill("#eox_ContentPane_3_txtPbzPin", password,
        {timeout: 15000})
    }
    else {
      // The browser profile may have saved the credentials.
      // Chromium hides autofilled values from the page until the user
      // interacts with it, but they match `:autofill`.
      log("No credentials configured, wait for the browser to autofill them")
      await page.waitForSelector("#eox_ContentPane_3_txtPbzPin:autofill",
        {timeout: 10000})
    }
    await page.click("#eox_ContentPane_3_LOGIN_PBZ", {timeout: 15000})
  }
  catch (error) {
    log(`Automated login stopped (${error.message.split("\n")[0]})`)
    log("Please complete the login manually in the browser …")
  }

  log("Wait for the login …")
  await page.waitForSelector("a[href*='services/logout']", {timeout: 600000})
  await closeAdPopup(page)
}


// After the login an advertisement may cover the page
async function closeAdPopup (page) {
  const closeButton = page.locator(".modal.show button",
    {hasText: "Schließen"})
  try {
    await closeButton.click({timeout: 5000})
    log("Closed the popup")
  }
  catch { /* No popup was shown */ }
}


// The file name of a "Content-Disposition" header, without directories
// and without the counter the bank appends for each download
// (e.g. "OnlineAuszug-Nr111-2026-10-08-2.pdf")
export function getFileName (disposition) {
  const encoded = disposition.match(/filename\*=(?:UTF-8|utf-8)''([^;]+)/)
  const plain = disposition.match(/filename="?([^";]+)"?/)
  const name = encoded
    ? decodeURIComponent(encoded[1])
    : plain?.[1]
  return name
    ? path.basename(name.trim())
      .replace(/(\d{4}-\d{2}-\d{2})-\d+(\.\w+)$/, "$1$2")
    : null
}


export async function downloadDocuments (
  page,
  {outputDir, shallDownloadAll = false},
) {
  log("Go to the Online-Postkorb")
  await page.goto(`${baseUrl}/depot-konto/online-postkorb`,
    {timeout: 30000})
  await page.waitForLoadState("networkidle")
  await closeAdPopup(page)

  await fse.ensureDir(outputDir)
  let downloadCounter = 0

  while (true) {
    const rows = page.locator(".postbox-table tbody tr")
    const rowCount = await rows.count()

    for (let index = 0; index < rowCount; index++) {
      const row = rows.nth(index)
      const subject = (await row.locator("td")
        .nth(3)
        .innerText()).trim()
      const date = (await row.locator("time")
        .first()
        .innerText()).trim()
      const isUnread = (await row.getAttribute("class") || "")
        .split(" ")
        .includes("unread")
      if (!isUnread && !shallDownloadAll) {
        continue
      }

      await row.hover()
      const {body, headers} = await captureAttachmentResponse(page, () =>
        row.locator("button", {hasText: "Dokument herunterladen"})
          .click())
      // The file is served in a popup, which stays empty
      for (const otherPage of page.context()
        .pages()) {
        if (otherPage !== page) {
          await otherPage.close()
        }
      }

      const fileName = getFileName(headers["content-disposition"] || "") ??
        `${date} ${subject}.pdf`
      const filePath = path.join(outputDir, fileName)
      if (await fse.pathExists(filePath)) {
        log(`Skip ${date} ${subject} (already downloaded)`)
        continue
      }
      await fse.writeFile(filePath, body)
      log(`Saved ${date} ${subject}`)
      console.info(filePath)
      downloadCounter += 1
    }

    const nextButton = page.locator(".pagination button")
      .last()
    if (await nextButton.count() === 0 || await nextButton.isDisabled()) {
      break
    }
    await nextButton.click()
    await page.waitForLoadState("networkidle")
  }

  log(`Downloaded ${downloadCounter} new documents`)
}


async function main () {
  const outputDir = process.argv[2]
  if (!outputDir) {
    console.error("Usage: node fnz.js <dir> [all]")
    process.exitCode = 1
    return
  }

  const credentials = await getCredentials("FNZ", "FNZ Bank")
  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: "fnz",
    acceptDownloads: false,
  })

  try {
    await login(page, credentials)
    await downloadDocuments(page, {
      outputDir,
      shallDownloadAll: process.argv[3] === "all",
    })
  }
  catch (error) {
    await dumpDebugFiles(page, "fnz-debug")
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
