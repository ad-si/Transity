// Downloads documents (e.g. "Versicherungsschein", "Bescheinigung")
// from the "Postfach" of the Versicherungskammer Bayern (VKB) Kundenportal
// at https://www.vkb.de/service/kundenportal.html
//
// Usage:
//   node vkb.js <dir> [all]
//
// Downloads the unread documents (or with `all` all documents)
// which are not yet in <dir>, with the names the portal gives them
// (e.g. "KK000052206502_Versicherungsschein_02_2026-11-24-02.00.34.9860.pdf"),
// and prints their paths to stdout.
// Downloading marks the documents as read in the Postfach.
//
// The portal signs each API request, so the documents are downloaded
// by clicking them in the Postfach and capturing the portal's responses.
// The first download of a session must be confirmed
// with a code which the portal sends via SMS (entered in the portal,
// the script requests it).
//
// Environment: VKB_USERNAME, VKB_PASSWORD
// (else manual login)

import path from "node:path"
import {pathToFileURL} from "node:url"

import fse from "fs-extra"

import {dumpDebugFiles, getCredentials, launchBrowser} from "../browser.js"

const log = console.warn
const portalUrl = "https://www.vkb.de/service/kundenportal.html"


export async function login (page, {username, password}) {
  log(`Open ${portalUrl}`)
  await page.goto(`${portalUrl}#/kp/postfach`, {timeout: 30000})

  try {
    await page.waitForSelector("#login_password", {timeout: 30000})
    if (username && password) {
      await page.fill("#login_username", username, {timeout: 15000})
      await page.fill("#login_password", password, {timeout: 15000})
    }
    else {
      // The browser profile may have saved the credentials.
      // Chromium hides autofilled values from the page until the user
      // interacts with it, but they match `:autofill`.
      log("No credentials configured, wait for the browser to autofill them")
      await page.waitForSelector("#login_password:autofill",
        {timeout: 10000})
    }
    await page.click("button[type='submit']", {timeout: 15000})
  }
  catch (error) {
    log(`Automated login stopped (${error.message.split("\n")[0]})`)
    log("Please complete the login manually in the browser …")
  }

  log("Wait for the login …")
  await page.waitForURL(/#\/kp\//, {timeout: 600000})
}


function isPostfachResponse (response) {
  return response.url()
    .includes("/postfach/select/v1") &&
    response.request()
      .method() === "POST"
}


// Reads the portal's list of documents whenever it loads it.
// The body must be read right away, as it's gone after the page reloads
// (e.g. after the login). Must be called before the login,
// as the portal may load the list right after it (e.g. on the dashboard).
export function watchPostfach (page) {
  const postfach = {sendungen: null, error: null}
  page.on("response", async response => {
    if (!isPostfachResponse(response)) {
      return
    }
    try {
      if (!response.ok()) {
        throw new Error(`HTTP ${response.status()}`)
      }
      const {data, errors} = await response.json()
      if (errors?.length > 0) {
        throw new Error(JSON.stringify(errors))
      }
      postfach.sendungen = data?.sendungen ?? []
    }
    catch (error) {
      postfach.error = error
    }
  })
  return postfach
}


// The login reloads the page, which destroys the page's JavaScript context,
// so retry until the reload is done
async function openPostfach (page) {
  for (let attempt = 1; ; attempt++) {
    try {
      await page.waitForLoadState("load")
      await page.evaluate(() => {
        window.location.hash = "#/kp/postfach"
      })
      return
    }
    catch (error) {
      if (attempt >= 5 || !/context was destroyed/.test(error.message)) {
        throw error
      }
      await page.waitForTimeout(1000)
    }
  }
}


// Opens the Postfach and returns the portal's list of "Sendungen"
// (each with its "dokumente")
export async function loadPostfach (page, postfach) {
  log("Go to the Postfach")
  await openPostfach(page)

  // The portal may still have been starting up and gone to the dashboard
  const firstRow = page.locator("tbody tr.row")
    .first()
  try {
    await firstRow.waitFor({timeout: 15000})
  }
  catch {
    await openPostfach(page)
    await firstRow.waitFor({timeout: 30000})
  }

  for (let waited = 0; !postfach.sendungen && waited < 30000; waited += 500) {
    await page.waitForTimeout(500)
  }
  if (!postfach.sendungen) {
    throw new Error("Loading the Postfach failed: " +
      (postfach.error?.message ?? "the portal didn't load it"))
  }
  return postfach.sendungen
}


// The documents to download, each with the number of documents
// of its "Sendung" (needed for the file name)
export function selectDocuments (sendungen, {shallDownloadAll = false} = {}) {
  return sendungen.flatMap(sendung => {
    const dokumente = sendung.dokumente ?? []
    return dokumente
      .filter(dokument =>
        dokument.mediaType === "application/pdf" &&
        (shallDownloadAll || dokument.gelesen === "U"))
      .map(dokument => ({dokument, sendungSize: dokumente.length}))
  })
}


// Same name as the portal gives the file (`buildPdfDownloadDateiname`)
export function getFileName (dokument, sendungSize) {
  const counter = sendungSize > 1
    ? `_${String(dokument.lfdNr)
      .padStart(2, "0")
      .slice(-2)}`
    : ""
  const timestamp = dokument.dokumentTimestamp.slice(0, -2)
  return path.basename(
    `${dokument.ordnungsBegriff}_${dokument.typ}${counter}_${timestamp}.pdf`
      .replaceAll(/[/\\:]/g, "-"),
  )
}


// Identifies a table row of the Postfach by its date and document names
export function getRowKey (date, typen) {
  return `${date}|${typen.join("|")}`
}


// The Postfach shows the dates in the time zone of the browser
export function getSendungKey (sendung, timeZone) {
  const date = new Intl.DateTimeFormat("de-DE", {
    timeZone,
    day: "2-digit",
    month: "2-digit",
    year: "numeric",
  })
    .format(new Date(sendung.timestamp))
  const typen = (sendung.dokumente ?? []).map(dokument => dokument.typ)
  return getRowKey(date, typen)
}


// Clicks the button of a document and returns the PDF of the portal's
// response. The first download asks for an SMS code in the portal.
async function downloadDokument (page, button, dokument) {
  const responsePromise = page.waitForResponse(
    response => response.url()
      .includes("/postfach/pdf/v1") &&
      response.status() === 200 &&
      (response.request()
        .postData() ?? "")
        .includes(dokument.dokumentId),
    {timeout: 600000},
  )
  await button.click()

  // The first download of a session shows a dialog to confirm it
  // with an SMS code. Request the code, so the user only has to enter it.
  const requestCodeButton = page.getByRole("button", {name: "Code anfordern"})
  const isStepUpRequired = await Promise.race([
    responsePromise.then(() => false),
    requestCodeButton.waitFor({timeout: 60000})
      .then(() => true, () => false),
  ])
  if (isStepUpRequired) {
    await requestCodeButton.click()
    log("Please enter the code sent via SMS in the browser …")
  }
  const response = await responsePromise

  // Some documents are opened in a popup, which stays empty
  for (const otherPage of page.context()
    .pages()) {
    if (otherPage !== page) {
      await otherPage.close()
    }
  }

  const pdf = await response.body()
  if (pdf.subarray(0, 5)
    .toString() !== "%PDF-") {
    throw new Error("The response is no PDF file")
  }
  return pdf
}


export async function downloadDocuments (
  page,
  {outputDir, postfach, shallDownloadAll = false},
) {
  const sendungen = await loadPostfach(page, postfach)
  await fse.ensureDir(outputDir)

  // The documents to download, by Sendung, in the order of the table
  const pending = new Map()
  for (const {dokument, sendungSize} of
    selectDocuments(sendungen, {shallDownloadAll})) {
    const filePath = path.join(outputDir, getFileName(dokument, sendungSize))
    if (await fse.pathExists(filePath)) {
      log(`Skip ${dokument.dokumentTimestamp.slice(0, 10)} ${dokument.typ}` +
        " (already downloaded)")
    }
    else {
      pending.set(dokument.dokumentId, filePath)
    }
  }
  log(`Found ${pending.size} documents to download`)

  // Several Sendungen can have the same date and documents
  const timeZone = await page.evaluate(() =>
    new Intl.DateTimeFormat()
      .resolvedOptions().timeZone)
  const sendungenByKey = new Map()
  for (const sendung of sendungen) {
    const key = getSendungKey(sendung, timeZone)
    sendungenByKey.set(key, [...sendungenByKey.get(key) ?? [], sendung])
  }

  let downloadCounter = 0
  let failureCounter = 0

  while (pending.size > 0) {
    const rows = page.locator("tbody tr.row")
    const rowCount = await rows.count()

    for (let index = 0; index < rowCount && pending.size > 0; index++) {
      const row = rows.nth(index)
      const date = (await row.locator("td.col-date span")
        .first()
        .innerText()).trim()
      const buttons = row.locator("td.col-dokumente button")
      const typen = (await row.locator("td.col-dokumente .p-button-label")
        .allInnerTexts())
        .map(typ => typ.trim())
      const sendung = sendungenByKey.get(getRowKey(date, typen))
        ?.shift()
      if (!sendung) {
        log(`Skip unknown row "${date}: ${typen.join(", ")}"`)
        continue
      }

      for (const [position, dokument] of sendung.dokumente.entries()) {
        const filePath = pending.get(dokument.dokumentId)
        if (!filePath) {
          continue
        }
        const description = `${date} ${dokument.typ}`
        try {
          const pdf = await downloadDokument(
            page, buttons.nth(position), dokument)
          await fse.writeFile(filePath, pdf)
          log(`Saved ${description}`)
          console.info(filePath)
          downloadCounter += 1
        }
        catch (error) {
          log(`Failed to download ${description}: ${error.message}`)
          failureCounter += 1
        }
        pending.delete(dokument.dokumentId)
      }
    }

    if (pending.size === 0) {
      break
    }
    const nextButton = page.locator(".p-paginator-next")
      .first()
    if (await nextButton.count() === 0 || await nextButton.isDisabled()) {
      break
    }
    const firstRowText = await rows.first()
      .innerText()
    await nextButton.click()
    await page.waitForFunction(
      text => document.querySelector("tbody tr.row")?.innerText !== text,
      firstRowText,
      {timeout: 30000},
    )
  }

  log(`Downloaded ${downloadCounter} new documents`)
  if (pending.size > 0) {
    log(`${pending.size} documents weren't found in the Postfach`)
  }
  if (failureCounter + pending.size > 0) {
    throw new Error(
      `${failureCounter + pending.size} documents could not be downloaded`)
  }
}


async function main () {
  const outputDir = process.argv[2]
  if (!outputDir) {
    console.error("Usage: node vkb.js <dir> [all]")
    process.exitCode = 1
    return
  }

  const credentials = await getCredentials("VKB", "VKB Kundenportal")
  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: "vkb",
    // The portal saves the PDFs via a link to a blob URL.
    // They are read from the network responses instead.
    acceptDownloads: false,
  })

  try {
    const postfach = watchPostfach(page)
    await login(page, credentials)
    await downloadDocuments(page, {
      outputDir,
      postfach,
      shallDownloadAll: process.argv[3] === "all",
    })
  }
  catch (error) {
    await dumpDebugFiles(page, "vkb-debug")
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
