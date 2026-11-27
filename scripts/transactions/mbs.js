// Downloads data from the online banking of the Mittelbrandenburgische
// Sparkasse (MBS) or any other Sparkasse (see `SPARKASSE_URL` below).
//
// Usage:
//   node mbs.js
//   Print the transactions of the last 90 days as YAML
//
//   node mbs.js from <YYYY-MM-DD>
//   Print the raw CSV export (CAMT V2) from the date until today
//
//   node mbs.js MT940 [<YYYY-MM>]
//   Print the MT940 file of the month (default: the previous month)
//
//   node mbs.js postbox <dir>
//   Download the new account and credit card statements
//   from the electronic postbox as <dir>/<year>/<YYYY-MM>_mbs.pdf
//   and <dir>/<year>/<YYYY-MM-DD>_credit_card_statement.pdf
//
//   node mbs.js documents <dir> [from <YYYY-MM-DD>]
//   Download all documents of any type (statements,
//   "Informationen/Dokumente", …) posted to the electronic postbox
//   since the date (default: 90 days ago) which are not yet in <dir>,
//   with the names the bank gives them, and print their paths to stdout
//
// Environment: MBS_USERNAME, MBS_PASSWORD (else manual login)
// The login has to be confirmed with pushTAN.

import { execFile } from "node:child_process"
import path from "node:path"
import { promisify } from "node:util"

import fse from "fs-extra"

import converter from "converter"
import { temporaryDirectory, temporaryFile } from "tempy"
import yaml from "js-yaml"

import {
  rmEmptyString,
  toDDdotMMdotYYYY,
  keysToEnglish,
  noteToAccount,
  sanitizeYaml,
} from "../helpers.js"
import {
  captureAttachment,
  dumpDebugFiles,
  getCredentials,
  getNewestFiledMonth,
  launchBrowser,
} from "../browser.js"

// Any Sparkasse with the same online banking (e.g.
// SPARKASSE_URL=https://www.sparkasse-oberhessen.de) can be used,
// with the credentials SPARKASSE_USERNAME and SPARKASSE_PASSWORD.
// Defaults to the Mittelbrandenburgische Sparkasse (MBS_USERNAME, …).
const baseUrl = process.env.SPARKASSE_URL || "https://www.mbs.de"



async function normalizeAndPrint (filePathTemp) {
  const csvnorm = await import("csvnorm")
  const csv2json = converter({
    from: "csv",
    to: "json",
    // TODO: Use again when http://github.com/doowb/converter/issues/19 is fixed
    // to: 'yml',
  })

  let jsonTemp = ""
  csv2json.on("data", chunk => {
    jsonTemp += chunk
  })
  csv2json.on("end", () => {
    const transactions = JSON
      .parse(jsonTemp)
      .map(keysToEnglish)
      .reverse() // Now sorted ascending by value date
      .map(transaction => {
        const newFields = {
          utc: transaction["value-utc"] < transaction["entry-utc"]
            ? transaction["value-utc"]
            : transaction["entry-utc"],
          note: "",
        }
        const sortedTransaction = Object.assign(newFields, transaction)

        if (sortedTransaction["value-utc"] === sortedTransaction.utc) {
          delete sortedTransaction["value-utc"]
        }
        if (sortedTransaction["entry-utc"] === sortedTransaction.utc) {
          delete sortedTransaction["entry-utc"]
        }

        const account = noteToAccount(transaction.to)
        const transfersObj = transaction.amount.startsWith("-")
          ? {
            transfers: [{
              from: "mbs:giro",
              to: account,
              amount: transaction.amount.slice(1) + transaction.currency,
            }],
          }
          : {
            transfers: [{
              from: account,
              to: "mbs:giro",
              amount: transaction.amount + transaction.currency,
            }],
          }
        const newTransaction = Object.assign(sortedTransaction, transfersObj)

        delete newTransaction.to
        delete newTransaction.amount
        delete newTransaction.currency

        return JSON.parse(JSON.stringify(newTransaction, rmEmptyString))
      })

    const yamlString = sanitizeYaml(yaml.dump({transactions}))

    console.info(yamlString)
  })

  csvnorm.default({
    readableStream: fse.createReadStream(filePathTemp),
    writableStream: csv2json,
  })
}


async function downloadRange (options = {}) {
  const {
    page,
    filePathTemp,
    exportLabel = "Excel (CSV-CAMT V2)",
    startDate,
    endDate,
  } = options
  const log = console.warn

  try {
    // Without an explicit range the default ("3 Monate") is exported
    if (startDate && endDate) {
      const vonDate = toDDdotMMdotYYYY(startDate)
      const bisDate = toDDdotMMdotYYYY(endDate)
      log(`Set range ${vonDate} to ${bisDate}`)

      // Element ids and option values are randomized on every page load,
      // but the page exposes them via its global `IF` registry
      const ids = await page.evaluate(() => ({
        /* global IF */
        zeitraum: IF.get("zeitraumId"),
        von: IF.get("datumVonId"),
        bis: IF.get("datumBisId"),
        // Some Sparkassen have no separate filter button
        // and apply the range with "Aktualisieren" instead
        apply: IF.get("filterAnwendenButton") || IF.get("refreshButtonName"),
      }))

      await page.evaluate(opts => {
        const select = document.getElementById(opts.zeitraum)
        select.value = Array
          .from(select.options)
          .find(option => option.textContent.trim() === "Eigener Zeitraum")
          .value
        select.dispatchEvent(new Event("change", {bubbles: true}))

        for (const [id, value] of [
          [opts.von, opts.vonDate],
          [opts.bis, opts.bisDate],
        ]) {
          const input = document.getElementById(id)
          input.value = value
          input.dispatchEvent(new Event("input", {bubbles: true}))
          input.dispatchEvent(new Event("change", {bubbles: true}))
          // Marks the current form to detect when it was re-rendered
          input.dataset.transityOld = "true"
        }

        document.getElementById(opts.apply).click()
      }, {...ids, vonDate, bisDate})

      // Ranges reaching back more than 90 days may require a TAN approval
      log("Wait for filtered list (confirm the pushTAN prompt if necessary) …")
      await page.waitForFunction(
        opts => {
          // Not every Sparkasse shows the summary line with the range
          const summary = document.querySelector(".umsatzanzahl")
          if (summary) {
            return summary.textContent.includes(opts.bisDate)
          }
          try {
            const vonInput = document.getElementById(IF.get("datumVonId"))
            return vonInput && !vonInput.dataset.transityOld &&
              vonInput.value === opts.vonDate
          }
          catch {
            return false  // The page is being re-rendered
          }
        },
        {vonDate, bisDate},
        {timeout: 180000},
      )
    }

    log(`Download "${exportLabel}" file`)
    await page.waitForTimeout(2000)  // Let the page settle after re-renders

    // The export links exist in the DOM even while their menu is closed
    // (and clicking through the menu fails when a re-render closes it again),
    // so find the link by its label and trigger the download directly
    const file = await captureAttachment(page, async () => {
      const linkWasFound = await page.evaluate(label => {
        const link = Array
          .from(document.querySelectorAll("a"))
          .find(anchor => anchor.textContent.trim() === label)
        if (link) {
          link.click()
        }
        return Boolean(link)
      }, exportLabel)

      if (!linkWasFound) {
        throw new Error(`No export link labeled "${exportLabel}" was found`)
      }
    })
    await fse.writeFile(filePathTemp, file)
  }
  catch (error) {
    await dumpDebugFiles(page, "umsaetze-debug")
    throw error
  }
}


// Extracts the PDFs of a downloaded document archive
// into a temporary directory and returns their paths
// (`unzip` is preinstalled on macOS and Linux, so no dependency is needed)
async function extractPdfs (zipPath) {
  const extractDir = temporaryDirectory()
  await promisify(execFile)("unzip", ["-j", "-q", zipPath, "-d", extractDir])

  const pdfNames = (await fse.readdir(extractDir))
    .filter(name => name.toLowerCase().endsWith(".pdf"))
    .sort()

  if (pdfNames.length === 0) {
    throw new Error(`No PDF in ${zipPath}`)
  }

  return pdfNames.map(name => `${extractDir}/${name}`)
}


// Opens the inbox of the electronic postbox with all entries
// and returns the result of `scan` (an array evaluated in the page).
// Downloading navigates away and invalidates the per-render link tokens,
// so this must be called again before every download.
async function scanPostbox (page, scan) {
  const log = console.warn

  // Right after a download the inbox may still redirect
  // to the busy download page, so retry the scan a few times
  for (let attempt = 1; attempt <= 3; attempt++) {
    await page.goto(
      `${baseUrl}/de/home/onlinebanking/e_postfach/Posteingang.html`,
      {timeout: 30000},
    )
    await page.waitForTimeout(5000)  // Let the page settle

    // By default the inbox only lists 10 entries of the last few months.
    // Option values are randomized, so options are picked by their label.
    for (const label of ["Gesamtzeitraum", "50"]) {
      const wasChanged = await page.evaluate(optionLabel => {
        const select = Array
          .from(document.querySelectorAll("select"))
          .find(sel => Array
            .from(sel.options)
            .some(option => option.textContent.trim() === optionLabel))
        const option = Array
          .from(select?.options ?? [])
          .find(anOption => anOption.textContent.trim() === optionLabel)
        if (!select || !option || select.value === option.value) {
          return false
        }
        select.value = option.value
        select.dispatchEvent(new Event("change", {bubbles: true}))
        return true
      }, label)

      if (wasChanged) {
        log(`Set postbox filter to "${label}"`)
        await page.waitForTimeout(5000)
      }
    }

    const entries = await page.evaluate(scan)
    if (entries.length > 0) {
      return entries
    }
    log(`No documents found on scan attempt ${attempt}, retrying …`)

    // The busy bulk download page keeps the whole session captive
    // until its "Zurück" link is clicked
    await page.evaluate(() => {
      const backLink = Array
        .from(document.querySelectorAll("a"))
        .find(anchor => anchor.textContent.trim() === "Zurück")
      backLink?.click()
    })
    await page.waitForTimeout(3000)
  }

  return []
}


// Clicks the "Herunterladen" link with the `href`
// and returns the paths of the downloaded PDFs in a temporary directory
// (named as the bank names them)
async function downloadPostboxEntry (page, href) {
  const downloadPromise = page.waitForEvent("download", {timeout: 60000})
  // Prevent an unhandled rejection from masking errors of the click
  downloadPromise.catch(() => {})
  await page.evaluate(anHref => {
    Array
      .from(document.querySelectorAll("a"))
      .find(anchor => anchor.href === anHref)
      .click()
  }, href)

  // Documents are usually served as a zip archive
  // (statements contain one PDF, messages may contain several)
  const download = await downloadPromise
  const fileName = path.basename(download.suggestedFilename() || "document")
  let pdfPaths = null
  if (fileName.toLowerCase().endsWith(".zip")) {
    const zipPath = temporaryFile({name: "document.zip"})
    await download.saveAs(zipPath)
    pdfPaths = await extractPdfs(zipPath)
  }
  else {
    pdfPaths = [`${temporaryDirectory()}/${fileName}`]
    await download.saveAs(pdfPaths[0])
  }

  // The "Dokumente speichern" page blocks the session
  // until its process is explicitly cancelled
  await page.click("a.nav-back", {timeout: 15000})
  await page.waitForTimeout(3000)

  return pdfPaths
}


async function downloadPostboxStatements (options = {}) {
  const { page, outputDir } = options
  const log = console.warn

  try {
    log("Go to electronic postbox")
    let downloadCounter = 0

    for (;;) {
      // Associate each "Herunterladen" link
      // with its "Kontoauszug X/YYYY" row
      const documents = await scanPostbox(page, () => Array
        .from(document.querySelectorAll("a"))
        .filter(anchor => anchor.textContent.trim() === "Herunterladen")
        .map(anchor => {
          let container = anchor.parentElement
          const titlePattern =
            /(Kontoauszug\s+(\d+)\/(\d{4}))|(Kreditkartenabrechnung.*?(\d{2})\.(\d{2})\.(\d{4}))/
          while (
            container &&
            container.querySelectorAll("a").length < 10 &&
            !titlePattern.test(container.textContent)
          ) {
            container = container.parentElement
          }
          const hasUniqueDownloadLink = container
            ? Array
              .from(container.querySelectorAll("a"))
              .filter(anc => anc.textContent.trim() === "Herunterladen")
              .length === 1
            : false
          if (!container || !hasUniqueDownloadLink) {
            return null
          }

          const text = container.textContent.replace(/\s+/g, " ")
          const statement = text.match(/Kontoauszug\s+(\d+)\/(\d{4})/)
          if (statement) {
            return {
              name: `${statement[2]}-${
                statement[1].padStart(2, "0")}_mbs`,
              href: anchor.href,
            }
          }

          const card = text.match(
            /Kreditkartenabrechnung.*?(\d{2})\.(\d{2})\.(\d{4})/)
          return card
            ? {
              name: `${card[3]}-${card[2]}-${card[1]}_credit_card_statement`,
              href: anchor.href,
            }
            : null
        })
        .filter(Boolean),
      )

      if (documents.length === 0) {
        throw new Error("No statement documents were found in the postbox")
      }

      if (downloadCounter === 0) {
        log(`Found documents: ${documents.map(doc => doc.name).join(", ")}`)

        if (process.env.NODE_DEBUG) {
          await dumpDebugFiles(page, "postbox-debug")
        }
      }

      // Statements are filed per year, e.g. bank-statements/2026/…
      const toFilePath = doc =>
        `${outputDir}/${doc.name.slice(0, 4)}/${doc.name}.pdf`

      // The postbox reaches back much further than this repository
      const newestFiledMonth = await getNewestFiledMonth(outputDir, "_mbs.pdf")
      if (newestFiledMonth && downloadCounter === 0) {
        log(`Skipping everything up to and including ${newestFiledMonth}`)
      }

      let nextDoc = null
      for (const doc of documents) {
        if (newestFiledMonth && doc.name.slice(0, 7) <= newestFiledMonth) {
          continue
        }
        if (!await fse.pathExists(toFilePath(doc))) {
          nextDoc = doc
          break
        }
      }
      if (!nextDoc) {
        break
      }

      const filePath = toFilePath(nextDoc)
      await fse.ensureDir(`${outputDir}/${nextDoc.name.slice(0, 4)}`)
      log(`Download ${filePath}`)
      const pdfPaths = await downloadPostboxEntry(page, nextDoc.href)
      if (pdfPaths.length !== 1) {
        throw new Error(`Expected exactly one PDF for ${nextDoc.name}, ` +
          `got ${pdfPaths.length}`)
      }
      await fse.move(pdfPaths[0], filePath, {overwrite: true})

      downloadCounter += 1
    }

    log(`Downloaded ${downloadCounter} new documents`)
  }
  catch (error) {
    await dumpDebugFiles(page, "umsaetze-debug")
    throw error
  }
}


// Every entry of the postbox (statements, "Informationen/Dokumente", …)
// as `{text, date, href}`, with `date` as "YYYY-MM-DD" (null if not found)
function scanPostboxEntries () {
  const isDownloadLink = anchor =>
    anchor.textContent.trim() === "Herunterladen"
  const countDownloadLinks = element => Array
    .from(element.querySelectorAll("a"))
    .filter(isDownloadLink)
    .length

  return Array
    .from(document.querySelectorAll("a"))
    .filter(isDownloadLink)
    .map(anchor => {
      // The entry is the largest element with only this download link
      let entry = anchor
      while (
        entry.parentElement &&
        countDownloadLinks(entry.parentElement) === 1
      ) {
        entry = entry.parentElement
      }
      const text = entry.textContent.replace(/\s+/g, " ").trim()
      // The first date of an entry is the date it was posted
      const date = text.match(/(\d{2})\.(\d{2})\.(\d{4})/)
      return {
        text,
        date: date ? `${date[3]}-${date[2]}-${date[1]}` : null,
        href: anchor.href,
      }
    })
}


// Downloads all documents posted since `startDate` (YYYY-MM-DD)
// which are not yet in `outputDir`, with the names the bank gives them,
// and prints their paths to stdout
async function downloadPostboxDocuments (options = {}) {
  const { page, outputDir, startDate } = options
  const log = console.warn

  try {
    log("Go to electronic postbox")
    await fse.ensureDir(outputDir)

    // Entries are identified by their text (and position among entries
    // with the same text), as the links change on each scan
    const handledKeys = new Set()
    let downloadCounter = 0

    for (;;) {
      const occurrences = new Map()
      const entries = (await scanPostbox(page, scanPostboxEntries))
        .map(entry => {
          const occurrence = (occurrences.get(entry.text) ?? 0) + 1
          occurrences.set(entry.text, occurrence)
          return {...entry, key: `${entry.text}#${occurrence}`}
        })
      if (handledKeys.size === 0) {
        if (entries.length === 0) {
          throw new Error("No documents were found in the postbox")
        }
        if (process.env.NODE_DEBUG) {
          await dumpDebugFiles(page, "postbox-debug")
        }
        for (const entry of entries) {
          if (!entry.date) {
            log(`No date found in entry "${entry.text}"`)
          }
        }
      }

      const nextEntry = entries.find(entry =>
        !handledKeys.has(entry.key) &&
        (!entry.date || entry.date >= startDate))
      if (!nextEntry) {
        break
      }
      handledKeys.add(nextEntry.key)
      if (!nextEntry.date) {
        continue
      }

      log(`Download "${nextEntry.text}"`)
      for (const pdfPath of await downloadPostboxEntry(page, nextEntry.href)) {
        const filePath = path.join(outputDir, path.basename(pdfPath))
        if (await fse.pathExists(filePath)) {
          log(`Skip ${filePath} (already downloaded)`)
          continue
        }
        await fse.move(pdfPath, filePath)
        console.info(filePath)
        downloadCounter += 1
      }
    }

    log(`Downloaded ${downloadCounter} new documents`)
  }
  catch (error) {
    await dumpDebugFiles(page, "postbox-debug")
    throw error
  }
}


async function getTransactions (options = {}) {
  const daysAgo = new Date()
  daysAgo.setDate(daysAgo.getDate() - options.numberOfDays)
  const {
    startDate = daysAgo,
    endDate = new Date(),
    username,
    password,
    shallShowBrowser = true,
  } = options

  const filePathTemp = temporaryFile({name: "transactions.csv"})
  const log = console.warn

  const {browser, page} = await launchBrowser({shallShowBrowser})

  try {
    const url = `${baseUrl}/de/home/login-online-banking.html`
    log(`Open ${url}`)
    await page.goto(url, {timeout: 30000})

    try {
      await page.click(
        ".ebutton a[data-form='.eprivacy_optin_decline']",
        {timeout: 5000},
      )
      log("Dismissed cookie consent banner")
    }
    catch {
      log("No cookie consent banner appeared")
    }

    if (username && password) {
      log("Enter username")
      await page.fill(
        "input[autocomplete=username]", username, {timeout: 15000})

      // The password field is either revealed on the same page
      // or on a follow-up page after submitting the username
      const passwordSelector = "input[type=password]"
      if (!await page.isVisible(passwordSelector)) {
        await page.click("input[type=submit]", {timeout: 15000})
        await page.waitForSelector(passwordSelector, {timeout: 30000})
      }

      log("Enter password")
      await page.fill(passwordSelector, password, {timeout: 15000})
      await page.click("input[type=submit]", {timeout: 15000})
    }
    else {
      log("Please log in manually in the browser …")
    }

    log("Wait for login (confirm the pushTAN prompt if necessary) …")
    try {
      await page.waitForSelector("text=Abmelden", {timeout: 600000})
    }
    catch (error) {
      await dumpDebugFiles(page, "umsaetze-debug")
      throw error
    }


    if (process.argv[2] === "postbox") {
      await downloadPostboxStatements({
        page,
        outputDir: process.argv[3] || ".",
      })
      return
    }

    if (process.argv[2] === "documents") {
      const fromIndex = process.argv.indexOf("from")
      const fromDate = fromIndex > -1
        ? process.argv[fromIndex + 1]
        : new Date(Date.now() - 90 * 24 * 60 * 60 * 1000)
          .toISOString()
          .slice(0, 10)
      if (!/^\d{4}-\d{2}-\d{2}$/.test(fromDate ?? "")) {
        throw new Error(`Date must be formatted as YYYY-MM-DD: "${fromDate}"`)
      }
      await downloadPostboxDocuments({
        page,
        outputDir: process.argv[3] || ".",
        startDate: fromDate,
      })
      return
    }

    log("Go to transactions page")
    try {
      await page.goto(
        `${baseUrl}/de/home/onlinebanking/umsaetze/umsaetze.html`,
        {timeout: 30000},
      )
      await page.waitForSelector(
        "button:has-text('Exportieren'):visible",
        {timeout: 30000},
      )
    }
    catch (error) {
      await dumpDebugFiles(page, "umsaetze-debug")
      throw error
    }


    if (process.argv[2] === "from") {
      // Raw CSV export from the given date until today
      await downloadRange({
        page,
        filePathTemp,
        startDate: new Date(`${process.argv[3]}T00:00:00Z`),
        endDate: new Date(),
      })
      process.stdout.write(await fse.readFile(filePathTemp))
    }
    else if (process.argv[2] === "MT940") {
      // Optional month argument (YYYY-MM), defaults to the previous month
      const monthArg = process.argv[3]
      let firstDay = null
      let lastDay = null

      if (monthArg) {
        if (!/^\d{4}-\d{2}$/.test(monthArg)) {
          throw new Error(`Month must be formatted as YYYY-MM: "${monthArg}"`)
        }
        const [year, month] = monthArg.split("-").map(Number)
        firstDay = new Date(Date.UTC(year, month - 1, 1))
        lastDay = new Date(Date.UTC(year, month, 0))
      }
      else {
        lastDay = new Date(
          new Date()
            .setUTCDate(0),
        )
        firstDay = new Date(
          new Date(lastDay)
            .setUTCDate(1),
        )
      }

      await downloadRange({
        page,
        filePathTemp,
        exportLabel: "Text (MT940)",
        startDate: firstDay,
        endDate: lastDay,
      })

      console.info(await fse.readFile(filePathTemp, "utf-8"))
    }
    else {
      // Export the default range ("3 Monate") shown after opening the page
      await downloadRange({
        page,
        filePathTemp,
      })
      await normalizeAndPrint(filePathTemp)
    }
  }
  finally {
    await browser.close()
  }
}


async function main () {
  const answers = process.env.SPARKASSE_URL
    ? await getCredentials("SPARKASSE", "Sparkasse")
    : await getCredentials("MBS", "MBS")

  try {
    await getTransactions({
      username: answers.username,
      password: answers.password,
      shallShowBrowser: true,
      numberOfDays: 90,  // More than 90 days trigger a tan prompt
      // startDate: new Date('2016-01-01'),
    })
  }
  catch (error) {
    console.error(error)
    process.exitCode = 1
  }
}

main()
