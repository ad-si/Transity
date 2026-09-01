import { execFile } from "node:child_process"
import { promisify } from "node:util"

import fse from "fs-extra"

import converter from "converter"
import inquirer from "inquirer"
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
  dumpDebugFiles,
  getNewestFiledMonth,
  launchBrowser,
} from "../browser.js"

const prompt = inquirer.createPromptModule({ output: process.stderr })


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
        apply: IF.get("filterAnwendenButton"),
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
        }

        document.getElementById(opts.apply).click()
      }, {...ids, vonDate, bisDate})

      // Ranges reaching back more than 90 days may require a TAN approval
      log("Wait for filtered list (confirm the pushTAN prompt if necessary) …")
      await page.waitForSelector(
        `.umsatzanzahl:has-text("${bisDate}")`,
        {timeout: 180000},
      )
    }

    log(`Download "${exportLabel}" file`)
    const downloadPromise = page.waitForEvent("download", {timeout: 60000})
    // Prevent an unhandled rejection from masking errors of the steps below
    downloadPromise.catch(() => {})

    await page.waitForTimeout(2000)  // Let the page settle after re-renders

    // The export links exist in the DOM even while their menu is closed
    // (and clicking through the menu fails when a re-render closes it again),
    // so find the link by its label and trigger the download directly
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

    const download = await downloadPromise
    await download.saveAs(filePathTemp)
  }
  catch (error) {
    await dumpDebugFiles(page, "umsaetze-debug")
    throw error
  }
}


// Extracts the single PDF of a downloaded document archive
// (`unzip` is preinstalled on macOS and Linux, so no dependency is needed)
async function extractSinglePdf (zipPath, targetPath) {
  const extractDir = temporaryDirectory()
  await promisify(execFile)("unzip", ["-j", "-q", zipPath, "-d", extractDir])

  const pdfNames = (await fse.readdir(extractDir))
    .filter(name => name.toLowerCase().endsWith(".pdf"))

  if (pdfNames.length !== 1) {
    throw new Error(
      `Expected exactly one PDF in ${zipPath}, got ${pdfNames.length}`)
  }

  await fse.move(`${extractDir}/${pdfNames[0]}`, targetPath, {overwrite: true})
}


async function downloadPostboxDocuments (options = {}) {
  const { page, outputDir } = options
  const log = console.warn

  try {
    log("Go to electronic postbox")
    let downloadCounter = 0

    // Downloading navigates away and invalidates the per-render link tokens,
    // so re-open the inbox and re-scan the list before every download
    for (;;) {
      // Right after a download the inbox may still redirect
      // to the busy download page, so retry the scan a few times
      let documents = []
      for (let attempt = 1; attempt <= 3; attempt++) {
        await page.goto(
          "https://www.mbs.de/de/home/onlinebanking" +
            "/e_postfach/Posteingang.html",
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

        // Associate each "Herunterladen" link
        // with its "Kontoauszug X/YYYY" row
        documents = await page.evaluate(() => Array
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

        if (documents.length > 0) {
          break
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

      if (documents.length === 0) {
        throw new Error("No statement documents were found in the postbox")
      }

      if (downloadCounter === 0) {
        log(`Found documents: ${documents.map(doc => doc.name).join(", ")}`)

        if (process.env.NODE_DEBUG) {
          const stats = await page.evaluate(() => ({
            downloadLinks: Array
              .from(document.querySelectorAll("a"))
              .filter(anchor => anchor.textContent.trim() === "Herunterladen")
              .length,
            pageSize: Array
              .from(document.querySelectorAll("select"))
              .find(sel => Array
                .from(sel.options)
                .some(option => option.textContent.trim() === "50"))
              ?.value,
            titles: Array
              .from(document.querySelectorAll("h3, h4, .mkp-headline-05"))
              .map(element => element.textContent.replace(/\s+/g, " ").trim())
              .filter(Boolean)
              .slice(0, 60),
          }))
          log(JSON.stringify(stats, null, 2))
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
      const downloadPromise = page.waitForEvent("download", {timeout: 60000})
      // Prevent an unhandled rejection from masking errors of the click
      downloadPromise.catch(() => {})
      await page.evaluate(href => {
        Array
          .from(document.querySelectorAll("a"))
          .find(anchor => anchor.href === href)
          .click()
      }, nextDoc.href)

      // Documents are served as a zip archive containing a single PDF
      const download = await downloadPromise
      const zipPath = temporaryFile({name: "statement.zip"})
      await download.saveAs(zipPath)
      await extractSinglePdf(zipPath, filePath)

      // The "Dokumente speichern" page blocks the session
      // until its process is explicitly cancelled
      await page.click("a.nav-back", {timeout: 15000})
      await page.waitForTimeout(3000)

      downloadCounter += 1
    }

    log(`Downloaded ${downloadCounter} new documents`)
  }
  catch (error) {
    await dumpDebugFiles(page, "umsaetze-debug")
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

  const baseUrl = "https://www.mbs.de"
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

    log("Enter username")
    await page.fill("input[autocomplete=username]", username, {timeout: 15000})

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

    log("Wait for login (confirm the pushTAN prompt if necessary) …")
    try {
      await page.waitForSelector("text=Abmelden", {timeout: 180000})
    }
    catch (error) {
      await dumpDebugFiles(page, "umsaetze-debug")
      throw error
    }


    if (process.argv[2] === "postbox") {
      await downloadPostboxDocuments({
        page,
        outputDir: process.argv[3] || ".",
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


    if (process.argv[2] === "MT940") {
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
  try {
    // Load credentials from a .env file in the working directory if present
    process.loadEnvFile()
  }
  catch { /* No .env file available */ }

  let answers = {
    username: process.env.MBS_USERNAME,
    password: process.env.MBS_PASSWORD,
  }

  if (!answers.username || !answers.password) {
    const promptValues = [
      {
        type: "input",
        name: "username",
        message: "MBS Username:",
      },
      {
        type: "password",
        name: "password",
        message: "MBS Password:",
      },
    ]
    answers = await prompt(promptValues)
  }

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
