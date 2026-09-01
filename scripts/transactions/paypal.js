// Downloads the PayPal activity CSV
// (balance affecting transactions since the last download)
// from https://www.paypal.com/reports/dlog and prints it to stdout.
//
// Usage:
//   node paypal.js                Create and download a new report
//   node paypal.js existing [n]   Download the nth existing report (default 1)

import fse from "fs-extra"
import inquirer from "inquirer"
import { temporaryFile } from "tempy"

import {
  dumpDebugFiles,
  getNewestFiledMonth,
  launchBrowser,
} from "../browser.js"

const prompt = inquirer.createPromptModule({ output: process.stderr })
const log = console.warn


async function createAndDownloadReport (options = {}) {
  const { page, filePathTemp, existingRowNumber = null } = options

  try {
    if (!existingRowNumber) {
      // The form defaults are already correct:
      // type "Balance affecting", range "Since last download", format "CSV"
      log("Create activity report")
      await page.click("[data-testid=ActivityCreateReport]", {timeout: 30000})
    }

    const downloadPromise = page.waitForEvent("download", {timeout: 600000})
    // Prevent an unhandled rejection from masking errors of the steps below
    downloadPromise.catch(() => {})

    // The list also contains download buttons of previously created reports,
    // so the new report must be picked by its request date (today)
    const expectedRequestDate = new Date()
      .toLocaleDateString("en-US", {
        month: "short",
        day: "numeric",
        year: "numeric",
      })

    log("Wait for report generation …")
    let downloadWasClicked = false

    for (let tryNumber = 1; tryNumber <= 60; tryNumber++) {
      const status = await page.evaluate(opts => {
        const rows = Array
          .from(document.querySelectorAll("[data-testid=tablebody] tr"))
          .filter(row => row.cells.length >= 5)
        const row = opts.existingRowNumber
          ? rows[opts.existingRowNumber - 1]
          : rows.find(aRow =>  // Newest report comes first
            aRow.cells[1].textContent.trim() === opts.expectedRequestDate)

        if (!row) {
          return "Report does not show up in the list yet"
        }

        const button = row.cells[4].querySelector("button")
        if (button && button.textContent.trim() === "Download") {
          button.click()
          return "downloading"
        }

        return `Report status is "${row.cells[4].textContent.trim()}"`
      }, {expectedRequestDate, existingRowNumber})

      if (status === "downloading") {
        downloadWasClicked = true
        break
      }

      log(`${status} (attempt ${tryNumber}) …`)
      await page.evaluate(() => {
        document
          .querySelector("[data-testid=table-refresh-button] button")
          ?.click()
      })
      await page.waitForTimeout(5000)
    }

    if (!downloadWasClicked) {
      throw new Error("The report never became downloadable")
    }

    log("Download report file")
    const download = await downloadPromise
    await download.saveAs(filePathTemp)
  }
  catch (error) {
    await dumpDebugFiles(page, "paypal-debug")
    throw error
  }
}


async function downloadStatements (options = {}) {
  const { page, outputDir } = options

  try {
    log("Go to monthly statements")
    // The report overview renders differently per account type and its
    // "Download Report" links have no href, so go to the list directly
    await page.goto(
      "https://www.paypal.com/reports/accountStatements",
      {timeout: 60000},
    )
    await page.waitForTimeout(10000)  // Let the page settle

    // Rows list whole-month ranges as "5/1/26 - 5/31/26" or "5/1/26 - 6/1/26".
    // Cell texts run together without whitespace, so no anchors can be used.
    const statements = await page.evaluate(() => Array
      .from(document.querySelectorAll("tr"))
      .map((row, rowIndex) => {
        const text = row.textContent.replace(/\s+/g, " ")
        const match = text.match(/(\d{1,2})\/1\/(\d{2}) - \d{1,2}\/\d{1,2}\/\d{2}/)
        const hasDownload = Array
          .from(row.querySelectorAll("a, button"))
          .some(element => element.textContent.trim() === "Download")
        return match && hasDownload
          ? {
            month: `20${match[2]}-${match[1].padStart(2, "0")}`,
            rowIndex,
          }
          : null
      })
      .filter(Boolean),
    )

    log(`Found ${statements.length} statements, newest: ${
      statements.slice(0, 3).map(stmt => stmt.month).join(", ") || "none"}`)

    if (statements.length === 0) {
      await dumpDebugFiles(page, "paypal-statements-debug")
    }

    // Only fetch statements newer than the newest already filed one,
    // as the online archive reaches back much further than this repo
    const newestFiledMonth = await getNewestFiledMonth(outputDir, "_paypal.pdf")
    if (newestFiledMonth) {
      log(`Skipping everything up to and including ${newestFiledMonth}`)
    }
    const missingStatements = statements
      .filter(stmt => !newestFiledMonth || stmt.month > newestFiledMonth)

    let downloadCounter = 0
    for (const statement of missingStatements) {
      // Statements are filed per year, e.g. bank-statements/2026/…
      const filePath = `${outputDir}/${statement.month.slice(0, 4)}` +
        `/${statement.month}_paypal.pdf`
      if (await fse.pathExists(filePath)) {
        continue
      }
      await fse.ensureDir(`${outputDir}/${statement.month.slice(0, 4)}`)

      log(`Download ${filePath}`)
      const downloadPromise = page.waitForEvent("download", {timeout: 120000})
      // Prevent an unhandled rejection from masking errors of the click
      downloadPromise.catch(() => {})
      await page.evaluate(rowIndex => {
        Array
          .from(document.querySelectorAll("tr")[rowIndex]
            .querySelectorAll("a, button"))
          .find(element => element.textContent.trim() === "Download")
          .click()
      }, statement.rowIndex)
      const download = await downloadPromise
      await download.saveAs(filePath)
      downloadCounter += 1
      await page.waitForTimeout(2000)
    }

    log(`Downloaded ${downloadCounter} new statements`)
  }
  catch (error) {
    await dumpDebugFiles(page, "paypal-statements-debug")
    throw error
  }
}


async function getActivity (options = {}) {
  const {
    username,
    password,
    existingRowNumber,
    statementsDir = null,
    shallShowBrowser = true,
  } = options

  const baseUrl = "https://www.paypal.com"
  const reportUrl = `${baseUrl}/reports/dlog`
  const loginUrl = `${baseUrl}/signin?returnUri=${
    encodeURIComponent(reportUrl)}`
  const filePathTemp = temporaryFile({name: "paypal-activity.csv"})

  const {browser, page} = await launchBrowser({
    shallShowBrowser,
    persistentProfileName: "paypal",
  })

  try {
    log(`Open ${loginUrl}`)
    await page.goto(loginUrl, {timeout: 30000})

    // Depending on the remembered session PayPal shows only some of these
    // steps, so each one is applied only if its element is actually present
    try {
      await page.waitForTimeout(3000)

      if (await page.isVisible("#email")) {
        log("Enter email")
        await page.fill("#email", username, {timeout: 30000})
        await page.click("#btnNext", {timeout: 15000})
        await page.waitForTimeout(3000)
      }

      // Prefer the passkey prompt: it is confirmed with Touch ID,
      // whereas the password login also requires a one-time code.
      // Set PAYPAL_USE_PASSWORD=1 to force the password login instead.
      const hasPasskeyPrompt = !process.env.PAYPAL_USE_PASSWORD &&
        await page.isVisible("#logIn_start")

      if (hasPasskeyPrompt) {
        log("Confirm the passkey prompt (Touch ID) in the browser …")
        await page.click("#logIn_start", {timeout: 15000})
      }
      else {
        // The password field is hidden behind
        // "Anders bestätigen" > "Mit Passwort einloggen".
        // Both are inert until revealed, so click them via the DOM.
        if (!await page.isVisible("#password")) {
          log("Switch from passkey to password login")
          await page.evaluate(() => {
            document.getElementById("logIn_tryAnotherWay")?.click()
          })
          await page.waitForTimeout(3000)
          await page.evaluate(() => {
            document.getElementById("loginWithPassword")?.click()
          })
          await page.waitForSelector("#password", {timeout: 30000})
        }

        log("Enter password")
        await page.fill("#password", password, {timeout: 30000})
        await page.click("#btnLogin", {timeout: 15000})
      }
    }
    catch (error) {
      log(`Automated login stopped (${error.message.split("\n")[0]})`)
      log("Please complete the login manually in the browser …")
    }

    log("Wait for report page (solve captcha / confirm 2FA if prompted) …")
    try {
      await page.waitForURL("**/reports/dlog**", {timeout: 300000})
      await page.waitForSelector(
        "[data-testid=ActivityCreateReport]",
        {timeout: 60000},
      )
    }
    catch (error) {
      await dumpDebugFiles(page, "paypal-debug")
      throw error
    }

    if (statementsDir) {
      await downloadStatements({page, outputDir: statementsDir})
      return
    }

    await createAndDownloadReport({page, filePathTemp, existingRowNumber})

    // Trailing blank lines break the CSV parser of the YAML converter
    console.info((await fse.readFile(filePathTemp, "utf-8")).trimEnd())
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
    username: process.env.PAYPAL_USERNAME,
    password: process.env.PAYPAL_PASSWORD,
  }

  if (!answers.username || !answers.password) {
    const promptValues = [
      {
        type: "input",
        name: "username",
        message: "PayPal Username:",
      },
      {
        type: "password",
        name: "password",
        message: "PayPal Password:",
      },
    ]
    answers = await prompt(promptValues)
  }

  const existingRowNumber = process.argv[2] === "existing"
    ? Number(process.argv[3] || 1)
    : null
  const statementsDir = process.argv[2] === "statements"
    ? process.argv[3] || "."
    : null

  try {
    await getActivity({
      username: answers.username,
      password: answers.password,
      existingRowNumber,
      statementsDir,
      shallShowBrowser: true,
    })
  }
  catch (error) {
    console.error(error)
    process.exitCode = 1
  }
}

main()
