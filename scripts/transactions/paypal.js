// Downloads the PayPal activity CSV
// (balance affecting transactions since the last download)
// from https://www.paypal.com/reports/dlog and prints it to stdout.
//
// Usage:
//   node paypal.js                Create and download a new report
//   node paypal.js existing [n]   Download the nth existing report (default 1)
//   node paypal.js from <date> [to <date>]
//                                 Create a report from <date> (YYYY-MM-DD)
//                                 until today (or the "to" date)
//                                 instead of "since last download".
//                                 (PayPal rejects too long ranges)
//   node paypal.js statements [dir]  Download missing monthly statements
//
// Environment: PAYPAL_USERNAME, PAYPAL_PASSWORD (else manual login),
// PAYPAL_PROFILE (browser profile name, default "paypal")

import fse from "fs-extra"
import { temporaryFile } from "tempy"

import {
  dumpDebugFiles,
  getCredentials,
  getNewestFiledMonth,
  launchBrowser,
} from "../browser.js"

const log = console.warn


// "D/M/YYYY" or "M/D/YYYY" as used by the report form's date fields
function formatFormDate (date, isDayFirst) {
  const day = date.getDate()
  const month = date.getMonth() + 1
  return isDayFirst
    ? `${day}/${month}/${date.getFullYear()}`
    : `${month}/${day}/${date.getFullYear()}`
}


// "Oct 2, 2026" as shown in the report list
function toListDate (date) {
  return date.toLocaleDateString("en-US", {
    month: "short",
    day: "numeric",
    year: "numeric",
  })
}


async function createAndDownloadReport (options = {}) {
  const {
    page,
    filePathTemp,
    existingRowNumber = null,
    startDate = null,
    endDate = null,
  } = options

  try {
    if (!existingRowNumber) {
      // The form defaults are already correct:
      // type "Balance affecting", range "Since last download", format "CSV"
      if (startDate) {
        await page.click("#text-input-undefined", {timeout: 30000})
        // The "From"/"To" fields use the account's locale ("M/D/YYYY" or
        // "D/M/YYYY"). "To" is prefilled with today, which reveals the order.
        const today = new Date()
        const endValue = await page.inputValue("#end")
        const isDayFirst = endValue ===
          `${today.getDate()}/${today.getMonth() + 1}/${today.getFullYear()}`
        const end = endDate ?? today
        log(`Set date range ${formatFormDate(startDate, isDayFirst)} - ${
          formatFormDate(end, isDayFirst)}`)
        for (const [selector, date] of [
          ["#start", startDate],
          ["#end", end],
        ]) {
          await page.click(selector)
          await page.keyboard.press("Meta+A")
          await page.keyboard.type(
            formatFormDate(date, isDayFirst), {delay: 40})
          await page.keyboard.press("Tab")
          await page.waitForTimeout(800)
        }
      }
      log("Create activity report")
      await page.click("[data-testid=ActivityCreateReport]", {timeout: 30000})
    }

    // Read the report from the API response instead of the download it
    // triggers, as Chromium crashes (SIGSEGV) when Playwright handles it
    const responsePromise = page.waitForResponse(
      response => response.url()
        .includes("/reports/apis/common/ql") &&
        /attachment/.test(response.headers()["content-disposition"] || ""),
      {timeout: 600000},
    )
    // Prevent an unhandled rejection from masking errors of the steps below
    responsePromise.catch(() => {})

    // The list also contains download buttons of previously created reports,
    // so the new report must be picked by its request date (today)
    const expectedRequestDate = toListDate(new Date())
    // Reports created earlier today with another range must not be picked
    const expectedRangeStart = startDate ? toListDate(startDate) : null

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
            aRow.cells[1].textContent.trim() === opts.expectedRequestDate &&
            (!opts.expectedRangeStart || aRow.cells[2].textContent.trim()
              .startsWith(opts.expectedRangeStart)))

        if (!row) {
          return "Report does not show up in the list yet"
        }

        const button = row.cells[4].querySelector("button")
        if (button && button.textContent.trim() === "Download") {
          button.click()
          return "downloading"
        }

        return `Report status is "${row.cells[4].textContent.trim()}"`
      }, {expectedRequestDate, expectedRangeStart, existingRowNumber})

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
    const response = await responsePromise
    await fse.writeFile(filePathTemp, await response.body())
  }
  catch (error) {
    await dumpDebugFiles(page, "paypal-debug")
    throw error
  }
}


// Lists the whole-month statements of the statements page.
// Rows show ranges as "5/1/26 - 5/31/26" or "5/1/26 - 6/1/26".
// Cell texts run together without whitespace, so no anchors can be used.
function listStatements (page) {
  return page.evaluate(() => Array
    .from(document.querySelectorAll("tr"))
    .map((row, rowIndex) => {
      const text = row.textContent.replace(/\s+/g, " ")
      const match = text
        .match(/(\d{1,2})\/1\/(\d{2}) - \d{1,2}\/\d{1,2}\/\d{2}/)
      const isReady = Array
        .from(row.querySelectorAll("a, button"))
        .some(element => element.textContent.trim() === "Download")
      return match
        ? {
          month: `20${match[2]}-${match[1].padStart(2, "0")}`,
          rowIndex,
          isReady,
        }
        : null
    })
    .filter(Boolean),
  )
}


// Business accounts get monthly statements automatically,
// personal accounts only on demand ("Create Report" with a custom range)
async function createStatement (page, month) {
  const [year, monthNumber] = month.split("-")
    .map(Number)
  const lastDay = new Date(Date.UTC(year, monthNumber, 0))
    .getUTCDate()
  const mm = String(monthNumber)
    .padStart(2, "0")

  log(`Create statement ${month}`)
  await page.click("[data-testid=btn__generateReport]")
  await page.click("[data-testid=dropdown_fileFormat]")
  await page.getByRole("option", {name: "PDF"})
    .click()
  await page.click("[data-testid=tableColumnDateRange]")
  await page.getByRole("option", {name: "Custom"})
    .click()
  for (const [selector, value] of [
    ["#text-input-rangeStart", `${mm}/01/${year}`],
    ["#text-input-rangeEnd", `${mm}/${lastDay}/${year}`],
  ]) {
    await page.click(selector)
    await page.keyboard.press("Meta+A")
    await page.keyboard.type(value, {delay: 30})
    await page.keyboard.press("Tab")
    await page.waitForTimeout(400)
  }
  await page.click("[data-testid=btn__createReport]")
  await page.waitForTimeout(3000)
}


function addMonths (month, count) {
  const date = new Date(`${month}-01T00:00:00Z`)
  date.setUTCMonth(date.getUTCMonth() + count)
  return date.toISOString()
    .slice(0, 7)
}


async function downloadStatements (options = {}) {
  const { page, outputDir } = options
  const statementsUrl = "https://www.paypal.com/reports/accountStatements"

  try {
    log("Go to monthly statements")
    // The report overview renders differently per account type and its
    // "Download Report" links have no href, so go to the list directly
    await page.goto(statementsUrl, {timeout: 60000})
    await page.waitForTimeout(10000)  // Let the page settle

    let statements = await listStatements(page)
    log(`Found ${statements.length} statements`)

    // Only fetch statements newer than the newest already filed one,
    // as the online archive reaches back much further than this repo
    const newestFiledMonth = await getNewestFiledMonth(outputDir, "_paypal.pdf")
    if (newestFiledMonth) {
      log(`Skipping everything up to and including ${newestFiledMonth}`)
    }
    const lastCompleteMonth = addMonths(new Date()
      .toISOString()
      .slice(0, 7), -1)
    const wantedMonths = []
    for (
      let month = newestFiledMonth
        ? addMonths(newestFiledMonth, 1)
        : statements.map(stmt => stmt.month)
          .sort()[0] ?? lastCompleteMonth;
      month <= lastCompleteMonth;
      month = addMonths(month, 1)
    ) {
      wantedMonths.push(month)
    }

    for (const month of wantedMonths) {
      if (!statements.some(stmt => stmt.month === month)) {
        await createStatement(page, month)
      }
    }

    // Wait until all wanted statements are generated
    for (let tryNumber = 1; tryNumber <= 60; tryNumber++) {
      await page.goto(statementsUrl, {timeout: 60000})
      await page.waitForTimeout(10000)
      const currentStatements = await listStatements(page)
      statements = currentStatements
      const pending = wantedMonths.filter(month => !currentStatements
        .some(stmt => stmt.month === month && stmt.isReady))
      if (pending.length === 0) {
        break
      }
      log(`Waiting for ${pending.length} statements to be generated …`)
      await page.waitForTimeout(20000)
    }

    let downloadCounter = 0
    for (const month of wantedMonths) {
      const statement = statements
        .find(stmt => stmt.month === month && stmt.isReady)
      if (!statement) {
        log(`Statement ${month} is not available`)
        continue
      }
      // Statements are filed per year, e.g. bank-statements/2026/…
      const filePath = `${outputDir}/${month.slice(0, 4)}/${month}_paypal.pdf`
      if (await fse.pathExists(filePath)) {
        continue
      }
      await fse.ensureDir(`${outputDir}/${month.slice(0, 4)}`)

      log(`Download ${filePath}`)
      // Read the PDF from the API response instead of the download it
      // triggers, as Chromium crashes (SIGSEGV) when Playwright handles it
      const responsePromise = page.waitForResponse(
        response => response.url()
          .includes("/reports/apis/rux/report/download"),
        {timeout: 120000},
      )
      // Prevent an unhandled rejection from masking errors of the click
      responsePromise.catch(() => {})
      await page.evaluate(rowIndex => {
        Array
          .from(document.querySelectorAll("tr")[rowIndex]
            .querySelectorAll("a, button"))
          .find(element => element.textContent.trim() === "Download")
          .click()
      }, statement.rowIndex)
      const response = await responsePromise
      await fse.writeFile(filePath, await response.body())
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


// After the password login PayPal may ask how to confirm it
// (text message, PayPal app, …). Choose the push notification of the app.
async function chooseAppConfirmation (page) {
  const appOption = page
    .locator("label, button, [role=radio], [role=button], li")
    .filter({hasText: /PayPal[- ]?App/i})
    .first()

  try {
    await appOption.waitFor({state: "visible", timeout: 20000})
  }
  catch {
    if (!page.url().includes("/reports/dlog")) {
      log("No PayPal app option found for the confirmation")
      await dumpDebugFiles(page, "paypal-2fa")
    }
    return
  }

  log("Choose confirmation with the PayPal app")
  await appOption.click()
  await page.waitForTimeout(1000)

  const submitButton = page
    .locator("button[type=submit], button")
    .filter({hasText: /^\s*(Weiter|Next|Continue|Senden|Send)\s*$/i})
    .first()
  if (await submitButton.isVisible()) {
    await submitButton.click()
  }
  log("Confirm the login in the PayPal app …")
}


async function getActivity (options = {}) {
  const {
    username,
    password,
    existingRowNumber,
    startDate,
    endDate,
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
    // Separate profiles keep the sessions of several PayPal accounts
    // (e.g. personal and business) apart
    persistentProfileName: process.env.PAYPAL_PROFILE || "paypal",
    // Reports are read from the network responses instead
    acceptDownloads: false,
  })

  try {
    log(`Open ${loginUrl}`)
    await page.goto(loginUrl, {timeout: 30000})

    // Depending on the remembered session PayPal shows only some of these
    // steps, so each one is applied only if its element is actually present
    try {
      await page.waitForTimeout(3000)

      if (await page.isVisible("#email")) {
        if (username) {
          log("Enter email")
          await page.fill("#email", username, {timeout: 30000})
        }
        else {
          // Chromium hides autofilled values from the page until the user
          // interacts with it, but they match `:autofill`
          log("No username configured, wait for the browser to autofill it")
          await page.waitForSelector("#email:autofill", {timeout: 10000})
        }
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
            document.getElementById("logIn_tryAnotherWay")
              ?.click()
          })
          await page.waitForTimeout(3000)
          await page.evaluate(() => {
            document.getElementById("loginWithPassword")
              ?.click()
          })
          await page.waitForSelector("#password", {timeout: 30000})
        }

        if (password) {
          log("Enter password")
          await page.fill("#password", password, {timeout: 30000})
        }
        else {
          log("No password configured, wait for the browser to autofill it")
          await page.waitForSelector("#password:autofill", {timeout: 10000})
        }
        await page.click("#btnLogin", {timeout: 15000})
        await chooseAppConfirmation(page)
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

    await createAndDownloadReport({
      page,
      filePathTemp,
      existingRowNumber,
      startDate,
      endDate,
    })

    // Trailing blank lines break the CSV parser of the YAML converter
    console.info((await fse.readFile(filePathTemp, "utf-8")).trimEnd())
  }
  finally {
    await browser.close()
  }
}


async function main () {
  const answers = await getCredentials("PAYPAL", "PayPal")

  const existingRowNumber = process.argv[2] === "existing"
    ? Number(process.argv[3] || 1)
    : null
  const startDate = process.argv[2] === "from"
    ? new Date(`${process.argv[3]}T00:00:00`)
    : null
  const endDate = startDate && process.argv[4] === "to"
    ? new Date(`${process.argv[5]}T00:00:00`)
    : null
  for (const date of [startDate, endDate]) {
    if (date && isNaN(date)) {
      console.error(`Invalid date in "${process.argv.slice(2).join(" ")}", ` +
        "expected: from <YYYY-MM-DD> [to <YYYY-MM-DD>]")
      process.exit(1)
    }
  }
  const statementsDir = process.argv[2] === "statements"
    ? process.argv[3] || "."
    : null

  try {
    await getActivity({
      username: answers.username,
      password: answers.password,
      existingRowNumber,
      startDate,
      endDate,
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
