// Downloads the transactions CSV ("Umsatzliste") of the HypoVereinsbank
// giro account from https://my.hypovereinsbank.de and writes it to stdout
// unchanged (UTF-16LE, as exported by the bank).
//
// Usage:
//   node hypovereinsbank.js [from <YYYY-MM-DD>] [to <YYYY-MM-DD>]
//   (default: the last 90 days until today)
//
// Environment: HYPOVEREINSBANK_USERNAME (Direct Banking Nummer),
// HYPOVEREINSBANK_PASSWORD (else manual login)
//
// The login has to be confirmed in the HVB app (SCA).

import {
  captureAttachment,
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
    if (!username || !password) {
      throw new Error("No credentials configured")
    }
    await page.fill("#username", username, {timeout: 15000})
    await page.fill("#px2", password, {timeout: 15000})
    await page.click("#loginCommandButton", {timeout: 15000})
  }
  catch (error) {
    log(`Automated login stopped (${error.message.split("\n")[0]})`)
    log("Please complete the login manually in the browser …")
  }

  log("Wait for the login (confirm it in the HVB app) …")
  await page.waitForURL(/finanzstatus\.jsp/, {timeout: 600000})
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


main()
