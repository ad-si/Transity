// Prints the total balance of the FNZ Bank (formerly ebase / Finvesto) depot
// from https://portal.fnz.de
//
// Environment: FNZ_USERNAME (Zugangs-ID), FNZ_PASSWORD (PIN)
// (else prompted for, or manual login)

import {pathToFileURL} from "node:url"

import {prettyPrint} from "../helpers.js"
import {dumpDebugFiles, getCredentials, launchBrowser} from "../browser.js"
import {login} from "../documents/fnz.js"


const log = process.env.NODE_DEBUG
  ? console.warn
  : () => {}


// The "Gesamtbestand" of all depots on the start page,
// e.g. `<span class="sum">50.273,<i>93</i> €</span>`
export async function readBalance (page) {
  log("Retrieve current balance")
  const balanceSelector = ".depot-data-head .sum"
  await page.waitForSelector(balanceSelector, {timeout: 30000})
  const balance = await page.evaluate(
    selector => document
      .querySelector(selector)
      .textContent
      .replace(/[€\s]|EUR/g, "")
      .replace(/\./g, "")
      .replace(/,/g, "."),
    balanceSelector,
  )
  return balance + " €"
}


async function getBalance (options = {}) {
  const {
    username,
    password,
    isDevMode = false,
    shallShowBrowser = true,
  } = options

  if (isDevMode) return "1234.56 €"

  const {browser, page} = await launchBrowser({
    shallShowBrowser,
    persistentProfileName: "fnz",
  })

  try {
    await login(page, {username, password})
    return await readBalance(page)
  }
  catch (error) {
    await dumpDebugFiles(page, "finvesto-debug")
    throw error
  }
  finally {
    await browser.close()
  }
}


async function main () {
  try {
    const credentials = await getCredentials("FNZ", "FNZ Bank")
    const balance = await getBalance(credentials)
    prettyPrint("Finvesto", balance)
    process.exit(0)
  }
  catch (error) {
    console.error(error)
    process.exit(1)
  }
}


// Allows importing the functions, e.g. to test them in a running browser
if (
  process.argv[1] &&
  import.meta.url === pathToFileURL(process.argv[1]).href
) {
  main()
}
