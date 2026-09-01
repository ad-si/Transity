import assert from "assert"

import inquirer from "inquirer"

import {prettyPrint} from "../helpers.js"
import {launchBrowser} from "../browser.js"


const prompt = inquirer.createPromptModule({ output: process.stderr })
const log = process.env.NODE_DEBUG
  ? console.warn
  : () => {}


async function getBalance (options = {}) {
  const {
    username,
    password,
    isDevMode = false,
    shallShowBrowser = true,
  } = options

  assert(username)
  assert(password)

  if (isDevMode) return "1234.56 €"

  const baseUrl = "https://portal.ebase.com"
  const loginUrl = `${baseUrl}/(e1)/finvesto`

  const {browser, page} = await launchBrowser({shallShowBrowser})

  try {
    log(`Open ${loginUrl}`)
    await page.goto(loginUrl)
    await page.waitForSelector("#loginfelder")


    log("Log in")
    await page.fill("#eox_ContentPane_3_depotNrTextBox", username)
    await page.fill("#eox_ContentPane_3_pinTextBox", password)
    await page.click("#eox_ContentPane_3_LOGIN")
    await page.waitForSelector(".tabNavBody")


    log("Retrieve current balance")
    const balance = await page.evaluate(
      selector => document
        .querySelector(selector)
        .textContent
        .replace(/\./g, "")
        .replace(/,/g, "."),
      "#eox_ContentPane_4_VermoegensuebersichtBody1_" +
        "repeaterDepotsKonten_ctl02_lblBestandGesamt",
    )

    return balance + " €"
  }
  finally {
    await browser.close()
  }
}

const promptValues = [
  {
    type: "input",
    name: "username",
    message: "Finvesto Username:",
  },
  {
    type: "password",
    name: "password",
    message: "Finvesto Password:",
  },
]

prompt(promptValues)
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint("Finvesto", balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
