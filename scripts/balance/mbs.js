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

  const baseUrl = "https://www.mbs.de"
  const loginUrl = `${baseUrl}/de/home.html`

  const {browser, page} = await launchBrowser({shallShowBrowser})

  try {
    log(`Open ${loginUrl}`)
    await page.goto(loginUrl)
    await page.waitForSelector(".loginlogout")


    log("Log in")
    await page.fill(".loginlogout input[type=text]", username)
    await page.fill(".loginlogout input[type=password]", password)
    await page.click("input[value=Anmelden]")
    await page.waitForSelector(".mbf-finanzstatus")


    log("Retrieve current balance")
    return await page.evaluate(
      selector => document
        .querySelector(selector)
        .textContent
        .replace(/\./g, "")
        .replace(/,/g, ".")
        .replace(/EUR/, "€"),
      ".mbf-finanzstatus .tablefooter .balance .offscreen",
    )
  }
  finally {
    await browser.close()
  }
}

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

prompt(promptValues)
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint("mbs.de", balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
