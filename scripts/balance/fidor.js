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

  const baseUrl = "https://banking.fidor.de"
  const loginUrl = `${baseUrl}/login`

  const {browser, page} = await launchBrowser({shallShowBrowser})

  try {
    log(`Open ${loginUrl}`)
    await page.goto(loginUrl)
    await page.waitForSelector("#new_user")


    log("Log in")
    await page.fill("#user_email", username)
    await page.fill("#user_password", password)
    await page.click("button#login")
    await page.waitForSelector(".available-balance")


    log("Retrieve current balance")
    return await page.evaluate(
      selector => document
        .querySelector(selector)
        .textContent
        .replace(/\./g, "")
        .replace(/,/g, "."),
      ".available-balance .main-amount",
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
    message: "Fidor Username:",
  },
  {
    type: "password",
    name: "password",
    message: "Fidor Password:",
  },
]

prompt(promptValues)
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint("fidor.de", balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
