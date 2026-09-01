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

  const baseUrl = "https://www.dkb.de"
  const loginUrl = `${baseUrl}/banking`

  const {browser, page} = await launchBrowser({shallShowBrowser})

  try {
    log(`Open ${loginUrl}`)
    await page.goto(loginUrl)
    await page.waitForSelector("#login")


    log("Log in")
    await page.fill("#loginInputSelector", username)
    await page.fill("#pinInputSelector", password)
    await page.click("#buttonlogin")
    await page.waitForSelector("#financialStatus")


    log("Retrieve current balance")
    const balance = await page.evaluate(
      selector => document
        .querySelector(selector)
        .textContent
        .replace(/\./g, "")
        .replace(/,/g, "."),
      "#financialStatus #summe-gruppe-0 strong span",
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
    message: "DKB Username:",
  },
  {
    type: "password",
    name: "password",
    message: "DKB Password:",
  },
]

prompt(promptValues)
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint("dkb.de", balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
