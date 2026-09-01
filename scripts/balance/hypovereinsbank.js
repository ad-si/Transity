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

  const baseUrl = "https://my.hypovereinsbank.de"
  const loginUrl = `${baseUrl}/login?view=/de/login.jsp`

  const {browser, page} = await launchBrowser({shallShowBrowser})

  try {
    log(`Open ${loginUrl}`)
    await page.goto(loginUrl)
    await page.waitForSelector("#loginPanel")


    log("Log in")
    await page.fill("#username", username)
    await page.fill("#px2", password)
    await page.click("#loginCommandButton")
    await page.waitForSelector(".startpagemoney")


    log("Retrieve current balance")
    return await page.evaluate(
      selector => document
        .querySelector(selector)
        .textContent
        .replace(/\./g, "")
        .replace(/,/g, ".")
        .replace(/EUR/, " €"),
      ".startpagemoney",
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
    message: "HypoVereinsbank Username:",
  },
  {
    type: "password",
    name: "password",
    message: "HypoVereinsbank Password:",
  },
]

prompt(promptValues)
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint("hypovereinsbank.de", balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
