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

  if (isDevMode) {
    return ["1234.56 €", "1234.56 €"]
  }

  const baseUrl = "https://www.paypal.com"
  const balanceURl = "https://www.paypal.com/businessexp/money"
  const loginUrl = `${baseUrl}/signin?returnUri=${
    encodeURIComponent(balanceURl)}`

  const {browser, page} = await launchBrowser({shallShowBrowser})

  try {
    log(`Open ${loginUrl}`)
    await page.goto(loginUrl)
    await page.waitForSelector("#email")

    log("Enter email")
    await page.fill("#email", username)
    await page.click("#btnNext")
    await page.waitForFunction(() => !document
      .getElementById("splitPassword").classList
      .contains("hide"),
    )

    log("Enter password")
    await page.fill("#password", password)
    await page.click("#btnLogin")
    await page.waitForSelector(".multi-currency")

    log("Retrieve current balances")
    return await page.evaluate(
      selector => Array
        .from(document.querySelectorAll(selector))
        .map(element => element
          .childNodes[0]
          .nodeValue
          .replace(/,(\d\d)\xa0(\w{3})(\n= .+\n)?/g, ".$1 $2")
          .replace("EUR", "€")
          .replace("USD", "$"),
        ),
      ".multi-currency .currency-amt-newexp",
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
    message: "PayPal Username:",
  },
  {
    type: "password",
    name: "password",
    message: "PayPal Password:",
  },
]

prompt(promptValues)
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint("paypal.com", balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
