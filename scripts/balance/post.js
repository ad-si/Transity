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
    isDevMode,
    shallShowBrowser = true,
  } = options

  if (isDevMode) return "1234.56 €"

  const baseUrl = "https://portokasse.deutschepost.de"

  const {browser, page} = await launchBrowser({shallShowBrowser})

  try {
    log(`Open ${baseUrl}`)
    await page.goto(baseUrl)
    await page.waitForSelector("#email")


    log("Log in")
    await page.fill("#email", username)
    await page.fill("#password", password)
    await page.click("button.actionbutton[type=submit]")
    await page.waitForSelector("#txtWalletBalance")


    log("Retrieve current balance")
    return await page.evaluate(() => document
      .querySelector("#txtWalletBalance")
      .textContent
      .replace(/,(\d\d)\xa0€$/, ".$1 €"),
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
    message: "Portokasse Username:",
  },
  {
    type: "password",
    name: "password",
    message: "Portokasse Password:",
  },
]

prompt(promptValues)
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint("portokasse.deutschepost.de", balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
