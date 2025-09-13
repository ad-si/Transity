import assert from "assert"

import inquirer from "inquirer"

import {prettyPrint} from "../helpers.js"


const prompt = inquirer.createPromptModule({ output: process.stderr })
const log = process.env.NODE_DEBUG
  ? console.warn
  : () => {}


async function getBalance (options = {}) {
  const {
    username,
    password,
    isDevMode = false,
    nightmare,
  } = options

  assert(username)
  assert(password)

  if (isDevMode) return "1234.56 €"

  const baseUrl = "https://banking.fidor.de"
  const loginUrl = `${baseUrl}/login`


  log(`Open ${loginUrl}`)
  await nightmare
    .goto(loginUrl)
    .wait("#new_user")


  log("Log in")
  await nightmare
    .insert("#user_email", username)
    .insert("#user_password", password)
    .click("button#login")
    .wait(".available-balance")


  log("Retrieve current balance")
  return await nightmare
    .evaluate(
      selector => document
        .querySelector(selector)
        .textContent
        .replace(/\./g, "")
        .replace(/,/g, "."),
      ".available-balance .main-amount",
    )
    .end()
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
