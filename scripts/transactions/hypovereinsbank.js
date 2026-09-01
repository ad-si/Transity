import fse from "fs-extra"

import yaml from "js-yaml"
import { temporaryFile } from "tempy"
import converter from "converter"
import inquirer from "inquirer"

import {
  rmEmptyString,
  toDdotMdotYYYY,
  keysToEnglish,
  noteToAccount,
  sanitizeYaml,
} from "../helpers.js"
import { launchBrowser } from "../browser.js"

const prompt = inquirer.createPromptModule({ output: process.stderr })


async function normalizeAndPrint (filePathTemp) {
  const csvnorm = await import("csvnorm")
  const csv2json = converter({
    from: "csv",
    to: "json",
    // TODO: Use again when http://github.com/doowb/converter/issues/19 is fixed
    // to: 'yml',
  })

  let jsonTemp = ""
  csv2json.on("data", chunk => {
    jsonTemp += chunk
  })
  csv2json.on("end", () => {
    const transactions = JSON
      .parse(jsonTemp)
      .map(keysToEnglish)
      .map(transaction => {
        const currency = transaction.currency.replace("EUR", "€")
        const sortedTransaction = {
          utc: transaction["value-utc"] < transaction["entry-utc"]
            ? transaction["value-utc"]
            : transaction["entry-utc"],
        }

        if (transaction["value-utc"] !== sortedTransaction.utc) {
          sortedTransaction["value-utc"] = transaction["value-utc"]
        }
        if (transaction["entry-utc"] !== sortedTransaction.utc) {
          sortedTransaction["entry-utc"] = transaction["entry-utc"]
        }

        sortedTransaction.note = transaction.note

        const account = noteToAccount(transaction.note)
        const transfersObj = transaction.amount.startsWith("-")
          ? {
            transfers: [{
              from: "hypo:giro",
              to: account,
              amount: transaction.amount.slice(1) + currency,
            }],
          }
          : {
            transfers: [{
              from: account,
              to: "hypo:giro",
              amount: transaction.amount + currency,
            }],
          }
        const newTransaction = Object.assign(sortedTransaction, transfersObj)

        delete newTransaction.amount
        delete newTransaction.currency
        if (newTransaction["entry-utc"] === newTransaction.utc) {
          delete newTransaction["entry-utc"]
        }

        return JSON.parse(JSON.stringify(newTransaction, rmEmptyString))
      })

    const yamlString = sanitizeYaml(yaml.dump({transactions}))

    console.info(yamlString)
  })

  csvnorm.default({
    readableStream: fse.createReadStream(filePathTemp),
    writableStream: csv2json,
  })
}


async function downloadRange (options = {}) {
  const {
    page,
    filePathTemp,
    startDate,
    endDate,
  } = options

  const startInputSelector = "#dateFrom_input"
  const endInputSelector = "#dayTo_input"
  const log = process.env.NODE_DEBUG
    ? console.warn
    : () => {}

  log(
    `Enter range ${
      startDate
        .toISOString()
        .slice(0, 10)
    } to ${
      endDate
        .toISOString(10)
        .slice(0, 10)
    }`,
  )
  await page.fill(startInputSelector, toDdotMdotYYYY(startDate))
  await page.fill(endInputSelector, toDdotMdotYYYY(endDate))
  await page.click("#showtransactions")
  await page.waitForTimeout(25000) // Time to enter TAN

  log(`Download CSV file to ${filePathTemp}`)
  const downloadPromise = page.waitForEvent("download")
  await page.click("a[title=CSV]")
  const download = await downloadPromise
  await download.saveAs(filePathTemp)
}


async function getTransactions (options = {}) {
  const daysAgo = new Date()
  daysAgo.setDate(daysAgo.getDate() - options.numberOfDays)
  const {
    startDate = daysAgo,
    endDate = new Date(),
    username,
    password,
    shallShowBrowser = true,
  } = options

  const baseUrl = "https://my.hypovereinsbank.de"
  const filePathTemp = temporaryFile({name: "hypovereinsbank-transactions.csv"})
  const log = process.env.NODE_DEBUG
    ? console.warn
    : () => {}

  const {browser, page} = await launchBrowser({shallShowBrowser})

  try {
    const url = `${baseUrl}/login?view=/de/login.jsp`
    log(`Open ${url}`)
    await page.goto(url)
    await page.waitForSelector("#loginPanel")


    log("Log in")
    await page.fill("#loginPanel #username", username)
    await page.fill("#loginPanel #px2", password)
    await page.click("#loginCommandButton")
    await page.waitForSelector(".startpagemoney")


    log("Go to transactions page")
    await page.goto(
      `${baseUrl}/portal?view=/de/banking/konto/kontofuehrung/umsaetze.jsp`,
    )
    await page.waitForSelector("#dateFrom")


    await downloadRange({page, filePathTemp, startDate, endDate})
    await normalizeAndPrint(filePathTemp)
  }
  finally {
    await browser.close()
  }
}


async function main () {
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
  const answers = await prompt(promptValues)

  return getTransactions({
    username: answers.username,
    password: answers.password,
    shallShowBrowser: true,
    numberOfDays: 80,
    // startDate: new Date('2019-03-01'),
  })
}


main()
