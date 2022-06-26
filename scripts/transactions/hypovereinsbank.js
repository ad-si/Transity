import fse from "fs-extra"

import Nightmare from "nightmare"
import nightmareDownloadManager from "nightmare-inline-download"
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

const prompt = inquirer.createPromptModule({ output: process.stderr })

nightmareDownloadManager(Nightmare)



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
              // TODO: Remove when github.com/adius/csvnorm/issues/1 is solved
              amount: transaction.amount === "0,00"
                ? 0
                : transaction.amount + currency,
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
    nightmare,
    filePathTemp,
    // type = 'CSV-CAMT-Format',
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
  await nightmare
    .insert(startInputSelector, "")
    .insert(startInputSelector, toDdotMdotYYYY(startDate))

    .insert(endInputSelector, "")
    .insert(endInputSelector, toDdotMdotYYYY(endDate))
    .click("#showtransactions")
    .wait(25000) // Time to enter TAN

  log(`Download CSV file to ${filePathTemp}`)
  return await nightmare
    .click("a[title=CSV]")
    .download(filePathTemp)
    .end()
}


async function getTransactions (options = {}) {
  const daysAgo = new Date()
  daysAgo.setDate(daysAgo.getDate() - options.numberOfDays)
  const {
    startDate = daysAgo,
    endDate = new Date(),
    username,
    password,
    shallShowBrowser = false,
  } = options

  const nightmare = new Nightmare({show: shallShowBrowser})
  const baseUrl = "https://my.hypovereinsbank.de"
  const filePathTemp = temporaryFile({name: "hypovereinsbank-transactions.csv"})
  const log = process.env.NODE_DEBUG
    ? console.warn
    : () => {}

  const url = `${baseUrl}/login?view=/de/login.jsp`
  log(`Open ${url}`)
  await nightmare
    .goto(url)
    .wait("#loginPanel")


  log("Log in")
  await nightmare
    .insert("#loginPanel #username", username)
    .insert("#loginPanel #px2", password)
    .click("#loginCommandButton")
    .wait(".startpagemoney")


  log("Go to transactions page")
  await nightmare
    .goto(`${baseUrl}/portal?view=/de/banking/konto/kontofuehrung/umsaetze.jsp`)
    .wait("#dateFrom")


  await downloadRange({nightmare, filePathTemp, startDate, endDate})
  normalizeAndPrint(filePathTemp)
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
