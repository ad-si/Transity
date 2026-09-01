import fse from "fs-extra"

import yaml from "js-yaml"
import { temporaryFile } from "tempy"
import converter from "converter"
import inquirer from "inquirer"

import {
  toDDdotMMdotYYYY,
  keysToEnglish,
  noteToAccount,
  sanitizeYaml,
} from "../helpers.js"
import { launchBrowser } from "../browser.js"

const prompt = inquirer.createPromptModule({ output: process.stderr })


function rmEmptyString (key, value) {
  return value === ""
    ? undefined
    : value
}


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
        const note = transaction.note
          .replace(/<br\s+\/>/g, "\n")
        const amount = transaction.amount + " €"
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

        sortedTransaction.type = transaction.type
        sortedTransaction.note = note

        const transfersObj = transaction.amount.startsWith("-")
          ? {
            transfers: [{
              from: "dkb:giro",
              to: noteToAccount(note),
              amount: amount.slice(1),
            }],
          }
          : {
            transfers: [{
              from: noteToAccount(note),
              to: "dkb:giro",
              amount,
            }],
          }
        const newTransaction = Object.assign(sortedTransaction, transfersObj)

        delete newTransaction.amount

        return JSON.parse(JSON.stringify(newTransaction, rmEmptyString))
      })
      .sort((transA, transB) =>
        // Oldest first
        String(transA.utc)
          .localeCompare(String(transB.utc), "en"),
      )

    const yamlString = sanitizeYaml(yaml.dump({transactions}))

    console.info(yamlString)
  })

  csvnorm.default({
    encoding: "latin1",
    readableStream: fse.createReadStream(filePathTemp),
    skipLinesStart: 6,
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

  const startInputSelector = "[name=transactionDate]"
  const endInputSelector = "[name=toTransactionDate]"
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

  await page.fill(startInputSelector, toDDdotMMdotYYYY(startDate))
  await page.fill(endInputSelector, toDDdotMMdotYYYY(endDate))
  await page.click("#searchbutton")
  await page.reload() // Necessary to avoid race condition


  log(`Download CSV file to ${filePathTemp}`)
  const downloadPromise = page.waitForEvent("download")
  await page.click("[tid=csvExport]")
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

  const baseUrl = "https://www.dkb.de"
  const filePathTemp = temporaryFile({name: "dkb-transactions.csv"})
  const log = process.env.NODE_DEBUG
    ? console.warn
    : () => {}

  const {browser, page} = await launchBrowser({shallShowBrowser})

  try {
    const loginUrl = `${baseUrl}/banking`
    log(`Open ${loginUrl}`)
    await page.goto(loginUrl)
    await page.waitForSelector("#login")


    log("Log in")
    await page.fill("#loginInputSelector", username)
    await page.fill("#pinInputSelector", password)
    await page.click("#buttonlogin")
    await page.waitForSelector("#summe-gruppe-0")


    log("Go to transactions page")
    // Doesn't work => click link instead
    await page.goto(`${baseUrl}/banking/finanzstatus/kontoumsaetze?$event=init`)
    // .click('#gruppe-0_1 .evt-paymentTransaction')
    await page.waitForSelector(".form.validate")

    // Select date picker
    await page.click("[name=searchPeriodRadio]")

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
      message: "DKB Username:",
    },
    {
      type: "password",
      name: "password",
      message: "DKB Password:",
    },
  ]
  const answers = await prompt(promptValues)

  return getTransactions({
    username: answers.username,
    password: answers.password,
    shallShowBrowser: true,
    numberOfDays: 1095,
    // startDate: new Date('2016-01-01'),
  })
}


main()
