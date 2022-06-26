import fse from "fs-extra"

import converter from "converter"
import inquirer from "inquirer"
import Nightmare from "nightmare"
import { temporaryFile } from "tempy"
import yaml from "js-yaml"

import nightmareDownloadManager from "nightmare-inline-download"

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
      .reverse() // Now sorted ascending by value date
      .map(transaction => {
        const newFields = {
          utc: transaction["value-utc"] < transaction["entry-utc"]
            ? transaction["value-utc"]
            : transaction["entry-utc"],
          note: "",
        }
        const sortedTransaction = Object.assign(newFields, transaction)

        if (sortedTransaction["value-utc"] === sortedTransaction.utc) {
          delete sortedTransaction["value-utc"]
        }
        if (sortedTransaction["entry-utc"] === sortedTransaction.utc) {
          delete sortedTransaction["entry-utc"]
        }

        const account = noteToAccount(transaction.to)
        const transfersObj = transaction.amount.startsWith("-")
          ? {
            transfers: [{
              from: "mbs:giro",
              to: account,
              amount: transaction.amount.slice(1) + transaction.currency,
            }],
          }
          : {
            transfers: [{
              from: account,
              to: "mbs:giro",
              // TODO: Remove when github.com/adius/csvnorm/issues/1 is solved
              amount: transaction.amount === "0,00"
                ? 0
                : transaction.amount + transaction.currency,
            }],
          }
        const newTransaction = Object.assign(sortedTransaction, transfersObj)

        delete newTransaction.to
        delete newTransaction.amount
        delete newTransaction.currency

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
    type = "CSV-CAMT-Format",
    startDate,
    endDate,
  } = options

  const startInputSelector = "#zeitraumKalender input[type=text]:first-of-type"
  const endInputSelector = "#zeitraumKalender input[type=text]:last-of-type"
  const log = process.env.NODE_DEBUG
    ? console.warn
    // eslint-disable-next-line @typescript-eslint/no-empty-function
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

    .click(".bcontinue input[type=submit]")
    .wait(options.numberOfDays > 90 ? 25000 : 0)  // Time to enter TAN


  const optionId = await nightmare
    .evaluate(
      (selector, done) => {
        done(
          null,
          document
            .querySelector(".bpageselect select option:last-of-type")
            .value,
        )
      },
      ".bpageselect select",
    )

  log(`Show all entries of option "${optionId}"`)
  await nightmare
    .select(".bpageselect select", optionId)
    .click(".bpageselect input[type=submit]")
    .refresh() // Necessary to update the list


  log(`Download ${type} file`)
  return await nightmare
    .click(`input[value=${type}]`)
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
  const baseUrl = "https://www.mbs.de"
  const filePathTemp = temporaryFile({name: "transactions.csv"})
  const log = process.env.NODE_DEBUG
    ? console.warn
    // eslint-disable-next-line @typescript-eslint/no-empty-function
    : () => {}

  const url = `${baseUrl}/de/home.html`
  log(`Open ${url}`)
  await nightmare
    .goto(url)
    .wait(".loginlogout")


  log("Log in")
  await nightmare
    .insert(".loginlogout input[type=text]", username)
    .insert(".loginlogout input[type=password]", password)
    .click("input[value=Anmelden]")
    .wait(".mbf-finanzstatus")


  log("Go to transactions page")
  await nightmare
    .goto(`${baseUrl}/de/home/onlinebanking/umsaetze/umsaetze.html`)
    .wait("#zeitraumKalender")


  if (process.argv[2] === "MT940") {
    const previousMonthLastDay = new Date(
      new Date()
        .setUTCDate(0),
    )
    const previousMonthFirstDay = new Date(
      new Date(previousMonthLastDay)
        .setUTCDate(1),
    )

    await downloadRange({
      nightmare,
      filePathTemp,
      type: "MT940-Format",
      startDate: previousMonthFirstDay,
      endDate: previousMonthLastDay,
    })

    console.info(await fse.readFile(filePathTemp, "utf-8"))
  }
  else {
    await downloadRange({nightmare, filePathTemp, startDate, endDate})
    normalizeAndPrint(filePathTemp)
  }
}


async function main () {
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
  const answers = await prompt(promptValues)

  try {
    getTransactions({
      username: answers.username,
      password: answers.password,
      shallShowBrowser: true,
      numberOfDays: 90,  // More than 90 days trigger a tan prompt
      // startDate: new Date('2016-01-01'),
    })
  }
  catch (error) {
    console.error(error)
  }
}


main()
