const fse = require('fs-extra')

const Nightmare = require('nightmare')
const nightmareDownloadManager = require('nightmare-inline-download')
const yaml = require('js-yaml')
const tempy = require('tempy')
const csvnorm = require('csvnorm')
const converter = require('converter')
const inquirer = require('inquirer')

const {
  rmEmptyString,
  toDdotMdotYYYY,
  keysToEnglish,
  noteToAccount,
} = require('../helpers.js')

const prompt = inquirer.createPromptModule({ output: process.stderr })

nightmareDownloadManager(Nightmare)



function normalizeAndPrint (filePathTemp) {
  const csv2json = converter({
    from: 'csv',
    to: 'json',
    // TODO: Use again when http://github.com/doowb/converter/issues/19 is fixed
    // to: 'yml',
  })

  let jsonTemp = ''
  csv2json.on('data', chunk => {
    jsonTemp += chunk
  })
  csv2json.on('end', () => {
    const transactions = JSON
      .parse(jsonTemp)
      .map(keysToEnglish)
      .map(transaction => {
        const currency = transaction.currency.replace('EUR', '€')
        const sortedTransaction = Object.assign(
          {
            utc: transaction.utc,
            'entry-utc': transaction['entry-utc'],
            note: transaction.note,
          },
          transaction,
        )
        const transfersObj = transaction.amount.startsWith('-')
          ? {
            transfers: [{
              from: 'hypo:giro',
              to: noteToAccount(transaction.note),
              amount: transaction.amount.slice(1) + currency,
            }],
          }
          : {
            transfers: [{
              from: noteToAccount(transaction.note),
              to: 'hypo:giro',
              // TODO: Remove when github.com/adius/csvnorm/issues/1 is solved
              amount: transaction.amount === '0,00'
                ? 0
                : transaction.amount + currency,
            }],
          }
        const newTransaction = Object.assign(sortedTransaction, transfersObj)

        delete newTransaction.amount
        delete newTransaction.currency
        if (newTransaction['entry-utc'] === newTransaction.utc) {
          delete newTransaction['entry-utc']
        }

        return JSON.parse(JSON.stringify(newTransaction, rmEmptyString))
      })

    const yamlString = yaml
      .dump(transactions)
      .replace(/^- /gm, '\n-\n  ')
      .replace(/^([\w- ]+): '(.+)'$/gm, '$1: $2')

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

  const startInputSelector = '#dateFrom_input'
  const endInputSelector = '#dayTo_input'
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
    }`
  )
  await nightmare
    .insert(startInputSelector, '')
    .insert(startInputSelector, toDdotMdotYYYY(startDate))

    .insert(endInputSelector, '')
    .insert(endInputSelector, toDdotMdotYYYY(endDate))

    .click('#showtransactions')


  log(`Download CSV file to ${filePathTemp}`)
  return await nightmare
    .click('a[title=CSV]')
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
  const baseUrl = 'https://my.hypovereinsbank.de'
  const filePathTemp = tempy.file({name: 'hypovereinsbank-transactions.csv'})
  const log = process.env.NODE_DEBUG
    ? console.warn
    : () => {}

  const url = `${baseUrl}/login?view=/de/login.jsp`
  log(`Open ${url}`)
  await nightmare
    .goto(url)
    .wait('#loginPanel')


  log('Log in')
  await nightmare
    .insert('#loginPanel #username', username)
    .insert('#loginPanel #px2', password)
    .click('#loginCommandButton')
    .wait('.startpagemoney')


  log('Go to transactions page')
  await nightmare
    .goto(`${baseUrl}/portal?view=/de/banking/konto/kontofuehrung/umsaetze.jsp`)
    .wait('#dateFrom')


  await downloadRange({nightmare, filePathTemp, startDate, endDate})
  normalizeAndPrint(filePathTemp)
}


async function main () {
  const promptValues = [
    {
      type: 'input',
      name: 'username',
      message: 'HypoVereinsbank Username:',
    },
    {
      type: 'password',
      name: 'password',
      message: 'HypoVereinsbank Password:',
    },
  ]
  const answers = await prompt(promptValues)

  return getTransactions({
    username: answers.username,
    password: answers.password,
    shallShowBrowser: true,
    numberOfDays: 100,
    // startDate: new Date('2016-01-01'),
  })
}


main()
