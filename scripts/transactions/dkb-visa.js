const fse = require('fs-extra')
const path = require('path')

const Nightmare = require('nightmare')
const nightmareDownloadManager = require('nightmare-inline-download')
const yaml = require('js-yaml')
const tempy = require('tempy')
const csvnorm = require('csvnorm')
const converter = require('converter')
const inquirer = require('inquirer')

const {
  toDdotMdotYYYY,
  keysToEnglish,
  noteToAccount,
} = require('../helpers.js')

const prompt = inquirer.createPromptModule({ output: process.stderr })

nightmareDownloadManager(Nightmare)


function rmEmptyString (key, value) {
  return value === ''
    ? undefined
    : value
}


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
        const note = transaction.note
          .replace(/<br\s+\/>/g, '\n')
        const amount = transaction.amount + ' €'
        const sortedTransaction = {
            utc: transaction['value-utc'] < transaction['entry-utc']
              ? transaction['value-utc']
              : transaction['entry-utc'],
          }

        if (transaction['value-utc'] !== sortedTransaction.utc) {
          sortedTransaction['value-utc'] = transaction['value-utc']
        }
        if (transaction['entry-utc'] !== sortedTransaction.utc) {
          sortedTransaction['entry-utc'] = transaction['entry-utc']
        }

        sortedTransaction.type = transaction.type
        sortedTransaction.note = note

        const transfersObj = transaction.amount.startsWith('-')
          ? {
              transfers: [{
                from: 'dkb:visa',
                to: noteToAccount(note),
                amount: amount.slice(1),
                'original-amount': transaction['original-amount']
              }]
            }
          : {
              transfers: [{
                from: noteToAccount(note),
                to: 'dkb:visa',
                // TODO: Remove when github.com/adius/csvnorm/issues/1 is solved
                amount: transaction.amount === '0,00' ? '0 €' : amount,
                'original-amount': transaction['original-amount']
              }]
            }
        const newTransaction = Object.assign(sortedTransaction, transfersObj)

        delete newTransaction.amount

        return JSON.parse(JSON.stringify(newTransaction, rmEmptyString))
      })
      .sort((a, b) =>
        // Oldest first
        String(a.utc).localeCompare(String(b.utc), 'en')
      )

    const yamlString = yaml
      .dump(transactions)
      .replace(/^- /gm, '\n-\n  ')
      .replace(/^([\w- ]+): '(.+)'$/gm, '$1: $2')

    console.info(yamlString)
  })

  csvnorm.default({
    encoding: 'latin1',
    readableStream: fse.createReadStream(filePathTemp),
    skipLinesStart: 7,
    writableStream: csv2json,
  })
}


async function downloadRange (options = {}) {
  const {
    nightmare,
    filePathTemp,
    startDate,
    endDate,
  } = options

  const startInputSelector = '[tid=postingDate]'
  const endInputSelector = '[tid=toPostingDate]'
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

    .click('#searchbutton')


  log(`Download CSV file to ${filePathTemp}`)
  return await nightmare
    .click('[tid=csvExport]')
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
  const baseUrl = 'https://www.dkb.de'
  const filePathTemp = tempy.file({name: 'dkb-transactions.csv'})
  const log = process.env.NODE_DEBUG
    ? console.warn
    : () => {}

  const loginUrl = `${baseUrl}/banking`
  log(`Open ${loginUrl}`)
  await nightmare
    .goto(loginUrl)
    .wait('#login')


  log('Log in')
  await nightmare
    .insert('#loginInputSelector', username)
    .insert('#pinInputSelector', password)
    .click('#buttonlogin')
    .wait('#summe-gruppe-0')


  log('Go to transactions page')
  await nightmare
    // Doesn't work => click link instead
    // .goto(`${baseUrl}/banking/finanzstatus/kontoumsaetze?$event=init`)
    .click('#gruppe-0_1 .evt-paymentTransaction')
    .wait('.form.validate')

  // Select date picker
  await nightmare.click('[name=filterType]')

  await downloadRange({nightmare, filePathTemp, startDate, endDate})
  normalizeAndPrint(filePathTemp)
}


async function main () {
  const answers = await
    prompt([
      {
        type: 'input',
        name: 'username',
        message: 'DKB Username:',
      },
      {
        type: 'password',
        name: 'password',
        message: 'DKB Password:',
      },
    ])

  return getTransactions({
    username: answers.username,
    password: answers.password,
    shallShowBrowser: true,
    numberOfDays: 1095,  // Maximum of 1095 days (3 years - 1 day)
    // startDate: new Date('2016-01-01'),
  })
}


main()
