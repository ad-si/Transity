const fse = require('fs-extra')
const path = require('path')

const Nightmare = require('nightmare')
const nightmareDownloadManager = require('nightmare-inline-download')
const yaml = require('js-yaml')
const tempy = require('tempy')
const csvnorm = require('csvnorm')
const converter = require('converter')

const {toDayMonthYear} = require('../helpers.js')

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
      .reverse() // Now sorted ascending by value date
      .map(transaction => {
        if (transaction.Betrag.startsWith('-')) {
          transaction = Object.assign(
            {
              from: 'MBS/Girokonto',
              to: transaction['Beguenstigter/Zahlungspflichtiger'],
              amount: transaction.Betrag.slice(1) + transaction.Waehrung,
            },
            transaction
          )
        }
        else {
          transaction = Object.assign(
            {
              from: transaction['Beguenstigter/Zahlungspflichtiger'],
              to: 'MBS/Girokonto',
              // TODO: Remove when github.com/adius/csvnorm/issues/1 is solved
              amount: transaction.Betrag === '0,00'
                ? 0
                : transaction.Betrag + transaction.Waehrung,
            },
            transaction
          )
        }

        transaction.iban = transaction['Kontonummer/IBAN']
        delete transaction['Kontonummer/IBAN']

        transaction.bic = transaction['BIC (SWIFT-Code)']
        delete transaction['BIC (SWIFT-Code)']

        transaction['end to end id'] =
          transaction['Kundenreferenz (End-to-End)']
        delete transaction['Kundenreferenz (End-to-End)']

        delete transaction.Auftragskonto
        delete transaction['Beguenstigter/Zahlungspflichtiger']
        delete transaction.Betrag
        delete transaction.Info
        delete transaction.Waehrung

        function rmEmptyString (key, value) {
          return value === ''
            ? undefined
            : value
        }

        return JSON.parse(JSON.stringify(transaction, rmEmptyString))
      })

    const yamlString = yaml
      .dump(transactions)
      .replace(/^- /gm, '\n-\n  ')
      .replace(/^([\w- ]+): '(.+)'$/gm, '$1: $2')

    console.info(yamlString)
  })

  csvnorm({
    readableStream: fse.createReadStream(filePathTemp),
    writableStream: csv2json,
  })
}


async function downloadRange (options = {}) {
  const {
    nightmare,
    filePathTemp,
    type = 'CSV-CAMT-Format',
    startDate,
    endDate,
  } = options

  const startInputSelector = '#zeitraumKalender input[type=text]:first-of-type'
  const endInputSelector = '#zeitraumKalender input[type=text]:last-of-type'
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
    .insert(startInputSelector, toDayMonthYear(startDate))

    .insert(endInputSelector, '')
    .insert(endInputSelector, toDayMonthYear(endDate))

    .click('.bcontinue input[type=submit]')
    .refresh() // Necessary to update the list


  const optionId = await nightmare
    .evaluate(
      (selector, done) => {
        done(
          null,
          document
            .querySelector('.bpageselect select option:last-of-type')
            .value
        )
      },
      '.bpageselect select'
    )

  log(`Show all entries of option "${optionId}"`)
  await nightmare
    .select('.bpageselect select', optionId)
    .click('.bpageselect input[type=submit]')
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
  const baseUrl = 'https://www.mbs.de'
  const filePathTemp = tempy.file({name: 'transactions.csv'})
  const log = process.env.NODE_DEBUG
    ? console.warn
    : () => {}

  const url = `${baseUrl}/de/home.html`
  log(`Open ${url}`)
  await nightmare
    .goto(url)
    .wait('.loginlogout')


  log('Log in')
  await nightmare
    .insert('.loginlogout input[type=text]', username)
    .insert('.loginlogout input[type=password]', password)
    .click('input[value=Anmelden]')
    .wait('.mbf-finanzstatus')


  log('Go to transactions page')
  await nightmare
    .goto(`${baseUrl}/de/home/onlinebanking/umsaetze/umsaetze.html`)
    .wait('#zeitraumKalender')


  if (process.argv[2] === 'MT940') {
    const previousMonthLastDay = new Date(
      new Date()
        .setUTCDate(0)
    )
    const previousMonthFirstDay = new Date(
      new Date(previousMonthLastDay)
        .setUTCDate(1)
    )

    await downloadRange({
      nightmare,
      filePathTemp,
      type: 'MT940-Format',
      startDate: previousMonthFirstDay,
      endDate: previousMonthLastDay,
    })

    console.info(await fse.readFile(filePathTemp, 'utf-8'))
  }
  else {
    await downloadRange({nightmare, filePathTemp, startDate, endDate})
    normalizeAndPrint(filePathTemp)
  }
}


async function main () {
  try {
    const envYaml = await fse.readFile(
      path.resolve(__dirname, '../environment.yaml')
    )
    const environment = yaml.safeLoad(envYaml)

    return getTransactions({
      username: environment.mbs.username,
      password: environment.mbs.password,
      shallShowBrowser: true,
      numberOfDays: 100,
      // startDate: new Date('2016-01-01'),
    })
  }
  catch (error) {
    if (error.code === 'ENOENT') {
      console.warn(
        'Retrieve the environment.yaml file from gopass' +
        'and add it to the project\'s root directory'
      )
    }
    else {
      console.error(error)
    }
  }
}


main()
