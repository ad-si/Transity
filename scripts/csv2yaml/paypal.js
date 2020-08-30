const fse = require('fs-extra')
const yaml = require('js-yaml')
const csvnorm = require('csvnorm')
const converter = require('converter')
const chrono = require('chrono-node')


const {
  rmEmptyString,
  keysToEnglish,
} = require('../helpers.js')


function normalizeAndPrint (filePathTemp) {
  const csv2json = converter({
    from: 'csv',
    to: 'json',
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
        const currency = transaction.Currency
          .replace('EUR', '€')
          .replace('USD', '$')
          .trim()

        const sortedTransaction = Object.assign(
          {
            utc: chrono
              .parseDate([
                transaction.Date,
                transaction.Time,
                transaction.TimeZone,
              ].join(' '))
              .toISOString()
              .replace('T', ' ')
              .replace('.000Z', ''),
            note: transaction.Subject,
          },
          transaction,
        )
        const transfer = transaction.Gross.startsWith('-')
          ? {
            from: '_todo_:paypal:' +
              transaction.Currency
                .toLowerCase()
                .trim(),
            to: transaction.Name
              ? `${transaction.Name} <${transaction['To Email Address']}>`
              : 'paypal',
            amount: transaction.Gross.slice(1) + ' ' + currency,
          }
          : {
            from: transaction.Name
              ? `${transaction.Name} <${transaction['From Email Address']}>`
              : 'paypal',
            to: '_todo_:paypal:' +
              transaction.Currency
                .toLowerCase()
                .trim(),
            amount: transaction.Gross + ' ' + currency,
          }

        const newTransaction = Object.assign(
          sortedTransaction,
          {transfers: [transfer]},
        )

        if (Number(transaction.Fee) !== 0) {
          newTransaction.transfers.push({
            from: '_todo_:paypal:' +
              transaction.Currency
                .toLowerCase()
                .trim(),
            to: 'paypal',
            amount: transaction.Fee.slice(1) + ' ' + currency,
            tags: ['fee'],
          })
        }

        delete newTransaction.Subject
        delete newTransaction.Name
        delete newTransaction.Balance
        delete newTransaction.Gross
        delete newTransaction.Net
        delete newTransaction.Fee
        delete newTransaction.Currency
        delete newTransaction.Date
        delete newTransaction.Time
        delete newTransaction.TimeZone
        delete newTransaction['From Email Address']
        delete newTransaction['To Email Address']

        return JSON.parse(JSON.stringify(newTransaction, rmEmptyString))
      })

    const yamlString = yaml
      .dump({transactions})
      .replace(/^ {2}- /gm, '\n  -\n    ')
      .replace(/^([\w- ]+): '(.+)'$/gm, '$1: $2')
      .replace(/utc: 20(.+)$/gm, 'utc: \'20$1\'')

    console.info(yamlString)
  })

  csvnorm.default({
    dateFormat: 'dd/mm/yyyy',
    readableStream: fse.createReadStream(filePathTemp),
    writableStream: csv2json,
  })
}

normalizeAndPrint(process.argv[2])
