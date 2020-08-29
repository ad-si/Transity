const fse = require('fs-extra')
const yaml = require('js-yaml')
const csvnorm = require('csvnorm')
const converter = require('converter')

const {
  rmEmptyString,
  keysToEnglish,
  noteToAccount,
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
      .reverse() // Now sorted ascending by value date
      .map(transaction => {
        const newFields = {
          utc: transaction['value-utc'] < transaction['entry-utc']
            ? transaction['value-utc']
            : transaction['entry-utc'],
          note: '',
        }
        const sortedTransaction = Object.assign(newFields, transaction)

        if (sortedTransaction['value-utc'] === sortedTransaction.utc) {
          delete sortedTransaction['value-utc']
        }
        if (sortedTransaction['entry-utc'] === sortedTransaction.utc) {
          delete sortedTransaction['entry-utc']
        }

        const account = noteToAccount(transaction.to)
        const transfersObj = transaction.amount.startsWith('-')
          ? {
            transfers: [{
              from: 'mbs:giro',
              to: account,
              amount: transaction.amount.slice(1) + transaction.currency,
            }],
          }
          : {
            transfers: [{
              from: account,
              to: 'mbs:giro',
              // TODO: Remove when github.com/adius/csvnorm/issues/1 is solved
              amount: transaction.amount === '0,00'
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

    const yamlString = yaml
      .dump({transactions})
      .replace(/^ {2}- /gm, '\n  -\n    ')
      .replace(/^([\w- ]+): '(.+)'$/gm, '$1: $2')
      .replace(/utc: 20(.+)$/gm, 'utc: \'20$1\'')

    console.info(yamlString)
  })

  csvnorm.default({
    readableStream: fse.createReadStream(filePathTemp),
    writableStream: csv2json,
  })
}

normalizeAndPrint(process.argv[2])
