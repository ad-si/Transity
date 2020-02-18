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
      .map(transaction => {
        const currency = transaction.currency.replace('EUR', '€')
        const sortedTransaction = Object.assign(
          {
            utc: transaction.utc || transaction['entry-utc'],
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
        if (newTransaction['value-utc'] === newTransaction.utc) {
          delete newTransaction['value-utc']
        }

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
