const fse = require('fs-extra')
const yaml = require('js-yaml')
const csvnorm = require('csvnorm')
const converter = require('converter')

const {
  rmEmptyString,
  keysToEnglish,
  noteToAccount,
  sanitizeYaml,
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
              'original-amount': transaction['original-amount'],
            }],
          }
          : {
            transfers: [{
              from: noteToAccount(note),
              to: 'dkb:visa',
              // TODO: Remove when github.com/adius/csvnorm/issues/1 is solved
              amount: transaction.amount === '0,00' ? '0 €' : amount,
              'original-amount': transaction['original-amount'],
            }],
          }
        const newTransaction = Object.assign(sortedTransaction, transfersObj)

        delete newTransaction.amount

        return JSON.parse(JSON.stringify(newTransaction, rmEmptyString))
      })
      .sort((transA, transB) =>
        // Oldest first
        String(transA.utc)
          .localeCompare(String(transB.utc), 'en'),
      )

    const yamlString = sanitizeYaml(yaml.dump({transactions}))

    console.info(yamlString)
  })

  csvnorm.default({
    encoding: 'latin1',
    readableStream: fse.createReadStream(filePathTemp),
    skipLinesStart: 6,
    writableStream: csv2json,
  })
}

normalizeAndPrint(process.argv[2])
