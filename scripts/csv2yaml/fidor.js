const fse = require("fs-extra")
const yaml = require("js-yaml")
const csvnorm = require("csvnorm")
const converter = require("converter")

const {
  rmEmptyString,
  keysToEnglish,
  noteToAccount,
  sanitizeYaml,
} = require("../helpers.js")


function normalizeAndPrint (filePathTemp) {
  const csv2json = converter({
    from: "csv",
    to: "json",
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
        const currency = " €"
        const sortedTransaction = {
          utc: transaction.date,
          note: transaction.note + "\n" + transaction.note2,
        }
        const account = noteToAccount(transaction.note) || "_todo_"
        const transfersObj = transaction.amount.startsWith("-")
          ? {
            transfers: [{
              from: "fidor:giro",
              to: account,
              amount: transaction.amount.slice(1) + currency,
            }],
          }
          : {
            transfers: [{
              from: account,
              to: "fidor:giro",
              // TODO: Remove when https://github.com/adius/csvnorm/issues/1
              //       is solved
              amount: transaction.amount === "0,00"
                ? 0
                : transaction.amount + currency,
            }],
          }
        const newTransaction = Object.assign(sortedTransaction, transfersObj)

        delete newTransaction.amount

        return JSON.parse(JSON.stringify(newTransaction, rmEmptyString))
      })

    const yamlString = sanitizeYaml(yaml.dump({transactions}))

    console.info(yamlString)
  })

  csvnorm.default({
    encoding: "utf-8",
    readableStream: fse.createReadStream(filePathTemp),
    writableStream: csv2json,
  })
}

normalizeAndPrint(process.argv[2])
