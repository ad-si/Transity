import fse from "fs-extra"
import yaml from "js-yaml"
import converter from "converter"

import {
  rmEmptyString,
  keysToEnglish,
  noteToAccount,
  sanitizeYaml,
} from "../helpers.js"


async function normalizeAndPrint (filePathTemp) {
  const csvnorm = await import("csvnorm")
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

        const account = noteToAccount(transaction.to) || "_todo_"
        const transfersObj = transaction.amount.startsWith("-")
          ? {
            transfers: [{
              from: "_todo_:mbs:giro",
              to: account,
              amount: transaction.amount.slice(1) + transaction.currency,
            }],
          }
          : {
            transfers: [{
              from: account,
              to: "_todo_:mbs:giro",
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

normalizeAndPrint(process.argv[2])
