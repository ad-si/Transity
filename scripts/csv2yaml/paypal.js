import fse from "fs-extra"
import yaml from "js-yaml"
import converter from "converter"
import chrono from "chrono-node"

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
      .map(transaction => {
        const currency = transaction.currency
          .replace("EUR", "€")
          .replace("USD", "$")
          .trim()

        const sortedTransaction = Object.assign(
          {
            utc: chrono
              .parseDate([
                transaction.date,
                transaction.time,
                transaction.timezone,
              ].join(" "))
              .toISOString()
              .replace("T", " ")
              .replace(".000Z", ""),
            note: transaction.subject,
          },
          transaction,
        )
        const account = noteToAccount(transaction.name)
        const transfer = transaction.gross.startsWith("-")
          ? {
            from: "_todo_:paypal:" +
              transaction.currency
                .toLowerCase()
                .trim(),
            to: account ? account : "paypal",
            amount: transaction.gross.slice(1) + " " + currency,
          }
          : {
            from: account ? account : "paypal",
            to: "_todo_:paypal:" +
              transaction.currency
                .toLowerCase()
                .trim(),
            amount: transaction.gross + " " + currency,
          }

        const newTransaction = Object.assign(
          sortedTransaction,
          {transfers: [transfer]},
        )

        if (Number(transaction.fee) !== 0) {
          newTransaction.transfers.push({
            from: "_todo_:paypal:" +
              transaction.currency
                .toLowerCase()
                .trim(),
            to: "paypal",
            amount: transaction.fee.slice(1) + " " + currency,
            tags: ["fee"],
          })
        }

        delete newTransaction.subject
        delete newTransaction.name
        delete newTransaction.balance
        delete newTransaction.gross
        delete newTransaction.net
        delete newTransaction.fee
        delete newTransaction.currency
        delete newTransaction.date
        delete newTransaction.time
        delete newTransaction.timezone
        delete newTransaction.from_email_address
        delete newTransaction.to_email_address

        return JSON.parse(JSON.stringify(newTransaction, rmEmptyString))
      })

    const yamlString = sanitizeYaml(yaml.dump({transactions}))

    console.info(yamlString)
  })

  csvnorm.default({
    dateFormat: "dd/mm/yyyy",
    readableStream: fse.createReadStream(filePathTemp),
    writableStream: csv2json,
  })
}

normalizeAndPrint(process.argv[2])
