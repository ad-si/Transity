import fse from "fs-extra"
import yaml from "js-yaml"
import converter from "converter"

import {
  rmEmptyString,
  keysToEnglish,
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
      .map(transac => {
        const currency = transac.currency
          .trim()
          .replace("EUR", "€")
        const sortedTransaction = Object.assign(
          {
            utc: transac.utc || transac["entry-utc"],
            "entry-utc": transac["entry-utc"],
            note: transac.Umsatzart + " " + transac.Fonds + " " + transac.isin,
          },
          transac,
        )

        const transfersObj = Number(transac["Entgelt in EUR"])
          ? {
            transfers: [
              {
                from: "_todo_:ebase",
                to: "ebase",
                amount: -transac.shares + " " + transac.isin,
              },
            ],
          }
          : {
            transfers: [
              {
                from: "_todo_",
                to: "ebase",
                amount: transac.amount + " " + currency,
              },
              {
                from: "ebase",
                to: "adrian:ebase",
                amount: transac.shares + " " + transac.isin,
              },
            ],
          }
        const newTransaction = Object.assign(sortedTransaction, transfersObj)

        delete newTransaction.amount
        delete newTransaction.currency
        delete newTransaction.shares
        delete newTransaction.isin
        if (newTransaction["entry-utc"] === newTransaction.utc) {
          delete newTransaction["entry-utc"]
        }
        if (newTransaction["value-utc"] === newTransaction.utc) {
          delete newTransaction["value-utc"]
        }

        return JSON.parse(JSON.stringify(newTransaction, rmEmptyString))
      })
      .sort((txA, txB) => txA.utc.localeCompare(txB.utc))

    const yamlString = sanitizeYaml(yaml.dump({transactions}))

    console.info(yamlString)
  })

  csvnorm.default({
    readableStream: fse.createReadStream(filePathTemp),
    writableStream: csv2json,
  })
}

normalizeAndPrint(process.argv[2])
