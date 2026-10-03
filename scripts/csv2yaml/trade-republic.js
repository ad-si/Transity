// Converts the Trade Republic transaction export CSV
// (Profile → Transaction export) to Transity YAML.
//
// Usage:
//   node trade-republic.js <file.csv> [--owner <id>] [--after <YYYY-MM-DD>]
//
// Trade Republic has two accounts per customer:
// "cash" (the clearing / savings account) and "wealth" (the securities).
// Trades and corporate actions move the shares (by ISIN)
// between the wealth account and the `trade_republic` entity,
// while all money moves through the cash account.
// Amounts are booked net of fees and withheld taxes,
// as only the net amount reaches the cash account.

import { realpathSync } from "node:fs"
import { pathToFileURL } from "node:url"

import fse from "fs-extra"
import yaml from "js-yaml"

import {
  absolute,
  formatScaled,
  sanitizeYaml,
  scale,
  toScaled,
} from "../helpers.js"


// RFC 4180 CSV parser (quoted fields may contain commas, quotes, newlines)
export function parseCsv (text) {
  const rows = []
  let row = []
  let field = ""
  let isQuoted = false

  for (let pos = 0; pos < text.length; pos++) {
    const char = text[pos]
    if (isQuoted) {
      if (char === "\"" && text[pos + 1] === "\"") {
        field += "\""
        pos++
      }
      else if (char === "\"") {
        isQuoted = false
      }
      else {
        field += char
      }
    }
    else if (char === "\"") {
      isQuoted = true
    }
    else if (char === ",") {
      row.push(field)
      field = ""
    }
    else if (char === "\n" || char === "\r") {
      if (char === "\r" && text[pos + 1] === "\n") {
        pos++
      }
      row.push(field)
      rows.push(row)
      row = []
      field = ""
    }
    else {
      field += char
    }
  }
  if (field || row.length > 0) {
    row.push(field)
    rows.push(row)
  }

  const [header, ...records] = rows.filter(aRow => aRow.some(Boolean))
  return records.map(record =>
    Object.fromEntries(header.map((key, index) => [key, record[index] ?? ""])))
}


export function rowToTransaction (row, accounts) {
  const { cash, wealth, broker } = accounts
  const net = toScaled(row.amount) + toScaled(row.fee) + toScaled(row.tax)
  const shares = toScaled(row.shares)
  const currency = row.currency === "EUR" ? "€" : row.currency
  const isin = row.symbol

  const transaction = {
    utc: row.date,
    note: row.description,
    "transaction-id": row.transaction_id,
  }
  const transfers = []

  function moneyTransfer (counterparty) {
    const amount = `${formatScaled(absolute(net), 2)} ${currency}`
    return net < 0n
      ? { from: cash, to: counterparty, amount }
      : { from: counterparty, to: cash, amount }
  }

  function sharesTransfer () {
    const amount = `${formatScaled(absolute(shares))} ${isin}`
    return shares < 0n
      ? { from: wealth, to: broker, amount }
      : { from: broker, to: wealth, amount }
  }

  if (row.category === "TRADING") {
    if (row.price) {
      transaction["price-per-share"] =
        `${formatScaled(toScaled(row.price))} ${currency}`
    }
    transfers.push(moneyTransfer(broker), sharesTransfer())
  }
  else if (row.category === "CORPORATE_ACTION") {
    transaction.note ||= `${row.type} ${isin}`
    transaction.note += ` - ${row.name}, shares: ${formatScaled(shares)}`
    if (shares !== 0n) {
      transfers.push(sharesTransfer())
    }
    if (net !== 0n) {
      transfers.push(moneyTransfer(broker))
    }
  }
  else if (row.type === "REFERRAL") {
    transfers.push(moneyTransfer(broker))
  }
  else if (row.type === "INTEREST_PAYMENT") {
    transfers.push(moneyTransfer(`${broker}:_interest_`))
  }
  else if (row.type === "DIVIDEND") {
    if (!transaction.note) {
      const perShare = shares === 0n
        ? 0n
        : toScaled(row.original_amount) * scale / shares
      transaction.note = `${row.name} - Dividend per share of ` +
        `${formatScaled(perShare)} ${row.original_currency}`
    }
    transfers.push(moneyTransfer(`${broker}:_dividends_`))
  }
  else if (["EARNINGS", "PRE_DETERMINED_TAX_BASE"].includes(row.type)) {
    // Vorabpauschale: Only the withheld tax is debited
    transaction.note ||= `Vorabpauschale for ISIN ${isin}`
    transfers.push(moneyTransfer("tax_office"))
  }
  else {
    // Deposits, withdrawals, card payments, …
    const counterparty = row.counterparty_name || "_todo_"
    transaction.note = [
      row.description,
      row.counterparty_name,
      row.counterparty_iban,
      row.payment_reference,
    ]
      .filter(Boolean)
      .join(" | ")
    transaction.type = row.type
    transfers.push(moneyTransfer(counterparty))
    if (!row.counterparty_name) {
      console.warn(
        `Unknown counterparty for ${row.type} on ${row.date}: ` +
          `${row.amount} ${row.currency}`,
      )
    }
  }

  if (transfers.length === 0) {
    console.warn(`Skipped ${row.type} on ${row.date}: Nothing was transferred`)
    return null
  }

  transaction.transfers = transfers
  return JSON.parse(JSON.stringify(transaction, (key, value) =>
    value === "" ? undefined : value))
}


export function convert (csvText, options = {}) {
  const { owner = "owner", after = null } = options
  const accounts = {
    cash: `${owner}:trade_republic:cash`,
    wealth: `${owner}:trade_republic:wealth`,
    broker: "trade_republic",
  }

  return parseCsv(csvText)
    .filter(row => !after || row.date > after)
    .sort((rowA, rowB) =>
      // Oldest first
      (rowA.date + rowA.datetime).localeCompare(rowB.date + rowB.datetime))
    .map(row => rowToTransaction(row, accounts))
    .filter(Boolean)
}


function parseArgs (args) {
  const options = {}
  const positional = []
  for (let pos = 0; pos < args.length; pos++) {
    if (args[pos] === "--owner") {
      options.owner = args[++pos]
    }
    else if (args[pos] === "--after") {
      options.after = args[++pos]
    }
    else {
      positional.push(args[pos])
    }
  }
  return { filePath: positional[0], options }
}


async function main () {
  const { filePath, options } = parseArgs(process.argv.slice(2))
  if (!filePath) {
    console.error(
      "Usage: trade-republic.js <file.csv> [--owner <id>] [--after <date>]")
    process.exitCode = 1
    return
  }
  const transactions = convert(await fse.readFile(filePath, "utf-8"), options)
  console.info(sanitizeYaml(yaml.dump({transactions}, {lineWidth: -1})))
}

if (import.meta.url === pathToFileURL(realpathSync(process.argv[1])).href) {
  main()
}
