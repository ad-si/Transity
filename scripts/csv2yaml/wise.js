// Converts Wise balance statements (JSON, as downloaded by
// `scripts/transactions/wise.js --json`) to Transity YAML.
//
// Usage:
//   node wise.js <statements.json> [--owner <id>] [--after <YYYY-MM-DD>]
//
// All balances are booked to the `<owner>:wise` account.
// Payments in other currencies, conversions, and fund trades
// (Wise "Interest"/"Stocks") are booked as exchanges with the
// `wise` entity, so that Transity can derive the exchange rates.
// Money added from a bank account is booked from `_todo_`
// and must be checked against the entry of the bank account.

import { realpathSync } from "node:fs"
import { pathToFileURL } from "node:url"

import fse from "fs-extra"
import yaml from "js-yaml"

import {
  absolute,
  formatScaled,
  sanitizeYaml,
  toScaled,
} from "../helpers.js"


const provider = "wise"


function commodity (currency) {
  return currency === "EUR" ? "€" : currency
}

// Amounts are JSON numbers like `{value: -120.16, currency: "EUR"}`
function scaledValue (amount) {
  return toScaled(String(amount?.value ?? 0))
}

function formatAmount (scaled, currency, decimals = 2) {
  return `${formatScaled(absolute(scaled), decimals)} ${commodity(currency)}`
}

// "2026-08-06T13:13:32.072193Z" → "2026-08-06 13:13:32"
function toUtc (isoDate) {
  return isoDate.slice(0, 19)
    .replace("T", " ")
}


function counterpartyOf (details) {
  return details.recipient?.name ||
    details.senderName ||
    details.merchant?.name ||
    null
}


// Fund trades of Wise "Interest" or "Stocks".
// A buy moves cash into the fund, a sell (e.g. to pay a fee) back.
function tradeTransfers (attributions, account) {
  const transfers = []
  for (const attribution of attributions ?? []) {
    if (attribution.state !== "FINALISED") {
      console.warn(
        `Skipped ${attribution.state} trade of ${attribution.assetId.value}`)
      continue
    }
    const isin = attribution.assetId.value
    const money = formatAmount(
      scaledValue(attribution.stepAmount),
      attribution.stepAmount.currency,
    )
    const units = `${formatScaled(toScaled(String(attribution.tradedUnits)))} ` +
      isin
    const utc = toUtc(attribution.tradeTime)

    if (attribution.tradeSide === "BUY") {
      transfers.push(
        { utc, from: account, to: provider, amount: money },
        { utc, from: provider, to: account, amount: units },
      )
    }
    else {
      transfers.push(
        { utc, from: account, to: provider, amount: units },
        { utc, from: provider, to: account, amount: money },
      )
    }
  }
  return transfers
}


export function statementTransactionToTransaction (entry, account) {
  const { details } = entry
  const amount = scaledValue(entry.amount)
  const fee = scaledValue(entry.totalFees)
  const currency = entry.amount.currency
  const counterparty = counterpartyOf(details) || "_todo_"
  const exchange = entry.exchangeDetails

  const transaction = {
    utc: toUtc(entry.date),
    note: [details.description, details.paymentReference]
      .filter(Boolean)
      .join(" | "),
    "transaction-id": entry.referenceNumber,
  }
  const transfers = []

  if (entry.type === "DEBIT" && amount !== 0n) {
    // The debited amount includes the fees
    const principal = absolute(amount) - fee

    if (details.type === "ACCRUAL_CHARGE") {
      // Fees of Wise "Interest"/"Stocks"
      transfers.push({
        from: account,
        to: provider,
        amount: formatAmount(absolute(amount), currency),
        tags: ["fees"],
      })
    }
    else if (details.type === "CONVERSION") {
      // The other balance has a matching credit entry
      transfers.push({
        from: account,
        to: provider,
        amount: formatAmount(principal, currency),
      })
    }
    else if (exchange && exchange.toAmount.currency !== currency) {
      transfers.push(
        {
          from: account,
          to: provider,
          amount: formatAmount(principal, currency),
        },
        {
          from: provider,
          to: counterparty,
          amount: formatAmount(
            scaledValue(exchange.toAmount),
            exchange.toAmount.currency,
          ),
        },
      )
    }
    else {
      transfers.push({
        from: account,
        to: counterparty,
        amount: formatAmount(principal, currency),
      })
    }

    if (fee !== 0n && details.type !== "ACCRUAL_CHARGE") {
      transfers.push({
        from: account,
        to: provider,
        amount: formatAmount(fee, currency),
        tags: ["fees"],
      })
    }
  }
  else if (entry.type === "CREDIT" && amount !== 0n) {
    transfers.push({
      from: details.type === "CONVERSION" ? provider : counterparty,
      to: account,
      amount: formatAmount(amount + fee, currency),
    })
    if (fee !== 0n) {
      transfers.push({
        from: account,
        to: provider,
        amount: formatAmount(fee, currency),
        tags: ["fees"],
      })
    }
  }

  transfers.push(...tradeTransfers(entry.activityAssetAttributions, account))

  if (transfers.length === 0) {
    console.warn(
      `Skipped ${details.type} on ${transaction.utc}: Nothing was transferred`)
    return null
  }
  if (transfers.some(transfer =>
    transfer.from === "_todo_" || transfer.to === "_todo_")) {
    console.warn(`Unknown counterparty for ${details.type} ` +
      `on ${transaction.utc}: ${entry.amount.value} ${currency}`)
  }

  transaction.transfers = transfers
  return transaction
}


// `statements` is a list of balance statements
// as returned by the Wise API (`statement.json`)
export function convert (statements, options = {}) {
  const { owner = "owner", after = null } = options
  const account = `${owner}:wise`

  return statements
    .flatMap(statement => statement.transactions)
    .filter(entry => !after || entry.date.slice(0, 10) > after)
    // Oldest first
    .sort((entryA, entryB) => entryA.date.localeCompare(entryB.date))
    .map(entry => statementTransactionToTransaction(entry, account))
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
      "Usage: wise.js <statements.json> [--owner <id>] [--after <date>]")
    process.exitCode = 1
    return
  }
  const statements = JSON.parse(await fse.readFile(filePath, "utf-8"))
  const transactions = convert(statements, options)
  console.info(sanitizeYaml(yaml.dump({transactions}, {lineWidth: -1})))
}

if (import.meta.url === pathToFileURL(realpathSync(process.argv[1])).href) {
  main()
}
