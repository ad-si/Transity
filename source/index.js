const path = require('path')
const assert = require('assert')

const Ybdb = require('ybdb')
const math = require('mathjs')
const lodash = require('lodash')
const chalk = require('chalk')
const {Instant} = require('@datatypes/moment')

const debug = false


function addAccountAndCommodityToMap (account, commodity, map) {
  assert(account)
  assert(commodity)
  assert(map)

  if (!map.has(account)) {
    map.set(account, new Map())
  }

  const commodityQuantityMap = map.get(account)

  if (!commodityQuantityMap.has(commodity)) {
    commodityQuantityMap.set(commodity, 0)
  }
}

function toDatetimeArray (date) {
  const unknown = ['????-??-??', ' '.repeat(5)]
  if (!date) return unknown

  return new Instant(date)
    .toISOString()
    .slice(0, 16)
    .split('T')
}

function getPaddedQuantity (quantity, quantityWidth) {
  if (typeof quantity !== 'number') {
    throw new Error(`Quantity must be a number and not "${quantity}"`)
  }
  quantity = Number(quantity)
  const quantityStr = quantity.toString()
  const quantityString = quantityStr.includes('.')
    ? /\.[0-9]{1,2}$/.test(quantityStr)
      ? quantity.toFixed(2)
      : quantityStr
    : quantityStr + '   '

  const dimmedString = quantityString
    .padStart(quantityWidth)
    .split('.')
    .map((fragment, index) => {
      if (index === 0) return fragment
      else return chalk.dim(fragment)
    })
    .join(chalk.dim('.'))

  if (quantity < 0) return chalk.red(dimmedString)
  // Avoids output "-0"
  else if (quantity === 0) return chalk.dim('0   '.padStart(quantityWidth))
  else return dimmedString
}

function debugLog (value) {
  if (debug) {
    // eslint-disable-next-line no-console
    console.dir(value, {depth: null, colors: true})
  }
}

function normalizeTransfer (transfer) {
  const stringify = val => val ? String(val) : ''
  const numberify = val => val ? Number(val) : 0
  let transObj = {}

  debugLog(transfer)

  if (Array.isArray(transfer)) {
    const [quantity, ...commodityFrags] = stringify(transfer[2])
      .split(' ')

    transObj = {
      utc: transfer[0],
      from: transfer[1],
      to: transfer[3],
      quantity: quantity,
      commodity: commodityFrags.join(' '),
    }

    if (typeof transfer[4] !== 'undefined') {
      transObj.return = transfer[4]
    }
  }
  else {
    transObj = transfer
  }

  if (transObj.hasOwnProperty('amount')) {
    const amountFrags = stringify(transObj.amount)
      .split(' ')
    delete transObj.amount
    transObj.quantity = transObj.quantity
      ? transObj.quantity
      : amountFrags[0]
    transObj.commodity = transObj.commodity
      ? transObj.commodity
      : amountFrags[1]
  }

  const date = transObj['value-date'] ||
    transObj.date ||
    transObj.utc

  const normalizedTransfer = {
    utc: date ? new Instant(date) : undefined,
    from: stringify(transObj.from),
    to: stringify(transObj.to),
    quantity: numberify(transObj.quantity),
    commodity: stringify(transObj.commodity),
    title: stringify(transObj.title || transObj.desc),
  }

  debugLog(normalizedTransfer)

  return normalizedTransfer
}


async function getDb (config) {
  const db = new Ybdb({
    storagePaths: [
      path.join(config.directory, 'accounts.yaml'),
      path.join(config.directory, 'commodities.yaml'),
      path.join(config.directory, 'transactions'),
    ],
  })

  const initializedDb = await db.init()

  initializedDb._.mixin({
    log: array => {
      // eslint-disable-next-line no-console
      console.dir(array, {depth: null, colors: true})
      return array
    },
    toAccountEntries: map => {
      return Array
        .from(map.entries())
    },
    printBalance: accountEntries => {
      const accountWidth = 40
      const byAlphabet = (entryA, entryB) =>
        entryA[0].localeCompare(entryB[0])

      // TODO: Make available via cli flag
      // eslint-disable-next-line
      const byBalance = (entryA, entryB) => {
        const valueA = entryA[1]
          .values()
          .next().value
        const valueB = entryB[1]
          .values()
          .next().value
        return valueB - valueA
      }

      accountEntries
        .sort(byAlphabet)
        .forEach(entry => {
          const [account, commodityMap] = entry
          process.stdout.write(account.padStart(accountWidth))

          let firstRun = true
          const quantityWidth = 11
          const commodityWidth = 15

          commodityMap.forEach((quantity, commodity) => {
            if (!firstRun) {
              // Correctly indent accounts with several commodities
              process.stdout.write(' '.repeat(accountWidth))
            }

            const paddedQuantity = getPaddedQuantity(quantity, quantityWidth)
            const paddedCommodity = commodity.padEnd(commodityWidth)
            console.info(`${paddedQuantity} ${paddedCommodity}`)

            firstRun = false
          })
        })
    },
    normalizeTransactions: array => array.map(transaction => {
      let realTransaction = {}
      let transactionMeta = {}

      if (!Array.isArray(transaction.transfers)) {
        const title = transaction.title || transaction.desc
        // Transaction is actually just one transfer
        realTransaction = {
          title,
          transfers: [transaction],
        }
        transactionMeta = {title}
      }
      else {
        realTransaction = transaction
        transactionMeta = lodash.cloneDeep(
          lodash.omit(transaction, ['transfers'])
        )
        delete transactionMeta.transfers
      }

      realTransaction.transfers = realTransaction.transfers
        .map(transfer => {
          debugLog('++++++++++++++++++++++++++++++++++++++++++++++++++++++')
          debugLog(transfer)
          // Merge meta data from transaction into transfers
          const fullTransfer = Object.assign({}, transactionMeta, transfer)
          return normalizeTransfer(fullTransfer)
        })

      delete realTransaction.date
      delete realTransaction['entry-date']
      delete realTransaction['value-date']
      delete realTransaction.amount
      delete realTransaction.from
      delete realTransaction.to
      delete realTransaction.quantity

      return realTransaction
    }),
    reduceToTransfers: array => array.reduce(
      (transfers, current) => transfers.concat(current.transfers),
      []
    ),
    sortByDate: array => array.sort(
      (itemA, itemB) => {
        if (!itemA.utc) {
          if (!itemB.utc) return 0
          else return 1
        }
        if (!itemB.utc) return -1
        return itemA.utc - itemB.utc
      }
    ),
    removeEmptyAccounts: accountEntries => {
      return accountEntries
        .filter(accountEntry => Array
          .from(accountEntry[1].values())
          .some(balance => balance !== 0)
        )
        .map(accountEntry => {
          accountEntry[1].forEach((balance, commodity) => {
            if (balance === 0) accountEntry[1].delete(commodity)
          })
          return accountEntry
        })
    },
    printTransfers: transfers => transfers.forEach(transfer => {
      const datetimeArray = toDatetimeArray(transfer.utc)
      console.info(
        '%s %s | %s => %s | %s %s | %s',
        datetimeArray[0],
        datetimeArray[1] !== '00:00'
          ? datetimeArray[1]
          : ' '.repeat(5),
        transfer.from.padEnd(15), // eslint-disable-line
        transfer.to.padEnd(20), // eslint-disable-line
        getPaddedQuantity(transfer.quantity, 8),
        transfer.commodity.padEnd(3),  // eslint-disable-line
        transfer.title.slice(0, 50),  // eslint-disable-line
      )
    }),
  })

  return initializedDb
}

async function _renderBalance (initalizedDb) {
  const isValidTransfer = transfer =>
    transfer.from &&
    transfer.to &&
    transfer.quantity &&
    transfer.commodity
  const normalizedTransactions = initalizedDb
    .get('transactions')
    .flatten() // TODO: This should already be retured flattened from ybdb
    .normalizeTransactions()

  /*
    Data-structure for balance:
    Map {
      'john' => Map { '€' => 0, '$' => 0 },
      'anna' => Map { '€' => 0 },
    }
  */
  const dataStructureForBalance = normalizedTransactions
    .reduce(
      (accountBalanceMap, transaction) => {
        transaction.transfers
          .filter(isValidTransfer)
          .forEach(transfer => {
            addAccountAndCommodityToMap(
              transfer.from,
              transfer.commodity,
              accountBalanceMap
            )
            addAccountAndCommodityToMap(
              transfer.to,
              transfer.commodity,
              accountBalanceMap
            )
          })
        return accountBalanceMap
      },
      new Map()
    )
    .value()

  // TODO: Print warnings for accounts which are referenced
  // in transfers, but not defined in the accounts list

  return normalizedTransactions
    .reduce(
      (accountToBalance, transaction) => {
        transaction.transfers
          .filter(isValidTransfer)
          .forEach(transfer => {
            const from = accountToBalance.get(transfer.from)
            const to = accountToBalance.get(transfer.to)

            from.set(
              transfer.commodity,
              math.number(
                math
                  .chain(math.bignumber(from.get(transfer.commodity)))
                  .subtract(math.bignumber(Number(transfer.quantity)))
                  .valueOf()
              )
            )

            to.set(
              transfer.commodity,
              math.number(
                math
                  .chain(math.bignumber(to.get(transfer.commodity)))
                  .add(math.bignumber(Number(transfer.quantity)))
                  .valueOf()
              )
            )
          })
        return accountToBalance
      },
      dataStructureForBalance
    )
    .toAccountEntries()
    .removeEmptyAccounts()
    .printBalance()
    .value()
}

async function _renderTransfers (initalizedDb) {
  initalizedDb
    .get('transactions')
    .flatten() // TODO: This should already be retured flattened from ybdb
    .normalizeTransactions()
    .reduceToTransfers()
    .sortByDate()
    .printTransfers()
    .value()
}


module.exports = {
  async renderBalance (config) {
    _renderBalance(await getDb(config))
  },

  async renderTransfers (config) {
    _renderTransfers(await getDb(config))
  },
}
