const path = require('path')
const Ybdb = require('ybdb')
const math = require('mathjs')
const lodash = require('lodash')

const dataPath = '/Users/adrian/Transactions'
const db = new Ybdb({
  storagePaths: [
    path.join(dataPath, 'accounts.yaml'),
    path.join(dataPath, 'commodities.yaml'),
    path.join(dataPath, 'transactions'),
  ],
})

function addAccountAndCommodityToMap (account, commodity, map) {
  if (!map.has(account)) {
    map.set(account, new Map())
  }

  const commodityQuantityMap = map
    .get(account)

  if (!commodityQuantityMap.has(commodity)) {
    commodityQuantityMap.set(commodity, 0)
  }
}


async function renderBalance () {
  const inintalizedDb = await db.init()

  inintalizedDb._.mixin({
    log: array => {
      // eslint-disable-next-line no-console
      console.dir(array, {depth: null, colors: true})
      return array
    },
    printBalance: map => {
      const accountWidth = 40
      map.forEach((commodityMap, account) => {
        account = (' '.repeat(accountWidth) + account).slice(-accountWidth)
        process.stdout.write(`${account} `)

        let firstRun = true
        const quantityWidth = 10
        const commodityWidth = 15

        commodityMap.forEach((quantity, commodity) => {
          if (!firstRun) {
            process.stdout.write(' '.repeat(accountWidth + 1))
          }

          const paddedQuantity = (
            ' '.repeat(quantityWidth) +
            quantity.toFixed(2)
          ).replace(/\.00$/, '   ')

          const paddedCommodity = commodity + ' '.repeat(commodityWidth)

          console.info(
            paddedQuantity.slice(-quantityWidth) + ' ' +
            paddedCommodity.slice(0, commodityWidth)
          )
          firstRun = false
        })
      })
    },
    normalizeTransactions: array => {
      return array.map(transaction => {
        if (!Array.isArray(transaction.transfers)) {
          // Was written in single transfer style
          const transfer = lodash.clone(transaction)
          delete transfer.date
          transfer.title = transfer.title || transaction.desc
          transaction.transfers = [transfer]

          delete transaction.amount
          delete transaction.from
          delete transaction.to
          delete transaction.quantity
        }

        transaction.transfers = transaction.transfers.map(
          transfer => {
            if (!Array.isArray(transfer)) {
              if (transfer.hasOwnProperty('amount')) {
                const amountFrags = transfer.amount.split(' ')
                delete transfer.amount
                transfer.quantity = transaction.quantity || amountFrags[0]
                transfer.commodity = transaction.commodity || amountFrags[1]
              }

              return transfer
            }

            const [quantity, ...commodityFrags] = transfer[2].split(' ')

            const normalizedTransfer = {
              utc: transfer[0],
              from: transfer[1],
              quantity,
              commodity: commodityFrags.join(' '),
              to: transfer[3],
            }

            if (typeof transfer[4] !== 'undefined') {
              normalizedTransfer.return = transfer[4]
            }

            return normalizedTransfer
          }
        )
        return transaction
      })
    },
  })

  const normalizedTransactions = inintalizedDb
    .get('transactions')
    .flatten() // TODO: This should already be retured flattened from ybdb
    .normalizeTransactions()

  // Data-structure for balance:
  // Map {
  //   'john' => Map { '€' => 0, '$' => 0 },
  //   'anna' => Map { '€' => 0 },
  // }
  const dataStructureForBalance = normalizedTransactions
    .reduce(
      (accountBalanceMap, transaction) => {
        transaction.transfers.forEach(transfer => {
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
        transaction.transfers.forEach(transfer => {
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
    .printBalance()
    .value()
}

renderBalance()
