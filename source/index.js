const path = require('path')
const Ybdb = require('ybdb')
const db = new Ybdb({
  storageFile: path.join(__dirname, 'database.yaml'),
})

db._.mixin({
  log: (array) => {
    // eslint-disable-next-line
    console.dir(array, {depth: null, colors: true})
    return array
  },
  normalizeTransactions: (array) => {
    return array.map(transaction => {
      if (!Array.isArray(transaction.transfers)) return transaction

      transaction.transfers = transaction.transfers.map(
        transfer => {
          if (!Array.isArray(transfer)) return transfer

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

const normalizedTransactions = db
  .get('transactions')
  .normalizeTransactions()


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

normalizedTransactions
  .reduce(
    (accountToBalance, transaction) => {
      transaction.transfers.forEach(transfer => {
        const from = accountToBalance.get(transfer.from)
        const to = accountToBalance.get(transfer.to)

        from.set(
          transfer.commodity,
          from.get(transfer.commodity) - Number(transfer.quantity)
        )

        to.set(
          transfer.commodity,
          to.get(transfer.commodity) + Number(transfer.quantity)
        )
      })
      return accountToBalance
    },
    dataStructureForBalance
  )
  .log()
  .value()


// console.dir(transactions, {depth: null, colors: true})
// console.dir(accounts, {depth: null, colors: true})
// console.dir(commodities, {depth: null, colors: true})
