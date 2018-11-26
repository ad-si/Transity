function prettyFormat (account, balance) {
  const paddingStart = 28
  return account.padStart(paddingStart) + ': ' + balance
}

function prettyPrint (account, balance) {
  console.info(prettyFormat(account, balance))
}

module.exports = {prettyFormat, prettyPrint}
