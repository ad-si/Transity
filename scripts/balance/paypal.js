const assert = require('assert')

const inquirer = require('inquirer')
const Nightmare = require('nightmare')

const {prettyPrint} = require('../helpers.js')

const log = process.env.NODE_DEBUG
  ? console.warn
  : () => {}



let paypalBalancesPromise = null
function getPaypalBalanceFunc (commodity) {
  return async config => {
    if (paypalBalancesPromise == null) {
      paypalBalancesPromise = getPaypalBalances(config)
    }

    const balances = await paypalBalancesPromise
    const balanceMap = {
      eur: balances[0],
      usd: balances[1],
    }
    return balanceMap[commodity]
  }
}


async function getBalance (options = {}) {
  const {
    showBrowser = true,
    username,
    password,
    isDevMode = false,
  } = options

  assert(username)
  assert(password)

  if (isDevMode) {
    return ['1234.56 €', '1234.56 €']
  }

  const nightmare = new Nightmare({show: showBrowser})
  const baseUrl = 'https://www.paypal.com'
  const balanceURl = 'https://www.paypal.com/businessexp/money'
  const loginUrl = `${baseUrl}/signin?returnUri=${
    encodeURIComponent(balanceURl)}`


  log(`Open ${loginUrl}`)
  await nightmare
    .goto(loginUrl)
    .wait('#email')

  log('Enter email')
  await nightmare
    .insert('#email', username)
    .click('#btnNext')
    .wait(() => !document
      .getElementById('splitPassword').classList
      .contains('hide')
    )

  log('Enter password')
  await nightmare
    .insert('#password', password)
    .click('#btnLogin')
    .wait('.multi-currency')

  log('Retrieve current balances')
  return await nightmare
    .evaluate(
      selector => Array
        .from(document.querySelectorAll(selector))
        .map(element => element
          .childNodes[0]
          .nodeValue
          .replace(/,(\d\d)\xa0(\w{3})(\n= .+\n)?/g, '.$1 $2')
          .replace('EUR', '€')
          .replace('USD', '$')
        ),
      '.multi-currency .currency-amt-newexp'
    )
    .end()
}


inquirer
  .prompt([
    {
      type: 'input',
      name: 'username',
      message: 'PayPal Username:',
    },
    {
      type: 'password',
      name: 'password',
      message: 'PayPal Password:',
    },
  ])
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint('paypal.com', balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
