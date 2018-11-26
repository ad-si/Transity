const assert = require('assert')

const inquirer = require('inquirer')
const Nightmare = require('nightmare')

const {prettyPrint} = require('../helpers.js')

const log = process.env.NODE_DEBUG
  ? console.warn
  : () => {}


async function getBalance (options = {}) {
  const {
    showBrowser = true,
    username,
    password,
    isDevMode = false,
  } = options

  assert(username)
  assert(password)

  if (isDevMode) return '1234.56 €'

  const nightmare = new Nightmare({show: showBrowser})
  const baseUrl = 'https://www.dkb.de'
  const loginUrl = `${baseUrl}/banking`


  log(`Open ${loginUrl}`)
  await nightmare
    .goto(loginUrl)
    .wait('#login')


  log('Log in')
  await nightmare
    .insert('#loginInputSelector', username)
    .insert('#pinInputSelector', password)
    .click('#buttonlogin')
    .wait('#financialStatus')


  log('Retrieve current balance')
  const balance = await nightmare
    .evaluate(
      selector => document
        .querySelector(selector)
        .textContent
        .replace(/\./g, '')
        .replace(/,/g, '.'),
      '#financialStatus #summe-gruppe-0 strong span'
    )
    .end()

  return balance + ' €'
}


inquirer
  .prompt([
    {
      type: 'input',
      name: 'username',
      message: 'DKB Username:',
    },
    {
      type: 'password',
      name: 'password',
      message: 'DKB Password:',
    },
  ])
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint('dkb.de', balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
