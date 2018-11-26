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
    isDevMode,
  } = options

  if (isDevMode) return '1234.56 €'

  const nightmare = new Nightmare({show: showBrowser})
  const baseUrl = 'https://portokasse.deutschepost.de'


  log(`Open ${baseUrl}`)
  await nightmare
    .goto(baseUrl)
    .wait('#email')


  log('Log in')
  await nightmare
    .insert('#email', username)
    .insert('#password', password)
    .click('button.actionbutton[type=submit]')
    .wait('#txtWalletBalance')


  log('Retrieve current balance')
  return await nightmare
    .evaluate(() => document
      .querySelector('#txtWalletBalance')
      .textContent
      .replace(/,(\d\d)\xa0€$/, '.$1 €')
    )
    .end()
}


inquirer
  .prompt([
    {
      type: 'input',
      name: 'username',
      message: 'Portokasse Username:',
    },
    {
      type: 'password',
      name: 'password',
      message: 'Portokasse Password:',
    },
  ])
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint('portokasse.deutschepost.de', balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
