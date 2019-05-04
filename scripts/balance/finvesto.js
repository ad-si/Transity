const assert = require('assert')

const inquirer = require('inquirer')
const Nightmare = require('nightmare')

const {prettyPrint} = require('../helpers.js')

const prompt = inquirer.createPromptModule({ output: process.stderr })
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
  const baseUrl = 'https://portal.ebase.com'
  const loginUrl = `${baseUrl}/(e1)/finvesto`


  log(`Open ${loginUrl}`)
  await nightmare
    .goto(loginUrl)
    .wait('#loginfelder')


  log('Log in')
  await nightmare
    .insert('#eox_ContentPane_3_depotNrTextBox', username)
    .insert('#eox_ContentPane_3_pinTextBox', password)
    .click('#eox_ContentPane_3_LOGIN')
    .wait('.tabNavBody')


  log('Retrieve current balance')
  const balance = await nightmare
    .evaluate(
      selector => document
        .querySelector(selector)
        .textContent
        .replace(/\./g, '')
        .replace(/,/g, '.'),
      '#eox_ContentPane_4_VermoegensuebersichtBody1_' +
        'repeaterDepotsKonten_ctl02_lblBestandGesamt'
    )
    .end()

  return balance + ' €'
}

const promptValues = [
  {
    type: 'input',
    name: 'username',
    message: 'Finvesto Username:',
  },
  {
    type: 'password',
    name: 'password',
    message: 'Finvesto Password:',
  },
]

prompt(promptValues)
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint('Finvesto', balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
