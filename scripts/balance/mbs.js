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
  const baseUrl = 'https://www.mbs.de'
  const loginUrl = `${baseUrl}/de/home.html`


  log(`Open ${loginUrl}`)
  await nightmare
    .goto(loginUrl)
    .wait('.loginlogout')


  log('Log in')
  await nightmare
    .insert('.loginlogout input[type=text]', username)
    .insert('.loginlogout input[type=password]', password)
    .click('input[value=Anmelden]')
    .wait('.mbf-finanzstatus')


  log('Retrieve current balance')
  return await nightmare
    .evaluate(
      selector => document
        .querySelector(selector)
        .textContent
        .replace(/\./g, '')
        .replace(/,/g, '.')
        .replace(/EUR/, '€'),
      '.mbf-finanzstatus .tablefooter .balance .offscreen'
    )
    .end()
}


prompt([
    {
      type: 'input',
      name: 'username',
      message: 'AWS Username:',
    },
    {
      type: 'password',
      name: 'password',
      message: 'AWS Password:',
    },
  ])
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint('mbs.de', balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
