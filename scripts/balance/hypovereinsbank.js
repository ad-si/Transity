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
  const baseUrl = 'https://my.hypovereinsbank.de'
  const loginUrl = `${baseUrl}/login?view=/de/login.jsp`


  log(`Open ${loginUrl}`)
  await nightmare
    .goto(loginUrl)
    .wait('#loginPanel')


  log('Log in')
  await nightmare
    .insert('#username', username)
    .insert('#px2', password)
    .click('#loginCommandButton')
    .wait('.startpagemoney')


  log('Retrieve current balance')
  return await nightmare
    .evaluate(
      selector => document
        .querySelector(selector)
        .textContent
        .replace(/\./g, '')
        .replace(/,/g, '.')
        .replace(/EUR/, ' €'),
      '.startpagemoney'
    )
    .end()
}

const promptValues = [
  {
    type: 'input',
    name: 'username',
    message: 'HypoVereinsbank Username:',
  },
  {
    type: 'password',
    name: 'password',
    message: 'HypoVereinsbank Password:',
  },
]

prompt(promptValues)
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint('hypovereinsbank.de', balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })
