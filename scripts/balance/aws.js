const assert = require("assert")

const inquirer = require("inquirer")
const Nightmare = require("nightmare")

const {prettyPrint} = require("../helpers.js")

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

  if (isDevMode) return "1234.56 €"

  const nightmare = new Nightmare({show: showBrowser})
  const loginUrl = `https://www.amazon.com
    /ap/signin?openid.assoc_handle=aws
    &openid.return_to=https%3A%2F%2Fsignin.aws.amazon.com
      %2Foauth%3Fcoupled_root%3Dtrue%26response_type%3Dcode
      %26redirect_uri%3Dhttps%253A%252F%252Fconsole.aws.amazon.com
      %252Fbilling%252Fhome%253Fstate%253DhashArgs%252523credits
      %2526isauthcode%253Dtrue%26client_id%3Darn%253Aaws
      %253Aiam%253A%253A934814114565%253Auser%252Fportal-aws-auth
    &openid.mode=checkid_setup
    &openid.ns=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0
    &openid.identity=http%3A%2F%2Fspecs.openid.net%2Fauth
      %2F2.0%2Fidentifier_select
    &openid.claimed_id=http%3A%2F%2Fspecs.openid.net
      %2Fauth%2F2.0%2Fidentifier_select
    &action=&disableCorpSignUp=&clientContext=&marketPlaceId=&poolName=
    &authCookies=&pageId=aws.login
    &siteState=registering%2CEN_US&accountStatusPolicy=P1&sso=
    &openid.pape.preferred_auth_policies=MultifactorPhysical
    &openid.pape.max_auth_age=120
    &openid.ns.pape=http%3A%2F%2Fspecs.openid.net%2Fextensions%2Fpape%2F1.0
    &server=%2Fap%2Fsignin%3Fie%3DUTF8
    &accountPoolAlias=
    &forceMobileApp=0&language=EN_US&forceMobileLayout=0
    &email=${encodeURIComponent(username)}
  `.replace(/\s/g, "")
  // URl above redirects to billing page.
  // FIXME: Find better way to login
  // const billingUrl = 'https://console.aws.amazon.com/billing/home#credits'
  const valueContainer =
    ".awsui-table-container " +
    "> table > tbody > tr > td:nth-child(4) > span > span"

  log("Open login URL")
  await nightmare
    .goto(loginUrl)
    .wait("#ap_password")

  log("Log in")
  await nightmare
    .insert("#ap_password", password)
    .click("#signInSubmit-input")
    .wait(valueContainer)

  return await nightmare
    .evaluate(
      container => document
        .querySelector(container)
        .textContent
        .replace(/\$([0-9,.]+)$/, "$1 $")
        .replace(/,/g, ""),
      valueContainer,
    )
    .end()
}

const prompValues = [
  {
    type: "input",
    name: "username",
    message: "AWS Username:",
  },
  {
    type: "password",
    name: "password",
    message: "AWS Password:",
  },
]

prompt(prompValues)
  .then(async answers => {
    try {
      const balance = await getBalance(answers)
      prettyPrint("aws.amazon.com", balance)
      process.exit(0)
    }
    catch (error) {
      console.error(error)
      process.exit(1)
    }
  })

