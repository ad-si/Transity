// Downloads the statements of all Wise balances
// and prints them as Transity YAML (or as the raw JSON with `--json`).
//
// Usage:
//   node wise.js [--from <YYYY-MM-DD>] [--to <YYYY-MM-DD>]
//                [--owner <id>] [--json]
//
// `--from` defaults to 90 days ago, `--to` to today,
// `--owner` (prefix of the `<owner>:wise` account) to "owner".
//
// Environment: WISE_USERNAME (email), WISE_PASSWORD,
// else manual login in the browser.
// WISE_PROFILE (browser profile name, default "wise").
// The login must be confirmed in the Wise app.
//
// The Wise API does not serve statements of EU/UK personal accounts
// to personal API tokens anymore. Therefore the statements are requested
// via the gateway of the web app with the session of the logged in page.

import { convert } from "../csv2yaml/wise.js"
import {
  dumpDebugFiles,
  getCredentials,
  launchBrowser,
} from "../browser.js"
import { sanitizeYaml } from "../helpers.js"

import yaml from "js-yaml"

const log = console.warn
const baseUrl = "https://wise.com"
// Public token of the web app, the session is identified by its cookies
const webAppToken = "Tr4n5f3rw153"
// The API rejects longer statement intervals (max. 469 days)
const maxIntervalDays = 365


function toIsoDate (date) {
  return date.toISOString()
    .slice(0, 10)
}

function addDays (isoDate, days) {
  const date = new Date(isoDate)
  date.setUTCDate(date.getUTCDate() + days)
  return toIsoDate(date)
}


function parseArgs (args) {
  const ninetyDaysAgo = new Date()
  ninetyDaysAgo.setDate(ninetyDaysAgo.getDate() - 90)
  const options = {
    from: toIsoDate(ninetyDaysAgo),
    to: toIsoDate(new Date()),
    owner: "owner",
    shallPrintJson: false,
  }

  for (let pos = 0; pos < args.length; pos++) {
    if (["--from", "--to", "--owner"].includes(args[pos])) {
      options[args[pos].slice(2)] = args[++pos]
    }
    else if (args[pos] === "--json") {
      options.shallPrintJson = true
    }
    else {
      throw new Error(`Unknown argument "${args[pos]}"`)
    }
  }

  for (const key of ["from", "to"]) {
    if (!/^\d{4}-\d{2}-\d{2}$/.test(options[key])) {
      throw new Error(`--${key} must be formatted as YYYY-MM-DD`)
    }
  }

  return options
}


function isLoginPage (url) {
  return /^\/(login|signin|account-selector)/.test(new URL(url).pathname)
}


async function login (page, credentials) {
  log(`Open ${baseUrl}/home`)
  await page.goto(`${baseUrl}/home`, {timeout: 60000})
  await page.waitForTimeout(3000)

  if (!isLoginPage(page.url())) {
    log("Still logged in from a previous session")
    return
  }

  if (credentials.username && credentials.password) {
    try {
      log("Enter email and password")
      await page.fill("input[name=email]", credentials.username,
        {timeout: 30000})
      await page.fill("input[name=password]", credentials.password)
      await page.keyboard.press("Enter")
    }
    catch (error) {
      log(`Automatic login failed (${error.message.split("\n")[0]}), ` +
        "please continue manually in the browser")
    }
  }
  else {
    log("Please log in manually in the browser …")
  }

  log("Wait for login (confirm it in the Wise app) …")
  await page.waitForURL(url => !isLoginPage(url.href), {timeout: 600000})
}


// Requests the gateway of the web app from within the logged in page
async function gatewayGet (page, path) {
  return page.evaluate(async ({url, token}) => {
    const response = await fetch(url, {
      credentials: "include",
      headers: {
        "accept": "application/json",
        "x-access-token": token,
      },
    })
    if (!response.ok) {
      throw new Error(
        `${url} failed with ${response.status}: ${await response.text()}`)
    }
    return response.json()
  }, {url: `${baseUrl}/gateway${path}`, token: webAppToken})
}


async function getPersonalProfileId (page) {
  const profiles = await gatewayGet(page, "/v1/profiles")
  const profile = profiles.find(aProfile => aProfile.type === "personal")
  if (!profile) {
    throw new Error("No personal Wise profile found")
  }
  return profile.id
}


async function getStatements (page, {from, to}) {
  const profileId = await getPersonalProfileId(page)
  const balances = await gatewayGet(page,
    `/v4/profiles/${profileId}/balances?types=STANDARD,SAVINGS`)
  const statements = []

  for (const balance of balances) {
    log(`Download statement of ${balance.currency} balance ${balance.id} ` +
      `from ${from} to ${to}`)
    const statement = {...balance, transactions: []}

    for (
      let start = from;
      start <= to;
      start = addDays(start, maxIntervalDays)
    ) {
      const endExclusive = [addDays(start, maxIntervalDays), addDays(to, 1)]
        .sort()[0]
      const part = await gatewayGet(page,
        `/v1/profiles/${profileId}/balance-statements/${balance.id}` +
        `/statement.json?currency=${balance.currency}&type=COMPACT` +
        `&intervalStart=${start}T00:00:00.000Z` +
        `&intervalEnd=${endExclusive}T00:00:00.000Z`)
      statement.transactions.push(...part.transactions)
    }

    statements.push(statement)
  }

  return statements
}


async function main () {
  const options = parseArgs(process.argv.slice(2))
  const credentials = await getCredentials("WISE", "Wise")

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: process.env.WISE_PROFILE || "wise",
  })

  try {
    await login(page, credentials)
    const statements = await getStatements(page, options)

    if (options.shallPrintJson) {
      console.info(JSON.stringify(statements, null, 2))
    }
    else {
      const transactions = convert(statements, {owner: options.owner})
      console.info(sanitizeYaml(yaml.dump({transactions}, {lineWidth: -1})))
    }
  }
  catch (error) {
    await dumpDebugFiles(page, "wise-debug")
    throw error
  }
  finally {
    await browser.close()
  }
}


main()
  .catch(error => {
    console.error(error)
    process.exitCode = 1
  })
