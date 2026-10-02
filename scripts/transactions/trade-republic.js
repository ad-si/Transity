// Downloads the Trade Republic transaction export
// (cash and wealth account, incl. trades, dividends, interest, taxes)
// and prints it as Transity YAML (or as the raw CSV with `--csv`).
//
// Usage:
//   node trade-republic.js [--from <YYYY-MM-DD>] [--to <YYYY-MM-DD>]
//                          [--owner <id>] [--csv]
//
// `--from` defaults to 90 days ago, `--to` to today,
// `--owner` (prefix of the cash and wealth accounts) to "owner".
//
// Environment: TRADE_REPUBLIC_USERNAME (phone number),
// TRADE_REPUBLIC_PASSWORD (PIN), else manual login in the browser.
// TRADE_REPUBLIC_PROFILE (browser profile name, default "trade-republic").
// The login must always be confirmed in the app or via SMS code.
//
// The browser must be visible, as the AWS WAF in front of
// app.traderepublic.com does not let headless browsers through.

import { convert } from "../csv2yaml/trade-republic.js"
import {
  dumpDebugFiles,
  getCredentials,
  launchBrowser,
} from "../browser.js"
import { sanitizeYaml } from "../helpers.js"

import yaml from "js-yaml"

const log = console.warn
const appUrl = "https://app.traderepublic.com"
const exportUrl = "https://api.traderepublic.com/api/v1" +
  "/portfolio-analytics/transactions/export"


function toIsoDate (date) {
  return date.toISOString()
    .slice(0, 10)
}


function parseArgs (args) {
  const ninetyDaysAgo = new Date()
  ninetyDaysAgo.setDate(ninetyDaysAgo.getDate() - 90)
  const options = {
    from: toIsoDate(ninetyDaysAgo),
    to: toIsoDate(new Date()),
    owner: "owner",
    shallPrintCsv: false,
  }

  for (let pos = 0; pos < args.length; pos++) {
    if (["--from", "--to", "--owner"].includes(args[pos])) {
      options[args[pos].slice(2)] = args[++pos]
    }
    else if (args[pos] === "--csv") {
      options.shallPrintCsv = true
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


async function login (page, credentials) {
  log(`Open ${appUrl}/login`)
  await page.goto(`${appUrl}/login`, {timeout: 60000})
  await page.waitForTimeout(3000)

  if (!new URL(page.url()).pathname.startsWith("/login")) {
    log("Still logged in from a previous session")
    return
  }

  if (credentials.username && credentials.password) {
    try {
      log("Enter phone number")
      // The country code is selected separately and defaults to +49
      await page.fill(
        "input[name=username]",
        credentials.username.replace(/^\+49/, "")
          .replace(/\s/g, ""),
        {timeout: 30000},
      )
      await page.keyboard.press("Enter")

      log("Enter PIN")
      // The focus moves to the PIN field(s) automatically
      await page.waitForSelector("input[name=username]", {
        state: "detached",
        timeout: 30000,
      })
      await page.waitForTimeout(1000)
      await page.keyboard.type(credentials.password, {delay: 100})
    }
    catch (error) {
      log(`Automatic login failed (${error.message.split("\n")[0]}), ` +
        "please continue manually in the browser")
    }
  }
  else {
    log("Please log in manually in the browser …")
  }

  log("Wait for login (confirm it in the app or enter the SMS code) …")
  await page.waitForURL(url => !url.pathname.startsWith("/login"), {
    timeout: 600000,
  })
}


// Uses the same API as the web app's "Statements → Transaction export",
// with the session cookies of the logged in page
async function exportCsv (page, {from, to}) {
  log(`Export transactions from ${from} to ${to}`)

  return page.evaluate(async ({baseUrl, range}) => {
    const wafToken = document.cookie
      .match(/(?:^|; )aws-waf-token=([^;]+)/)?.[1]
    const headers = {
      "content-type": "application/json",
      "x-tr-platform": "web-pro",
    }
    if (wafToken) {
      headers["x-aws-waf-token"] = wafToken
    }

    async function request (path, init = {}) {
      const response = await fetch(`${baseUrl}/${path}`, {
        credentials: "include",
        headers,
        ...init,
      })
      if (!response.ok) {
        throw new Error(
          `${path} failed with ${response.status}: ${await response.text()}`)
      }
      return response
    }

    const { jobId } = await (await request("request", {
      method: "POST",
      body: JSON.stringify(range),
    })).json()

    for (let attempt = 0; attempt < 120; attempt++) {
      const { status } = await (await request(`status?jobId=${jobId}`)).json()
      if (status === "COMPLETED") {
        return (await request(`download?jobId=${jobId}`)).text()
      }
      if (["FAILED", "ERROR", "CANCELLED"].includes(status)) {
        throw new Error(`Export job ${jobId} ended with status ${status}`)
      }
      await new Promise(resolve => setTimeout(resolve, 1000))
    }
    throw new Error(`Export job ${jobId} did not complete in time`)
  }, {baseUrl: exportUrl, range: {from, to}})
}


async function main () {
  const options = parseArgs(process.argv.slice(2))
  const credentials = await getCredentials("TRADE_REPUBLIC", "Trade Republic")

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName:
      process.env.TRADE_REPUBLIC_PROFILE || "trade-republic",
  })

  try {
    await login(page, credentials)
    const csv = await exportCsv(page, options)

    if (options.shallPrintCsv) {
      process.stdout.write(csv)
    }
    else {
      const transactions = convert(csv, {owner: options.owner})
      console.info(sanitizeYaml(yaml.dump({transactions}, {lineWidth: -1})))
    }
  }
  catch (error) {
    await dumpDebugFiles(page, "trade-republic-debug")
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
