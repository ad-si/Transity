import os from "node:os"
import path from "node:path"

import fse from "fs-extra"
import { chromium } from "playwright"
import { temporaryFile } from "tempy"


// Returns the "YYYY-MM" of the newest already downloaded statement
// in a `<baseDir>/<year>/<YYYY-MM><suffix>` directory tree (null if empty).
// Online archives reach back much further than the local repository,
// so this is used to only download statements which are actually missing.
export async function getNewestFiledMonth (baseDir, suffix) {
  if (!await fse.pathExists(baseDir)) {
    return null
  }

  const months = []
  for (const yearDir of await fse.readdir(baseDir)) {
    if (!/^\d{4}$/.test(yearDir)) {
      continue
    }
    for (const fileName of await fse.readdir(path.join(baseDir, yearDir))) {
      if (fileName.endsWith(suffix)) {
        months.push(fileName.slice(0, 7))
      }
    }
  }

  return months.length > 0
    ? months.sort()
      .at(-1)
    : null
}


// Returns `{username, password}` from the environment
// (`<PREFIX>_USERNAME`, `<PREFIX>_PASSWORD`, also via a `.env` file
// in the working directory), else prompts for them in a terminal.
// Without a terminal both stay undefined
// and the login has to be completed manually in the browser.
export async function getCredentials (prefix, displayName) {
  try {
    process.loadEnvFile()
  }
  catch { /* No .env file available */ }

  const credentials = {
    username: process.env[`${prefix}_USERNAME`],
    password: process.env[`${prefix}_PASSWORD`],
  }

  if ((credentials.username && credentials.password) || !process.stdin.isTTY) {
    return credentials
  }

  const inquirer = (await import("inquirer")).default
  const prompt = inquirer.createPromptModule({ output: process.stderr })
  return prompt([
    { type: "input", name: "username", message: `${displayName} Username:` },
    { type: "password", name: "password", message: `${displayName} Password:` },
  ])
}


// Runs `trigger` (e.g. a click on an export link) and returns
// `{body, headers}` of the first response served as an attachment.
// The request is answered with an empty response, so no browser download
// happens: Chromium crashes (SIGSEGV) when Playwright handles downloads.
// Also captures files served in popups (e.g. FNZ Bank's Postkorb).
export async function captureAttachmentResponse (
  page,
  trigger,
  {timeout = 60000} = {},
) {
  const context = page.context()
  let captured = null
  await context.route("**/*", async route => {
    const response = await route.fetch()
    const headers = response.headers()
    const disposition = headers["content-disposition"] || ""
    if (!captured && /attachment/i.test(disposition)) {
      captured = {body: await response.body(), headers}
      await route.fulfill({status: 204, body: ""})
    }
    else {
      await route.fulfill({response})
    }
  })

  try {
    await trigger()
    for (let waited = 0; !captured && waited < timeout; waited += 500) {
      await page.waitForTimeout(500)
    }
  }
  finally {
    await context.unrouteAll({behavior: "ignoreErrors"})
  }

  if (!captured) {
    throw new Error("No file was served")
  }
  return captured
}


// Like `captureAttachmentResponse`, but only returns the body
export async function captureAttachment (page, trigger, options) {
  return (await captureAttachmentResponse(page, trigger, options)).body
}


// Dump the page state for debugging selectors after site redesigns
// (must never throw, as it would mask the error which triggered the dump)
export async function dumpDebugFiles (page, namePrefix = "page-debug") {
  try {
    const debugHtmlPath = temporaryFile({name: `${namePrefix}.html`})
    const debugImagePath = temporaryFile({name: `${namePrefix}.png`})
    await fse.writeFile(debugHtmlPath, await page.content())
    await page.screenshot({path: debugImagePath, fullPage: true})
    console.warn(`Current URL: ${page.url()}`)
    console.warn(`Saved page HTML to ${debugHtmlPath}`)
    console.warn(`Saved screenshot to ${debugImagePath}`)
  }
  catch (error) {
    console.warn(`Dumping the page state failed: ${error.message}`)
  }
}


// The system Chrome is used by default.
// `TRANSITY_BROWSER=chromium` uses Playwright's bundled Chromium instead
// (system Chrome 154 sometimes crashes with SIGSEGV during downloads).
function systemChannel () {
  if (process.env.TRANSITY_BROWSER === "chromium") {
    throw new Error("Bundled Chromium requested")
  }
  return "chrome"
}


export async function launchBrowser (options = {}) {
  const {
    shallShowBrowser = false,
    // Set a name to store cookies etc. across runs in a named profile.
    // This avoids repeated security challenges and 2FA prompts
    // on sites which support remembering the device.
    persistentProfileName = null,
    // Set to false to block downloads (e.g. when reading
    // the file from the network response instead)
    acceptDownloads = true,
  } = options

  // Without this flag `navigator.webdriver` is `true` and several sites
  // (e.g. PayPal) refuse to even serve their login security challenge
  const args = ["--disable-blink-features=AutomationControlled"]

  // Allows inspecting the running browser (e.g. with `connectOverCDP`)
  // to update selectors after site redesigns
  if (process.env.TRANSITY_DEBUG_PORT) {
    args.push(`--remote-debugging-port=${process.env.TRANSITY_DEBUG_PORT}`)
  }

  if (persistentProfileName) {
    const userDataDir = path.join(
      os.homedir(), ".cache", "transity", persistentProfileName)
    const contextOptions = {
      headless: !shallShowBrowser,
      acceptDownloads,
      args,
    }
    let context = null
    try {
      // Use the system Chrome if available
      // (avoids a separate browser download)
      context = await chromium.launchPersistentContext(
        userDataDir, { ...contextOptions, channel: systemChannel() })
    }
    catch {
      context = await chromium.launchPersistentContext(
        userDataDir, contextOptions)
    }
    const page = context.pages()[0] ?? await context.newPage()
    // Leave time for manual 2FA/TAN confirmation during logins
    page.setDefaultTimeout(120000)

    // Closing the context also closes the browser
    return { browser: context, page }
  }

  const launchOptions = { headless: !shallShowBrowser, args }
  let browser = null

  try {
    // Use the system Chrome if available (avoids a separate browser download)
    browser = await chromium.launch({
      ...launchOptions,
      channel: systemChannel(),
    })
  }
  catch {
    browser = await chromium.launch(launchOptions)
  }

  const context = await browser.newContext({ acceptDownloads })
  const page = await context.newPage()
  // Leave time for manual 2FA/TAN confirmation during logins
  page.setDefaultTimeout(120000)

  return { browser, page }
}
