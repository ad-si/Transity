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
    ? months.sort().at(-1)
    : null
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


export async function launchBrowser (options = {}) {
  const {
    shallShowBrowser = false,
    // Set a name to store cookies etc. across runs in a named profile.
    // This avoids repeated security challenges and 2FA prompts
    // on sites which support remembering the device.
    persistentProfileName = null,
  } = options

  // Without this flag `navigator.webdriver` is `true` and several sites
  // (e.g. PayPal) refuse to even serve their login security challenge
  const args = ["--disable-blink-features=AutomationControlled"]

  if (persistentProfileName) {
    const userDataDir = path.join(
      os.homedir(), ".cache", "transity", persistentProfileName)
    const contextOptions = {
      headless: !shallShowBrowser,
      acceptDownloads: true,
      args,
    }
    let context = null
    try {
      // Use the system Chrome if available
      // (avoids a separate browser download)
      context = await chromium.launchPersistentContext(
        userDataDir, { ...contextOptions, channel: "chrome" })
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
    browser = await chromium.launch({ ...launchOptions, channel: "chrome" })
  }
  catch {
    browser = await chromium.launch(launchOptions)
  }

  const context = await browser.newContext({ acceptDownloads: true })
  const page = await context.newPage()
  // Leave time for manual 2FA/TAN confirmation during logins
  page.setDefaultTimeout(120000)

  return { browser, page }
}
