// Downloads scanned letters from the Caya "Document Cockpit"
// at https://app.caya.com
//
// Usage:
//   node caya.js <dir> [from <YYYY-MM-DD>] [to <YYYY-MM-DD>]
//     [sender <text>] [search <text>] [list]
//
// Selects the documents received in the date range (default: all)
// of all root folders (inbox, archive, trash, shared with me):
// - `sender <text>`: only documents whose sender name contains the text
//   (case-insensitive)
// - `search <text>`: only documents matched by Caya's full text search
//   (also finds letters whose sender wasn't recognized,
//   but is fuzzy and also matches similar words)
// Saves the documents which are not yet in <dir> as
// `<received date>_<sender>_<subject>.pdf`
// (e.g. "2026-03-02_wikimedia_deutschland_wikipedia_wird_25_danke_dass.pdf")
// and prints their paths to stdout.
// With `list` the documents are only printed (tab separated), not saved.
// Documents are never moved, tagged or marked as read.
//
// The login has to be completed manually in the browser.
// It's remembered in the browser profile.

import path from "node:path"
import {pathToFileURL} from "node:url"

import fse from "fs-extra"

import {dumpDebugFiles, launchBrowser} from "../browser.js"

const log = console.warn
const appUrl = "https://app.caya.com/app/folder/inbox"
const apiUrl = "https://customer-api.caya.com/"

const documentFields = `
  id
  createdAt
  filename
  file
  folder { title }
  metadata { senderName subject tags }
`


// Opens the Document Cockpit and returns the (1 hour valid) ID token,
// taken from the first authorized request of the app
export async function login (page) {
  log(`Open ${appUrl}`)
  const apiRequest = page.waitForRequest(
    request => request.url()
      .startsWith(apiUrl) &&
      Boolean(request.headers().authorization),
    {timeout: 600000},
  )
  await page.goto(appUrl, {timeout: 30000})
  if (!page.url()
    .includes("/app/")) {
    log("Please log in in the browser …")
  }
  return (await apiRequest).headers().authorization
}


// Sends a query to Caya's GraphQL API
export async function queryApi (page, token, query, variables = {}) {
  const response = await page.request.post(apiUrl, {
    headers: {authorization: token, "content-type": "application/json"},
    data: {query, variables},
  })
  const result = await response.json()
  if (result.errors) {
    throw new Error(
      `Caya API: ${result.errors.map(error => error.message)
        .join(", ")}`)
  }
  return result.data
}


// `{createdAt_gte, createdAt_lte}` filter of a date range
function getDateFilter ({from, to}) {
  const filter = {}
  /* eslint-disable camelcase -- Field names of the Caya API */
  if (from) {
    filter.createdAt_gte = `${from}T00:00:00.000Z`
  }
  if (to) {
    filter.createdAt_lte = `${to}T23:59:59.999Z`
  }
  /* eslint-enable camelcase */
  return filter
}


// Returns the documents of all root folders received in the date range
export async function listDocuments (page, token, {from, to} = {}) {
  const {meFolders} = await queryApi(page, token, `query {
    meFolders {
      inbox { id }
      archive { id }
      trash { id }
      sharedWithMe { id }
    }
  }`)
  const query = `query (
    $after: String
    $where: ContainerDocumentsByFolderWhereInput!
  ) {
    connection: getContainerDocumentsByFolderConnection(
      after: $after
      first: 100
      orderBy: createdAt_DESC
      where: $where
    ) {
      edges { node { ... on ContainerDocument { ${documentFields} } } }
      pageInfo { endCursor hasNextPage }
    }
  }`

  const documents = []
  for (const folder of Object.values(meFolders)) {
    if (!folder?.id) {
      continue
    }
    let after = null
    do {
      const {connection} = await queryApi(page, token, query, {
        after,
        where: {folder: {id: folder.id}, ...getDateFilter({from, to})},
      })
      documents.push(...connection.edges.map(edge => edge.node))
      after = connection.pageInfo.hasNextPage
        ? connection.pageInfo.endCursor
        : null
    } while (after)
  }
  return documents
}


// Returns the documents found by the full text search
// received in the date range
export async function searchDocuments (page, token, {text, from, to}) {
  const query = `query (
    $cursor: [String!]
    $where: SearchContainerDocumentWhereInput!
  ) {
    connection: searchContainerDocumentConnection(
      after: $cursor
      first: 50
      where: $where
    ) {
      edges { node { ... on ContainerDocument { ${documentFields} } } }
      pageInfo { endCursor hasNextPage }
    }
  }`

  const documents = []
  let cursor = null
  do {
    const {connection} = await queryApi(page, token, query, {
      cursor,
      where: {text, ...getDateFilter({from, to})},
    })
    documents.push(...connection.edges.map(edge => edge.node))
    cursor = connection.pageInfo.hasNextPage
      ? connection.pageInfo.endCursor
      : null
  } while (cursor)
  return documents
}


// Lowercase ASCII words joined by "_", at most `maxWords` of them
// ("Förderung Freien Wissens" → "foerderung_freien_wissens")
export function toSnakeCase (text, maxWords = Infinity) {
  return text
    .toLowerCase()
    .replace(/ä/g, "ae")
    .replace(/ö/g, "oe")
    .replace(/ü/g, "ue")
    .replace(/ß/g, "ss")
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "")
    .split(/[^a-z0-9]+/)
    .filter(Boolean)
    .slice(0, maxWords)
    .join("_")
}


// Sender name without the description after a dash and legal forms
// ("Wikimedia Deutschland - Gesellschaft zur … e. V."
// → "wikimedia_deutschland")
const legalFormPattern =
  /\b(e\.\s?V\.|GmbH & Co\. KG|g?GmbH|mbH|AG|KG|SE|Aktiengesellschaft)(?=\s|$)/g

export function getSenderLabel (senderName) {
  const name = (senderName || "")
    .split(/\s+[-–]\s+/)[0]
    .replace(legalFormPattern, "")
  return toSnakeCase(name, 3) || "unknown"
}


// `<received date>_<sender>_<subject>.pdf` (dates in German time)
export function getFileName (document) {
  const date = new Date(document.createdAt)
    .toLocaleDateString("sv-SE", {timeZone: "Europe/Berlin"})
  const subject = toSnakeCase(
    document.metadata?.subject || document.metadata?.tags?.[0] || "", 5)
  return [date, getSenderLabel(document.metadata?.senderName), subject]
    .filter(Boolean)
    .join("_") + ".pdf"
}


function normalize (text) {
  return (text || "").normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "")
    .toLowerCase()
}


// Returns the documents selected by the options, oldest first
export async function getDocuments (
  page,
  token,
  {from, to, sender, search} = {},
) {
  const documents = search
    ? await searchDocuments(page, token, {text: search, from, to})
    : await listDocuments(page, token, {from, to})
  return documents
    .filter(document => !sender ||
      normalize(document.metadata?.senderName)
        .includes(normalize(sender)))
    .sort((documentA, documentB) =>
      documentA.createdAt.localeCompare(documentB.createdAt))
}


export async function downloadDocuments (page, documents, {outputDir}) {
  await fse.ensureDir(outputDir)
  const usedNames = new Set()
  let downloadCounter = 0

  for (const document of documents) {
    // Several letters of a sender on the same day get a counter
    let fileName = getFileName(document)
    for (let counter = 2; usedNames.has(fileName); counter++) {
      fileName = getFileName(document)
        .replace(/\.pdf$/, `_${counter}.pdf`)
    }
    usedNames.add(fileName)

    const filePath = path.join(outputDir, fileName)
    if (await fse.pathExists(filePath)) {
      log(`Skip ${fileName} (already downloaded)`)
      continue
    }
    // `file` is a presigned S3 URL, which doesn't need authorization
    const response = await page.request.get(document.file)
    const body = await response.body()
    if (!response.ok() || body.subarray(0, 5)
      .toString() !== "%PDF-") {
      throw new Error(
        `Downloading ${fileName} failed (HTTP ${response.status()})`)
    }
    await fse.writeFile(filePath, body)
    log(`Saved ${fileName}`)
    console.info(filePath)
    downloadCounter += 1
  }

  log(`Downloaded ${downloadCounter} new documents`)
}


export function formatDocument (document) {
  return [
    document.createdAt.slice(0, 10),
    document.folder?.title,
    document.metadata?.senderName,
    document.metadata?.subject,
    document.metadata?.tags?.join(","),
  ]
    .map(value => value ?? "")
    .join("\t")
}


export function parseArgs (args) {
  const options = {outputDir: args[0], shallOnlyList: false}
  for (let index = 1; index < args.length; index++) {
    const arg = args[index]
    if (arg === "list") {
      options.shallOnlyList = true
    }
    else if (["from", "to", "sender", "search"].includes(arg)) {
      options[arg] = args[++index]
    }
    else {
      throw new Error(`Unknown argument "${arg}"`)
    }
  }
  for (const key of ["from", "to"]) {
    if (options[key] && !/^\d{4}-\d{2}-\d{2}$/.test(options[key])) {
      throw new Error(`"${key}" must be a date like 2026-01-31`)
    }
  }
  return options
}


async function main () {
  let options = null
  try {
    options = parseArgs(process.argv.slice(2))
  }
  catch (error) {
    console.error(error.message)
  }
  if (!options?.outputDir) {
    console.error(
      "Usage: node caya.js <dir> [from <YYYY-MM-DD>] [to <YYYY-MM-DD>] " +
      "[sender <text>] [search <text>] [list]")
    process.exitCode = 1
    return
  }

  const {browser, page} = await launchBrowser({
    shallShowBrowser: true,
    persistentProfileName: "caya",
    acceptDownloads: false,
  })

  try {
    const token = await login(page)
    const documents = await getDocuments(page, token, options)
    log(`Found ${documents.length} documents`)
    if (options.shallOnlyList) {
      for (const document of documents) {
        console.info(formatDocument(document))
      }
    }
    else {
      await downloadDocuments(page, documents, options)
    }
  }
  catch (error) {
    await dumpDebugFiles(page, "caya-debug")
    console.error(error)
    process.exitCode = 1
  }
  finally {
    await browser.close()
  }
}


// Allows importing the functions, e.g. to test them in a running browser
if (
  process.argv[1] &&
  import.meta.url === pathToFileURL(process.argv[1]).href
) {
  main()
}
