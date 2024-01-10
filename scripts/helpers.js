export function sanitizeYaml (yaml) {
  if (typeof yaml !== "string") {
    throw new Error("YAML must be passed as a string")
  }
  return yaml
    .replace(/^ {2}- /gm, "\n  -\n    ")
    .replace(/^ {2}([^:]+): '(.+)'$/gm, "  $1: $2")
    .replace(/utc: ([0-9TZ:.-]+)$/gm, "utc: '$1'")
}


export function prettyFormat (account, balance) {
  const paddingStart = 28
  return account.padStart(paddingStart) + ": " + balance
}


export function prettyPrint (account, balance) {
  console.info(prettyFormat(account, balance))
}


export function rmEmptyString (key, value) {
  return value === ""
    ? undefined
    : value
}


export function toDdotMdotYYYY (date) {
  return [
    date.getUTCDate(),
    date.getUTCMonth() + 1,
    date.getUTCFullYear(),
  ].join(".")
}


export function toDDdotMMdotYYYY (date) {
  return [
    ("0" + String(date.getUTCDate())).slice(-2),
    ("0" + String(date.getUTCMonth() + 1)).slice(-2),
    date.getUTCFullYear(),
  ].join(".")
}


export function keysToEnglish (object) {
  const newObject = {}
  Object.entries(object)
    .forEach(entry => {
      // Order of replacements is important!
      // (Mostly longer German words first)
      const newKey = entry[0]
        .replace(/^Beguenstigter\/Zahlungspflichtiger$/i, "to")
        .replace(/^Versand- und Bearbeitungsgebühr/,
          "shipping_and_handling_amount")
        .replace(/^Zugehöriger Transaktionscode$/i, "reference_txn_id")
        .replace(/^Auftraggeber \/ Begünstigter$/i, "from")
        .replace(/^Kundenreferenz \(End-to-End\)$/i, "end-to-end-id")
        .replace(/^Empfänger E-Mail-Adresse$/i, "to_email_address")
        .replace(/^Absender E-Mail-Adresse$/i, "from_email_address")
        .replace(/^Auswirkung auf Guthaben$/i, "balance_impact")
        .replace(/^Ursprünglicher Betrag$/i, "original-amount")
        .replace(/^Versicherungsbetrag$/i, "insurance_amount")
        .replace(/^Zahlungsbetrag in ZW$/i, "amount")
        .replace(/^Artikelbezeichnung$/i, "item_title")
        .replace(/^Zahlungswährung \(ZW\)$/i, "currency")
        .replace(/^Transaktionscode$/i, "transaction_id")
        .replace(/^Fondswährung \(FW\)$/i, "fond-currency")
        .replace(/^BIC \(SWIFT-Code\)$/i, "bic")
        .replace(/^Verwendungszweck$/i, "note")
        .replace(/^Kontonummer\/IBAN$/i, "account-id")
        .replace(/^Rechnungsnummer$/i, "invoice_number")
        .replace(/^Lieferadresse$/i, "shipping_address")
        .replace(/^Auftragskonto$/i, "tracking-account")
        .replace(/^Adresszeile 1$/i, "address_line_1")
        .replace(/^Adress-Status$/i, "address_status")
        .replace(/^Option 2 Wert$/i, "option_2_value")
        .replace(/^Option 1 Wert$/i, "option_1_value")
        .replace(/^Option 2 Name$/i, "option_2_name")
        .replace(/^Option 1 Name$/i, "option_1_name")
        .replace(/^Ländervorwahl$/i, "country_code")
        .replace(/^Kundenreferenz$/i, "customer-id")
        .replace(/^Empfangsnummer$/i, "receipt_id")
        .replace(/^Glaeubiger ID$/i, "creditor-id")
        .replace(/^Gläubiger-ID$/i, "creditor-id")
        .replace(/^Mandatsreferenz$/i, "mandate")
        .replace(/^Buchungsdatum$/i, "entry-utc")
        .replace(/^Umsatzsteuer$/i, "sales_tax")
        .replace(/^Wertstellung$/i, "value-utc")
        .replace(/^Kontonummer$/i, "account-id")
        .replace(/^Artikelnummer$/i, "item_id")
        .replace(/^Adresszusatz$/i, "address_line_2")
        .replace(/^Valutadatum$/i, "value-utc")
        .replace(/^Buchungstag$/i, "entry-utc")
        .replace(/^Depotnummer$/i, "depot-id")
        .replace(/^Betrag \(EUR\)$/i, "amount")
        .replace(/^Buchungstext$/i, "type")
        .replace(/^Beschreibung$/i, "note")
        .replace(/^Belegdatum$/i, "entry-utc")
        .replace(/^Zollnummer$/i, "custom_number")
        .replace(/^Bundesland$/i, "state")
        .replace(/^Kursdatum$/i, "stock-price-date")
        .replace(/^Guthaben$/i, "balance")
        .replace(/^Zeitzone$/i, "timezone")
        .replace(/^Waehrung$/i, "currency")
        .replace(/^Telefon$/i, "contact_phone_number")
        .replace(/^Währung$/i, "currency")
        .replace(/^Valuta$/i, "value-utc")
        .replace(/^Betreff$/i, "subject")
        .replace(/^Anzahl$/i, "quantity")
        .replace(/^Anteile$/i, "shares")
        .replace(/^Status$/i, "status")
        .replace(/^Betrag$/i, "amount")
        .replace(/^Hinweis$/i, "note")
        .replace(/^Brutto$/i, "gross")
        .replace(/^Uhrzeit$/i, "time")
        .replace(/^Ref. Nr.$/i, "id")
        .replace(/^Gebühr$/i, "fee")
        .replace(/^Datum$/i, "date")
        .replace(/^Netto$/i, "net")
        .replace(/^Land$/i, "country")
        .replace(/^Wert$/i, "amount")
        .replace(/^Name$/i, "name")
        .replace(/^ISIN$/i, "isin")
        .replace(/^Info$/i, "info")
        .replace(/^BIC$/i, "bic")
        .replace(/^BLZ$/i, "bank-code")
        .replace(/^Ort$/i, "town/city")
        .replace(/^PLZ$/i, "zip/postal_code")
        .replace(/^Typ$/i, "type")

      newObject[newKey.toLowerCase()] = entry[1]
    })
  return newObject
}

export function noteToAccount (note = "") {
  // Remove misleading terms
  note = note.replace("Apple Pay", "")

  // Sorted by ascending importance
  // I.e. later keywords overwrite selection
  /* eslint-disable quote-props */
  const mappings = {
    "paypal": "paypal",
    "amazon": "amazon",
    "amazon prime": "amazon:prime",
    "patreon": "patreon",
    "facebook": "facebook",
    "google ireland limited cloud platform": "google:cloud",
    "day night sports gmbh": "day_night_sports",
    "spotify": "spotify",
    "apple": "apple",
    "itunes": "apple:itunes",
    "mozilla foundation": "mozilla_foundation",
    "mozilla": "mozilla",
    "namecheap": "namecheap",
    "name-cheap": "namecheap",
    "irccloud": "irccloud",
    "flattr": "flattr",
    "u s metric association": "us_metric_association",
    "github": "github",
    "digitalocean": "digitalocean",
    "easyjet": "easyjet",
    "linode": "linode",
    "macy''s": "macys",
    "flightfox": "flightfox",
    "hyperhq": "hyperhq",
    "at&t": "at-and-t",
    "remote year": "remote_year",
    "ergodox ez": "ergodox_ez",
    "starbucks": "starbucks",
    "stdlib": "stdlib",
    "uber.com": "uber",
    "walgreens": "walgreens",
    "dropbox": "dropbox",
    "saturn electro": "saturn",
    "dropscan": "dropscan",
    "google ireland limited": "google",
    "jimmy joy": "jimmy_joy",
    "wecircberlin": "circ",
    "lime ride": "lime",
    "lime electric": "lime",
    "bird rides": "bird",
    "google": "google",
    "free software foundation": "free_software_foundation",
    "landr audio": "landr",
    "mzla technologies": "mzla_technologies",
    "vodafone": "vodafone",
    "mailgun": "mailgun",
    "fontis publishing": "fontis_publishing",
    "gumroad": "gumroad",
    "uber payments": "uber",
    "uptime robot": "uptimerobot",
    "booking.com": "booking.com",

    // German
    "penny": "penny",
    "rewe": "rewe",
    "ihk ": "ihk",
    "etl ": "etl",
    "lidl": "lidl",
    "caya": "caya",
    "condor": "condor",
    "conrad electronic": "conrad",
    "edeka": "edeka",
    "auslandseinsatz": "dkb:visa",
    "db bahn": "deutsche_bahn",
    "db vertrieb gmbh": "deutsche_bahn",
    "deutsche post ag": "deutsche_post",
    "e-plus service gmbh": "eplus",
    "ebay gmbh": "ebay",
    "huk24 ag": "huk24",
    "nextbike gmbh": "nextbike",
    "rossmann": "rossmann",
    "siehe anlage": " ",
    "spar dankt": "spar",
    "sparda-bank": "sparda",
    "united domains ag": "united_domains",
    "mittelbrandenburgische spk": "mbs",
    "finanzamt": "tax_office",
    "finanz amt": "tax_office",
    "wikimedia deutschland": "wikimedia",
    "mcpaper": "mcpaper",
    "atmosfair": "atmosfair",
    "qthority": "qthority",
    "nosh good taste": "nosh",
    "musikhaus thomann": "thomann",
    "fratellis frankfurt": "fratellis_ristorante",
    "fratellis rist": "fratellis_ristorante",
    "hansemerkur speziale kv": "hansemerkur",
    "zwanzigeins e.v.": "zwanzigeins",
    "tier de": "tier",
    "zalando": "zalando",
    "hetzner online": "hetzner",
    "golem media": "golem",
    "pro rauchfrei": "pro_rauchfrei",
    "new atlas": "new_atlas",
    "media markt": "media_markt",
    "trade republic": "trade_republic",
    "simple communication": "simple_fax",
    "dak-gesundheit": "dak",
    "diebayerische": "die_bayerische",
    "beamtenkra": "bayerische_beamtenkrankenkasse",
    "mainova": "mainova",
    "telekom": "telekom",
    "hessischer rund funk": "hessischer_rundfunk",
    "versicherungska mmer bayern": "vkb",
    "netcup gmbh": "netcup",
    "docmorris": "docmorris",
    "feram gmbh": "feram",
  }
  /* eslint-enable quote-props */
  let account = note

  Object.entries(mappings)
    .forEach(entry => {
      if (
        note && note
          .toLowerCase()
          .replace(/\s\s+/g, " ")
          .includes(entry[0])
      ) {
        account = entry[1]
      }
    })

  return account
}
