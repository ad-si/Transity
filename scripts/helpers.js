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
        .replace("Beguenstigter/Zahlungspflichtiger", "to")
        .replace("Versand- und Bearbeitungsgebühr",
          "shipping_and_handling_amount")
        .replace("Zugehöriger Transaktionscode", "reference_txn_id")
        .replace("Auftraggeber / Begünstigter", "from")
        .replace("Kundenreferenz (End-to-End)", "end-to-end-id")
        .replace("Empfänger E-Mail-Adresse", "to_email_address")
        .replace("Absender E-Mail-Adresse", "from_email_address")
        .replace("Auswirkung auf Guthaben", "balance_impact")
        .replace("Ursprünglicher Betrag", "original-amount")
        .replace("Versicherungsbetrag", "insurance_amount")
        .replace("Zahlungsbetrag in ZW", "amount")
        .replace("Artikelbezeichnung", "item_title")
        .replace("Zahlungswährung (ZW)", "currency")
        .replace("Transaktionscode", "transaction_id")
        .replace("Fondswährung (FW)", "fond-currency")
        .replace("BIC (SWIFT-Code)", "bic")
        .replace("Verwendungszweck", "note")
        .replace("Kontonummer/IBAN", "account-id")
        .replace("Rechnungsnummer", "invoice_number")
        .replace("Lieferadresse", "shipping_address")
        .replace("Auftragskonto", "tracking-account")
        .replace("Adresszeile 1", "address_line_1")
        .replace("Adress-Status", "address_status")
        .replace("Option 2 Wert", "option_2_value")
        .replace("Option 1 Wert", "option_1_value")
        .replace("Option 2 Name", "option_2_name")
        .replace("Option 1 Name", "option_1_name")
        .replace("Ländervorwahl", "country_code")
        .replace("Kundenreferenz", "customer-id")
        .replace("Empfangsnummer", "receipt_id")
        .replace("Glaeubiger ID", "creditor-id")
        .replace("Gläubiger-ID", "creditor-id")
        .replace("Mandatsreferenz", "mandate")
        .replace("Buchungsdatum", "entry-utc")
        .replace("Umsatzsteuer", "sales_tax")
        .replace("Wertstellung", "value-utc")
        .replace("Kontonummer", "account-id")
        .replace("Artikelnummer", "item_id")
        .replace("Adresszusatz", "address_line_2")
        .replace("Valutadatum", "value-utc")
        .replace("Buchungstag", "entry-utc")
        .replace("Depotnummer", "depot-id")
        .replace("Betrag (EUR)", "amount")
        .replace("Buchungstext", "type")
        .replace("Beschreibung", "note")
        .replace("Belegdatum", "entry-utc")
        .replace("Zollnummer", "custom_number")
        .replace("Bundesland", "state")
        .replace("Kursdatum", "stock-price-date")
        .replace("Guthaben", "balance")
        .replace("Zeitzone", "timezone")
        .replace("Waehrung", "currency")
        .replace("Telefon", "contact_phone_number")
        .replace("Währung", "currency")
        .replace("Valuta", "value-utc")
        .replace("Betreff", "subject")
        .replace("Anzahl", "quantity")
        .replace("Anteile", "shares")
        .replace("Status", "status")
        .replace("Betrag", "amount")
        .replace("Hinweis", "note")
        .replace("Brutto", "gross")
        .replace("Uhrzeit", "time")
        .replace("Ref. Nr.", "id")
        .replace("Gebühr", "fee")
        .replace("Datum", "date")
        .replace("Netto", "net")
        .replace("Land", "country")
        .replace("Wert", "amount")
        .replace("Name", "name")
        .replace("ISIN", "isin")
        .replace("Info", "info")
        .replace("BIC", "bic")
        .replace("BLZ", "bank-code")
        .replace("Ort", "town/city")
        .replace("PLZ", "zip/postal_code")
        .replace("Typ", "type")

      newObject[newKey] = entry[1]
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
    "rewe": "rewe",
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
    "ihk ": "ihk",
    "etl ": "etl",
    "lidl": "lidl",
    "caya": "caya",
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
