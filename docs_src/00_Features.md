## Features

- Easily editable and processable file format based on [YAML]
- Modeled on transactions instead of debiting / crediting accounts \
    → Support for complex transactions made up of several transfers
    - Dedicated payer (from) and payee (to) fields (ledger only supports payee)
- No misuse of accounts as categories / tags \
    → Direct support for tags
- Clear separation between
    - Physical account (e.g. wallet, bank account)
    - Entities (e.g. my mum, a company)
    - Purpose of transaction (e.g. food, travel)
- No hard-coded asset / liability connotation as it is viewpoint dependent \
    → Choose viewpoint by setting the owner of the journal
- Initial balances
- High precision timestamps in ISO 8601 format
- Reference external files (e.g. receipts, contracts, bank statements, …)
- Safety checks
    - BigInt fractional numbers to eliminate rounding errors
    - Verifies exclusive use of predefined entities
    - Checks in transactions match with verification balances
    - Checks that referenced external files exist
        and that all external files are referenced
- Export to other formats for post-processing
    - [Gnuplot] - For trends
    - [(H)ledger Format] - For using (H)ledger exclusive features
    - CSV and TSV - For further processing in spreadsheet software
    - XLSX aka Excel - For further processing in spreadsheet software
- Multi file support

[YAML]: http://yaml.org
[Gnuplot]: http://www.gnuplot.info
[(H)ledger Format]: http://hledger.org/journal.html
