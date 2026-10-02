## Changelog

This changelog only contains user facing changes of the app.


### Unreleased

#### Rust Rewrite

- Rewrite Transity in Rust ([be49141])
    - Install it with `cargo install transity`
    - The legacy PureScript implementation is kept in `purescript/`,
        but is no longer built
- Compile the in-browser playground to WebAssembly ([9487b6c])

#### CLI

- Add subcommand `balance-all` and only show the owner's balance
    with `balance` ([1a8fcb3])
- Add subcommand `entities` to list all entities ([852281f])
- Add subcommand `entities-sorted` to list all entities
    sorted by name ([ded785d])
- Add subcommand `files` to list referenced files
    with their reference counts ([b9c1759])
- Add subcommand `xlsx` to export transfers to an Excel file,
    including file links and transaction notes ([ceb0bed], [f828e3c])
- Add support for passing several journal files ([c0ba028])
- Add `--begin` and `--end` flags to all transaction-based
    subcommands to filter by a time window ([6c6c5d3])
- Add `--owner` flag to all transaction-based subcommands ([329e1b3])
- Add `--tag` flag to filter transfers by entity tags,
    with support for `and`, `or`, `not`, and parentheses ([37898b8])
- Display account hierarchies in balance commands,
    with parent accounts aggregating their children ([0fa680d])
- Support `/` as account separator and make the separator
    configurable via `config.separator` ([1a3d29c])
- Only include past transactions in balance commands
    by default ([75c0a97])
- Don't list commodities with an amount of 0 in `balance` ([9878f32])
- Show full error context and a clickable location
    for YAML parse errors ([73243ed])
- Support several journal paths for `unused-files` ([42e3ed2])
    and ignore `.DS_Store` files ([671d1e3])
- Show the full help text when invoked without arguments ([d615ef6])
- Fix: Accept RFC 3339 timestamps with fractional seconds ([a752a26])
- Fix: Correctly filter accounts with an empty commodity map ([fe12987])

#### Web App

- Add subcommand `server` to serve a web app
    with a balance view ([c066349])
- Add a transactions view ([b18814c]) with:
    - Newest transactions first ([270ff1d])
    - Transfer notes shown as hover tooltips ([39cf8d8])
    - A column with clickable previews
        of referenced files ([ba4aba6], [7cf0ed6])
    - A toggle to highlight transactions without files ([da7e178])
- Use path-based URLs for the tabs ([f289b0d])
- Switch between light and dark mode
    based on the system setting ([8b5659e])

#### Import Scripts

- Replace Nightmare with Playwright for all scraping scripts ([27836bd])
- Add a script to download PayPal transactions ([2481060])
- Rewrite MBS script for the redesigned website
    and add download of postbox documents ([623a054])
- DKB: Normalize amounts with thousands separators ([f63aedc])
    and resolve the payer account from the "from" field ([330dce1])
- Make CSV conversion scripts more robust ([eb9e555])
- Extend translations for CSV conversion scripts ([abc0f58], [443e12a])
- Fix CSV converter for Finvesto ([443e12a])
- Fix import of chrono-node in PayPal CSV converter ([cbe6720])
- Use Bun instead of Node.js ([2a2e3d5])

#### Documentation

- Build documentation website with mdBook ([77ab998])
- Add a page about performance ([939e445])
- Add an explanation of plain text accounting ([c9d2370])
- Add many more tools to the "Related" section

[be49141]: https://github.com/ad-si/Transity/commit/be49141
[9487b6c]: https://github.com/ad-si/Transity/commit/9487b6c
[1a8fcb3]: https://github.com/ad-si/Transity/commit/1a8fcb3
[852281f]: https://github.com/ad-si/Transity/commit/852281f
[ded785d]: https://github.com/ad-si/Transity/commit/ded785d
[b9c1759]: https://github.com/ad-si/Transity/commit/b9c1759
[ceb0bed]: https://github.com/ad-si/Transity/commit/ceb0bed
[f828e3c]: https://github.com/ad-si/Transity/commit/f828e3c
[c0ba028]: https://github.com/ad-si/Transity/commit/c0ba028
[6c6c5d3]: https://github.com/ad-si/Transity/commit/6c6c5d3
[329e1b3]: https://github.com/ad-si/Transity/commit/329e1b3
[37898b8]: https://github.com/ad-si/Transity/commit/37898b8
[0fa680d]: https://github.com/ad-si/Transity/commit/0fa680d
[1a3d29c]: https://github.com/ad-si/Transity/commit/1a3d29c
[75c0a97]: https://github.com/ad-si/Transity/commit/75c0a97
[9878f32]: https://github.com/ad-si/Transity/commit/9878f32
[73243ed]: https://github.com/ad-si/Transity/commit/73243ed
[42e3ed2]: https://github.com/ad-si/Transity/commit/42e3ed2
[671d1e3]: https://github.com/ad-si/Transity/commit/671d1e3
[d615ef6]: https://github.com/ad-si/Transity/commit/d615ef6
[a752a26]: https://github.com/ad-si/Transity/commit/a752a26
[fe12987]: https://github.com/ad-si/Transity/commit/fe12987
[c066349]: https://github.com/ad-si/Transity/commit/c066349
[b18814c]: https://github.com/ad-si/Transity/commit/b18814c
[270ff1d]: https://github.com/ad-si/Transity/commit/270ff1d
[39cf8d8]: https://github.com/ad-si/Transity/commit/39cf8d8
[ba4aba6]: https://github.com/ad-si/Transity/commit/ba4aba6
[7cf0ed6]: https://github.com/ad-si/Transity/commit/7cf0ed6
[da7e178]: https://github.com/ad-si/Transity/commit/da7e178
[f289b0d]: https://github.com/ad-si/Transity/commit/f289b0d
[8b5659e]: https://github.com/ad-si/Transity/commit/8b5659e
[27836bd]: https://github.com/ad-si/Transity/commit/27836bd
[2481060]: https://github.com/ad-si/Transity/commit/2481060
[623a054]: https://github.com/ad-si/Transity/commit/623a054
[f63aedc]: https://github.com/ad-si/Transity/commit/f63aedc
[330dce1]: https://github.com/ad-si/Transity/commit/330dce1
[eb9e555]: https://github.com/ad-si/Transity/commit/eb9e555
[abc0f58]: https://github.com/ad-si/Transity/commit/abc0f58
[443e12a]: https://github.com/ad-si/Transity/commit/443e12a
[cbe6720]: https://github.com/ad-si/Transity/commit/cbe6720
[2a2e3d5]: https://github.com/ad-si/Transity/commit/2a2e3d5
[77ab998]: https://github.com/ad-si/Transity/commit/77ab998
[939e445]: https://github.com/ad-si/Transity/commit/939e445
[c9d2370]: https://github.com/ad-si/Transity/commit/c9d2370


### 0.8.0 (2020-09-09)

- Add CLI command to show version number ([5f9cc03])
- Add csv2yaml scripts for MBS and PayPal ([d1b4840])
- Add subcommand "unused-files" to list unreferenced files ([c107a9a])
- Minor fixes and improvements for csv2yaml and transactions scripts ([202c3d4])
- Minor improvements for retrieval scripts ([a1c6410])
- Warn about non existent referenced files ([a6fbf8b])

[5f9cc03]: https://github.com/feramhq/transity/commit/5f9cc03
[d1b4840]: https://github.com/feramhq/transity/commit/d1b4840
[c107a9a]: https://github.com/feramhq/transity/commit/c107a9a
[202c3d4]: https://github.com/feramhq/transity/commit/202c3d4
[a1c6410]: https://github.com/feramhq/transity/commit/a1c6410
[a6fbf8b]: https://github.com/feramhq/transity/commit/a6fbf8b


### 0.7.0 (2020-02-18)

- Improve normalization of crawled transactions ([ac78c05])
- Improve scripts for transactions loading & parsing ([c7c558e])
- Switch to AGPL and improve wording of license documentation ([8dde588])

[ac78c05]: https://github.com/feramhq/transity/commit/ac78c05
[c7c558e]: https://github.com/feramhq/transity/commit/c7c558e
[8dde588]: https://github.com/feramhq/transity/commit/8dde588


### 0.6.0 (2019-10-20)

- Add comparison between Transity and Hledger entries ([acf219b])
- Add screenshots ([acf219b])

[acf219b]: https://github.com/feramhq/transity/commit/acf219b


### 0.5.0 (2019-05-04)

- Deploy simple web version of Transity at [feram.io/transity] <!----> (5cc24f6)
- Fix several typos and grammatical errors (0f670a7)

[feram.io/transity]: https://www.feram.io/transity


### 0.4.2 (2019-04-26)

- Only add relevant files to npm package (1c9dc47)
- Update dependencies (83992a7)


### 0.4.1 (2019-04-26)

- Simplify installation by pre-building Transity
    and only delivering the built files in the npm package (459d3c0)
- Add a changelog (c33e03b)


### 0.4.0 (2019-04-25)

- Add scripts to retrieve the balance and transactions
    from several German banks (097eb93, 204874e)
- Use BigInts instead of Ints for amounts to eliminate rounding errors (30f5408)
- Add support for initial balances (d6f5799)
- Add support for verification balances (as demonstrated in
    [verification-balances.yaml](examples/verification-balances.yaml)) (33684ae)
- Add support for signed amounts (d8ecabd)
- Switch to GPL-3.0-or-later license (53c0c0f)
- Fix `npm install` by using psc-package instead of bower (5cada63)


### 0.3.0 (2018-09-10)

- Add command `transfers` (3ae89fc)
- Add command `ledger-entries` to export to the ledger file format (4be8374)
- Add commands `csv` and `tsv` to print entries in as CSV / TSV (8587e22)


### 0.2.1 (2018-06-05)

- Fix test command for CI, fix typos (ac81a8e)
- Fix references (42f17b3)


### 0.2.0 (2018-06-05)

- Don't coerce invalid dates to 1970-01-01 (07f99f5)
- Add `gplot` subcommands to allow piping to gnuplot (c25b445)
- Add `entries` CLI command to list all entries (3021290)
- Exit with status code 1 if parsing or validation fails (a7aaf1c)
- Verify accounts after parsing ledger file (37aff69)
- Add color support for terminal printing (8b6505c)
- Implement alignment of entries (bfa602f)


### 0.1.0-alpha (2018-01-18)

- Indent entries in balance only as deep as necessary (bcc61a7)
- Disallow accounts with empty ids, improve error messages (af22b62)
- Sort entities and accounts ascending in balance output (0ad4aa9)
- Display horizontal line under ledger meta infos (62d069a)
- Display better error messages for invalid YAML (7fd26d2)
- Extend list of features, improve import script (2f624d5)
- Add support to print balance from command line (4fc0270)
- Add support for showing the balance (9c89724)
- Add FAQ section to readme (56d876f)
- Read and print transactions from yaml file (33b0106)
- Add import section to readme.md (94d3708)
- Improve layout, colorize output,
    support arbitrary precision accounts (df078c9)
- Add a CLI, add commands `balance` and `transactions` (c87e74e)
