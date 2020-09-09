# Changelog

This changelog only contains user facing changes of the app itself.


## 0.8.0 (2020-09-09)

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


## 0.7.0 (2020-02-18)

- Improve normalization of crawled transactions ([ac78c05])
- Improve scripts for transactions loading & parsing ([c7c558e])
- Switch to AGPL and improve wording of license documentation ([8dde588])

[ac78c05]: https://github.com/feramhq/transity/commit/ac78c05
[c7c558e]: https://github.com/feramhq/transity/commit/c7c558e
[8dde588]: https://github.com/feramhq/transity/commit/8dde588


## 0.6.0 (2019-10-20)

- Add comparison between Transity and Hledger entries ([acf219b])
- Add screenshots ([acf219b])

[acf219b]: https://github.com/feramhq/transity/commit/acf219b


## 0.5.0 (2019-05-04)

- Deploy simple web version of Transity at [feram.io/transity] <!----> (5cc24f6)
- Fix several typos and grammatical errors (0f670a7)

[feram.io/transity]: https://www.feram.io/transity


## 0.4.2 (2019-04-26)

- Only add relevant files to npm package (1c9dc47)
- Update dependencies (83992a7)


## 0.4.1 (2019-04-26)

- Simplify installation by pre-building Transity
    and only delivering the built files in the npm package (459d3c0)
- Add a changelog (c33e03b)


## 0.4.0 (2019-04-25)

- Add scripts to retrieve the balance and transactions
    from several German banks (097eb93, 204874e)
- Use BigInts instead of Ints for amounts to eliminate rounding errors (30f5408)
- Add support for initial balances (d6f5799)
- Add support for verification balances (as demonstrated in
    [verification-balances.yaml](examples/verification-balances.yaml)) (33684ae)
- Add support for signed amounts (d8ecabd)
- Switch to GPL-3.0-or-later license (53c0c0f)
- Fix `npm install` by using psc-package instead of bower (5cada63)


## 0.3.0 (2018-09-10)

- Add command `transfers` (3ae89fc)
- Add command `ledger-entries` to export to the ledger file format (4be8374)
- Add commands `csv` and `tsv` to print entries in as CSV / TSV (8587e22)


## 0.2.1 (2018-06-05)

- Fix test command for CI, fix typos (ac81a8e)
- Fix references (42f17b3)


## 0.2.0 (2018-06-05)

- Don't coerce invalid dates to 1970-01-01 (07f99f5)
- Add `gplot` subcommands to allow piping to gnuplot (c25b445)
- Add `entries` CLI command to list all entries (3021290)
- Exit with status code 1 if parsing or validation fails (a7aaf1c)
- Verify accounts after parsing ledger file (37aff69)
- Add color support for terminal printing (8b6505c)
- Implement alignment of entries (bfa602f)


## 0.1.0-alpha (2018-01-18)

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
