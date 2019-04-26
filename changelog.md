# 0.4.0 (2019-04-25)

- Add scripts to retrieve the balance and transactions
    from several German banks (097eb93, 204874e)
- Use BigInts instead of Ints for amounts to eliminate rounding errors (30f5408)
- Add support for initial balances (d6f5799)
- Add support for verification balances (as demonstrated in
    [verification-balances.yaml](examples/verification-balances.yaml)) (33684ae)
- Add support for signed amounts (d8ecabd)
- Switch to GPL-3.0-or-later license (53c0c0f)
- Fix `npm install` by using psc-package instead of bower (5cada63)


# 0.3.0 (2018-09-10)

- Add command `transfers` (3ae89fc)
- Add command `ledger-entries` to export to the ledger file format (4be8374)
- Add commands `csv` and `tsv` to print entries in as CSV / TSV (8587e22)


# 0.2.1 (2018-06-05)

- Fix test command for CI, fix typos (ac81a8e)
- Fix references (42f17b3)


# 0.2.0 (2018-06-05)

- Don't coerce invalid dates to 1970-01-01 (07f99f5)
- Add `gplot` subcommands to allow piping to gnuplot (c25b445)
- Add `entries` CLI command to list all entries (3021290)
- Exit with status code 1 if parsing or validation fails (a7aaf1c)
- Verify accounts after parsing ledger file (37aff69)
- Add color support for terminal printing (8b6505c)
- Implement alignment of entries (bfa602f)


# 0.1.0-alpha (2018-01-18)

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
