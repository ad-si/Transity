## Import

### AI Powered

We built a dedicated OpenAI GPT to convert any financial data
(e.g. CSVs, bank statements, chat history, …) to a Transity journal file.

Check it out at
[chat.openai.com/g/g-aUph953Vj-transity](
  https://chat.openai.com/g/g-aUph953Vj-transity).


### From Ledger CLI

Execute the included ledger2transity script:

```shell
./ledger2transity.sh examples/hledger.journal > transactions.csv
```

Convert `transactions.csv` to YAML with e.g. [browserling.com/tools/csv-to-yaml]

[browserling.com/tools/csv-to-yaml]: https://browserling.com/tools/csv-to-yaml


**Attention:**

- Merge adjacent entries as each entry only debits / credits an account.
  A transaction always involves 2 accounts (`from` and `to`).
  (For expenses basically copy the ledger-account from the second entry
  into the `from` field of the first entry)
- `from` and `to` might be reversed for income
  (depending on how the `payee` field was used)
- Account names of Ledger-CLI are interpreted as tags
  Transity understands accounts as **physical accounts**
- The note is duplicated in the `tags` field.
  There is no way to get only the tags in Ledger-CLI 😔


### Scripts

Transity includes a few scripts located at [./scripts](./scripts) to
automate a Chrome browser to download data.


### Retrieving Data from Banks

It supports downloading CSV files of all
transactions and converting them to journal
files and retrieving the current account balance:

```sh
node scripts/transactions/hypovereinsbank.js > transactions.yaml
```

This will prompt you for your credentials and afterwards automate
a headless Chrome instance to download and convert the data.

Currently supported accounts for transactions:

- [DKB Visa Card](https://dkb.de)
- [DKB Giro Account](https://dkb.de)
- [HypoVereinsbank](https://www.hypovereinsbank.de)
- [MBS](https://mbs.de)

Currently supported accounts for balances:

- [AWS](https://aws.amazon.com)
- [DKB](https://dkb.de)
- [Fidor](https://fidor.de)
- [Finvesto](https://finvesto.de)
- [HypoVereinsbank](https://www.hypovereinsbank.de)
- [MBS](https://mbs.de)
- [PayPal](https://paypal.com)
- [Deutsche Post Petty Cash](https://portokasse.deutschepost.de)

Contributions are very welcome!
