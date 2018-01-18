# Transity

[![Build Status][]][travis]

The plain text accounting tool of the future.
Keep track of your 💵, 🕘, 🐖, 🐄, 🍻 on your command line.

[Build Status]: https://travis-ci.com/adius/transity.svg?token=o8saMqgg3F8qbjfsyJiu&branch=master
[travis]: https://travis-ci.com/adius/transity


## List of Features / TODOs

- [x] Modeled on transactions instead of debiting / crediting accounts
  => Support for complex transactions made up of several transfers
- [x] Dedicated payer (from) and payee (to) fields (ledger only supports payee)
- [x] No misuse of accounts as categories / tags => direct support for them
- [ ] No hardcoded asset / liability connotation as it is viewpoint dependent
  => Choose viewpoint when printing the balance
- [x] Easily editable & processable file format based on [YAML](http://yaml.org)
- Clear separation between
  - [x] Physical account (e.g. wallet, bank account) => spatial
  - [ ] Entities (e.g. my mum, a company) => relational
  - [ ] Purpose of transaction (food, travel) => functional
- [x] High precision timestamps
  - [ ] Including nanoseconds
- [ ] Support for all states of transaction lifecycle
  1. Request - Request to exchange a commodity
  1. Offer - Specification of commodity & expected trade item
  1. Acceptance - Affirmation of interest in offered exchange
  1. Fulfillments
  1. Certification - Acknowledgement that exchange was performed
- [ ] Support for any type of commodity (e.g. even time and messages)
- [ ] Differentiation between transfers, transactions & exchanges
  - [ ] Special syntax for exchanges
- [ ] Meta data for all entities (transactions, accounts, entities, …)
- [ ] Verification of data consistency
- [ ] CSV import
- [ ] Link to receipt file
- [ ] Dashboard
- [ ] Budgets (including progress visualization)
- Export to various formats for postprocessing
  - [ ] [Gnuplot] (for trends)
  - [ ] [Graphviz] (for account / entity relations)
  - [ ] [JS-Sequence-Diagrams] (sequence of transactions)
- Additional features for crypto currencies
  - TODO: Think about what features exactly
- [ ] Multi file support
- [ ] Cache-files to speed up processing of large data sets
- [ ] Support for time limited commodities (e.g. subscription for a month)
- Commodities
  - [ ] Treat as scientific units (e.g 1 k€ == 1000 €)
  - [ ] Hard vs Soft vs Fungible vs …
  - [ ] Define which are allowed / prohibited for each account

[Gnuplot]: http://www.gnuplot.info
[Graphviz]: https://graphviz.org
[JS-Sequence-Diagrams]: https://bramp.github.io/js-sequence-diagrams


## Installation

### From Source

1. `git clone https://github.com/adius/transity`
1. `cd transity`
1. `pulp server` - Wait until it displays `Bundled.` and then stop the server
1. And you are ready to go:
    ```shell
    $ node output/app.js balance tests/ledger.yaml
                anna      1.00 evil-machine
                     -49978.02 €
                 ben    -50.00 $
                         -1.12 BTC
                       -100.00 €
           evil-corp     -1.00 evil-machine
                      50015.00 €
            good-inc   -100.00 €
        grocery-shop     11.97 €
           john:giro     50.00 $
                          1.12 BTC
                         85.00 €
         john:wallet     66.05 €
    ```


## Ledger File Format

A ledger must be a YAML file with following format:

```yaml
commodities:
  - id: €
    name: Euro
    alias:
      - EUR
    note: Currency used in the European Union
    utc: 2017-04-02 19:33:53

entities:
  - id: anna
    name: Anna Smith
    utc: 2017-04-02 19:33:28
    tags:
      - person
    accounts:
      - id: wallet
        name: Wallet
        note: Anna's black wallet
        utc: 2017-04-02 19:33:28
        tags:
          - wallet

  - id: evil-corp
    name: Evil Corporation
    utc: 2017-04-02 19:33:28
    note: The Evil Corporation in the United States of Evil
    tags:
      - company

transactions:
  - title: Purchase of evil machine
    transfers:
      - utc: 2017-02-17
        from: anna
        to: evil-corp
        amount: 50000 €
      - utc: 2017-02-17
        from: evil-corp
        to: anna
        amount: 1 evil-machine
```


## Import from Ledger CLI

Update the ledger filepath in following command,
copy it into your CLI and execute it.

```sh
bash -c '{ \
  echo date,id,from,to,amount,note,tags; \
  ledger \
    --file ledgers/main.ledger \
    --csv-format \'%(
        format_date(date,"%Y-%m-%d")),%(
        quoted(code)),%(
        quoted("")),%(
        quoted(payee)),%(quoted(display_amount)),%(
        quoted(join(trim(xact.note | "{{PLACEHOLDER}}")))),%(
        quoted("- " + display_account + "\\\\n- " +
          join(trim(note | "{{PLACEHOLDER}}")))
        )\n\' \
    --sort date \
    csv; \
} \
| sed "s/{{PLACEHOLDER}}//g" \
| sed \'s/\\\\"/""/g\' \
> transactions.csv'
```

(The ugly `{{PLACEHOLDER}}` workaround is necessary to make it work
if no note is specified for a transaction,
the second `sed` command fixes escaping of `"` in CSV)

Convert `transactions.csv` to YAML with e.g. [browserling.com/tools/csv-to-yaml]

[browserling.com/tools/csv-to-yaml]: https://browserling.com/tools/csv-to-yaml


**Attention:**

- Merge adjacent entries as each entry only debits / credits an account.
  An transaction always involves 2 accounts (`from` and `to`).
  (For expenses basically copy the ledger-account from the second entry
  into the `from` field of the first entry)
- `from` and `to` might reversed for income
  (depending on how the `payee` field was used)
- Account names of Ledger-CLI are interpreted as tags
  Transity understands accounts as **physical accounts**
- The note is duplicated in the `tags` field.
  There is no way to get only the tags in Ledger-CLI 😔


## FAQ

### Why another plain text accounting tool?

Existing accounting tools are historically based on the notion of an account.
You add money (debit) and you remove money (credit).
(If this sounds backwards to you, read [this explanation])

[this explanation]:
  http://simplerestaurantaccounting.com/debit-and-credit-accounting-terminology-is-confusing

For example you get 50 € from your Mum and buy some Food.

```txt
Account | Debit   | Credit
--------|---------|--------
Wallet  | 50.00 € |
Wallet  |         | 20.00 €
---------------------------
```

Simple, but also incomplete.
Where did the money come from, where did it go?
This led to double entry bookkeeping.
Whenever you add some money to an account you have to remove the same
amount from another.


```txt
Account | Debit   | Credit
--------|---------|--------
Wallet  | 50.00 € |
Mum     |         | 50.00 €
Wallet  |         | 20.00 €
Food    | 20.00 € |
---------------------------
```

But you *must never forget a posting*,
because otherwise your account won't balance.

```txt
Account | Debit   | Credit
--------|---------|--------
Wallet  | 50.00 € |
Mum     |         | 50.00 €
Wallet  |         | 20.00 €
---------------------------
```

Oops, where did the money go? 🤷‍

If this looks (and sounds) confusing or too complicated, you're not alone!
It made sense in former times as this layout makes it easier
to add up the amounts by hand, but not in times of computers.

So how can we simplify it?
It's actually quite easy:
We just have to model it in terms of transactions, and not accounts.

```txt
Amount | From   | To
-------|--------|--------
50 €   | Mum    | Wallet
20 €   | Wallet | Food
-------------------------
```

- Simple - No more confusing debit / credit / asset / liability mumbo jumbo
- Intuitive - Just like you would talk about it
- Safe - It's obvious if you forget to fill out a field

Together with some further changes it yields a
**easier understandable, more robust and more complete**
representation of accounting!


### Why is it written in PureScript?

PureScript leverages strong static typing and can therefore
give more guarantees about the functionality of the code
than weakly typed or untyped languages (like JavaScript).

You wouldn't want your money get lost in rounding errors or
be turned to `undefined`, would you? 😉

Also:
Just like Haskell, it's a beautiful functional language!
Once you you've seen the light you wouldn't want to write any other
kind of language.


### Why is it not written in Haskell?

PureScript can also easily be used in the browser or get deployed
as a cloud function as it simply compiles to JavaScript.
With Haskell you'd have to use another language for a web frontend
or quarrel with experimental stuff like GHCJS.


## Related

- http://plaintextaccounting.org - Best of plain text accounting.
- https://cs007.blog - Personal finance for engineers.
- http://principlesofaccounting.com - Online tutorial on accounting.
- https://npoacct.sfconservancy.org -
    Effort to create accounting software for non-profit organizations.
- https://github.com/nuex/t - sh script for working with ledger timelog files.
- https://github.com/bankscrap/bankscrap -
    Ruby gem to extract balance and transactions from multiple banks.
- https://github.com/prashants/webzash -
    Easy to use web based double entry accounting software.

