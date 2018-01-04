# Transity

The plain text accounting tool of the future.
Keep track of your 💵, 🕘, 🐖, 🐄, 🍻 on your command line.


## Why another plain text accounting tool?

Existing accounting tools are historically based on the notion of an account.
You add money (debit) and you remove money (credit).
(If this sounds backwards read [this explanation])

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

But you *must never forget an account*,
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

Together with some furter changes it yields a
**easier understandable and more robust & complete**
representation of accounting!

Let's introduce accounting to the 21. century! 😁


## List of Features

- Modeled on transactions instead of debiting / crediting accounts
- Easily editable & processable file format ([YAML](http://yaml.org))
- Clear separation between
  - Physical account (e.g. wallet, bank account) => spatial
  - Entities (e.g. my mum, a company) => relational
  - Purpose of transaction (food, travel) => functional
- No hardcoded asset / liability connotation as it is viewpoint dependent
  => Choose viewpoint when printing the balance
- High precision timestamps
- Support for all states of transaction lifecycle
  1. Request - Request to exchange a commodity
  1. Offer - Specification of commodity & expected trade item
  1. Acceptance - Affirmation of interest in offered exchange
  1. Fulfillments
  1. Certification - Acknowledgement that exchange was performed
- Support for any type of commodity (e.g. even time and messages)
- Differentiation between transfers, transactions and exchanges
- Meta data for all entities (transactions, accounts, entities, …)


## Import from Ledger CLI

```sh
bash -c '{ echo date,id,payee,account,commodity,amount,note; ledger csv; } \
> transactions.csv'
```

Convert `transactions.csv` to YAML with e.g. [browserling.com/tools/csv-to-yaml]

[browserling.com/tools/csv-to-yaml]:
  https://www.browserling.com/tools/csv-to-yaml


## Ledger Format

A ledger must be a YAML file with following format:

```yaml
commodities:
  - id: €
    name: Euro
    alias:
      - EUR
    desc: Currency used in the European Union
    utc: 2017-04-02 19:33:53

accounts:
  - id: anna
    name: Anna Smith
    utc: 2017-04-02 19:33:28
    tags:
      - person
    children:
      - id: wallet
        name: Wallet
        desc: Anna's black wallet
        utc: 2017-04-02 19:33:28
        tags:
          - wallet
  - id: evil-corp
    name: Evil Corporation
    utc: 2017-04-02 19:33:28
    desc: The Evil Corporation in the United States of Evil
    tags:
      - company

transactions:
  - title: Purchase of evil machine
    transfers:
      - utc: 2017-02-17
        from: anna
        amount: 50000 €
        to: evil-corp
      - utc: 2017-02-17
        from: evil-corp
        amount: 1 evil-machine
        to: anna
```


## TODO

- [ ] Nanoseconds
- [ ] List all used transaction keys (to easily spot typos)
- [ ] Verify transaction keys and values
- [ ] CSV import
- [ ] http://www.principlesofaccounting.com
