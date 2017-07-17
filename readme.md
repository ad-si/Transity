# Transity

The plain text accounting tool of the future.
Keep track of your 💵, 🕘, 🐖, 🐄, 🍻 on your command line.


## Import from Ledger CLI

```sh
bash -c '{ echo date,id,payee,account,commodity,amount,note; ledger csv; } \
> transactions.csv'
```

Convert `transactions.csv` to YAML with e.g. [browserling.com/tools/csv-to-yaml]

[browserling.com/tools/csv-to-yaml]:
  https://www.browserling.com/tools/csv-to-yaml


## Data structures

### Commodities

A commodity can be anything with can be assigned a quantity or amount.
E.g. money, time, pigs, cows, coordinates, messages.

There are 4 special commodities to simplify the tracking of sales:

1. Request
1. Offer
1. Acceptance
1. Certification


A table of commodities looks like this:

| Id | Name          | Description                                 | Alias Of |
|:--:|:--------------|:--------------------------------------------|:--------:|
| 1  | Request       | Request to exchange a commodity             |          |
| 2  | Offer         | Name commodity & expected trade item        |          |
| 3  | Acceptance    | Affirmation of interest in offered exchange |          |
| 3  | Certification | Acknowledgement that exchange was performed |          |
| 4  | €             | Currency used in the European Union         |          |
| 5  | EUR           |                                             |    4     |
| 6  | Cow           | Most common type of domesticated ungulates  |          |
| …  | …             | …                                           |    …     |


### Transfers

Movement of a commodity from one account to another at a specific point in time.
End datetime is optional and matches the start datetime if missing.

A table of transfers looks like this:

| Id | Transaction Id |  Start Datetime  |   End Datetime   |
|:--:|:--------------:|:----------------:|:----------------:|
| 1  |       1        | 2017-02-26 09:16 | 2017-02-26 09:17 |
| 2  |       1        | 2017-02-26 16:25 | 2017-02-26 16:28 |
| …  |       …        |        …         |        …         |

Continuation:

| Giver | Receiver | Amount | Commodity |
|:-----:|:--------:|:------:|:---------:|
|   1   |    2     |  300   |     4     |
|   2   |    1     |   1    |     6     |
|   …   |    …     |   …    |     …     |



### Transactions

Several related transfers which balance each other.
A transaction has following stages:

1. Request ("Hi, I'd like to buy something")
1. Offer ("Hi, I can sell you cow for 300 €")
1. Acceptance ("Ok, sounds good")
1. Fulfillment (Buyer gives seller the cow)
1. Fulfillment (Seller gives buyer the money)
1. Certification (Buyer gets receipt for successful transaction)

A table of transactions looks like this:

| Id | Titel                     |     Request      |
|:--:|:--------------------------|:----------------:|
| 1  | Buy cow at farmers market | 2017-02-25 09:16 |
| …  | …                         |        …         |

continuation …

|      Offer       |    Acceptance    |
|:----------------:|:----------------:|
| 2017-02-27 18:35 | 2017-02-27 18:37 |
|        …         |        …         |


### Accounts

An account is an entity which can store / contain / use commodities.

A table of accounts looks like this:

| Id |     Datetime     | Name      | Descriptions            |
|:--:|:----------------:|:----------|:------------------------|
| 1  | 2017-02-29 16:25 | Evil Corp | The Evil Corporation    |
| 2  | 2017-02-29 16:25 | John Doe  | CEO of Evil Corporation |
| …  |        …         | …         | …                       |


### Tags

A tag is a category / value which can be associated with an account or
a transfer.
The name can be namespaced with `:`.

| Id | Name       |
|:--:|:-----------|
| 1  | Food:Apple |
| 2  | Animal     |
| 2  | Car        |
| …  | …          |


## Backends

Transity can store the data in various backends as long as
they can produce following data structure:

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
