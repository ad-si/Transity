## Ideas

- Features for duplicates
    - Print list of possible duplicates
    - Label an entry explicitly as a duplicate to store it in several places
- CSV import
- Dashboard
- Budgets (including progress visualization)
- Cache-files to speed up processing of large data sets
- Generate EPC QR Codes for transfers
- LSP server for journal files
- Export to [Graphviz] (for account / entity relations)
- Export to [JS-Sequence-Diagrams] (sequence of transactions)
- Meta data for all entities (transactions, accounts, entities, …)
- Nanosecond precision for timestamps
- Additional features for crypto currencies
- Commodities
    - Treat as scientific units (e.g 1 k€ == 1000 €)
    - First class support for any type of commodity (e.g. time and messages)
    - Support for time limited commodities (e.g. subscription for a month)
    - Define which are allowed / prohibited for each account
    - Hard vs Soft vs Fungible vs …
- Differentiation between transfers, transactions & exchanges
    - Special syntax for exchanges
- Support for all states of transaction life cycle
    1. Request - Request to exchange a commodity
    1. Offer - Specification of commodity & expected trade item
    1. Acceptance - Affirmation of interest in offered exchange
    1. Fulfillments
    1. Certification - Acknowledgment that exchange was performed

[Graphviz]: https://graphviz.org
[JS-Sequence-Diagrams]: https://bramp.github.io/js-sequence-diagrams


### Entry / Value Date

There are no separate fields for entry or value dates necessary.
Simply use ISO 8601 [time intervals] to specify the duration of a transfer.

[time intervals]: https://en.wikipedia.org/wiki/ISO_8601#Time_intervals

```yaml
transactions:
  - id: '123456789'
    note: Deposit of savings
    transfers:
      - utc: 2018-01-04T12:00--05T22:10
        from: john
        to: bank
        amount: 100 €
```


### Syntax

This is a first concept of an alternative syntax for the journal file:

```transity
# Comments after a hash

2016-04-16 18:50:28
| 1 year registration of domain "example.org"
+tagOne  # Tags are written after a plus
+tagTwo
id: 20135604  # Arbitrary metadata
# Transactions are indentend by 2 spaces
  john -> paypal : 9.95 €
  paypal -> namecheap : {10 + 0.69} $
  paypal -> icann : 0.18 $ +fee
  namecheap -> john : 1 Domain
```

### Database Backend

Alternatively the data could be stored in a database
so that the YAML file is only produced temporarily for viewing.


#### Relational Data Structure

##### Commodities

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


##### Transfers

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



##### Transactions

Several related transfers which balance each other.
A transaction has following stages:

1. Request ("Hi, I'd like to buy something")
1. Offer ("Hi, I can sell you a cow for 300 €")
1. Acceptance ("Ok, sounds good")
1. Fulfillment (Buyer gives seller the money)
1. Fulfillment (Seller gives buyer the cow)
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


##### Accounts

An account is an entity which can store / contain / use commodities.

A table of accounts looks like this:

| Id |     Datetime     | Name      | Descriptions            |
|:--:|:----------------:|:----------|:------------------------|
| 1  | 2017-02-29 16:25 | Evil Corp | The Evil Corporation    |
| 2  | 2017-02-29 16:25 | John Doe  | CEO of Evil Corporation |
| …  |        …         | …         | …                       |


##### Tags

A tag is a category / value which can be associated with an account or
a transfer.
The name can be namespaced with `:`.

| Id | Name       |
|:--:|:-----------|
| 1  | Food:Apple |
| 2  | Animal     |
| 2  | Car        |
| …  | …          |
