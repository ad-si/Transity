# Database Backend

Alternatively the data can be stored in a database
so that the YAML file is only produced temporarily for viewing


## Relational Data Structure

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
