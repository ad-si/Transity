# Transity

Keep track of your 💵, 🕘, 🐖, 🐄, 🍻 or anything else.


## Transactions

Several related transfers which balance each other.
A transaction has following stages:

1. Request by  ("Hi, I'd like to buy something")
1. Offer ("Hi, I can sell you cow for 300 €")
1. Acceptance ("Ok, sounds good")
1. Fulfillment for Requestor ("Gets the cow")
1. Fulfillment for Providers ("Gets the money")
1. Requestor gets receipt for successful transaction

A table of transactions looks like this:

| Id | Titel                     |     Request      |      Offer       |
|:--:|:--------------------------|:----------------:|:----------------:|
| 1  | Buy cow at farmers market | 2017-02-25 09:16 | 2017-02-27 18:35 |
| …  | …                         |        …         |        …         |


## Account

An account is an entity which can store / contain / use commodities.
The name can be namespaced with `:`.

A table of accounts looks like this:

| Id |     Datetime     | Name              |
|:--:|:----------------:|:------------------|
| 1  | 2017-02-29 16:25 | Company:Evil Corp |
| 2  | 2017-02-29 16:25 | Person:John Doe   |
| …  |        …         | …                 |


## Commodity

A commodity can be anything with can be assigned a quantity or amount.
E.g. money, time, pigs, cows, coordinates, messages

A table of commodities looks like this:

| Id | Title         | Description                                 | Alias Of |
|:--:|:--------------|:--------------------------------------------|:--------:|
| 1  | Request       | Request to exchange a commodity             |          |
| 2  | Offer         | Name commodity & expected trade item        |          |
| 3  | Acceptance    | Affirmation of interest in offered exchange |          |
| 3  | Certification | Acknowledgement that exchange was performed |          |
| 4  | Currency:€    | Currency used in the European Union         |          |
| 5  | Currency:EUR  |                                             |    4     |
| 6  | Animal:Cow    | Most common type of domesticated ungulates  |          |
| …  | …             | …                                           |    …     |


There are 4 special commodities to simplify the tracking of sales:

1. Request
1. Offer
1. Acceptance
1. Certification


## Transfer

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
|   1   |    2     |  300   |     €     |
|   2   |    1     |   1    |    Cow    |
|   …   |    …     |   …    |     …     |
