# Guide

## Entry / Value Date

There are no separate fields for entry or value dates necessary.
Simply use ISO 8601 [time intervals] to specify the duration of a transfer.

[time intervals]: https://en.wikipedia.org/wiki/ISO_8601#Time_intervals

```yaml
transactions:
  - id: '123456789'
    note: Deposit of savings
    transfers:
      - date: 2018-01-04T12:00--05T22:10
        from: john
        to: bank
        amount: 100 €
```


## Syntax

This is an alternative syntax for the YAML ledger file.
It is highly experimental and feedback is very welcome!

```transity
2016-04-16 18:50:28
#20135604
1 year registration of domain "example.org"
john      -> paypal    :  9.95 €
paypal    -> namecheap : 10.69 $
paypal    -> icann     :  0.18 $
namecheap -> john      :  1    Domain
```