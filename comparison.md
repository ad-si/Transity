# Comparison of Transity and Hledger Entries

## A transity entry: a transaction made of up four "transfers":

```yaml
utc: '2016-04-16 18:50:28'
tags: [domain]
note: 1 year registration of domain "ferambot.com"
transfers:
      - from: adrian:hypo:giro
        to: paypal
        amount: 9.94 €
        transaction-id: '24360863'
      - from: paypal
        to: namecheap
        amount: 10.87 $
      - from: namecheap
        to: icann
        amount: 0.18 $
        tag: [fee]
      - from: namecheap
        to: feram
        amount: 1 domain-year
        note: ferambot.com
```


## Possible Translations to Hledger:

### Version A

```ledger
2016-04-16 1 year registration of domain "ferambot.com"  ; domain:
  adrian:hypo:giro  -9.94 €
  paypal             9.94 €        ; transaction-id: 24360863
  paypal           -10.87 $
  namecheap         10.87 $
  namecheap        -0.18 $
  icann             0.18 $         ; fee:
  namecheap        -1 domain-year
  feram             1 domain-year  ; note: ferambot.com
```


## Version B

```ledger
2016-04-16 1 year registration of domain "ferambot.com"  ; domain:, transaction-id: 24360863
  adrian:hypo:giro
  paypal             9.94 €

2016-04-16 1 year registration of domain "ferambot.com"  ; domain:
  paypal
  namecheap         10.87 $

2016-04-16 1 year registration of domain "ferambot.com"  ; domain:, fee:
  namecheap
  icann             0.18 $

2016-04-16 1 year registration of domain "ferambot.com"  ; domain:, note: ferambot.com
  namecheap
  feram             1 domain-year
```
