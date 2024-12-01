## Comparison

[Ledger] and [Hledger]'s transactions are a (balanced) group of account postings.
Transity's transactions are a group of transfers between two accounts.

[Ledger]: https://www.ledger-cli.org/
[Hledger]: https://hledger.org/


### Syntax

Checkout the files [hledger.journal] and [journal.yaml]
for similar transactions modeled in Hledger and in Transity.

[hledger.journal]: ./examples/hledger.journal
[journal.yaml]: ./examples/journal.yaml

There is a lot of ambiguity in the ledger journal format.
Are you able to tell the difference between the 2 options?


```ledger
2019-01-17 Bought food
  expenses:food  $10
  assets:cash
```

vs

```ledger
2019-01-17 Bought food
  assets:cash
  expenses:food  $10
```

Also, it lacks some fields for more precise recording
of which parties where involved.

- What food?
- Where did you buy it?
- When exactly did you buy it?
- Which supermarket?

```ledger
2019-01-17 Bought food
  expenses:food  $10
  assets:cash
```

#### Example

A Transity entry as a transaction made of up four transfers:

```yaml
utc: '2024-04-16 18:50:28'
tags: [domain]
note: 1 year registration of domain "ad-si.com"
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
    note: ad-si.com
```


**Possible Translations to Hledger:**

**Version A:**

```ledger
2024-04-16 1 year registration of domain "ad-si.com"  ; domain:
  adrian:hypo:giro  -9.94 €
  paypal             9.94 €        ; transaction-id: 24360863
  paypal           -10.87 $
  namecheap         10.87 $
  namecheap        -0.18 $
  icann             0.18 $         ; fee:
  namecheap        -1 domain-year
  feram             1 domain-year  ; note: ad-si.com
```

**Version B**:

```ledger
2024-04-16 1 year registration of domain "ad-si.com"  ; domain:, transaction-id: 24360863
  adrian:hypo:giro
  paypal             9.94 €

2024-04-16 1 year registration of domain "ad-si.com"  ; domain:
  paypal
  namecheap         10.87 $

2024-04-16 1 year registration of domain "ad-si.com"  ; domain:, fee:
  namecheap
  icann             0.18 $

2024-04-16 1 year registration of domain "ad-si.com"  ; domain:, note: ad-si.com
  namecheap
  feram             1 domain-year
```


### Reporting

```shell
hledger --file examples/hledger.journal balance
# vs
transity balance examples/journal.yaml
```

```shell
hledger --file examples/hledger.journal register
# vs
transity transactions examples/journal.yaml
```

```shell
hledger --file examples/hledger.journal register --output-format=csv
# vs
transity entries examples/journal.yaml
```


### Features

Several features are missing in Ledger and Hledger:

- No support for precise timestamps (transactions only have an associated date)
- No first class support for Gnuplot
  (Check out [Report Scripts for Ledger CLI with Gnuplot] for some scripts)

[Report Scripts for Ledger CLI with Gnuplot]:
  https://www.sundialdreams.com/report-scripts-for-ledger-cli-with-gnuplot


### Performance

Measured with hyperfine including 3 warmups on an early 2015 MacBook Pro.

*For a journal file with around 2000 entries:*

Transity:
```txt
Benchmark #1: transity balance journals/main.yaml
  Time (mean ± σ):      1.287 s ±  0.021 s    [User: 1.790 s, System: 0.140 s]
  Range (min … max):    1.250 s …  1.324 s    10 runs
```

Hledger:
```txt
Benchmark #1: hledger -f test.ledger balance
  Time (mean ± σ):     409.6 ms ±   6.1 ms    [User: 366.6 ms, System: 28.5 ms]
  Range (min … max):   398.8 ms … 417.6 ms    10 runs
```

Ledger:
```txt
Benchmark #1: ledger -f test.ledger balance
  Time (mean ± σ):      76.3 ms ±   9.1 ms    [User: 62.7 ms, System: 9.4 ms]
  Range (min … max):    65.1 ms … 101.8 ms    28 runs
```
