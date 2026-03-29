## FAQ

### Why another plain text accounting tool?

Existing accounting tools are historically based on the notion of an account.
You add money (debit) and you remove money (credit).
(If this sounds backwards to you, read [this explanation])

[this explanation]:
  http://simplerestaurantaccounting.com/debit-and-credit-accounting-terminology-is-confusing

For example you get 50 € from your mum and buy some food for 20 €.

```txt
Account | Debit | Credit
--------|-------|--------
Wallet  |  50 € |
Wallet  |       |   20 €
```

Simple, but also incomplete.
Where did the money come from, where did it go?
This led to double entry bookkeeping.
Whenever you add some money to an account,
you have to remove the same amount from another.


```txt
Account | Debit | Credit
--------|-------|--------
Wallet  |  50 € |
Mum     |       |   50 €
Wallet  |       |   20 €
Food    |  20 € |
```

But you *must never forget a posting*,
because otherwise your account won't balance.

```txt
Account | Debit | Credit
--------|-------|--------
Wallet  |  50 € |
Mum     |       |   50 €
Wallet  |       |   20 €
```

Oops, where did the money go? 🤷‍

If this looks (and sounds) confusing or too complicated, you're not alone!
It made sense in former times as this layout makes it easier
to add up the amounts by hand, but not in times of computers.

So how can we simplify it?
It's actually quite easy:
We just have to model it in terms of transactions, and not accounts.

```txt
From   | To     | Amount
-------|--------|--------
Mum    | Wallet |   50 €
Wallet | Food   |   20 €
```

- **Simple** - No more confusing debit / credit / asset / liability mumbo jumbo
- **Intuitive** - Just like you would talk about it
- **Safe** - It's obvious if you forget to fill out a field

Together with some further changes it yields an
**easier to understand, more robust and more complete**
representation of accounting!
