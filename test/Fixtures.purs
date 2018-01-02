module Test.Fixtures where

transactionJsonString :: String
transactionJsonString = """
{
  "entryDate": "2014-12-24",
  "valueDate": "2015-01-05",
  "from": "john:giro",
  "to": "evil-corp",
  "amount": "15 €",
  "desc": "Money for evil deal"
}
"""


transactionYamlString :: String
transactionYamlString = """
entryDate: '2014-12-24'
valueDate: '2015-01-05'
from: 'john:giro'
to: 'evil-corp'
amount: '15 €'
desc: 'Money for evil deal'
"""


transactionShowed :: String
transactionShowed = """
  (Right (Transaction
    { amount: (Amount 15.0 "€")
    , entryDate: (DateTime
      (Date (Year 2014) December (Day 24))
      (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
    , from: "john:giro"
    , to: "evil-corp"
    , valueDate: (DateTime
      (Date (Year 2015) January (Day 5))
      (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
    }
  ))
"""



ledgerJsonString :: String
ledgerJsonString = """
{
  "owner": "John Doe",
  "transactions": [
    {
      "entryDate": "2014-12-24",
      "valueDate": "2015-01-05",
      "from": "john:giro",
      "to": "evil-corp",
      "amount": "15 €",
      "desc": "Money for evil deal"
    },
    {
      "entryDate": "2015-01-05",
      "valueDate": "2015-01-05",
      "from": "good-inc",
      "to": "john:wallet",
      "amount": "100 €",
      "desc": "Money for another evil deal"
    }
  ]
}
"""


ledgerYamlString :: String
ledgerYamlString = """
owner: John Doe
additional: Additional values are ignored
transactions:
  - entryDate: '2014-12-24'
    valueDate: '2015-01-05'
    from: john:giro
    to: evil-corp
    amount: 15 €
    desc: Money for evil deal

  - entryDate: '2015-01-05'
    valueDate: '2015-01-05'
    from: good-inc
    to: john:wallet
    amount: 100 €
    desc: Money for another evil deal
    additional: Additional values are ignored
"""


ledgerShowed :: String
ledgerShowed = """
  (Right (Ledger
    { owner: "John Doe"
    , transactions:
      [ (Transaction
        { amount: (Amount 15.0 "€")
        , entryDate: (DateTime
          (Date (Year 2014) December (Day 24))
          (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
        , from: "john:giro"
        , to: "evil-corp"
        , valueDate: (DateTime
          (Date (Year 2015) January (Day 5))
          (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0))) })
      , (Transaction
        { amount: (Amount 100.0 "€")
        , entryDate: (DateTime
          (Date (Year 2015) January (Day 5))
          (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
        , from: "good-inc"
        , to: "john:wallet"
        , valueDate: (DateTime
          (Date (Year 2015) January (Day 5))
          (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
        })
      ]
    }
  ))
"""
