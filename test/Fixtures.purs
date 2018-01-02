module Test.Fixtures where

import Transity.Data.Amount (Amount(..))
import Transity.Data.Ledger (Ledger(..))
import Transity.Data.Transaction (Transaction(..))
import Transity.Utils (stringToDateTime)


transaction :: Transaction
transaction = Transaction
  { entryDate: stringToDateTime "2014-12-24"
  , valueDate: stringToDateTime "2015-01-05"
  , from: "john:giro"
  , to: "evil-corp"
  , amount: Amount 15.0 "€"
  }


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


transactionPretty :: String
transactionPretty = "\
  \2015-01-05T00:00:00 |       john:giro =>       evil-corp     15.000   € | \n\
  \" -- Fix syntax highlighting: "


ledger :: Ledger
ledger = Ledger
  { owner: "John Doe"
  , transactions:
      [ Transaction
        { entryDate: stringToDateTime "2014-12-24"
        , valueDate: stringToDateTime "2015-01-05"
        , from: "john:giro"
        , to: "evil-corp"
        , amount: Amount 15.0 "€"
        }
      , Transaction
        { entryDate: stringToDateTime "2015-01-05"
        , valueDate: stringToDateTime "2015-01-05"
        , from: "good-inc"
        , to: "john:wallet"
        , amount: Amount 100.0 "€"
        }
      ]
  }


ledgerMultiTrans :: Ledger
ledgerMultiTrans = Ledger
  { owner: "John Doe"
  , transactions:
      [ Transaction
        { entryDate: stringToDateTime "2014-12-20"
        , valueDate: stringToDateTime "2014-12-21"
        , from: "john:giro"
        , to: "john:wallet"
        , amount: Amount 80.0 "€"
        }
      , Transaction
        { entryDate: stringToDateTime "2015-01-01"
        , valueDate: stringToDateTime "2015-01-02"
        , from: "john:wallet"
        , to: "john:giro"
        , amount: Amount 60.0 "€"
        }
      ]
  }


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
          (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
        })
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


ledgerPretty :: String
ledgerPretty = "\
  \Ledger by John Doe\n\
  \2015-01-05T00:00:00 |       john:giro =>       evil-corp     15.000   € | \n\
  \2015-01-05T00:00:00 |        good-inc =>     john:wallet    100.000   € | \n\
  \" -- Fix syntax highlighting: "


ledgerBalance :: String
ledgerBalance = "\
  \                                                   john:giro   -15.000   €\n\
  \                                                   evil-corp    15.000   €\n\
  \                                                    good-inc  -100.000   €\n\
  \                                                 john:wallet   100.000   €\n\
  \" -- fix syntax highlighting: "


ledgerBalanceMultiTrans :: String
ledgerBalanceMultiTrans = "\
  \                                                   john:giro   -20.000   €\n\
  \                                                 john:wallet    20.000   €\n\
  \" -- fix syntax highlighting: "
