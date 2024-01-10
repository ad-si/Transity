module Test.Fixtures where

import Prelude ((<>), ($))

import Data.Map (fromFoldable)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Monoid (power)
import Data.Newtype (wrap, unwrap)
import Data.Rational ((%))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Transity.Data.Amount (Amount(..), Commodity(Commodity))
import Transity.Data.Account (Account(..))
import Transity.Data.Balance (Balance(..))
import Transity.Data.Entity as Entity
import Transity.Data.CommodityMap (CommodityMap)
import Transity.Data.Ledger (Ledger(..), BalanceMap)
import Transity.Data.Transaction (Transaction(..))
import Transity.Data.Transfer (Transfer(..))
import Transity.Utils (stringToDateTime, indentSubsequent)

-- | Transfer Examples

transferMinimal :: Transfer
transferMinimal = Transfer
  { utc: Nothing
  , from: "john:giro"
  , to: "evil-corp"
  , amount: Amount (15 % 1) (Commodity "€")
  , note: Nothing
  }

transferSimple :: Transfer
transferSimple = Transfer
  { utc: stringToDateTime "2014-12-24"
  , from: "john:giro"
  , to: "evil-corp"
  , amount: Amount (15 % 1) (Commodity "€")
  , note: Just "A note with special chars like < and &"
  }

transferSimpleJson :: String
transferSimpleJson = """
{
  "utc": "2014-12-24",
  "from": "john:giro",
  "to": "evil-corp",
  "amount": "15 €",
  "note": "A note with special chars like < and &"
}
"""

transferSimpleYaml :: String
transferSimpleYaml = """
utc: '2014-12-24'
from: john:giro
to: evil-corp
amount: 15 €
note: A note with special chars like < and &
"""

transferSimpleShowed :: String
transferSimpleShowed = """
(Transfer
  { amount: (Amount 15 % 1 (Commodity "€"))
  , from: "john:giro"
  , note: (Just "A note with special chars like < and &")
  , to: "evil-corp"
  , utc: (Just (DateTime
      (Date (Year 2014) December (Day 24))
      (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0))))
  }
)
"""

transferSimplePretty :: String
transferSimplePretty = "\
  \2014-12-24 00:00 \
  \|       john:giro ->       evil-corp :    15    €          \
  \| A note with special chars like < and &\n\
  \" -- Fix syntax highlighting: "

transferSimpleB :: Transfer
transferSimpleB = Transfer
  { utc: stringToDateTime "2007-03-29"
  , from: "evil-corp"
  , to: "flower-power"
  , amount: Amount (7 % 1) (Commodity "€")
  , note: Just "Bought some flowers"
  }

transferSimpleBShowed :: String
transferSimpleBShowed = """
(Transfer
  { amount: (Amount 7 % 1 (Commodity "USD"))
  , from: "carlos:wallet"
  , note: (Just "Bought some flowers")
  , to: "flower-power"
  , utc: (Just (DateTime
      (Date (Year 2007) March (Day 29))
      (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0))))
  }
)
"""


-- | Transaction Examples

transactionZero :: Transaction
transactionZero = Transaction
  { id: Nothing
  , utc: Nothing
  , note: Nothing
  , files: []
  , transfers: []
  }


transactionNoAccount :: String
transactionNoAccount = """
owner: John
transactions:
  - transfers:
    - from: John
      to: ""
      amount: 7 €
"""

transactionNoAccountPretty :: String
transactionNoAccountPretty = ""
  <> " " `power` 86 <> "7.00 €       \n"
  <> " " `power` 76 <> "John     -7.00 €       \n"


transactionSimple :: Transaction
transactionSimple = Transaction
  { id: Just "abcxyz"
  , utc: stringToDateTime "2014-12-24"
  , note: Just "A short note about this transaction"
  -- Used for testing HYPERLINKs in XLSX files
  , files: ["fixtures/example.txt"]
  , transfers: [ transferSimple ]
  }

transactionSimpleJson :: String
transactionSimpleJson = """
{
  "id": "abcxyz",
  "utc": "2014-12-24",
  "note": "A short note about this transaction",
  "transfers": [
    """ <> transferSimpleJson <> """
  ]
}
"""

transactionSimpleYaml :: String
transactionSimpleYaml = """
id: abcxyz
utc: '2014-12-24'
note: A short note about this transaction
transfers:
  - """ <> indentSubsequent 4 transferSimpleYaml <> """
"""

transactionSimpleShowed :: String
transactionSimpleShowed = """
  (Transaction
    { files: []
    , id: (Just "abcxyz")
    , note: (Just "A short note about this transaction")
    , transfers:
      [ """ <> transferSimpleShowed <> """
      ]
    , utc: (Just (DateTime
        (Date (Year 2014) December (Day 24))
        (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0))))
    }
  )
"""


transactionSimplePretty :: String
transactionSimplePretty = "\
  \2014-12-24 00:00 | A short note about this transaction | (id abcxyz)\n\
  \    " <> transferSimplePretty <> "\
  \    \n\
  \" -- Fix syntax highlighting: "


transactionSimpleB :: Transaction
transactionSimpleB = Transaction
  { id: Just "defghi"
  , utc: stringToDateTime "2001-05-13"
  , note: Just "Another note"
  , files: ["filepath/to/another-receipt.pdf"]
  , transfers: [ transferSimpleB ]
  }

transactionSimpleBShowed :: String
transactionSimpleBShowed = """
  (Transaction
    { id: (Just "defghi")
    , note: (Just "Another note")
    , files: ["filepath/to/another-receipt.pdf"]
    , transfers:
      [ """ <> transferSimpleBShowed <> """
      ]
    , utc: (Just (DateTime
        (Date (Year 2001) May (Day 13))
        (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0))))
    }
  )
"""


accountPretty :: String
accountPretty = ""
  <> "test  12 $\n"
  <> "      2 EUR\n"

accountPrettyAligned :: String
accountPrettyAligned = ""
  <> "  test       12         $        \n"
  <> "              2         EUR      \n"


commodityMapPretty :: String
commodityMapPretty = ""
  <> "12 $\n"
  <> "2 EUR"


commodityMapPrettyAligned :: String
commodityMapPrettyAligned = ""
  <> "     12         $        \n"
  <> "      2         EUR      "

-- | Ledger Examples

ledger :: Ledger
ledger = Ledger
  { owner: "John Doe"
  , entities: Nothing
  , transactions:
      [ transactionSimple ]
  }


ledgerMultiTrans :: Ledger
ledgerMultiTrans = Ledger
  { owner: "John Doe"
  , entities: Nothing
  , transactions:
      [ transactionSimple
      , transactionSimpleB
      ]
  }


ledgerEntities :: Ledger
ledgerEntities = Ledger
  { owner: "John Doe"
  , entities: Just
      [ wrap $ (unwrap Entity.zero) { id = "Anna" }
      , wrap $ (unwrap Entity.zero) { id = "Bob" }
      ]
  , transactions: []
  }


ledgerEntitiesShowed :: String
ledgerEntitiesShowed = """
  entities:
    - id: Anna
    - id: Bob
"""


ledgerJson :: String
ledgerJson = """
{
  "entities": [
    {"id": "abcxyz"},
    {"id": "evil-corp"},
    {"id": "john:giro"}
  ],
  "owner": "John Doe",
  "transactions": [
    """ <> transactionSimpleJson <> """
  ]
}
"""


balanceJson :: String
balanceJson = """
{
  "utc": "2017-04-02 20:11:45",
  "amounts": ["7 €", "-8 $", "+9 BTC"]
}
"""

balanceShowed :: String
balanceShowed = """
(Balance
  (DateTime
    (Date (Year 2017) April (Day 2))
    (Time (Hour 20) (Minute 11) (Second 45) (Millisecond 0)))
  (fromFoldable
    [ (Tuple (Commodity "$") (Amount -8 % 1 (Commodity "$")))
    , (Tuple (Commodity "BTC") (Amount 9 % 1 (Commodity "BTC")))
    , (Tuple (Commodity "€") (Amount 7 % 1 (Commodity "€")))
    ]))
"""


commodityMap :: CommodityMap
commodityMap = fromFoldable
  [(Tuple
    (Commodity "€")
    (Amount (100 % 1) (Commodity "€")))
  ]


balanceMap :: BalanceMap
balanceMap =
  fromFoldable [Tuple "john" commodityMap]


account :: Account
account = Account
  { id: "wallet"
  , commodityMap
  , balances: Just
      [ (Balance
        (unsafePartial $ fromJust $ stringToDateTime "2017-04-02 20:11:45")
        (fromFoldable
          [(Tuple (Commodity "€") (Amount (100 % 1) (Commodity "€")))]))
      ]
  }


accountJson :: String
accountJson = """
{ "id": "_default_",
  "balances": [
    { "utc": "2017-04-02 20:11:45",
      "amounts": ["100 €"]
    },
    { "utc": "2014-05-01 12:00",
      "amounts": ["200 €", "1 evil_machine"]
    }]}
"""


accountShowed :: String
accountShowed = """
(Account
  { balances: (Just
      [ (Balance
          (DateTime
            (Date (Year 2017) April (Day 2))
            (Time (Hour 20) (Minute 11) (Second 45) (Millisecond 0)))
          (fromFoldable
            [(Tuple
                (Commodity "€")
                (Amount 100 % 1 (Commodity "€")
                ))]))
      , (Balance
          (DateTime
            (Date (Year 2014) May (Day 1))
            (Time (Hour 12) (Minute 0) (Second 0) (Millisecond 0)))
          (fromFoldable
            [ (Tuple
                (Commodity "evil_machine")
                (Amount 1 % 1 (Commodity "evil_machine")))
            , (Tuple
                (Commodity "€")
                (Amount 200 % 1 (Commodity "€")))
            ]))
      ])
  , commodityMap: (fromFoldable [])
  , id: "_default_"
  })
"""


entityJson :: String
entityJson = """
{ "id": "john",
  "accounts": [
    { "id": "_default_",
      "balances": [
        { "utc": "2017-04-02 20:11:45",
          "amounts": ["100 €"]
        },
        { "utc": "2014-05-01 12:00",
          "amounts": ["200 €", "1 evil_machine"] } ] } ] }
"""

entityShowed :: String
entityShowed = """
(Entity
  {accounts: (Just
    [ (Account
        { balances: (Just
            [ (Balance
                (DateTime
                  (Date (Year 2017) April (Day 2))
                  (Time (Hour 20) (Minute 11) (Second 45) (Millisecond 0)))
                (fromFoldable
                  [ (Tuple
                      (Commodity "€")
                      (Amount 100 % 1 (Commodity "€")))
                  ]))
            , (Balance
                (DateTime
                  (Date (Year 2014) May (Day 1))
                  (Time (Hour 12) (Minute 0) (Second 0) (Millisecond 0)))
                (fromFoldable
                  [ (Tuple
                      (Commodity "evil_machine")
                      (Amount 1 % 1 (Commodity "evil_machine")))
                  , (Tuple
                      (Commodity "€")
                      (Amount 200 % 1 (Commodity "€")))
                  ]))
            ])
        , commodityMap: (fromFoldable [])
        , id: "_default_"
        })
    ])
  , id: "john"
  , name: Nothing
  , note: Nothing
  , tags: Nothing
  , utc: Nothing
  })
"""

ledgerYaml :: String
ledgerYaml = """
entities:
  - id: abcxyz
  - id: evil-corp
  - id: john:giro
owner: John Doe
additional: Additional values are ignored
transactions:
  - """ <> indentSubsequent 4 transactionSimpleYaml <> """
"""

ledgerLedger :: String
ledgerLedger  = """2014-12-24 A short note about this transaction
  evil-corp  15 €
  john:giro
"""

idToEntityStr :: String -> String
idToEntityStr id = """
(Entity
  { accounts: Nothing
  , id: """ <> "\"" <> id <> "\"" <> """
  , name: Nothing
  , note: Nothing
  , tags: Nothing
  , utc: Nothing
  })
"""


ledgerShowed :: String
ledgerShowed = """
  (Ledger
    { entities: (Just
      [ """ <> idToEntityStr "abcxyz" <> """
      , """ <> idToEntityStr "evil-corp" <> """
      , """ <> idToEntityStr "john:giro" <> """
      ])
    , owner: "John Doe"
    , transactions:
      [ """ <> transactionSimpleShowed <> """
      ]
    }
  )
"""


ledgerPretty :: String
ledgerPretty = """Journal for "John Doe"
================================================================================
2014-12-24 00:00 | A short note about this transaction | (id abcxyz)
    """ <> transferSimplePretty <> "    \n"


ledgerBalanceOwner :: String
ledgerBalanceOwner = ""
  <> "  john:giro  -15 €\n"


ledgerBalanceAll :: String
ledgerBalanceAll = ""
  <> "  evil-corp   15 €\n"
  <> "  john:giro  -15 €\n"


ledgerBalanceMultiTrans :: String
ledgerBalanceMultiTrans = ""
  <> "     evil-corp    8 €\n"
  <> "  flower-power    7 €\n"
  <> "     john:giro  -15 €\n"
