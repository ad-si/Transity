module Test.Fixtures where

import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (power)
import Data.Rational (fromInt)
import Prelude ((<>))
import Transity.Data.Amount (Amount(..), Commodity(Commodity))
import Transity.Data.Ledger (Ledger(..))
import Transity.Data.Transaction (Transaction(..))
import Transity.Data.Transfer (Transfer(..))
import Transity.Utils (stringToDateTime, indentSubsequent)

-- | Transfer Examples

transferZero :: Transfer
transferZero = Transfer
  { utc: Nothing
  , from: ""
  , to: ""
  , amount: Amount (fromInt 0) (Commodity "")
  , note: Nothing
  }

transferMinimal :: Transfer
transferMinimal = Transfer
  { utc: Nothing
  , from: "john:giro"
  , to: "evil-corp"
  , amount: Amount (fromInt 15) (Commodity "€")
  , note: Nothing
  }

transferSimple :: Transfer
transferSimple = Transfer
  { utc: Just (stringToDateTime "2014-12-24")
  , from: "john:giro"
  , to: "evil-corp"
  , amount: Amount (fromInt 15) (Commodity "€")
  , note: Just "A little note"
  }

transferSimpleJson :: String
transferSimpleJson = """
{
  "utc": "2014-12-24",
  "from": "john:giro",
  "to": "evil-corp",
  "amount": "15 €",
  "note": "A little note"
}
"""

transferSimpleYaml :: String
transferSimpleYaml = """
utc: '2014-12-24'
from: john:giro
to: evil-corp
amount: 15 €
note: A little note
"""

transferSimpleShowed :: String
transferSimpleShowed = """
(Transfer
  { amount: (Amount 15 % 1 (Commodity "€"))
  , from: "john:giro"
  , note: (Just "A little note")
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
  \| A little note\n\
  \" -- Fix syntax highlighting: "

transferSimpleB :: Transfer
transferSimpleB = Transfer
  { utc: Just (stringToDateTime "2007-03-29")
  , from: "evil-corp"
  , to: "flower-power"
  , amount: Amount (fromInt 7) (Commodity "€")
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
  , receipt: Nothing
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
  , utc: Just (stringToDateTime "2014-12-24")
  , note: Just "A short note about this transaction"
  , receipt: Just "filepath/to/receipt.pdf"
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
    { id: (Just "abcxyz")
    , note: (Just "A short note about this transaction")
    , receipt: Nothing
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
  , utc: Just (stringToDateTime "2001-05-13")
  , note: Just "Another note"
  , receipt: Just "filepath/to/another-receipt.pdf"
  , transfers: [ transferSimpleB ]
  }

transactionSimpleBShowed :: String
transactionSimpleBShowed = """
  (Transaction
    { id: (Just "defghi")
    , note: (Just "Another note")
    , receipt: (Just "filepath/to/another-receipt.pdf")
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
  <> "test  12.0 $\n"
  <> "      2.0 EUR\n"

accountPrettyAligned :: String
accountPrettyAligned = ""
  <> "  test       12         $        \n"
  <> "              2         EUR      \n"


commodityMapPretty :: String
commodityMapPretty = ""
  <> "12.0 $\n"
  <> "2.0 EUR"


commodityMapPrettyAligned :: String
commodityMapPrettyAligned = ""
  <> "     12         $        \n"
  <> "      2         EUR      "

-- | Ledger Examples

ledger :: Ledger
ledger = Ledger
  { owner: "John Doe"
  , transactions:
      [ transactionSimple ]
  }


ledgerMultiTrans :: Ledger
ledgerMultiTrans = Ledger
  { owner: "John Doe"
  , transactions:
      [ transactionSimple
      , transactionSimpleB
      ]
  }


ledgerJson :: String
ledgerJson = """
{
  "owner": "John Doe",
  "transactions": [
    """ <> transactionSimpleJson <> """
  ]
}
"""


ledgerYaml :: String
ledgerYaml = """
owner: John Doe
additional: Additional values are ignored
transactions:
  - """ <> indentSubsequent 4 transactionSimpleYaml <> """
"""


ledgerShowed :: String
ledgerShowed = """
  (Ledger
    { owner: "John Doe"
    , transactions:
      [ """ <> transactionSimpleShowed <> """
      ]
    }
  )
 """


ledgerPretty :: String
ledgerPretty = """Ledger for "John Doe"
================================================================================
2014-12-24 00:00 | A short note about this transaction | (id abcxyz)
    """ <> transferSimplePretty <> "    \n"


ledgerBalance :: String
ledgerBalance = ""
  <> "  evil-corp   15 €\n"
  <> "  john:giro  -15 €\n"


ledgerBalanceMultiTrans :: String
ledgerBalanceMultiTrans = ""
  <> "     evil-corp    8 €\n"
  <> "  flower-power    7 €\n"
  <> "     john:giro  -15 €\n"
