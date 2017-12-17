module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)


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


transactionYamlString :: String
transactionYamlString = """
entryDate: '2014-12-24'
valueDate: '2015-01-05'
from: 'john:giro'
to: 'evil-corp'
amount: '15 €'
desc: 'Money for evil deal'
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


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  -- Test parsing of JSON
  log $ show $ jsonStringToTransaction transactionJsonString
  log $ show $ jsonStringToLedger ledgerJsonString

  -- Test parsing of YAML
  log $ show $ yamlStringToTransaction transactionYamlString
  log $ show $ yamlStringToLedger ledgerYamlString
