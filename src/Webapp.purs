module Webapp where

import Data.Result (Result(..))
import Prelude (bind, pure, ($))
import Transity.Data.Config (ColorFlag(..))
import Transity.Data.Ledger as Ledger

getBalance :: String -> String
getBalance journal = do
  let
    result = do
      ledger <- Ledger.fromYaml journal
      pure $ Ledger.showBalance Ledger.BalanceAll ColorNo ledger

  case result of
    Ok output -> output
    Error message -> message
