module Test.Main where

import Control.Monad.Eff (Eff)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Prelude (Unit, discard, show, (#))
import Test.Fixtures
  ( transactionJsonString
  , transactionYamlString
  , transactionShowed
  , ledgerJsonString
  , ledgerYamlString
  , ledgerShowed
  )
import Test.Spec
  ( describe
  , it
  -- , pending
  )
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Transity.Data.Ledger (jsonStringToLedger, yamlStringToLedger)
import Transity.Data.Transaction
  ( jsonStringToTransaction
  , yamlStringToTransaction
  )


rmWhitespace :: String -> String
rmWhitespace string =
  let
    whitespace = unsafeRegex "\\s+" global
  in
    replace whitespace "" string


main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Transity" do
    it "converts a JSON string to a Transaction" do
      let
        actual = transactionJsonString
          # jsonStringToTransaction
          # show
          # rmWhitespace
      actual `shouldEqual` (rmWhitespace transactionShowed)


    it "converts a JSON string to a Ledger" do
      let
        actual = ledgerJsonString
          # jsonStringToLedger
          # show
          # rmWhitespace
      actual `shouldEqual` (rmWhitespace ledgerShowed)


    it "converts a YAML string to a Transaction" do
      let
        actual = transactionYamlString
          # yamlStringToTransaction
          # show
          # rmWhitespace
      actual `shouldEqual` (rmWhitespace transactionShowed)


    it "converts a YAML string to a Ledger" do
      let
        actual = ledgerYamlString
          # yamlStringToLedger
          # show
          # rmWhitespace
      actual `shouldEqual` (rmWhitespace ledgerShowed)

