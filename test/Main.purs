module Test.Main where

import Control.Monad.Eff (Eff)
import Data.Map as Map
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(Tuple))
import Prelude (Unit, discard, show, (#), ($))
import Test.Fixtures
  ( transaction
  , transactionJsonString
  , transactionYamlString
  , transactionShowed
  , transactionPretty

  , commodityMapPretty

  , ledger
  , ledgerJsonString
  , ledgerYamlString
  , ledgerShowed
  , ledgerPretty
  , ledgerBalance

  , ledgerMultiTrans
  , ledgerBalanceMultiTrans
  )
import Test.Spec
  ( describe
  -- , describeOnly
  , it
  -- , itOnly
  )
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Transity.Data.Account as Account
import Transity.Data.Amount
  ( Amount(Amount)
  , Commodity(Commodity)
  , prettyShowAmount
  )
import Transity.Data.Ledger
  ( jsonStringToLedger
  , yamlStringToLedger
  , prettyShowLedger
  , showBalance
  )
import Transity.Data.Transaction
  ( jsonStringToTransaction
  , yamlStringToTransaction
  , prettyShowTransaction
  )


rmWhitespace :: String -> String
rmWhitespace string =
  let
    whitespace = unsafeRegex "\\s+" global
  in
    replace whitespace "" string


main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Transity" $
    describe "Data" do
      describe "Amount" do
        it "pretty shows an amount" do
          let
            actual = prettyShowAmount (Amount 37.0 (Commodity "€"))
          actual `shouldEqual` "    37.000   €"


      describe "Account" do
        it "can add an amount to a commodity map" do
          let
            expectedMap = Map.fromFoldable
              [(Tuple (Commodity "€") (Amount 37.0 (Commodity "€")))]
            emptyMap = Map.empty :: Account.CommodityMap
            amount = Amount 37.0 (Commodity "€")
            actualMap = emptyMap `Account.addAmountToMap` amount
          actualMap `shouldEqual` expectedMap

        it "can subtract an amount from a commodity map" do
          let
            expectedMap = Map.fromFoldable
              [(Tuple (Commodity "€") (Amount 37.0 (Commodity "€")))]
            initialMap = Map.fromFoldable
              [(Tuple (Commodity "€") (Amount 42.0 (Commodity "€")))]
            amount = Amount 5.0 (Commodity "€")
            actualMap = initialMap `Account.subtractAmountFromMap` amount
          actualMap `shouldEqual` expectedMap

        it "pretty shows a commodity map" do
          let
            commodityMap = Map.fromFoldable
              [ (Tuple (Commodity "€") (Amount 42.0 (Commodity "€")))
              , (Tuple (Commodity "$") (Amount 12.0 (Commodity "$")))
              ]
            actualPretty = Account.prettyShowCommodityMap commodityMap
          actualPretty `shouldEqual` commodityMapPretty


      describe "Transaction" do
        it "converts a JSON string to a Transaction" do
          let
            actual = transactionJsonString
              # jsonStringToTransaction
              # show
              # rmWhitespace
          actual `shouldEqual` (rmWhitespace transactionShowed)


        it "converts a YAML string to a Transaction" do
          let
            actual = transactionYamlString
              # yamlStringToTransaction
              # show
              # rmWhitespace
          actual `shouldEqual` (rmWhitespace transactionShowed)


        it "pretty shows a transaction" do
          (prettyShowTransaction transaction) `shouldEqual` transactionPretty


      describe "Ledger" do
        it "converts a JSON string to a Ledger" do
          let
            actual = ledgerJsonString
              # jsonStringToLedger
              # show
              # rmWhitespace
          actual `shouldEqual` (rmWhitespace ledgerShowed)


        it "converts a YAML string to a Ledger" do
          let
            actual = ledgerYamlString
              # yamlStringToLedger
              # show
              # rmWhitespace
          actual `shouldEqual` (rmWhitespace ledgerShowed)


        it "pretty shows a ledger" do
          (prettyShowLedger ledger) `shouldEqual` ledgerPretty


        it "pretty shows the balance of all accounts" do
          (showBalance ledger) `shouldEqual` ledgerBalance


        it "supports multiple transactions on one account" do
          (showBalance ledgerMultiTrans) `shouldEqual` ledgerBalanceMultiTrans
