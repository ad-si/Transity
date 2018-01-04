module Test.Main where

import Control.Monad.Eff (Eff)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Rational (fromInt, (%))
import Data.Tuple (Tuple(Tuple))
import Prelude (Unit, discard, show, (#))
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
import Transity.Utils (digitsToRational)


rmWhitespace :: String -> String
rmWhitespace string =
  let
    whitespace = unsafeRegex "\\s+" global
  in
    replace whitespace "" string


main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Utils" do
    describe "digitsToRational" do
      it "converts 137 to 137/1" do
        (digitsToRational "137") `shouldEqual` (Just (137 % 1))

      it "converts 13 to 13/1" do
        (digitsToRational "13") `shouldEqual` (Just (13 % 1))

      it "converts 3 to 3/1" do
        (digitsToRational "3") `shouldEqual` (Just (3 % 1))

      it "converts 0 to 0/1" do
        (digitsToRational "0") `shouldEqual` (Just (0 % 1))

      it "converts 0.3 to 3/10" do
        (digitsToRational "0.3") `shouldEqual` (Just (3 % 10))

      it "converts .3 to 3/10" do
        (digitsToRational ".3") `shouldEqual` (Just (3 % 10))

      it "converts 0.300 to 3/10" do
        (digitsToRational "0.300") `shouldEqual` (Just (3 % 10))

      it "converts 2.1 to 21/10" do
        (digitsToRational "2.1") `shouldEqual` (Just (21 % 10))

      it "converts 3.21 to 321/100" do
        (digitsToRational "3.21") `shouldEqual` (Just (321 % 100))

      it "converts 12.3456 to 123456/10000" do
        (digitsToRational "12.3456") `shouldEqual` (Just (123456 % 10000))

      it "converts abc to Nothing" do
        (digitsToRational "abc") `shouldEqual` Nothing

  describe "Transity" do
    describe "Data" do
      describe "Amount" do
        it "pretty shows an amount" do
          let
            actual = prettyShowAmount (Amount (fromInt 37) (Commodity "€"))
          actual `shouldEqual` "    37.000   €"


      describe "Account" do
        it "can add an amount to a commodity map" do
          let
            expectedMap = Map.fromFoldable
              [(Tuple (Commodity "€") (Amount (fromInt 37) (Commodity "€")))]
            emptyMap = Map.empty :: Account.CommodityMap
            amount = Amount (fromInt 37) (Commodity "€")
            actualMap = emptyMap `Account.addAmountToMap` amount
          actualMap `shouldEqual` expectedMap

        it "can subtract an amount from a commodity map" do
          let
            expectedMap = Map.fromFoldable
              [(Tuple (Commodity "€") (Amount (fromInt 37) (Commodity "€")))]
            initialMap = Map.fromFoldable
              [(Tuple (Commodity "€") (Amount (fromInt 42) (Commodity "€")))]
            amount = Amount (fromInt 5) (Commodity "€")
            actualMap = initialMap `Account.subtractAmountFromMap` amount
          actualMap `shouldEqual` expectedMap

        it "pretty shows a commodity map" do
          let
            commodityMap = Map.fromFoldable
              [ (Tuple (Commodity "€") (Amount (fromInt 42) (Commodity "€")))
              , (Tuple (Commodity "$") (Amount (fromInt 12) (Commodity "$")))
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
