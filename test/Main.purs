module Test.Main where

import Control.Applicative (when)
import Control.Bind (discard)
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff)
import Data.Array (zipWith)
import Data.Eq ((/=))
-- import Data.Foldable (and)
import Data.Foldable (fold)
import Data.Function ((#))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Rational (fromInt, (%))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String (toCharArray, length)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit)
import Test.Fixtures
  ( accountPretty
  , commodityMapPretty
  , ledger
  , ledgerBalance
  , ledgerBalanceMultiTrans
  , ledgerJson
  , ledgerMultiTrans
  , ledgerPretty
  , ledgerShowed
  , ledgerYaml
  , transactionSimple
  , transactionSimpleJson
  , transactionSimplePretty
  , transactionSimpleShowed
  , transactionSimpleYaml
  , transferSimple
  , transferSimpleJson
  , transferSimplePretty
  , transferSimpleShowed
  )
import Test.Spec
  ( describe
  , it
  -- , describeOnly
  -- , itOnly
  )
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Transity.Data.Account (Account(..), CommodityMap)
import Transity.Data.Account
  ( addAmountToMap
  , subtractAmountFromMap
  , showPretty
  , commodityMapShowPretty
  ) as Account
import Transity.Data.Amount (Amount(..), Commodity(..))
import Transity.Data.Amount (showPretty) as Amount
import Transity.Data.Ledger (Ledger(..))
import Transity.Data.Ledger as Ledger
import Transity.Data.Transaction (Transaction(..))
import Transity.Data.Transaction as Transaction
import Transity.Data.Transfer (Transfer(..))
import Transity.Data.Transfer (fromJson, showPretty) as Transfer
import Transity.Utils (digitsToRational, indentSubsequent)


rmWhitespace :: String -> String
rmWhitespace str =
  let
    whitespace = unsafeRegex "\\s+" global
  in
    replace whitespace "" str


wrapRight :: String -> String
wrapRight string =
  "(Right " <> string <> ")"


shouldEqualString :: forall r. String -> String -> Aff r Unit
shouldEqualString v1 v2 =
  v1 <> "\n" <> v2
    # indentSubsequent 2
    # fail
    # when (v1 /= v2)


compareChar :: forall r. String -> String -> Aff r Unit
compareChar actual expected =
  let
    comparisonArray = zipWith
      shouldEqual
      (toCharArray actual)
      (toCharArray expected)
  in do
    fold comparisonArray
    (length actual) `shouldEqual` (length expected)



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
            actual = Amount.showPretty (Amount (fromInt 37) (Commodity "€"))
          actual `shouldEqual` "     37.00 €       "


      describe "Account" do
        it "can add an amount to a commodity map" do
          let
            expectedMap = Map.fromFoldable
              [(Tuple (Commodity "€") (Amount (fromInt 37) (Commodity "€")))]
            emptyMap = Map.empty :: CommodityMap
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
            actualPretty = Account.commodityMapShowPretty commodityMap
          actualPretty `shouldEqual` commodityMapPretty

        it "pretty shows an account" do
          let
            commodityMap = Map.fromFoldable
              [ (Tuple (Commodity "€") (Amount (fromInt 42) (Commodity "€")))
              , (Tuple (Commodity "$") (Amount (fromInt 12) (Commodity "$")))
              ]
            actualPretty = Account.showPretty (Account "test" commodityMap)
          actualPretty `shouldEqual` accountPretty


      describe "Transfer" do
        it "converts a simple JSON string to a Transfer" do
          let
            actual = transferSimpleJson
              # Transfer.fromJson
              # show
              # rmWhitespace
            expected = transferSimpleShowed
              # wrapRight
              # rmWhitespace
          actual `shouldEqual` expected

        it "pretty shows a transfer" do
          let
            actual = Transfer.showPretty transferSimple
          actual `compareChar` transferSimplePretty


      describe "Transaction" do
        it "converts a JSON string to a Transaction" do
          let
            actual = transactionSimpleJson
              # Transaction.fromJson
              # show
              # rmWhitespace
            expected = transactionSimpleShowed
              # wrapRight
              # rmWhitespace
          actual `shouldEqual` expected


        it "converts a YAML string to a Transaction" do
          let
            actual = transactionSimpleYaml
              # Transaction.fromYaml
              # show
              # rmWhitespace
            expected = transactionSimpleShowed
              # wrapRight
              # rmWhitespace
          actual `shouldEqual` expected


        it "pretty shows a transaction" do
          let
              actual = Transaction.showPretty transactionSimple
          actual `shouldEqual` transactionSimplePretty


      describe "Ledger" do
        it "converts a JSON string to a Ledger" do
          let
            actual = ledgerJson
              # Ledger.fromJson
              # show
              # rmWhitespace
            expected = ledgerShowed
              # wrapRight
              # rmWhitespace
          actual `shouldEqual` expected


        it "converts a YAML string to a Ledger" do
          let
            actual = ledgerYaml
              # Ledger.fromYaml
              # show
              # rmWhitespace
            expected = ledgerShowed
              # wrapRight
              # rmWhitespace
          actual `shouldEqual` expected


        it "pretty shows a ledger" do
          (Ledger.showPretty ledger) `shouldEqualString` ledgerPretty


        it "pretty shows the balance of all accounts" do
          (Ledger.showBalance ledger) `shouldEqualString` ledgerBalance


        it "supports multiple transactions on one account" do
          let
            actual = Ledger.showBalance ledgerMultiTrans
          actual `shouldEqualString` ledgerBalanceMultiTrans
