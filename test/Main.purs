module Test.Main where

import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff)
import Data.Array (zipWith)
import Data.Eq ((/=))
import Data.Result (Result(Error, Ok))
import Data.Foldable (fold)
import Data.Function ((#), ($))
import Data.Functor (map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (power)
import Data.Rational (fromInt, (%))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String
  (Pattern(..), Replacement(..), toCharArray, length, replaceAll)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Test.Fixtures
import Test.Spec (describe, it)
-- import Test.Spec as Test
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Transity.Data.Account (Account(..))
import Transity.Data.Account as Account
import Transity.Data.Amount (Amount(..), Commodity(..))
import Transity.Data.Amount (showPretty, showPrettyAligned) as Amount
import Transity.Data.CommodityMap (CommodityMap)
import Transity.Data.CommodityMap as CommodityMap
import Transity.Data.Ledger as Ledger
import Transity.Data.Transaction as Transaction
import Transity.Data.Transfer (fromJson, showPretty) as Transfer
import Transity.Utils (digitsToRational, indentSubsequent, ColorFlag(..))


rmWhitespace :: String -> String
rmWhitespace str =
  let
    whitespace = unsafeRegex "\\s+" global
  in
    replace whitespace "" str


wrapRight :: String -> String
wrapRight string =
  "(Ok " <> string <> ")"


testEqualityTo :: String -> String -> Result String String
testEqualityTo actual expected =
  if (actual /= expected)
  then Error
    $ indentSubsequent 2
    $    "=========== Actual ===========\n"
      <> replaceAll (Pattern "\n") (Replacement "|\n") actual <> "|\n"
      <> "========== Expected ==========\n"
      <> replaceAll (Pattern "\n") (Replacement "|\n") expected <> "|\n"
      <> "=============================="
      <> "\n\n"
  else Ok ""


shouldBeOk :: forall r. Result String String -> Aff r Unit
shouldBeOk value = case value of
  Error error -> fail error
  Ok _ -> (pure unit)


shouldEqualString :: forall r. String -> String -> Aff r Unit
shouldEqualString v1 v2 =
  case v1 `testEqualityTo` v2 of
    Error error -> fail error
    Ok _ -> (pure unit)


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

      let
        digits = "1." <> ("5" `power` 10)

      it ("converts " <> digits <> " to Nothing (Int range overflow)") do
        (digitsToRational digits) `shouldEqual` Nothing


  describe "Transity" do
    describe "Data" do
      describe "Amount" do
        it "pretty shows an amount" do
          let
            actual = Amount.showPretty (Amount (fromInt 37) (Commodity "€"))
          actual `shouldEqual` "37 €"

        it "pretty shows and aligns an amount" do
          let
            actual = Amount.showPrettyAligned ColorNo 8 5 7
              (Amount (37237 % 1000) (Commodity "EUR"))
          actual `shouldEqualString` "      37.237  EUR    "

        it "pretty shows and aligns an amount and hides fractional part" do
          let
            actual = Amount.showPrettyAligned ColorNo 8 5 7
              (Amount (fromInt 37) (Commodity "EUR"))
          actual `shouldEqualString` "      37      EUR    "


      describe "Account" do
        it "can add an amount to a commodity map" do
          let
            expectedMap = Map.fromFoldable
              [(Tuple (Commodity "€") (Amount (fromInt 37) (Commodity "€")))]
            emptyMap = Map.empty :: CommodityMap
            amount = Amount (fromInt 37) (Commodity "€")
            actualMap = emptyMap `CommodityMap.addAmountToMap` amount
          actualMap `shouldEqual` expectedMap

        it "can subtract an amount from a commodity map" do
          let
            expectedMap = Map.fromFoldable
              [(Tuple (Commodity "€") (Amount (fromInt 37) (Commodity "€")))]
            initialMap = Map.fromFoldable
              [(Tuple (Commodity "€") (Amount (fromInt 42) (Commodity "€")))]
            amount = Amount (fromInt 5) (Commodity "€")
            actualMap = initialMap `CommodityMap.subtractAmountFromMap` amount
          actualMap `shouldEqual` expectedMap


        let commodityMap = Map.fromFoldable
              [ (Tuple (Commodity "€") (Amount (fromInt 2) (Commodity "EUR")))
              , (Tuple (Commodity "$") (Amount (fromInt 12) (Commodity "$")))
              ]

        it "pretty shows a commodity map" do
          let actualPretty = CommodityMap.showPretty commodityMap
          actualPretty `shouldEqualString` commodityMapPretty

        it "pretty shows and aligns a commodity map" do
          let actualPretty =
                CommodityMap.showPrettyAligned ColorNo 7 8 9 commodityMap
          actualPretty `shouldEqualString` commodityMapPrettyAligned


        it "pretty shows an account" do
          let actualPretty = Account.showPretty (Account "test" commodityMap)
          actualPretty `shouldEqualString` accountPretty

        it "pretty shows and aligns an account" do
          let
            widthRecord =
              { account: 6
              , integer: 7
              , fraction: 8
              , commodity: 9
              }
            actualPretty = Account.showPrettyAligned
              ColorNo
              widthRecord
              (Account "test" commodityMap)
          actualPretty `shouldEqualString` accountPrettyAligned


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
          actual `shouldEqualString` transferSimplePretty


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
          actual `shouldEqualString` transactionSimplePretty


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
          actual `shouldEqualString` expected


        it "converts a YAML string to a Ledger" do
          let
            actual = ledgerYaml
              # Ledger.fromYaml
              # show
              # rmWhitespace
            expected = ledgerShowed
              # wrapRight
              # rmWhitespace
          actual `shouldEqualString` expected


        it "fails if a transfer contains an empty field" do
          expectError
            (shouldBeOk do
              actual <- transactionNoAccount
                # Ledger.fromYaml
                # map (Ledger.showBalance ColorNo)
              expected <- Ok transactionNoAccountPretty
              actual `testEqualityTo` expected
            )


        it "pretty shows a ledger" do
          (Ledger.showPretty ledger) `shouldEqualString` ledgerPretty


        it "pretty shows the balance of all accounts" do
          (Ledger.showBalance ColorNo ledger) `shouldEqualString` ledgerBalance


        it "supports multiple transactions on one account" do
          let
            actual = Ledger.showBalance ColorNo ledgerMultiTrans
          actual `shouldEqualString` ledgerBalanceMultiTrans

        it "serializes to HLedger format" do
          (Ledger.entriesToLedger ledger) `shouldEqualString` ledgerLedger
