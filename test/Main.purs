module Test.Main where

import Test.Fixtures

import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (find, zipWith)
import Data.Array as Array
import Data.Eq ((/=))
import Data.Foldable (fold)
import Data.Function ((#), ($))
import Data.Functor (map)
import Data.Map (fromFoldable)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Monoid (power)
import Data.Newtype (modify, over, unwrap, wrap)
import Data.Rational (Rational, (%))
import Data.Result (Result(Error, Ok), fromEither, isError, isOk)
import Data.Ring (negate)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String (Pattern(..), Replacement(..), length, replaceAll)
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import JS.BigInt (fromString) as BigInt
import Main (buildLedgerAndRun)
import Oclis.Types (CliArgPrim(TextArg))
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, (==))
import Test.Fixtures as Fixtures
import Test.Spec (describe, it)
import Test.Spec.Assertions (expectError, fail, shouldEqual, shouldSatisfy)
import Test.Spec.Assertions.String (shouldContain)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Transity.Data.Account (Account(..))
import Transity.Data.Account (Account(..), addAmount, subtractAmount) as AccountModule
import Transity.Data.Account as Account
import Transity.Data.Amount (Amount(..), Commodity(..))
import Transity.Data.Amount
  ( isZero
  , negate
  , parseAmount
  , showPretty
  , showPrettyAligned
  , subtract
  , toWidthRecord
  ) as Amount
import Transity.Data.Balance (Balance(..))
import Transity.Data.CommodityMap
  ( CommodityMap
  , fromAmounts
  , isCommodityMapZero
  )
import Transity.Data.CommodityMap as CommodityMap
import Transity.Data.Config (ColorFlag(..))
import Transity.Data.Entity (Entity(..))
import Transity.Data.Entity as Entity
import Transity.Data.Ledger (BalanceFilter(..), Ledger(..), getEntries)
import Transity.Data.Ledger as Ledger
import Transity.Data.Transaction (Transaction(..))
import Transity.Data.Transaction as Transaction
import Transity.Data.Transfer (Transfer(..), negateTransfer)
import Transity.Data.Transfer as Transfer
import Transity.Utils
  ( SortOrder(..)
  , alignNumber
  , capitalize
  , dateShowPretty
  , dateShowPrettyLong
  , digitsToRational
  , indentSubsequent
  , lengthOfNumParts
  , mergeWidthRecords
  , padEnd
  , padStart
  , ratioZero
  , stringToDateTime
  , stringifyJsonDecodeError
  , utcToIsoDateString
  , utcToIsoString
  , widthRecordZero
  )
import Transity.Xlsx (FileEntry(..), entriesAsXlsx, writeToZip)

rmWhitespace :: String -> String
rmWhitespace str =
  let
    whitespace = unsafeRegex "\\s+" global
  in
    replace whitespace "" str

wrapWithOk :: String -> String
wrapWithOk string =
  "(Ok " <> string <> ")"

testEqualityTo :: String -> String -> Result String String
testEqualityTo actual expected =
  if (actual /= expected)
  then Error
    $ indentSubsequent 2
    $ "=========== Actual ===========\n"
        <> replaceAll (Pattern "\n") (Replacement "|\n") actual
        <> "|\n"
        <> "========== Expected ==========\n"
        <> replaceAll (Pattern "\n") (Replacement "|\n") expected
        <> "|\n"
        <> "=============================="
        <> "\n\n"
  else Ok ""

shouldBeOk :: Result String String -> Aff Unit
shouldBeOk value = case value of
  Error error -> fail error
  Ok _ -> (pure unit)

shouldEqualString :: String -> String -> Aff Unit
shouldEqualString v1 v2 =
  case v1 `testEqualityTo` v2 of
    Error error -> fail error
    Ok _ -> (pure unit)

compareChar :: String -> String -> Aff Unit
compareChar actual expected =
  let
    comparisonArray = zipWith
      shouldEqual
      (toCharArray actual)
      (toCharArray expected)
  in
    do
      fold comparisonArray
      (length actual) `shouldEqual` (length expected)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
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
        (digitsToRational "3.21") `shouldEqual`
          (Just (321 % 100))

      it "converts 12.3456 to 123456/10000" do
        (digitsToRational "12.3456") `shouldEqual`
          (Just (123456 % 10000))

      it "converts -0.3 to -3/10" do
        (digitsToRational "-0.3") `shouldEqual`
          (Just (-3 % 10))

      it "converts abc to Nothing" do
        (digitsToRational "abc") `shouldEqual` Nothing

      let
        digits = "1." <> ("5" `power` 10)

      it ("converts " <> digits <> " to valid Rational") do
        let
          bigIntRatio :: Maybe Rational
          bigIntRatio = do
            a <- BigInt.fromString "3111111111"
            b <- BigInt.fromString "2000000000"
            pure (a % b)

        -- This would fail with a range overflow
        -- if Ints instead of BigInt were used
        (Just $ digitsToRational digits) `shouldEqual` (Just bigIntRatio)

  describe "Xlsx" do
    it "writes files to a ZIP archive" do
      writeToZip
        (Just "test/_deletable_.zip")
        [ FileEntry
            { path: "path/to/file"
            , content: "Some file content"
            }
        ]
      pure unit

    it "creates an XLSX file" do
      let files = entriesAsXlsx ledger
      writeToZip
        (Just "test/_deletable_.xlsx")
        files
      pure unit

    it "HTML escapes content in XML files" do
      let
        files = entriesAsXlsx ledger
        sheetMb = find
          (\(FileEntry file) -> file.path == "xl/worksheets/sheet1.xml")
          files

      case sheetMb of
        Nothing -> fail "Does not contain a sheet"
        Just (FileEntry sheet) ->
          sheet.content `shouldContain` "special chars like &lt; and &amp;"

  describe "Transity" do
    describe "Data" do
      describe "Amount" do
        it "pretty shows an amount" do
          let
            actual = Amount.showPretty
              (Amount (37 % 1) (Commodity "€"))
          actual `shouldEqual` "37 €"

        it "pretty shows and aligns an amount" do
          let
            actual = Amount.showPrettyAligned ColorNo 8 5 7
              (Amount (37237 % 1000) (Commodity "EUR"))
          actual `shouldEqualString` "      37.237  EUR    "

        it "pretty shows and aligns an amount and hides fractional part" do
          let
            actual = Amount.showPrettyAligned ColorNo 8 5 7
              (Amount (37 % 1) (Commodity "EUR"))
          actual `shouldEqualString` "      37      EUR    "

      describe "Account" do
        it "converts a JSON string to an Account" do
          let
            (result :: Result String Account) = do
              jsonObj <- fromEither $ jsonParser accountJson
              stringifyJsonDecodeError $ fromEither $ decodeJson jsonObj
            actual = result
              # show
              # rmWhitespace
            expected = accountShowed
              # wrapWithOk
              # rmWhitespace
          actual `shouldEqualString` expected

        it "encodes an Account as JSON" do
          stringify (encodeJson Account.zero)
            `shouldEqualString`
              rmWhitespace
                """
              {
                "id": "",
                "commodityMap": [],
                "balances": null
              }
            """

        it "can add an amount to a commodity map" do
          let
            expectedMap = Map.fromFoldable
              [ ( Tuple
                    (Commodity "€")
                    (Amount (37 % 1) (Commodity "€"))
                )
              ]
            emptyMap = Map.empty :: CommodityMap
            amount = Amount (37 % 1) (Commodity "€")
            actualMap = emptyMap `CommodityMap.addAmountToMap` amount
          actualMap `shouldEqual` expectedMap

        it "can subtract an amount from a commodity map" do
          let
            expectedMap = Map.fromFoldable
              [ ( Tuple
                    (Commodity "€")
                    (Amount (37 % 1) (Commodity "€"))
                )
              ]
            initialMap = Map.fromFoldable
              [ ( Tuple
                    (Commodity "€")
                    (Amount (42 % 1) (Commodity "€"))
                )
              ]
            amount = Amount (5 % 1) (Commodity "€")
            actualMap = initialMap `CommodityMap.subtractAmountFromMap` amount
          actualMap `shouldEqual` expectedMap

        it "can check if a commodity map as only zero of each commodity" do
          let
            commodityMapZero = Map.fromFoldable
              [ (Tuple (Commodity "€") (Amount ratioZero (Commodity "€")))
              , (Tuple (Commodity "$") (Amount ratioZero (Commodity "$")))
              ]
          (isCommodityMapZero commodityMapZero) `shouldEqual` true

        let
          commodityMap = Map.fromFoldable
            [ ( Tuple
                  (Commodity "EUR")
                  (Amount (2 % 1) (Commodity "EUR"))
              )
            , ( Tuple
                  (Commodity "$")
                  (Amount (12 % 1) (Commodity "$"))
              )
            ]

        it "pretty shows a commodity map" do
          let actualPretty = CommodityMap.showPretty commodityMap
          actualPretty `shouldEqualString` commodityMapPretty

        it "pretty shows and aligns a commodity map" do
          let
            actualPretty =
              CommodityMap.showPrettyAligned ColorNo 7 8 9 commodityMap
          actualPretty `shouldEqualString` commodityMapPrettyAligned

        it "pretty shows an account" do
          let actualPretty = Account.showPretty "test" commodityMap
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
              "test"
              commodityMap
          actualPretty `shouldEqualString` accountPrettyAligned

      describe "Transfer" do
        it "converts a simple JSON string to a Transfer" do
          let
            actual = transferSimpleJson
              # Transfer.fromJson
              # show
              # rmWhitespace
            expected = transferSimpleShowed
              # wrapWithOk
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
              # wrapWithOk
              # rmWhitespace
          actual `shouldEqualString` expected

        it "converts a YAML string to a Transaction" do
          let
            actual = transactionSimpleYaml
              # Transaction.fromYaml
              # show
              # rmWhitespace
            expected = transactionSimpleShowed
              # wrapWithOk
              # rmWhitespace
          actual `shouldEqualString` expected

        it "pretty shows a transaction" do
          let
            actual = Transaction.showPretty transactionSimple
          actual `shouldEqualString` transactionSimplePretty

      describe "Balance" do
        it "converts a JSON string to a Balance" do
          let
            result = do
              jsonObj <- fromEither $ jsonParser balanceJson
              stringifyJsonDecodeError $ fromEither $ decodeJson jsonObj

            actual = (result :: Result String Balance)
              # show
              # rmWhitespace
            expected = balanceShowed
              # wrapWithOk
              # rmWhitespace
          actual `shouldEqualString` expected

      describe "Entity" do
        it "converts a JSON string to an Entity" do
          let
            actual = entityJson
              # Entity.fromJson
              # show
              # rmWhitespace
            expected = entityShowed
              # wrapWithOk
              # rmWhitespace
          actual `shouldEqualString` expected

        it "converts an Entity to an array of accounts with long id" do
          let
            accountWithId = over Account.Account
              (_ { id = "_default_" })
              Account.zero
            entity = over Entity.Entity
              (_ { accounts = Just [ account, accountWithId ], id = "John" })
              Entity.zero
            accounts = Entity.toAccountsWithId entity

          (show accounts) `shouldEqualString`
            ( show
                [ ( Account
                      { balances:
                          ( Just
                              [ ( Balance
                                    ( unsafePartial $ fromJust
                                        $ stringToDateTime "2017-04-02 20:11:45"
                                    )
                                    ( Map.fromFoldable
                                        [ ( Tuple
                                              (Commodity "€")
                                              (Amount (100 % 1) (Commodity "€"))
                                          )
                                        ]
                                    )
                                )
                              ]
                          )
                      , commodityMap:
                          ( Map.fromFoldable
                              [ ( Tuple
                                    (Commodity "€")
                                    (Amount (100 % 1) (Commodity "€"))
                                )
                              ]
                          )
                      , id: "John:wallet"
                      }
                  )
                , ( Account
                      { balances: Nothing
                      , commodityMap: (Map.fromFoldable [])
                      , id: "John:_default_"
                      }
                  )
                ]
            )

        it "converts an Entitie's balances to an array of transfers" do
          let
            entity = over Entity.Entity
              (_ { accounts = Just [ account ], id = "John" })
              Entity.zero
            transfers = Entity.toTransfers entity

          (rmWhitespace $ show transfers) `shouldEqualString`
            ( rmWhitespace
                """
              [(Transfer
                  { amount: (Amount 100 % 1 (Commodity "€"))
                  , from: "John:wallet"
                  , note: Nothing
                  , to: "_void_"
                  , utc: (Just (DateTime
                      (Date (Year 2017) April (Day 2))
                      (Time (Hour 20) (Minute 11) (Second 45) (Millisecond 0))))
                  })
              ]
            """
            )

      describe "Ledger" do
        it "converts a JSON string to a Ledger" do
          let
            actual = ledgerJson
              # Ledger.fromJson
              # show
              # rmWhitespace
            expected = ledgerShowed
              # wrapWithOk
              # rmWhitespace
          actual `shouldEqualString` expected

        it "converts a YAML string to a Ledger" do
          let
            actual = ledgerYaml
              # Ledger.fromYaml
              # show
              # rmWhitespace
            expected = ledgerShowed
              # wrapWithOk
              # rmWhitespace
          actual `shouldEqualString` expected

        it "checks if balance map is zero" do
          (Ledger.isBalanceMapZero balanceMap) `shouldEqual` false

        it "checks if amount in balance map is zero" do
          let
            balMap = fromFoldable
              [ Tuple "john" $ fromFoldable
                  [ ( Tuple
                        (Commodity "€")
                        (Amount (100 % 1) (Commodity "€"))
                    )
                  , ( Tuple
                        (Commodity "$")
                        (Amount ratioZero (Commodity "$"))
                    )
                  ]
              ]
            isZeroUSD = Ledger.isAmountInMapZero balMap "john" (Commodity "$")
            isZeroEUR = Ledger.isAmountInMapZero balMap "john" (Commodity "€")

          isZeroUSD `shouldEqual` true
          isZeroEUR `shouldEqual` false

        it "adds a transfer to a balance map" do
          let
            emptyMap = fromFoldable [ Tuple "john" Map.empty ]
            transfer = Transfer
              { utc: stringToDateTime "2014-12-24"
              , from: "john:wallet"
              , to: "anna"
              , amount: Amount (15 % 1) (Commodity "€")
              , note: Just "A note with special chars like < and &"
              }
            expected = fromFoldable
              [ Tuple "anna:_default_" $ fromFoldable
                  [ ( Tuple
                        (Commodity "€")
                        (Amount (15 % 1) (Commodity "€"))
                    )
                  ]
              , Tuple "john" $ fromFoldable []
              , Tuple "john:wallet" $ fromFoldable
                  [ ( Tuple
                        (Commodity "€")
                        (Amount (-15 % 1) (Commodity "€"))
                    )
                  ]
              ]
            actual = emptyMap `Ledger.addTransfer` transfer

          (show actual) `shouldEqualString` (show expected)

        describe "Verification" do
          it "ledger without verification balances is valid" do
            let verification = Ledger.verifyLedgerBalances ledger

            verification `shouldSatisfy` isOk

          it "fails if verification balances are incorrect" do
            let
              ledgerValid = Ledger.fromYaml
                """
                  owner: John Doe
                  entities:
                    - id: anna
                      accounts:
                        - id: wallet
                          balances:
                            - utc: '2000-01-01 12:00'
                              amounts: []
                            - utc: '2010-01-01 12:00'
                              amounts: [5 €]
                    - id: ben
                      accounts: [id: wallet]
                  transactions:
                    - utc: '2005-01-01 12:00'
                      transfers:
                        - from: ben:wallet
                          to: anna:wallet
                          amount: 3 €
                """
              verification = ledgerValid >>= Ledger.verifyLedgerBalances

            (isError verification) `shouldEqual` true

          it "fails if verification balance has too many entries" do
            let
              ledgerValid = Ledger.fromYaml
                """
                  owner: John Doe
                  entities:
                    - id: anna
                      accounts:
                        - id: wallet
                          balances:
                            - utc: '2000-01-01 12:00'
                              amounts: [0 €]
                            - utc: '2010-01-01 12:00'
                              amounts: [3 €, 5 $, 5 BTC]
                    - id: ben
                      accounts: [id: wallet]
                  transactions:
                    - utc: '2005-01-01 12:00'
                      transfers:
                        - from: ben:wallet
                          to: anna:wallet
                          amount: 3 €
                        - from: ben:wallet
                          to: anna:wallet
                          amount: 5 $
                """
              verification = ledgerValid >>= Ledger.verifyLedgerBalances

            (show verification) `shouldContain` "off by 5 BTC"

          it "passes if verification balance is correct" do
            let
              ledgerValid = Ledger.fromYaml
                """
                  owner: John Doe
                  entities:
                    - id: anna
                      accounts:
                        - id: wallet
                          balances:
                            - utc: '2000-01-01 12:00'
                              amounts: []
                            - utc: '2010-01-01 12:00'
                              amounts: [3 €]
                    - id: ben
                      accounts: [id: wallet]
                  transactions:
                    - utc: '2005-01-01 12:00'
                      transfers:
                        - from: ben:wallet
                          to: anna:wallet
                          amount: 3 €
                """
              verification = ledgerValid >>= Ledger.verifyLedgerBalances
              expected = Ledger
                { entities:
                    ( Just
                        [ ( Entity
                              { accounts:
                                  ( Just
                                      [ ( Account
                                            { balances:
                                                ( Just
                                                    [ ( Balance
                                                          ( unsafePartial
                                                              $ fromJust
                                                              $ stringToDateTime
                                                                  "2000-01-01 12:00"
                                                          )
                                                          (fromFoldable [])
                                                      )
                                                    , ( Balance
                                                          ( unsafePartial
                                                              $ fromJust
                                                              $ stringToDateTime
                                                                  "2010-01-01 12:00"
                                                          )
                                                          ( fromFoldable
                                                              [ ( Tuple
                                                                    ( Commodity
                                                                        "€"
                                                                    )
                                                                    ( Amount
                                                                        (3 % 1)
                                                                        ( Commodity
                                                                            "€"
                                                                        )
                                                                    )
                                                                )
                                                              ]
                                                          )
                                                      )
                                                    ]
                                                )
                                            , commodityMap: (fromFoldable [])
                                            , id: "wallet"
                                            }
                                        )
                                      ]
                                  )
                              , id: "anna"
                              , name: Nothing
                              , note: Nothing
                              , tags: Nothing
                              , utc: Nothing
                              }
                          )
                        , ( Entity
                              { accounts:
                                  ( Just
                                      [ ( Account
                                            { balances: Nothing
                                            , commodityMap: (fromFoldable [])
                                            , id: "wallet"
                                            }
                                        )
                                      ]
                                  )
                              , id: "ben"
                              , name: Nothing
                              , note: Nothing
                              , tags: Nothing
                              , utc: Nothing
                              }
                          )
                        ]
                    )
                , owner: Just "John Doe"
                , transactions:
                    [ ( Transaction
                          { id: Nothing
                          , note: Nothing
                          , files: []
                          , transfers:
                              [ ( Transfer
                                    { amount:
                                        ( Amount
                                            (3 % 1)
                                            (Commodity "€")
                                        )
                                    , from: "ben:wallet"
                                    , note: Nothing
                                    , to: "anna:wallet"
                                    , utc: Nothing
                                    }
                                )
                              ]
                          , utc:
                              ( Just
                                  ( unsafePartial $ fromJust
                                      $ stringToDateTime "2005-01-01 12:00"
                                  )
                              )
                          }
                      )
                    ]
                }

            (show verification) `shouldEqualString`
              (show (Ok expected :: Result String Ledger.Ledger))

          it "passes if verification balances are correct" do
            let
              ledgerValid = Ledger.fromYaml
                """
                  owner: John Doe
                  entities:
                    - id: anna
                      accounts:
                        - id: wallet
                          balances:
                            - utc: '2000-01-01 12:00'
                              amounts: []
                            - utc: '2010-01-01 12:00'
                              amounts: [3 €, 3 $]
                    - id: ben
                      accounts: [id: wallet]
                  transactions:
                    - utc: '2005-01-01 12:00'
                      transfers:
                        - from: ben:wallet
                          to: anna:wallet
                          amount: 3 €
                        - from: ben:wallet
                          to: anna:wallet
                          amount: 3 $
                """
              verification = ledgerValid >>= Ledger.verifyLedgerBalances

            (isOk verification) `shouldEqual` true

          it "passes if verification balances at different UTCs are correct" do
            let
              ledgerValid = Ledger.fromYaml
                """
                  owner: John Doe
                  entities:
                    - id: anna
                      accounts:
                        - id: wallet
                          balances:
                            - utc: '2000-01-01 12:00'
                              amounts: []
                            - utc: '2006-01-01 12:00'
                              amounts: [3 €]
                            - utc: '2010-01-01 12:00'
                              amounts: [3 €, 4 $]
                    - id: ben
                      accounts: [id: wallet]
                  transactions:
                    - utc: '2005-01-01 12:00'
                      transfers:
                        - from: ben:wallet
                          to: anna:wallet
                          amount: 3 €
                    - utc: '2007-01-01 12:00'
                      transfers:
                        - from: ben:wallet
                          to: anna:wallet
                          amount: 4 $
                """
              verification = ledgerValid >>= Ledger.verifyLedgerBalances

            (isOk verification) `shouldEqual` true

          it "verifies balances for combined journals" do
            execResult <- liftEffect $ buildLedgerAndRun
              "."
              "test/fixtures/journal1.yaml"
              [ TextArg "test/fixtures/journal2.yaml" ]
              (\_ledger -> pure $ Ok unit)

            (isError execResult) `shouldEqual` true

        it "subtracts a transfer from a balance map" do
          let
            result = balanceMap `Ledger.subtractTransfer` transferSimple
            expected = Map.fromFoldable
              [ ( Tuple "evil-corp:_default_"
                    ( Map.fromFoldable
                        [ ( Tuple
                              (Commodity "€")
                              (Amount (-15 % 1) (Commodity "€"))
                          )
                        ]
                    )
                )
              , ( Tuple "john"
                    ( Map.fromFoldable
                        [ ( Tuple
                              (Commodity "€")
                              (Amount (100 % 1) (Commodity "€"))
                          )
                        ]
                    )
                )
              , ( Tuple "john:giro"
                    ( Map.fromFoldable
                        [ ( Tuple
                              (Commodity "€")
                              (Amount (15 % 1) (Commodity "€"))
                          )
                        ]
                    )
                )
              ]

          (show result) `shouldEqualString` (show expected)

        it "fails if a transfer contains an empty field" do
          expectError
            ( shouldBeOk do
                actual <- transactionNoAccount
                  # Ledger.fromYaml
                  # map (Ledger.showBalance BalanceAll ColorNo)
                expected <- Ok transactionNoAccountPretty
                actual `testEqualityTo` expected
            )

        it "pretty shows a ledger" do
          (Ledger.showPretty ledger) `shouldEqualString` ledgerPretty

        it "pretty shows all accounts" do
          (ledgerEntities # Ledger.showEntities CustomSort # rmWhitespace)
            `shouldEqualString`
              (rmWhitespace ledgerEntitiesShowed)

        it "pretty shows the balance of owner" do
          (Ledger.showBalance (BalanceOnly "john") ColorNo ledger)
            `shouldEqualString` ledgerBalanceOwner

        it "pretty shows the balance of all accounts" do
          (Ledger.showBalance BalanceAll ColorNo ledger)
            `shouldEqualString` ledgerBalanceAll

        it "supports multiple transactions on one account" do
          let
            actual = Ledger.showBalance BalanceAll ColorNo ledgerMultiTrans

          actual `shouldEqualString` ledgerBalanceMultiTrans

        it "serializes to HLedger format" do
          (Ledger.entriesToLedger ledger) `shouldEqualString` ledgerLedger

        it "keeps first owner when combining several ledgers" do
          let
            (Ledger combined) =
              Fixtures.ledger
                <> (Fixtures.ledger2 # modify (_ { owner = Nothing }))

          combined.owner `shouldEqual` (Just "John Doe")

        it "serializes to entries format" do
          let actual = Ledger.showEntries "," ledger
          actual `shouldSatisfy` (\x -> x /= Nothing)

        it "serializes to entries-by-account format" do
          let actual = Ledger.showEntriesByAccount ledger
          actual `shouldSatisfy` (\x -> x /= Nothing)

        it "pretty shows transfers" do
          let actual = Ledger.showTransfers ColorNo ledger
          actual `shouldContain` "john:giro"

        it "shows balance filtered to owner only" do
          (Ledger.showBalance BalanceOnlyOwner ColorNo ledger)
            `shouldEqualString` ""

        it "verifies accounts — passes when all accounts are declared" do
          let
            result =
              Ledger.fromYaml
                """
                owner: John Doe
                entities:
                  - id: anna
                    accounts:
                      - id: wallet
                  - id: ben
                    accounts: [id: wallet]
                transactions:
                  - utc: '2005-01-01 12:00'
                    transfers:
                      - from: ben:wallet
                        to: anna:wallet
                        amount: 3 €
              """
                >>= Ledger.verifyAccounts

          (isOk result) `shouldEqual` true

        it "verifies accounts — fails when an account is undeclared" do
          let
            result =
              Ledger.fromYaml
                """
                owner: John Doe
                entities:
                  - id: anna
                    accounts:
                      - id: wallet
                transactions:
                  - utc: '2005-01-01 12:00'
                    transfers:
                      - from: undeclared:wallet
                        to: anna:wallet
                        amount: 3 €
              """
                >>= Ledger.verifyAccounts

          (isError result) `shouldEqual` true

        it "shows entities sorted alphabetically" do
          let
            sortedLedger = Ledger
              { owner: Just "John Doe"
              , entities: Just
                  [ wrap $ (unwrap Entity.zero) { id = "Zara" }
                  , wrap $ (unwrap Entity.zero) { id = "Anna" }
                  , wrap $ (unwrap Entity.zero) { id = "mike" }
                  ]
              , transactions: []
              }
          let output = Ledger.showEntities Alphabetically sortedLedger
          output `shouldContain` "Anna"

        it "returns message when ledger has no entities" do
          let output = Ledger.showEntities CustomSort ledger
          output `shouldEqual` "Journal does not contain any entities"

    describe "Amount" do
      it "parses a valid amount string" do
        (Amount.parseAmount "15 €")
          `shouldEqual` (Ok (Amount (15 % 1) (Commodity "€")))

      it "fails to parse an amount with missing commodity" do
        (Amount.parseAmount "15")
          `shouldSatisfy` isError

      it "negates an amount" do
        (Amount.negate (Amount (15 % 1) (Commodity "€")))
          `shouldEqual` (Amount (-15 % 1) (Commodity "€"))

      it "subtracts two amounts of the same commodity" do
        ( Amount.subtract
            (Amount (10 % 1) (Commodity "€"))
            (Amount (3 % 1) (Commodity "€"))
        ) `shouldEqual` (Amount (7 % 1) (Commodity "€"))

      it "isZero returns true for zero amount" do
        (Amount.isZero (Amount ratioZero (Commodity "€")))
          `shouldEqual` true

      it "isZero returns false for non-zero amount" do
        (Amount.isZero (Amount (1 % 1) (Commodity "€")))
          `shouldEqual` false

      it "toWidthRecord computes correct widths" do
        let wr = Amount.toWidthRecord (Amount (1234 % 10) (Commodity "EUR"))
        wr.integer `shouldEqual` 3
        wr.fraction `shouldEqual` 2
        wr.commodity `shouldEqual` 3

    describe "CommodityMap" do
      it "builds a commodity map from an array of amounts" do
        let
          amounts =
            [ Amount (10 % 1) (Commodity "€")
            , Amount (5 % 1) (Commodity "€")
            , Amount (3 % 1) (Commodity "$")
            ]
          result = fromAmounts amounts
          euroAmount = Map.lookup (Commodity "€") result
          usdAmount = Map.lookup (Commodity "$") result
        euroAmount `shouldEqual` Just (Amount (15 % 1) (Commodity "€"))
        usdAmount `shouldEqual` Just (Amount (3 % 1) (Commodity "$"))

      it "toWidthRecord computes correct widths across commodities" do
        let
          cm = Map.fromFoldable
            [ Tuple (Commodity "€") (Amount (1234 % 10) (Commodity "€"))
            , Tuple (Commodity "USD") (Amount (5 % 1) (Commodity "USD"))
            ]
          wr = CommodityMap.toWidthRecord cm
        wr.commodity `shouldEqual` 3

    describe "Transfer" do
      it "parses a transfer from YAML" do
        let
          actual = transferSimpleYaml
            # Transfer.fromYaml
            # show
            # rmWhitespace
          expected = transferSimpleShowed
            # wrapWithOk
            # rmWhitespace
        actual `shouldEqualString` expected

      it "negates a transfer" do
        let negated = negateTransfer transferSimple
        (show negated) `shouldContain` "-15"

      it "fails if 'from' is empty" do
        let
          result = Transfer.fromJson
            """{"from":"","to":"anna","amount":"5 €"}"""
        (isError result) `shouldEqual` true

      it "fails if 'to' is empty" do
        let
          result = Transfer.fromJson
            """{"from":"john","to":"","amount":"5 €"}"""
        (isError result) `shouldEqual` true

      it "fails if amount is zero" do
        let
          result = Transfer.fromJson
            """{"from":"john","to":"anna","amount":"0 €"}"""
        (isError result) `shouldEqual` true

    describe "Utils" do
      it "capitalizes a string" do
        (capitalize "hello") `shouldEqual` "Hello"

      it "capitalize on empty string returns empty" do
        (capitalize "") `shouldEqual` ""

      it "indents subsequent lines" do
        (indentSubsequent 2 "line1\nline2\nline3")
          `shouldEqual` "line1\n  line2\n  line3"

      it "padStart pads a string on the left" do
        (padStart 5 "ab") `shouldEqual` "   ab"

      it "padEnd pads a string on the right" do
        (padEnd 5 "ab") `shouldEqual` "ab   "

      it "lengthOfNumParts for integer" do
        let Tuple i f = lengthOfNumParts 42.0
        i `shouldEqual` 2
        f `shouldEqual` 0

      it "lengthOfNumParts for decimal" do
        let Tuple i f = lengthOfNumParts 3.14
        i `shouldEqual` 1
        f `shouldEqual` 3

      it "mergeWidthRecords takes max of each field" do
        let
          a = widthRecordZero { account = 5, integer = 3 }
          b = widthRecordZero { account = 2, integer = 7 }
          m = mergeWidthRecords a b
        m.account `shouldEqual` 5
        m.integer `shouldEqual` 7

      it "utcToIsoString formats a datetime" do
        let
          dt = unsafePartial $ fromJust $ stringToDateTime "2014-12-24 10:30:45"
        (utcToIsoString dt) `shouldEqual` "2014-12-24T10:30:45"

      it "utcToIsoDateString formats a date only" do
        let
          dt = unsafePartial $ fromJust $ stringToDateTime "2014-12-24 10:30:45"
        (utcToIsoDateString dt) `shouldEqual` "2014-12-24"

      it "dateShowPretty formats a datetime without seconds" do
        let
          dt = unsafePartial $ fromJust $ stringToDateTime "2014-12-24 10:30:00"
        (dateShowPretty dt) `shouldEqual` "2014-12-24 10:30"

      it "dateShowPrettyLong formats a datetime with seconds" do
        let
          dt = unsafePartial $ fromJust $ stringToDateTime "2014-12-24 10:30:45"
        (dateShowPrettyLong dt) `shouldEqual` "2014-12-24 10:30:45"

    describe "Account" do
      it "adds an amount to an account" do
        let
          acc = AccountModule.addAmount Account.zero
            (Amount (10 % 1) (Commodity "€"))
          (Account { commodityMap }) = acc
          result = Map.lookup (Commodity "€") commodityMap
        result `shouldEqual` Just (Amount (10 % 1) (Commodity "€"))

      it "subtracts an amount from an account" do
        let
          acc0 = AccountModule.addAmount Account.zero
            (Amount (10 % 1) (Commodity "€"))
          acc1 = AccountModule.subtractAmount acc0
            (Amount (3 % 1) (Commodity "€"))
          (Account { commodityMap }) = acc1
          result = Map.lookup (Commodity "€") commodityMap
        result `shouldEqual` Just (Amount (7 % 1) (Commodity "€"))

      it "toWidthRecord accounts for id length" do
        let
          cm = Map.fromFoldable
            [ Tuple (Commodity "€") (Amount (5 % 1) (Commodity "€")) ]
          wr = Account.toWidthRecord "long-account-name" cm
        wr.account `shouldEqual` 17

    describe "Amount" do
      it "appends two amounts of the same commodity" do
        let
          result = Amount (3 % 1) (Commodity "€") <> Amount (4 % 1)
            (Commodity "€")
        result `shouldEqual` Amount (7 % 1) (Commodity "€")

      it "append with mismatched commodities yields INVALID COMPUTATION" do
        let
          result = Amount (3 % 1) (Commodity "€") <> Amount (4 % 1)
            (Commodity "$")
        result `shouldEqual` Amount ratioZero (Commodity "INVALID COMPUTATION")

      it "subtract with mismatched commodities yields INVALID COMPUTATION" do
        let
          result = Amount.subtract (Amount (3 % 1) (Commodity "€"))
            (Amount (4 % 1) (Commodity "$"))
        result `shouldEqual` Amount ratioZero (Commodity "INVALID COMPUTATION")

    describe "Transaction" do
      it "toTransfers promotes transaction UTC to transfers without one" do
        let
          transfers = Transaction.toTransfers [ transactionSimple ]
          Transfer { utc } = unsafePartial $ fromJust $ find
            (\(Transfer t) -> t.from == "john:giro")
            transfers
        utc `shouldEqual` stringToDateTime "2014-12-24"

      it "showTransfersWithDate includes account names" do
        let output = Transaction.showTransfersWithDate ColorNo transactionSimple
        output `shouldContain` "john:giro"

    describe "Transfer" do
      it "showPrettyColorized includes account names" do
        let output = Transfer.showPrettyColorized transferSimple
        output `shouldContain` "john:giro"

    describe "Entity" do
      it "fromJson parses an entity" do
        let result = Entity.fromJson entityJson
        (isOk result) `shouldEqual` true

      it "showPretty includes entity id" do
        let
          entity = Entity
            { id: "john"
            , name: Just "John Doe"
            , note: Just "A note"
            , utc: Nothing
            , tags: Just [ "person" ]
            , accounts: Nothing
            }
        (Entity.showPretty entity) `shouldContain` "john"

    describe "Ledger" do
      it "showBalance BalanceOnly with non-matching filter returns empty" do
        (Ledger.showBalance (BalanceOnly "nonexistent") ColorNo ledger)
          `shouldEqual` ""

      it "getEntries returns Nothing when transfers lack UTC" do
        let
          noUtcLedger = Ledger
            { owner: Just "Test"
            , entities: Nothing
            , transactions:
                [ Transaction
                    { id: Nothing
                    , utc: Nothing
                    , note: Nothing
                    , files: []
                    , transfers: [ transferMinimal ]
                    }
                ]
            }
        (Ledger.getEntries noUtcLedger) `shouldEqual` Nothing

      it "getEntries includes initial entity balance entries" do
        let
          entityLedger = Ledger
            { owner: Just "John Doe"
            , entities: Just
                [ Entity.Entity
                    { id: "anna"
                    , name: Nothing
                    , note: Nothing
                    , utc: Nothing
                    , tags: Nothing
                    , accounts: Just
                        [ Account
                            { id: "wallet"
                            , commodityMap: Map.empty
                            , balances: Just
                                [ Balance
                                    ( unsafePartial $ fromJust
                                        $ stringToDateTime "2020-01-01"
                                    )
                                    ( Map.fromFoldable
                                        [ Tuple (Commodity "€")
                                            (Amount (100 % 1) (Commodity "€"))
                                        ]
                                    )
                                ]
                            }
                        ]
                    }
                ]
            , transactions: []
            }
        let result = Ledger.getEntries entityLedger
        result `shouldSatisfy` (\x -> x /= Nothing)

      it "showEntries content contains account and commodity" do
        let result = Ledger.showEntries "," ledger
        case result of
          Nothing -> fail "Expected entries"
          Just s -> do
            s `shouldContain` "john:giro"
            s `shouldContain` "€"

      it "showEntriesByAccount groups by account and commodity" do
        let result = Ledger.showEntriesByAccount ledger
        case result of
          Nothing -> fail "Expected entries"
          Just s -> do
            s `shouldContain` "john:giro"
            s `shouldContain` "evil-corp"

      it "combines transactions from two ledgers" do
        let
          Ledger combined = Fixtures.ledger <> Fixtures.ledger2
        (Array.length combined.transactions) `shouldEqual` 2

    describe "Transfer" do
      it "showPretty with no UTC date uses spaces instead" do
        let output = Transfer.showPretty transferMinimal
        -- Without a UTC, the date area is blank spaces
        output `shouldContain` "john:giro"

    describe "Utils" do
      it "alignNumber with ColorNo formats a positive number" do
        let result = alignNumber ColorNo 5 3 42.5
        result `shouldContain` "42"
        result `shouldContain` ".5"

      it "alignNumber with ColorNo formats a negative number" do
        let result = alignNumber ColorNo 5 3 (-7.0)
        result `shouldContain` "-7"
