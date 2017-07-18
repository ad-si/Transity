{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as Text
import           Data.Maybe (fromJust)
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as ByteString
import           Control.Applicative
import           Lib
import Data.Function

data Transaction = Transaction {
  entryDate :: String,
  valueDate :: String,
  from :: String,
  to :: String,
  amount :: String
} deriving (Show)

instance Yaml.FromJSON Transaction where
  parseJSON (Yaml.Object v) = Transaction <$>
    v Yaml..: "entry-date" <*>
    v Yaml..: "value-date" <*>
    v Yaml..: "from" <*>
    v Yaml..: "to" <*>
    v Yaml..: "amount"
  parseJSON _ = error "Can't parse Transaction from YAML"


prettyTrans (Transaction {from = f, to = t, amount = a}) =
  f ++ " => " ++ t ++ ": " ++ a

main = do
  let path = "test/fixtures/transactions.yaml"
  maybeTransactions <- Yaml.decodeFile path :: IO (Maybe [Transaction])
  fromJust maybeTransactions
    & map (print . prettyTrans)
    & sequence
