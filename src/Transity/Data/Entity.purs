module Transity.Data.Entity where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Map (values)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Result (Result(..), fromEither, toEither)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Transity.Data.Account (Account(..))
import Transity.Data.Account as Account
import Transity.Data.Balance (Balance(..))
import Transity.Data.CommodityMap (CommodityMap)
import Transity.Data.Transfer (Transfer(..))
import Transity.Utils
  ( dateShowPretty
  , getFieldMaybe
  , getObjField
  , resultWithJsonDecodeError
  , stringToDateTime
  , stringifyJsonDecodeError
  )

newtype Entity = Entity
  { id :: String
  , name :: Maybe String
  , note :: Maybe String
  , utc :: Maybe DateTime
  , tags :: Maybe (Array String)
  , accounts :: Maybe (Array Account)
  }

derive instance genericEntity :: Generic Entity _
derive instance newtypeEntity :: Newtype Entity _
derive newtype instance eqEntity :: Eq Entity

instance showEntity :: Show Entity where
  show = genericShow

instance decodeEntity :: DecodeJson Entity where
  decodeJson json = toEither $ resultWithJsonDecodeError $ decodeJsonEntity json

decodeJsonEntity :: Json -> Result String Entity
decodeJsonEntity json = do
  object <- maybe (Error "Entity is not an object") Ok (toObject json)

  id <- object `getObjField` "id"
  name <- object `getFieldMaybe` "name"
  note <- object `getFieldMaybe` "note"
  utc <- object `getFieldMaybe` "utc"
  tags <- object `getFieldMaybe` "tags"
  accounts <- object `getFieldMaybe` "accounts"

  pure $ Entity
    { id
    , name
    , note
    , utc: utc >>= stringToDateTime
    , tags
    , accounts
    }

fromJson :: String -> Result String Entity
fromJson json = do
  jsonObj <- fromEither $ jsonParser json
  stringifyJsonDecodeError $ fromEither $ decodeJson jsonObj

zero :: Entity
zero = Entity
  { id: ""
  , name: Nothing
  , note: Nothing
  , utc: Nothing
  , tags: Nothing
  , accounts: Nothing
  }

showPretty :: Entity -> String
showPretty (Entity entity) =
  entity.id
    <> " | "
    <> (fromMaybe "" entity.name)
    <> " | "
    <> (fromMaybe "" entity.note)
    <> " | "
    <> (fromMaybe "" (entity.utc <#> dateShowPretty))
    <> " | "
    <> (joinWith ", " $ fromMaybe [] entity.tags)
    <> " | "
    <>
      ( joinWith ", " $ (fromMaybe [] entity.accounts)
          <#> (\(Account acc) -> Account.showPretty acc.id acc.commodityMap)
      )

-- | Map to fully qualified array of accounts
-- | (e.g _default_ becomes john:_default_)
toAccountsWithId :: Entity -> Array Account
toAccountsWithId (Entity entity) =
  (fromMaybe [] entity.accounts)
    <#> \(Account a) -> Account a { id = entity.id <> ":" <> a.id }

-- | Map the entity's balance to an array of balancing transfers
toTransfers :: Entity -> Array Transfer
toTransfers entity =
  let
    accounts = toAccountsWithId entity

    comMapToTransfers ::
      forall a.
      { id :: String | a } ->
      DateTime ->
      CommodityMap ->
      Array Transfer
    comMapToTransfers accountRec utc comMap =
      (values comMap)
        # Array.fromFoldable
        <#>
          ( \amount -> Transfer
              { utc: Just utc
              , from: accountRec.id
              , to: "_void_"
              , amount
              , note: Nothing
              }
          )

    accToTrans :: Account -> Array Transfer
    accToTrans (Account account) =
      (fromMaybe [] account.balances)
        <#>
          ( \(Balance utc comMap) ->
              comMapToTransfers account utc comMap
          )
        # fold
  in
    accounts <#> accToTrans # fold

