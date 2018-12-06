module Transity.Data.Entity where

import Prelude

import Data.Argonaut.Core (toObject, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime (DateTime)
import Data.Result (Result(..), toEither, fromEither)
import Data.String (joinWith)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing), maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Transity.Data.Account (Account(..))
import Transity.Data.Account as Account
import Transity.Utils
  ( getObjField
  , getFieldMaybe
  , stringToDateTime
  , dateShowPretty
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

instance showEntity :: Show Entity where
  show = genericShow

instance decodeEntity :: DecodeJson Entity where
  decodeJson json = toEither $ decodeJsonEntity json



decodeJsonEntity :: Json -> Result String Entity
decodeJsonEntity json = do
  object <- maybe (Error "Entity is not an object") Ok (toObject json)

  id       <- object `getObjField` "id"
  name     <- object `getFieldMaybe` "name"
  note     <- object `getFieldMaybe` "note"
  utc      <- object `getFieldMaybe` "utc"
  tags     <- object `getFieldMaybe` "tags"
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
  fromEither $ decodeJson jsonObj


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
  <> (joinWith ", " $ map Account.showPretty $ fromMaybe [] entity.accounts)


-- | Map to fully qualified array of accounts
-- | (e.g _default becomes john:_default_)
toAccountsWithId :: Entity -> Array Account
toAccountsWithId (Entity entity) =
  (fromMaybe [] entity.accounts)
  <#> \(Account a) -> Account a {id = entity.id <> ":" <> a.id}
