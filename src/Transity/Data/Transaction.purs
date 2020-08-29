module Transity.Data.Transaction where

import Prelude
  ( class Show, class Eq, bind, map, pure
  , (#), ($), (<>), (>>=), (<#>), (/=)
  )

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (toObject, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators (getFieldOptional, defaultField)
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime (DateTime)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (class Newtype)
import Data.Result (Result(..), toEither, fromEither)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Foreign (renderForeignError)
import Text.Format (format, width)
import Transity.Data.Transfer (Transfer(..))
import Transity.Data.Transfer as Transfer
import Transity.Utils
  ( getObjField, getFieldMaybe, stringToDateTime
  , dateShowPretty, indentSubsequent, ColorFlag(..))


-- newtype FilePath = FilePath String

newtype Transaction = Transaction
  { id :: Maybe String
  , utc :: Maybe DateTime
  , note :: Maybe String
  , files :: Array String
  , transfers :: Array Transfer
  }

derive instance genericTransaction :: Generic Transaction _
derive instance newtypeTransaction :: Newtype Transaction _
derive newtype instance eqTransaction :: Eq Transaction

instance showTransaction :: Show Transaction where
  show = genericShow

instance decodeTransaction :: DecodeJson Transaction where
  decodeJson json = toEither $ decodeJsonTransaction json


decodeJsonTransaction :: Json -> Result String Transaction
decodeJsonTransaction json = do
  object <- maybe (Error "Transaction is not an object") Ok (toObject json)

  id        <- object `getFieldMaybe` "id"
  utc       <- object `getFieldMaybe` "utc"
  note      <- object `getFieldMaybe` "note"
  files     <- fromEither $ object `getFieldOptional` "files" `defaultField` []
  transfers <- object `getObjField` "transfers"

  pure $ Transaction
    { id
    , utc: utc >>= stringToDateTime
    , note
    , files
    , transfers
    }


fromJson :: String -> Result String Transaction
fromJson string = do
  json <- fromEither $ jsonParser string
  transaction <- fromEither $ decodeJson json
  pure transaction


fromYaml :: String -> Result String Transaction
fromYaml yaml =
  let
    result = yaml
      # parseYAMLToJson
      # runExcept
      # fromEither
  in
    case result of
      Error error -> Error
        ( "Could not parse YAML: "
          <> fold (map renderForeignError error)
        )
      Ok json -> fromEither $ decodeJson json


toTransfers :: Array Transaction -> Array Transfer
toTransfers transactions =
  transactions
    <#> (\(Transaction transac) -> transac.transfers
            <#> (\(Transfer transf) -> Transfer (transf
                  { utc = if transf.utc /= Nothing
                          then transf.utc
                          else transac.utc
                  }
              )))
    # fold


showTransfersWithDate :: ColorFlag -> Transaction -> String
showTransfersWithDate colorFlag (Transaction transac) =
  transac.transfers
    <#> (\(Transfer transf) -> Transfer (transf
            { utc = if transf.utc /= Nothing
                    then transf.utc
                    else transac.utc
            }
        ))
    <#> Transfer.showPrettyColorized
    # fold


showPretty :: Transaction -> String
showPretty = showPrettyAligned ColorNo


showPrettyAligned :: ColorFlag -> Transaction -> String
showPrettyAligned colorFlag (Transaction tact) =
  let
    transfersPretty = map
      (Transfer.showPrettyAligned colorFlag 15 15 5 3 10)
      tact.transfers
    offsetDate = 16
    offsetIndentation = 4
    addId str = " | (id " <> str <> ")"
  in
    fromMaybe (" " `power` offsetDate) (map dateShowPretty tact.utc)
    <> " | " <> format (width 30) (fromMaybe "NO NOTE" tact.note)
    <> fromMaybe "" (map addId tact.id)
    <> indentSubsequent offsetIndentation ("\n" <> fold transfersPretty)
    <> "\n"
