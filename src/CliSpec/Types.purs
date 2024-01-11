module CliSpec.Types where

import Prelude (
  class Eq, class Show, bind, max, pure, map,
  ($), (+), (<>), (==), (||), (&&), (>>=), (<$>), (#), (<#>)
)

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array ((:), length, drop, take, null, concat)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Result (Result(Ok, Error), toEither)
import Data.Show.Generic (genericShow)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (toCharArray, fromCharArray)


data CliArgPrim
  = StringArg String
  | IntArg Int
  | NumberArg Number
  | BooleanArg Boolean
  | NullArg

derive instance genericCliArgPrim :: Generic CliArgPrim _
derive instance eqCliArgPrim :: Eq CliArgPrim
instance showCliArgPrim :: Show CliArgPrim where
  show = genericShow


-- | A full CLI invocation is an array of `CliArgument`s
data CliArgument
  = CmdArg String
  | FlagShort Char
  | FlagLong String
  | ValArg CliArgPrim
  | ValArgList (Array CliArgPrim)

derive instance genericCliArgument :: Generic CliArgument _
derive instance eqCliArgument :: Eq CliArgument
instance showCliArgument :: Show CliArgument where
  show = genericShow


-- | One argument can lead to multiple `CliArgument`s
-- | e.g. `-ab` -> `[FlagShort 'a', FlagShort 'b']`
parseCliArgument :: String -> Array CliArgument
parseCliArgument arg = do
  let
    chars = arg # toCharArray :: Array Char
    charsRest = chars # drop 2 :: Array Char

  case chars # take 2 of
    ['-', '-'] -> [FlagLong $ charsRest # fromCharArray]

    ['-', singleFlag ] -> FlagShort singleFlag
                            : if null charsRest
                              then []
                              else charsRest # map FlagShort

    _ -> [ValArg $ StringArg arg]


parseCliArguments :: Array String -> Array CliArgument
parseCliArguments arguments =
  let
    firstArg = arguments # take 1
    restArgs = arguments # drop 1
  in
  (firstArg <#> CmdArg)
  <> (restArgs
      # map parseCliArgument
      # concat)


type Command = {
  name :: String,
  description :: String,
  arguments :: Array String,
  funcName :: Maybe String
}

type CliSpec =
  { name :: String
  , description :: String
  , version :: String
  , funcName :: String
  , commands :: Array Command
  -- , options :: Array String
  }

-- derive instance genericCliSpec :: Generic CliSpec _
-- -- derive instance newtypeCliSpec :: Newtype CliSpec _
-- derive newtype instance eqCliSpec :: Eq CliSpec

-- instance showCliSpec :: Show CliSpec
-- -- where
-- --   show = genericShow

-- instance encodeCliSpec :: EncodeJson CliSpec
-- -- where
-- --   encodeJson a = genericEncodeJson a

-- instance decodeCliSpec :: DecodeJson CliSpec
-- -- where
-- --   decodeJson json =
-- --     toEither $ resultWithJsonDecodeError $ decodeJsonCliSpec json
