module CliSpec.Types where

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Prelude (class Eq, class Show, bind, map, pure, show, (#), (<>))


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
instance decodeJsonCliArgPrim :: DecodeJson CliArgPrim where
  decodeJson = genericDecodeJson


cliArgPrimToString :: CliArgPrim -> String
cliArgPrimToString arg = case arg of
  StringArg str -> str
  IntArg int -> show int
  NumberArg num -> show num
  BooleanArg bool -> show bool
  NullArg -> "null"

-- | A full CLI invocation is an array of `CliArgument`s
data CliArgument
  = CmdArg String
  | FlagShort Char
  | FlagLong String
  | OptionShort Char CliArgPrim
  | OptionShortList Char (Array CliArgPrim)
  | OptionLong String CliArgPrim
  | OptionLongList String (Array CliArgPrim)
  | ValArg CliArgPrim
  | ValArgList (Array CliArgPrim)
  -- TODO: Add support for the following list types
  -- | ValArgList (Array String)
  -- | ValArgListInt (Array Int)
  -- | ValArgListNumber (Array Number)
  -- | ValArgListBoolean (Array Boolean)


derive instance genericCliArgument :: Generic CliArgument _
derive instance eqCliArgument :: Eq CliArgument
instance showCliArgument :: Show CliArgument where
  show = genericShow

cliArgToString :: CliArgument -> String
cliArgToString arg = case arg of
  CmdArg cmd -> cmd
  FlagShort flag -> "-" <> show flag
  FlagLong flag -> "--" <> flag
  OptionLong name val -> "--" <> name <> "=" <> cliArgPrimToString val
  OptionLongList name vals ->
    "--" <> name <> "=" <> (vals # map cliArgPrimToString # joinWith ",")
  OptionShort name val -> "-" <> show name <> "=" <> cliArgPrimToString val
  OptionShortList name vals ->
    "-" <> show name <> "=" <> (vals # map cliArgPrimToString # joinWith ",")
  ValArg val -> cliArgPrimToString val
  ValArgList vals -> vals # map cliArgPrimToString # joinWith ","


type Argument = {
  name :: String,
  description :: String,
  type :: String,
  optional :: Maybe Boolean,
  default :: Maybe CliArgPrim
}

type Option = {
  name :: Maybe String,
  shortName :: Maybe String, -- TODO: Change to Char
  description :: String,
  argument :: Maybe Argument,
  optional :: Maybe Boolean,
  default :: Maybe CliArgPrim
}

type CliSpecRaw =
  { name :: String
  , description :: String
  , version :: Maybe String
  , enforceValidName :: Maybe Boolean
  , funcName :: Maybe String
  , options :: Maybe (Array Option)
  , arguments :: Maybe (Array Argument)
  , commands :: Maybe (Array CliSpec)
  }

-- | Must be a newtype to avoid circular references
newtype CliSpec = CliSpec CliSpecRaw

derive instance genericCliSpec :: Generic CliSpec _
derive instance eqCliSpec :: Eq CliSpec
derive instance newtypeCliSpec :: Newtype CliSpec _
instance showCliSpec :: Show CliSpec where
  show = \(CliSpec specRaw) -> show specRaw
instance decodeJsonCliSpec :: DecodeJson CliSpec where
  decodeJson = \json -> do
    raw <- decodeJson json
    pure (CliSpec raw)


emptyCliSpecRaw :: CliSpecRaw
emptyCliSpecRaw =
  { name: ""
  , description: ""
  , funcName: Nothing
  , enforceValidName: Nothing
  , version: Nothing
  , options: Nothing
  , arguments: Nothing
  , commands: Nothing
  }


emptyCliSpec :: CliSpec
emptyCliSpec =
  CliSpec emptyCliSpecRaw
