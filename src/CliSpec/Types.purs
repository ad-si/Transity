module CliSpec.Types where

import Prelude (class Eq, class Show, map, (#), ($), (+), (==))

import Data.Array ((:), drop, take, null, concat, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
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
parseCliArgument :: Int -> String -> Array CliArgument
parseCliArgument index arg = do
  let
    chars = arg # toCharArray :: Array Char
    charsRest = chars # drop 2 :: Array Char

  case chars # take 2 of
    ['-', '-'] -> [FlagLong $ charsRest # fromCharArray]

    ['-', singleFlag ] -> FlagShort singleFlag
                            : if null charsRest
                              then []
                              else charsRest # map FlagShort

    _ -> if index == 0
          then [CmdArg arg]
          else [ValArg $ StringArg arg]


parseCliArguments :: Int -> Array String -> Array CliArgument
parseCliArguments subcmdLevels arguments = do
  arguments
    # mapWithIndex (\index ->
        parseCliArgument
          (if subcmdLevels == 0 then index else index + 1)
      )
    # concat


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
