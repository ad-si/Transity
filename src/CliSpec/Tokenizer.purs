module CliSpec.Tokenizer (
  CliArgToken(..),
  tokenizeCliArgument,
  tokenizeCliArguments
)
  where

import Prelude (class Eq, class Show, map, (#), (&&), (/=), (<#>), (==))

import Data.Array (concat, drop, groupBy, null, take, (:))
import Data.Foldable (elem)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (toCharArray, fromCharArray)

import CliSpec.Types (CliArgPrim(..))


-- | Intermediate representation of CLI arguments.
-- | This might not yet differentiate correctly between `Option`s and `Flag`s
-- | and between `CmdArg`s and `ValArg`s.
data CliArgToken =
  TextToken String -- ^ Could be a command or a value argument
  | FlagShortToken Char
  | FlagLongToken String
  | OptionShortToken Char CliArgPrim -- ^ `-n=3`
  | OptionShortListToken Char (Array CliArgPrim) -- ^ `-i=3,4,5`
  | OptionLongToken String CliArgPrim -- ^ `--num=3`
  | OptionLongListToken String (Array CliArgPrim) -- ^ `--items=3,4,5`
  | ValArgToken CliArgPrim
  | ValArgListToken (Array CliArgPrim)
  | SeparatorToken

derive instance genericCliArgToken :: Generic CliArgToken _
derive instance eqCliArgToken :: Eq CliArgToken
instance showCliArgToken :: Show CliArgToken where
  show = genericShow


optionLongTokenFromChars :: Array Char -> Array CliArgToken
optionLongTokenFromChars charsRest =
  let
    groupedChars = charsRest
      # groupBy (\a b -> a /= '=' && b /= '=')
    keyPart = ['x'] -- TODO
    valuePart = ['y'] -- TODO
  in
    [OptionLongToken
      (keyPart # fromCharArray)
      (StringArg (valuePart # fromCharArray))
    ]


optionShortokenFromChars :: Array Char -> Array CliArgToken
optionShortokenFromChars charsRest =
  let
    groupedChars = charsRest
      # groupBy (\a b -> a /= '=' && b /= '=')
    keyPart = ['a'] -- TODO
    valuePart = ['b'] -- TODO
  in
    case keyPart of
      [char] ->
        [OptionShortToken char (StringArg (valuePart # fromCharArray))]

      _ -> []


-- | Parse CLI arguments into a list of `CliArgToken`s
-- | One argument can lead to multiple `CliArgToken`s
-- | e.g. `-ab` -> `[FlagShortToken 'a', FlagShortToken 'b']`
tokenizeCliArgument :: String -> Array CliArgToken
tokenizeCliArgument arg = do
  let
    chars = arg # toCharArray :: Array Char
    charsRest = chars # drop 2 :: Array Char

  case chars # take 2 of
    ['-', '-'] ->
      if charsRest == []
      then [SeparatorToken]
      else
        if '=' `elem` charsRest
        then optionLongTokenFromChars charsRest
        else
          [FlagLongToken (charsRest # fromCharArray)]

    ['-', singleFlag ] ->
      if '=' `elem` charsRest
      then optionShortokenFromChars charsRest
      else
        FlagShortToken singleFlag
          : if null charsRest
            then []
            else charsRest # map FlagShortToken

    _ -> [TextToken arg]



tokenizeCliArguments :: Array String -> Array CliArgToken
tokenizeCliArguments arguments = do
  arguments
    <#> tokenizeCliArgument
    # concat


