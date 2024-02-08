module CliSpec.Parser
  ( findFlagLong
  , findSubCmd
  , tokensToCliArguments
  )
  where

import Data.Result

import CliSpec.Tokenizer (CliArgToken(..))
import CliSpec.Types (CliArgPrim(..), CliArgument(..), CliSpec(..), Option)
import Data.Array (drop, find, foldl, head, last, zip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (singleton)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Prelude (show, (#), ($), (&&), (/=), (<#>), (<>), (==))


findFlagShort :: Maybe (Array Option) -> Char -> Maybe Option
findFlagShort cliSpecOptionsMb flagChar = do
  cliSpecOptionsMb
    # fromMaybe []
    # find (\opt -> opt.shortName == Just (singleton flagChar))


findFlagLong :: Maybe (Array Option) -> String -> Maybe Option
findFlagLong cliSpecOptionsMb flagName = do
  cliSpecOptionsMb
    # fromMaybe []
    # find (\opt -> opt.name == Just flagName)

findOptionShort :: Maybe (Array Option) -> Char -> Maybe Option
findOptionShort cliSpecOptionsMb flagChar = do
  cliSpecOptionsMb
    # fromMaybe []
    # find (\opt -> opt.shortName == Just (singleton flagChar))


findOptionLong :: Maybe (Array Option) -> String -> Maybe Option
findOptionLong cliSpecOptionsMb flagName = do
  cliSpecOptionsMb
    # fromMaybe []
    # find (\opt -> opt.name == Just flagName)


findSubCmd :: Maybe (Array CliSpec) -> String -> Maybe CliSpec
findSubCmd cliSpecCommands value  = do
  cliSpecCommands
    # fromMaybe []
    # find (\(CliSpec cmd) -> cmd.name == value)


-- | Verify that the remaining tokens are allowed
-- | for the given command specification and return
-- | the corresponding `CliArgument`s.
verifyTokensAreAllowed
  :: CliSpec
  -> Array CliArgToken
  -> Result String (Array CliArgument)
verifyTokensAreAllowed (CliSpec cliSpecRaw) tokens = do
  let
    argsAndTokens = zip
      (cliSpecRaw.arguments # fromMaybe [])
      tokens

  argsAndTokens
    # (foldl
        (\(Tuple cliArgs remainingTokens) (Tuple arg token) ->
          if remainingTokens == []
          then
            -- Finish looping, but don't change cliArgs anymore
            Tuple cliArgs []
          else
            case arg.type, token of
              "Text", TextToken txt ->
                Tuple
                  (cliArgs <> [Ok $ ValArg (TextArg txt)])
                  (remainingTokens # drop 1)

              "List-Text", TextToken _ ->
                Tuple
                  (cliArgs
                    <> [(remainingTokens
                          <#> (\tok -> case tok of
                              TextToken t -> Ok $ TextArg t
                              _ -> Error $ "Unsupported token: " <> show tok
                            )
                          # sequence
                          <#> ValArgList
                        )]
                  )
                  [] -- No more remaining tokens

              _, _ ->
                Tuple
                  (cliArgs <> [Error $ "Invalid argument:" <> show arg])
                  (remainingTokens # drop 1)
        )
        (Tuple [] tokens)
      )
    # (\(Tuple cliArgs _) -> cliArgs)
    # sequence



-- | Determine the correct value of the `CliArgToken`s
-- | by matching them against the spec.
-- | Especially for the differentiation between `Option`s and `Flag`s.
tokensToCliArguments
  :: CliSpec
  -> Array CliArgToken
  -> Result String (Array CliArgument)
tokensToCliArguments cliSpec@(CliSpec cliSpecRaw) tokens = do
  let
    mainCmdRes :: Result String CliSpec
    mainCmdRes = case tokens # head of
      Just (TextToken cmdName) ->
        if
          cliSpecRaw.name /= cmdName &&
          cliSpecRaw.enforceValidName == Just true
        then Error $
          "ERROR: \""
            <> cliSpecRaw.name
            <> "\" is executed with the differently named executable \""
            <> cmdName
            <> "\""
        else Ok cliSpec
      _ -> Error $
            "Something went wrong. "
            <> "The first token should be a command or a value."


  -- If the first token after the main command is a subcommand
  -- parse recursively the rest of the tokens.
  case tokens # drop 1 # head of
    Just (TextToken name) ->
      case findSubCmd cliSpecRaw.commands name of
        -- Is subcommand
        Just cmd ->
          case tokensToCliArguments cmd (tokens # drop 1) of
            Ok args -> case mainCmdRes of
              Error err -> Error err
              Ok _ -> Ok $ [CmdArg cliSpecRaw.name] <> args
            err -> err
        -- Is value
        Nothing ->
          case mainCmdRes of
              Error err -> Error err
              Ok mainCmd ->
                verifyTokensAreAllowed mainCmd (tokens # drop 1)
                  <#> (\cliArgs -> [CmdArg cliSpecRaw.name] <> cliArgs )

    _ -> do
      let
        toError :: CliArgToken -> Result String CliArgument
        toError token = Error $ case token of
          FlagLongToken flagName ->
            "Unknown flag: " <> flagName
          FlagShortToken flagChar ->
            "Unknown flag: " <> singleton flagChar
          OptionShortToken flagChar _ ->
            "Unknown option: " <> singleton flagChar
          OptionLongToken flagName _ ->
            "Unknown option: " <> flagName
          _ ->
            "Unknown token"

        remainingTokensWithSucc :: Array (Tuple CliArgToken (Maybe CliArgToken))
        remainingTokensWithSucc =
          zip
            (tokens # drop 1)
            ((tokens # drop 2 <#> Just) <> [Nothing])

        options :: Array (Result String CliArgument)
        options =
          remainingTokensWithSucc
          # foldl
            (\acc (Tuple token nextTokenMb) -> case token of
                FlagLongToken flagName ->
                  case findFlagLong cliSpecRaw.options flagName of
                    Just flagOrOpt -> case flagOrOpt.argument of
                      Just _arg ->
                        case nextTokenMb of
                          Just (TextToken val) ->
                            -- TODO: Check if val is allowed at this position
                            acc <> [Ok $ OptionLong flagName (TextArg val)]
                          _ ->  acc <> [Ok $ FlagLong flagName]
                      Nothing ->  acc <> [Ok $ FlagLong flagName]
                    Nothing ->
                      -- Maybe it's a long option
                      case findOptionLong cliSpecRaw.options flagName of
                        Just _ ->
                          acc <> [Ok $ OptionLong flagName (TextArg "TODO")]
                        Nothing ->
                          acc <> [toError token]

                FlagShortToken flagChar ->
                  case findFlagShort cliSpecRaw.options flagChar of
                    Just _ ->
                      acc <> [Ok $ OptionShort flagChar (TextArg "TODO")]
                    Nothing ->
                      -- Maybe it's a short option
                      case findOptionShort cliSpecRaw.options flagChar of
                        Just _ ->
                          acc <> [Ok $ OptionShort flagChar (TextArg "TODO")]
                        Nothing ->
                          acc <> [toError token]

                OptionShortToken flagChar _arg ->
                  case findFlagShort cliSpecRaw.options flagChar of
                    Just _option ->
                      acc <> [Ok $ OptionShort flagChar (TextArg "TODO")]
                    Nothing ->
                      acc <> [toError token]

                OptionLongToken flagName _arg ->
                  case findFlagLong cliSpecRaw.options flagName of
                    Just _option ->
                      acc <> [Ok $ OptionLong flagName (TextArg "TODO")]
                    Nothing ->
                      acc <> [toError token]

                TextToken txt ->
                  case acc # last of
                    -- This token was already consumed so don't add it
                    Just (Ok (OptionLong _ _)) -> acc
                    _ -> acc <> [Ok $ ValArg (TextArg txt)]

                _ -> []
            )
            []

      sequence
        $ [mainCmdRes <#> (\(CliSpec cmdSpec) -> CmdArg cmdSpec.name)]
        <> options
