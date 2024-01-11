module Test.CliSpec where

import Prelude (Unit, (#))

import Control.Bind (discard)
import Data.String (Pattern(..), split)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import CliSpec.Types (CliArgPrim(..), CliArgument(..), parseCliArguments)


parseCliStr :: String -> Array CliArgument
parseCliStr str =
  str
    # split (Pattern " ")
    # parseCliArguments


tests :: Spec Unit
tests = do
  describe "CliSpec" do
    describe "Parser" do
      it "parses a CLI invocation" do
        (parseCliStr "xx dir")
          `shouldEqual` [CmdArg "xx", ValArg (StringArg "dir")]

      it "parses a CLI invocation with a long flag" do
        (parseCliStr "xx --version")
          `shouldEqual` [CmdArg "xx", FlagLong "version"]

      it "parses a CLI invocation with a short flag" do
        (parseCliStr "xx -a")
          `shouldEqual` [CmdArg "xx", FlagShort 'a']

      it "parses a CLI invocation with several short flags" do
        (parseCliStr "xx -ab")
          `shouldEqual` [CmdArg "xx", FlagShort 'a', FlagShort 'b']

      it "parses a CLI invocation with a long flag and an argument" do
        (parseCliStr "xx --verbose dir")
          `shouldEqual`
            [ CmdArg "xx"
            , FlagLong "verbose"
            , ValArg (StringArg "dir")
            ]
