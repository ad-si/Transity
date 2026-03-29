module Transity.Data.Config where

import Prelude (class Eq)

-- | Flag to switch colorized output on or off
data ColorFlag = ColorYes | ColorNo

derive instance eqColorFlag :: Eq ColorFlag

type Config = { colorState :: ColorFlag }

config :: Config
config =
  { colorState: ColorYes
  }
