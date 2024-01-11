module Transity.Plot where

import Prelude (show, (#), (<>))
import Data.Array ((:))
import Data.Newtype (class Newtype)
import Data.String (joinWith)


newtype GplotConfig = GplotConfig
  { title :: String
  , labelY :: String
  , terminalType :: String
  , lineColor :: String
  , data :: String
  , imgWidth :: Int
  , imgHeight :: Int
  }

derive instance newtypeGplotConfig :: Newtype GplotConfig _


configDefault :: GplotConfig
configDefault = GplotConfig
  { title: "Made with Transity"
  , labelY: "Commodity"
  , terminalType: "pngcairo"
  , lineColor: "#808080"
  , data: ""
  , imgWidth: 840
  , imgHeight: 420
  }


configSetData :: GplotConfig -> String ->  GplotConfig
configSetData (GplotConfig config) dataString =
  GplotConfig (config { data = dataString})


gplotTable :: String -> String
gplotTable dataString = "$data << EOD\n"
  -- TODO: Must be added to each data block
  -- <> "utc account quantity commodity\n"
  <> dataString
  <> "\nEOD\n"


-- | Code (without data) for gnuplot impulse diagram
-- | to visualize transfers on one account
gplotCode :: GplotConfig -> String
gplotCode (GplotConfig conf) =
  gplotTable conf.data <>
  ((
    ("set terminal " <> conf.terminalType
      <> " size " <> show conf.imgWidth <> ", " <> show conf.imgHeight) :
    ("set title '" <> conf.title <> "'") :
    "set key outside nobox" :
    ("set style line 12 lc rgb'" <> conf.lineColor <> "' lt 0 lw 1") :
    "set grid back ls 12" :
    "set grid xtics ytics mxtics" :
    "set style fill solid" :
    "set xdata time" :
    "set timefmt '%Y-%m-%dT%H:%M:%S'" :
    "set format x '%Y-W%W'" :
    "set xlabel 'ISO Week'" :
    "set xtics rotate by 30 right" :
    "set zeroaxis" :
    ("set ylabel '" <> conf.labelY <> "'") :
    "plot for [i=0:*] $data index i \
      \using 1:3 \
      \with impulses \
      \title columnhead(1)" :
    "" :
    []
  ) # joinWith ";")


-- | Code (without data) for cumuluative gnuplot step chart
-- | to visualize balance on one account
gplotCodeCumul :: GplotConfig -> String
gplotCodeCumul (GplotConfig conf) =
  gplotTable conf.data <>
  ((
    ("set terminal " <> conf.terminalType
      <> " size " <> show conf.imgWidth <> ", " <> show conf.imgHeight) :
    ("set title '" <> conf.title <> "'") :
    "set key outside nobox" :
    ("set style line 12 lc rgb'" <> conf.lineColor <> "' lt 0 lw 1") :
    "set grid back ls 12" :
    "set grid xtics ytics mxtics" :
    "set xdata time" :
    "set timefmt '%Y-%m-%dT%H:%M:%S'" :
    "set format x '%Y-W%W'" :
    "set xlabel 'ISO Week'" :
    "set xtics rotate by 30 right" :
    "set zeroaxis" :
    "set yrange [*<0:0<*]" :
    ("set ylabel '" <> conf.labelY <> "'") :
    "plot for [i=0:*] $data index i \
      \using 1:3 \
      \smooth cumulative with fillsteps \
      \title columnhead(1)" :
    "" :
    []
  ) # joinWith ";")
