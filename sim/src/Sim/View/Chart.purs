-- | The shot chart: pressure, flow, water temperature against its
-- | target, and weight in the cup, each on its own scale.
module Sim.View.Chart (chart) where

import Prelude

import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.String (joinWith)
import Halogen.HTML as HH
import Sim.View.Svg (at, line, num, polyline, svg)
import Sim.World (Sample)

width :: Number
width = 600.0

height :: Number
height = 240.0

chart :: forall w i. Array Sample -> HH.HTML w i
chart samples =
  let
    lastT = foldl (\acc sample -> max acc sample.t) 0.0 samples
    span = max 40.0 lastT
    x sample = sample.t / span * width
    y lo hi value = height - (clamp lo hi value - lo) / (hi - lo) * height
    points pick = joinWith " " (map (\sample -> num (x sample) <> "," <> num (pick sample)) samples)
    trace name pick =
      polyline [ at "class" name, at "fill" "none", at "points" (points pick) ]
    gridLine i =
      let
        gy = num (toNumber i * height / 4.0)
      in
        line [ at "class" "grid", at "x1" "0", at "x2" (num width), at "y1" gy, at "y2" gy ]
  in
    svg [ at "viewBox" ("0 0 " <> num width <> " " <> num height), at "class" "chart" ]
      ( map gridLine [ 0, 1, 2, 3, 4 ]
          <>
            [ trace "bar" (y 0.0 12.0 <<< _.bar)
            , trace "flow" (y 0.0 12.0 <<< _.mlPerS)
            , trace "target" (y 80.0 100.0 <<< _.targetC)
            , trace "water" (y 80.0 100.0 <<< _.groupWaterC)
            , trace "grams" (y 0.0 60.0 <<< _.grams)
            ]
      )