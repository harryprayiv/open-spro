-- | Two drawings: the forecast of a shot with its steps marked, and the
-- | slow thermal traces of the scope.
module Sim.View.Plots (ForecastView, forecastPlot, scopePlot) where

import Prelude

import Data.Array (catMaybes, head, index, last, length, mapWithIndex, range)
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Halogen.HTML as HH
import Sim.Forecast (Forecast)
import Sim.View.Svg (at, circle, label, line, num, polyline, rect, svg)
import Sim.Link (ScopeSample)

width :: Number
width = 600.0

grid :: forall w i. Number -> Array (HH.HTML w i)
grid height = map gridLine [ 0, 1, 2, 3, 4 ]
  where
  gridLine i =
    let
      gy = num (toNumber i * height / 4.0)
    in
      line [ at "class" "grid", at "x1" "0", at "x2" (num width), at "y1" gy, at "y2" gy ]

scaled :: Number -> Number -> Number -> Number -> Number
scaled height lo hi value = height - (clamp lo hi value - lo) / (hi - lo) * height

trace :: forall w i. String -> Array { x :: Number, y :: Number } -> HH.HTML w i
trace name points =
  polyline
    [ at "class" name
    , at "fill" "none"
    , at "points" (joinWith " " (map (\p -> num p.x <> "," <> num p.y) points))
    ]

type ForecastView =
  { forecast :: Forecast
  -- The names of the profile's steps, in order.
  , names :: Array String
  -- The step open in the designer.
  , open :: Int
  , goalSeconds :: Maybe Number
  , goalGrams :: Number
  }

-- | The forecast shot: a band for each step with its name, the pressure
-- | asked for and reached, the flow, the weight in the cup, and a mark
-- | where the recipe wants the shot to end. The time axis is in whole
-- | quarter minutes, so a longer shot looks longer.
forecastPlot :: forall w i. ForecastView -> HH.HTML w i
forecastPlot view =
  let
    f = view.forecast
    height = 190.0
    longest = max f.seconds (fromMaybe 0.0 view.goalSeconds)
    span = toNumber (max 3 (ceil ((longest + 2.0) / 15.0))) * 15.0
    gramsTop = max 60.0 (view.goalGrams * 1.25)
    x t = t / span * width
    y = scaled height
    starts = mapWithIndex (\i start -> { i, start }) f.stepStarts
    band entry = entry.start <#> \start ->
      let
        later = catMaybes (map _.start (catMaybes (map (\j -> index starts j) (range (entry.i + 1) (length starts)))))
        end = if entry.i + 1 >= length starts then f.seconds else fromMaybe f.seconds (head later)
        name = fromMaybe "" (index view.names entry.i)
      in
        [ rect
            [ at "class" ("band" <> (if entry.i == view.open then " open" else if entry.i `mod` 2 == 1 then " odd" else ""))
            , at "x" (num (x start)), at "y" "0"
            , at "width" (num (max 0.0 (x end - x start))), at "height" (num height)
            ]
        , label [ at "class" "band-name", at "x" (num (x start + 4.0)), at "y" "12" ] (show (entry.i + 1) <> " " <> name)
        ]
    tick i =
      let
        t = toNumber i * 15.0
      in
        label [ at "class" "axis", at "x" (num (x t + 2.0)), at "y" (num (height - 3.0)) ] (show (i * 15) <> " s")
    goal = case view.goalSeconds of
      Just seconds ->
        [ line [ at "class" "goal-line", at "x1" (num (x seconds)), at "x2" (num (x seconds)), at "y1" "0", at "y2" (num height) ]
        , circle [ at "class" "goal", at "cx" (num (x seconds)), at "cy" (num (y 0.0 gramsTop view.goalGrams)), at "r" "5" ]
        ]
      Nothing ->
        [ line
            [ at "class" "goal-line", at "x1" "0", at "x2" (num width)
            , at "y1" (num (y 0.0 gramsTop view.goalGrams)), at "y2" (num (y 0.0 gramsTop view.goalGrams))
            ]
        ]
    asked = catMaybes (map (\s -> map (\bar -> { x: x s.t, y: y 0.0 12.0 bar }) s.askedBar) f.samples)
    pick lo hi field = map (\s -> { x: x s.t, y: y lo hi (field s) }) f.samples
  in
    svg [ at "viewBox" ("0 0 " <> num width <> " " <> num height), at "class" "chart" ]
      ( join (catMaybes (map band starts))
          <> grid height
          <> map tick (range 0 (ceil (span / 15.0) - 1))
          <> goal
          <>
            [ trace "target" asked
            , trace "bar" (pick 0.0 12.0 _.bar)
            , trace "flow" (pick 0.0 12.0 _.mlPerS)
            , trace "grams" (pick 0.0 gramsTop _.grams)
            ]
      )

-- | Reservoir, group block, steam pressure and heater draw over the
-- | time the scope remembers.
scopePlot :: forall w i. Number -> Array ScopeSample -> HH.HTML w i
scopePlot budgetWatts samples =
  let
    height = 200.0
    first = maybe 0.0 _.t (head samples)
    final = maybe 0.0 _.t (last samples)
    span = max 60.0 (final - first)
    x sample = (sample.t - first) / span * width
    y = scaled height
    pick lo hi field = map (\sample -> { x: x sample, y: y lo hi (field sample) }) samples
  in
    svg [ at "viewBox" ("0 0 " <> num width <> " " <> num height), at "class" "chart" ]
      ( grid height
          <>
            [ trace "watts" (pick 0.0 (max 1.0 budgetWatts) _.heaterW)
            , trace "steam" (pick 0.0 2.5 _.steamBar)
            , trace "reservoir" (pick 0.0 125.0 _.reservoirC)
            , trace "group" (pick 0.0 125.0 _.groupC)
            ]
      )
