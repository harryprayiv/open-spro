-- | Where a cup lands on the coffee map from the press packet: a wheel
-- | of 24 notes in 8 families, running clockwise from light to dark.
-- | The roast sets where a coffee starts on the wheel and the extraction
-- | moves it: an under-extracted cup slides back towards the bright,
-- | sharp side and an over-extracted one slides on towards the roasted
-- | side. How the cup was brewed gives it a second position, across the
-- | wheel instead of round it: its weight, from sweet and clear to heavy
-- | in body, and its clarity, which is how distinctly its notes show.
-- | This is a model for the simulator, to be corrected by tasting.
module Sim.Flavor
  ( Family
  , families
  , notes
  , noteCount
  , place
  , strength
  , Character
  , character
  , characterNote
  , shows
  , expected
  , Route(..)
  , route
  , routeNote
  ) where

import Prelude

import Data.Array (concatMap, filter, index, mapWithIndex, sortBy)
import Data.Foldable (foldl)
import Data.Int (floor, toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (abs)
import Sim.Plant (Brewed)
import Sim.Recipe (Roast, development, roastLabel, roasts)

type Family = { name :: String, fill :: String, pale :: String, lightText :: Boolean, notes :: Array String }

-- | The families in wheel order, with the packet's colours: `fill` for
-- | the family and `pale` for its notes.
families :: Array Family
families =
  [ { name: "FLORAL", fill: "#9fbba6", pale: "#d8d9c1", lightText: false, notes: [ "JASMINE", "ROSE", "CHAMOMILE" ] }
  , { name: "CITRUS", fill: "#c9a24a", pale: "#e4d1a6", lightText: false, notes: [ "LEMON", "ORANGE", "GRAPEFRUIT" ] }
  , { name: "FRUIT", fill: "#e88a22", pale: "#efc893", lightText: false, notes: [ "APPLE", "PEACH", "BLACKBERRY" ] }
  , { name: "SWEET", fill: "#b08d57", pale: "#dac9ab", lightText: false, notes: [ "HONEY", "CARAMEL", "MOLASSES" ] }
  , { name: "NUT", fill: "#8a5a2e", pale: "#ccb69c", lightText: true, notes: [ "ALMOND", "HAZELNUT", "WALNUT" ] }
  , { name: "COCOA", fill: "#4a2e1b", pale: "#b3a595", lightText: true, notes: [ "MILK CHOCOLATE", "DARK CHOCOLATE", "CACAO NIB" ] }
  , { name: "SPICE", fill: "#bc6a14", pale: "#e6ba8a", lightText: true, notes: [ "CINNAMON", "CLOVE", "BLACK PEPPER" ] }
  , { name: "ROAST", fill: "#17120f", pale: "#a09a90", lightText: true, notes: [ "TOAST", "TOBACCO", "SMOKE" ] }
  ]

notes :: Array String
notes = concatMap _.notes families

noteCount :: Int
noteCount = 24

-- | How far round the wheel a balanced cup of this roast sits, from 0
-- | at the light end to 1 at the dark end.
home :: Roast -> Number
home roast = 0.06 + 0.86 * development roast

-- | The extraction a balanced cup has.
centre :: Number
centre = 0.675

-- | How far round the wheel a cup lands, from 0 to 1.
place :: Roast -> Number -> Number
place roast extracted = clamp 0.0 1.0 (home roast + 1.2 * (extracted - centre))

-- | How strongly a note shows in a cup that lands at `at`: 1 at the
-- | note itself, fading to nothing two and a half notes away.
strength :: Number -> Int -> Number
strength at note =
  let
    middle = (toNumber note + 0.5) / toNumber noteCount
    away = abs (at - middle) * toNumber noteCount
  in
    clamp 0.0 1.0 (1.0 - away / 2.5)

-- | The second position of a cup.
type Character =
  { -- From -1, sweet and clear, to 1, heavy in body.
    weight :: Number
  -- From 0, notes blurred together, to 1, each note distinct.
  , clarity :: Number
  }

-- | Weight comes from the pressure the coffee dissolved at and from
-- | when the heat reached it. Pressure presses oils and fine solids into
-- | the cup, which is body. A cup whose coffee dissolved while the puck
-- | was still well under the water temperature is lighter and sweeter.
-- | Clarity comes from the evenness of the puck.
character :: Number -> Brewed -> Character
character waterC b =
  let
    pressed = (b.atBar - 8.25) / 2.5
    lateHeat = ((waterC - b.atC) - 8.0) / 4.0
  in
    { weight: clamp (-1.0) 1.0 (0.8 * pressed - 0.2 * lateHeat)
    , clarity: clamp 0.0 1.0 ((b.evenness - 0.6) / 0.35)
    }

characterNote :: Character -> String
characterNote c =
  let
    weight
      | c.weight < -0.2 = "Sweet and clear, light in body."
      | c.weight > 0.2 = "Heavy in body, with less sweetness and clarity."
      | otherwise = "Between sweet and heavy."
    clarity
      | c.clarity < 0.5 = " The notes are blurred by an uneven puck."
      | otherwise = ""
    lever
      | c.weight > 0.0 = " For more sweetness: a longer soak, a gentler ramp, or a lower peak pressure."
      | otherwise = " For more body: a shorter soak and full pressure sooner."
  in
    weight <> clarity <> lever

-- | How strongly a note shows in a cup of this clarity that lands at
-- | `at`. A blurred cup shows only the note it lands on, and faintly.
shows :: Character -> Number -> Int -> Number
shows c at note = strength at note * (0.45 + 0.55 * c.clarity)

-- | The notes a cup should show, strongest first.
expected :: Roast -> Number -> Character -> Array { note :: Int, name :: String, strength :: Number }
expected roast extracted c =
  let
    at = place roast extracted
    all = mapWithIndex (\i name -> { note: i, name, strength: shows c at i }) notes
  in
    sortBy (comparing (negate <<< _.strength)) (filter (\n -> n.strength > 0.05) all)

-- | How to get from where the cup lands to a note that is wanted.
data Route
  = Already
  -- Reachable with these beans by changing how much is extracted.
  | ExtractMore
  | ExtractLess
  -- Out of reach for this roast. The roast named is where the note
  -- sits, and the flag says whether extracting more helps meanwhile.
  | OtherRoast Roast Boolean

route :: Roast -> Number -> Int -> Route
route roast extracted note =
  let
    wanted = (toNumber note + 0.5) / toNumber noteCount
    at = place roast extracted
    -- Extraction can move a cup about this far either way before it
    -- stops tasting balanced.
    reach = 0.09
    nearest = foldl (\best r -> if abs (home r - wanted) < abs (home best - wanted) then r else best) roast roasts
  in
    if strength at note >= 0.6 then Already
    else if abs (wanted - home roast) <= reach then (if wanted > at then ExtractMore else ExtractLess)
    else OtherRoast nearest (wanted > at)

routeNote :: Roast -> Int -> Route -> String
routeNote roast note found =
  let
    name = fromMaybe "" (index notes note)
    family = fromMaybe "" (map _.name (index families (floor (toNumber note / 3.0))))
    wanted = name <> " (" <> family <> ")"
  in
    case found of
      Already -> wanted <> ": this cup should already show it."
      ExtractMore -> wanted <> ": within reach of these beans. Extract a little more: grind finer, brew hotter, soak longer, or run a longer ratio."
      ExtractLess -> wanted <> ": within reach of these beans. Extract a little less: grind coarser, brew cooler, or stop sooner."
      OtherRoast other more ->
        wanted <> ": out of reach for a " <> roastLabel roast <> " roast. It sits nearest a " <> roastLabel other
          <> " roast. With these beans, the closest you get is by extracting " <> (if more then "more" else "less")
          <> ", and past a point that costs balance."
