-- | One piece of advice for a cup that would not come out right, found
-- | by trying changes to the recipe on a copy of the machine and naming
-- | the change that works best. It is worked out when asked for, because
-- | it costs a few dozen forecast shots. The changes tried are to the grind, the
-- | water temperature and the soak, alone and together, and never to a
-- | setting the barista holds.
module Sim.Advice
  ( Advice
  , Free
  , advise
  , needed
  ) where

import Prelude

import Data.Array (catMaybes, filter, head, sortBy)
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Data.Number.Format (fixed, toStringWith)
import Data.String (drop, joinWith, take, toUpper)
import Sim.Cup (Taste(..), taste)
import Sim.Forecast (Forecast, forecast)
import Sim.Params (Params)
import Sim.Recipe (Recipe, grindLimits, puckOf, secondsLimits, soakLimits, tempLimits, toProfile)
import Sim.Types (Profile, ShotState(..))

type Advice =
  { -- What to do, as a sentence.
    text :: String
  -- The recipe the advice was worked out for.
  , from :: Recipe
  -- The recipe with the advice taken, and the shot it is forecast to pull.
  , recipe :: Recipe
  , forecast :: Forecast
  }

-- | The settings the barista has left for the machine to suggest.
type Free = { grind :: Boolean, temp :: Boolean, soak :: Boolean, seconds :: Boolean }

-- | A change to try: grinder steps, degrees and seconds of soak, each
-- | added to what the recipe has.
type Move = { grind :: Number, temp :: Number, soak :: Number }

-- | The extraction at the middle of the balanced band.
centre :: Number
centre = 0.675

-- | The evenness below which advice is worth giving for the puck alone.
evenEnough :: Number
evenEnough = 0.85

-- | Whether a forecast calls for advice: the shot reaches its weight
-- | and the cup is out of balance or the puck is uneven.
needed :: Forecast -> Boolean
needed f =
  f.outcome == Just StoppedByWeight
    && (taste f.extracted /= Balanced || f.brewed.evenness < evenEnough)

-- | How far a forecast is from a good shot: 0 is an even puck extracted
-- | to the middle of the balanced band and finished on time, and 1 is
-- | about the edge of balance.
miss :: Recipe -> Forecast -> Number
miss wanted f =
  if f.outcome /= Just StoppedByWeight then 100.0
  else
    abs (f.extracted - centre) / 0.075
      + max 0.0 (0.92 - f.brewed.evenness) * 5.0
      + abs (f.seconds - wanted.seconds) / 12.0
      + (if f.paceSpent then 0.4 else 0.0)

-- | The changes worth trying in one round, simplest first. A cup that
-- | needs more gets a finer grind, hotter water and a longer soak. A cup
-- | that needs less gets the reverse, except that a channeled puck is
-- | offered a soak whichever way its extraction is off, because the soak
-- | is what mends the puck.
moves :: Boolean -> Boolean -> Array Move
moves more uneven =
  let
    m grind temp soak = { grind, temp, soak }
  in
    if more then
      [ m (-1.0) 0.0 0.0, m (-2.0) 0.0 0.0
      , m 0.0 1.0 0.0, m 0.0 2.0 0.0
      , m 0.0 0.0 3.0, m 0.0 0.0 6.0, m 0.0 0.0 10.0
      , m (-1.0) 0.0 3.0, m (-1.0) 0.0 6.0, m (-2.0) 0.0 6.0, m (-2.0) 0.0 10.0
      , m (-1.0) 1.0 0.0
      ]
    else
      [ m 1.0 0.0 0.0, m 2.0 0.0 0.0
      , m 0.0 (-1.0) 0.0, m 0.0 (-2.0) 0.0
      , m 0.0 0.0 (-3.0), m 0.0 0.0 (-6.0)
      , m 1.0 (-1.0) 0.0, m 1.0 0.0 (-3.0)
      ]
        <> (if uneven then [ m 0.0 0.0 6.0, m 1.0 0.0 6.0, m 2.0 0.0 6.0 ] else [])

-- | The recipe with a move made, or nothing if the move touches a held
-- | setting or runs into a limit. A free shot time moves with the soak,
-- | which adds a little under a second to the shot for each second.
moved :: Free -> Recipe -> Move -> Maybe Recipe
moved free r move =
  let
    grind = clamp grindLimits.min grindLimits.max (r.grind + move.grind)
    tempC = clamp tempLimits.min tempLimits.max (r.tempC + move.temp)
    soakS = clamp soakLimits.min soakLimits.max (r.soakS + move.soak)
    seconds =
      if free.seconds then clamp secondsLimits.min secondsLimits.max (r.seconds + 0.8 * (soakS - r.soakS))
      else r.seconds
    held =
      (move.grind /= 0.0 && not free.grind)
        || (move.temp /= 0.0 && not free.temp)
        || (move.soak /= 0.0 && not free.soak)
    blocked =
      (move.grind /= 0.0 && grind /= r.grind + move.grind)
        || (move.temp /= 0.0 && tempC /= r.tempC + move.temp)
        || (move.soak /= 0.0 && soakS /= r.soakS + move.soak)
  in
    if held || blocked then Nothing
    else Just r { grind = grind, tempC = tempC, soakS = soakS, seconds = seconds }

n0 :: Number -> String
n0 = toStringWith (fixed 0)

n1 :: Number -> String
n1 = toStringWith (fixed 1)

n2 :: Number -> String
n2 = toStringWith (fixed 2)

-- | A move in the words a barista would use.
say :: Recipe -> Recipe -> String
say before after =
  let
    steps count = if count == 1.0 then "one step" else if count == 2.0 then "two steps" else n1 count <> " steps"
    grind
      | after.grind < before.grind = [ "grind " <> steps (before.grind - after.grind) <> " finer" ]
      | after.grind > before.grind = [ "grind " <> steps (after.grind - before.grind) <> " coarser" ]
      | otherwise = []
    temp
      | after.tempC > before.tempC = [ "brew " <> n0 (after.tempC - before.tempC) <> " C hotter" ]
      | after.tempC < before.tempC = [ "brew " <> n0 (before.tempC - after.tempC) <> " C cooler" ]
      | otherwise = []
    soak
      | after.soakS > before.soakS && before.soakS == 0.0 = [ "add a " <> n0 after.soakS <> " s line-pressure soak" ]
      | after.soakS > before.soakS = [ "soak " <> n0 (after.soakS - before.soakS) <> " s longer" ]
      | after.soakS < before.soakS && after.soakS == 0.0 = [ "drop the soak" ]
      | after.soakS < before.soakS = [ "soak " <> n0 (before.soakS - after.soakS) <> " s shorter" ]
      | otherwise = []
    seconds
      | after.seconds /= before.seconds = [ "allow " <> n0 after.seconds <> " s for the shot" ]
      | otherwise = []
    parts = grind <> temp <> soak <> seconds
    sentence = case parts of
      [ only ] -> only
      [ first, second ] -> first <> " and " <> second
      _ -> joinWith ", " parts
  in
    sentence

type Tried = { move :: Move, recipe :: Recipe, forecast :: Forecast, miss :: Number }

-- | The change that would mend this recipe's cup, if there is one. The
-- | search goes in rounds: each round tries every move on the best
-- | recipe so far and keeps the one that helps most, and it stops when
-- | the cup is right, when nothing helps, or after four rounds. The
-- | advice is the whole distance from the recipe as it stands.
advise :: Params -> Array Profile -> Free -> Recipe -> Forecast -> Maybe Advice
advise params shapes free recipe now =
  if not (needed now) then Nothing
  else
    let
      run r = forecast params (toProfile shapes r) (puckOf r)
      round :: Recipe -> Forecast -> Array Tried
      round r shot =
        let
          -- A channeled puck that is not over-extracted is treated as
          -- one that needs more: what it lacks is contact with the water.
          more = taste shot.extracted /= Over
          uneven = shot.brewed.evenness < evenEnough
          try move = moved free r move <#> \next ->
            let
              made = run next
              -- A small charge for each setting changed, so the simpler
              -- of two equal changes wins.
              changes = (if move.grind /= 0.0 then 1.0 else 0.0) + (if move.temp /= 0.0 then 1.0 else 0.0) + (if move.soak /= 0.0 then 1.0 else 0.0)
            in
              { move, recipe: next, forecast: made, miss: miss next made + 0.06 * changes }
        in
          sortBy (comparing _.miss) (catMaybes (map try (moves more uneven)))
      search :: Int -> Recipe -> Forecast -> { recipe :: Recipe, forecast :: Forecast }
      search rounds r shot =
        if rounds <= 0 || not (needed shot) then { recipe: r, forecast: shot }
        else case head (round r shot) of
          Just best | best.miss < miss r shot - 0.1 -> search (rounds - 1) best.recipe best.forecast
          _ -> { recipe: r, forecast: shot }
      found = search 4 recipe now
      f = found.forecast
      verdict =
        if needed f then " That is as near as the grind, the water and the soak will bring it."
        else ""
      outcome =
        "Forecast with that: " <> n1 f.grams <> " g in " <> n1 f.seconds <> " s, extracted " <> n2 f.extracted
          <> ", evenness " <> n2 f.brewed.evenness <> "." <> verdict
      -- When the advice includes a soak, say what the plainer remedy
      -- of moving the water temperature alone would do.
      temperatureOnly = head (filter (\t -> t.move.grind == 0.0 && t.move.soak == 0.0 && abs t.move.temp == 2.0) (round recipe now))
      beside = case temperatureOnly of
        Just t | found.recipe.soakS /= recipe.soakS ->
          " Changing the water temperature alone (" <> say recipe t.recipe <> ") gives "
            <> n2 t.forecast.extracted <> " at evenness " <> n2 t.forecast.brewed.evenness <> "."
        _ -> ""
    in
      if found.recipe == recipe then Nothing
      else
        Just
          { text: upperFirst (say recipe found.recipe) <> ". " <> outcome <> beside
          , from: recipe
          , recipe: found.recipe
          , forecast: f
          }

upperFirst :: String -> String
upperFirst text = toUpper (take 1 text) <> drop 1 text
