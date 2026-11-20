-- | What a shot would do, found by running it on a copy of the machine.
-- | The copy is warm and ready, has the present parameters and a fresh
-- | puck of the given coffee, and is thrown away afterwards.
module Sim.Forecast (Forecast, forecast) where

import Prelude

import Data.Array (find, last, length, mapWithIndex)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Sim.Controller (Event(..), shotInProgress)
import Sim.Params (Params)
import Sim.Plant (Puck)
import Sim.Types (MachineState(..), PowerMode(..), Profile, ShotState)
import Sim.World (Sample, World, dt, send, stepWorld, warmUp, worldWith)

type Forecast =
  { samples :: Array Sample
  -- When each step began, in seconds from the start of the shot. A
  -- step the shot never reached is absent.
  , stepStarts :: Array (Maybe Number)
  , seconds :: Number
  , grams :: Number
  , peakBar :: Number
  -- How far pacing had moved the pressure by the end of the shot, and
  -- whether that was all it is allowed.
  , paceBar :: Number
  , paceSpent :: Boolean
  -- The share of what the coffee can give up that reached the cup.
  , extracted :: Number
  , outcome :: Maybe ShotState
  -- Why the machine would not start the shot, if it would not.
  , refused :: Maybe String
  -- Spec invariants broken during the run.
  , violations :: Array String
  }

runUntil :: Number -> (World -> Boolean) -> World -> World
runUntil limit done w
  | done w || limit <= 0.0 = w
  | otherwise = runUntil (limit - dt) done (stepWorld w)

forecast :: Params -> Profile -> Puck -> Forecast
forecast params profile puck =
  let
    start = do
      woken <- send (WakeRequested Full) (worldWith params)
      chosen <- send (ProfileSelected profile) woken
      let ready = runUntil 120.0 (\w -> w.ctl.state == Ready) (warmUp chosen)
      send ShotRequested ready { puck = puck }
  in
    case start of
      Left reason ->
        { samples: [], stepStarts: [], seconds: 0.0, grams: 0.0, peakBar: 0.0, paceBar: 0.0, paceSpent: false, extracted: 0.0, outcome: Nothing, refused: Just reason, violations: [] }
      Right begun ->
        let
          done = runUntil (params.config.shotTimeout + 5.0) (\w -> not (shotInProgress w.ctl)) begun
          shot = last done.ctl.history
          samples = done.trace
          paceBar = fromMaybe 0.0 (map _.paceBar (last samples))
          peak = foldl (\acc sample -> max acc sample.bar) 0.0 samples
          startOf i = map _.t (find (\sample -> sample.step == i) samples)
        in
          { samples
          , stepStarts: mapWithIndex (\i _ -> startOf i) (NEA.toArray profile.steps)
          , seconds: fromMaybe 0.0 (shot >>= \s -> map (_ - s.startedAt) s.endedAt)
          , grams: fromMaybe 0.0 (shot >>= _.finalGrams)
          , peakBar: peak
          , paceBar
          -- Spent means at the pacing limit, or pushing up against the
          -- ceiling a bar under the pressure limit.
          , paceSpent:
              params.config.paceLimitBar > 0.0
                && ( abs paceBar >= params.config.paceLimitBar - 0.05
                      || (paceBar > 0.3 && peak >= params.config.brewPressureLimitBar - 1.05)
                  )
          , extracted: done.plant.extracted
          , outcome: if length done.ctl.history > 0 then map _.state shot else Nothing
          , refused: Nothing
          , violations: done.violations
          }
