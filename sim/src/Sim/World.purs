-- | Plant and controller wired together, with a recorded trace of the
-- | current shot. The page and the scenario tests both drive this.
module Sim.World
  ( World
  , Sample
  , Happening
  , dt
  , initialWorld
  , stepWorld
  , stepMany
  , send
  , sensors
  , warmUp
  ) where

import Prelude

import Data.Array (last, length, nub, snoc, takeEnd)
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Sim.Controller (Ctl, Event(..), handle, initialCtl, shotInProgress, tick)
import Sim.Invariants (violated)
import Sim.Plant (Disturbance, Plant, freshPuck, initialPlant, noDisturbance, readSensors, stepPlant, warmPlant)
import Sim.Profiles as Profiles
import Sim.Types (MachineState(..), Sensors, ShotState(..), TwoWay(..), config)

-- | One point of the shot chart.
type Sample =
  { t :: Number
  , bar :: Number
  , mlPerS :: Number
  , mixC :: Number
  , groupWaterC :: Number
  , targetC :: Number
  , grams :: Number
  }

-- | A line of the machine's log. `bad` marks the ones to show in red.
type Happening = { at :: Number, text :: String, bad :: Boolean }

type World =
  { now :: Number
  , plant :: Plant
  , dist :: Disturbance
  , ctl :: Ctl
  -- Grams of coffee in the basket for the next shot.
  , doseG :: Number
  , trace :: Array Sample
  , shotStartedAt :: Number
  , lastSampleAt :: Number
  -- Every spec invariant seen broken since the world was created.
  , violations :: Array String
  -- What the machine did by itself, newest last.
  , happenings :: Array Happening
  }

-- | Simulated seconds per step, for both the plant and the controller.
dt :: Number
dt = 0.025

initialWorld :: World
initialWorld =
  { now: 0.0
  , plant: initialPlant
  , dist: noDisturbance
  , ctl: initialCtl (NEA.head Profiles.all)
  , doseG: 18.0
  , trace: []
  , shotStartedAt: 0.0
  , lastSampleAt: 0.0
  , violations: []
  , happenings: []
  }

sensors :: World -> Sensors
sensors w = readSensors w.dist w.plant

-- | Controller cycle, then plant step, then bookkeeping.
stepWorld :: World -> World
stepWorld w =
  let
    now = w.now + dt
    s = sensors w
    ctl = tick dt now s w.ctl
    plant = stepPlant dt w.dist ctl.act w.plant
    broken = violated s ctl
    sampleDue = shotInProgress ctl && now - w.lastSampleAt >= 0.1 && length w.trace < 2000
    sample =
      { t: now - w.shotStartedAt
      , bar: plant.brewBar
      , mlPerS: plant.flowMlPerS
      , mixC: plant.mixSensorC
      , groupWaterC: plant.groupWaterC
      , targetC: fromMaybe plant.mixSensorC ctl.mixTargetC
      , grams: plant.cupGrams
      }
  in
    w
      { now = now
      , ctl = ctl
      , plant = plant
      , trace = if sampleDue then snoc w.trace sample else w.trace
      , lastSampleAt = if sampleDue then now else w.lastSampleAt
      , violations = if broken == [] then w.violations else nub (w.violations <> broken)
      , happenings = takeEnd 8 (w.happenings <> changes now w.ctl ctl)
      }

-- | Describe what one controller cycle changed without being asked.
changes :: Number -> Ctl -> Ctl -> Array Happening
changes now before after =
  let
    say bad text = [ { at: now, text, bad } ]
    stateChange
      | before.state == after.state = []
      | after.state == Faulted =
          say true
            ( "Fault: " <> maybe "unknown" (show <<< _.kind) (last after.faults)
                <> ". Heaters are off and the machine needs clearing under Service."
            )
      | after.state == Ready = say false "Ready."
      | after.state == Sleeping =
          say true ("Went to sleep by itself after " <> show (round (config.idleSleepAfter / 60.0)) <> " idle minutes. Wake it to carry on.")
      | before.state == Ready && after.state == Heating =
          say false "Dropped out of ready while a temperature recovers."
      | otherwise = []
    fillChange
      | before.act.fillValve == Closed && after.act.fillValve == Open = say false "Steam boiler filling."
      | otherwise = []
    shotChange = case last after.history of
      Just shot | length after.history > length before.history ->
        say (shot.state == Aborted) ("Shot ended: " <> show shot.state <> ".")
      _ -> []
  in
    stateChange <> fillChange <> shotChange

stepMany :: Int -> World -> World
stepMany n w
  | n <= 0 = w
  | otherwise = stepMany (n - 1) (stepWorld w)

-- | Deliver an event to the controller. A shot that starts gets a fresh
-- | puck, an empty cup and a new trace.
send :: Event -> World -> Either String World
send event w = do
  ctl <- handle w.now (sensors w) event w.ctl
  pure case event of
    ShotRequested ->
      w
        { ctl = ctl
        , plant = freshPuck w.doseG w.plant
        , trace = []
        , shotStartedAt = w.now
        , lastSampleAt = w.now
        }
    _ -> w { ctl = ctl }

-- | Skip the warm-up: put the plant at operating temperature.
warmUp :: World -> World
warmUp w = w { plant = warmPlant w.ctl.groupTargetC w.plant }