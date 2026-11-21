-- | The simulated machine: plant and controller wired together, with a
-- | recorded trace of the current shot. The simulated backend, the
-- | forecast and the scenarios drive this. The page does not: it hears
-- | about a world only through `Sim.Backend`.
module Sim.World
  ( World
  , module Sim.Link
  , dt
  , worldWith
  , initialWorld
  , paramsOf
  , setParams
  , stepWorld
  , stepMany
  , send
  , sensors
  , warmUp
  ) where

import Prelude

import Data.Array (length, nub, snoc)
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Maybe (fromMaybe, maybe)
import Sim.Controller (Ctl, Event(..), handle, initialCtl, shotInProgress, tick)
import Sim.Controller as Controller
import Sim.Invariants (violated)
import Sim.Link (Sample)
import Sim.Params (Params)
import Sim.Params as Params
import Sim.Plant (Disturbance, Plant, Puck, freshPuck, initialPlant, noDisturbance, readSensors, stepPlant, warmPlant)
import Sim.Profiles as Profiles
import Sim.Types (Sensors)

type World =
  { now :: Number
  , plant :: Plant
  , dist :: Disturbance
  , ctl :: Ctl
  -- Grams of coffee in the basket for the next shot.
  , puck :: Puck
  , trace :: Array Sample
  , shotStartedAt :: Number
  , lastSampleAt :: Number
  -- Every spec invariant seen broken since the world was created.
  , violations :: Array String
  }

-- | Simulated seconds per step, for both the plant and the controller.
dt :: Number
dt = 0.025

-- | A cold, sleeping machine built from the given parameters.
worldWith :: Params -> World
worldWith params =
  { now: 0.0
  , plant: initialPlant params.plant
  , dist: noDisturbance
  , ctl: initialCtl params.config params.tuning (NEA.head Profiles.all)
  , puck: { doseG: params.plant.referenceDoseG, resistance: 1.0, solubility: 1.0 }
  , trace: []
  , shotStartedAt: 0.0
  , lastSampleAt: 0.0
  , violations: []
  }

initialWorld :: World
initialWorld = worldWith Params.defaults

paramsOf :: World -> Params
paramsOf w = { plant: w.plant.k, config: w.ctl.cfg, tuning: w.ctl.tune }

-- | Change parameters on a running machine. Temperatures, pressures and
-- | the shot in progress carry on from where they are.
setParams :: Params -> World -> World
setParams params w = w
  { plant = w.plant { k = params.plant }
  , ctl = Controller.setParams params.config params.tuning w.ctl
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
    broken = violated ctl.cfg s (Controller.status ctl)
    sampleDue = shotInProgress ctl && now - w.lastSampleAt >= 0.1 && length w.trace < 2000
    sample =
      { t: now - w.shotStartedAt
      , bar: plant.brewBar
      , mlPerS: plant.flowMlPerS
      , mixC: plant.mixSensorC
      , groupWaterC: plant.groupWaterC
      , targetC: fromMaybe plant.mixSensorC ctl.mixTargetC
      , grams: plant.cupGrams
      , step: maybe 0 _.stepIndex ctl.shot
      , askedBar: ctl.pressureTargetBar
      , paceBar: ctl.paceTrimBar
      }
  in
    w
      { now = now
      , ctl = ctl
      , plant = plant
      , trace = if sampleDue then snoc w.trace sample else w.trace
      , lastSampleAt = if sampleDue then now else w.lastSampleAt
      , violations = if broken == [] then w.violations else nub (w.violations <> broken)
      }

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
        , plant = freshPuck w.puck w.plant
        , trace = []
        , shotStartedAt = w.now
        , lastSampleAt = w.now
        }
    _ -> w { ctl = ctl }

-- | Skip the warm-up: put the plant at operating temperature.
warmUp :: World -> World
warmUp w = w { plant = warmPlant w.ctl.cfg.reservoirTargetC w.ctl.groupTargetC w.plant }
