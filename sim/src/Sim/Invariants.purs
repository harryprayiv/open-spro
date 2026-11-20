-- | The invariants of spec/machine.allium, evaluated against the live
-- | controller state after every cycle. OneShotAtATime has no entry
-- | because `Ctl` holds at most one running shot by construction.
module Sim.Invariants
  ( Check
  , checks
  , violated
  ) where

import Prelude

import Data.Array (filter)
import Sim.Controller (Ctl, heaterDrawWatts, shotInProgress)
import Sim.Types (GroupValve(..), MachineState(..), MixValve(..), PowerMode(..), PumpMode(..), Sensors, TwoWay(..), config)

type Check = { name :: String, holds :: Boolean }

implies :: Boolean -> Boolean -> Boolean
implies a b = not a || b

checks :: Sensors -> Ctl -> Array Check
checks s c =
  let
    a = c.act
    inShot = shotInProgress c
  in
    [ { name: "CircuitBudget"
      , holds: heaterDrawWatts a <= config.circuitBudgetWatts
      }
    , { name: "NoHeatUnlessAwake"
      , holds:
          (c.state == Sleeping || c.state == Faulted) `implies`
            (not a.brewElementOn && not a.steamElementOn && not a.groupHeaterOn && not a.heatEnabled)
      }
    , { name: "SteamElementNeedsWater"
      , holds: a.steamElementOn `implies` s.steamLevelOk
      }
    , { name: "SteamOnlyInFullMode"
      , holds: a.steamElementOn `implies` (c.requestedMode == Full)
      }
    , { name: "PumpAndBypassExclusive"
      , holds: (a.bypassValve == Open) `implies` (a.pump == Stopped)
      }
    , { name: "HydraulicsIdleOutsideShot"
      , holds:
          not inShot `implies`
            (a.pump == Stopped && a.bypassValve == Closed && a.groupValve == ToDrain && a.mixValve == Parked)
      }
    , { name: "MixRegulatesDuringShot"
      , holds: inShot `implies` (a.mixValve == Regulating)
      }
    , { name: "NoFillDuringShot"
      , holds: inShot `implies` (a.fillValve == Closed)
      }
    ]

violated :: Sensors -> Ctl -> Array String
violated s c = map _.name (filter (not <<< _.holds) (checks s c))