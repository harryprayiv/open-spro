-- | The invariants of spec/machine.allium, evaluated after every cycle
-- | against what the controller reports. The controller is the Rust
-- | core, and these checks are written here, apart from it, so they are
-- | an independent watch on it. OneShotAtATime has no entry because the
-- | controller has one slot for a running shot by construction.
-- |
-- | The core computes in single precision and the limits here are held
-- | in double, so the two numeric bounds allow for the rounding of one
-- | single-precision number.
module Sim.Invariants
  ( Check
  , checks
  , violated
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (filter)
import Sim.Controller (Ctl, heaterDrawWatts, shotInProgress)
import Sim.Types (GroupValve(..), MachineState(..), MixValve(..), PowerMode(..), PumpMode(..), Sensors, TwoWay(..))

type Check = { name :: String, holds :: Boolean }

-- | More than the rounding of a single-precision number of the size of
-- | a pressure in bar, and far less than anything that matters.
rounding :: Number
rounding = 1.0e-5

implies :: Boolean -> Boolean -> Boolean
implies a b = not a || b

checks :: Sensors -> Ctl -> Array Check
checks s c =
  let
    a = c.act
    inShot = shotInProgress c
  in
    [ { name: "CircuitBudget"
      , holds: heaterDrawWatts c.cfg a <= c.cfg.circuitBudgetWatts
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
    , { name: "PaceTrimBounded"
      , holds: c.paceTrimBar >= -c.cfg.paceLimitBar - rounding && c.paceTrimBar <= c.cfg.paceLimitBar + rounding
      }
    , { name: "NoPaceOutsideShot"
      , holds: not inShot `implies` (c.paceTrimBar == 0.0)
      }
    , { name: "PressureSetpointUnderLimit"
      , holds: case c.pressureTargetBar of
          Just bar -> bar <= c.cfg.brewPressureLimitBar - 1.0 + rounding
          Nothing -> true
      }
    , { name: "NoFillDuringShot"
      , holds: inShot `implies` (a.fillValve == Closed)
      }
    ]

violated :: Sensors -> Ctl -> Array String
violated s c = map _.name (filter (not <<< _.holds) (checks s c))
