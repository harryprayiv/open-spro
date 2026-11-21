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

import Data.Maybe (Maybe(..), isJust)
import Data.Array (filter)
import Sim.Controller (heaterDrawWatts)
import Sim.Link (FaderMode(..), Status)
import Sim.Types (Config, GroupValve(..), MachineState(..), MixValve(..), PowerMode(..), PumpMode(..), Sensors, TwoWay(..))

type Check = { name :: String, holds :: Boolean }

-- | More than the rounding of a single-precision number of the size of
-- | a pressure in bar, and far less than anything that matters.
rounding :: Number
rounding = 1.0e-5

implies :: Boolean -> Boolean -> Boolean
implies a b = not a || b

checks :: Config -> Sensors -> Status -> Array Check
checks cfg s c =
  let
    a = c.act
    inShot = isJust c.shot
  in
    [ { name: "CircuitBudget"
      , holds: heaterDrawWatts cfg a <= cfg.circuitBudgetWatts
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
      , holds: c.paceTrimBar >= -cfg.paceLimitBar - rounding && c.paceTrimBar <= cfg.paceLimitBar + rounding
      }
    , { name: "NoPaceOutsideShot"
      , holds: not inShot `implies` (c.paceTrimBar == 0.0)
      }
    , { name: "PressureSetpointUnderLimit"
      , holds: case c.pressureTargetBar of
          Just bar -> bar <= cfg.brewPressureLimitBar - 1.0 + rounding
          Nothing -> true
      }
    , { name: "NoFillDuringShot"
      , holds: inShot `implies` (a.fillValve == Closed)
      }
    , { name: "OneHoldAtATime"
      , holds: case c.shot of
          Just shot -> not (isJust shot.manual && isJust shot.manualC)
          Nothing -> true
      }
    , { name: "HoldMatchesSelector"
      , holds: case c.shot of
          Just shot ->
            (isJust shot.manual `implies` (c.faderMode == OnPressure))
              && (isJust shot.manualC `implies` (c.faderMode == OnTemperature))
          Nothing -> true
      }
    , { name: "HeldWaterUnderLimit"
      , holds: case c.shot >>= _.manualC of
          Just celsius -> celsius <= cfg.mixLimitC - 1.0 + 10.0 * rounding
          Nothing -> true
      }
    ]

violated :: Config -> Sensors -> Status -> Array String
violated cfg s c = map _.name (filter (not <<< _.holds) (checks cfg s c))
