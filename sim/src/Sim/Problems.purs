-- | Everything that is wrong right now, gathered in one place so the
-- | page can say it in words and the drawings can mark it in red.
module Sim.Problems
  ( Problems
  , problems
  , anyProblem
  , silentAfter
  ) where

import Prelude

import Data.Array (filter, null)
import Data.Int (round)
import Data.Maybe (isNothing)
import Data.String (joinWith)
import Sim.Backend (SimView)
import Sim.Machine (Machine)
import Sim.Types (FaultKind, MachineState(..), PowerMode(..), TwoWay(..))

type Problems =
  { lines :: Array String
  , faults :: Array FaultKind
  , supplyCut :: Boolean
  , coldBlocked :: Boolean
  , hardWater :: Boolean
  , levelLow :: Boolean
  -- Nothing has been heard from the machine for a while, so what the
  -- page shows is old.
  , silent :: Boolean
  }

-- | Real milliseconds without a report before the page says so. The
-- | machine reports twenty times a second.
silentAfter :: Number
silentAfter = 1000.0

-- | `silentMs` is how long it is since the machine was last heard
-- | from. The machine's own troubles come from its last report. The
-- | simulator's view adds what a real machine could not report: which
-- | disturbances are switched on and which invariants have been broken.
problems :: Number -> Machine -> SimView -> Problems
problems silentMs m sim =
  let
    c = m.status
    s = m.sensors
    faults = map _.kind (filter (isNothing <<< _.clearedAt) c.faults)
    supplyCut = not sim.dist.inletOpen
    levelLow = c.requestedMode == Full && not s.steamLevelOk && c.act.fillValve == Closed && c.state /= Sleeping
    silent = silentMs > silentAfter
    whenTrue condition text = if condition then [ text ] else []
  in
    { faults
    , supplyCut
    , coldBlocked: sim.dist.coldBranchBlocked
    , hardWater: sim.dist.hardWater
    , levelLow
    , silent
    , lines:
        whenTrue silent ("No word from the machine for " <> show (round (silentMs / 1000.0)) <> " s. What is shown is the last it said.")
          <> map (\kind -> "Fault: " <> show kind <> ". Heaters are off. Clear it under Service.") faults
          <> whenTrue supplyCut "Water supply is cut off."
          <> whenTrue sim.dist.coldBranchBlocked "Cold branch is blocked."
          <> whenTrue c.waterWarning "Inlet water is out of range for scale prevention."
          <> whenTrue c.blowdownDue "Steam boiler blowdown is due."
          <> whenTrue levelLow "Steam boiler level is low and it is not filling."
          <> whenTrue (not (null sim.violations)) ("Spec invariant broken: " <> joinWith ", " sim.violations)
    }

anyProblem :: Problems -> Boolean
anyProblem p = not (null p.lines)
