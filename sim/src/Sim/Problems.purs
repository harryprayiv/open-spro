-- | Everything that is wrong right now, gathered in one place so the
-- | page can say it in words and the drawings can mark it in red.
module Sim.Problems
  ( Problems
  , problems
  , anyProblem
  ) where

import Prelude

import Data.Array (filter, null)
import Data.Maybe (isNothing)
import Data.String (joinWith)
import Sim.Types (FaultKind, MachineState(..), PowerMode(..), TwoWay(..))
import Sim.World (World, sensors)

type Problems =
  { lines :: Array String
  , faults :: Array FaultKind
  , supplyCut :: Boolean
  , coldBlocked :: Boolean
  , hardWater :: Boolean
  , levelLow :: Boolean
  }

problems :: World -> Problems
problems w =
  let
    c = w.ctl
    s = sensors w
    faults = map _.kind (filter (isNothing <<< _.clearedAt) c.faults)
    supplyCut = not w.dist.inletOpen
    levelLow = c.requestedMode == Full && not s.steamLevelOk && c.act.fillValve == Closed && c.state /= Sleeping
    whenTrue condition text = if condition then [ text ] else []
  in
    { faults
    , supplyCut
    , coldBlocked: w.dist.coldBranchBlocked
    , hardWater: w.dist.hardWater
    , levelLow
    , lines:
        map (\kind -> "Fault: " <> show kind <> ". Heaters are off. Clear it under Service.") faults
          <> whenTrue supplyCut "Water supply is cut off."
          <> whenTrue w.dist.coldBranchBlocked "Cold branch is blocked."
          <> whenTrue c.waterWarning "Inlet water is out of range for scale prevention."
          <> whenTrue c.blowdownDue "Steam boiler blowdown is due."
          <> whenTrue levelLow "Steam boiler level is low and it is not filling."
          <> whenTrue (not (null w.violations)) ("Spec invariant broken: " <> joinWith ", " w.violations)
    }

anyProblem :: Problems -> Boolean
anyProblem p = not (null p.lines)