-- | What passes between the page and a machine. The page sends
-- | requests and hears messages, and that is all it has: it never calls
-- | the controller and never reads its memory. A simulated machine
-- | (`Sim.Backend`) and, later, the real one behind the Pi's service
-- | both speak in these types.
-- |
-- | Nothing here says when a message arrives. A request is answered
-- | some time after it was sent, a report describes the machine as it
-- | was some time ago, and either may never arrive at all.
module Sim.Link
  ( Event(..)
  , eventLabel
  , Status
  , ShotNow
  , ShotEnd
  , restingStatus
  , Telemetry
  , Request
  , Message(..)
  , Sample
  , ScopeSample
  , Happening
  , shotNow
  , shotEnded
  ) where

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe)
import Sim.Types (Actuators, ClientKind, Fault, FaultKind, MachineState(..), PowerMode(..), Profile, Sensors, Shot, ShotState(..), idleActuators)

-- | What a client can ask of the machine.
data Event
  = WakeRequested PowerMode
  | SleepRequested
  | ProfileSelected Profile
  | ShotRequested
  | StepAdvanceRequested
  | ShotStopRequested
  | FaultsCleared ClientKind
  | BlowdownConfirmed
  | WaterWarningAcknowledged
  | SensorFailed FaultKind

-- | An event in a few words, for telling the barista which request
-- | went unanswered.
eventLabel :: Event -> String
eventLabel = case _ of
  WakeRequested _ -> "wake"
  SleepRequested -> "sleep"
  ProfileSelected _ -> "the recipe"
  ShotRequested -> "start shot"
  StepAdvanceRequested -> "next step"
  ShotStopRequested -> "stop"
  FaultsCleared _ -> "clear faults"
  BlowdownConfirmed -> "confirm blowdown"
  WaterWarningAcknowledged -> "acknowledge water warning"
  SensorFailed _ -> "break a sensor"

-- | The running shot as the controller reports it: its place in the
-- | selected profile and when it and its present step began.
type ShotNow = { stepIndex :: Int, startedAt :: Number, stepStartedAt :: Number }

-- | The last shot to have ended.
type ShotEnd =
  { state :: ShotState
  , stepIndex :: Int
  , startedAt :: Number
  , endedAt :: Number
  -- Absent for a shot a fault aborted.
  , finalGrams :: Maybe Number
  }

-- | Everything the controller reports about itself. This is the status
-- | message of firmware/spro-core/src/core_wire.rs, decoded.
type Status =
  { requestedMode :: PowerMode
  , state :: MachineState
  , act :: Actuators
  , groupTargetC :: Number
  , mixTargetC :: Maybe Number
  , pressureTargetBar :: Maybe Number
  , flowTargetMlPerS :: Maybe Number
  -- How far pacing has moved the pressure setpoint.
  , paceTrimBar :: Number
  -- Flow into the group as the controller estimates it. The machine
  -- has no flow meter.
  , flowMlPerS :: Number
  , heaterWatts :: Int
  , fillsSinceBlowdown :: Int
  , blowdownDue :: Boolean
  , waterWarning :: Boolean
  , shot :: Maybe ShotNow
  -- How many shots have ended since power-on, and the last of them.
  , shotsEnded :: Int
  , lastShot :: Maybe ShotEnd
  -- The controller's newest faults, oldest first.
  , faults :: Array Fault
  }

-- | The status of a machine that is asleep and has done nothing. It is
-- | what the page shows before it has heard from the machine.
restingStatus :: Status
restingStatus =
  { requestedMode: Asleep
  , state: Sleeping
  , act: idleActuators
  , groupTargetC: 0.0
  , mixTargetC: Nothing
  , pressureTargetBar: Nothing
  , flowTargetMlPerS: Nothing
  , paceTrimBar: 0.0
  , flowMlPerS: 0.0
  , heaterWatts: 0
  , fillsSinceBlowdown: 0
  , blowdownDue: false
  , waterWarning: false
  , shot: Nothing
  , shotsEnded: 0
  , lastShot: Nothing
  , faults: []
  }

-- | One report from the machine: its clock, what its sensors read and
-- | its status, all at the same moment.
type Telemetry = { at :: Number, sensors :: Sensors, status :: Status }

-- | A request carries a number so its answer can be matched to it.
type Request = { id :: Int, event :: Event }

data Message
  = Report Telemetry
  -- The machine's answer to the request with this number: accepted, or
  -- refused with its reason.
  | Answer { id :: Int, refused :: Maybe String }

-- | One point of the shot chart.
type Sample =
  { t :: Number
  , bar :: Number
  , mlPerS :: Number
  , mixC :: Number
  , groupWaterC :: Number
  , targetC :: Number
  , grams :: Number
  -- The step the shot was in, and the pressure the controller was
  -- asking for, if it was asking for one.
  , step :: Int
  , askedBar :: Maybe Number
  -- How far pacing had moved the pressure setpoint.
  , paceBar :: Number
  }

-- | One point of the long, slow traces: the thermal state every couple
-- | of seconds.
type ScopeSample =
  { t :: Number
  , reservoirC :: Number
  , groupC :: Number
  , steamBar :: Number
  , heaterW :: Number
  }

-- | A line of the machine's log. `bad` marks the ones to show in red.
type Happening = { at :: Number, text :: String, bad :: Boolean }

-- | The controller reports a shot by its place in a profile. Whoever is
-- | listening knows which profile it last had accepted, and with that
-- | the report becomes a shot with its steps and names.
shotNow :: Profile -> ShotNow -> Shot
shotNow profile now =
  { profile
  , state: Running
  , stepIndex: now.stepIndex
  , step: fromMaybe (NEA.head profile.steps) (NEA.index profile.steps now.stepIndex)
  , startedAt: now.startedAt
  , stepStartedAt: now.stepStartedAt
  , endedAt: Nothing
  , finalGrams: Nothing
  }

shotEnded :: Profile -> ShotEnd -> Shot
shotEnded profile end =
  { profile
  , state: end.state
  , stepIndex: end.stepIndex
  , step: fromMaybe (NEA.head profile.steps) (NEA.index profile.steps end.stepIndex)
  , startedAt: end.startedAt
  , stepStartedAt: end.startedAt
  , endedAt: Just end.endedAt
  , finalGrams: end.finalGrams
  }
