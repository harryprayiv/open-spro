-- | Vocabulary shared by the simulated plant, the controller and the page.
-- | Names follow spec/machine.allium.
module Sim.Types
  ( PowerMode(..)
  , MachineState(..)
  , TwoWay(..)
  , GroupValve(..)
  , MixValve(..)
  , PumpMode(..)
  , StepMode(..)
  , ShotState(..)
  , FaultKind(..)
  , ClientKind(..)
  , Curve(..)
  , Step
  , Profile
  , Shot
  , Fault
  , Sensors
  , Actuators
  , Config
  , Tuning
  , defaultConfig
  , defaultTuning
  , idleActuators
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)

data PowerMode = Asleep | BrewReady | Full

derive instance eqPowerMode :: Eq PowerMode

instance showPowerMode :: Show PowerMode where
  show Asleep = "asleep"
  show BrewReady = "brew_ready"
  show Full = "full"

data MachineState = Sleeping | Heating | Ready | Faulted

derive instance eqMachineState :: Eq MachineState

instance showMachineState :: Show MachineState where
  show Sleeping = "sleeping"
  show Heating = "heating"
  show Ready = "ready"
  show Faulted = "faulted"

data TwoWay = Closed | Open

derive instance eqTwoWay :: Eq TwoWay

instance showTwoWay :: Show TwoWay where
  show Closed = "closed"
  show Open = "open"

data GroupValve = ToDrain | ToGroup

derive instance eqGroupValve :: Eq GroupValve

instance showGroupValve :: Show GroupValve where
  show ToDrain = "to_drain"
  show ToGroup = "to_group"

data MixValve = Parked | Regulating

derive instance eqMixValve :: Eq MixValve

instance showMixValve :: Show MixValve where
  show Parked = "parked"
  show Regulating = "regulating"

data PumpMode = Stopped | PressureControl | FlowControl

derive instance eqPumpMode :: Eq PumpMode

instance showPumpMode :: Show PumpMode where
  show Stopped = "stopped"
  show PressureControl = "pressure_control"
  show FlowControl = "flow_control"

data StepMode = LinePrebrew | Pressure | Flow

derive instance eqStepMode :: Eq StepMode

instance showStepMode :: Show StepMode where
  show LinePrebrew = "line_prebrew"
  show Pressure = "pressure"
  show Flow = "flow"

data ShotState
  = Running
  | StoppedByWeight
  | StoppedByTime
  | StoppedByUser
  | Aborted

derive instance eqShotState :: Eq ShotState

instance showShotState :: Show ShotState where
  show Running = "running"
  show StoppedByWeight = "stopped_by_weight"
  show StoppedByTime = "stopped_by_time"
  show StoppedByUser = "stopped_by_user"
  show Aborted = "aborted"

data FaultKind
  = SensorOpen
  | SensorShort
  | OverTemperature
  | OverPressure
  | FillTimeout
  | NoInletPressure
  | ScaleLost
  | Watchdog
  | MixOverTemperature

derive instance eqFaultKind :: Eq FaultKind

instance showFaultKind :: Show FaultKind where
  show SensorOpen = "sensor_open"
  show SensorShort = "sensor_short"
  show OverTemperature = "over_temperature"
  show OverPressure = "over_pressure"
  show FillTimeout = "fill_timeout"
  show NoInletPressure = "no_inlet_pressure"
  show ScaleLost = "scale_lost"
  show Watchdog = "watchdog"
  show MixOverTemperature = "mix_over_temperature"

data ClientKind = Paddle | Remote | Service | Supervisor

derive instance eqClientKind :: Eq ClientKind

instance showClientKind :: Show ClientKind where
  show Paddle = "paddle"
  show Remote = "remote"
  show Service = "service"
  show Supervisor = "supervisor"

-- | How a step's setpoints travel from where they were when the step
-- | began to the step's own targets.
data Curve = Jump | Linear | Ease | Exponential

derive instance eqCurve :: Eq Curve

instance showCurve :: Show Curve where
  show Jump = "jump"
  show Linear = "linear"
  show Ease = "ease"
  show Exponential = "exponential"

-- | One step of a profile. The spec links steps with `next`; here the
-- | order of the profile's array carries the same information.
type Step =
  { name :: String
  , mode :: StepMode
  , curve :: Curve
  -- Seconds the setpoints take to reach this step's targets.
  , rampS :: Number
  , targetC :: Number
  , targetBar :: Maybe Number
  , targetMlPerS :: Maybe Number
  , maxDuration :: Number
  , exitAboveBar :: Maybe Number
  , exitAboveGrams :: Maybe Number
  }

-- | A profile always has a first step, so the array is non-empty.
type Profile =
  { name :: String
  , targetGrams :: Number
  , groupBaseC :: Number
  , maxTargetC :: Number
  -- How long the shot should take to reach its weight. When present,
  -- the machine paces the shot towards it.
  , targetSeconds :: Maybe Number
  , steps :: NonEmptyArray Step
  }

type Shot =
  { profile :: Profile
  , state :: ShotState
  , stepIndex :: Int
  , step :: Step
  , startedAt :: Number
  , stepStartedAt :: Number
  , endedAt :: Maybe Number
  , finalGrams :: Maybe Number
  }

type Fault =
  { kind :: FaultKind
  , raisedAt :: Number
  , clearedAt :: Maybe Number
  }

-- | What the firmware can read.
type Sensors =
  { inletBar :: Number
  , inletConductivityUs :: Number
  , inletC :: Number
  , brewBar :: Number
  , brewBoilerC :: Number
  , mixC :: Number
  , groupC :: Number
  , groupWaterC :: Number
  , steamBar :: Number
  , steamLevelOk :: Boolean
  , cupGrams :: Number
  }

-- | What the firmware can drive. `pumpSpeed` and `mixPosition` run from
-- | 0 to 1 and only mean something in the matching pump and mix modes.
type Actuators =
  { heatEnabled :: Boolean
  , brewElementOn :: Boolean
  , steamElementOn :: Boolean
  , groupHeaterOn :: Boolean
  , pump :: PumpMode
  , pumpSpeed :: Number
  , bypassValve :: TwoWay
  , fillValve :: TwoWay
  , groupValve :: GroupValve
  , mixValve :: MixValve
  , mixPosition :: Number
  }

idleActuators :: Actuators
idleActuators =
  { heatEnabled: false
  , brewElementOn: false
  , steamElementOn: false
  , groupHeaterOn: false
  , pump: Stopped
  , pumpSpeed: 0.0
  , bypassValve: Closed
  , fillValve: Closed
  , groupValve: ToDrain
  , mixValve: Parked
  , mixPosition: 0.0
  }

-- | The config block of the spec. Durations are in seconds.
type Config =
  { circuitBudgetWatts :: Int
  , brewElementWatts :: Int
  , steamElementWatts :: Int
  , groupHeaterWatts :: Int
  , reservoirTargetC :: Number
  , reservoirHeadroomC :: Number
  , steamTargetBar :: Number
  , readyBandC :: Number
  , dropOutBandC :: Number
  , steamReadyBandBar :: Number
  , steamDropOutBandBar :: Number
  , brewLimitC :: Number
  , mixLimitC :: Number
  , groupLimitC :: Number
  , steamLimitBar :: Number
  , brewPressureLimitBar :: Number
  -- The most the machine may move a pressure setpoint, either way, to
  -- pace a shot towards its target time.
  , paceLimitBar :: Number
  , minInletBar :: Number
  , maxInletConductivityUs :: Number
  , fillTimeout :: Number
  , shotTimeout :: Number
  , idleSleepAfter :: Number
  , blowdownAfterFills :: Int
  }

defaultConfig :: Config
defaultConfig =
  { circuitBudgetWatts: 1440
  , brewElementWatts: 600
  , steamElementWatts: 1000
  , groupHeaterWatts: 150
  , reservoirTargetC: 104.0
  , reservoirHeadroomC: 6.0
  , steamTargetBar: 1.8
  , readyBandC: 0.3
  , dropOutBandC: 0.8
  , steamReadyBandBar: 0.15
  , steamDropOutBandBar: 0.3
  , brewLimitC: 118.0
  , mixLimitC: 99.0
  , groupLimitC: 110.0
  , steamLimitBar: 2.3
  , brewPressureLimitBar: 11.0
  , paceLimitBar: 3.0
  , minInletBar: 1.5
  , maxInletConductivityUs: 250.0
  , fillTimeout: 90.0
  , shotTimeout: 120.0
  , idleSleepAfter: 2700.0
  , blowdownAfterFills: 200
  }

-- | What the firmware knows about its own hardware and how hard its
-- | loops push. On the real machine the calibration half is measured on
-- | the bench and the gain half is tuned against a scope.
type Tuning =
  { -- calibration
    pumpMaxMlPerS :: Number
  , pumpSlipMlPerSBar :: Number
  , needleMlPerSBar :: Number
  , coldBranchMaxRatio :: Number
  -- pressure loop
  , pumpKp :: Number
  , pumpKi :: Number
  , pumpIntegralLimit :: Number
  , pumpIntegralAboveBar :: Number
  -- mix loop
  , mixTrimGain :: Number
  , mixTrimShare :: Number
  , mixTrimDelayS :: Number
  , mixMinFlowMlPerS :: Number
  -- pacing a shot towards its target time
  , paceGain :: Number
  , paceFilterS :: Number
  , paceStartGrams :: Number
  -- heater sharing
  , heaterTurnS :: Number
  }

defaultTuning :: Tuning
defaultTuning =
  { pumpMaxMlPerS: 10.0
  , pumpSlipMlPerSBar: 0.15
  , needleMlPerSBar: 0.6
  , coldBranchMaxRatio: 2.0
  , pumpKp: 0.06
  , pumpKi: 0.05
  , pumpIntegralLimit: 0.6
  , pumpIntegralAboveBar: 1.0
  , mixTrimGain: 0.004
  , mixTrimShare: 0.5
  , mixTrimDelayS: 1.5
  , mixMinFlowMlPerS: 0.3
  , paceGain: 0.6
  , paceFilterS: 1.0
  , paceStartGrams: 6.0
  , heaterTurnS: 2.0
  }
