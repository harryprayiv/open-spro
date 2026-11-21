-- | The machine's controller, as the page and the simulated plant see
-- | it. The controller itself is the Rust crate in firmware/spro-core,
-- | the same code the microcontroller runs, loaded here as wasm through
-- | `Sim.Core`. This module holds none of its logic. It turns events,
-- | sensors and parameters into the core's flat messages, and turns the
-- | status the core sends back into the record the page reads.
-- |
-- | A `Ctl` belongs to the simulated machine. The page does not hold
-- | one. It hears the `status` of one in reports that arrive over the
-- | link, late and not always.
module Sim.Controller
  ( Ctl
  , module Sim.Link
  , status
  , initialCtl
  , setParams
  , handle
  , tick
  , heaterDrawWatts
  , shotInProgress
  ) where

import Prelude

import Data.Array (concatMap, index, length, range, snoc)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (isNaN, nan)
import Sim.Core as Core
import Sim.Link (Event(..))
import Sim.Link (ShotEnd, Status, shotEnded, shotNow) as Link
import Sim.Types (Actuators, ClientKind(..), Config, Curve(..), Fault, FaultKind(..), GroupValve(..), MachineState(..), MixValve(..), PowerMode(..), Profile, PumpMode(..), Sensors, Shot, ShotState(..), Step, StepMode(..), Tuning, TwoWay(..))

type Ctl =
  { -- The parameters the core was last given.
    cfg :: Config
  , tune :: Tuning
  -- The controller's own memory. Only the core reads it.
  , core :: Core.State
  -- What the controller reports about itself, decoded from its status.
  , requestedMode :: PowerMode
  , state :: MachineState
  , act :: Actuators
  , groupTargetC :: Number
  , mixTargetC :: Maybe Number
  , pressureTargetBar :: Maybe Number
  , flowTargetMlPerS :: Maybe Number
  -- How far pacing has moved the pressure setpoint.
  , paceTrimBar :: Number
  -- Flow into the group as the controller estimates it.
  , flowEstimate :: Number
  , fillsSinceBlowdown :: Int
  , blowdownDue :: Boolean
  , waterWarning :: Boolean
  -- The running shot, if there is one, and the controller's newest
  -- faults, oldest first.
  , shot :: Maybe Shot
  , faults :: Array Fault
  -- What the simulated machine keeps beside the controller, for the
  -- scenarios and the forecast: the profile it last accepted and a log
  -- of ended shots. The controller reports only how many shots have
  -- ended and the last of them.
  , selected :: Profile
  , history :: Array Shot
  , shotsEnded :: Int
  , lastShot :: Maybe Link.ShotEnd
  }

-- | What the controller reports about itself, as it goes out over the
-- | link. Every field came from the core's status message.
status :: Ctl -> Link.Status
status c =
  { requestedMode: c.requestedMode
  , state: c.state
  , act: c.act
  , groupTargetC: c.groupTargetC
  , mixTargetC: c.mixTargetC
  , pressureTargetBar: c.pressureTargetBar
  , flowTargetMlPerS: c.flowTargetMlPerS
  , paceTrimBar: c.paceTrimBar
  , flowMlPerS: c.flowEstimate
  , heaterWatts: heaterDrawWatts c.cfg c.act
  , fillsSinceBlowdown: c.fillsSinceBlowdown
  , blowdownDue: c.blowdownDue
  , waterWarning: c.waterWarning
  , shot: c.shot <#> \shot -> { stepIndex: shot.stepIndex, startedAt: shot.startedAt, stepStartedAt: shot.stepStartedAt }
  , shotsEnded: c.shotsEnded
  , lastShot: c.lastShot
  , faults: c.faults
  }

------------------------------------------------------------
-- Messages to the core. The order of the fields is fixed by
-- firmware/spro-core/src/core_wire.rs.
------------------------------------------------------------

opInit :: Int
opInit = 0

opSetParams :: Int
opSetParams = 1

opTick :: Int
opTick = 2

opEvent :: Int
opEvent = 3

flag :: Boolean -> Number
flag b = if b then 1.0 else 0.0

-- | A value that may be missing travels as NaN when it is.
optional :: Maybe Number -> Number
optional = fromMaybe nan

sensorsWire :: Sensors -> Array Number
sensorsWire s =
  [ s.inletBar
  , s.inletConductivityUs
  , s.inletC
  , s.brewBar
  , s.brewBoilerC
  , s.mixC
  , s.groupC
  , s.groupWaterC
  , s.steamBar
  , flag s.steamLevelOk
  , s.cupGrams
  ]

configWire :: Config -> Array Number
configWire c =
  [ toNumber c.circuitBudgetWatts
  , toNumber c.brewElementWatts
  , toNumber c.steamElementWatts
  , toNumber c.groupHeaterWatts
  , c.reservoirTargetC
  , c.reservoirHeadroomC
  , c.steamTargetBar
  , c.readyBandC
  , c.dropOutBandC
  , c.steamReadyBandBar
  , c.steamDropOutBandBar
  , c.brewLimitC
  , c.mixLimitC
  , c.groupLimitC
  , c.steamLimitBar
  , c.brewPressureLimitBar
  , c.paceLimitBar
  , c.minInletBar
  , c.maxInletConductivityUs
  , c.fillTimeout
  , c.shotTimeout
  , c.idleSleepAfter
  , toNumber c.blowdownAfterFills
  ]

tuningWire :: Tuning -> Array Number
tuningWire t =
  [ t.pumpMaxMlPerS
  , t.pumpSlipMlPerSBar
  , t.needleMlPerSBar
  , t.coldBranchMaxRatio
  , t.pumpKp
  , t.pumpKi
  , t.pumpIntegralLimit
  , t.pumpIntegralAboveBar
  , t.mixTrimGain
  , t.mixTrimShare
  , t.mixTrimDelayS
  , t.mixMinFlowMlPerS
  , t.paceGain
  , t.paceFilterS
  , t.paceStartGrams
  , t.heaterTurnS
  ]

stepWire :: Step -> Array Number
stepWire s =
  [ case s.mode of
      LinePrebrew -> 0.0
      Pressure -> 1.0
      Flow -> 2.0
  , case s.curve of
      Jump -> 0.0
      Linear -> 1.0
      Ease -> 2.0
      Exponential -> 3.0
  , s.rampS
  , s.targetC
  , optional s.targetBar
  , optional s.targetMlPerS
  , s.maxDuration
  , optional s.exitAboveBar
  , optional s.exitAboveGrams
  ]

profileWire :: Profile -> Array Number
profileWire p =
  [ p.targetGrams
  , p.groupBaseC
  , p.maxTargetC
  , optional p.targetSeconds
  , toNumber (NEA.length p.steps)
  ]
    <> concatMap stepWire (NEA.toArray p.steps)

powerModeWire :: PowerMode -> Number
powerModeWire = case _ of
  Asleep -> 0.0
  BrewReady -> 1.0
  Full -> 2.0

clientWire :: ClientKind -> Number
clientWire = case _ of
  Paddle -> 0.0
  Remote -> 1.0
  Service -> 2.0
  Supervisor -> 3.0

faultKinds :: Array FaultKind
faultKinds =
  [ SensorOpen, SensorShort, OverTemperature, OverPressure, FillTimeout, NoInletPressure, ScaleLost, Watchdog, MixOverTemperature ]

faultKindWire :: FaultKind -> Number
faultKindWire = case _ of
  SensorOpen -> 0.0
  SensorShort -> 1.0
  OverTemperature -> 2.0
  OverPressure -> 3.0
  FillTimeout -> 4.0
  NoInletPressure -> 5.0
  ScaleLost -> 6.0
  Watchdog -> 7.0
  MixOverTemperature -> 8.0

-- | An event's number and whatever it carries.
eventWire :: Event -> { code :: Number, carries :: Array Number }
eventWire = case _ of
  WakeRequested mode -> { code: 0.0, carries: [ powerModeWire mode ] }
  SleepRequested -> { code: 1.0, carries: [] }
  ProfileSelected profile -> { code: 2.0, carries: profileWire profile }
  ShotRequested -> { code: 3.0, carries: [] }
  StepAdvanceRequested -> { code: 4.0, carries: [] }
  ShotStopRequested -> { code: 5.0, carries: [] }
  FaultsCleared client -> { code: 6.0, carries: [ clientWire client ] }
  BlowdownConfirmed -> { code: 7.0, carries: [] }
  WaterWarningAcknowledged -> { code: 8.0, carries: [] }
  SensorFailed kind -> { code: 9.0, carries: [ faultKindWire kind ] }

------------------------------------------------------------
-- The status the core sends back
------------------------------------------------------------

-- | The fixed part of a status is this long, and each fault after it
-- | takes three numbers.
statusHead :: Int
statusHead = 34

-- | Read the core's reply into the record. The numbers are in the order
-- | `write_status` puts them in.
absorb :: Core.Reply -> Ctl -> Ctl
absorb reply c =
  let
    at i = fromMaybe 0.0 (index reply.status i)
    on i = at i /= 0.0
    present i = if isNaN (at i) then Nothing else Just (at i)
    pick :: forall a. a -> Array a -> Int -> a
    pick fallback choices i = fromMaybe fallback (index choices (round (at i)))
    shot =
      if on 21 then Just (Link.shotNow c.selected { stepIndex: round (at 22), startedAt: at 23, stepStartedAt: at 24 })
      else Nothing
    ended = round (at 25)
    lastShot =
      if ended == 0 then Nothing
      else
        Just
          { state: pick Aborted [ Running, StoppedByWeight, StoppedByTime, StoppedByUser, Aborted ] 26
          , stepIndex: round (at 30)
          , startedAt: at 27
          , endedAt: at 28
          , finalGrams: present 29
          }
    -- A shot has ended since the last status: add it to the log.
    history = case lastShot of
      Just end | ended /= c.shotsEnded -> snoc c.history (Link.shotEnded c.selected end)
      _ -> c.history
    fault i =
      let
        base = statusHead + 3 * i
      in
        { kind: pick Watchdog faultKinds base
        , raisedAt: at (base + 1)
        , clearedAt: present (base + 2)
        }
    faultCount = round (at 32)
  in
    c
      { core = reply.state
      , requestedMode = pick Asleep [ Asleep, BrewReady, Full ] 0
      , state = pick Faulted [ Sleeping, Heating, Ready, Faulted ] 1
      , act =
          { heatEnabled: on 2
          , brewElementOn: on 3
          , steamElementOn: on 4
          , groupHeaterOn: on 5
          , pump: pick Stopped [ Stopped, PressureControl, FlowControl ] 6
          , pumpSpeed: at 7
          , bypassValve: pick Closed [ Closed, Open ] 8
          , fillValve: pick Closed [ Closed, Open ] 9
          , groupValve: pick ToDrain [ ToDrain, ToGroup ] 10
          , mixValve: pick Parked [ Parked, Regulating ] 11
          , mixPosition: at 12
          }
      , groupTargetC = at 13
      , mixTargetC = present 14
      , pressureTargetBar = present 15
      , flowTargetMlPerS = present 16
      , paceTrimBar = at 17
      , flowEstimate = at 33
      , fillsSinceBlowdown = round (at 18)
      , blowdownDue = on 19
      , waterWarning = on 20
      , shot = shot
      , history = history
      , shotsEnded = ended
      , lastShot = lastShot
      , faults = if faultCount <= 0 then [] else map fault (range 0 (faultCount - 1))
      }

------------------------------------------------------------
-- The controller
------------------------------------------------------------

-- | A sleeping machine with the given parameters and profile.
initialCtl :: Config -> Tuning -> Profile -> Ctl
initialCtl cfg tune profile =
  absorb (Core.call Core.noState opInit 0.0 0.0 (configWire cfg <> tuningWire tune <> profileWire profile))
    { cfg
    , tune
    , core: Core.noState
    , requestedMode: Asleep
    , state: Sleeping
    , act:
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
    , groupTargetC: profile.groupBaseC
    , mixTargetC: Nothing
    , pressureTargetBar: Nothing
    , flowTargetMlPerS: Nothing
    , paceTrimBar: 0.0
    , flowEstimate: 0.0
    , fillsSinceBlowdown: 0
    , blowdownDue: false
    , waterWarning: false
    , shot: Nothing
    , faults: []
    , selected: profile
    , history: []
    , shotsEnded: 0
    , lastShot: Nothing
    }

-- | Give the controller new limits, setpoints, calibration and gains.
-- | Its temperatures, its pressures and a shot in progress carry on
-- | from where they are.
setParams :: Config -> Tuning -> Ctl -> Ctl
setParams cfg tune c =
  absorb (Core.call c.core opSetParams 0.0 0.0 (configWire cfg <> tuningWire tune)) c { cfg = cfg, tune = tune }

-- | Deliver an event. `Left` carries the controller's reason for
-- | refusing it, and then nothing has changed.
handle :: Number -> Sensors -> Event -> Ctl -> Either String Ctl
handle now s event c =
  let
    wire = eventWire event
    message = sensorsWire s <> wire.carries
    accepted = case event of
      ProfileSelected profile -> c { selected = profile }
      _ -> c
  in
    -- A message longer than the core's buffer is never sent.
    if length message > Core.bufferLen then Left "the profile has too many steps to send"
    else
      let
        reply = Core.call c.core opEvent wire.code now message
      in
        if reply.code /= 0 then Left (Core.rejectText reply.code)
        else Right (absorb reply accepted)

-- | One controller cycle. `dt` is the cycle time in seconds.
tick :: Number -> Number -> Sensors -> Ctl -> Ctl
tick dt now s c = absorb (Core.call c.core opTick dt now (sensorsWire s)) c

shotInProgress :: Ctl -> Boolean
shotInProgress c = isJust c.shot

-- | What the heaters that are on draw between them. This is worked out
-- | here, apart from the controller, so the CircuitBudget invariant is
-- | checked by code the controller did not write.
heaterDrawWatts :: Config -> Actuators -> Int
heaterDrawWatts cfg a =
  (if a.brewElementOn then cfg.brewElementWatts else 0)
    + (if a.steamElementOn then cfg.steamElementWatts else 0)
    + (if a.groupHeaterOn then cfg.groupHeaterWatts else 0)
