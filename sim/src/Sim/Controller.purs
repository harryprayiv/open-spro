-- | The firmware's behaviour, written as pure functions so the same
-- | logic can be exercised here before any Rust exists.
-- |
-- | `handle` is the event side of spec/machine.allium: each case names
-- | the rule it implements and returns `Left` with the unmet `requires`
-- | clause when the rule does not apply.
-- |
-- | `tick` is everything the spec triggers on a condition becoming true
-- | or on time passing, plus the control laws the spec leaves open.
module Sim.Controller
  ( Ctl
  , Event(..)
  , initialCtl
  , handle
  , tick
  , targetsMet
  , targetsLost
  , heaterDrawWatts
  , shotInProgress
  ) where

import Prelude

import Data.Array (snoc)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Sim.Curve (shape)
import Sim.Types (Actuators, ClientKind(..), Config, Fault, FaultKind(..), GroupValve(..), MachineState(..), MixValve(..), PowerMode(..), Profile, PumpMode(..), Sensors, Shot, ShotState(..), Step, StepMode(..), Tuning, TwoWay(..), idleActuators)

type Ctl =
  { -- Limits and setpoints, and calibration and gains. These are the
    -- firmware's stored parameters.
    cfg :: Config
  , tune :: Tuning
  , requestedMode :: PowerMode
  , state :: MachineState
  , lastActivityAt :: Number
  , act :: Actuators
  , groupTargetC :: Number
  , mixTargetC :: Maybe Number
  , pressureTargetBar :: Maybe Number
  , flowTargetMlPerS :: Maybe Number
  -- Where each setpoint stood when the present step began.
  , rampFrom :: { bar :: Number, flow :: Number, tempC :: Number }
  -- How far pacing has moved the pressure setpoint, the smoothed flow
  -- into the cup it works from, and the last scale reading.
  , paceTrimBar :: Number
  , cupRateGPerS :: Number
  , lastCupGrams :: Number
  , fillStartedAt :: Maybe Number
  , fillsSinceBlowdown :: Int
  , blowdownDue :: Boolean
  , waterWarning :: Boolean
  , selected :: Profile
  -- At most one shot can be running because there is one slot for it.
  , shot :: Maybe Shot
  , history :: Array Shot
  , faults :: Array Fault
  , pumpIntegral :: Number
  , mixTrim :: Number
  }

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

initialCtl :: Config -> Tuning -> Profile -> Ctl
initialCtl cfg tune profile =
  { cfg
  , tune
  , requestedMode: Asleep
  , state: Sleeping
  , lastActivityAt: 0.0
  , act: idleActuators
  , groupTargetC: profile.groupBaseC
  , mixTargetC: Nothing
  , pressureTargetBar: Nothing
  , flowTargetMlPerS: Nothing
  , rampFrom: { bar: 0.0, flow: 0.0, tempC: profile.groupBaseC }
  , paceTrimBar: 0.0
  , cupRateGPerS: 0.0
  , lastCupGrams: 0.0
  , fillStartedAt: Nothing
  , fillsSinceBlowdown: 0
  , blowdownDue: false
  , waterWarning: false
  , selected: profile
  , shot: Nothing
  , history: []
  , faults: []
  , pumpIntegral: 0.0
  , mixTrim: 0.0
  }

------------------------------------------------------------
-- Derived values of the spec
------------------------------------------------------------

shotInProgress :: Ctl -> Boolean
shotInProgress c = isJust c.shot

awake :: Ctl -> Boolean
awake c = c.state == Heating || c.state == Ready

inletOk :: Sensors -> Ctl -> Boolean
inletOk s c = s.inletBar >= c.cfg.minInletBar

brewSideReady :: Sensors -> Ctl -> Boolean
brewSideReady s c =
  let
    within target value =
      value >= target - c.cfg.readyBandC && value <= target + c.cfg.readyBandC
  in
    within c.cfg.reservoirTargetC s.brewBoilerC && within c.groupTargetC s.groupC

-- | The condition for becoming ready. The steam side counts as ready a
-- | little below its setpoint, because the element switches at the
-- | setpoint and the pressure hovers just under it.
targetsMet :: Sensors -> Ctl -> Boolean
targetsMet s c =
  brewSideReady s c
    && (c.requestedMode == BrewReady || s.steamBar >= c.cfg.steamTargetBar - c.cfg.steamReadyBandBar)

-- | The condition for ceasing to be ready. It uses wider bands than
-- | `targetsMet`, so a reading that hovers at the edge of the ready band
-- | does not flip the machine in and out of ready.
targetsLost :: Sensors -> Ctl -> Boolean
targetsLost s c =
  let
    outside target value =
      value < target - c.cfg.dropOutBandC || value > target + c.cfg.dropOutBandC
  in
    outside c.cfg.reservoirTargetC s.brewBoilerC
      || outside c.groupTargetC s.groupC
      || (c.requestedMode == Full && s.steamBar < c.cfg.steamTargetBar - c.cfg.steamDropOutBandBar)

needsFill :: Sensors -> Ctl -> Boolean
needsFill s c = not s.steamLevelOk && c.requestedMode == Full

overLimit :: Sensors -> Ctl -> Boolean
overLimit s c =
  s.brewBoilerC > c.cfg.brewLimitC
    || s.groupC > c.cfg.groupLimitC
    || s.steamBar > c.cfg.steamLimitBar

overPressure :: Sensors -> Ctl -> Boolean
overPressure s c = s.brewBar > c.cfg.brewPressureLimitBar

heaterDrawWatts :: Config -> Actuators -> Int
heaterDrawWatts cfg a =
  (if a.brewElementOn then cfg.brewElementWatts else 0)
    + (if a.steamElementOn then cfg.steamElementWatts else 0)
    + (if a.groupHeaterOn then cfg.groupHeaterWatts else 0)

-- | Flow into the group as the firmware estimates it, from pump speed
-- | and slip, or from the needle valve when the bypass is feeding.
flowEstimate :: Sensors -> Ctl -> Number
flowEstimate s c =
  if c.act.bypassValve == Open then max 0.0 (s.inletBar - s.brewBar) * c.tune.needleMlPerSBar
  else case c.act.pump of
    Stopped -> 0.0
    _ -> max 0.0 (c.act.pumpSpeed * c.tune.pumpMaxMlPerS - c.tune.pumpSlipMlPerSBar * s.brewBar)

------------------------------------------------------------
-- Shared effects
------------------------------------------------------------

-- | Pump stopped, bypass closed, group vented, mix valve parked.
hydraulicsIdle :: Actuators -> Actuators
hydraulicsIdle a = a
  { pump = Stopped
  , pumpSpeed = 0.0
  , bypassValve = Closed
  , groupValve = ToDrain
  , mixValve = Parked
  , mixPosition = 0.0
  }

-- | rule EnterFault
enterFault :: Number -> FaultKind -> Ctl -> Ctl
enterFault now kind c =
  let
    aborted = c.shot <#> \shot -> shot { state = Aborted, endedAt = Just now }
  in
    c
      { state = Faulted
      , act = idleActuators
      , faults = snoc c.faults { kind, raisedAt: now, clearedAt: Nothing }
      , shot = Nothing
      , history = case aborted of
          Just shot -> snoc c.history shot
          Nothing -> c.history
      , mixTargetC = Nothing
      , pressureTargetBar = Nothing
      , flowTargetMlPerS = Nothing
      , fillStartedAt = Nothing
      , pumpIntegral = 0.0
      , mixTrim = 0.0
      , paceTrimBar = 0.0
      }

-- | rule ApplySleep
applySleep :: Ctl -> Ctl
applySleep c = c
  { requestedMode = Asleep
  , state = Sleeping
  , act = idleActuators
  , mixTargetC = Nothing
  , pressureTargetBar = Nothing
  , flowTargetMlPerS = Nothing
  , fillStartedAt = Nothing
  }

-- | rule EndShot
endShot :: Number -> Sensors -> ShotState -> Shot -> Ctl -> Ctl
endShot now s outcome shot c =
  let
    finished = shot
      { state = outcome
      , endedAt = Just now
      , finalGrams = Just s.cupGrams
      }
  in
    c
      { shot = Nothing
      , history = snoc c.history finished
      , act = hydraulicsIdle c.act
      , mixTargetC = Nothing
      , pressureTargetBar = Nothing
      , flowTargetMlPerS = Nothing
      , lastActivityAt = now
      , pumpIntegral = 0.0
      , mixTrim = 0.0
      , paceTrimBar = 0.0
      }

-- | rules EnterLinePrebrew, EnterPressureStep and EnterFlowStep. The
-- | hydraulic configuration changes at once; the setpoints start from
-- | where they stood and are moved by `followRamp`.
enterStep :: Sensors -> Step -> Ctl -> Ctl
enterStep s step c =
  let
    from =
      { bar: fromMaybe s.brewBar c.pressureTargetBar
      , flow: fromMaybe (flowEstimate s c) c.flowTargetMlPerS
      , tempC: fromMaybe step.targetC c.mixTargetC
      }
    act = case step.mode of
      LinePrebrew -> c.act { pump = Stopped, pumpSpeed = 0.0, bypassValve = Open }
      Pressure -> c.act { bypassValve = Closed, pump = PressureControl }
      Flow -> c.act { bypassValve = Closed, pump = FlowControl }
  in
    c { act = act, rampFrom = from }

-- | rules SetStepTemperature and RampFinished, and the path between
-- | them: each setpoint travels from its value at step entry to the
-- | step's target along the step's curve, then holds.
followRamp :: Number -> Shot -> Ctl -> Ctl
followRamp now shot c =
  let
    step = shot.step
    fraction =
      if step.rampS <= 0.0 then 1.0
      else shape step.curve ((now - shot.stepStartedAt) / step.rampS)
    blend from to = from + (to - from) * fraction
  in
    c
      { mixTargetC = Just (blend c.rampFrom.tempC step.targetC)
      , pressureTargetBar = case step.mode of
          -- Pacing moves the setpoint, never past a bar short of the limit.
          Pressure ->
            Just
              ( clamp 0.5 (c.cfg.brewPressureLimitBar - 1.0)
                  (blend c.rampFrom.bar (fromMaybe 9.0 step.targetBar) + c.paceTrimBar)
              )
          _ -> Nothing
      , flowTargetMlPerS = case step.mode of
          Flow -> Just (blend c.rampFrom.flow (fromMaybe 2.0 step.targetMlPerS))
          _ -> Nothing
      }

-- | rules AdvanceStep and LastStepFinished
finishStep :: Number -> Sensors -> Shot -> Ctl -> Ctl
finishStep now s shot c =
  case NEA.index shot.profile.steps (shot.stepIndex + 1) of
    Just next ->
      let
        advanced = shot
          { stepIndex = shot.stepIndex + 1
          , step = next
          , stepStartedAt = now
          }
      in
        followRamp now advanced (enterStep s next c { shot = Just advanced })
    Nothing -> endShot now s StoppedByTime shot c

------------------------------------------------------------
-- Events
------------------------------------------------------------

handle :: Number -> Sensors -> Event -> Ctl -> Either String Ctl
handle now s event c = case event of

  -- rules Wake and ChangeMode
  WakeRequested mode
    | mode == Asleep -> Left "wake needs brew_ready or full"
    | c.state == Faulted -> Left "machine is faulted"
    | c.state == Sleeping ->
        Right c
          { requestedMode = mode
          , state = Heating
          , act = c.act { heatEnabled = true }
          , lastActivityAt = now
          }
    | otherwise ->
        Right c
          { requestedMode = mode
          , lastActivityAt = now
          , act =
              if mode == Full then c.act
              else c.act { steamElementOn = false, fillValve = Closed }
          , fillStartedAt = if mode == Full then c.fillStartedAt else Nothing
          }

  -- rule Sleep
  SleepRequested
    | not (awake c) -> Left "machine is not awake"
    | shotInProgress c -> Left "a shot is running"
    | otherwise -> Right (applySleep c)

  -- rule SelectProfile
  ProfileSelected profile
    | c.state == Faulted -> Left "machine is faulted"
    | shotInProgress c -> Left "a shot is running"
    | otherwise ->
        Right c
          { selected = profile
          , groupTargetC = profile.groupBaseC
          , lastActivityAt = now
          }

  -- rule StartShot
  ShotRequested
    | c.state /= Ready -> Left "machine is not ready"
    | targetsLost s c -> Left "temperatures have drifted out of range"
    | c.groupTargetC /= c.selected.groupBaseC -> Left "group is set for another profile"
    | s.brewBoilerC < c.selected.maxTargetC + c.cfg.reservoirHeadroomC ->
        Left "reservoir is not hot enough for this profile"
    | c.selected.maxTargetC > c.cfg.mixLimitC -> Left "profile asks for water above the mix limit"
    | shotInProgress c -> Left "a shot is already running"
    | c.act.fillValve == Open -> Left "steam boiler is filling"
    | not (inletOk s c) -> Left "no inlet pressure"
    | otherwise ->
        let
          first = NEA.head c.selected.steps
          shot =
            { profile: c.selected
            , state: Running
            , stepIndex: 0
            , step: first
            , startedAt: now
            , stepStartedAt: now
            , endedAt: Nothing
            , finalGrams: Nothing
            }
        in
          Right
            ( followRamp now shot
                ( enterStep s first c
                    { shot = Just shot
                    , lastActivityAt = now
                    , mixTargetC = Nothing
                    , paceTrimBar = 0.0
                    , cupRateGPerS = 0.0
                    , lastCupGrams = 0.0
                    , act = c.act { groupValve = ToGroup, mixValve = Regulating }
                    }
                )
            )

  -- rule AdvanceByUser
  StepAdvanceRequested -> case c.shot of
    Nothing -> Left "no shot is running"
    Just shot -> Right (finishStep now s shot c)

  -- rule StopByUser
  ShotStopRequested -> case c.shot of
    Nothing -> Left "no shot is running"
    Just shot -> Right (endShot now s StoppedByUser shot c)

  -- rule ClearFaults
  FaultsCleared client
    | c.state /= Faulted -> Left "machine is not faulted"
    | client /= Service -> Left "only the service panel clears faults"
    | overLimit s c -> Left "a temperature or steam pressure is still over its limit"
    | overPressure s c -> Left "brew pressure is still over its limit"
    | otherwise ->
        Right c
          { faults = c.faults <#> \f -> case f.clearedAt of
              Nothing -> f { clearedAt = Just now }
              Just _ -> f
          , requestedMode = Asleep
          , state = Sleeping
          }

  -- rule BlowdownDone
  BlowdownConfirmed
    | c.state /= Sleeping -> Left "blowdown is confirmed with the machine asleep"
    | otherwise -> Right c { fillsSinceBlowdown = 0, blowdownDue = false }

  -- rule WaterWarningCleared
  WaterWarningAcknowledged
    | s.inletConductivityUs > c.cfg.maxInletConductivityUs -> Left "inlet water is still out of range"
    | otherwise -> Right c { waterWarning = false }

  -- rule SensorFault
  SensorFailed kind
    | c.state == Faulted -> Left "machine is already faulted"
    | kind == SensorOpen || kind == SensorShort || kind == ScaleLost || kind == Watchdog ->
        Right (enterFault now kind c)
    | otherwise -> Left "not a sensor failure kind"

------------------------------------------------------------
-- Periodic work
------------------------------------------------------------

-- | One controller cycle. `dt` is the cycle time in seconds.
tick :: Number -> Number -> Sensors -> Ctl -> Ctl
tick dt now s c0 =
  if c0.state == Faulted then c0
  else case detectFault now s c0 of
    Just kind -> enterFault now kind c0
    Nothing ->
      c0
        # trackReadiness now s
        # idleSleep now
        # waterQuality s
        # steamFill now s
        # runShot dt now s
        # allocateHeaters now s

-- | rules OverLimit, OverPressure, MixTooHot, InletLostDuringShot and
-- | FillTimedOut
detectFault :: Number -> Sensors -> Ctl -> Maybe FaultKind
detectFault now s c
  | overLimit s c = Just OverTemperature
  | overPressure s c = Just OverPressure
  | shotInProgress c && s.mixC > c.cfg.mixLimitC = Just MixOverTemperature
  | shotInProgress c && not (inletOk s c) = Just NoInletPressure
  | otherwise = case c.fillStartedAt of
      Just started | c.act.fillValve == Open && started + c.cfg.fillTimeout <= now -> Just FillTimeout
      _ -> Nothing

-- | rules BecomeReady and FallBackToHeating. Becoming ready restarts
-- | the idle clock.
trackReadiness :: Number -> Sensors -> Ctl -> Ctl
trackReadiness now s c
  | not (awake c) = c
  | shotInProgress c = c
  | c.state == Ready = if targetsLost s c then c { state = Heating } else c
  | targetsMet s c = c { state = Ready, lastActivityAt = now }
  | otherwise = c

-- | rule IdleSleep
idleSleep :: Number -> Ctl -> Ctl
idleSleep now c
  | awake c && not (shotInProgress c) && c.lastActivityAt + c.cfg.idleSleepAfter <= now = applySleep c
  | otherwise = c

-- | rules HardWaterSeen and BlowdownBecomesDue
waterQuality :: Sensors -> Ctl -> Ctl
waterQuality s c = c
  { waterWarning = c.waterWarning || s.inletConductivityUs > c.cfg.maxInletConductivityUs
  , blowdownDue = c.blowdownDue || c.fillsSinceBlowdown >= c.cfg.blowdownAfterFills
  }

-- | rules FillNeeded, StartFill, RetryFillWhenInletReturns and FinishFill
steamFill :: Number -> Sensors -> Ctl -> Ctl
steamFill now s c
  | c.act.fillValve == Open && s.steamLevelOk =
      c
        { act = c.act { fillValve = Closed }
        , fillStartedAt = Nothing
        , fillsSinceBlowdown = c.fillsSinceBlowdown + 1
        }
  | c.act.fillValve == Closed && awake c && needsFill s c && inletOk s c && not (shotInProgress c) =
      c
        { act = c.act { fillValve = Open, steamElementOn = false }
        , fillStartedAt = Just now
        }
  | otherwise = c

-- | Step exits, shot stops and the pump and mix control laws.
runShot :: Number -> Number -> Sensors -> Ctl -> Ctl
runShot dt now s c = case c.shot of
  Nothing -> c
  Just shot
    -- rule StopOnWeight
    | s.cupGrams >= shot.profile.targetGrams -> endShot now s StoppedByWeight shot c
    -- rule StopOnTimeout
    | shot.startedAt + c.cfg.shotTimeout <= now -> endShot now s StoppedByTime shot c
    -- rules StepExitsOnTime, StepExitsOnPressure and StepExitsOnWeight
    | shot.stepStartedAt + shot.step.maxDuration <= now -> finishStep now s shot c
    | reached shot.step.exitAboveBar s.brewBar -> finishStep now s shot c
    | reached shot.step.exitAboveGrams s.cupGrams -> finishStep now s shot c
    | otherwise -> c # paceLaw dt now s shot # followRamp now shot # pumpLaw dt s shot.step # mixLaw dt now s shot

reached :: Maybe Number -> Number -> Boolean
reached limit value = case limit of
  Just l -> value >= l
  Nothing -> false

-- | rule PaceShot. With a target time on the profile, the machine works
-- | out the flow into the cup that would land the target weight on
-- | time, compares it with the flow the scale shows, and moves the
-- | pressure setpoint to close the gap. It acts only in pressure steps,
-- | only once coffee is reaching the cup, and only within the pacing
-- | limit. A slow puck gets more pressure and a fast one gets less.
paceLaw :: Number -> Number -> Sensors -> Shot -> Ctl -> Ctl
paceLaw dt now s shot c =
  let
    raw = (s.cupGrams - c.lastCupGrams) / dt
    rate = c.cupRateGPerS + (raw - c.cupRateGPerS) * min 1.0 (dt / max 0.05 c.tune.paceFilterS)
    measured = c { cupRateGPerS = rate, lastCupGrams = s.cupGrams }
    limit = c.cfg.paceLimitBar
  in
    case shot.profile.targetSeconds of
      Just seconds | shot.step.mode == Pressure && s.cupGrams >= c.tune.paceStartGrams ->
        let
          gramsLeft = max 0.0 (shot.profile.targetGrams - s.cupGrams)
          secondsLeft = seconds - (now - shot.startedAt)
          move wanted =
            measured { paceTrimBar = clamp (-limit) limit (c.paceTrimBar + c.tune.paceGain * (wanted - rate) * dt) }
        in
          -- With time in hand, aim to land the rest of the weight on time.
          -- In the last 3 s the wanted flow swings wildly with every gram,
          -- so the trim holds. Past the target time the shot is late, and
          -- the pressure keeps rising towards the limit until it ends.
          if secondsLeft >= 3.0 then move (gramsLeft / secondsLeft)
          else if secondsLeft < 0.0 then move (rate + 0.5)
          else measured
      _ -> measured { paceTrimBar = clamp (-limit) limit c.paceTrimBar }

-- | Pressure steps: proportional plus integral on brew pressure, with
-- | the integral frozen until pressure starts to build so filling the
-- | dry puck does not wind it up. Flow steps: pump speed from the target
-- | flow plus the slip expected at the present pressure.
pumpLaw :: Number -> Sensors -> Step -> Ctl -> Ctl
pumpLaw dt s step c = case step.mode of
  LinePrebrew -> c
  Pressure ->
    let
      err = fromMaybe 9.0 c.pressureTargetBar - s.brewBar
      integral =
        if s.brewBar > c.tune.pumpIntegralAboveBar then
          clamp 0.0 c.tune.pumpIntegralLimit (c.pumpIntegral + c.tune.pumpKi * err * dt)
        else c.pumpIntegral
      speed = clamp 0.0 1.0 (c.tune.pumpKp * err + integral)
    in
      c { pumpIntegral = integral, act = c.act { pumpSpeed = speed } }
  Flow ->
    let
      target = fromMaybe 2.0 c.flowTargetMlPerS
      speed = clamp 0.0 1.0 ((target + c.tune.pumpSlipMlPerSBar * s.brewBar) / c.tune.pumpMaxMlPerS)
    in
      c { act = c.act { pumpSpeed = speed } }

-- | Feed-forward valve position from the three temperatures, trimmed by
-- | the mix sensor. The trim is slow and bounded to a share of the
-- | feed-forward position, so a stale sensor reading cannot close the
-- | cold branch. It freezes for the first moments of a shot, while the
-- | sensor still reads the water that sat in the manifold, and below a
-- | flow threshold, where the sensor sits in nearly still water.
mixLaw :: Number -> Number -> Sensors -> Shot -> Ctl -> Ctl
mixLaw dt now s shot c =
  let
    target = fromMaybe shot.step.targetC c.mixTargetC
    span = s.brewBoilerC - s.inletC
    coldFraction =
      if span <= 1.0 then 0.0
      else clamp 0.0 0.66 ((s.brewBoilerC - target) / span)
    feedForward = coldFraction / (c.tune.coldBranchMaxRatio * (1.0 - coldFraction))
    trimLimit = c.tune.mixTrimShare * feedForward + 0.02
    trim =
      if flowEstimate s c > c.tune.mixMinFlowMlPerS && now - shot.startedAt > c.tune.mixTrimDelayS then
        clamp (-trimLimit) trimLimit (c.mixTrim + c.tune.mixTrimGain * (s.mixC - target) * dt)
      else c.mixTrim
  in
    c
      { mixTrim = trim
      , act = c.act { mixPosition = clamp 0.0 1.0 (feedForward + trim) }
      }

data Heater = BrewElement | SteamElement | GroupHeater

-- | The spec bounds the heater total with the CircuitBudget invariant
-- | and leaves the allocation open. This one gives the brew reservoir
-- | first call during a shot and otherwise alternates turns between the
-- | reservoir and the steam boiler.
allocateHeaters :: Number -> Sensors -> Ctl -> Ctl
allocateHeaters now s c =
  if not (awake c) then c
  else
    let
      wants = case _ of
        BrewElement -> s.brewBoilerC < c.cfg.reservoirTargetC
        GroupHeater -> s.groupC < c.groupTargetC
        SteamElement ->
          c.requestedMode == Full
            && s.steamLevelOk
            && c.act.fillValve == Closed
            && s.steamBar < c.cfg.steamTargetBar
      watts = case _ of
        BrewElement -> c.cfg.brewElementWatts
        GroupHeater -> c.cfg.groupHeaterWatts
        SteamElement -> c.cfg.steamElementWatts
      steamTurn = floor (now / max 0.1 c.tune.heaterTurnS) `mod` 2 == 0
      order
        | shotInProgress c = [ BrewElement, GroupHeater, SteamElement ]
        | steamTurn = [ GroupHeater, SteamElement, BrewElement ]
        | otherwise = [ GroupHeater, BrewElement, SteamElement ]
      off = c.act { brewElementOn = false, steamElementOn = false, groupHeaterOn = false }
      grant acc heater =
        if wants heater && acc.used + watts heater <= c.cfg.circuitBudgetWatts then
          { used: acc.used + watts heater
          , act: case heater of
              BrewElement -> acc.act { brewElementOn = true }
              GroupHeater -> acc.act { groupHeaterOn = true }
              SteamElement -> acc.act { steamElementOn = true }
          }
        else acc
    in
      c { act = (foldl grant { used: 0, act: off } order).act }
