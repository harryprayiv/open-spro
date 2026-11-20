-- | Every number that shapes how the machine behaves, in one table.
-- |
-- | There are four groups. `Physics` describes the simulated machine
-- | itself: thermal masses, heat losses, element power, the pump and the
-- | puck. On the real machine these are facts to be measured, and this
-- | group has no counterpart in firmware. The other three groups are
-- | what the firmware stores: limits and setpoints, the calibration of
-- | its own hardware, and the gains of its control loops.
-- |
-- | Each entry carries its own range, step and unit, so the settings
-- | panel, the stored text and any future firmware parameter list are
-- | all generated from this one table.
module Sim.Params
  ( Params
  , Group(..)
  , Entry
  , defaults
  , groups
  , groupTitle
  , groupNote
  , entries
  , inGroup
  , decimals
  , Reading
  , readings
  , encode
  , decode
  ) where

import Prelude

import Data.Array (filter, find)
import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Number.Format (fixed, toStringWith)
import Data.String (Pattern(..), joinWith, split)
import Sim.Plant (PlantConstants, defaultConstants)
import Sim.Types (Config, Tuning, defaultConfig, defaultTuning)

type Params =
  { plant :: PlantConstants
  , config :: Config
  , tuning :: Tuning
  }

defaults :: Params
defaults =
  { plant: defaultConstants
  , config: defaultConfig
  , tuning: defaultTuning
  }

data Group = Physics | Limits | Calibration | Loops

derive instance eqGroup :: Eq Group

groups :: Array Group
groups = [ Loops, Calibration, Limits, Physics ]

groupTitle :: Group -> String
groupTitle = case _ of
  Physics -> "Physics of the simulated machine"
  Limits -> "Limits and setpoints"
  Calibration -> "Calibration"
  Loops -> "Control loops"

groupNote :: Group -> String
groupNote = case _ of
  Physics -> "Simulation only. On the real machine these are measured, and matching them to the measurements is how the simulator is calibrated."
  Limits -> "Stored in firmware. These are the config block of the spec."
  Calibration -> "Stored in firmware. What the controller believes about its own pump and branches; it works best when these match the physics."
  Loops -> "Stored in firmware. Gains and timings, tuned against the scope."

type Entry =
  { key :: String
  , group :: Group
  , label :: String
  , unit :: String
  , min :: Number
  , max :: Number
  , step :: Number
  , get :: Params -> Number
  , set :: Number -> Params -> Params
  }

-- | How many decimals an entry's value is shown with.
decimals :: Entry -> Int
decimals entry
  | entry.step >= 1.0 = 0
  | entry.step >= 0.1 = 1
  | entry.step >= 0.01 = 2
  | otherwise = 3

inGroup :: Group -> Array Entry
inGroup group = filter (\entry -> entry.group == group) entries

physics
  :: String
  -> String
  -> String
  -> Number
  -> Number
  -> Number
  -> (PlantConstants -> Number)
  -> (Number -> PlantConstants -> PlantConstants)
  -> Entry
physics key label unit lo hi step get set =
  { key, group: Physics, label, unit, min: lo, max: hi, step
  , get: get <<< _.plant
  , set: \value p -> p { plant = set value p.plant }
  }

limit
  :: String
  -> String
  -> String
  -> Number
  -> Number
  -> Number
  -> (Config -> Number)
  -> (Number -> Config -> Config)
  -> Entry
limit key label unit lo hi step get set =
  { key, group: Limits, label, unit, min: lo, max: hi, step
  , get: get <<< _.config
  , set: \value p -> p { config = set value p.config }
  }

tuned
  :: Group
  -> String
  -> String
  -> String
  -> Number
  -> Number
  -> Number
  -> (Tuning -> Number)
  -> (Number -> Tuning -> Tuning)
  -> Entry
tuned group key label unit lo hi step get set =
  { key, group, label, unit, min: lo, max: hi, step
  , get: get <<< _.tuning
  , set: \value p -> p { tuning = set value p.tuning }
  }

entries :: Array Entry
entries =
  [ -- control loops
    tuned Loops "pumpKp" "Pressure loop, proportional" "speed per bar" 0.0 0.5 0.005 _.pumpKp (\v t -> t { pumpKp = v })
  , tuned Loops "pumpKi" "Pressure loop, integral" "speed per bar s" 0.0 0.5 0.005 _.pumpKi (\v t -> t { pumpKi = v })
  , tuned Loops "pumpIntegralLimit" "Pressure loop, integral ceiling" "speed" 0.0 1.0 0.05 _.pumpIntegralLimit (\v t -> t { pumpIntegralLimit = v })
  , tuned Loops "pumpIntegralAboveBar" "Pressure loop, integrate above" "bar" 0.0 5.0 0.1 _.pumpIntegralAboveBar (\v t -> t { pumpIntegralAboveBar = v })
  , tuned Loops "mixTrimGain" "Mix loop, trim gain" "position per C s" 0.0 0.05 0.001 _.mixTrimGain (\v t -> t { mixTrimGain = v })
  , tuned Loops "mixTrimShare" "Mix loop, trim limit as share of feed-forward" "" 0.0 1.0 0.05 _.mixTrimShare (\v t -> t { mixTrimShare = v })
  , tuned Loops "mixTrimDelayS" "Mix loop, trim waits after shot start" "s" 0.0 10.0 0.1 _.mixTrimDelayS (\v t -> t { mixTrimDelayS = v })
  , tuned Loops "mixMinFlowMlPerS" "Mix loop, trim frozen below flow" "ml/s" 0.0 2.0 0.05 _.mixMinFlowMlPerS (\v t -> t { mixMinFlowMlPerS = v })
  , tuned Loops "paceGain" "Pacing, gain" "bar per g/s s" 0.0 3.0 0.05 _.paceGain (\v t -> t { paceGain = v })
  , tuned Loops "paceFilterS" "Pacing, smoothing of the cup flow" "s" 0.2 6.0 0.1 _.paceFilterS (\v t -> t { paceFilterS = v })
  , tuned Loops "paceStartGrams" "Pacing, starts once the cup holds" "g" 0.5 15.0 0.5 _.paceStartGrams (\v t -> t { paceStartGrams = v })
  , tuned Loops "heaterTurnS" "Heater sharing, length of a turn" "s" 0.2 20.0 0.1 _.heaterTurnS (\v t -> t { heaterTurnS = v })

  -- calibration
  , tuned Calibration "calPumpMax" "Pump flow at full speed" "ml/s" 1.0 30.0 0.1 _.pumpMaxMlPerS (\v t -> t { pumpMaxMlPerS = v })
  , tuned Calibration "calPumpSlip" "Pump slip" "ml/s per bar" 0.0 1.0 0.01 _.pumpSlipMlPerSBar (\v t -> t { pumpSlipMlPerSBar = v })
  , tuned Calibration "calNeedle" "Needle valve conductance" "ml/s per bar" 0.05 3.0 0.05 _.needleMlPerSBar (\v t -> t { needleMlPerSBar = v })
  , tuned Calibration "calColdRatio" "Cold to hot flow with mix valve fully open" "" 0.2 6.0 0.1 _.coldBranchMaxRatio (\v t -> t { coldBranchMaxRatio = v })

  -- limits and setpoints
  , limit "circuitBudgetWatts" "Circuit budget" "W" 500.0 3600.0 10.0 (toNumber <<< _.circuitBudgetWatts) (\v c -> c { circuitBudgetWatts = round v })
  , limit "brewElementWatts" "Brew element rating" "W" 100.0 2000.0 10.0 (toNumber <<< _.brewElementWatts) (\v c -> c { brewElementWatts = round v })
  , limit "steamElementWatts" "Steam element rating" "W" 100.0 3000.0 10.0 (toNumber <<< _.steamElementWatts) (\v c -> c { steamElementWatts = round v })
  , limit "groupHeaterWatts" "Group heater rating" "W" 20.0 500.0 5.0 (toNumber <<< _.groupHeaterWatts) (\v c -> c { groupHeaterWatts = round v })
  , limit "reservoirTargetC" "Reservoir setpoint" "C" 90.0 115.0 0.5 _.reservoirTargetC (\v c -> c { reservoirTargetC = v })
  , limit "reservoirHeadroomC" "Reservoir headroom over hottest step" "C" 1.0 15.0 0.5 _.reservoirHeadroomC (\v c -> c { reservoirHeadroomC = v })
  , limit "steamTargetBar" "Steam setpoint" "bar" 0.8 2.2 0.05 _.steamTargetBar (\v c -> c { steamTargetBar = v })
  , limit "readyBandC" "Ready band" "C" 0.1 3.0 0.1 _.readyBandC (\v c -> c { readyBandC = v })
  , limit "dropOutBandC" "Drop-out band" "C" 0.2 6.0 0.1 _.dropOutBandC (\v c -> c { dropOutBandC = v })
  , limit "steamReadyBandBar" "Steam ready band" "bar" 0.0 0.5 0.01 _.steamReadyBandBar (\v c -> c { steamReadyBandBar = v })
  , limit "steamDropOutBandBar" "Steam drop-out band" "bar" 0.0 1.0 0.01 _.steamDropOutBandBar (\v c -> c { steamDropOutBandBar = v })
  , limit "brewLimitC" "Reservoir limit" "C" 105.0 130.0 0.5 _.brewLimitC (\v c -> c { brewLimitC = v })
  , limit "mixLimitC" "Mix outlet limit" "C" 90.0 105.0 0.5 _.mixLimitC (\v c -> c { mixLimitC = v })
  , limit "groupLimitC" "Group limit" "C" 95.0 130.0 0.5 _.groupLimitC (\v c -> c { groupLimitC = v })
  , limit "steamLimitBar" "Steam limit" "bar" 1.0 3.0 0.05 _.steamLimitBar (\v c -> c { steamLimitBar = v })
  , limit "brewPressureLimitBar" "Brew pressure limit" "bar" 6.0 12.0 0.1 _.brewPressureLimitBar (\v c -> c { brewPressureLimitBar = v })
  , limit "paceLimitBar" "Pacing may move pressure by" "bar" 0.0 4.0 0.1 _.paceLimitBar (\v c -> c { paceLimitBar = v })
  , limit "minInletBar" "Least inlet pressure" "bar" 0.2 3.0 0.1 _.minInletBar (\v c -> c { minInletBar = v })
  , limit "maxInletConductivityUs" "Most inlet conductivity" "uS/cm" 50.0 1000.0 10.0 _.maxInletConductivityUs (\v c -> c { maxInletConductivityUs = v })
  , limit "fillTimeout" "Fill timeout" "s" 10.0 600.0 5.0 _.fillTimeout (\v c -> c { fillTimeout = v })
  , limit "shotTimeout" "Shot timeout" "s" 20.0 300.0 5.0 _.shotTimeout (\v c -> c { shotTimeout = v })
  , limit "idleSleepAfter" "Idle sleep after" "s" 60.0 14400.0 60.0 _.idleSleepAfter (\v c -> c { idleSleepAfter = v })
  , limit "blowdownAfterFills" "Blowdown after fills" "fills" 10.0 2000.0 10.0 (toNumber <<< _.blowdownAfterFills) (\v c -> c { blowdownAfterFills = round v })

  -- physics
  , physics "ambientC" "Room temperature" "C" 5.0 40.0 0.5 _.ambientC (\v k -> k { ambientC = v })
  , physics "inletC" "Inlet water temperature" "C" 2.0 35.0 0.5 _.inletC (\v k -> k { inletC = v })
  , physics "regulatedLineBar" "Regulated line pressure" "bar" 0.5 6.0 0.1 _.regulatedLineBar (\v k -> k { regulatedLineBar = v })
  , physics "reservoirJPerK" "Reservoir heat capacity" "J/K" 500.0 20000.0 50.0 _.reservoirJPerK (\v k -> k { reservoirJPerK = v })
  , physics "reservoirLossWPerK" "Reservoir heat loss" "W/K" 0.1 10.0 0.1 _.reservoirLossWPerK (\v k -> k { reservoirLossWPerK = v })
  , physics "brewElementW" "Brew element power" "W" 100.0 2000.0 10.0 _.brewElementW (\v k -> k { brewElementW = v })
  , physics "groupJPerK" "Group block heat capacity" "J/K" 100.0 10000.0 50.0 _.groupJPerK (\v k -> k { groupJPerK = v })
  , physics "groupLossWPerK" "Group block heat loss" "W/K" 0.1 10.0 0.1 _.groupLossWPerK (\v k -> k { groupLossWPerK = v })
  , physics "groupHeaterW" "Group heater power" "W" 20.0 500.0 5.0 _.groupHeaterW (\v k -> k { groupHeaterW = v })
  , physics "steamMetalJPerK" "Steam boiler metal heat capacity" "J/K" 100.0 5000.0 50.0 _.steamMetalJPerK (\v k -> k { steamMetalJPerK = v })
  , physics "steamLossWPerK" "Steam boiler heat loss" "W/K" 0.1 15.0 0.1 _.steamLossWPerK (\v k -> k { steamLossWPerK = v })
  , physics "steamElementW" "Steam element power" "W" 100.0 3000.0 10.0 _.steamElementW (\v k -> k { steamElementW = v })
  , physics "steamProbeMl" "Steam boiler water at the level probe" "ml" 500.0 4000.0 50.0 _.steamProbeMl (\v k -> k { steamProbeMl = v })
  , physics "steamProbeHysteresisMl" "Level probe hysteresis" "ml" 5.0 300.0 5.0 _.steamProbeHysteresisMl (\v k -> k { steamProbeHysteresisMl = v })
  , physics "steamFillMlPerSBar" "Steam fill conductance" "ml/s per bar" 1.0 60.0 1.0 _.steamFillMlPerSBar (\v k -> k { steamFillMlPerSBar = v })
  , physics "steamDrawGPerS" "Steam drawn by the wand at 1.8 bar" "g/s" 0.1 4.0 0.1 _.steamDrawGPerS (\v k -> k { steamDrawGPerS = v })
  , physics "pumpMaxMlPerS" "Pump flow at full speed" "ml/s" 1.0 30.0 0.1 _.pumpMaxMlPerS (\v k -> k { pumpMaxMlPerS = v })
  , physics "pumpSlipMlPerSBar" "Pump slip" "ml/s per bar" 0.0 1.0 0.01 _.pumpSlipMlPerSBar (\v k -> k { pumpSlipMlPerSBar = v })
  , physics "needleMlPerSBar" "Needle valve conductance" "ml/s per bar" 0.05 3.0 0.05 _.needleMlPerSBar (\v k -> k { needleMlPerSBar = v })
  , physics "hydraulicComplianceMlPerBar" "Hydraulic compliance" "ml/bar" 0.05 5.0 0.05 _.hydraulicComplianceMlPerBar (\v k -> k { hydraulicComplianceMlPerBar = v })
  , physics "expansionValveBar" "Expansion valve opens at" "bar" 9.0 16.0 0.1 _.expansionValveBar (\v k -> k { expansionValveBar = v })
  , physics "coldBranchMaxRatio" "Cold to hot flow with mix valve fully open" "" 0.2 6.0 0.1 _.coldBranchMaxRatio (\v k -> k { coldBranchMaxRatio = v })
  , physics "mixVolumeMl" "Mix manifold volume" "ml" 0.2 10.0 0.1 _.mixVolumeMl (\v k -> k { mixVolumeMl = v })
  , physics "insertVolumeMl" "Group insert volume" "ml" 0.5 20.0 0.5 _.insertVolumeMl (\v k -> k { insertVolumeMl = v })
  , physics "mixSensorLagS" "Mix sensor lag" "s" 0.05 5.0 0.05 _.mixSensorLagS (\v k -> k { mixSensorLagS = v })
  , physics "referenceDoseG" "Dose the puck figures refer to" "g" 7.0 25.0 0.5 _.referenceDoseG (\v k -> k { referenceDoseG = v })
  , physics "puckFillMl" "Water a dry puck and headspace take up" "ml" 5.0 60.0 1.0 _.puckFillMl (\v k -> k { puckFillMl = v })
  , physics "puckFillMlPerSBar" "Puck conductance while filling" "ml/s per bar" 1.0 10.0 0.5 _.puckFillMlPerSBar (\v k -> k { puckFillMlPerSBar = v })
  , physics "puckResistanceBarSPerMl" "Wet puck resistance" "bar s/ml" 0.5 15.0 0.1 _.puckResistanceBarSPerMl (\v k -> k { puckResistanceBarSPerMl = v })
  , physics "extractionPerMl" "Extraction per ml through the puck" "share per ml" 0.005 0.1 0.001 _.extractionPerMl (\v k -> k { extractionPerMl = v })
  , physics "extractionPerC" "Extraction change per degree of the puck" "share per C" 0.0 0.15 0.005 _.extractionPerC (\v k -> k { extractionPerC = v })
  , physics "soakPerS" "Extraction into water held in the puck" "share per s" 0.0 0.05 0.001 _.soakPerS (\v k -> k { soakPerS = v })
  , physics "soakCapacity" "Most the held water can carry" "share" 0.02 0.5 0.01 _.soakCapacity (\v k -> k { soakCapacity = v })
  , physics "coffeeJPerGK" "Heat capacity of dry coffee" "J/g K" 0.5 4.0 0.1 _.coffeeJPerGK (\v k -> k { coffeeJPerGK = v })
  , physics "puckCouplingWPerK" "Heat passed between basket and puck" "W/K" 0.0 10.0 0.1 _.puckCouplingWPerK (\v k -> k { puckCouplingWPerK = v })
  , physics "gentleFillMlPerS" "Fastest fill a dry puck takes without harm" "ml/s" 0.5 10.0 0.1 _.gentleFillMlPerS (\v k -> k { gentleFillMlPerS = v })
  , physics "fillHarmPerMl" "Evenness lost to a faster fill" "share per ml" 0.0 0.1 0.001 _.fillHarmPerMl (\v k -> k { fillHarmPerMl = v })
  , physics "gentleRampBarPerS" "Steepest ramp an unsettled puck takes without harm" "bar/s" 0.1 10.0 0.1 _.gentleRampBarPerS (\v k -> k { gentleRampBarPerS = v })
  , physics "rampHarmPerBar" "Evenness lost to a steeper ramp" "share per bar" 0.0 0.5 0.01 _.rampHarmPerBar (\v k -> k { rampHarmPerBar = v })
  , physics "puckSwellS" "Time the wet grounds take to settle" "s" 0.5 30.0 0.5 _.puckSwellS (\v k -> k { puckSwellS = v })
  , physics "channelFlowGain" "Resistance a fully channeled puck loses" "share" 0.0 0.9 0.05 _.channelFlowGain (\v k -> k { channelFlowGain = v })
  , physics "channelExtractionLoss" "Extraction a fully channeled puck loses" "share" 0.0 1.0 0.05 _.channelExtractionLoss (\v k -> k { channelExtractionLoss = v })
  ]

-- | A figure worked out from the parameters, for the settings panel.
type Reading = { label :: String, value :: String }

-- | What the present physics implies: how fast each mass cools when
-- | left alone, and how fast its heater can drive it. These are the
-- | curves a scope would show on the real machine.
readings :: Params -> Array Reading
readings p =
  let
    k = p.plant
    n1 = toStringWith (fixed 1)
    minutes joulesPerK wattsPerK = n1 (joulesPerK / wattsPerK / 60.0) <> " min"
    perMinute watts joulesPerK = n1 (watts / joulesPerK * 60.0) <> " C/min"
    steamJPerK = k.steamProbeMl * 4.186 + k.steamMetalJPerK
    holdWatts lossWPerK target = n1 (lossWPerK * (target - k.ambientC)) <> " W"
  in
    [ { label: "Reservoir cooling time constant", value: minutes k.reservoirJPerK k.reservoirLossWPerK }
    , { label: "Reservoir heating rate, element full on", value: perMinute k.brewElementW k.reservoirJPerK }
    , { label: "Reservoir power to hold setpoint", value: holdWatts k.reservoirLossWPerK p.config.reservoirTargetC }
    , { label: "Group block cooling time constant", value: minutes k.groupJPerK k.groupLossWPerK }
    , { label: "Group block heating rate, heaters full on", value: perMinute k.groupHeaterW k.groupJPerK }
    , { label: "Steam boiler cooling time constant", value: minutes steamJPerK k.steamLossWPerK }
    , { label: "Steam boiler heating rate, element full on", value: perMinute k.steamElementW steamJPerK }
    , { label: "Mix sensor lag", value: toStringWith (fixed 2) k.mixSensorLagS <> " s" }
    , { label: "Transport delay, mix point to screen at 2 ml/s", value: n1 ((k.mixVolumeMl + k.insertVolumeMl) / 2.0) <> " s" }
    ]

-- | One `key=value` per line. Only values that differ from the defaults
-- | are written, so stored text stays valid when defaults change.
encode :: Params -> String
encode p =
  joinWith "\n"
    ( map (\entry -> entry.key <> "=" <> show (entry.get p))
        (filter (\entry -> entry.get p /= entry.get defaults) entries)
    )

-- | Unknown keys and unreadable numbers are ignored, and every value is
-- | clamped to its entry's range.
decode :: String -> Params
decode text = foldl apply defaults (split (Pattern "\n") text)
  where
  apply p line = case split (Pattern "=") line of
    [ key, raw ] -> case find (\entry -> entry.key == key) entries, Number.fromString raw of
      Just entry, Just value -> entry.set (clamp entry.min entry.max value) p
      _, _ -> p
    _ -> p
