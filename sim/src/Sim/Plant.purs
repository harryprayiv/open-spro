-- | The pretend machine: a lumped thermal and hydraulic model that the
-- | controller drives through `Actuators` and observes through `Sensors`.
-- | Every constant here is a guess to be replaced by bench measurements.
module Sim.Plant
  ( Plant
  , Disturbance
  , PlantConstants
  , constants
  , initialPlant
  , noDisturbance
  , stepPlant
  , readSensors
  , freshPuck
  , warmPlant
  ) where

import Prelude

import Data.Number (pow)
import Sim.Types (Actuators, GroupValve(..), MixValve(..), PumpMode(..), Sensors, TwoWay(..))

-- | Things the operator of the simulation can do to the plant that the
-- | controller does not command.
type Disturbance =
  { inletOpen :: Boolean
  , coldBranchBlocked :: Boolean
  , steamValveOpen :: Boolean
  , hardWater :: Boolean
  }

noDisturbance :: Disturbance
noDisturbance =
  { inletOpen: true
  , coldBranchBlocked: false
  , steamValveOpen: false
  , hardWater: false
  }

type Plant =
  { reservoirC :: Number
  , groupBlockC :: Number
  , steamC :: Number
  , steamWaterMl :: Number
  , steamLevelOk :: Boolean
  , lineBar :: Number
  , inletC :: Number
  , brewBar :: Number
  , mixTrueC :: Number
  , mixSensorC :: Number
  , groupWaterC :: Number
  , puckDoseG :: Number
  , puckWetMl :: Number
  , cupGrams :: Number
  , flowMlPerS :: Number
  }

type PlantConstants =
  { ambientC :: Number
  , regulatedLineBar :: Number
  , pumpMaxMlPerS :: Number
  , pumpSlipMlPerSBar :: Number
  , needleMlPerSBar :: Number
  , hydraulicComplianceMlPerBar :: Number
  , expansionValveBar :: Number
  , referenceDoseG :: Number
  , puckFillMl :: Number
  , puckFillMlPerSBar :: Number
  , puckResistanceBarSPerMl :: Number
  , coldBranchMaxRatio :: Number
  , mixVolumeMl :: Number
  , insertVolumeMl :: Number
  , mixSensorLagS :: Number
  , reservoirJPerK :: Number
  , reservoirLossWPerK :: Number
  , brewElementW :: Number
  , groupJPerK :: Number
  , groupLossWPerK :: Number
  , groupHeaterW :: Number
  , steamMetalJPerK :: Number
  , steamLossWPerK :: Number
  , steamElementW :: Number
  , steamProbeMl :: Number
  , steamProbeHysteresisMl :: Number
  , steamFillMlPerSBar :: Number
  , steamDrawGPerS :: Number
  }

constants :: PlantConstants
constants =
  { ambientC: 22.0
  , regulatedLineBar: 3.0
  , pumpMaxMlPerS: 10.0
  , pumpSlipMlPerSBar: 0.15
  , needleMlPerSBar: 0.6
  , hydraulicComplianceMlPerBar: 0.5
  , expansionValveBar: 12.0
  , referenceDoseG: 18.0
  , puckFillMl: 22.0
  , puckFillMlPerSBar: 6.0
  , puckResistanceBarSPerMl: 4.2
  , coldBranchMaxRatio: 2.0
  , mixVolumeMl: 1.0
  , insertVolumeMl: 4.0
  , mixSensorLagS: 0.4
  , reservoirJPerK: 3650.0
  , reservoirLossWPerK: 1.2
  , brewElementW: 600.0
  , groupJPerK: 1250.0
  , groupLossWPerK: 0.9
  , groupHeaterW: 150.0
  , steamMetalJPerK: 800.0
  , steamLossWPerK: 2.0
  , steamElementW: 1000.0
  , steamProbeMl: 1500.0
  , steamProbeHysteresisMl: 50.0
  , steamFillMlPerSBar: 15.0
  , steamDrawGPerS: 1.0
  }

initialPlant :: Plant
initialPlant =
  { reservoirC: constants.ambientC
  , groupBlockC: constants.ambientC
  , steamC: constants.ambientC
  , steamWaterMl: 1350.0
  , steamLevelOk: false
  , lineBar: constants.regulatedLineBar
  , inletC: 15.0
  , brewBar: 0.0
  , mixTrueC: constants.ambientC
  , mixSensorC: constants.ambientC
  , groupWaterC: constants.ambientC
  , puckDoseG: constants.referenceDoseG
  , puckWetMl: 0.0
  , cupGrams: 0.0
  , flowMlPerS: 0.0
  }

-- | A new dry puck of the given dose in the group and an empty cup on
-- | the scale.
freshPuck :: Number -> Plant -> Plant
freshPuck doseG p = p { puckDoseG = doseG, puckWetMl = 0.0, cupGrams = 0.0 }

-- | Jump the plant to a heated state so a session does not have to
-- | start with a twenty minute warm-up.
warmPlant :: Number -> Plant -> Plant
warmPlant groupTargetC p = p
  { reservoirC = 104.0
  , groupBlockC = groupTargetC
  , steamC = 133.0
  , steamWaterMl = constants.steamProbeMl
  , steamLevelOk = true
  , mixTrueC = groupTargetC
  , mixSensorC = groupTargetC
  , groupWaterC = groupTargetC
  }

clamp01 :: Number -> Number
clamp01 = clamp 0.0 1.0

-- | Gauge pressure of saturated steam at a temperature, from the Antoine
-- | equation for water.
saturationGaugeBar :: Number -> Number
saturationGaugeBar tC =
  let
    mmHg =
      if tC < 100.0 then pow 10.0 (8.07131 - 1730.63 / (233.426 + tC))
      else pow 10.0 (8.14019 - 1810.94 / (244.485 + tC))
  in
    max 0.0 (mmHg / 750.06 - 1.013)

stepPlant :: Number -> Disturbance -> Actuators -> Plant -> Plant
stepPlant dt d a p =
  let
    k = constants

    -- inlet
    lineTarget = if d.inletOpen then k.regulatedLineBar else 0.0
    lineBar = p.lineBar + (lineTarget - p.lineBar) * clamp01 (dt / 0.5)
    hasWater = lineBar > 0.2

    -- pressure stage
    toGroup = a.groupValve == ToGroup
    qPump = case a.pump of
      Stopped -> 0.0
      _ ->
        if hasWater then max 0.0 (a.pumpSpeed * k.pumpMaxMlPerS - k.pumpSlipMlPerSBar * p.brewBar)
        else 0.0
    qBypass =
      if a.bypassValve == Open then max 0.0 (lineBar - p.brewBar) * k.needleMlPerSBar
      else 0.0
    qSource = qPump + qBypass

    -- puck: a bigger dose soaks up more water and resists more
    doseScale = p.puckDoseG / k.referenceDoseG
    fillMl = k.puckFillMl * doseScale
    saturated = p.puckWetMl >= fillMl
    resistance = k.puckResistanceBarSPerMl * doseScale * (1.0 - 0.35 * clamp01 (p.cupGrams / 45.0))
    qPuck =
      if not toGroup then 0.0
      else if saturated then p.brewBar / resistance
      else p.brewBar * k.puckFillMlPerSBar
    brewBarRaw =
      if toGroup then p.brewBar + (qSource - qPuck) * dt / k.hydraulicComplianceMlPerBar
      else p.brewBar * (1.0 - clamp01 (dt / 0.15)) + qSource * dt / k.hydraulicComplianceMlPerBar
    brewBar = clamp 0.0 k.expansionValveBar brewBarRaw
    qGroup = if toGroup then qSource else 0.0
    puckWetMl = if saturated then p.puckWetMl else min fillMl (p.puckWetMl + qPuck * dt)
    cupGrams = if saturated then p.cupGrams + qPuck * dt else p.cupGrams

    -- temperature stage
    coldRatio =
      if d.coldBranchBlocked || a.mixValve == Parked then 0.0
      else clamp01 a.mixPosition * k.coldBranchMaxRatio
    coldFraction = coldRatio / (coldRatio + 1.0)
    blendC = coldFraction * p.inletC + (1.0 - coldFraction) * p.reservoirC
    flowing = qGroup > 0.02
    mixTrueC =
      if flowing then p.mixTrueC + (blendC - p.mixTrueC) * clamp01 (qGroup * dt / k.mixVolumeMl)
      else p.mixTrueC + (p.groupBlockC - p.mixTrueC) * clamp01 (dt / 20.0)
    mixSensorC = p.mixSensorC + (mixTrueC - p.mixSensorC) * clamp01 (dt / k.mixSensorLagS)
    groupWaterC =
      p.groupWaterC
        + (mixTrueC - p.groupWaterC) * clamp01 (qGroup * dt / k.insertVolumeMl)
        + (p.groupBlockC - p.groupWaterC) * clamp01 (dt * 0.08)
    qHot = qGroup * (1.0 - coldFraction)

    -- brew reservoir
    brewPowerW = if a.heatEnabled && a.brewElementOn then k.brewElementW else 0.0
    reservoirC =
      p.reservoirC
        +
          ( brewPowerW
              - k.reservoirLossWPerK * (p.reservoirC - k.ambientC)
              - qHot * 4.186 * (p.reservoirC - p.inletC)
          )
            * dt
            / k.reservoirJPerK

    -- group block
    groupPowerW = if a.groupHeaterOn then k.groupHeaterW else 0.0
    groupBlockC =
      p.groupBlockC
        + (groupPowerW - k.groupLossWPerK * (p.groupBlockC - k.ambientC)) * dt / k.groupJPerK

    -- steam boiler
    steamBarNow = saturationGaugeBar p.steamC
    steamPowerW = if a.heatEnabled && a.steamElementOn then k.steamElementW else 0.0
    drawGPerS =
      if d.steamValveOpen && steamBarNow > 0.2 then k.steamDrawGPerS * steamBarNow / 1.8
      else 0.0
    fillMlPerS =
      if a.fillValve == Open then max 0.0 (lineBar - steamBarNow) * k.steamFillMlPerSBar
      else 0.0
    steamJPerK = p.steamWaterMl * 4.186 + k.steamMetalJPerK
    heatedC =
      p.steamC
        +
          ( steamPowerW
              - k.steamLossWPerK * (p.steamC - k.ambientC)
              - drawGPerS * 2257.0
          )
            * dt
            / steamJPerK
    addedJPerK = fillMlPerS * dt * 4.186
    steamC = (heatedC * steamJPerK + p.inletC * addedJPerK) / (steamJPerK + addedJPerK)
    steamWaterMl = max 0.0 (p.steamWaterMl + (fillMlPerS - drawGPerS) * dt)
    steamLevelOk =
      if steamWaterMl >= k.steamProbeMl then true
      else if steamWaterMl < k.steamProbeMl - k.steamProbeHysteresisMl then false
      else p.steamLevelOk
  in
    { reservoirC
    , groupBlockC
    , steamC
    , steamWaterMl
    , steamLevelOk
    , lineBar
    , inletC: p.inletC
    , brewBar
    , mixTrueC
    , mixSensorC
    , groupWaterC
    , puckDoseG: p.puckDoseG
    , puckWetMl
    , cupGrams
    , flowMlPerS: qGroup
    }

readSensors :: Disturbance -> Plant -> Sensors
readSensors d p =
  { inletBar: p.lineBar
  , inletConductivityUs: if d.hardWater then 420.0 else 90.0
  , inletC: p.inletC
  , brewBar: p.brewBar
  , brewBoilerC: p.reservoirC
  , mixC: p.mixSensorC
  , groupC: p.groupBlockC
  , groupWaterC: p.groupWaterC
  , steamBar: saturationGaugeBar p.steamC
  , steamLevelOk: p.steamLevelOk
  , cupGrams: p.cupGrams
  }