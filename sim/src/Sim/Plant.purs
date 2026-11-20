-- | The pretend machine: a lumped thermal and hydraulic model that the
-- | controller drives through `Actuators` and observes through `Sensors`.
-- | Every constant here is a guess to be replaced by bench measurements.
module Sim.Plant
  ( Plant
  , Disturbance
  , PlantConstants
  , defaultConstants
  , initialPlant
  , noDisturbance
  , stepPlant
  , readSensors
  , Puck
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
  { -- The physical constants of this particular machine.
    k :: PlantConstants
  , reservoirC :: Number
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
  , puckResistance :: Number
  , puckSolubility :: Number
  , puckWetMl :: Number
  -- The share of what the coffee can give up that has left the puck.
  , extracted :: Number
  , cupGrams :: Number
  , flowMlPerS :: Number
  }

type PlantConstants =
  { ambientC :: Number
  , inletC :: Number
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
  , extractionPerMl :: Number
  , extractionPerC :: Number
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

defaultConstants :: PlantConstants
defaultConstants =
  { ambientC: 22.0
  , inletC: 15.0
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
  , extractionPerMl: 0.032
  , extractionPerC: 0.04
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

-- | A cold machine built with the given constants.
initialPlant :: PlantConstants -> Plant
initialPlant constants =
  { k: constants
  , reservoirC: constants.ambientC
  , groupBlockC: constants.ambientC
  , steamC: constants.ambientC
  , steamWaterMl: constants.steamProbeMl - 150.0
  , steamLevelOk: false
  , lineBar: constants.regulatedLineBar
  , inletC: constants.inletC
  , brewBar: 0.0
  , mixTrueC: constants.ambientC
  , mixSensorC: constants.ambientC
  , groupWaterC: constants.ambientC
  , puckDoseG: constants.referenceDoseG
  , puckResistance: 1.0
  , puckSolubility: 1.0
  , puckWetMl: 0.0
  , extracted: 0.0
  , cupGrams: 0.0
  , flowMlPerS: 0.0
  }

-- | What the barista put in the basket. `resistance` and `solubility`
-- | are relative to a medium roast at the reference grind: 1.0 is that
-- | coffee, more resistance slows the water and more solubility gives
-- | up flavour sooner.
type Puck = { doseG :: Number, resistance :: Number, solubility :: Number }

-- | A new dry puck of the given dose in the group and an empty cup on
-- | the scale.
freshPuck :: Puck -> Plant -> Plant
freshPuck puck p = p
  { puckDoseG = puck.doseG
  , puckResistance = puck.resistance
  , puckSolubility = puck.solubility
  , puckWetMl = 0.0
  , extracted = 0.0
  , cupGrams = 0.0
  }

-- | Jump the plant to a heated state so a session does not have to
-- | start with a twenty minute warm-up.
warmPlant :: Number -> Number -> Plant -> Plant
warmPlant reservoirTargetC groupTargetC p = p
  { reservoirC = reservoirTargetC
  , groupBlockC = groupTargetC
  , steamC = 133.0
  , steamWaterMl = p.k.steamProbeMl
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
    k = p.k

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
    resistance = k.puckResistanceBarSPerMl * doseScale * p.puckResistance * (1.0 - 0.35 * clamp01 (p.cupGrams / 45.0))
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
    -- extraction: each millilitre through the wet puck takes a share of
    -- what is left, a larger share from hotter water and a more soluble
    -- coffee
    perMl = k.extractionPerMl * p.puckSolubility * max 0.2 (1.0 + k.extractionPerC * (p.groupWaterC - 93.0))
    extracted =
      if saturated then p.extracted + (1.0 - p.extracted) * clamp01 (perMl * qPuck * dt)
      else p.extracted

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
    { k
    , reservoirC
    , groupBlockC
    , steamC
    , steamWaterMl
    , steamLevelOk
    , lineBar
    , inletC: k.inletC
    , brewBar
    , mixTrueC
    , mixSensorC
    , groupWaterC
    , puckDoseG: p.puckDoseG
    , puckResistance: p.puckResistance
    , puckSolubility: p.puckSolubility
    , puckWetMl
    , extracted
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
