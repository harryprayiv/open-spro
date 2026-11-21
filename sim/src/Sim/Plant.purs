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
  , Brewed
  , brewed
  , warmPlant
  , steamSafetyValveBar
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
  -- Temperature of the bed of coffee and the water it holds.
  , puckC :: Number
  -- How evenly the water passes through the puck: 1 is an even puck and
  -- lower is a channeled one. It only ever falls during a shot.
  , puckEvenness :: Number
  -- How far the wet grounds have swollen and bound together, from 0 for
  -- a dry puck to 1. A settled puck stands up to a steep pressure ramp.
  , puckSettled :: Number
  -- The share of what the coffee can give up that has dissolved into
  -- the water held in the puck and has not yet reached the cup.
  , dissolved :: Number
  -- The share of what the coffee can give up that has reached the cup.
  , extracted :: Number
  -- Running sums over everything that has dissolved, weighted by the
  -- bed temperature and the pressure at the moment it dissolved. Their
  -- ratio to the total says how hot and how hard the cup was brewed.
  , dissolvedTotal :: Number
  , dissolvedHeat :: Number
  , dissolvedBar :: Number
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
  -- Soaking: what the grounds give up to the water held in the puck
  -- each second, and how much that water can hold before it stops
  -- taking more.
  , soakPerS :: Number
  , soakCapacity :: Number
  -- Heat: how much heat a gram of dry coffee takes per degree, and how
  -- well the basket ties the puck to the group block.
  , coffeeJPerGK :: Number
  , puckCouplingWPerK :: Number
  -- Evenness: the fill rate and the pressure ramp a puck takes without
  -- harm, the harm done beyond them, how long the grounds take to
  -- swell, and what a channeled puck does to flow and extraction.
  , gentleFillMlPerS :: Number
  , fillHarmPerMl :: Number
  , gentleRampBarPerS :: Number
  , rampHarmPerBar :: Number
  , puckSwellS :: Number
  , channelFlowGain :: Number
  , channelExtractionLoss :: Number
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
  , extractionPerMl: 0.044
  , extractionPerC: 0.04
  , soakPerS: 0.012
  , soakCapacity: 0.15
  , coffeeJPerGK: 1.7
  , puckCouplingWPerK: 1.5
  , gentleFillMlPerS: 2.5
  , fillHarmPerMl: 0.008
  , gentleRampBarPerS: 1.0
  , rampHarmPerBar: 0.08
  , puckSwellS: 5.0
  , channelFlowGain: 0.5
  , channelExtractionLoss: 0.6
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
  , puckC: constants.ambientC
  , puckEvenness: 1.0
  , puckSettled: 0.0
  , dissolved: 0.0
  , extracted: 0.0
  , dissolvedTotal: 0.0
  , dissolvedHeat: 0.0
  , dissolvedBar: 0.0
  , cupGrams: 0.0
  , flowMlPerS: 0.0
  }

-- | What the barista put in the basket. `resistance` and `solubility`
-- | are relative to a medium roast at the reference grind: 1.0 is that
-- | coffee, more resistance slows the water and more solubility gives
-- | up flavour sooner.
type Puck = { doseG :: Number, resistance :: Number, solubility :: Number }

-- | A new dry puck of the given dose in the group and an empty cup on
-- | the scale. The grounds go in at room temperature and even.
freshPuck :: Puck -> Plant -> Plant
freshPuck puck p = p
  { puckDoseG = puck.doseG
  , puckResistance = puck.resistance
  , puckSolubility = puck.solubility
  , puckWetMl = 0.0
  , puckC = p.k.ambientC
  , puckEvenness = 1.0
  , puckSettled = 0.0
  , dissolved = 0.0
  , extracted = 0.0
  , dissolvedTotal = 0.0
  , dissolvedHeat = 0.0
  , dissolvedBar = 0.0
  , cupGrams = 0.0
  }

-- | How the cup was brewed, read off the puck after the shot: the
-- | temperature and the pressure the coffee dissolved at, averaged over
-- | everything that dissolved, and how even the puck stayed.
type Brewed = { atC :: Number, atBar :: Number, evenness :: Number }

brewed :: Plant -> Brewed
brewed p =
  if p.dissolvedTotal <= 1.0e-6 then { atC: p.puckC, atBar: 0.0, evenness: p.puckEvenness }
  else
    { atC: p.dissolvedHeat / p.dissolvedTotal
    , atBar: p.dissolvedBar / p.dissolvedTotal
    , evenness: p.puckEvenness
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

-- | The pressure at which the steam boiler's safety valve SRV_STM
-- | lifts. It is a certified mechanical part, bought set and never
-- | adjusted, so it is fixed here and is not among the parameters that
-- | can be changed. It acts on the boiler whatever the controller is
-- | doing.
steamSafetyValveBar :: Number
steamSafetyValveBar = 2.5

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

    -- puck: a bigger dose soaks up more water and resists more. A
    -- channeled puck lets the water past more easily.
    doseScale = p.puckDoseG / k.referenceDoseG
    fillMl = k.puckFillMl * doseScale
    saturated = p.puckWetMl >= fillMl
    uneven = 1.0 - p.puckEvenness
    resistance =
      k.puckResistanceBarSPerMl * doseScale * p.puckResistance
        * (1.0 - 0.35 * clamp01 (p.cupGrams / 45.0))
        * (1.0 - k.channelFlowGain * uneven)
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
    wetShare = clamp01 (p.puckWetMl / fillMl)

    -- evenness. Water that arrives faster than the dry grounds can take
    -- it up fingers through them, and a steep rise in pressure cracks a
    -- puck that has not yet swollen. A finer or denser puck carries a
    -- larger pressure difference and cracks more readily. Both kinds of
    -- harm take a share of the evenness that is left, so they never
    -- drive it below nothing.
    puckSettled = p.puckSettled + (1.0 - p.puckSettled) * wetShare * clamp01 (dt / k.puckSwellS)
    fillHarm =
      if saturated then 0.0
      else k.fillHarmPerMl * max 0.0 (qPuck / doseScale - k.gentleFillMlPerS) * dt
    rampHarm =
      if not toGroup then 0.0
      else
        k.rampHarmPerBar * max 0.0 ((brewBar - p.brewBar) - k.gentleRampBarPerS * dt)
          * (1.0 - p.puckSettled)
          * p.puckResistance
    puckEvenness = p.puckEvenness * (1.0 - clamp01 (fillHarm + rampHarm))

    -- puck temperature. The grounds start at room temperature and take
    -- their heat from the water: what stays in the puck while it fills
    -- mixes with it, what passes through a full puck exchanges with it,
    -- and the basket ties it loosely to the group block.
    puckJPerK = p.puckDoseG * k.coffeeJPerGK + p.puckWetMl * 4.186
    waterJPerK = qPuck * dt * 4.186
    puckWarmedC =
      if saturated then p.puckC + (p.groupWaterC - p.puckC) * clamp01 (waterJPerK / puckJPerK)
      else (p.puckC * puckJPerK + p.groupWaterC * waterJPerK) / (puckJPerK + waterJPerK)
    puckC = puckWarmedC + (p.groupBlockC - puckWarmedC) * clamp01 (k.puckCouplingWPerK * dt / puckJPerK)

    -- extraction, in two parts. Water passing through the wet puck
    -- washes out a share of what is left with each millilitre, a larger
    -- share from a hotter bed and a more soluble coffee, and a smaller
    -- one from a channeled puck, where much of the water misses the
    -- grounds. Meanwhile the grounds give up flavour to the water held
    -- in the puck for as long as they sit in it, flow or no flow, until
    -- that water has taken what it can hold. What is held in the puck
    -- reaches the cup only when the water after it pushes it out.
    hot = max 0.2 (1.0 + k.extractionPerC * (p.puckC - 93.0))
    left = max 0.0 (1.0 - p.extracted - p.dissolved)
    washed =
      if saturated then
        left * clamp01 (k.extractionPerMl * p.puckSolubility * hot * (1.0 - k.channelExtractionLoss * uneven) * qPuck * dt)
      else 0.0
    soaked =
      (left - washed) * clamp01 (k.soakPerS * p.puckSolubility * hot * wetShare * dt)
        * max 0.0 (1.0 - p.dissolved / k.soakCapacity)
    pushedOut = if saturated then p.dissolved * clamp01 (qPuck * dt / fillMl) else 0.0
    dissolved = p.dissolved + soaked - pushedOut
    extracted = p.extracted + washed + pushedOut
    dissolvedTotal = p.dissolvedTotal + washed + soaked
    dissolvedHeat = p.dissolvedHeat + (washed + soaked) * p.puckC
    dissolvedBar = p.dissolvedBar + (washed + soaked) * p.brewBar

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
    wandGPerS =
      if d.steamValveOpen && steamBarNow > 0.2 then k.steamDrawGPerS * steamBarNow / 1.8
      else 0.0
    -- Over its set pressure the safety valve lifts and vents steam to
    -- the drip tray, far faster than the element can make it, which
    -- carries the heat away and holds the pressure near the set
    -- point. The controller has no part in this.
    ventGPerS = 60.0 * max 0.0 (steamBarNow - steamSafetyValveBar)
    drawGPerS = wandGPerS + ventGPerS
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
    , puckC
    , puckEvenness
    , puckSettled
    , dissolved
    , extracted
    , dissolvedTotal
    , dissolvedHeat
    , dissolvedBar
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
