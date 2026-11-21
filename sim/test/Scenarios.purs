-- | Headless scenarios. Each drives the simulated machine through a
-- | sequence of events, prints what happened, and fails the run if an
-- | expectation or any spec invariant is broken.
module Test.Main (main) where

import Prelude

import Data.Array (drop, filter, last, length, range, replicate, take, zipWith)
import Data.Array.NonEmpty (fromArray, head, toArray)
import Data.Either (Either(..), isLeft, isRight)
import Data.Foldable (all, and, foldl, for_, maximum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Number (abs)
import Data.Number.Format (fixed, toStringWith)
import Data.String (Pattern(..), contains, joinWith)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Sim.Advice (advise)
import Sim.Backend (Backend, SimCommand(..))
import Sim.Backend as Backend
import Sim.Controller (shotInProgress)
import Sim.Cup as Cup
import Sim.Curve as Curve
import Sim.Flavor as Flavor
import Sim.Forecast (forecast)
import Sim.Link (Event(..), FaderMode(..), eventLabel)
import Sim.Machine (Machine, Outcome(..))
import Sim.Machine as Machine
import Sim.Params as Params
import Sim.Plant (Plant, brewed, noDisturbance, readSensors, steamSafetyValveBar, stepPlant)
import Sim.Profiles as Profiles
import Sim.Recipe as Recipe
import Sim.Shapes as Shapes
import Sim.Tint as Tint
import Sim.Types (ClientKind(..), Curve(..), FaultKind(..), GroupValve(..), MachineState(..), PowerMode(..), Profile, PumpMode(..), ShotState(..), StepMode(..), TwoWay(..), defaultConfig)
import Sim.World (World, dt, initialWorld, paramsOf, send, sensors, setParams, stepMany, stepWorld, warmUp, worldWith)

n1 :: Number -> String
n1 = toStringWith (fixed 1)

n2 :: Number -> String
n2 = toStringWith (fixed 2)

-- | Step until the predicate holds or the time limit passes.
runUntil :: Number -> (World -> Boolean) -> World -> World
runUntil limit done w
  | done w || limit <= 0.0 = w
  | otherwise = runUntil (limit - dt) done (stepWorld w)

expect :: String -> Boolean -> Effect Unit
expect what ok =
  if ok then log ("  ok    " <> what)
  else throw ("FAILED: " <> what)

sendOrFail :: Event -> World -> Effect World
sendOrFail event w = case send event w of
  Right next -> pure next
  Left reason -> throw ("event rejected: " <> reason)

coldStart :: Effect World
coldStart = do
  log "Cold start to ready, full mode"
  w0 <- sendOrFail (WakeRequested Full) initialWorld
  let w1 = runUntil 3600.0 (\w -> w.ctl.state == Ready) w0
  log ("  ready after " <> n1 (w1.now / 60.0) <> " min, " <> show w1.ctl.fillsSinceBlowdown <> " fill")
  expect "reaches ready within 40 minutes" (w1.ctl.state == Ready && w1.now < 2400.0)
  pure w1

-- | Ten idle minutes at temperature. The machine must not drop out of
-- | ready while its heaters switch around their setpoints.
holdsReady :: World -> Effect Unit
holdsReady w0 = do
  log "Ten idle minutes after reaching ready"
  let count acc w = if w.ctl.state == Ready then acc else acc + 1
  let run n acc w = if n <= 0 then acc else run (n - 1) (count acc w) (stepWorld w)
  let notReady = run 24000 0 w0
  log ("  cycles spent out of ready: " <> show notReady)
  expect "stays ready" (notReady == 0)

-- | The idle clock runs from the moment the machine became ready. It
-- | must still be awake just before the idle period ends, asleep just
-- | after, and usable again once woken.
idleSleepAndWake :: World -> Effect Unit
idleSleepAndWake ready = do
  log "Idle sleep and wake"
  let almost = runUntil (defaultConfig.idleSleepAfter - 30.0) (const false) ready
  expect "still ready just before the idle period ends" (almost.ctl.state == Ready)
  let asleep = runUntil 120.0 (\w -> w.ctl.state == Sleeping) almost
  expect "asleep after the idle period" (asleep.ctl.state == Sleeping)
  expect "a shot is refused while asleep" (isLeft (send ShotRequested asleep))
  woken <- sendOrFail (WakeRequested Full) asleep
  let again = runUntil 1800.0 (\w -> w.ctl.state == Ready) woken
  expect "ready again after waking" (again.ctl.state == Ready)
  expect "a shot is accepted" (isRight (send ShotRequested again))
  expect "no invariant broken" (again.violations == [])

pullShot :: World -> Profile -> Effect World
pullShot w0 profile = do
  log ("Shot: " <> profile.name)
  w1 <- sendOrFail (ProfileSelected profile) w0
  let w2 = runUntil 1800.0 (\w -> w.ctl.state == Ready) (stepMany 40 w1)
  w3 <- sendOrFail ShotRequested w2
  let w4 = runUntil 200.0 (\w -> not (shotInProgress w.ctl)) w3
  let peak = fromMaybe 0.0 (maximum (map _.bar w4.trace))
  let settled = map (\s -> abs (s.groupWaterC - s.targetC)) w4.trace
  let worst = foldl max 0.0 settled
  case last w4.ctl.history of
    Nothing -> throw "no shot recorded"
    Just shot -> do
      log
        ( "  " <> show shot.state
            <> ", " <> n1 (fromMaybe 0.0 shot.endedAt - shot.startedAt) <> " s"
            <> ", " <> n1 (fromMaybe 0.0 shot.finalGrams) <> " g"
            <> ", peak " <> n1 peak <> " bar"
            <> ", worst water-to-target gap " <> n1 worst <> " C"
        )
      for_ w4.ctl.faults \f -> log ("  fault: " <> show f.kind)
      expect "stops on weight" (shot.state == StoppedByWeight)
      expect "peak pressure under the limit" (peak < 11.0)
  pure w4

coldBranchFailure :: World -> Effect Unit
coldBranchFailure w0 = do
  log "Cold branch blocked during a shot"
  w1 <- sendOrFail (ProfileSelected Profiles.classic) w0
  let w2 = runUntil 1800.0 (\w -> w.ctl.state == Ready) (stepMany 40 w1)
  w3 <- sendOrFail ShotRequested w2 { dist = w2.dist { coldBranchBlocked = true } }
  let w4 = runUntil 60.0 (\w -> w.ctl.state == Faulted) w3
  let kinds = map _.kind w4.ctl.faults
  expect "aborts with mix_over_temperature" (kinds == [ MixOverTemperature ])
  expect "actuators are idle after the fault" (not w4.ctl.act.heatEnabled && not (shotInProgress w4.ctl))
  expect "no invariant broken" (w4.violations == [])

inletLoss :: World -> Effect Unit
inletLoss w0 = do
  log "Inlet pressure lost during a shot"
  w1 <- sendOrFail ShotRequested w0
  let w2 = stepMany 200 w1
  let w3 = runUntil 30.0 (\w -> w.ctl.state == Faulted) w2 { dist = w2.dist { inletOpen = false } }
  expect "aborts with no_inlet_pressure" (map _.kind w3.ctl.faults == [ NoInletPressure ])
  expect "no invariant broken" (w3.violations == [])

-- | The mechanical valves act with no controller at all. Here nothing
-- | reads a sensor or decides anything: the outputs are held where a
-- | failed controller could leave them, and the plant is run on its
-- | own.
mechanicalRelief :: World -> Effect Unit
mechanicalRelief w = do
  log "Mechanical relief, with the controller out of the loop"
  let
    k = (paramsOf w).plant
    stuck = w.ctl.act { heatEnabled = true, steamElementOn = true, brewElementOn = false }
    run :: Int -> Number -> Plant -> Number
    run steps most plant
      | steps <= 0 = most
      | otherwise =
          let
            next = stepPlant dt noDisturbance stuck plant
          in
            run (steps - 1) (max most (readSensors noDisturbance next).steamBar) next
    -- two hours with the steam element stuck on
    peak = run 288000 0.0 w.plant
  log ("  steam element stuck on for two hours: peak " <> n2 peak <> " bar")
  expect "the steam safety valve holds the boiler near its set pressure" (peak > steamSafetyValveBar - 0.05 && peak < steamSafetyValveBar + 0.2)
  expect "the firmware's steam limit is under the safety valve" (w.ctl.cfg.steamLimitBar < steamSafetyValveBar)
  expect "the firmware's brew pressure limit is under the expansion valve" (w.ctl.cfg.brewPressureLimitBar < k.expansionValveBar)
  let
    -- the pump flat out against a shut group: nowhere for the water to go
    deadHead = w.ctl.act { pump = PressureControl, pumpSpeed = 1.0, bypassValve = Closed, groupValve = ToDrain }
    press :: Int -> Number -> Plant -> Number
    press steps most plant
      | steps <= 0 = most
      | otherwise =
          let
            next = stepPlant dt noDisturbance deadHead plant
          in
            press (steps - 1) (max most next.brewBar) next
    brewPeak = press 2400 0.0 w.plant
  log ("  pump flat out for a minute: peak " <> n2 brewPeak <> " bar")
  expect "the expansion valve caps the brew pressure" (brewPeak <= k.expansionValveBar + 0.001)

steaming :: World -> Effect Unit
steaming w0 = do
  log "Two minutes of steaming"
  let fillsBefore = w0.ctl.fillsSinceBlowdown
  let w1 = runUntil 120.0 (const false) w0 { dist = w0.dist { steamValveOpen = true } }
  let w2 = runUntil 600.0 (\w -> w.ctl.state == Ready) w1 { dist = w1.dist { steamValveOpen = false } }
  log
    ( "  steam fell to " <> n1 (sensors w1).steamBar <> " bar, "
        <> show (w2.ctl.fillsSinceBlowdown - fillsBefore) <> " refill, ready again "
        <> n1 (w2.now - w1.now) <> " s after closing the valve"
    )
  expect "boiler refilled" (w2.ctl.fillsSinceBlowdown > fillsBefore)
  expect "no invariant broken" (w2.violations == [])

-- | Every curve starts at nothing and ends at the target, and never
-- | overshoots on the way.
curveShapes :: Effect Unit
curveShapes = do
  log "Curve shapes"
  for_ Curve.all \curve -> do
    expect (show curve <> " ends at the target") (abs (Curve.shape curve 1.0 - 1.0) < 1.0e-9)
    expect (show curve <> " holds after the ramp") (abs (Curve.shape curve 3.0 - 1.0) < 1.0e-9)
    expect (show curve <> " stays between start and target")
      (foldl (\ok x -> ok && Curve.shape curve x >= 0.0 && Curve.shape curve x <= 1.0) true [ 0.0, 0.1, 0.25, 0.5, 0.75, 0.9 ])
  expect "straight is halfway at half time" (abs (Curve.shape Linear 0.5 - 0.5) < 1.0e-9)
  expect "eased is halfway at half time" (abs (Curve.shape Ease 0.5 - 0.5) < 1.0e-9)
  expect "fast then settling is past halfway at half time" (Curve.shape Exponential 0.5 > 0.8)
  expect "every ramp but a jump starts from where the setpoint stood"
    (Curve.shape Linear 0.0 == 0.0 && Curve.shape Ease 0.0 == 0.0 && Curve.shape Exponential 0.0 == 0.0)

-- | A single pressure step that ramps in a straight line from zero to
-- | 8 bar over 10 seconds. The setpoint the controller holds must sit on
-- | that line while the ramp runs and at the target afterwards.
rampIsFollowed :: World -> Effect Unit
rampIsFollowed warm = do
  log "A straight 10 s ramp to 8 bar"
  let
    profile = Profiles.classic
      { name = "ramp test"
      , targetGrams = 500.0
      , steps = pure
          { name: "ramp"
          , mode: Pressure
          , curve: Linear
          , rampS: 10.0
          , targetC: Profiles.classic.maxTargetC
          , targetBar: Just 8.0
          , targetMlPerS: Nothing
          , maxDuration: 30.0
          , exitAboveBar: Nothing
          , exitAboveGrams: Nothing
          }
      }
  w1 <- sendOrFail (ProfileSelected profile) warm
  let w2 = runUntil 1800.0 (\w -> w.ctl.state == Ready) (stepMany 40 w1)
  w3 <- sendOrFail ShotRequested w2
  let startBar = fromMaybe (-1.0) w3.ctl.pressureTargetBar
  let mid = runUntil 5.0 (const false) w3
  let midBar = fromMaybe (-1.0) mid.ctl.pressureTargetBar
  let late = runUntil 10.0 (const false) mid
  let lateBar = fromMaybe (-1.0) late.ctl.pressureTargetBar
  log ("  setpoint " <> n1 startBar <> " bar at the start, " <> n1 midBar <> " at 5 s, " <> n1 lateBar <> " at 15 s; measured " <> n1 (sensors late).brewBar <> " bar at 15 s")
  expect "starts from the pressure in the group, not from the target" (startBar < 1.0)
  -- The ramp runs from the empty group's 0 bar. The setpoint shown is
  -- never under 0.5 bar, so the start reads 0.5 and the middle reads 4.
  expect "halfway at half the ramp time" (abs (midBar - 4.0) < 0.1)
  expect "at the target after the ramp" (abs (lateBar - 8.0) < 1.0e-6)
  expect "measured pressure has followed" (abs ((sensors late).brewBar - 8.0) < 0.6)
  expect "no invariant broken" (late.violations == [])

-- | Parameters: a change to the physics must show up in the plant, a
-- | change to a limit must show up in the controller, and the stored
-- | text must read back to the same values.
parameters :: World -> Effect Unit
parameters ready = do
  log "Parameters"
  let lossy = Params.defaults { plant = Params.defaults.plant { groupLossWPerK = 6.0 } }
  asleep <- sendOrFail SleepRequested ready
  let cooled p = (sensors (runUntil 600.0 (const false) (setParams p asleep))).groupC
  let slow = cooled Params.defaults
  let fast = cooled lossy
  log ("  group block after 10 unheated minutes: " <> n1 slow <> " C by default, " <> n1 fast <> " C with six times the loss")
  expect "more heat loss cools the group block faster" (fast < slow - 5.0)
  let tight = Params.defaults { config = Params.defaults.config { brewPressureLimitBar = 7.0 } }
  shot <- sendOrFail ShotRequested (setParams tight (warmUp ready))
  let held = runUntil 60.0 (\w -> not (shotInProgress w.ctl)) shot
  let heldPeak = fromMaybe 0.0 (maximum (map _.bar held.trace))
  log ("  9 bar shot with the limit lowered to 7 bar: peak " <> n1 heldPeak <> " bar")
  expect "a lowered pressure limit holds a 9 bar shot a bar under it, with no fault" (heldPeak <= 6.3 && held.ctl.faults == [])
  let changed = (paramsOf (setParams lossy ready)) { tuning = Params.defaults.tuning { pumpKp = 0.11 } }
  let back = Params.decode (Params.encode changed)
  expect "stored parameters read back the same"
    (back.plant.groupLossWPerK == 6.0 && back.tuning.pumpKp == 0.11 && back.config == Params.defaults.config)
  expect "defaults store as nothing" (Params.encode Params.defaults == "")
  expect "an out-of-range stored value is clamped" ((Params.decode "pumpKp=99").tuning.pumpKp == 0.5)
  expect "an unknown stored key is ignored" ((Params.decode "nonsense=1").tuning == Params.defaults.tuning)
  expect "a cold machine built from parameters carries them" ((worldWith lossy).plant.k.groupLossWPerK == 6.0)

-- | The user's shapes: editing keeps the summary fields true, and the
-- | stored text reads back to the same shapes.
shapes :: Effect Unit
shapes = do
  log "Custom shapes"
  let copy = Shapes.fork Profiles.declining
  let hotter = Shapes.editStep 0 _ { targetC = 96.0, curve = Ease, rampS = 7.5 } copy
  expect "the hottest step sets the profile's maximum" (hotter.maxTargetC == 96.0)
  let longer = Shapes.addStep hotter
  expect "adding a step adds one" (length (toArray longer.steps) == length (toArray hotter.steps) + 1)
  let one = foldl (\p _ -> Shapes.removeStep 0 p) longer [ 1, 2, 3, 4, 5, 6 ]
  expect "a profile keeps at least one step" (length (toArray one.steps) == 1)
  let stored = Shapes.decode (Shapes.encode [ hotter, longer ])
  expect "stored shapes read back the same" (stored == [ hotter, longer ])
  expect "unreadable stored text gives no shapes" (Shapes.decode "garbage\nS\tx" == [])

-- | Pacing: the same recipe on a puck that resists more, and on one
-- | that resists less. Left alone, the slow puck runs long and the fast
-- | one runs short. Paced to 30 s, both must land nearer 30 s, with the
-- | pressure moved in the right direction and by no more than the limit.
pacing :: Effect Unit
pacing = do
  log "Pacing a shot to 30 s"
  let plain = { doseG: 18.0, resistance: 1.0, solubility: 1.0 }
  let free = Profiles.classic
  let paced = Profiles.classic { targetSeconds = Just 30.0 }
  let puck factor = Params.defaults { plant = Params.defaults.plant { puckResistanceBarSPerMl = Params.defaults.plant.puckResistanceBarSPerMl * factor } }
  let off target f = abs (f.seconds - target)
  let report name f = log ("  " <> name <> ": " <> n1 f.grams <> " g in " <> n1 f.seconds <> " s, peak " <> n1 f.peakBar <> " bar, pacing at " <> n1 f.paceBar <> " bar" <> (if f.paceSpent then " (all it may use)" else ""))
  let slowFree = forecast (puck 1.5) free plain
  let slowPaced = forecast (puck 1.5) paced plain
  let fastFree = forecast (puck 0.7) free plain
  let fastPaced = forecast (puck 0.7) paced plain
  report "slow puck, left alone" slowFree
  report "slow puck, paced" slowPaced
  report "fast puck, left alone" fastFree
  report "fast puck, paced" fastPaced
  expect "a slow puck left alone runs long" (slowFree.seconds > 33.0)
  expect "pacing brings the slow puck nearer 30 s" (off 30.0 slowPaced < off 30.0 slowFree - 1.0)
  expect "it does so with more pressure" (slowPaced.peakBar > slowFree.peakBar + 0.5)
  expect "and stays a bar under the pressure limit" (slowPaced.peakBar <= Params.defaults.config.brewPressureLimitBar - 0.9)
  expect "a fast puck left alone runs short" (fastFree.seconds < 27.0)
  expect "pacing brings the fast puck nearer 30 s" (off 30.0 fastPaced < off 30.0 fastFree - 1.0)
  expect "it does so with less pressure" (fastPaced.paceBar < -0.5)
  expect "pacing leaves an unpaced shot alone" (slowFree.paceBar == 0.0 && fastFree.paceBar == 0.0)
  expect "every forecast shot stops on weight"
    (map _.outcome [ slowFree, slowPaced, fastFree, fastPaced ] == [ Just StoppedByWeight, Just StoppedByWeight, Just StoppedByWeight, Just StoppedByWeight ])
  expect "no invariant broken in any of them"
    (slowFree.violations <> slowPaced.violations <> fastFree.violations <> fastPaced.violations == [])
  let cold = forecast Params.defaults (Profiles.classic { maxTargetC = 103.0 }) plain
  expect "a recipe the machine would refuse is forecast as refused" (cold.refused /= Nothing)

-- | Recipes: the roast starting points follow the rule that lighter
-- | roasts get hotter, longer shots, and stored recipes read back.
recipes :: Effect Unit
recipes = do
  log "Recipes"
  case Recipe.builtIn of
    [ medium, light, dark ] -> do
      expect "a lighter roast starts hotter" (light.tempC > medium.tempC && medium.tempC > dark.tempC)
      expect "a lighter roast starts with a longer shot" (light.seconds > medium.seconds && medium.seconds > dark.seconds)
      let kept = light { name = "Gesha", roaster = "A roaster", roastedOn = "2026-11-02", paced = false, grind = -1.5, soakS = 7.0 }
      expect "stored recipes read back the same" (Recipe.decode (Recipe.encode [ kept, dark ]) == [ kept, dark ])
    _ -> throw "expected three built-in recipes"
  expect "a recipe stored before the soak was recorded reads with no soak"
    (map _.soakS (Recipe.decode "B\t0\t18.0\t2.0\t93.0\tR\t2026\tCity\t26.0\tpaced\t0.0") == [ 0.0 ])
  let starts = map (\roast -> Recipe.withRoastDefaults Recipe.house { roast = roast }) Recipe.roasts
  expect "roasts lighter than Full City start with a soak, and the rest with none"
    (map (\r -> r.soakS > 0.0) starts == map (\roast -> Recipe.development roast < 0.5) Recipe.roasts)
  expect "a lighter roast never starts with a shorter soak" (and (zipWith (\a b -> a.soakS >= b.soakS) starts (drop 1 starts)))
  let old = Recipe.decode "Old beans\t0\t18.0\t2.0\t93.0"
  expect "a recipe stored before beans were recorded still reads" (map _.name old == [ "Old beans" ] && map _.paced old == [ false ])

-- | Beans: at the same grind a dark roast runs slower than a light one,
-- | a coarser grind speeds it up, and the roast starting points land a
-- | balanced cup where the medium recipe on the wrong roast does not.
beans :: Effect Unit
beans = do
  log "Roast, grind and what lands in the cup"
  case Recipe.builtIn of
    [ medium, light, dark ] -> do
      let run r = forecast Params.defaults (Recipe.toProfile (toArray Profiles.all) r) (Recipe.puckOf r)
      let free r = run r { paced = false }
      let asMedium r = medium { roast = r.roast }
      let report name f = log ("  " <> name <> ": " <> n1 f.grams <> " g in " <> n1 f.seconds <> " s, extracted " <> toStringWith (fixed 2) f.extracted <> ", pacing at " <> n1 f.paceBar <> " bar")
      report "medium, its own recipe" (run medium)
      report "light, its own recipe" (run light)
      report "dark, its own recipe" (run dark)
      report "medium, its own recipe, unpaced" (free medium)
      report "light, its own recipe, unpaced" (free light)
      report "dark, its own recipe, unpaced" (free dark)
      report "light, the medium recipe" (run (asMedium light))
      report "dark, the medium recipe" (run (asMedium dark))
      report "dark, the medium recipe, unpaced" (free (asMedium dark))
      report "dark, the medium recipe, unpaced, 3 steps coarser" (free (asMedium dark) { grind = 3.0 })
      expect "a medium roast on its own recipe is balanced" (Cup.taste (run medium).extracted == Cup.Balanced)
      expect "a light roast on its own recipe is balanced" (Cup.taste (run light).extracted == Cup.Balanced)
      expect "a dark roast on its own recipe is balanced" (Cup.taste (run dark).extracted == Cup.Balanced)
      expect "a light roast on the medium recipe is under-extracted" (Cup.taste (run (asMedium light)).extracted == Cup.Under)
      expect "a dark roast on the medium recipe is over-extracted" (Cup.taste (run (asMedium dark)).extracted == Cup.Over)
      expect "unpaced, a dark roast runs slower than a light one at the same grind"
        ((free (asMedium dark)).seconds > (free (asMedium light)).seconds + 3.0)
      expect "a coarser grind speeds the dark roast up"
        ((free (asMedium dark) { grind = 3.0 }).seconds < (free (asMedium dark)).seconds - 3.0)
    _ -> throw "expected three built-in recipes"
  -- Every roast level, on the starting point the machine works out for it.
  let start roast = Recipe.withRoastDefaults plainRecipe { roast = roast }
  let run r = forecast Params.defaults (Recipe.toProfile (toArray Profiles.all) r) (Recipe.puckOf r)
  for_ Recipe.roasts \roast -> do
    let r = start roast
    let f = run r
    log
      ( "  " <> Recipe.roastLabel roast <> ": grind " <> n1 r.grind <> ", " <> n1 r.tempC <> " C, 1 to " <> toStringWith (fixed 2) r.ratio
          <> ", " <> n1 r.seconds <> " s wanted; " <> n1 f.grams <> " g in " <> n1 f.seconds <> " s, extracted " <> toStringWith (fixed 2) f.extracted
          <> ", pacing at " <> n1 f.paceBar <> " bar"
      )
    expect (Recipe.roastLabel roast <> " lands a balanced cup on its starting point") (Cup.taste f.extracted == Cup.Balanced)
    expect (Recipe.roastLabel roast <> " lands within 3 s of its time") (abs (f.seconds - r.seconds) < 3.0)
    expect (Recipe.roastLabel roast <> " breaks no invariant") (f.violations == [] && f.outcome == Just StoppedByWeight)
  let standard = run Recipe.house { paced = false }
  log ("  house standard, unpaced: " <> n1 standard.grams <> " g in " <> n1 standard.seconds <> " s, extracted " <> toStringWith (fixed 2) standard.extracted)
  expect "the house standard is 22 g in and 44 g out" (Recipe.house.doseG == 22.0 && Recipe.targetGrams Recipe.house == 44.0)
  expect "the house standard lands within 1.5 s of 30 s without pacing" (abs (standard.seconds - 30.0) < 1.5)
  expect "the house standard is balanced" (Cup.taste standard.extracted == Cup.Balanced)
  let starts = map start Recipe.roasts
  let falling pick = and (zipWith (\a b -> pick a >= pick b) starts (drop 1 starts))
  expect "a darker roast never starts hotter, longer or finer than a lighter one"
    (falling _.tempC && falling _.ratio && falling _.seconds && falling (negate <<< _.grind))
  expect "a recipe stored under the first three names still reads"
    (map (Recipe.roastLabel <<< _.roast) (Recipe.decode "B\t0\t18.0\t2.0\t93.0\tR\t2026\tDark\t26.0\tpaced\t0.0") == [ "French" ])
  where
  plainRecipe = { name: "", roaster: "", roastedOn: "", roast: Recipe.FullCity, profileIndex: 0, doseG: 18.0, ratio: 2.0, tempC: 93.0, seconds: 28.0, paced: true, grind: 0.0, soakS: 0.0 }

-- | The coffee map: a light roast lands among flowers, citrus and
-- | fruit, a dark one among cocoa, spice and roast, extraction moves a
-- | cup clockwise, and the route to a wanted note says the right thing.
coffeeMap :: Effect Unit
coffeeMap = do
  log "The coffee map"
  let clear = { weight: 0.0, clarity: 1.0 }
  let top roast extracted = map _.name (take 3 (Flavor.expected roast extracted clear))
  for_ Recipe.roasts \roast ->
    log ("  " <> Recipe.roastLabel roast <> ", balanced: " <> joinWith ", " (top roast 0.675))
  expect "a City roast leads with a citrus or fruit note" (Flavor.place Recipe.City 0.675 > 3.0 / 24.0 && Flavor.place Recipe.City 0.675 < 9.0 / 24.0)
  expect "a Full City roast lands among the sweet and nut notes" (Flavor.place Recipe.FullCity 0.675 > 9.0 / 24.0 && Flavor.place Recipe.FullCity 0.675 < 15.0 / 24.0)
  expect "a French roast lands among cocoa, spice and roast" (Flavor.place Recipe.French 0.675 > 15.0 / 24.0)
  expect "a darker roast never lands lighter" (and (zipWith (\a b -> Flavor.place a 0.675 <= Flavor.place b 0.675) Recipe.roasts (drop 1 Recipe.roasts)))
  expect "extracting more moves the cup towards the dark side" (Flavor.place Recipe.FullCity 0.75 > Flavor.place Recipe.FullCity 0.60)
  let is found wanted = case found, wanted of
        Flavor.Already, "already" -> true
        Flavor.ExtractMore, "more" -> true
        Flavor.ExtractLess, "less" -> true
        Flavor.OtherRoast _ _, "other" -> true
        _, _ -> false
  expect "a note the cup shows is already there" (is (Flavor.route Recipe.FullCity 0.675 13) "already")
  expect "a note just clockwise is reached by extracting more" (is (Flavor.route Recipe.FullCity 0.675 14) "more")
  expect "a note just anticlockwise is reached by extracting less" (is (Flavor.route Recipe.FullCity 0.675 11) "less")
  expect "jasmine is out of reach for a French roast, and a lighter roast is named"
    (case Flavor.route Recipe.French 0.675 0 of
      Flavor.OtherRoast other _ -> Recipe.development other < 0.2
      _ -> false)

-- | A single step straight to 9 bar, with no wetting step before it:
-- | the pump fills the dry puck as fast as it can and the pressure then
-- | rises as fast as the pump can drive it.
straight :: Profile
straight = Profiles.classic
  { name = "Straight 9 bar"
  , steps = pure
      { name: "brew"
      , mode: Pressure
      , curve: Jump
      , rampS: 0.0
      , targetC: Profiles.classic.maxTargetC
      , targetBar: Just 9.0
      , targetMlPerS: Nothing
      , maxDuration: 60.0
      , exitAboveBar: Nothing
      , exitAboveGrams: Nothing
      }
  }

-- | Pre-infusion: how the puck is wetted decides how even it stays,
-- | an uneven puck runs faster and gives up less, a soak gives up more
-- | and takes longer, and the grounds start cold.
preInfusion :: World -> Effect Unit
preInfusion warm = do
  log "Pre-infusion: evenness, soak and puck temperature"
  let plain = { doseG: 18.0, resistance: 1.0, solubility: 1.0 }
  let fine = plain { resistance = 1.3 }
  let run profile puck = forecast Params.defaults profile puck
  let report name f = log ("  " <> name <> ": " <> n1 f.grams <> " g in " <> n1 f.seconds <> " s, extracted " <> n2 f.extracted <> ", evenness " <> n2 f.brewed.evenness <> ", dissolved at " <> n1 f.brewed.atC <> " C and " <> n1 f.brewed.atBar <> " bar")
  let hard = run straight plain
  let classic = run Profiles.classic plain
  let line = run Profiles.linePrebrew { targetGrams = 36.0 } plain
  report "straight to 9 bar" hard
  report "classic, wetted at 2 ml/s" classic
  report "line pressure pre-brew" line
  expect "a fast fill and a steep ramp channel the puck" (Cup.channeled hard.brewed)
  expect "a wetted puck does not channel" (not (Cup.channeled classic.brewed) && not (Cup.channeled line.brewed))
  expect "the gentler the wetting, the more even the puck"
    (hard.brewed.evenness < classic.brewed.evenness && classic.brewed.evenness < line.brewed.evenness)
  expect "the channeled puck extracts less than the wetted one" (hard.extracted < classic.extracted - 0.03)
  -- The same pressure through the same puck, with only the evenness
  -- changed: the uneven one lets more water past.
  let even = Params.defaults { plant = Params.defaults.plant { rampHarmPerBar = 0.0, fillHarmPerMl = 0.0 } }
  let unharmed = forecast even straight plain
  report "straight to 9 bar, with the harm switched off" unharmed
  expect "with the harm switched off the same shot stays even" (unharmed.brewed.evenness == 1.0)
  expect "the channeled puck runs faster than the same puck kept even" (hard.seconds < unharmed.seconds - 0.5)
  expect "and extracts less" (hard.extracted < unharmed.extracted - 0.03)
  let fineHard = run straight fine
  let fineClassic = run Profiles.classic fine
  report "straight to 9 bar, finer puck" fineHard
  report "classic, finer puck" fineClassic
  expect "a finer puck channels more under the same treatment"
    (fineHard.brewed.evenness < hard.brewed.evenness && fineClassic.brewed.evenness < classic.brewed.evenness)
  -- Soak, through the recipe.
  let builtIns = toArray Profiles.all
  let recipe soak = Recipe.house { paced = false, soakS = soak }
  let pull soak = forecast Params.defaults (Recipe.toProfile builtIns (recipe soak)) (Recipe.puckOf (recipe soak))
  let none = pull 0.0
  let short = pull 6.0
  let long = pull 20.0
  report "house standard, no soak" none
  report "house standard, 6 s soak" short
  report "house standard, 20 s soak" long
  expect "a recipe with a soak has one more step, after the first"
    ( length (toArray (Recipe.toProfile builtIns (recipe 6.0)).steps) == length (toArray (Recipe.toProfile builtIns (recipe 0.0)).steps) + 1
        && map _.name (take 2 (toArray (Recipe.toProfile builtIns (recipe 6.0)).steps)) == [ "wet", "soak" ]
    )
  expect "the soak step is line pressure and cannot fault the pump laws" (none.violations <> short.violations <> long.violations == [])
  expect "a soak extracts more" (short.extracted > none.extracted && long.extracted > short.extracted)
  expect "a soak makes the shot longer, by less than its own length"
    (short.seconds > none.seconds + 2.0 && short.seconds < none.seconds + 6.0)
  expect "a soak leaves the puck more even" (short.brewed.evenness > none.brewed.evenness)
  expect "what a longer soak adds tails off" (long.extracted - short.extracted < (short.extracted - none.extracted) * 14.0 / 6.0)
  expect "a soaked cup dissolved at a lower pressure" (long.brewed.atBar < none.brewed.atBar - 1.0)
  -- Puck temperature.
  expect "the coffee dissolves in a bed cooler than the water" (none.brewed.atC < Recipe.house.tempC - 3.0 && none.brewed.atC > 70.0)
  let hot = forecast Params.defaults (Recipe.toProfile builtIns (recipe 0.0) { tempC = 96.0 }) (Recipe.puckOf (recipe 0.0))
  expect "hotter water gives a hotter bed and extracts more" (hot.brewed.atC > none.brewed.atC + 1.0 && hot.extracted > none.extracted)
  -- The live puck, step by step through a real shot.
  w1 <- sendOrFail (ProfileSelected Profiles.classic) warm
  let w2 = runUntil 1800.0 (\w -> w.ctl.state == Ready) (stepMany 40 w1)
  w3 <- sendOrFail ShotRequested w2
  expect "a fresh puck is dry, even, unsettled and at room temperature"
    (w3.plant.puckWetMl == 0.0 && w3.plant.puckEvenness == 1.0 && w3.plant.puckSettled == 0.0 && w3.plant.puckC == w3.plant.k.ambientC)
  let
    follow ok w =
      if not (shotInProgress w.ctl) then ok
      else
        let
          next = stepWorld w
          p = next.plant
          sound =
            p.puckEvenness <= w.plant.puckEvenness && p.puckEvenness >= 0.0
              && p.puckSettled >= w.plant.puckSettled && p.puckSettled <= 1.0
              && p.extracted >= w.plant.extracted
              && p.dissolved >= 0.0 && p.dissolved <= p.k.soakCapacity
              && p.extracted + p.dissolved <= 1.0
              && p.puckC <= Profiles.classic.maxTargetC + 0.5
        in
          follow (ok && sound) next
  expect "through a shot: evenness only falls, the puck only settles, the cup only gains, and nothing leaves its range" (follow true w3)
  let after = runUntil 200.0 (\w -> not (shotInProgress w.ctl)) w3
  expect "what the forecast says of the puck is what the shot did"
    (abs ((brewed after.plant).evenness - classic.brewed.evenness) < 0.02 && abs (after.plant.extracted - classic.extracted) < 0.02)

-- | The second position on the coffee map, and advice.
character :: Effect Unit
character = do
  log "Weight, clarity and advice"
  let builtIns = toArray Profiles.all
  let pull r = forecast Params.defaults (Recipe.toProfile builtIns r) (Recipe.puckOf r)
  let made r = Flavor.character r.tempC (pull r).brewed
  let house = Recipe.house { paced = false }
  let soaked = house { soakS = 20.0 }
  log ("  house standard: weight " <> n2 (made house).weight <> ", clarity " <> n2 (made house).clarity <> ". " <> Flavor.characterNote (made house))
  log ("  with a 20 s soak: weight " <> n2 (made soaked).weight <> ", clarity " <> n2 (made soaked).clarity <> ". " <> Flavor.characterNote (made soaked))
  expect "the house standard sits between sweet and heavy" (abs (made house).weight < 0.2)
  expect "a long soak moves the cup towards sweet and clear" ((made soaked).weight < (made house).weight - 0.3)
  let hard = Flavor.character 93.0 (forecast Params.defaults straight { doseG: 18.0, resistance: 1.3, solubility: 1.0 }).brewed
  expect "a channeled puck loses clarity" (hard.clarity < 0.5 && (made house).clarity > 0.9)
  let lit c = length (filter (\n -> n.strength >= 0.35) (Flavor.expected Recipe.FullCity 0.675 c))
  expect "a blurred cup shows fewer notes than a clear one" (lit hard < lit (made house))
  expect "weight and clarity stay in range"
    (hard.weight >= -1.0 && hard.weight <= 1.0 && hard.clarity >= 0.0 && (made soaked).weight >= -1.0)
  -- Advice.
  let free = { grind: true, temp: true, soak: true, seconds: true }
  let ask held r = advise Params.defaults builtIns held r (pull r)
  expect "a balanced, even cup gets no advice" (isNothing (ask free Recipe.house))
  let light = Recipe.house { roast = Recipe.City }
  case ask free light of
    Nothing -> throw "FAILED: an under-extracted cup gets advice"
    Just advice -> do
      log ("  City beans on the house recipe: " <> advice.text)
      expect "advice for an under-extracted cup extracts more" (advice.forecast.extracted > (pull light).extracted + 0.05)
      expect "with every setting free it names a soak" (advice.recipe.soakS > 0.0 && contains (Pattern "soak") advice.text)
      expect "and says what the water temperature alone would do" (contains (Pattern "water temperature alone") advice.text)
      expect "the forecast it quotes is the forecast of the recipe it gives"
        ((pull advice.recipe).extracted == advice.forecast.extracted && advice.from == light)
      expect "a free shot time moves with the soak" (advice.recipe.seconds > light.seconds)
  let holdSoak = free { soak = false, seconds = false }
  case ask holdSoak light of
    Nothing -> throw "FAILED: advice with the soak held"
    Just advice -> do
      log ("  the same, with soak and time held: " <> advice.text)
      expect "held settings are left alone" (advice.recipe.soakS == light.soakS && advice.recipe.seconds == light.seconds)
  expect "with everything held there is no advice" (isNothing (ask { grind: false, temp: false, soak: false, seconds: false } light))
  let dark = Recipe.house { roast = Recipe.French }
  case ask free dark of
    Nothing -> throw "FAILED: an over-extracted cup gets advice"
    Just advice -> do
      log ("  French beans on the house recipe: " <> advice.text)
      expect "advice for an over-extracted cup extracts less" (advice.forecast.extracted < (pull dark).extracted - 0.03)
      expect "and does not add a soak to an even puck" (advice.recipe.soakS == 0.0)
  expect "advice never leaves the limits of the recipe"
    ( case ask free light { tempC = Recipe.tempLimits.max - 1.0, grind = Recipe.grindLimits.min } of
        Just advice -> advice.recipe.tempC <= Recipe.tempLimits.max && advice.recipe.grind >= Recipe.grindLimits.min && advice.recipe.soakS <= Recipe.soakLimits.max
        Nothing -> true
    )
  expect "advice exists for the under-extracted sample" (isJust (ask free light))

-- | The controller is the Rust core, loaded as wasm. These check the
-- | seam itself: that a machine is a value, that refusals come from the
-- | core in its own words, that the core's fixed sizes are enforced,
-- | and that single precision inside it breaks no invariant.
controllerCore :: World -> Effect Unit
controllerCore ready = do
  log "The controller core"
  -- One wasm module serves every machine. Running one machine on must
  -- not disturb another, or a forecast would move the machine on the page.
  let once = stepWorld ready
  let elsewhere = stepMany 400 ready
  let again = stepWorld ready
  expect "a machine is a value: running one on leaves another as it was"
    (elsewhere.now > once.now && sensors once == sensors again && once.ctl.act == again.ctl.act && once.ctl.state == again.ctl.state)
  let reason w event = case send event w of
        Left why -> why
        Right _ -> ""
  expect "a refusal carries the core's own reason"
    (reason initialWorld ShotRequested == "machine is not ready" && reason initialWorld SleepRequested == "machine is not awake")
  let steps n = Profiles.classic { steps = fromMaybe Profiles.classic.steps (fromArray (replicate n (head Profiles.classic.steps))) }
  expect "a profile of twelve steps is accepted" (isRight (send (ProfileSelected (steps 12)) ready))
  expect "a profile of thirteen steps is refused by the core" (contains (Pattern "more than twelve") (reason ready (ProfileSelected (steps 13))))
  expect "a profile too long to send is refused before it is sent" (contains (Pattern "too many steps") (reason ready (ProfileSelected (steps 40))))
  let
    faultAndClear w _ = case send (SensorFailed SensorOpen) w >>= send (FaultsCleared Service) of
      Right next -> next
      Left _ -> w
  let worn = foldl faultAndClear initialWorld (replicate 11 unit)
  expect "the controller remembers its newest eight faults" (length worn.ctl.faults == 8 && and (map (isJust <<< _.clearedAt) worn.ctl.faults))
  -- A limit that single precision cannot hold exactly.
  let odd = Params.defaults { config = Params.defaults.config { brewPressureLimitBar = 9.3, paceLimitBar = 2.7 } }
  first <- sendOrFail ShotRequested (setParams odd (warmUp ready))
  let one = runUntil 200.0 (\w -> not (shotInProgress w.ctl)) first
  expect "limits that single precision rounds break no invariant" (one.violations == [] && one.ctl.faults == [])
  expect "the parameters the page holds are the ones it set" ((paramsOf one).config.brewPressureLimitBar == 9.3)
  let rested = runUntil 1800.0 (\w -> w.ctl.state == Ready) (warmUp one)
  second <- sendOrFail ShotRequested rested
  let two = runUntil 200.0 (\w -> not (shotInProgress w.ctl)) second
  expect "the log of shots is kept on this side, one entry per shot"
    (map _.state two.ctl.history == [ StoppedByWeight, StoppedByWeight ] && and (map (isJust <<< _.finalGrams) two.ctl.history))

-- | A page with no interface: a machine as last heard and the backend
-- | it is linked to, with a note of everything hearing did.
type Page = { backend :: Backend, machine :: Machine, clock :: Number, notes :: Array String }

-- | Let 50 real milliseconds pass, as one frame of the page does.
frame :: Page -> Page
frame p =
  let
    clock = p.clock + 50.0
    passed = Backend.advance 50.0 p.backend
    note = case _ of
      Accepted pending -> "accepted " <> eventLabel pending.event
      Refused pending reason -> "refused " <> eventLabel pending.event <> ": " <> reason
      CameToRest -> "came to rest"
    take acc message =
      let
        result = Machine.hear Params.defaults.config clock message acc.machine
      in
        acc { machine = result.machine, notes = acc.notes <> map note result.outcomes }
    heard = foldl take p { clock = clock, backend = passed.backend } passed.heard
    waited = Machine.expire clock heard.machine
  in
    heard { machine = waited.machine, notes = heard.notes <> map (\pending -> "lost " <> eventLabel pending.event) waited.lost }

-- | Let real milliseconds pass until the page shows what is waited for,
-- | or the limit runs out.
pass :: Number -> (Page -> Boolean) -> Page -> Page
pass limitMs done p
  | done p || limitMs <= 0.0 = p
  | otherwise = pass (limitMs - 50.0) done (frame p)

wait :: Number -> Page -> Page
wait ms = pass ms (const false)

asks :: Event -> Page -> Page
asks event p =
  let
    asked = Machine.ask p.clock event Nothing p.machine
  in
    p { machine = asked.machine, backend = Backend.request asked.request p.backend }

tells :: SimCommand -> Page -> Page
tells cmd p = p { backend = Backend.command cmd p.backend }

noted :: String -> Page -> Boolean
noted text p = length (filter (_ == text) p.notes) > 0

-- | The link: the page asks and hears, late, and sometimes not at all.
-- | Everything the page shows is checked here as the page gets it,
-- | through `Sim.Machine`, with the simulated machine out of reach
-- | behind `Sim.Backend`.
link :: Effect Unit
link = do
  log "The link between the page and the machine"
  let
    opened =
      tells (SetLink _ { delayMs = 200.0 })
        { backend: Backend.create Params.defaults, machine: Machine.fresh Profiles.classic, clock: 0.0, notes: [] }
  let early = wait 150.0 opened
  expect "nothing is heard before the first report has crossed the link" (not early.machine.heard)
  let listening = wait 350.0 early
  let behind = (Backend.view listening.backend).now - listening.machine.at
  log ("  with 200 ms each way, the page is " <> n2 behind <> " s behind the machine")
  expect "the page hears the machine, and what it shows is behind it by the delay or more" (listening.machine.heard && behind >= 0.19 && behind < 0.4)

  let woken = asks (WakeRequested Full) listening
  let soon = wait 150.0 woken
  expect "a request is not answered at once: the page still shows a sleeping machine" (soon.machine.status.state == Sleeping && length soon.machine.pending == 1)
  let awake = pass 2000.0 (noted "accepted wake") soon
  let seen = pass 1000.0 (\p -> p.machine.status.state == Heating) awake
  expect "the answer arrives later and the request stops waiting" (noted "accepted wake" awake && length awake.machine.pending == 0)
  expect "then the reports show the machine heating" (seen.machine.status.state == Heating)
  expect "an answer takes at least the delay each way" (awake.clock - woken.clock >= 400.0)

  let hasty = pass 2000.0 (\p -> length p.notes > length seen.notes) (asks ShotRequested seen)
  expect "a refusal arrives as an answer, in the controller's own words" (noted "refused start shot: machine is not ready" hasty)

  let ready = pass 60000.0 (\p -> p.machine.status.state == Ready) (tells WarmUp hasty)
  expect "the page sees the machine come ready by itself" (ready.machine.status.state == Ready && noted "came to rest" ready)
  expect "and its log, built from the reports, says so" (map _.text (last ready.machine.happenings) == Just "Ready.")

  -- A whole shot, seen only through the link.
  let pulling = wait 5000.0 (asks ShotRequested ready)
  expect "a running shot shows with the profile the page last had accepted"
    (map (\shot -> shot.profile.name) pulling.machine.shot == Just "Classic 9 bar" && noted "accepted start shot" pulling)
  let pulled = pass 90000.0 (\p -> length p.machine.history == 1) pulling
  let first = pulled.machine.trace
  log ("  one shot through the link: " <> show (map _.state pulled.machine.history) <> ", " <> show (length first) <> " chart points, " <> n1 (fromMaybe 0.0 (last pulled.machine.history >>= _.finalGrams)) <> " g")
  expect "the page logs the shot when the machine reports it ended"
    (map _.state pulled.machine.history == [ StoppedByWeight ] && map (\grams -> abs (grams - 36.0) < 1.0) (last pulled.machine.history >>= _.finalGrams) == Just true)
  expect "the chart was drawn from the reports, in order" (length first > 100 && and (zipWith (\a b -> a.t < b.t) first (drop 1 first)))
  expect "the flow on the chart is the controller's estimate, and it is there" (fromMaybe 0.0 (maximum (map _.mlPerS first)) > 1.0)
  expect "the slow traces are built from the reports too" (length pulled.machine.scope > 10)

  -- Reports dropped on the way.
  let again = pass 60000.0 (\p -> p.machine.status.state == Ready) (tells WarmUp (tells (SetLink _ { dropEvery = 3 }) pulled))
  let lossy = pass 90000.0 (\p -> length p.machine.history == 2) (asks ShotRequested again)
  let second = lossy.machine.trace
  log ("  with every third report dropped: " <> show (length second) <> " chart points")
  expect "with reports dropped the shot is still logged, once" (map _.state lossy.machine.history == [ StoppedByWeight, StoppedByWeight ])
  expect "and its chart has gaps but is still a chart" (length second < length first && length second > length first / 2)

  -- The link cut in the middle of a shot.
  let third = pass 60000.0 (\p -> p.machine.status.state == Ready) (tells WarmUp (tells (SetLink _ { dropEvery = 0 }) lossy))
  let running = wait 5000.0 (asks ShotRequested third)
  let cut = wait 4000.0 (tells (SetLink _ { cut = true }) running)
  expect "with the link cut the page goes on showing the last it heard"
    (isJust cut.machine.shot && cut.machine.at == (wait 200.0 cut).machine.at && Machine.silence cut.clock cut.machine > 3000.0)
  let unanswered = wait 4000.0 (asks ShotStopRequested cut)
  expect "a request into a cut link is given up on" (noted "lost stop" unanswered && length unanswered.machine.pending == 0)
  expect "and the machine never got it: the shot ran on by itself" ((Backend.view unanswered.backend).puck.extracted > 0.0)
  let back = pass 90000.0 (\p -> length p.machine.history == 3) (tells (SetLink _ { cut = false }) (wait 30000.0 unanswered))
  expect "when the link returns the page catches up, and logs the shot it missed the end of once"
    (map _.state back.machine.history == [ StoppedByWeight, StoppedByWeight, StoppedByWeight ] && isNothing back.machine.shot && Machine.silence back.clock back.machine < 500.0)
  let later = wait 3000.0 back
  expect "catching up does not log it twice" (length later.machine.history == 3)

  -- The machine restarts under the page.
  let restarted = wait 2000.0 (tells (Reset Params.defaults) later)
  expect "a machine whose clock has gone backwards is taken for a restart"
    (restarted.machine.at < later.machine.at && map _.text (last restarted.machine.happenings) == Just "The machine restarted.")
  expect "and its counters starting again add nothing to the log" (length restarted.machine.history == 3 && length restarted.machine.scope < 5)

  -- The fader. On a profile the motor carries it, eased. Taken in
  -- hand it is the pressure, between the manual limits.
  let cfg = Params.defaults.config
  let manual p = p.machine.status.shot >>= _.manual
  let target p = fromMaybe 0.0 p.machine.status.pressureTargetBar
  let
    watch frames p = foldl
      ( \acc _ ->
          let
            next = frame acc.page
            moved = abs (next.machine.status.fader - acc.page.machine.status.fader)
          in
            { page: next, most: max acc.most moved, top: max acc.top next.machine.status.fader }
      )
      { page: p, most: 0.0, top: 0.0 }
      (replicate frames unit)
  let followed = watch 400 (asks ShotRequested ready)
  log ("  on the profile the fader rose to " <> n2 followed.top <> " of its travel, and never moved more than " <> n2 followed.most <> " between reports")
  expect "on a profile the motor carries the fader up to the pressure asked for" (followed.top > 0.8 && followed.top <= 0.9001)
  expect "and it moves smoothly: no jump from one report to the next" (followed.most < 0.03)
  let rested = wait 3000.0 (pass 90000.0 (\p -> isNothing p.machine.status.shot) followed.page)
  expect "after the shot the fader goes back to rest" (rested.machine.status.fader < 0.01)

  let again2 = pass 60000.0 (\p -> p.machine.status.state == Ready) (tells WarmUp rested)
  let taken = wait 6000.0 (asks (FaderTaken 6.0) (wait 3000.0 (asks ShotRequested again2)))
  log ("  taken in hand at 6 bar: setpoint " <> n1 (target taken) <> " bar, measured " <> n1 taken.machine.sensors.brewBar <> " bar")
  expect "taken in hand, the fader is the pressure setpoint" (manual taken == Just 6.0 && target taken == 6.0 && abs (taken.machine.status.fader - 0.6) < 0.001)
  let stood = wait 12000.0 taken
  log ("  12 s later, with the puck full: measured " <> n1 stood.machine.sensors.brewBar <> " bar")
  expect "and once the puck has filled the pump brings the pressure to it" (abs (stood.machine.sensors.brewBar - 6.0) < 0.5)
  expect "in manual mode the steps stand still" (map _.stepIndex stood.machine.status.shot == map _.stepIndex taken.machine.status.shot && manual stood == Just 6.0)
  let high = wait 1000.0 (asks (FaderTaken 50.0) stood)
  let low = wait 1000.0 (asks (FaderTaken 0.0) high)
  expect "the machine holds the hand between the manual limits" (manual high == Just cfg.manualMaxBar && manual low == Just cfg.manualMinBar)
  let drawn = map _.askedBar low.machine.trace
  expect "the curve drawn by hand is on the page's chart"
    (length (filter (_ == Just 6.0) drawn) > 100 && length (filter (_ == Just cfg.manualMaxBar) drawn) > 5 && last drawn == Just (Just cfg.manualMinBar))
  let given = wait 1000.0 (asks FaderGiven low)
  let risen = wait 3000.0 given
  log ("  given back at " <> n1 cfg.manualMinBar <> " bar: setpoint " <> n1 (target given) <> " bar after 1 s and " <> n1 (target risen) <> " after 4 s, fader at " <> n2 risen.machine.status.fader)
  expect "given back, the profile carries on" (manual given == Nothing && isJust given.machine.status.shot && noted "accepted follow the profile" given)
  let finished = pass 90000.0 (\p -> isNothing p.machine.status.shot) given
  expect "and the shot ends on its weight as the profile says" (map _.state (last finished.machine.history) == Just StoppedByWeight)

  -- A shot pulled by hand from the first drop: start it and take the
  -- fader in the same breath, as pushing the fader up from rest does.
  let hand = pass 60000.0 (\p -> p.machine.status.state == Ready) (tells WarmUp (wait 2000.0 finished))
  let whole = wait 4000.0 (asks (FaderTaken 5.0) (asks ShotRequested hand))
  expect "a shot can be pulled by hand from the start" (manual whole == Just 5.0 && noted "accepted the fader" whole)
  let ended = pass 120000.0 (\p -> isNothing p.machine.status.shot) (wait 2000.0 (asks (FaderTaken 9.0) whole))
  expect "a shot by hand still stops on its weight" (map _.state (last ended.machine.history) == Just StoppedByWeight)
  let stray = wait 1000.0 (asks (FaderTaken 7.0) ended)
  expect "the fader moved with no shot running is refused" (noted "refused the fader: no shot is running" stray)
  expect "the fader broke no invariant" ((Backend.view stray.backend).violations == [])

  -- The selector knob chooses what the fader stands for.
  let water p = p.machine.status.shot >>= _.manualC
  let rewarmed = pass 60000.0 (\p -> p.machine.status.state == Ready) (tells WarmUp (wait 2000.0 stray))
  let onTemp = wait 500.0 (asks (FaderModeSelected OnTemperature) rewarmed)
  expect "the selector turns with the machine at rest" (onTemp.machine.status.faderMode == OnTemperature && noted "accepted the selector to temp" onTemp)
  let shown = wait 4000.0 (asks ShotRequested onTemp)
  log ("  on temperature the motor carried the fader to " <> n2 shown.machine.status.fader <> " of its travel for water asked for at " <> n1 (fromMaybe 0.0 shown.machine.status.mixTargetC) <> " C")
  expect "on temperature the motor carries the fader to the water asked for"
    (abs (shown.machine.status.fader - (fromMaybe 0.0 shown.machine.status.mixTargetC - 80.0) / 20.0) < 0.02)
  let cooler = wait 2000.0 (asks (FaderTaken 88.0) shown)
  expect "taken in hand on temperature, the fader is the water asked for"
    (water cooler == Just 88.0 && cooler.machine.status.mixTargetC == Just 88.0 && manual cooler == Nothing && abs (cooler.machine.status.fader - 0.4) < 0.001)
  let stepped = pass 60000.0 (\p -> map _.stepIndex p.machine.status.shot > map _.stepIndex cooler.machine.status.shot) cooler
  expect "and the profile's steps carry on with that water"
    (map _.stepIndex stepped.machine.status.shot > map _.stepIndex cooler.machine.status.shot && water stepped == Just 88.0)
  let hot = wait 1000.0 (asks (FaderTaken 150.0) stepped)
  expect "the machine holds the water between the manual limits" (water hot == Just cfg.manualMaxC)
  let onTime = wait 500.0 (asks (FaderModeSelected OnTime) hot)
  expect "turning the selector lets go of what the hand held" (water onTime == Nothing && onTime.machine.status.faderMode == OnTime && isJust onTime.machine.status.shot)
  let locked = wait 1000.0 (asks (FaderTaken 0.9) onTime)
  expect "on time a hand on the fader is refused" (noted "refused the fader: the selector is on time, where the fader only shows" locked)
  let run5 = wait 5000.0 locked
  log ("  on time the fader moved from " <> n2 locked.machine.status.fader <> " to " <> n2 run5.machine.status.fader <> " of its travel in 5 s")
  expect "on time the motor carries the fader along with the clock" (run5.machine.status.fader > locked.machine.status.fader + 0.02)
  let timed = pass 120000.0 (\p -> isNothing p.machine.status.shot) run5
  let turned = wait 3000.0 (asks (FaderModeSelected OnPressure) timed)
  expect "the shot ends and the fader goes back to rest" (turned.machine.status.fader < 0.01 && turned.machine.status.faderMode == OnPressure)
  expect "the selector broke no invariant" ((Backend.view turned.backend).violations == [])

  -- The time a heating machine has to go, which the display shows.
  let asleep = wait 1000.0 (tells (Reset Params.defaults) turned)
  let cold = wait 1000.0 (asks (WakeRequested Full) asleep)
  let promised = Backend.readyAt cold.backend
  let arrived = pass 2400000.0 (\p -> p.machine.status.state == Ready) (tells (SetSpeed 60) cold)
  log ("  a cold machine is promised ready at " <> n1 (fromMaybe 0.0 promised / 60.0) <> " min and is heard ready at " <> n1 (arrived.machine.at / 60.0) <> " min")
  expect "a heating machine is ready when the forecast said it would be, to within what the link adds"
    (cold.machine.status.state == Heating && maybe false (\at -> abs (at - arrived.machine.at) < 3.0) promised)
  expect "a machine that is asleep has no such forecast" (isNothing (Backend.readyAt asleep.backend))

  -- The same run gives the same messages.
  let rerun = pass 90000.0 (\p -> length p.machine.history == 1) (wait 5000.0 (asks ShotRequested ready))
  expect "the same run of the link gives the same shot to the last digit"
    (map _.finalGrams rerun.machine.history == map _.finalGrams pulled.machine.history && rerun.clock == pulled.clock && map _.bar rerun.machine.trace == map _.bar first)

-- | The colours of the night face.
tints :: Effect Unit
tints = do
  log "Cool to warm"
  let steps = map (\i -> Tint.coolToWarm (toNumber i / 10.0)) (range 0 10)
  log ("  " <> joinWith " " steps)
  expect "the ends are the two colours themselves" (Tint.coolToWarm 0.0 == Tint.cool && Tint.coolToWarm 1.0 == Tint.warm)
  expect "a fraction outside 0 to 1 is held to the nearer end" (Tint.coolToWarm (-3.0) == Tint.cool && Tint.coolToWarm 7.0 == Tint.warm)
  expect "a colour blended with itself is itself" (Tint.blend "#264a3a" "#264a3a" 0.37 == "#264a3a")
  let lights = map Tint.lightness steps
  let gaps = zipWith (\x y -> x - y) lights (drop 1 lights)
  log ("  lightness falls from " <> n2 (Tint.lightness Tint.cool) <> " to " <> n2 (Tint.lightness Tint.warm))
  expect "the lightness changes by the same amount at every step, to within the rounding of a colour to 8 bits"
    (all (\gap -> abs (gap - (Tint.lightness Tint.cool - Tint.lightness Tint.warm) / 10.0) < 0.004) gaps)
  expect "black is 0 and white is 1" (Tint.lightness "#000000" < 0.001 && abs (Tint.lightness "#ffffff" - 1.0) < 0.001)

main :: Effect Unit
main = do
  tints
  coffeeMap
  beans
  character
  curveShapes
  shapes
  recipes
  pacing
  cold <- coldStart
  expect "no invariant broken during warm-up" (cold.violations == [])
  holdsReady cold
  idleSleepAndWake cold
  let warm = warmUp cold
  for_ (toArray Profiles.all) \profile -> do
    after <- pullShot warm profile
    expect "no invariant broken" (after.violations == [])
  coldBranchFailure warm
  inletLoss warm
  steaming warm
  mechanicalRelief warm
  rampIsFollowed warm
  preInfusion warm
  controllerCore warm
  link
  parameters cold
  log "All scenarios passed."
