-- | Headless scenarios. Each drives the simulated machine through a
-- | sequence of events, prints what happened, and fails the run if an
-- | expectation or any spec invariant is broken.
module Test.Main (main) where

import Prelude

import Data.Array (drop, last, length, zipWith)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..), isLeft, isRight)
import Data.Foldable (and, foldl, for_, maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Data.Number.Format (fixed, toStringWith)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Sim.Controller (Event(..), shotInProgress)
import Sim.Cup as Cup
import Sim.Curve as Curve
import Sim.Forecast (forecast)
import Sim.Params as Params
import Sim.Profiles as Profiles
import Sim.Recipe as Recipe
import Sim.Shapes as Shapes
import Sim.Types (Curve(..), FaultKind(..), MachineState(..), PowerMode(..), Profile, ShotState(..), StepMode(..), defaultConfig)
import Sim.World (World, dt, initialWorld, paramsOf, send, sensors, setParams, stepMany, stepWorld, warmUp, worldWith)

n1 :: Number -> String
n1 = toStringWith (fixed 1)

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
  expect "the log says why" (map _.bad (last asleep.happenings) == Just true)
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
  let cooled p = (sensors (runUntil 600.0 (const false) (setParams p ready { ctl = ready.ctl { requestedMode = Asleep, state = Sleeping } }))).groupC
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
  let slowFree = forecast (puck 1.4) free plain
  let slowPaced = forecast (puck 1.4) paced plain
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
      let kept = light { name = "Gesha", roaster = "A roaster", roastedOn = "2026-11-02", paced = false, grind = -1.5 }
      expect "stored recipes read back the same" (Recipe.decode (Recipe.encode [ kept, dark ]) == [ kept, dark ])
    _ -> throw "expected three built-in recipes"
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
  let starts = map start Recipe.roasts
  let falling pick = and (zipWith (\a b -> pick a >= pick b) starts (drop 1 starts))
  expect "a darker roast never starts hotter, longer or finer than a lighter one"
    (falling _.tempC && falling _.ratio && falling _.seconds && falling (negate <<< _.grind))
  expect "a recipe stored under the first three names still reads"
    (map (Recipe.roastLabel <<< _.roast) (Recipe.decode "B\t0\t18.0\t2.0\t93.0\tR\t2026\tDark\t26.0\tpaced\t0.0") == [ "French" ])
  where
  plainRecipe = { name: "", roaster: "", roastedOn: "", roast: Recipe.FullCity, profileIndex: 0, doseG: 18.0, ratio: 2.0, tempC: 93.0, seconds: 28.0, paced: true, grind: 0.0 }

main :: Effect Unit
main = do
  beans
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
  rampIsFollowed warm
  parameters cold
  log "All scenarios passed."
