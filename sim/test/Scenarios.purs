-- | Headless scenarios. Each drives the simulated machine through a
-- | sequence of events, prints what happened, and fails the run if an
-- | expectation or any spec invariant is broken.
module Test.Main (main) where

import Prelude

import Data.Array (last)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..), isLeft, isRight)
import Data.Foldable (foldl, for_, maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Data.Number.Format (fixed, toStringWith)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Sim.Controller (Event(..), shotInProgress)
import Sim.Profiles as Profiles
import Sim.Types (FaultKind(..), MachineState(..), PowerMode(..), Profile, ShotState(..), config)
import Sim.World (World, dt, initialWorld, send, sensors, stepMany, stepWorld, warmUp)

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
  let almost = runUntil (config.idleSleepAfter - 30.0) (const false) ready
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

main :: Effect Unit
main = do
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
  log "All scenarios passed."