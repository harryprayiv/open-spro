-- | The machine as the page knows it: the last report it heard, and
-- | what it has pieced together from the reports so far. The page shows
-- | this and nothing else about the machine. It is always a little out
-- | of date, and when the link goes quiet it stays as it was.
-- |
-- | The machine reports little about the past, so the page keeps it:
-- | the chart of the shot, the log of ended shots, the slow thermal
-- | traces and the log of what the machine did by itself are all built
-- | here, one report at a time. A report that never arrives leaves a
-- | gap in them.
-- |
-- | Requests are remembered until they are answered. An answer that
-- | does not come within `answerWithin` is given up on.
module Sim.Machine
  ( Machine
  , Pending
  , Outcome(..)
  , answerWithin
  , fresh
  , ask
  , hear
  , expire
  , silence
  , busy
  ) where

import Prelude

import Data.Array (filter, find, last, length, partition, snoc, takeEnd)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Sim.Link (Event(..), Happening, Message(..), Request, Sample, ScopeSample, Status, Telemetry, restingStatus, shotEnded, shotNow)
import Sim.Recipe (Recipe)
import Sim.Types (Config, MachineState(..), Profile, Sensors, Shot, ShotState(..), TwoWay(..))

-- | A request that has been sent and not yet answered. A request that
-- | changed the recipe carries the recipe from before, to go back to if
-- | the machine refuses it or never answers.
type Pending = { id :: Int, event :: Event, sentAt :: Number, undo :: Maybe Recipe }

type Machine =
  { -- Whether any report has arrived, and when on the page's clock, in
    -- milliseconds, the last one did.
    heard :: Boolean
  , heardAt :: Number
  -- The last report: the machine's clock, its sensors and its status.
  , at :: Number
  , sensors :: Sensors
  , status :: Status
  -- The profile the machine last accepted.
  , selected :: Profile
  -- Pieced together from the reports.
  , shot :: Maybe Shot
  , history :: Array Shot
  , trace :: Array Sample
  , traceOf :: Number
  , lastSampleAt :: Number
  , happenings :: Array Happening
  , scope :: Array ScopeSample
  , lastScopeAt :: Number
  -- Requests awaiting an answer, and the number the next one will get.
  , pending :: Array Pending
  , nextId :: Int
  -- A request that puts the machine to sleep has been accepted and no
  -- report has arrived since.
  , askedToRest :: Boolean
  }

-- | What hearing a message did, for the page to act on.
data Outcome
  = Accepted Pending
  | Refused Pending String
  -- The machine became ready, went to sleep or faulted by itself.
  | CameToRest

-- | Real milliseconds a request waits for its answer.
answerWithin :: Number
answerWithin = 3000.0

-- | Seconds of machine time between points of the shot chart and of
-- | the slow traces, and how many of each are kept.
sampleEvery :: Number
sampleEvery = 0.1

scopeEvery :: Number
scopeEvery = 2.0

scopeKept :: Int
scopeKept = 900

-- | A machine nothing has been heard from. `profile` is what it is
-- | taken to have selected until it accepts another.
fresh :: Profile -> Machine
fresh profile =
  { heard: false
  , heardAt: 0.0
  , at: 0.0
  , sensors:
      { inletBar: 0.0
      , inletConductivityUs: 0.0
      , inletC: 0.0
      , brewBar: 0.0
      , brewBoilerC: 0.0
      , mixC: 0.0
      , groupC: 0.0
      , groupWaterC: 0.0
      , steamBar: 0.0
      , steamLevelOk: true
      , cupGrams: 0.0
      }
  , status: restingStatus
  , selected: profile
  , shot: Nothing
  , history: []
  , trace: []
  , traceOf: -1.0
  , lastSampleAt: 0.0
  , happenings: []
  , scope: []
  , lastScopeAt: 0.0
  , pending: []
  , nextId: 1
  , askedToRest: false
  }

-- | Whether the machine, as last heard, would refuse a change of
-- | profile. The page checks this before it asks, so the barista is
-- | told at once. The machine still has the last word.
busy :: Machine -> Maybe String
busy m
  | isJust m.status.shot = Just "a shot is running"
  | m.status.state == Faulted = Just "machine is faulted"
  | otherwise = Nothing

-- | Real milliseconds since the last report, or since the page began
-- | listening if there has been none.
silence :: Number -> Machine -> Number
silence clock m = max 0.0 (clock - m.heardAt)

-- | Make a request and remember it. `clock` is the page's clock.
ask :: Number -> Event -> Maybe Recipe -> Machine -> { machine :: Machine, request :: Request }
ask clock event undo m =
  { machine: m { pending = snoc m.pending { id: m.nextId, event, sentAt: clock, undo }, nextId = m.nextId + 1 }
  , request: { id: m.nextId, event }
  }

-- | Give up on requests that have waited too long.
expire :: Number -> Machine -> { machine :: Machine, lost :: Array Pending }
expire clock m =
  let
    split = partition (\p -> clock - p.sentAt > answerWithin) m.pending
  in
    { machine: m { pending = split.no }, lost: split.yes }

hear :: Config -> Number -> Message -> Machine -> { machine :: Machine, outcomes :: Array Outcome }
hear cfg clock message m = case message of
  Answer answer -> case find (\p -> p.id == answer.id) m.pending of
    -- An answer to a request already given up on is ignored.
    Nothing -> { machine: m, outcomes: [] }
    Just p ->
      let
        left = m { pending = filter (\q -> q.id /= p.id) m.pending }
      in
        case answer.refused of
          Just reason -> { machine: left, outcomes: [ Refused p reason ] }
          Nothing ->
            { machine: case p.event of
                ProfileSelected profile -> left { selected = profile }
                SleepRequested -> left { askedToRest = true }
                FaultsCleared _ -> left { askedToRest = true }
                _ -> left
            , outcomes: [ Accepted p ]
            }
  Report t -> report cfg clock t m

report :: Config -> Number -> Telemetry -> Machine -> { machine :: Machine, outcomes :: Array Outcome }
report cfg clock t m =
  let
    before = m.status
    after = t.status
    -- The first report, or a clock that has gone backwards because the
    -- machine restarted: there is no earlier report to compare with.
    restarted = m.heard && t.at < m.at
    first = not m.heard || restarted
    ended = after.shotsEnded /= before.shotsEnded
    history = case after.lastShot of
      Just end | ended && not first -> snoc m.history (shotEnded m.selected end)
      _ -> m.history
    -- The chart follows the running shot and keeps the last one.
    traced = case after.shot of
      Nothing -> { trace: m.trace, traceOf: m.traceOf, lastSampleAt: m.lastSampleAt }
      Just shot ->
        let
          new = shot.startedAt /= m.traceOf
          kept = if new then [] else m.trace
          due = new || (t.at - m.lastSampleAt >= sampleEvery - 1.0e-9 && length kept < 2000)
          sample =
            { t: t.at - shot.startedAt
            , bar: t.sensors.brewBar
            , mlPerS: after.flowMlPerS
            , mixC: t.sensors.mixC
            , groupWaterC: t.sensors.groupWaterC
            , targetC: fromMaybe t.sensors.mixC after.mixTargetC
            , grams: t.sensors.cupGrams
            , step: shot.stepIndex
            , askedBar: after.pressureTargetBar
            , paceBar: after.paceTrimBar
            }
        in
          { trace: if due then snoc kept sample else kept
          , traceOf: shot.startedAt
          , lastSampleAt: if due then t.at else m.lastSampleAt
          }
    scopeDue = first || t.at - m.lastScopeAt >= scopeEvery - 1.0e-9
    scopeSample =
      { t: t.at
      , reservoirC: t.sensors.brewBoilerC
      , groupC: t.sensors.groupC
      , steamBar: t.sensors.steamBar
      , heaterW: toNumber after.heaterWatts
      }
    scopeBefore = if restarted then [] else m.scope
    said =
      if restarted then [ { at: t.at, text: "The machine restarted.", bad: true } ]
      else if first then []
      else changes cfg m.askedToRest t.at before after
    settled = after.state /= before.state && (after.state == Ready || after.state == Sleeping || after.state == Faulted)
  in
    { machine: m
        { heard = true
        , heardAt = clock
        , at = t.at
        , sensors = t.sensors
        , status = after
        , shot = map (shotNow m.selected) after.shot
        , history = history
        , trace = traced.trace
        , traceOf = traced.traceOf
        , lastSampleAt = traced.lastSampleAt
        , happenings = takeEnd 8 (m.happenings <> said)
        , scope = if scopeDue then takeEnd scopeKept (snoc scopeBefore scopeSample) else scopeBefore
        , lastScopeAt = if scopeDue then t.at else m.lastScopeAt
        , askedToRest = false
        }
    , outcomes:
        if not first && settled then [ CameToRest ] else []
    }

-- | Describe what the machine did between two reports. `asked` says the
-- | page had just had a request accepted that puts the machine to
-- | sleep, so going to sleep is no news.
changes :: Config -> Boolean -> Number -> Status -> Status -> Array Happening
changes cfg asked now before after =
  let
    say bad text = [ { at: now, text, bad } ]
    stateChange
      | before.state == after.state = []
      | after.state == Faulted =
          say true
            ( "Fault: " <> maybe "unknown" (show <<< _.kind) (last after.faults)
                <> ". Heaters are off and the machine needs clearing under Service."
            )
      | after.state == Ready = say false "Ready."
      | after.state == Sleeping = if asked then [] else sleepLine
      | before.state == Ready && after.state == Heating =
          say false "Dropped out of ready while a temperature recovers."
      | otherwise = []
    sleepLine = say true ("Went to sleep by itself after " <> show (round (cfg.idleSleepAfter / 60.0)) <> " idle minutes. Wake it to carry on.")
    fillChange
      | before.act.fillValve == Closed && after.act.fillValve == Open = say false "Steam boiler filling."
      | otherwise = []
    shotChange = case after.lastShot of
      Just shot | after.shotsEnded /= before.shotsEnded ->
        say (shot.state == Aborted) ("Shot ended: " <> show shot.state <> ".")
      _ -> []
  in
    stateChange <> fillChange <> shotChange
