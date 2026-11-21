-- | A simulated machine at the far end of a link. It is the simulated
-- | world with a delay in front of it: requests reach it some time
-- | after they are sent, it reports at its own rate, and its reports
-- | and answers reach the page some time after that. Reports can be
-- | dropped and the link can be cut.
-- |
-- | The page gets nothing from here but messages. What the simulator
-- | alone knows (the puck, the disturbances, the invariants it has seen
-- | broken) comes out through `view`, and the things only a simulator
-- | can do go in through `command`. A real machine has neither.
-- |
-- | A `Backend` is a value and `advance` is a pure function of the time
-- | that has passed, so the same run always gives the same messages.
module Sim.Backend
  ( Backend
  , LinkSettings
  , SimCommand(..)
  , SimView
  , PuckView
  , defaultLink
  , reportEvery
  , create
  , advance
  , request
  , command
  , view
  , readyAt
  ) where

import Prelude

import Data.Array (length, partition, snoc)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Sim.Controller (status)
import Sim.Link (Message(..), Request)
import Sim.Params (Params)
import Sim.Plant (Brewed, Disturbance, Puck, brewed)
import Sim.World (World, dt, readyIn, send, sensors, setParams, stepWorld, warmUp, worldWith)

-- | How the link behaves. The delay applies to each direction, so a
-- | request is answered after twice the delay at the least.
type LinkSettings =
  { delayMs :: Number
  -- Drop every nth report. 0 drops none.
  , dropEvery :: Int
  -- Nothing gets through in either direction.
  , cut :: Boolean
  }

defaultLink :: LinkSettings
defaultLink = { delayMs: 60.0, dropEvery: 0, cut: false }

-- | Seconds of machine time between reports: twenty a second.
reportEvery :: Number
reportEvery = 0.05

newtype Backend = Backend
  { world :: World
  -- Simulated seconds per real second.
  , speed :: Int
  , link :: LinkSettings
  -- Real milliseconds since the backend was made.
  , wall :: Number
  , toPage :: Array { due :: Number, message :: Message }
  , toMachine :: Array { due :: Number, request :: Request }
  , lastReportAt :: Number
  , reports :: Int
  }

-- | What only a simulator can be told to do.
data SimCommand
  = SetSpeed Int
  | Disturb (Disturbance -> Disturbance)
  -- Put the plant at operating temperature.
  | WarmUp
  -- A cold machine with these parameters, and an empty link.
  | Reset Params
  | SetParams Params
  -- What the barista put in the basket for the next shot.
  | LoadPuck Puck
  | SetLink (LinkSettings -> LinkSettings)

-- | The simulated puck. The real machine cannot see any of this.
type PuckView =
  { wetMl :: Number
  , settled :: Number
  , bedC :: Number
  , evenness :: Number
  , extracted :: Number
  , dissolved :: Number
  }

-- | What the simulator knows that no machine could report.
type SimView =
  { now :: Number
  , speed :: Int
  , link :: LinkSettings
  , dist :: Disturbance
  -- Every spec invariant seen broken since the machine was made,
  -- checked on every controller cycle and not only on the reports
  -- that reach the page.
  , violations :: Array String
  -- The true flow into the group and the water in the steam boiler.
  , flowMlPerS :: Number
  , steamWaterMl :: Number
  , puck :: PuckView
  , brewed :: Brewed
  -- Messages on their way to the page and requests on their way to
  -- the machine.
  , inFlight :: Int
  }

-- | A cold, sleeping machine with the given parameters.
create :: Params -> Backend
create params = Backend
  { world: worldWith params
  , speed: 1
  , link: defaultLink
  , wall: 0.0
  , toPage: []
  , toMachine: []
  , lastReportAt: 0.0
  , reports: 0
  }

-- | Send a request towards the machine. On a cut link it is lost.
request :: Request -> Backend -> Backend
request r (Backend b) =
  if b.link.cut then Backend b
  else Backend b { toMachine = snoc b.toMachine { due: b.wall + b.link.delayMs, request: r } }

command :: SimCommand -> Backend -> Backend
command cmd (Backend b) = Backend case cmd of
  SetSpeed speed -> b { speed = clamp 1 60 speed }
  Disturb change -> b { world = b.world { dist = change b.world.dist } }
  WarmUp -> b { world = warmUp b.world }
  Reset params -> b { world = worldWith params, speed = 1, toPage = [], toMachine = [], lastReportAt = 0.0, reports = 0 }
  SetParams params -> b { world = setParams params b.world }
  LoadPuck puck -> b { world = b.world { puck = puck } }
  SetLink change -> b { link = change b.link }

-- | The time on the machine's clock at which a heating machine will
-- | report ready if it is left alone. This runs the simulation forward
-- | to find out, which takes the better part of a second, so it is
-- | asked for once and not on every frame.
readyAt :: Backend -> Maybe Number
readyAt (Backend b) = map (_ + b.world.now) (readyIn b.world)

view :: Backend -> SimView
view (Backend b) =
  let
    p = b.world.plant
  in
    { now: b.world.now
    , speed: b.speed
    , link: b.link
    , dist: b.world.dist
    , violations: b.world.violations
    , flowMlPerS: p.flowMlPerS
    , steamWaterMl: p.steamWaterMl
    , puck:
        { wetMl: p.puckWetMl
        , settled: p.puckSettled
        , bedC: p.puckC
        , evenness: p.puckEvenness
        , extracted: p.extracted
        , dissolved: p.dissolved
        }
    , brewed: brewed p
    , inFlight: length b.toPage + length b.toMachine
    }

-- | Let `ms` real milliseconds pass. Requests that have reached the
-- | machine are handled, the machine runs for that long at the present
-- | speed and reports as it goes, and whatever has reached the page by
-- | the end is handed over in the order it was sent.
advance :: Number -> Backend -> { backend :: Backend, heard :: Array Message }
advance ms (Backend before) =
  let
    wall = before.wall + ms
    arriving = partition (\entry -> entry.due <= wall) before.toMachine
    handled = foldl answer before { wall = wall, toMachine = arriving.no } arriving.yes
    steps = round (ms / (dt * 1000.0)) * handled.speed
    ran = run steps handled
    landing = partition (\entry -> entry.due <= wall) ran.toPage
  in
    { backend: Backend ran { toPage = landing.no }
    , heard: map _.message landing.yes
    }
  where
  -- A message leaves for the page, unless the link is cut.
  post message b =
    if b.link.cut then b
    else b { toPage = snoc b.toPage { due: b.wall + b.link.delayMs, message } }

  answer b entry = case send entry.request.event b.world of
    Left reason -> post (Answer { id: entry.request.id, refused: Just reason }) b
    Right world -> post (Answer { id: entry.request.id, refused: Nothing }) b { world = world }

  run n b
    | n <= 0 = b
    | otherwise = run (n - 1) (report b { world = stepWorld b.world })

  report b =
    if b.world.now - b.lastReportAt < reportEvery - 1.0e-9 then b
    else
      let
        count = b.reports + 1
        dropped = b.link.dropEvery > 0 && count `mod` b.link.dropEvery == 0
        counted = b { lastReportAt = b.world.now, reports = count }
        telemetry = { at: b.world.now, sensors: sensors b.world, status: status b.world.ctl }
      in
        if dropped then counted else post (Report telemetry) counted
