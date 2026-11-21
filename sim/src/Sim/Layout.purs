-- | How the page is arranged: three panes. The machine pane shows the
-- | front, the inside and the shot. The controls pane holds every
-- | control behind tabs. The bench pane holds testing and problems and
-- | can be put away. Every edge between two things on the page can be
-- | dragged, and the sizes that sets are kept here.
module Sim.Layout
  ( Tab(..)
  , BenchTab(..)
  , View
  , defaultView
  , tabs
  , tabTitle
  , benchTabs
  , benchTitle
  , encode
  , decode
  , Edge(..)
  , Sizes
  , defaultSizes
  , dragEdge
  , upright
  , sizeStyle
  , encodeSizes
  , decodeSizes
  ) where

import Prelude

import Data.Array (find, index)
import Data.Maybe (Maybe, fromMaybe)
import Data.Number as Number
import Data.String (Pattern(..), joinWith, split)

-- | The tabs of the controls pane.
data Tab = Brew | Cup | Shape | Tuning

derive instance eqTab :: Eq Tab

-- | The tabs of the bench pane.
data BenchTab = Faults | Readings | Scope

derive instance eqBenchTab :: Eq BenchTab

type View =
  { tab :: Tab
  , benchTab :: BenchTab
  , benchOpen :: Boolean
  }

defaultView :: View
defaultView = { tab: Brew, benchTab: Faults, benchOpen: false }

tabs :: Array Tab
tabs = [ Brew, Cup, Shape, Tuning ]

tabTitle :: Tab -> String
tabTitle = case _ of
  Brew -> "Brew"
  Cup -> "Cup"
  Shape -> "Profile"
  Tuning -> "Settings"

benchTabs :: Array BenchTab
benchTabs = [ Faults, Readings, Scope ]

benchTitle :: BenchTab -> String
benchTitle = case _ of
  Faults -> "Problems"
  Readings -> "Readings"
  Scope -> "Scope"

-- | Three words separated by commas. The titles double as the stored
-- | names.
encode :: View -> String
encode view =
  joinWith ","
    [ tabTitle view.tab
    , benchTitle view.benchTab
    , if view.benchOpen then "open" else "away"
    ]

-- | Anything unreadable falls back to the default for that part.
decode :: String -> View
decode text = case split (Pattern ",") text of
  [ tab, benchTab, open ] ->
    { tab: fromMaybe defaultView.tab (find (\t -> tabTitle t == tab) tabs)
    , benchTab: fromMaybe defaultView.benchTab (find (\t -> benchTitle t == benchTab) benchTabs)
    , benchOpen: open == "open"
    }
  _ -> defaultView

------------------------------------------------------------
-- Sizes
------------------------------------------------------------

-- | An edge that can be dragged. Each one sets one size.
data Edge
  -- between the machine pane and the controls pane
  = ControlsEdge
  -- between the controls pane and the bench pane
  | BenchEdge
  -- between the front of the machine and its inside
  | FrontEdge
  -- under the two drawings
  | DrawingsEdge
  -- under the chart
  | ChartEdge

derive instance eqEdge :: Eq Edge

-- | The sizes the edges set, in pixels: the widths of the controls
-- | pane, the bench pane and the front drawing, and the heights of the
-- | row of drawings and the chart. The machine pane takes
-- | the width the others leave, and the inside drawing takes what the
-- | front leaves.
type Sizes =
  { controls :: Number
  , bench :: Number
  , front :: Number
  , drawings :: Number
  , chart :: Number
  }

defaultSizes :: Sizes
defaultSizes = { controls: 560.0, bench: 420.0, front: 300.0, drawings: 350.0, chart: 200.0 }

-- | Whether the edge stands upright, and so is dragged from side to
-- | side. The others lie flat and are dragged up and down.
upright :: Edge -> Boolean
upright = case _ of
  ControlsEdge -> true
  BenchEdge -> true
  FrontEdge -> true
  DrawingsEdge -> false
  ChartEdge -> false

-- | No size is let go so small that its edge is lost, or so large that
-- | it could not be dragged back.
held :: Sizes -> Sizes
held s =
  { controls: clamp 320.0 1600.0 s.controls
  , bench: clamp 260.0 1200.0 s.bench
  , front: clamp 120.0 1200.0 s.front
  , drawings: clamp 120.0 1600.0 s.drawings
  , chart: clamp 60.0 1600.0 s.chart
  }

-- | An edge dragged by this many pixels to the right and down. The
-- | panes on the right grow as their left edge moves left.
dragEdge :: Edge -> Number -> Number -> Sizes -> Sizes
dragEdge edge dx dy s = held case edge of
  ControlsEdge -> s { controls = s.controls - dx }
  BenchEdge -> s { bench = s.bench - dx }
  FrontEdge -> s { front = s.front + dx }
  DrawingsEdge -> s { drawings = s.drawings + dy }
  ChartEdge -> s { chart = s.chart + dy }

-- | The sizes as CSS variables. The stylesheet uses them on a wide
-- | screen and ignores them on a phone.
sizeStyle :: Sizes -> String
sizeStyle s =
  joinWith "; "
    [ "--controls: " <> show s.controls <> "px"
    , "--bench: " <> show s.bench <> "px"
    , "--front: " <> show s.front <> "px"
    , "--drawings: " <> show s.drawings <> "px"
    , "--chart: " <> show s.chart <> "px"
    ]

-- | Five numbers separated by commas, in the order of the record.
encodeSizes :: Sizes -> String
encodeSizes s = joinWith "," (map show [ s.controls, s.bench, s.front, s.drawings, s.chart ])

-- | A size that is missing or unreadable falls back to its default, so
-- | what an older page stored (the first two numbers) still reads.
decodeSizes :: String -> Sizes
decodeSizes text =
  let
    parts = split (Pattern ",") text
    at :: Int -> Number -> Number
    at i fallback = fromMaybe fallback (index parts i >>= readNumber)
    d = defaultSizes
  in
    held
      { controls: at 0 d.controls
      , bench: at 1 d.bench
      , front: at 2 d.front
      , drawings: at 3 d.drawings
      , chart: at 4 d.chart
      }

readNumber :: String -> Maybe Number
readNumber = Number.fromString
