-- | How the page is arranged: three panes. The machine pane shows the
-- | front, the inside and the shot. The controls pane holds every
-- | control behind tabs. The bench pane holds testing and problems and
-- | can be put away. Every edge between two things on the page can be
-- | dragged, and the sizes that sets are kept here. The two drawings in
-- | the machine pane, the front of the machine and its inside, are
-- | windows of their own: each can be dragged about by its title and
-- | resized by its corner.
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
  , Window(..)
  , Box
  , boxOf
  , boxStyle
  , tidied
  , dragClass
  , Sizes
  , defaultSizes
  , dragEdge
  , sizeStyle
  , encodeSizes
  , decodeSizes
  ) where

import Prelude

import Data.Array (find, index)
import Data.Array as Array
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

-- | One of the two drawings in the machine pane.
data Window = FrontWindow | InsideWindow

derive instance eqWindow :: Eq Window

-- | Something that can be dragged. Each one sets one size, or the
-- | place or the size of one window.
data Edge
  -- between the machine pane and the controls pane
  = ControlsEdge
  -- between the controls pane and the bench pane
  | BenchEdge
  -- under the area the two drawings share
  | DrawingsEdge
  -- under the chart
  | ChartEdge
  -- a window, by its title
  | Move Window
  -- a window, by its corner
  | Resize Window

derive instance eqEdge :: Eq Edge

-- | Where a window is in the area the drawings share, and how big, in
-- | pixels from the area's top left corner.
type Box = { x :: Number, y :: Number, w :: Number, h :: Number }

-- | The sizes the edges set, in pixels: the widths of the controls
-- | pane and the bench pane, the heights of the drawings' area and of
-- | the chart, the two windows, and which of them is in front where
-- | they overlap. The machine pane takes the width the others leave.
type Sizes =
  { controls :: Number
  , bench :: Number
  , drawings :: Number
  , chart :: Number
  , front :: Box
  , inside :: Box
  , frontOnTop :: Boolean
  }

defaultSizes :: Sizes
defaultSizes =
  { controls: 560.0
  , bench: 420.0
  , drawings: 380.0
  , chart: 200.0
  , front: { x: 0.0, y: 0.0, w: 320.0, h: 380.0 }
  , inside: { x: 328.0, y: 0.0, w: 520.0, h: 380.0 }
  , frontOnTop: false
  }

-- | The two windows put back where they start, side by side, with
-- | everything else left as it is.
tidied :: Sizes -> Sizes
tidied s = s { front = defaultSizes.front, inside = defaultSizes.inside, frontOnTop = defaultSizes.frontOnTop, drawings = defaultSizes.drawings }

boxOf :: Window -> Sizes -> Box
boxOf window s = case window of
  FrontWindow -> s.front
  InsideWindow -> s.inside

-- | A window's place and size as CSS.
boxStyle :: Box -> String
boxStyle b =
  joinWith "; "
    [ "left: " <> show b.x <> "px"
    , "top: " <> show b.y <> "px"
    , "width: " <> show b.w <> "px"
    , "height: " <> show b.h <> "px"
    -- A window that starts inside the area never runs past its right
    -- edge, so a narrow page does not cut the drawing off.
    , "max-width: max(160px, calc(100% - " <> show b.x <> "px))"
    ]

-- | The class the page wears during a drag, which sets the pointer.
dragClass :: Edge -> String
dragClass = case _ of
  ControlsEdge -> " dragging"
  BenchEdge -> " dragging"
  DrawingsEdge -> " dragging flat"
  ChartEdge -> " dragging flat"
  Move _ -> " dragging moving"
  Resize _ -> " dragging sizing"

-- | No size is let go so small that it cannot be got hold of again, or
-- | so large that it could not be dragged back. A window stays where
-- | its title can be reached.
held :: Sizes -> Sizes
held s =
  s
    { controls = clamp 320.0 1600.0 s.controls
    , bench = clamp 260.0 1200.0 s.bench
    , drawings = clamp 120.0 2400.0 s.drawings
    , chart = clamp 60.0 1600.0 s.chart
    , front = box s.front
    , inside = box s.inside
    }
  where
  box b = { x: clamp 0.0 3000.0 b.x, y: clamp 0.0 2200.0 b.y, w: clamp 140.0 3000.0 b.w, h: clamp 120.0 2400.0 b.h }

-- | Something dragged by this many pixels to the right and down. The
-- | panes on the right grow as their left edge moves left. A window
-- | that is moved or resized comes to the front, and the drawings' area
-- | grows to hold a window dragged or stretched past its bottom.
dragEdge :: Edge -> Number -> Number -> Sizes -> Sizes
dragEdge edge dx dy s = held case edge of
  ControlsEdge -> s { controls = s.controls - dx }
  BenchEdge -> s { bench = s.bench - dx }
  DrawingsEdge -> s { drawings = s.drawings + dy }
  ChartEdge -> s { chart = s.chart + dy }
  Move window -> fitted (change window \b -> b { x = b.x + dx, y = b.y + dy })
  Resize window -> fitted (change window \b -> b { w = b.w + dx, h = b.h + dy })
  where
  change window f = case window of
    FrontWindow -> s { front = f s.front, frontOnTop = true }
    InsideWindow -> s { inside = f s.inside, frontOnTop = false }
  fitted t = t { drawings = max t.drawings (max (t.front.y + t.front.h) (t.inside.y + t.inside.h)) }

-- | The sizes as CSS variables. The stylesheet uses them on a wide
-- | screen and ignores them on a phone.
sizeStyle :: Sizes -> String
sizeStyle s =
  joinWith "; "
    [ "--controls: " <> show s.controls <> "px"
    , "--bench: " <> show s.bench <> "px"
    , "--drawings: " <> show s.drawings <> "px"
    , "--chart: " <> show s.chart <> "px"
    ]

-- | Thirteen numbers separated by commas: the four sizes, the front
-- | window's box, the inside window's box, and 1 if the front window
-- | is in front.
encodeSizes :: Sizes -> String
encodeSizes s =
  joinWith ","
    ( map show
        [ s.controls, s.bench, s.drawings, s.chart
        , s.front.x, s.front.y, s.front.w, s.front.h
        , s.inside.x, s.inside.y, s.inside.w, s.inside.h
        , if s.frontOnTop then 1.0 else 0.0
        ]
    )

-- | A size that is missing or unreadable falls back to its default.
-- | What an older page stored, which was fewer numbers in another
-- | order, keeps its two pane widths and starts the rest afresh.
decodeSizes :: String -> Sizes
decodeSizes text =
  let
    parts = split (Pattern ",") text
    current = Array.length parts == 13
    at :: Int -> Number -> Number
    at i fallback = fromMaybe fallback (index parts i >>= readNumber)
    now :: Int -> Number -> Number
    now i fallback = if current then at i fallback else fallback
    d = defaultSizes
  in
    held
      { controls: at 0 d.controls
      , bench: at 1 d.bench
      , drawings: now 2 d.drawings
      , chart: now 3 d.chart
      , front: { x: now 4 d.front.x, y: now 5 d.front.y, w: now 6 d.front.w, h: now 7 d.front.h }
      , inside: { x: now 8 d.inside.x, y: now 9 d.inside.y, w: now 10 d.inside.w, h: now 11 d.inside.h }
      , frontOnTop: now 12 0.0 > 0.5
      }

readNumber :: String -> Maybe Number
readNumber = Number.fromString
