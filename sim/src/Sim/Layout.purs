-- | How the page is arranged: a desk of five windows. The front of the
-- | machine, its inside, the shot, the controls and the bench are each
-- | a window of their own. Any of them can be dragged anywhere on the
-- | desk by its title and resized by any side or corner, and the one
-- | touched last lies over the others. The bench can be put away. The
-- | places and sizes are kept here.
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
  , windows
  , windowName
  , windowTitle
  , Box
  , boxOf
  , boxStyle
  , Grip
  , grips
  , gripName
  , dragClass
  , Sizes
  , arranged
  , tiled
  , defaultSizes
  , raised
  , dragEdge
  , encodeSizes
  , decodeSizes
  ) where

import Prelude

import Data.Array (drop, elemIndex, filter, find, index, length, mapMaybe, range, snoc, sortBy, take)
import Data.Foldable (foldl, minimumBy, sum)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
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
-- Windows
------------------------------------------------------------

-- | The windows on the desk.
data Window = FrontWindow | InsideWindow | ShotWindow | ControlsWindow | BenchWindow

derive instance eqWindow :: Eq Window

windows :: Array Window
windows = [ FrontWindow, InsideWindow, ShotWindow, ControlsWindow, BenchWindow ]

-- | A window's name in the stylesheet.
windowName :: Window -> String
windowName = case _ of
  FrontWindow -> "front"
  InsideWindow -> "inside"
  ShotWindow -> "shot"
  ControlsWindow -> "controls"
  BenchWindow -> "bench"

-- | The word on a window's title.
windowTitle :: Window -> String
windowTitle = case _ of
  FrontWindow -> "machine"
  InsideWindow -> "inside"
  ShotWindow -> "shot"
  ControlsWindow -> "controls"
  BenchWindow -> "bench"

-- | Something that can be dragged: a window by its title, or a window
-- | by one of its sides or corners.
data Edge
  = Move Window
  | Resize Window Grip

derive instance eqEdge :: Eq Edge

-- | Which sides of a window a grip pulls: -1 is the left or the top,
-- | 1 the right or the bottom, 0 leaves that direction alone.
type Grip = { across :: Int, down :: Int }

-- | The four sides and the four corners.
grips :: Array Grip
grips =
  [ { across: 0, down: -1 }, { across: 0, down: 1 }, { across: -1, down: 0 }, { across: 1, down: 0 }
  , { across: -1, down: -1 }, { across: 1, down: -1 }, { across: -1, down: 1 }, { across: 1, down: 1 }
  ]

-- | A grip's name in the stylesheet: n, s, w, e, nw, ne, sw, se.
gripName :: Grip -> String
gripName g = (if g.down < 0 then "n" else if g.down > 0 then "s" else "") <> (if g.across < 0 then "w" else if g.across > 0 then "e" else "")

-- | Where a window is on the desk, and how big, in pixels from the
-- | desk's top left corner.
type Box = { x :: Number, y :: Number, w :: Number, h :: Number }

-- | Every window's box, and the windows from the one at the bottom of
-- | the pile to the one on top.
type Sizes =
  { front :: Box
  , inside :: Box
  , shot :: Box
  , controls :: Box
  , bench :: Box
  , order :: Array Window
  }

-- | The windows laid out to fill a page of this width and height with
-- | none over another: the front beside the inside, the shot under
-- | both, the controls down the right. The bench, which starts put
-- | away, lies over the bottom of the left side when it is taken out.
arranged :: Number -> Number -> Sizes
arranged pageWide pageHigh =
  let
    gap = 10.0
    -- The top bar takes about this much of the page's height.
    wide = max 700.0 pageWide - 2.0 * gap
    high = max 480.0 (pageHigh - 58.0) - 2.0 * gap
    controlsWide = clamp 340.0 560.0 (wide * 0.34)
    leftWide = wide - controlsWide - gap
    topHigh = clamp 240.0 560.0 (high * 0.54)
    -- The drawing of the front is a little higher than it is wide.
    frontWide = clamp 200.0 (leftWide * 0.5) (topHigh * 0.86)
    benchWide = clamp 300.0 460.0 (wide * 0.3)
    benchHigh = clamp 240.0 620.0 (high * 0.6)
  in
    { front: { x: gap, y: gap, w: frontWide, h: topHigh }
    , inside: { x: gap + frontWide + gap, y: gap, w: leftWide - frontWide - gap, h: topHigh }
    , shot: { x: gap, y: gap + topHigh + gap, w: leftWide, h: high - topHigh - gap }
    , controls: { x: gap + leftWide + gap, y: gap, w: controlsWide, h: high }
    , bench: { x: gap + leftWide - benchWide, y: gap + high - benchHigh, w: benchWide, h: benchHigh }
    , order: [ ShotWindow, InsideWindow, FrontWindow, ControlsWindow, BenchWindow ]
    }

-- | The windows that are out, fitted to a page of this width and
-- | height so that none lies over another and together they fill it,
-- | each kept as near as can be to where it is and to the shape it has.
-- | The page is cut in two by a straight line, upright or level, each
-- | part is cut again, and so on until every part holds one window.
-- | The two sides of a cut share the room by the area their windows
-- | hold now, so a window that was made large stays the larger. Every
-- | way of cutting is tried, which for five windows is a few hundred,
-- | and the one that moves and reshapes the windows least is taken.
tiled :: Number -> Number -> Array Window -> Sizes -> Sizes
tiled pageWide pageHigh shown s =
  held (foldl (\t p -> setBox p.window p.box t) s (pack page (map (\window -> { window, box: boxOf window s }) shown)).placed)
  where
  gap = 10.0
  page = { x: gap, y: gap, w: max 700.0 pageWide - 2.0 * gap, h: max 480.0 (pageHigh - 58.0) - 2.0 * gap }
  square n = n * n
  -- How far a window is from its box when it is given this room: how
  -- far its middle moves, as a share of the page, and how much its
  -- shape changes. Room too small to use counts heavily against a
  -- cut.
  strain p room =
    square ((room.x + room.w / 2.0 - p.box.x - p.box.w / 2.0) / page.w)
      + square ((room.y + room.h / 2.0 - p.box.y - p.box.h / 2.0) / page.h)
      + 0.3 * square (Number.log ((room.w / room.h) / (p.box.w / p.box.h)))
      + (if room.w < 1.5 * smallestWide || room.h < 1.5 * smallestHigh then 10.0 else 0.0)
  pack area placed = case placed of
    [] -> { cost: 0.0, placed: [] }
    [ only ] -> { cost: strain only area, placed: [ only { box = area } ] }
    _ ->
      fromMaybe { cost: 0.0, placed: [] }
        (minimumBy (comparing _.cost) (map (cutAt area) (cuts true placed <> cuts false placed)))
  cutAt area cut =
    let
      share = clamp 0.12 0.88 (ground cut.first / (ground cut.first + ground cut.rest))
      wide = (area.w - gap) * share
      high = (area.h - gap) * share
      one = pack (if cut.sideways then area { w = wide } else area { h = high }) cut.first
      other = pack (if cut.sideways then area { x = area.x + wide + gap, w = area.w - gap - wide } else area { y = area.y + high + gap, h = area.h - gap - high }) cut.rest
    in
      { cost: one.cost + other.cost, placed: one.placed <> other.placed }
  -- Every way of dividing the windows in two across one direction, in
  -- the order their middles lie in.
  cuts sideways placed =
    let
      middle p = if sideways then p.box.x + p.box.w / 2.0 else p.box.y + p.box.h / 2.0
      inOrder = sortBy (comparing middle) placed
    in
      range 1 (length placed - 1) <#> \k -> { sideways, first: take k inOrder, rest: drop k inOrder }
  ground group = sum (map (\p -> p.box.w * p.box.h) group)

-- | One window's box replaced.
setBox :: Window -> Box -> Sizes -> Sizes
setBox window b s = case window of
  FrontWindow -> s { front = b }
  InsideWindow -> s { inside = b }
  ShotWindow -> s { shot = b }
  ControlsWindow -> s { controls = b }
  BenchWindow -> s { bench = b }

-- | The arrangement for a page whose size is not known yet.
defaultSizes :: Sizes
defaultSizes = arranged 1500.0 900.0

boxOf :: Window -> Sizes -> Box
boxOf window s = case window of
  FrontWindow -> s.front
  InsideWindow -> s.inside
  ShotWindow -> s.shot
  ControlsWindow -> s.controls
  BenchWindow -> s.bench

-- | A window's place, size and height in the pile as CSS.
boxStyle :: Window -> Sizes -> String
boxStyle window s =
  let
    b = boxOf window s
  in
    joinWith "; "
      [ "left: " <> show b.x <> "px"
      , "top: " <> show b.y <> "px"
      , "width: " <> show b.w <> "px"
      , "height: " <> show b.h <> "px"
      , "z-index: " <> show (1 + fromMaybe 0 (elemIndex window s.order))
      ]

-- | The window put on top of the pile.
raised :: Window -> Sizes -> Sizes
raised window s = s { order = snoc (filter (_ /= window) s.order) window }

-- | The class the desk wears during a drag, which sets the pointer.
dragClass :: Edge -> String
dragClass = case _ of
  Move _ -> " dragging moving"
  Resize _ g -> " dragging sizing-" <> gripName g

-- | The smallest a window is let go.
smallestWide :: Number
smallestWide = 140.0

smallestHigh :: Number
smallestHigh = 110.0

-- | No window is let go so small that it cannot be got hold of again,
-- | or so far off that its title could not be reached.
held :: Sizes -> Sizes
held s = s { front = box s.front, inside = box s.inside, shot = box s.shot, controls = box s.controls, bench = box s.bench }
  where
  box b = { x: clamp 0.0 6000.0 b.x, y: clamp 0.0 4000.0 b.y, w: clamp smallestWide 6000.0 b.w, h: clamp smallestHigh 4000.0 b.h }

-- | A window dragged by this many pixels to the right and down, by its
-- | title or by a grip. It comes to the top of the pile.
dragEdge :: Edge -> Number -> Number -> Sizes -> Sizes
dragEdge edge dx dy s = held case edge of
  Move window -> change window \b -> b { x = b.x + dx, y = b.y + dy }
  Resize window g -> change window (pulled g)
  where
  change window f = setBox window (f (boxOf window s)) (raised window s)
  -- A side that is pulled moves with the pointer while the side across
  -- from it stays where it is. Nothing goes below the smallest size or
  -- past the top or the left of the desk.
  pulled g b =
    let
      wide =
        if g.across > 0 then { x: b.x, w: max smallestWide (b.w + dx) }
        else if g.across < 0 then let moved = clamp (-b.x) (b.w - smallestWide) dx in { x: b.x + moved, w: b.w - moved }
        else { x: b.x, w: b.w }
      high =
        if g.down > 0 then { y: b.y, h: max smallestHigh (b.h + dy) }
        else if g.down < 0 then let moved = clamp (-b.y) (b.h - smallestHigh) dy in { y: b.y + moved, h: b.h - moved }
        else { y: b.y, h: b.h }
    in
      { x: wide.x, y: high.y, w: wide.w, h: high.h }

-- | Twenty-five numbers separated by commas: the box of each window in
-- | the order of `windows`, then the pile from bottom to top, each
-- | window as its place in `windows`.
encodeSizes :: Sizes -> String
encodeSizes s =
  joinWith ","
    ( map show
        ( (windows >>= \window -> let b = boxOf window s in [ b.x, b.y, b.w, b.h ])
            <> mapMaybe (\window -> map toNumber (elemIndex window windows)) s.order
        )
    )

-- | Nothing unless the text is what `encodeSizes` writes, which is
-- | also the answer for what an older page stored. The page then lays
-- | the windows out afresh.
decodeSizes :: String -> Maybe Sizes
decodeSizes text = do
  numbers <- traverse Number.fromString (split (Pattern ",") text)
  if length numbers /= 25 then Nothing else pure unit
  let
    at i = fromMaybe 0.0 (index numbers i)
    box i = { x: at (4 * i), y: at (4 * i + 1), w: at (4 * i + 2), h: at (4 * i + 3) }
    pile = mapMaybe (\i -> index windows (floor (at (20 + i)))) [ 0, 1, 2, 3, 4 ]
    -- A pile that does not name every window once is not trusted.
    whole = length pile == 5 && filter (\window -> length (filter (_ == window) pile) /= 1) windows == []
  pure
    ( held
        { front: box 0
        , inside: box 1
        , shot: box 2
        , controls: box 3
        , bench: box 4
        , order: if whole then pile else defaultSizes.order
        }
    )
