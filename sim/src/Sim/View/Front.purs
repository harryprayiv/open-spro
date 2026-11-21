-- | The machine seen from the front, drawn plainly: a brushed steel
-- | face under a band of charred cedar, one dial for both pressures, an
-- | e-ink display of the shot, three small temperature
-- | readouts, heater lamps, the fader with its selector drum and a
-- | window for the value it is at, the group
-- | with its portafilter in front of a concrete backsplash, the cup on
-- | the scale and the steam wand on its turret. Steel, cedar, wood and
-- | concrete come from the images in sim/imgs/textures; every textured
-- | shape has a flat colour beneath it in case an image is missing.
-- |
-- | The front has two looks. By day it is as described. By night the
-- | steel goes dark, the dial is lit from behind, every hand and figure
-- | takes a colour from cool to warm by how far along its scale it is,
-- | and a light hidden above the cup picks the shot out of the dark.
module Sim.View.Front
  ( FrontInput
  , Ink
  , front
  ) where

import Prelude

import Data.Array (concat, mapWithIndex, range)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number (abs, cos, floor, pi, sin)
import Data.Number.Format (fixed, toStringWith)
import Data.String (joinWith, toLower)
import Data.String as String
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sim.Brand as Brand
import Sim.Link (Event(..), FaderMode(..), faderWord, nextFaderMode)
import Sim.Mascot (Mascot)
import Sim.Mascot as Mascot
import Sim.Model (Action(..))
import Sim.Palette (leds, scheme)
import Sim.Tint (coolToWarm)
import Sim.Types (MachineState(..), PowerMode(..))
import Sim.View.Svg (Attr, at, circle, defs, falling, fitted, g, glow, label, line, num, path, picture, polyline, rect, svg, texture)

type FrontInput =
  { now :: Number
  -- Whether the page is in its night theme.
  , night :: Boolean
  -- Whether play is arcade, which eases the lamps.
  , arcade :: Boolean
  , state :: MachineState
  , brewBar :: Number
  , steamBar :: Number
  , reservoirC :: Number
  , groupC :: Number
  , waterC :: Number
  , cupGrams :: Number
  , targetGrams :: Number
  , brewElementOn :: Boolean
  , steamElementOn :: Boolean
  , groupHeaterOn :: Boolean
  , pouring :: Boolean
  , steaming :: Boolean
  -- Whether the steam lever is pulled down, which opens the valve.
  , steamOpen :: Boolean
  -- Where the fader is along its travel, from 0 to 1, whether a shot
  -- is running and whether it is being pulled by hand.
  , fader :: Number
  -- What the selector says the fader's travel stands for.
  , mode :: FaderMode
  -- The value the fader is at, written out with its unit.
  , faderValue :: String
  -- What the e-ink display shows.
  , ink :: Ink
  , running :: Boolean
  , manual :: Boolean
  -- Something on the page's problem list is active.
  , problem :: Boolean
  -- How fast coffee is reaching the cup, in grams a second.
  , outFlow :: Number
  -- The colour of the coffee leaving the basket.
  , streamColour :: String
  }

-- | The shot as the e-ink display shows it. Times are in seconds from
-- | the start of the shot and pressures in bar.
type Ink =
  -- how much time the display is wide, and how far the shot has run
  { span :: Number
  , place :: Number
  , running :: Boolean
  -- whether the machine is asleep
  , asleep :: Boolean
  -- whether the machine is warming up with no shot running, and how
  -- many seconds it has to go if that is known
  , heating :: Boolean
  , readyIn :: Maybe Number
  -- whether the machine has been ready before, in which case heating
  -- is a short spell and not the first warm-up
  , settled :: Boolean
  -- the profile's steps, and which of them the shot is in
  , bands :: Array { start :: Number, end :: Number }
  , step :: Maybe Int
  -- the pressure the profile will ask for, and the pressure asked for
  -- so far
  , target :: Array { t :: Number, bar :: Number }
  , drawn :: Array { t :: Number, bar :: Number }
  -- the limits the fader is held to while the pressure is set by hand
  , limits :: Maybe { low :: Number, high :: Number }
  -- what is about to be brewed: the beans, a line about them, and the
  -- name of the profile
  , bean :: String
  , details :: String
  , profile :: String
  }

-- | The red of a limit: the arcs on the dial and the outline of a
-- | faulted machine.
bad :: String
bad = scheme.alarm

-- | The palette of the front: black lettering on steel, off-white
-- | figures in black windows, and one orange for whatever is live.
ink :: String
ink = "#1c1d1f"

quiet :: String
quiet = "#4a4e52"

steelEdge :: String
steelEdge = "#6f757b"

paper :: String
paper = "#f4f2ec"

window :: String
window = "#141516"

orange :: String
orange = scheme.orange

sans :: String
sans = "'Helvetica Neue', Helvetica, Arial, sans-serif"

gold :: String
gold = scheme.gold

-- | What differs between the day face and the night face.
type Look =
  { night :: Boolean
  , arcade :: Boolean
  -- lettering on the face, and the quieter lettering of labels
  , lettering :: String
  , label :: String
  -- the colour of a hand or a figure this far along its scale, from 0
  -- to 1, and of the smaller steam scale
  , figure :: Number -> String
  , steamFigure :: Number -> String
  -- a figure in a black window, this far along its scale
  , lit :: Number -> String
  -- the wash over the steel, bare metal parts, and a lamp that is off
  , wash :: String
  , washStrength :: String
  , metal :: String
  , metalEdge :: String
  , dark :: String
  }

lookOf :: Boolean -> Boolean -> Look
lookOf night arcade =
  if night then
    { night
    , arcade
    , lettering: "#f4eedf"
    , label: "#a39a8d"
    , figure: coolToWarm
    , steamFigure: coolToWarm
    , lit: coolToWarm
    , wash: "#0b0908"
    , washStrength: "0.66"
    , metal: "#4a4d50"
    , metalEdge: "#2a2c2e"
    , dark: "#3a3632"
    }
  else
    { night
    , arcade
    , lettering: ink
    , label: quiet
    , figure: const ink
    , steamFigure: const orange
    , lit: const paper
    , wash: "#ffffff"
    , washStrength: "0.42"
    , metal: "#c5cace"
    , metalEdge: steelEdge
    , dark: "#8d9297"
    }

textures :: String
textures = "../imgs/textures/"

------------------------------------------------------------
-- Proportions
------------------------------------------------------------

-- | The golden ratio. The machine's proportions are taken from it: a
-- | length divided by `phi` gives the shorter side of a golden
-- | rectangle, or the longer part of a line cut at its golden section.
phi :: Number
phi = 1.6180339887

-- | The body is 340 wide and its foot is at 276. The steel face under
-- | the cedar band is a golden rectangle of that width, which sets
-- | where the band ends.
bodyWide :: Number
bodyWide = 340.0

faceHigh :: Number
faceHigh = bodyWide / phi

faceTop :: Number
faceTop = 276.0 - faceHigh

-- | How far the fader strip and the row under it sit below where they
-- | were first drawn, which was for a band ending at 52.
lowered :: Number
lowered = faceTop - 52.0

-- | The dial's outer ring is as wide as the longer part of the face's
-- | height cut at its golden section, and it is centred between the
-- | row above it and the foot of the body.
dialRadius :: Number
dialRadius = faceHigh / phi / 2.0 - 3.0

dialMiddle :: Number
dialMiddle = (124.0 + lowered + 276.0) / 2.0

-- | The backsplash is a golden rectangle on the height between the
-- | body and the tray. The portafilter's ring is as wide as the
-- | backsplash is high, and the group over it is narrower again by the
-- | same ratio. The drip tray is the width of the body divided by
-- | `phi`, and the two posts take what the tray has over the
-- | backsplash.
wallHigh :: Number
wallHigh = 124.0

wallWide :: Number
wallWide = wallHigh * phi

wallLeft :: Number
wallLeft = 200.0 - wallWide / 2.0

groupWide :: Number
groupWide = wallHigh / phi

trayWide :: Number
trayWide = bodyWide / phi

postWide :: Number
postWide = (trayWide - wallWide) / 2.0

-- | The e-ink screen is a golden rectangle 91 wide.
screenWide :: Number
screenWide = 91.0

screenHigh :: Number
screenHigh = screenWide / phi

-- | A rectangle with a flat colour and the named texture over it.
faced :: forall w i. String -> String -> Array Attr -> Array (HH.HTML w i)
faced name fallback attrs =
  [ rect (attrs <> [ at "fill" fallback ])
  , rect (attrs <> [ at "fill" ("url(#" <> name <> ")") ])
  ]

-- | Brushed steel: the texture with a wash over it. By day the wash is
-- | white, so black lettering reads on it. By night it is near black.
steel :: forall w i. Look -> Array Attr -> Array (HH.HTML w i)
steel k attrs =
  faced "alu" "#b9bec4" attrs
    <> [ rect (attrs <> [ at "fill" k.wash, at "opacity" k.washStrength ]) ]

-- | A point on a dial. Zero degrees is straight up, positive is clockwise.
onDial :: Number -> Number -> Number -> Number -> { x :: Number, y :: Number }
onDial cx cy radius degrees =
  let
    a = degrees * pi / 180.0
  in
    { x: cx + radius * sin a, y: cy - radius * cos a }

-- | One dial for both pressures: a white face in a thin black ring,
-- | hairline marks and plain numerals. The outer scale and the long
-- | black hand are the brew pressure. The inner scale and the short
-- | orange hand are the steam pressure. Each scale has a red arc from
-- | its `redFrom` up. By night the face is dark and lit from behind,
-- | and each mark, numeral and hand is coloured by where it is on its
-- | scale.
type Scale = { most :: Int, labelEvery :: Int, redFrom :: Number, value :: Number }

dualGauge :: forall w i. Look -> Number -> Number -> Scale -> Scale -> HH.HTML w i
dualGauge k cx cy brew steam =
  let
    r = dialRadius
    sweep scale v = -135.0 + 270.0 * clamp 0.0 1.0 (v / toNumber scale.most)
    arc radius scale =
      let
        from = onDial cx cy radius (sweep scale scale.redFrom)
        to = onDial cx cy radius (sweep scale (toNumber scale.most))
      in
        "M" <> num from.x <> "," <> num from.y
          <> " A" <> num radius <> "," <> num radius <> " 0 0 1 "
          <> num to.x <> "," <> num to.y
    along scale v = clamp 0.0 1.0 (v / toNumber scale.most)
    mark colourAt from long short scale parts n =
      let
        whole = n `mod` parts == 0
        colour = colourAt (along scale (toNumber n / toNumber parts))
        angle = sweep scale (toNumber n / toNumber parts)
        outer = onDial cx cy from angle
        inner = onDial cx cy (from - (if whole then long else short)) angle
      in
        line
          [ at "x1" (num outer.x), at "y1" (num outer.y)
          , at "x2" (num inner.x), at "y2" (num inner.y)
          , at "stroke" colour, at "stroke-width" (if whole then "1.3" else "0.6")
          ]
    numeral colourAt radius size scale n =
      let
        p = onDial cx cy radius (sweep scale (toNumber n))
        colour = colourAt (along scale (toNumber n))
      in
        label
          [ at "x" (num p.x), at "y" (num (p.y + size * 0.36))
          , at "text-anchor" "middle", at "font-size" (num size), at "font-family" sans, at "fill" colour
          ]
          (show n)
    labelled scale = map (_ * scale.labelEvery) (range 0 (scale.most / scale.labelEvery))
    hand scale reach width colourAt =
      let
        angle = sweep scale scale.value
        colour = colourAt (along scale scale.value)
        tip = onDial cx cy reach angle
        tail = onDial cx cy 9.0 (angle + 180.0)
      in
        line
          [ at "x1" (num tail.x), at "y1" (num tail.y), at "x2" (num tip.x), at "y2" (num tip.y)
          , at "stroke" colour, at "stroke-width" width, at "stroke-linecap" "round"
          ]
    word y colour text =
      label
        [ at "x" (num cx), at "y" (num (cy + y))
        , at "text-anchor" "middle", at "font-size" "5.5", at "font-family" sans
        , at "letter-spacing" "0.6", at "fill" colour
        ]
        text
  in
    g []
      ( [ circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num (r + 3.0)), at "fill" (if k.night then gold else ink) ]
        , circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num (if k.night then r + 1.6 else r)), at "fill" (if k.night then "#14100d" else paper) ]
        , circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num r), at "fill" (if k.night then "url(#dial-light)" else "none") ]
        , path [ at "d" (arc (r - 4.5) brew), at "fill" "none", at "stroke" bad, at "stroke-width" "2" ]
        , path [ at "d" (arc (r - 31.5) steam), at "fill" "none", at "stroke" bad, at "stroke-width" "1.5" ]
        ]
          <> map (mark k.figure (r - 6.0) 7.0 3.5 brew 2) (range 0 (brew.most * 2))
          <> map (numeral k.figure (r - 20.0) 8.5 brew) (labelled brew)
          <> map (mark k.steamFigure (r - 33.0) 4.0 2.0 steam 2) (range 0 (steam.most * 2))
          <> map (numeral k.steamFigure (r - 43.5) 6.5 steam) (labelled steam)
          <>
            [ word 49.0 k.label "brew  bar"
            , word 30.0 (if k.night then k.label else orange) "steam"
            , hand steam (r - 34.0) "1.6" k.steamFigure
            , hand brew (r - 8.0) "2" k.figure
            , circle [ at "cx" (num cx), at "cy" (num cy), at "r" "3.4", at "fill" (if k.night then gold else ink) ]
            ]
      )

-- | A lamp: a small dot that is lit or not, with its name under it. A
-- | lit lamp has a halo, which shows more in the dark.
-- | The classes of a lamp. In arcade play the stylesheet eases it: it
-- | comes on quickly and lingers before it fades, so a heater that is
-- | being pulsed shows as a steady glow and not a flicker. In realistic
-- | play it follows its output exactly.
led :: Look -> Boolean -> String
led k lit = "led" <> (if k.arcade then " eased" else "") <> (if lit then " on" else " off")

lamp :: forall w i. Look -> Number -> Number -> String -> Boolean -> String -> HH.HTML w i
lamp k x y title lit colour =
  g []
    [ circle
        [ at "class" (led k lit), at "cx" (num x), at "cy" (num y), at "r" "7"
        , at "fill" colour, at "opacity" (if not lit then "0" else if k.night then "0.3" else "0.16")
        ]
    , circle
        [ at "class" (led k lit), at "cx" (num x), at "cy" (num y), at "r" "3.6"
        , at "fill" (if lit then colour else k.dark)
        , at "stroke" ink, at "stroke-width" "0.6"
        ]
    , label
        [ at "x" (num x), at "y" (num (y + 12.0)), at "text-anchor" "middle"
        , at "font-size" "5.5", at "font-family" sans, at "letter-spacing" "0.3", at "fill" k.label
        ]
        title
    ]

-- | A small figure in a black window, its name to the left of it. The
-- | last number is how far along its scale the figure is, from 0 to 1,
-- | which by night decides its colour.
readout :: forall w i. Look -> Number -> String -> String -> Number -> HH.HTML w i
readout k y title value fraction =
  g []
    [ label
        [ at "x" "301", at "y" (num (y + 11.5)), at "text-anchor" "end", at "font-size" "5.5"
        , at "font-family" sans, at "letter-spacing" "0.3", at "fill" k.label
        ]
        title
    -- the window is 17 high and phi squared times that wide
    , rect [ at "x" "306", at "y" (num y), at "width" (num (17.0 * phi * phi)), at "height" "17", at "rx" "2", at "fill" window ]
    , label
        [ at "x" (num (306.0 + 17.0 * phi * phi / 2.0)), at "y" (num (y + 12.5)), at "text-anchor" "middle"
        , at "font-size" "10.5", at "font-family" "monospace", at "fill" (k.lit fraction)
        ]
        value
    ]

-- | The e-ink display, in greys. E-ink has no colour and holds its
-- | picture with the power off.
-- |
-- | With the machine ready and no shot running it says what is about
-- | to be brewed, laid out like the screen of an old music player: a
-- | black bar along the top with the mark from `Sim.Brand`, the beans,
-- | a line about them, the profile with how long it runs, and the
-- | profile's shape along the bottom.
-- |
-- | While a shot runs the bar carries the profile's name and the time,
-- | and the shot fills the rest as a timeline. Each step of the
-- | profile is a band, the step the shot is in is the darkest, the
-- | pressure the profile will ask for is a dashed line and the
-- | pressure asked for so far is a solid black one. An upright line
-- | marks how far the shot has run.
-- |
-- | While the machine is asleep the display says so, with a slow
-- | snore drawn on it, and pressing it wakes the machine for brewing
-- | and steam. If `Sim.Mascot` names a character, the character is
-- | shown in place of the snore, saying its lines. `now` is the time, in
-- | seconds, that picks which of its frames is up.
-- |
-- | While the machine warms up for the first time the display says so
-- | instead, with the time it has to go counting down. Once it has been
-- | ready, a later spell of heating, as after a change of profile or of
-- | beans, leaves the timeline up, so the shape just chosen can be
-- | seen, and says how long is left in a line along the bottom. In the simulator the wait can be
-- | skipped, and the display says that too: pressing it puts the
-- | machine at temperature at once.
inkDisplay :: forall w. Look -> Number -> Ink -> HH.HTML w Action
inkDisplay k now page =
  let
    left = 177.0
    top = 146.0
    width = screenWide
    height = screenHigh
    span = max 1.0 page.span
    -- The screen is a golden rectangle and is composed by cutting it
    -- at golden sections. The card's words take the longer part of
    -- its height and the profile's shape the shorter part under them.
    -- The words' part is cut again: the title bar is its height
    -- divided by phi three times, and the rule between the beans and
    -- the profile is at the golden section of what the bar leaves.
    -- Across, the profile's row is cut at the golden section of the
    -- width, which leaves a square on the left for the name.
    wordsHigh = height / phi
    titleHigh = wordsHigh / (phi * phi * phi)
    ruleDown = titleHigh + (wordsHigh - titleHigh) / phi
    cutAcross = width / phi
    -- The two places the shot is plotted: the whole screen under the
    -- bar while a shot runs, and the shorter part of the card before
    -- one.
    full = { top: top + titleHigh, height: height - titleHigh, head: 8.0, numbered: true }
    strip = { top: top + wordsHigh, height: height - wordsHigh, head: 3.0, numbered: false }
    x t = left + clamp 0.0 1.0 (t / span) * width
    y r pressure = r.top + r.height - clamp 0.0 12.0 pressure / 12.0 * (r.height - r.head)
    shade i
      | page.step == Just i = "#a9aaa4"
      | i `mod` 2 == 0 = "#d6d7d0"
      | otherwise = "#c4c5be"
    band r i seg =
      [ rect
          [ at "x" (num (x seg.start)), at "y" (num r.top)
          , at "width" (num (max 0.0 (x seg.end - x seg.start))), at "height" (num r.height)
          , at "fill" (shade i)
          ]
      ]
        <>
          if r.numbered then
            [ label
                [ at "x" (num (x seg.start + 2.0)), at "y" (num (r.top + 6.2)), at "font-size" "5"
                , at "font-family" sans, at "fill" "#55564f"
                ]
                (show (i + 1))
            ]
          else []
    points r samples = joinWith " " (map (\s -> num (x s.t) <> "," <> num (y r s.bar)) samples)
    limit r pressure =
      line
        [ at "x1" (num left), at "x2" (num (left + width)), at "y1" (num (y r pressure)), at "y2" (num (y r pressure))
        , at "stroke" "#55564f", at "stroke-width" "0.6", at "stroke-dasharray" "1 2"
        ]
    limits r = case page.limits of
      Just held -> [ limit r held.low, limit r held.high ]
      Nothing -> []
    middle = left + width / 2.0
    words size weight spacing colour down text =
      label
        [ at "x" (num middle), at "y" (num (top + down)), at "text-anchor" "middle"
        , at "font-size" size, at "font-family" sans, at "font-weight" weight
        , at "letter-spacing" spacing, at "fill" colour
        ]
        text
    twoDigits n = if n < 10 then "0" <> show n else show n
    countdown seconds =
      let
        whole = Int.ceil seconds
      in
        show (whole / 60) <> ":" <> twoDigits (whole `mod` 60)
    press tip action =
      HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "rect")
        [ HP.attr (AttrName "x") (num left), HP.attr (AttrName "y") (num top)
        , HP.attr (AttrName "width") (num width), HP.attr (AttrName "height") (num height)
        , HP.attr (AttrName "fill") "transparent"
        , HP.attr (AttrName "class") "fader-press"
        , HE.onClick \_ -> action
        ]
        [ HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "title") [] [ HH.text tip ] ]
    underline halfWidth =
      line
        [ at "x1" (num (middle - halfWidth)), at "x2" (num (middle + halfWidth)), at "y1" (num (top + 49.8)), at "y2" (num (top + 49.8))
        , at "stroke" "#1c1c1a", at "stroke-width" "0.35"
        ]
    -- One z of the snore. The stylesheet brings the three in one after
    -- another and then clears them, in steps, as e-ink would.
    z order across down size =
      label
        [ at "class" ("snore snore-" <> show order), at "x" (num (middle + across)), at "y" (num (top + down))
        , at "text-anchor" "middle", at "font-size" size, at "font-family" sans, at "font-weight" "300"
        , at "font-style" "italic", at "fill" "#1c1c1a"
        ]
        "z"
    wake = press "Wake the machine, for brewing and steam" (Send (WakeRequested Full))
    -- A character on the left, in greys whatever its colours, and what
    -- it says in a bubble on the right with a tail towards it.
    character :: Mascot -> Array (HH.HTML w Action)
    character mascot =
      let
        lines = Array.take 4 mascot.says
        count = toNumber (Array.length lines)
        bubbleTop = top + 26.0 - (count * 6.4 + 7.0) / 2.0
        say i text =
          label
            [ at "x" (num (left + 66.5)), at "y" (num (bubbleTop + 8.6 + 6.4 * toNumber i)), at "text-anchor" "middle"
            , at "font-size" "4.8", at "font-family" sans, at "letter-spacing" "0.3", at "fill" "#1c1c1a"
            ]
            text
        face = case Mascot.frameAt now mascot of
          Just file ->
            [ picture file (left + 3.0) (top + 5.0) 40.0 42.0
                [ at "style" "filter: grayscale(1) contrast(1.15)" ]
            ]
          Nothing -> []
        bubble =
          if Array.null lines then []
          else
            [ path
                [ at "d"
                    ( "M" <> num (left + 46.5) <> "," <> num (top + 23.0) <> " L" <> num (left + 42.0) <> "," <> num (top + 26.0)
                        <> " L" <> num (left + 46.5) <> "," <> num (top + 29.0)
                    )
                , at "fill" "#f1f2ec", at "stroke" "#1c1c1a", at "stroke-width" "0.5", at "stroke-linejoin" "round"
                ]
            , rect
                [ at "x" (num (left + 46.0)), at "y" (num bubbleTop), at "width" "41", at "height" (num (count * 6.4 + 7.0))
                , at "rx" "4", at "fill" "#f1f2ec", at "stroke" "#1c1c1a", at "stroke-width" "0.5"
                ]
            -- the bubble's own edge is hidden where the tail joins it
            , line
                [ at "x1" (num (left + 46.0)), at "x2" (num (left + 46.0)), at "y1" (num (top + 23.6)), at "y2" (num (top + 28.4))
                , at "stroke" "#f1f2ec", at "stroke-width" "0.9"
                ]
            ]
      in
        face <> bubble <> Array.mapWithIndex say lines <> [ wake ]
    sleeping = case Mascot.asleep of
      Just mascot -> character mascot
      Nothing -> snoring
    snoring =
      [ words "5" "400" "1.6" "#55564f" 11.0 "ASLEEP"
      , line
          [ at "x1" (num (middle - 14.0)), at "x2" (num (middle + 14.0)), at "y1" (num (top + 15.0)), at "y2" (num (top + 15.0))
          , at "stroke" "#8f908a", at "stroke-width" "0.5"
          ]
      , z 1 (-11.0) 38.0 "8"
      , z 2 (-1.0) 33.0 "12"
      , z 3 12.0 27.5 "16"
      ]
        <>
          [ words "4.6" "400" "0.4" "#1c1c1a" 48.5 "press here to wake the machine"
              , underline 37.0
              , wake
              ]
    remaining = case page.readyIn of
      Just seconds -> countdown seconds
      Nothing -> "--:--"
    -- A short spell of heating on a machine that has been ready: one
    -- line along the bottom of the timeline.
    reheating =
      [ rect [ at "x" (num left), at "y" (num (top + height - 8.5)), at "width" (num width), at "height" "8.5", at "fill" "#1c1c1a" ]
      , label
          [ at "x" (num (left + 3.0)), at "y" (num (top + height - 2.6)), at "font-size" "4.8", at "font-family" sans
          , at "letter-spacing" "0.3", at "fill" "#e3e4dd"
          ]
          ("heating  " <> remaining)
      ]
        <>
          [ label
                  [ at "x" (num (left + width - 3.0)), at "y" (num (top + height - 2.6)), at "text-anchor" "end", at "font-size" "4.8"
                  , at "font-family" sans, at "letter-spacing" "0.3", at "fill" "#e3e4dd", at "text-decoration" "underline"
                  ]
                  "press to skip"
              , press "Skip the wait. Only the simulator can do this." SkipWarmUp
              ]
    warming =
      [ words "5" "400" "1.6" "#55564f" 11.0 "WARMING UP"
      , line
          [ at "x1" (num (middle - 14.0)), at "x2" (num (middle + 14.0)), at "y1" (num (top + 15.0)), at "y2" (num (top + 15.0))
          , at "stroke" "#8f908a", at "stroke-width" "0.5"
          ]
      , words "17" "300" "0.5" "#1c1c1a" 33.0 remaining
      , words "4.6" "400" "0.4" "#55564f" 41.5 "until the machine is ready"
      ]
        <>
          [ words "4.6" "400" "0.4" "#1c1c1a" 48.5 "press here to skip the wait"
              , underline 30.0
              , press "Skip the warm-up. Only the simulator can do this." SkipWarmUp
              ]
    -- The shot plotted in one of the two places.
    curve r =
      concat (mapWithIndex (band r) page.bands)
        <> limits r
        <>
          [ polyline
              [ at "fill" "none", at "points" (points r page.target)
              , at "stroke" "#6f706a", at "stroke-width" (if r.numbered then "0.9" else "0.7"), at "stroke-dasharray" "2 1.5"
              ]
          , polyline
              [ at "fill" "none", at "points" (points r page.drawn)
              , at "stroke" "#1c1c1a", at "stroke-width" (if r.numbered then "1.4" else "1"), at "stroke-linejoin" "round"
              ]
          ]
        <>
          if page.running then
            [ line
                [ at "x1" (num (x page.place)), at "x2" (num (x page.place)), at "y1" (num r.top), at "y2" (num (r.top + r.height))
                , at "stroke" "#1c1c1a", at "stroke-width" "0.9"
                ]
            ]
          else []
    -- Words that would be wider than the room they have are squeezed
    -- to fit it, letters and spaces together, so a long name is
    -- narrower and never cut short. `each` is about how wide one
    -- character is at the size the words are set in.
    squeezed room each text =
      if toNumber (String.length text) * each > room then
        [ at "textLength" (num room), at "lengthAdjust" "spacingAndGlyphs" ]
      else []
    -- A name in two lines of as near the same length as its spaces
    -- allow, or in one if it has no space to break at.
    halves text =
      let
        parts = String.split (String.Pattern " ") text
        join = joinWith " "
        uneven n =
          let
            over = String.length (join (Array.take n parts)) - String.length (join (Array.drop n parts))
          in
            if over < 0 then -over else over
        best = Array.foldl (\a n -> if uneven n < uneven a then n else a) 1 (range 1 (max 1 (Array.length parts - 1)))
      in
        if Array.length parts < 2 then [ text ] else [ join (Array.take best parts), join (Array.drop best parts) ]
    inSeconds n = toStringWith (fixed 0) n <> " s"
    -- The bar along the top: the mark at the left, a few words after
    -- it and a figure at the right, in white on black.
    mark = case Brand.logo.picture of
      Just file ->
        [ picture (Mascot.address file) (left + 1.5) (top + 0.9) 13.0 (titleHigh - 1.8)
            [ at "style" "filter: grayscale(1)" ]
        ]
      Nothing ->
        [ label
            [ at "x" (num (left + 2.5)), at "y" (num (top + 5.9)), at "font-size" "5.2", at "font-family" sans
            , at "fill" "#e3e4dd"
            ]
            Brand.logo.mark
        ]
    titleBar fit middleWords figure =
      [ rect [ at "x" (num left), at "y" (num top), at "width" (num width), at "height" (num titleHigh), at "fill" "#1c1c1a" ] ]
        <> mark
        <>
          -- The words start clear of the mark and are cut short of the
          -- figure.
          [ label
              ( [ at "x" (num (left + 18.5)), at "y" (num (top + 5.7)), at "font-size" "4.2"
                , at "font-family" sans, at "letter-spacing" "0.7", at "fill" "#e3e4dd"
                ]
                  <> fit
              )
              middleWords
          , label
              [ at "x" (num (left + width - 2.5)), at "y" (num (top + 5.7)), at "text-anchor" "end", at "font-size" "4.4"
              , at "font-family" sans, at "fill" "#e3e4dd"
              ]
              figure
          ]
    -- The baseline of the profile's row, in the middle of its part.
    rowDown = (ruleDown + wordsHigh) / 2.0 + 1.8
    -- The beans between the bar and the rule: the name, in one line if
    -- it fits and in two smaller ones if it does not, and under it the
    -- line about them.
    centred down size weight colour each text =
      label
        ( [ at "x" (num middle), at "y" (num (top + down)), at "text-anchor" "middle", at "font-size" (num size)
          , at "font-family" sans, at "font-weight" weight, at "fill" colour
          ]
            <> squeezed (width - 8.0) each text
        )
        text
    beans =
      if String.length page.bean <= 20 then
        [ centred (titleHigh + 8.4) 7.2 "700" "#1c1c1a" 4.15 page.bean
        , centred (ruleDown - 2.2) 4.2 "400" "#55564f" 2.3 page.details
        ]
      else
        Array.mapWithIndex (\i text -> centred (titleHigh + 5.6 + 5.2 * toNumber i) 5.2 "700" "#1c1c1a" 3.0 text) (halves page.bean)
          <> [ centred (ruleDown - 1.5) 3.7 "400" "#55564f" 2.05 page.details ]
    -- With no shot running: what is about to be brewed. The beans in
    -- the largest letters, a line about them, then the profile beside
    -- a play mark with how long it runs, and its shape along the
    -- bottom.
    nowBrewing =
      titleBar [] "NOW BREWING" ""
        <> beans
        <>
          [ line
              [ at "x1" (num (left + 5.0)), at "x2" (num (left + width - 5.0)), at "y1" (num (top + ruleDown)), at "y2" (num (top + ruleDown))
              , at "stroke" "#8f908a", at "stroke-width" "0.4"
              ]
          , line
              [ at "x1" (num (left + cutAcross)), at "x2" (num (left + cutAcross)), at "y1" (num (top + ruleDown + 2.2)), at "y2" (num (top + wordsHigh - 2.2))
              , at "stroke" "#8f908a", at "stroke-width" "0.4"
              ]
          , path
              [ at "d" ("M" <> num (left + 4.5) <> "," <> num (top + rowDown - 3.9) <> " l3.6,2.1 l-3.6,2.1 z")
              , at "fill" "#1c1c1a"
              ]
          , label
              ( [ at "x" (num (left + 10.5)), at "y" (num (top + rowDown)), at "font-size" "5", at "font-family" sans
                , at "font-weight" "500", at "fill" "#1c1c1a"
                ]
                  <> squeezed (cutAcross - 13.0) 2.75 page.profile
              )
              page.profile
          , label
              [ at "x" (num (left + (cutAcross + width) / 2.0)), at "y" (num (top + rowDown)), at "text-anchor" "middle", at "font-size" "5"
              , at "font-family" sans, at "fill" "#55564f"
              ]
              (inSeconds (maybe span _.end (Array.last page.bands)))
          ]
        <> curve strip
    -- With a shot running: the profile's name and the time in the bar,
    -- and the shot filling the rest.
    brewing =
      let
        name = String.toUpper page.profile
      in
        titleBar (squeezed (width - 36.0) 3.25 name) name (inSeconds page.place) <> curve full
    shotView = if page.running then brewing else nowBrewing
  in
    g []
      ( [ rect [ at "x" (num (left - 5.0)), at "y" (num (top - 5.0)), at "width" (num (width + 10.0)), at "height" (num (height + 10.0)), at "rx" "3", at "fill" window ]
        , rect [ at "x" (num left), at "y" (num top), at "width" (num width), at "height" (num height), at "fill" "#e3e4dd" ]
        ]
          <>
            -- The sleeping and warming screens were set out for a
            -- screen 52 high and are centred in this one.
            ( if page.asleep then [ g [ at "transform" ("translate(0," <> num ((height - 52.0) / 2.0) <> ")") ] sleeping ]
              else if page.heating && not page.settled then [ g [ at "transform" ("translate(0," <> num ((height - 52.0) / 2.0) <> ")") ] warming ]
              else if page.heating then shotView <> reheating
              else shotView
            )
          <>
            -- By night the panel has only its dim front light.
            ( if k.night then
                [ rect [ at "x" (num left), at "y" (num top), at "width" (num width), at "height" (num height), at "fill" "#1a1206", at "opacity" "0.42", at "pointer-events" "none" ] ]
              else []
            )
      )

-- | The window under the fader: the value the fader is
-- | at. What the value is a value of is in the selector's window beside
-- | it.
drumWide :: Number
drumWide = 28.0 * phi

valueLeft :: Number
valueLeft = 40.0 + drumWide + 8.0

valueWide :: Number
valueWide = 28.0 * phi * phi

faderWindow :: forall w i. Look -> String -> Number -> HH.HTML w i
faderWindow k value fraction =
  g []
    -- 28 high and phi squared times that wide, beside the selector's
    -- window, which is 28 high and phi times that wide
    [ rect [ at "x" (num valueLeft), at "y" (num (96.0 + lowered)), at "width" (num valueWide), at "height" "28", at "rx" "2", at "fill" window ]
    , label
        [ at "x" (num (valueLeft + valueWide / 2.0)), at "y" (num (116.0 + lowered)), at "text-anchor" "middle"
        , at "font-size" "14", at "font-family" "monospace", at "fill" (k.lit fraction)
        ]
        value
    ]

-- | The selector: a drum with the three words written round it, turned
-- | by the knurled wheel that stands out of the left flank. The window
-- | beside the wheel faces the front and shows the word the drum is
-- | on, with the edges of its neighbours above and below. On time,
-- | where the fader cannot be moved, the word is red.
selector :: forall w. Look -> FaderMode -> HH.HTML w Action
selector k mode =
  let
    next = nextFaderMode mode
    before = nextFaderMode next
    groove i =
      let
        gy = 99.5 + lowered + 3.5 * toNumber i
      in
        line [ at "x1" "9", at "x2" "29", at "y1" (num gy), at "y2" (num gy), at "stroke" k.metalEdge, at "stroke-width" "0.8" ]
    neighbour y text =
      label
        [ at "x" (num (40.0 + drumWide / 2.0)), at "y" (num (y + lowered)), at "text-anchor" "middle", at "font-size" "5", at "font-family" sans
        , at "letter-spacing" "0.5", at "fill" "#6c7176"
        ]
        (toLower (faderWord text))
  in
    g []
      ( [ rect [ at "x" "8", at "y" (num (96.0 + lowered)), at "width" "22", at "height" "28", at "rx" "2.5", at "fill" k.metal, at "stroke" k.metalEdge, at "stroke-width" "0.8" ]
        , g [] (map groove (range 0 6))
        , rect [ at "x" "40", at "y" (num (96.0 + lowered)), at "width" (num drumWide), at "height" "28", at "rx" "2", at "fill" window ]
        , neighbour 102.5 before
        , label
            [ at "x" (num (40.0 + drumWide / 2.0)), at "y" (num (113.0 + lowered)), at "text-anchor" "middle", at "font-size" "7.4", at "font-family" sans
            , at "letter-spacing" "0.6", at "fill" (if mode == OnTime then "#ff4d3d" else paper)
            ]
            (toLower (faderWord mode))
        , neighbour 122.0 next
        , HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "rect")
            [ HP.attr (AttrName "x") "4", HP.attr (AttrName "y") (num (92.0 + lowered))
            , HP.attr (AttrName "width") (num (40.0 + drumWide)), HP.attr (AttrName "height") "36"
            , HP.attr (AttrName "fill") "transparent"
            , HP.attr (AttrName "class") "fader-press"
            , HE.onClick \_ -> Send (FaderModeSelected next)
            ]
            [ HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "title") []
                [ HH.text ("The selector: the fader is on " <> toLower (faderWord mode) <> ". Click to turn it to " <> toLower (faderWord next) <> ".") ]
            ]
        ]
      )

stateText :: MachineState -> String
stateText = case _ of
  Sleeping -> "asleep"
  Heating -> "heating"
  Ready -> "ready"
  Faulted -> "fault"

stateColour :: MachineState -> String
stateColour = case _ of
  Sleeping -> "#8d9297"
  Heating -> leds.working
  Ready -> leds.ready
  Faulted -> leds.fault

asleep :: MachineState -> Boolean
asleep = case _ of
  Sleeping -> true
  _ -> false

faulted :: MachineState -> Boolean
faulted = case _ of
  Faulted -> true
  _ -> false

fill :: String -> Attr
fill = at "fill"

front :: forall w. FrontInput -> HH.HTML w Action
front v =
  let
    k = lookOf v.night v.arcade
    -- how far along a scale of temperature a reading is
    warmth low high c = clamp 0.0 1.0 ((c - low) / (high - low))
    phase = v.now * 2.0 - floor (v.now * 2.0)
    -- where the middle of the fader's cap sits over the slot
    capX = 60.5 + 279.0 * clamp 0.0 1.0 v.fader
    -- the scale under the slot: eleven marks, with the reading the
    -- selector has chosen written at the ends and the middle
    reading i = case v.mode, i of
      OnPressure, _ -> show i
      OnTemperature, _ -> show (80 + 2 * i)
      OnTime, 0 -> "start"
      OnTime, 5 -> "half"
      OnTime, _ -> "end"
    mark i =
      let
        mx = 60.5 + 27.9 * toNumber i
      in
        g []
          ( [ line [ at "x1" (num mx), at "x2" (num mx), at "y1" (num (81.0 + lowered)), at "y2" (num ((if i `mod` 5 == 0 then 86.0 else 84.0) + lowered)), at "stroke" "#b9bec4", at "stroke-width" "0.8" ] ]
              <>
                if i `mod` 5 == 0 then
                  [ label [ at "x" (num mx), at "y" (num (69.0 + lowered)), at "text-anchor" "middle", at "font-size" "6", at "font-family" sans, at "letter-spacing" "0.4", fill "#b9bec4" ] (reading i) ]
                else []
          )
    fraction = clamp 0.0 1.0 (v.cupGrams / max 1.0 v.targetGrams)
    top = 392.0 - 36.0 * fraction
    -- The pour, from 0 to 1: how fast coffee is reaching the cup,
    -- against the 2 g a second of a full spout. Everything under the
    -- basket is drawn from this one number. Near 0 there are slow,
    -- small drips. As it rises the drips come faster and fatter.
    -- From 0.4 they have run together into a thread, which thickens
    -- into the full cone and column at 1. Turn the flow down during a
    -- shot and the column thins and breaks back into drips.
    pour = if v.pouring then clamp 0.0 1.0 (v.outFlow / 2.0) else 0.0
    threaded = pour >= 0.4
    thick = clamp 0.0 1.0 ((pour - 0.4) / 0.6)
    -- the column: half its width, how far the cone spreads across the
    -- basket, and a slight side-to-side wander
    half = 0.8 + 4.7 * thick
    spread = 10.0 + 34.0 * thick
    sway = if phase < 0.5 then 0.0 else 0.5
    inset y = (394.0 - y) * 6.0 / 42.0
    scaleWide = 16.0 * phi * phi
    scaleLeft = wallLeft + wallWide - 4.0 - scaleWide
    paddle = "M138,316.5 L118,316 C100,315.5 88,309 68,309 C55,309 46,313.5 46,319.5 C46,325.5 55,330 68,330 C88,330 100,323.5 118,323 L138,322.5 Z"
    -- the top of the coffee, as far down as the crema reaches
    creamTo = top + min 14.0 ((392.0 - top) * 0.55)
    crema =
      "M" <> num (178.0 - inset top) <> "," <> num top
        <> " L" <> num (222.0 + inset top) <> "," <> num top
        <> " L" <> num (222.0 + inset creamTo) <> "," <> num creamTo
        <> " L" <> num (178.0 - inset creamTo) <> "," <> num creamTo <> " Z"
    coffee =
      "M" <> num (178.0 - inset top) <> "," <> num top
        <> " L" <> num (222.0 + inset top) <> "," <> num top
        <> " L221.7,392 L178.3,392 Z"
    -- Before the first drops fall, water pushed into the puck shows on
    -- the underside of the bottomless basket: beads form, and a bulge
    -- of coffee swells in the middle. How far it swells goes with the
    -- pressure behind it, from nothing at 1 bar to its fullest at 9,
    -- and with how hot the water is, from nothing at 70 C to its
    -- fullest at 90. A soak at low pressure wets the puck and shows
    -- nothing here. As the pour grows the bulge is drawn down into
    -- it, and it comes back if the flow is turned down with pressure
    -- still behind it.
    swell =
      if v.running then
        clamp 0.0 1.0 ((v.brewBar - 1.0) / 8.0)
          * clamp 0.0 1.0 ((v.waterC - 70.0) / 20.0)
          * clamp 0.0 1.0 (1.0 - pour * 1.5)
      else 0.0
    -- Drips, while the pour is too slow to hold together. They leave
    -- the lowest point under the basket and fall to the coffee in the
    -- cup. There are more of them in the air, and they are fatter, the
    -- faster the pour: one at a crawl, then two, then three.
    drips hang =
      if pour < 0.02 || threaded then []
      else
        let
          count = if pour < 0.12 then 1 else if pour < 0.26 then 2 else 3
          -- how many times a second one of them makes the fall
          rate = if pour < 0.06 then 0.6 else if pour < 0.12 then 1.0 else 1.4
          size = 0.7 + 1.5 * pour
          from = 338.0 + hang
          place i =
            let
              along = v.now * rate + toNumber i / toNumber count
            in
              from + (top - from) * (along - floor along)
          drip i =
            let
              at' = place i
            in
              path
                [ at "d"
                    ( "M200," <> num (at' - 3.2 * size) <> " Q" <> num (200.0 + 2.2 * size) <> "," <> num (at' + 0.6 * size) <> " 200,"
                        <> num (at' + 1.6 * size) <> " Q" <> num (200.0 - 2.2 * size) <> "," <> num (at' + 0.6 * size) <> " 200,"
                        <> num (at' - 3.2 * size) <> " Z"
                    )
                , fill v.streamColour
                ]
        in
          map drip (range 0 (count - 1))
    -- how far the bulge hangs below the basket
    hangs = if swell <= 0.0 then 0.0 else (0.6 + 9.0 * swell) * (if phase < 0.5 then 1.0 else 1.06)
    bulge =
      if swell <= 0.0 then []
      else
        let
          -- it trembles a little as it hangs
          depth = hangs
          reach = 8.0 + 30.0 * swell
          bead x =
            let
              -- the beads nearest the middle are the largest
              r = (0.6 + 1.7 * swell) * (1.0 - abs (x - 200.0) / 70.0)
            in
              circle [ at "cx" (num x), at "cy" (num (338.0 + r * 0.7)), at "r" (num r), fill v.streamColour ]
        in
          map bead [ 166.0, 177.0, 188.0, 212.0, 223.0, 234.0 ]
            <>
              [ path
                  [ at "d"
                      ( "M" <> num (200.0 - reach) <> ",338 Q200," <> num (338.0 + 2.0 * depth) <> " "
                          <> num (200.0 + reach) <> ",338 Z"
                      )
                  , fill v.streamColour
                  ]
              ]
            <>
              -- a glint on it, since it is wet
              [ path
                  [ at "d"
                      ( "M" <> num (200.0 - reach * 0.45) <> "," <> num (338.0 + depth * 0.35) <> " Q"
                          <> num (200.0 - reach * 0.2) <> "," <> num (338.0 + depth * 0.75) <> " 200,"
                          <> num (338.0 + depth * 0.8)
                      )
                  , fill "none", at "stroke" "#ffffff", at "stroke-width" "0.7", at "stroke-linecap" "round", at "opacity" "0.45"
                  ]
              ]
    -- Steam off the tip of the wand, drawn as a cartoon would: a run
    -- of clouds that swell as they fall away, each one three
    -- overlapping rounds with one outline about the lot. They stay
    -- white the whole way, so they show against the dark, with a pale
    -- haze behind each, and the last of them shrinks to nothing
    -- instead of fading to grey.
    cloud scale n =
      let
        drop = (toNumber n + phase) * 10.0
        -- full size until the last stretch of the fall, then closing up
        left = clamp 0.0 1.0 ((50.0 - drop) / 12.0)
        size = scale * (3.6 + drop / 3.6) * left
        cx = 368.5 + (if n `mod` 2 == 0 then -1.0 else 1.0) * (2.0 + drop / 6.0)
        cy = 364.0 + drop
        rounds =
          [ { x: cx, y: cy, r: size }
          , { x: cx - size * 0.85, y: cy + size * 0.25, r: size * 0.68 }
          , { x: cx + size * 0.85, y: cy + size * 0.2, r: size * 0.72 }
          ]
        haze = circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num (size * 2.1)), fill "#ffffff", at "opacity" "0.1" ]
        edge c = circle [ at "cx" (num c.x), at "cy" (num c.y), at "r" (num (c.r + 0.9)), fill (if v.night then "#b9bec4" else "#55595d") ]
        body c = circle [ at "cx" (num c.x), at "cy" (num c.y), at "r" (num c.r), fill "#ffffff" ]
      in
        g [] ([ haze ] <> map edge rounds <> map body rounds)
    -- With the boiler at pressure the valve gives a full run of steam.
    -- Opened on a boiler with no pressure in it, as on a machine woken
    -- for brewing only or one still warming up, it gives a thin wisp:
    -- the lever has done something, and there is nothing behind it.
    steam
      | v.steaming = map (cloud 1.0) (range 0 4)
      | v.steamOpen = map (cloud 0.32) (range 0 1)
      | otherwise = []
  in
    svg [ at "viewBox" "0 0 400 444", at "class" "machine" ]
      ( [ defs
            [ fitted "sugi" (textures <> "sugsugiBan.png") 30.0 26.0 340.0 250.0
            , fitted "wall" (textures <> "brutalistWall.png") wallLeft 276.0 wallWide wallHigh
            , fitted "alu" (textures <> "bushedAlum.png") 30.0 26.0 340.0 250.0
            , texture "wood" (textures <> "woodPanel01.png") 160.0
            , texture "stone" (textures <> "brutalistTexture.png") 200.0
            , glow "dial-light" "#e88a22" 0.26
            , glow "pool" "#ffe9c4" 0.75
            , falling "beam" "#ffe9c4" 0.5 0.06
            , falling "crema" "#e0b072" 0.95 0.0
            ]
        ]
          -- the counter
          <> faced "stone" "#8b8a86" [ at "x" "0", at "y" "426", at "width" "400", at "height" "18", at "rx" "3" ]
          <> (if v.night then [ rect [ at "x" "0", at "y" "426", at "width" "400", at "height" "18", at "rx" "3", fill "#000000", at "opacity" "0.72" ] ] else [])
          -- the steam turret: a steel barrel standing out of the right
          -- flank, with the steam lever in a slot on top
          <>
            [ rect [ at "x" (num (378.0 - 21.0 / phi)), at "y" "152", at "width" (num (42.0 / phi)), at "height" "42", at "rx" "3", fill k.metal, at "stroke" k.metalEdge, at "stroke-width" "0.8" ]
            , rect [ at "x" "373", at "y" "149", at "width" "10", at "height" "5", at "rx" "1.5", fill ink ]
            -- The steam lever, as on a slot machine: an arm with a ball
            -- on the end. It stands up with the valve shut. Pulled, it
            -- swings towards whoever is at the machine and down, which
            -- from the front is the arm shortening to nothing and then
            -- growing downwards. The stylesheet moves it; this only
            -- says which way it is.
            , g [ at "class" ("lever" <> (if v.steamOpen then " pulled" else "")) ]
                [ line
                    [ at "x1" "378", at "y1" "152", at "x2" "378", at "y2" "124"
                    , at "stroke" (if v.night then "#8b9197" else "#3a3d40"), at "stroke-width" "3.4", at "stroke-linecap" "round"
                    ]
                , circle [ at "cx" "378", at "cy" "120", at "r" "6.5", fill scheme.orange, at "stroke" ink, at "stroke-width" "0.8" ]
                , circle [ at "cx" "376", at "cy" "118", at "r" "1.8", fill "#ffffff", at "opacity" "0.55" ]
                ]
            , HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "rect")
                [ HP.attr (AttrName "x") "364", HP.attr (AttrName "y") "110"
                , HP.attr (AttrName "width") "30", HP.attr (AttrName "height") "84"
                , HP.attr (AttrName "fill") "transparent"
                , HP.attr (AttrName "class") "fader-press"
                , HE.onClick \_ -> Disturb \d -> d { steamValveOpen = not d.steamValveOpen }
                ]
                [ HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "title") []
                    [ HH.text (if v.steamOpen then "Push the lever up to shut the steam off" else "Pull the lever down for steam") ]
                ]
            ]
          -- the body: a band of charred cedar across the top and a
          -- face of brushed steel under it
          <> faced "sugi" "#1a110c" [ at "x" "30", at "y" "26", at "width" "340", at "height" "250", at "rx" "8" ]
          <> steel k [ at "x" "30", at "y" (num faceTop), at "width" "340", at "height" (num faceHigh), at "rx" "8" ]
          <> steel k [ at "x" "30", at "y" (num faceTop), at "width" "340", at "height" "12" ]
          <>
            [ line [ at "x1" "30", at "x2" "370", at "y1" (num faceTop), at "y2" (num faceTop), at "stroke" ink, at "stroke-width" "0.8" ]
            , rect
                [ at "x" "30", at "y" "26", at "width" "340", at "height" "250", at "rx" "8", fill "none"
                , at "stroke" (if faulted v.state then bad else if v.night then "#3a3632" else "#55595d")
                , at "stroke-width" (if faulted v.state then "5" else "1")
                ]
            , dualGauge k 104.0 dialMiddle
                { most: 12, labelEvery: 2, redFrom: 11.0, value: v.brewBar }
                { most: 3, labelEvery: 1, redFrom: 2.3, value: v.steamBar }
            -- the machine's state: a dot and a word
            , circle [ at "class" (led k true), at "cx" (num (valueLeft + valueWide + 14.0)), at "cy" (num (110.0 + lowered)), at "r" "3", fill (stateColour v.state) ]
            , label
                [ at "x" (num (valueLeft + valueWide + 22.0)), at "y" (num (112.5 + lowered)), at "font-size" "7.5", at "font-family" sans
                , at "letter-spacing" "0.5", fill k.lettering
                ]
                (stateText v.state)
            , inkDisplay k v.now v.ink
            , faderWindow k v.faderValue v.fader
            -- the three temperatures, one over the other
            , readout k 148.0 "reservoir" (num v.reservoirC) (warmth 20.0 110.0 v.reservoirC)
            , readout k 171.0 "group" (num v.groupC) (warmth 20.0 100.0 v.groupC)
            , readout k 194.0 "water" (num v.waterC) (warmth 20.0 100.0 v.waterC)
            -- the lamps, in a row
            , lamp k 200.0 232.0 "reservoir" v.brewElementOn leds.working
            , lamp k 240.0 232.0 "steam" v.steamElementOn leds.working
            , lamp k 280.0 232.0 "group" v.groupHeaterOn leds.working
            , lamp k 330.0 232.0 "problem" v.problem leds.fault
            -- The fader: a black strip with a slot in it and a scale,
            -- across the top of the face, as far from the group and
            -- its heat as the face allows. The cap is the slider laid
            -- over this drawing by the page, so it can be taken in
            -- hand. The stylesheet places that slider and has to agree
            -- with where the strip is drawn.
            , rect [ at "x" "46", at "y" (num (60.0 + lowered)), at "width" "308", at "height" "30", at "rx" "3", fill window ]
            , rect [ at "x" "56", at "y" (num (71.0 + lowered)), at "width" "288", at "height" "8", at "rx" "4", fill "#000000", at "stroke" "#3a3d40", at "stroke-width" "0.8" ]
            , rect
                [ at "x" "58", at "y" (num (73.5 + lowered)), at "width" (num (max 0.0 (capX - 58.0))), at "height" "3", at "rx" "1.5"
                , fill (if v.manual then leds.byHand else if v.running then k.lit v.fader else "#3a3d40")
                ]
            , g [] (map mark (range 0 10))
            , selector k v.mode
            ]
          -- The backsplash: board-marked concrete between two steel
          -- posts, so the stream and the cup are seen against something
          -- light.
          <> faced "wall" "#bcbcbe" [ at "x" (num wallLeft), at "y" "276", at "width" (num wallWide), at "height" (num wallHigh) ]
          <> [ rect [ at "x" (num wallLeft), at "y" "276", at "width" (num wallWide), at "height" "8", fill "#000000", at "opacity" "0.2" ] ]
          <>
            -- By night the wall is in the dark, and a ring of light
            -- hidden under the group, just above the cup, throws a
            -- beam down onto the cup and a pool onto the tray.
            ( if v.night then
                [ rect [ at "x" (num wallLeft), at "y" "276", at "width" (num wallWide), at "height" (num wallHigh), fill "#000000", at "opacity" "0.84" ]
                , path [ at "d" "M170,336 L230,336 L282,400 L118,400 Z", fill "url(#beam)" ]
                , rect [ at "x" "118", at "y" "384", at "width" "164", at "height" "32", fill "url(#pool)" ]
                ]
              else []
            )
          <>
            -- The scale's display, up on the backsplash and clear of
            -- the tray, where water goes.
            -- 16 high and phi squared times that wide, against the
            -- wall's right edge
            [ rect [ at "x" (num scaleLeft), at "y" "280.5", at "width" (num scaleWide), at "height" "16", at "rx" "2", fill window, at "stroke" "#3a3d40", at "stroke-width" "0.6" ]
            , label
                [ at "x" (num (scaleLeft + scaleWide / 2.0)), at "y" "291.8", at "text-anchor" "middle", at "font-size" "8.6"
                , at "font-family" "monospace", fill (k.lit (clamp 0.0 1.0 (v.cupGrams / max 1.0 v.targetGrams)))
                ]
                (num v.cupGrams <> " g")
            ]
          <> steel k [ at "x" (num (wallLeft - postWide)), at "y" "276", at "width" (num postWide), at "height" (num wallHigh) ]
          <> steel k [ at "x" (num (wallLeft + wallWide)), at "y" "276", at "width" (num postWide), at "height" (num wallHigh) ]
          -- group and portafilter
          <> steel k [ at "x" (num (200.0 - groupWide / 2.0)), at "y" "276", at "width" (num groupWide), at "height" "22", at "rx" "2" ]
          <>
            -- The button on the group: it starts the profile, and stops
            -- a shot that is running. It is a plain steel button with
            -- no light in it.
            [ rect [ at "x" (num (200.0 - groupWide / 2.0)), at "y" "276", at "width" (num groupWide), at "height" "22", at "rx" "2", fill "none", at "stroke" steelEdge, at "stroke-width" "0.8" ]
            , circle [ at "cx" "200", at "cy" "287", at "r" "7.5", fill k.metalEdge ]
            , circle [ at "cx" "200", at "cy" "287", at "r" "6.2", fill k.metal ]
            , circle [ at "cx" "200", at "cy" "287", at "r" "4.2", fill "none", at "stroke" k.metalEdge, at "stroke-width" "0.5" ]
            -- The lamp beside it says whether the machine will pull a
            -- shot: dark asleep, the working colour while it heats,
            -- the ready colour when it will, beating while a shot
            -- runs, and the fault colour when it will not.
            , circle [ at "cx" "226", at "cy" "287", at "r" "4.4", fill ink ]
            , circle
                [ at "class" (led k (not (asleep v.state)) <> (if v.running then " beat" else ""))
                , at "cx" "226", at "cy" "287", at "r" "2.9"
                , fill case v.state of
                    Sleeping -> k.dark
                    Heating -> leds.working
                    Ready -> leds.ready
                    Faulted -> leds.fault
                ]
            , HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "circle")
                [ HP.attr (AttrName "cx") "200", HP.attr (AttrName "cy") "287", HP.attr (AttrName "r") "13"
                , HP.attr (AttrName "fill") "transparent"
                , HP.attr (AttrName "class") "fader-press"
                , HE.onClick \_ -> Send (if v.running then ShotStopRequested else ShotRequested)
                ]
                [ HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "title") [] [ HH.text (if v.running then "Stop the shot" else "Run the profile") ] ]
            ]
          -- the portafilter: a flat wooden paddle of a handle, slim at
          -- the neck where a steel ferrule holds it and broad at the
          -- end; a wide ring; and the underside of a bottomless basket
          <>
            [ path [ at "d" paddle, fill "#1a110c" ]
            , path [ at "d" paddle, fill "url(#wood)" ]
            , path [ at "d" paddle, fill "#120c08", at "opacity" "0.45", at "stroke" "#0c0806", at "stroke-width" "0.6" ]
            , rect [ at "x" "128", at "y" "314.5", at "width" "12", at "height" "10", at "rx" "1", fill k.metal, at "stroke" k.metalEdge, at "stroke-width" "0.6" ]
            ]
          <> steel k [ at "x" "142", at "y" "298", at "width" "116", at "height" "13" ]
          <>
            [ rect [ at "x" "138", at "y" "311", at "width" "124", at "height" "17", at "rx" "2", fill ink ]
            , path [ at "d" "M148,328 L252,328 Q250,337 240,338 L160,338 Q150,337 148,328 Z", fill "#c9ced3", at "stroke" steelEdge, at "stroke-width" "1" ]
            -- the steam wand: out of the foot of the turret on a ball
            -- joint, swung forward and a little inward
            , circle [ at "cx" "378", at "cy" "199", at "r" "8", fill k.metal, at "stroke" k.metalEdge, at "stroke-width" "1" ]
            , path
                [ at "d" "M378,204 C377,250 372,300 369,348", fill "none"
                , at "stroke" (if v.night then "#2a2c2e" else "#8b9197"), at "stroke-width" "7", at "stroke-linecap" "round"
                ]
            , path
                [ at "d" "M378,204 C377,250 372,300 369,348", fill "none"
                , at "stroke" (if v.night then "#6f757b" else "#e3e6e9"), at "stroke-width" "4.6", at "stroke-linecap" "round"
                ]
            , path [ at "d" "M365,346 L373,347 L372,358 L365.5,357 Z", fill ink ]
            ]
          -- drip tray, scale platform, cup
          <> steel k [ at "x" (num (200.0 - trayWide / 2.0)), at "y" "400", at "width" (num trayWide), at "height" "26", at "rx" "2" ]
          <>
            [ rect [ at "x" (num (200.0 - trayWide / 2.0)), at "y" "400", at "width" (num trayWide), at "height" "26", at "rx" "2", fill "none", at "stroke" steelEdge, at "stroke-width" "0.8" ]
            , rect [ at "x" "150", at "y" "394", at "width" "100", at "height" "6", fill "#c5cace", at "stroke" steelEdge, at "stroke-width" "0.6" ]
            , path [ at "d" "M172,352 L228,352 L222,394 L178,394 Z", fill "#f4f1ea", at "stroke" "#8a8378", at "stroke-width" "1.5" ]
            , path [ at "d" coffee, fill "#4a2d18" ]
            -- The crema: the coffee is lighter towards its top, most of
            -- all in the top third.
            , path [ at "d" crema, fill "url(#crema)" ]
            ]
          <>
            ( if threaded then
                -- the streams off the basket gather into one cone, then
                -- fall as a single column that fattens with the flow
                [ path
                    [ at "d"
                        ( "M" <> num (200.0 - spread) <> ",338 Q" <> num (200.0 - half) <> ",346 " <> num (200.0 - half + sway) <> ",358 L"
                            <> num (200.0 + half + sway) <> ",358 Q" <> num (200.0 + half) <> ",346 " <> num (200.0 + spread) <> ",338 Z"
                        )
                    , fill v.streamColour
                    ]
                , rect
                    [ at "x" (num (200.0 - half + sway)), at "y" "357"
                    , at "width" (num (half * 2.0)), at "height" (num (max 0.0 (top - 357.0))), fill v.streamColour
                    ]
                ]
              else []
            )
          <> bulge
          <> drips hangs
          <> steam
      )
