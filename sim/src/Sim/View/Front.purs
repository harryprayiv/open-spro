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
import Data.Maybe (Maybe(..))
import Data.Number (abs, cos, floor, pi, sin)
import Data.Number.Format (fixed, toStringWith)
import Data.String (joinWith, toLower)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
  , flowMlPerS :: Number
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

lookOf :: Boolean -> Look
lookOf night =
  if night then
    { night
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
    r = 58.0
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
lamp :: forall w i. Look -> Number -> Number -> String -> Boolean -> String -> HH.HTML w i
lamp k x y title lit colour =
  g []
    [ circle
        [ at "cx" (num x), at "cy" (num y), at "r" "7"
        , at "fill" colour, at "opacity" (if not lit then "0" else if k.night then "0.3" else "0.16")
        ]
    , circle
        [ at "cx" (num x), at "cy" (num y), at "r" "3.6"
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
    , rect [ at "x" "306", at "y" (num y), at "width" "48", at "height" "17", at "rx" "2", at "fill" window ]
    , label
        [ at "x" "330", at "y" (num (y + 12.5)), at "text-anchor" "middle"
        , at "font-size" "10.5", at "font-family" "monospace", at "fill" (k.lit fraction)
        ]
        value
    ]

-- | The e-ink display: the shot as a timeline, in greys. Each step of
-- | the profile is a band, the step the shot is in is the darkest, the
-- | pressure the profile will ask for is a dashed line and the
-- | pressure asked for so far is a solid black one. An upright line
-- | marks how far the shot has run. E-ink has no colour and holds its
-- | picture with the power off.
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
    top = 93.0
    width = 91.0
    height = 52.0
    span = max 1.0 page.span
    x t = left + clamp 0.0 1.0 (t / span) * width
    y bar = top + height - clamp 0.0 12.0 bar / 12.0 * (height - 9.0)
    shade i
      | page.step == Just i = "#a9aaa4"
      | i `mod` 2 == 0 = "#d6d7d0"
      | otherwise = "#c4c5be"
    band i seg =
      [ rect
          [ at "x" (num (x seg.start)), at "y" (num top)
          , at "width" (num (max 0.0 (x seg.end - x seg.start))), at "height" (num height)
          , at "fill" (shade i)
          ]
      , label
          [ at "x" (num (x seg.start + 2.0)), at "y" (num (top + 6.5)), at "font-size" "5"
          , at "font-family" sans, at "fill" "#55564f"
          ]
          (show (i + 1))
      ]
    points samples = joinWith " " (map (\s -> num (x s.t) <> "," <> num (y s.bar)) samples)
    limit bar =
      line
        [ at "x1" (num left), at "x2" (num (left + width)), at "y1" (num (y bar)), at "y2" (num (y bar))
        , at "stroke" "#55564f", at "stroke-width" "0.6", at "stroke-dasharray" "1 2"
        ]
    limits = case page.limits of
      Just held -> [ limit held.low, limit held.high ]
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
      , words "4.6" "400" "0.4" "#1c1c1a" 48.5 "press here to wake the machine"
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
      , label
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
      , words "4.6" "400" "0.4" "#1c1c1a" 48.5 "press here to skip the wait"
      , underline 30.0
      , press "Skip the warm-up. Only the simulator can do this." SkipWarmUp
      ]
    shotView =
      concat (mapWithIndex band page.bands)
        <> limits
        <>
          [ polyline
              [ at "fill" "none", at "points" (points page.target)
              , at "stroke" "#6f706a", at "stroke-width" "0.9", at "stroke-dasharray" "2 1.5"
              ]
          , polyline
              [ at "fill" "none", at "points" (points page.drawn)
              , at "stroke" "#1c1c1a", at "stroke-width" "1.4", at "stroke-linejoin" "round"
              ]
          ]
        <> playLine
        <>
          [ label
              [ at "x" (num (left + width - 2.0)), at "y" (num (top + height - 2.5)), at "text-anchor" "end"
              , at "font-size" "5.5", at "font-family" sans, at "fill" "#1c1c1a"
              ]
              (toStringWith (fixed 0) (if page.running then page.place else span) <> " s")
          ]
    playLine =
      if page.running then
        [ line
            [ at "x1" (num (x page.place)), at "x2" (num (x page.place)), at "y1" (num top), at "y2" (num (top + height))
            , at "stroke" "#1c1c1a", at "stroke-width" "0.9"
            ]
        ]
      else []
  in
    g []
      ( [ rect [ at "x" "172", at "y" "88", at "width" "101", at "height" "62", at "rx" "3", at "fill" window ]
        , rect [ at "x" (num left), at "y" (num top), at "width" (num width), at "height" (num height), at "fill" "#e3e4dd" ]
        ]
          <>
            ( if page.asleep then sleeping
              else if page.heating && not page.settled then warming
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

-- | The window over the middle of the fader: the value the fader is
-- | at. What the value is a value of is in the selector's window beside
-- | it.
faderWindow :: forall w i. Look -> String -> Number -> HH.HTML w i
faderWindow k value fraction =
  g []
    [ rect [ at "x" "154", at "y" "200", at "width" "92", at "height" "28", at "rx" "2", at "fill" window ]
    , label
        [ at "x" "200", at "y" "220", at "text-anchor" "middle"
        , at "font-size" "15", at "font-family" "monospace", at "fill" (k.lit fraction)
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
        gy = 203.5 + 3.5 * toNumber i
      in
        line [ at "x1" "9", at "x2" "29", at "y1" (num gy), at "y2" (num gy), at "stroke" k.metalEdge, at "stroke-width" "0.8" ]
    neighbour y text =
      label
        [ at "x" "68", at "y" y, at "text-anchor" "middle", at "font-size" "5", at "font-family" sans
        , at "letter-spacing" "0.5", at "fill" "#6c7176"
        ]
        (toLower (faderWord text))
  in
    g []
      ( [ rect [ at "x" "8", at "y" "200", at "width" "22", at "height" "28", at "rx" "2.5", at "fill" k.metal, at "stroke" k.metalEdge, at "stroke-width" "0.8" ]
        , g [] (map groove (range 0 6))
        , rect [ at "x" "40", at "y" "200", at "width" "56", at "height" "28", at "rx" "2", at "fill" window ]
        , neighbour "206.5" before
        , label
            [ at "x" "68", at "y" "217", at "text-anchor" "middle", at "font-size" "8", at "font-family" sans
            , at "letter-spacing" "0.6", at "fill" (if mode == OnTime then "#ff4d3d" else paper)
            ]
            (toLower (faderWord mode))
        , neighbour "226" next
        , HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "rect")
            [ HP.attr (AttrName "x") "4", HP.attr (AttrName "y") "196"
            , HP.attr (AttrName "width") "96", HP.attr (AttrName "height") "36"
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

faulted :: MachineState -> Boolean
faulted = case _ of
  Faulted -> true
  _ -> false

fill :: String -> Attr
fill = at "fill"

front :: forall w. FrontInput -> HH.HTML w Action
front v =
  let
    k = lookOf v.night
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
          ( [ line [ at "x1" (num mx), at "x2" (num mx), at "y1" "255", at "y2" (if i `mod` 5 == 0 then "260" else "258"), at "stroke" "#b9bec4", at "stroke-width" "0.8" ] ]
              <>
                if i `mod` 5 == 0 then
                  [ label [ at "x" (num mx), at "y" "243", at "text-anchor" "middle", at "font-size" "6", at "font-family" sans, at "letter-spacing" "0.4", fill "#b9bec4" ] (reading i) ]
                else []
          )
    fraction = clamp 0.0 1.0 (v.cupGrams / max 1.0 v.targetGrams)
    top = 392.0 - 36.0 * fraction
    -- stream: half its width, how far it spreads across the basket and
    -- a slight side-to-side wander
    half = clamp 1.5 5.5 (1.2 + v.flowMlPerS * 0.9)
    spread = clamp 14.0 44.0 (16.0 + v.flowMlPerS * 9.0)
    sway = if phase < 0.5 then 0.0 else 0.5
    inset y = (394.0 - y) * 6.0 / 42.0
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
    -- nothing here. The first gram or so leaves the bulge as drips.
    -- Then the stream takes over, and the bulge is drawn down into it
    -- over the first 4 g in the cup.
    swell =
      if v.running then
        clamp 0.0 1.0 ((v.brewBar - 1.0) / 8.0)
          * clamp 0.0 1.0 ((v.waterC - 70.0) / 20.0)
          * clamp 0.0 1.0 (1.0 - v.cupGrams / 4.0)
      else 0.0
    -- The stream proper starts once the drips have run together.
    streaming = v.pouring && v.cupGrams > 1.2
    bulge =
      if swell <= 0.0 then []
      else
        let
          -- it trembles a little as it hangs
          depth = (0.6 + 9.0 * swell) * (if phase < 0.5 then 1.0 else 1.06)
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
              -- drips off the lowest point of it, until the stream starts
              ( if v.pouring && not streaming then
                  let
                    from = 338.0 + depth
                    fall offset = from + (top - from) * (phase + offset - floor (phase + offset))
                    drip offset =
                      path
                        [ at "d"
                            ( "M200," <> num (fall offset - 3.2) <> " Q202.2," <> num (fall offset + 0.6) <> " 200,"
                                <> num (fall offset + 1.6) <> " Q197.8," <> num (fall offset + 0.6) <> " 200," <> num (fall offset - 3.2) <> " Z"
                            )
                        , fill v.streamColour
                        ]
                  in
                    [ drip 0.0, drip 0.5 ]
                else []
              )
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
            , fitted "wall" (textures <> "brutalistWall.png") 88.0 276.0 224.0 124.0
            , fitted "alu" (textures <> "bushedAlum.png") 30.0 26.0 340.0 250.0
            , texture "wood" (textures <> "woodPanel01.png") 160.0
            , texture "stone" (textures <> "brutalistTexture.png") 200.0
            , glow "dial-light" "#e88a22" 0.26
            , glow "pool" "#ffe9c4" 0.75
            , falling "beam" "#ffe9c4" 0.5 0.06
            ]
        ]
          -- the counter
          <> faced "stone" "#8b8a86" [ at "x" "0", at "y" "426", at "width" "400", at "height" "18", at "rx" "3" ]
          <> (if v.night then [ rect [ at "x" "0", at "y" "426", at "width" "400", at "height" "18", at "rx" "3", fill "#000000", at "opacity" "0.72" ] ] else [])
          -- the steam turret: a steel barrel standing out of the right
          -- flank, with the steam lever in a slot on top
          <>
            [ rect [ at "x" "364", at "y" "152", at "width" "28", at "height" "42", at "rx" "3", fill k.metal, at "stroke" k.metalEdge, at "stroke-width" "0.8" ]
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
          <> steel k [ at "x" "30", at "y" "52", at "width" "340", at "height" "224", at "rx" "8" ]
          <> steel k [ at "x" "30", at "y" "52", at "width" "340", at "height" "12" ]
          <>
            [ line [ at "x1" "30", at "x2" "370", at "y1" "52", at "y2" "52", at "stroke" ink, at "stroke-width" "0.8" ]
            , rect
                [ at "x" "30", at "y" "26", at "width" "340", at "height" "250", at "rx" "8", fill "none"
                , at "stroke" (if faulted v.state then bad else if v.night then "#3a3632" else "#55595d")
                , at "stroke-width" (if faulted v.state then "5" else "1")
                ]
            , dualGauge k 104.0 128.0
                { most: 12, labelEvery: 2, redFrom: 11.0, value: v.brewBar }
                { most: 3, labelEvery: 1, redFrom: 2.3, value: v.steamBar }
            -- the machine's state: a dot and a word
            , circle [ at "cx" "196", at "cy" "79", at "r" "3", fill (stateColour v.state) ]
            , label
                [ at "x" "204", at "y" "81.5", at "font-size" "7.5", at "font-family" sans
                , at "letter-spacing" "0.5", fill k.lettering
                ]
                (stateText v.state)
            , inkDisplay k v.now v.ink
            , faderWindow k v.faderValue v.fader
            -- the three temperatures, one over the other
            , readout k 96.0 "reservoir" (num v.reservoirC) (warmth 20.0 110.0 v.reservoirC)
            , readout k 119.0 "group" (num v.groupC) (warmth 20.0 100.0 v.groupC)
            , readout k 142.0 "water" (num v.waterC) (warmth 20.0 100.0 v.waterC)
            -- the lamps, in a row
            , lamp k 200.0 172.0 "reservoir" v.brewElementOn leds.working
            , lamp k 240.0 172.0 "steam" v.steamElementOn leds.working
            , lamp k 280.0 172.0 "group" v.groupHeaterOn leds.working
            , lamp k 330.0 172.0 "problem" v.problem leds.fault
            -- the fader: a black strip across the front with a slot in
            -- it and a scale. The cap is the slider laid over this
            -- drawing by the page, so it can be taken in hand.
            , rect [ at "x" "46", at "y" "234", at "width" "308", at "height" "30", at "rx" "3", fill window ]
            , rect [ at "x" "56", at "y" "245", at "width" "288", at "height" "8", at "rx" "4", fill "#000000", at "stroke" "#3a3d40", at "stroke-width" "0.8" ]
            , rect
                [ at "x" "58", at "y" "247.5", at "width" (num (max 0.0 (capX - 58.0))), at "height" "3", at "rx" "1.5"
                , fill (if v.manual then leds.byHand else if v.running then k.lit v.fader else "#3a3d40")
                ]
            , g [] (map mark (range 0 10))
            , selector k v.mode
            ]
          -- The backsplash: board-marked concrete between two steel
          -- posts, so the stream and the cup are seen against something
          -- light.
          <> faced "wall" "#bcbcbe" [ at "x" "88", at "y" "276", at "width" "224", at "height" "124" ]
          <> [ rect [ at "x" "88", at "y" "276", at "width" "224", at "height" "8", fill "#000000", at "opacity" "0.2" ] ]
          <>
            -- By night the wall is in the dark, and a ring of light
            -- hidden under the group, just above the cup, throws a
            -- beam down onto the cup and a pool onto the tray.
            ( if v.night then
                [ rect [ at "x" "88", at "y" "276", at "width" "224", at "height" "124", fill "#000000", at "opacity" "0.84" ]
                , path [ at "d" "M170,336 L230,336 L282,400 L118,400 Z", fill "url(#beam)" ]
                , rect [ at "x" "118", at "y" "384", at "width" "164", at "height" "32", fill "url(#pool)" ]
                ]
              else []
            )
          <> steel k [ at "x" "84", at "y" "276", at "width" "5", at "height" "124" ]
          <> steel k [ at "x" "311", at "y" "276", at "width" "5", at "height" "124" ]
          -- group and portafilter
          <> steel k [ at "x" "150", at "y" "276", at "width" "100", at "height" "22", at "rx" "2" ]
          <>
            -- The button on the group: it starts the profile, and stops
            -- a shot that is running.
            [ rect [ at "x" "150", at "y" "276", at "width" "100", at "height" "22", at "rx" "2", fill "none", at "stroke" steelEdge, at "stroke-width" "0.8" ]
            , circle [ at "cx" "200", at "cy" "287", at "r" "7.5", fill ink ]
            , circle [ at "cx" "200", at "cy" "287", at "r" "3.2", fill (if v.running then leds.working else leds.ready) ]
            , HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "circle")
                [ HP.attr (AttrName "cx") "200", HP.attr (AttrName "cy") "287", HP.attr (AttrName "r") "13"
                , HP.attr (AttrName "fill") "transparent"
                , HP.attr (AttrName "class") "fader-press"
                , HE.onClick \_ -> Send (if v.running then ShotStopRequested else ShotRequested)
                ]
                [ HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "title") [] [ HH.text (if v.running then "Stop the shot" else "Run the profile") ] ]
            ]
          -- the portafilter: a long cedar handle, a wide ring and the
          -- underside of a bottomless basket
          <> faced "wood" "#1a110c" [ at "x" "52", at "y" "312", at "width" "96", at "height" "14", at "rx" "7" ]
          <> [ rect [ at "x" "52", at "y" "312", at "width" "96", at "height" "14", at "rx" "7", fill "#120c08", at "opacity" "0.55" ] ]
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
          <> steel k [ at "x" "80", at "y" "400", at "width" "240", at "height" "26", at "rx" "2" ]
          <>
            [ rect [ at "x" "80", at "y" "400", at "width" "240", at "height" "26", at "rx" "2", fill "none", at "stroke" steelEdge, at "stroke-width" "0.8" ]
            , rect [ at "x" "150", at "y" "394", at "width" "100", at "height" "6", fill "#c5cace", at "stroke" steelEdge, at "stroke-width" "0.6" ]
            , path [ at "d" "M172,352 L228,352 L222,394 L178,394 Z", fill "#f4f1ea", at "stroke" "#8a8378", at "stroke-width" "1.5" ]
            , path [ at "d" coffee, fill "#5a3a22" ]
            , rect [ at "x" "170", at "y" "404", at "width" "60", at "height" "16", at "rx" "2", fill window ]
            , label
                [ at "x" "200", at "y" "416", at "text-anchor" "middle", at "font-size" "11"
                , at "font-family" "monospace", fill paper
                ]
                (num v.cupGrams <> " g")
            ]
          <>
            ( if streaming then
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
          <> steam
      )
