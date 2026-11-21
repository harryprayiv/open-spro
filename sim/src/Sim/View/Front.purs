-- | The machine seen from the front, drawn plainly: a brushed steel
-- | face under a band of charred cedar, one dial for both pressures, an
-- | e-ink display of the shot, three small temperature
-- | readouts, heater lamps, the fader with its selector drum and a
-- | window for the value it is at, the group
-- | with its portafilter in front of a concrete backsplash, the cup on
-- | the scale and the steam wand on its turret. Steel, cedar, wood and
-- | concrete come from the images in sim/imgs/textures; every textured
-- | shape has a flat colour beneath it in case an image is missing.
module Sim.View.Front
  ( FrontInput
  , Ink
  , front
  ) where

import Prelude

import Data.Array (concat, mapWithIndex, range)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (cos, floor, pi, sin)
import Data.Number.Format (fixed, toStringWith)
import Data.String (joinWith, toLower)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sim.Link (Event(..), FaderMode(..), faderWord, nextFaderMode)
import Sim.Model (Action(..))
import Sim.Types (MachineState(..))
import Sim.View.Svg (Attr, at, circle, defs, fitted, g, label, line, num, path, polyline, rect, svg, texture)

type FrontInput =
  { now :: Number
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

bad :: String
bad = "#d32f2f"

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
orange = "#e2572b"

sans :: String
sans = "'Helvetica Neue', Helvetica, Arial, sans-serif"

textures :: String
textures = "../imgs/textures/"

-- | A rectangle with a flat colour and the named texture over it.
faced :: forall w i. String -> String -> Array Attr -> Array (HH.HTML w i)
faced name fallback attrs =
  [ rect (attrs <> [ at "fill" fallback ])
  , rect (attrs <> [ at "fill" ("url(#" <> name <> ")") ])
  ]

-- | Brushed steel: the texture with a wash of white over it, so black
-- | lettering reads on it.
steel :: forall w i. Array Attr -> Array (HH.HTML w i)
steel attrs =
  faced "alu" "#b9bec4" attrs
    <> [ rect (attrs <> [ at "fill" "#ffffff", at "opacity" "0.42" ]) ]

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
-- | its `redFrom` up.
type Scale = { most :: Int, labelEvery :: Int, redFrom :: Number, value :: Number }

dualGauge :: forall w i. Number -> Number -> Scale -> Scale -> HH.HTML w i
dualGauge cx cy brew steam =
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
    mark colour from long short scale parts n =
      let
        whole = n `mod` parts == 0
        angle = sweep scale (toNumber n / toNumber parts)
        outer = onDial cx cy from angle
        inner = onDial cx cy (from - (if whole then long else short)) angle
      in
        line
          [ at "x1" (num outer.x), at "y1" (num outer.y)
          , at "x2" (num inner.x), at "y2" (num inner.y)
          , at "stroke" colour, at "stroke-width" (if whole then "1.3" else "0.6")
          ]
    numeral colour radius size scale n =
      let
        p = onDial cx cy radius (sweep scale (toNumber n))
      in
        label
          [ at "x" (num p.x), at "y" (num (p.y + size * 0.36))
          , at "text-anchor" "middle", at "font-size" (num size), at "font-family" sans, at "fill" colour
          ]
          (show n)
    labelled scale = map (_ * scale.labelEvery) (range 0 (scale.most / scale.labelEvery))
    hand scale reach width colour =
      let
        angle = sweep scale scale.value
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
      ( [ circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num (r + 3.0)), at "fill" ink ]
        , circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num r), at "fill" paper ]
        , path [ at "d" (arc (r - 4.5) brew), at "fill" "none", at "stroke" bad, at "stroke-width" "2" ]
        , path [ at "d" (arc (r - 31.5) steam), at "fill" "none", at "stroke" bad, at "stroke-width" "1.5" ]
        ]
          <> map (mark ink (r - 6.0) 7.0 3.5 brew 2) (range 0 (brew.most * 2))
          <> map (numeral ink (r - 20.0) 8.5 brew) (labelled brew)
          <> map (mark orange (r - 33.0) 4.0 2.0 steam 2) (range 0 (steam.most * 2))
          <> map (numeral orange (r - 43.5) 6.5 steam) (labelled steam)
          <>
            [ word 49.0 quiet "brew  bar"
            , word 30.0 orange "steam"
            , hand steam (r - 34.0) "1.6" orange
            , hand brew (r - 8.0) "2" ink
            , circle [ at "cx" (num cx), at "cy" (num cy), at "r" "3.4", at "fill" ink ]
            ]
      )

-- | A lamp: a small dot that is lit or not, with its name under it.
lamp :: forall w i. Number -> Number -> String -> Boolean -> String -> HH.HTML w i
lamp x y title lit colour =
  g []
    [ circle
        [ at "cx" (num x), at "cy" (num y), at "r" "3.6"
        , at "fill" (if lit then colour else "#8d9297")
        , at "stroke" ink, at "stroke-width" "0.6"
        ]
    , label
        [ at "x" (num x), at "y" (num (y + 12.0)), at "text-anchor" "middle"
        , at "font-size" "5.5", at "font-family" sans, at "letter-spacing" "0.3", at "fill" quiet
        ]
        title
    ]

-- | A small figure in a black window, its name to the left of it.
readout :: forall w i. Number -> String -> String -> HH.HTML w i
readout y title value =
  g []
    [ label
        [ at "x" "301", at "y" (num (y + 11.5)), at "text-anchor" "end", at "font-size" "5.5"
        , at "font-family" sans, at "letter-spacing" "0.3", at "fill" quiet
        ]
        title
    , rect [ at "x" "306", at "y" (num y), at "width" "48", at "height" "17", at "rx" "2", at "fill" window ]
    , label
        [ at "x" "330", at "y" (num (y + 12.5)), at "text-anchor" "middle"
        , at "font-size" "10.5", at "font-family" "monospace", at "fill" paper
        ]
        value
    ]

-- | The e-ink display: the shot as a timeline, in greys. Each step of
-- | the profile is a band, the step the shot is in is the darkest, the
-- | pressure the profile will ask for is a dashed line and the
-- | pressure asked for so far is a solid black one. An upright line
-- | marks how far the shot has run. E-ink has no colour and holds its
-- | picture with the power off.
inkDisplay :: forall w i. Ink -> HH.HTML w i
inkDisplay page =
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
          <> concat (mapWithIndex band page.bands)
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
      )

-- | The window over the middle of the fader: what the fader stands
-- | for, and the value it is at. On time, where the fader cannot be
-- | moved, the word is red.
faderWindow :: forall w i. FaderMode -> String -> HH.HTML w i
faderWindow mode value =
  g []
    [ rect [ at "x" "146", at "y" "200", at "width" "108", at "height" "28", at "rx" "2", at "fill" window ]
    , label
        [ at "x" "153", at "y" "217.5", at "font-size" "6", at "font-family" sans
        , at "letter-spacing" "0.5", at "fill" (if mode == OnTime then "#ff4d3d" else "#9aa0a6")
        ]
        (toLower (faderWord mode))
    , label
        [ at "x" "248", at "y" "220", at "text-anchor" "end"
        , at "font-size" "15", at "font-family" "monospace", at "fill" paper
        ]
        value
    ]

-- | The selector: a drum with the three words written round it, turned
-- | by the knurled wheel that stands out of the left flank. The window
-- | beside the wheel faces the front and shows the word the drum is
-- | on, with the edges of its neighbours above and below. On time,
-- | where the fader cannot be moved, the word is red.
selector :: forall w. FaderMode -> HH.HTML w Action
selector mode =
  let
    next = nextFaderMode mode
    before = nextFaderMode next
    groove i =
      let
        gy = 203.5 + 3.5 * toNumber i
      in
        line [ at "x1" "9", at "x2" "29", at "y1" (num gy), at "y2" (num gy), at "stroke" steelEdge, at "stroke-width" "0.8" ]
    neighbour y text =
      label
        [ at "x" "68", at "y" y, at "text-anchor" "middle", at "font-size" "5", at "font-family" sans
        , at "letter-spacing" "0.5", at "fill" "#6c7176"
        ]
        (toLower (faderWord text))
  in
    g []
      ( [ rect [ at "x" "8", at "y" "200", at "width" "22", at "height" "28", at "rx" "2.5", at "fill" "#c5cace", at "stroke" steelEdge, at "stroke-width" "0.8" ]
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
  Heating -> "#e2572b"
  Ready -> "#2f9e5b"
  Faulted -> "#d32f2f"

faulted :: MachineState -> Boolean
faulted = case _ of
  Faulted -> true
  _ -> false

fill :: String -> Attr
fill = at "fill"

front :: forall w. FrontInput -> HH.HTML w Action
front v =
  let
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
    puff n =
      let
        drop = (toNumber n + phase) * 9.0
      in
        circle
          [ at "cx" (num (368.5 + (if n `mod` 2 == 0 then -3.0 else 3.0)))
          , at "cy" (num (366.0 + drop))
          , at "r" (num (3.0 + drop / 5.0))
          , fill "#ffffff"
          , at "stroke" "#b9bec4"
          , at "stroke-width" "0.6"
          , at "opacity" (num (0.75 - drop / 40.0))
          ]
  in
    svg [ at "viewBox" "0 0 400 444", at "class" "machine" ]
      ( [ defs
            [ fitted "sugi" (textures <> "sugsugiBan.png") 30.0 26.0 340.0 250.0
            , fitted "wall" (textures <> "brutalistWall.png") 88.0 276.0 224.0 124.0
            , fitted "alu" (textures <> "bushedAlum.png") 30.0 26.0 340.0 250.0
            , texture "wood" (textures <> "woodPanel01.png") 160.0
            , texture "stone" (textures <> "brutalistTexture.png") 200.0
            ]
        ]
          -- the counter
          <> faced "stone" "#8b8a86" [ at "x" "0", at "y" "426", at "width" "400", at "height" "18", at "rx" "3" ]
          -- the steam turret: a steel barrel standing out of the right
          -- flank, with the steam valve's cedar knob on top
          <>
            [ rect [ at "x" "364", at "y" "152", at "width" "28", at "height" "42", at "rx" "3", fill "#c5cace", at "stroke" steelEdge, at "stroke-width" "0.8" ]
            , rect [ at "x" "376", at "y" "142", at "width" "6", at "height" "10", fill "#c5cace", at "stroke" steelEdge, at "stroke-width" "0.6" ]
            , rect [ at "x" "369", at "y" "130", at "width" "20", at "height" "13", at "rx" "2", fill "#1a110c" ]
            , rect [ at "x" "369", at "y" "130", at "width" "20", at "height" "13", at "rx" "2", fill "url(#wood)", at "opacity" "0.55" ]
            ]
          -- the body: a band of charred cedar across the top and a
          -- face of brushed steel under it
          <> faced "sugi" "#1a110c" [ at "x" "30", at "y" "26", at "width" "340", at "height" "250", at "rx" "8" ]
          <> steel [ at "x" "30", at "y" "52", at "width" "340", at "height" "224", at "rx" "8" ]
          <> steel [ at "x" "30", at "y" "52", at "width" "340", at "height" "12" ]
          <>
            [ line [ at "x1" "30", at "x2" "370", at "y1" "52", at "y2" "52", at "stroke" ink, at "stroke-width" "0.8" ]
            , rect
                [ at "x" "30", at "y" "26", at "width" "340", at "height" "250", at "rx" "8", fill "none"
                , at "stroke" (if faulted v.state then bad else "#55595d")
                , at "stroke-width" (if faulted v.state then "5" else "1")
                ]
            , dualGauge 104.0 128.0
                { most: 12, labelEvery: 2, redFrom: 11.0, value: v.brewBar }
                { most: 3, labelEvery: 1, redFrom: 2.3, value: v.steamBar }
            -- the machine's state: a dot and a word
            , circle [ at "cx" "196", at "cy" "79", at "r" "3", fill (stateColour v.state) ]
            , label
                [ at "x" "204", at "y" "81.5", at "font-size" "7.5", at "font-family" sans
                , at "letter-spacing" "0.5", fill ink
                ]
                (stateText v.state)
            , inkDisplay v.ink
            , faderWindow v.mode v.faderValue
            -- the three temperatures, one over the other
            , readout 96.0 "reservoir" (num v.reservoirC)
            , readout 119.0 "group" (num v.groupC)
            , readout 142.0 "water" (num v.waterC)
            -- the lamps, in a row
            , lamp 200.0 172.0 "reservoir" v.brewElementOn orange
            , lamp 240.0 172.0 "steam" v.steamElementOn orange
            , lamp 280.0 172.0 "group" v.groupHeaterOn orange
            , lamp 330.0 172.0 "problem" v.problem bad
            -- the fader: a black strip across the front with a slot in
            -- it and a scale. The cap is the slider laid over this
            -- drawing by the page, so it can be taken in hand.
            , rect [ at "x" "46", at "y" "234", at "width" "308", at "height" "30", at "rx" "3", fill window ]
            , rect [ at "x" "56", at "y" "245", at "width" "288", at "height" "8", at "rx" "4", fill "#000000", at "stroke" "#3a3d40", at "stroke-width" "0.8" ]
            , rect
                [ at "x" "58", at "y" "247.5", at "width" (num (max 0.0 (capX - 58.0))), at "height" "3", at "rx" "1.5"
                , fill (if v.manual then orange else if v.running then paper else "#3a3d40")
                ]
            , g [] (map mark (range 0 10))
            , selector v.mode
            ]
          -- The backsplash: board-marked concrete between two steel
          -- posts, so the stream and the cup are seen against something
          -- light.
          <> faced "wall" "#bcbcbe" [ at "x" "88", at "y" "276", at "width" "224", at "height" "124" ]
          <> [ rect [ at "x" "88", at "y" "276", at "width" "224", at "height" "8", fill "#000000", at "opacity" "0.2" ] ]
          <> steel [ at "x" "84", at "y" "276", at "width" "5", at "height" "124" ]
          <> steel [ at "x" "311", at "y" "276", at "width" "5", at "height" "124" ]
          -- group and portafilter
          <> steel [ at "x" "150", at "y" "276", at "width" "100", at "height" "22", at "rx" "2" ]
          <>
            -- The button on the group: it starts the profile, and stops
            -- a shot that is running.
            [ rect [ at "x" "150", at "y" "276", at "width" "100", at "height" "22", at "rx" "2", fill "none", at "stroke" steelEdge, at "stroke-width" "0.8" ]
            , circle [ at "cx" "200", at "cy" "287", at "r" "7.5", fill ink ]
            , circle [ at "cx" "200", at "cy" "287", at "r" "3.2", fill (if v.running then orange else "#2f9e5b") ]
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
          <> steel [ at "x" "142", at "y" "298", at "width" "116", at "height" "13" ]
          <>
            [ rect [ at "x" "138", at "y" "311", at "width" "124", at "height" "17", at "rx" "2", fill ink ]
            , path [ at "d" "M148,328 L252,328 Q250,337 240,338 L160,338 Q150,337 148,328 Z", fill "#c9ced3", at "stroke" steelEdge, at "stroke-width" "1" ]
            -- the steam wand: out of the foot of the turret on a ball
            -- joint, swung forward and a little inward
            , circle [ at "cx" "378", at "cy" "199", at "r" "8", fill "#c9ced3", at "stroke" steelEdge, at "stroke-width" "1" ]
            , path
                [ at "d" "M378,204 C377,250 372,300 369,348", fill "none"
                , at "stroke" "#8b9197", at "stroke-width" "7", at "stroke-linecap" "round"
                ]
            , path
                [ at "d" "M378,204 C377,250 372,300 369,348", fill "none"
                , at "stroke" "#e3e6e9", at "stroke-width" "4.6", at "stroke-linecap" "round"
                ]
            , path [ at "d" "M365,346 L373,347 L372,358 L365.5,357 Z", fill ink ]
            ]
          -- drip tray, scale platform, cup
          <> steel [ at "x" "80", at "y" "400", at "width" "240", at "height" "26", at "rx" "2" ]
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
            ( if v.pouring then
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
          <> (if v.steaming then map puff (range 0 3) else [])
      )
