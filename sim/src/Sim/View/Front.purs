-- | The machine seen from the front: two dial gauges, three temperature
-- | displays, heater lamps, the paddle, the group with its portafilter,
-- | the cup on the scale and the steam wand. Brushed aluminium, brass,
-- | wood and concrete come from the images in sim/imgs/textures; every
-- | textured shape has a flat colour beneath it in case an image is
-- | missing.
module Sim.View.Front
  ( FrontInput
  , front
  ) where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Data.Number (cos, floor, pi, sin)
import Halogen.HTML as HH
import Sim.Types (MachineState(..))
import Sim.View.Svg (Attr, at, circle, defs, g, label, line, num, path, rect, sheen, svg, texture)

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
  -- 0 is off, 1 is pre-brew, 2 is brew.
  , paddle :: Int
  -- Something on the page's problem list is active.
  , problem :: Boolean
  , flowMlPerS :: Number
  -- The colour of the coffee leaving the basket.
  , streamColour :: String
  }

bad :: String
bad = "#d32f2f"

steel :: String
steel = "#d9dcdf"

steelDark :: String
steelDark = "#8b9197"

panel :: String
panel = "#1f1b19"

textures :: String
textures = "../imgs/textures/"

-- | A rectangle with a flat colour and the named texture over it.
faced :: forall w i. String -> String -> Array Attr -> Array (HH.HTML w i)
faced name fallback attrs =
  [ rect (attrs <> [ at "fill" fallback ])
  , rect (attrs <> [ at "fill" ("url(#" <> name <> ")") ])
  ]

-- | A point on a dial. Zero degrees is straight up, positive is clockwise.
onDial :: Number -> Number -> Number -> Number -> { x :: Number, y :: Number }
onDial cx cy radius degrees =
  let
    a = degrees * pi / 180.0
  in
    { x: cx + radius * sin a, y: cy - radius * cos a }

-- | A dial gauge. The scale from `redFrom` up is banded in red, and the
-- | needle hub turns red while the reading is inside the band.
gauge :: forall w i. Number -> Number -> String -> Int -> Int -> Number -> Number -> HH.HTML w i
gauge cx cy title maxValue labelEvery redFrom value =
  let
    r = 52.0
    sweep v = -135.0 + 270.0 * clamp 0.0 1.0 (v / toNumber maxValue)
    bandStart = onDial cx cy (r - 3.0) (sweep redFrom)
    bandEnd = onDial cx cy (r - 3.0) (sweep (toNumber maxValue))
    band =
      "M" <> num bandStart.x <> "," <> num bandStart.y
        <> " A" <> num (r - 3.0) <> "," <> num (r - 3.0) <> " 0 0 1 "
        <> num bandEnd.x <> "," <> num bandEnd.y
    tick n =
      let
        major = n `mod` labelEvery == 0
        outer = onDial cx cy (r - 5.0) (sweep (toNumber n))
        inner = onDial cx cy (r - (if major then 15.0 else 10.0)) (sweep (toNumber n))
      in
        line
          [ at "x1" (num outer.x), at "y1" (num outer.y)
          , at "x2" (num inner.x), at "y2" (num inner.y)
          , at "stroke" "#1d1d1b", at "stroke-width" (if major then "2" else "1")
          ]
    number n =
      let
        p = onDial cx cy (r - 23.0) (sweep (toNumber n))
      in
        label
          [ at "x" (num p.x), at "y" (num (p.y + 3.5))
          , at "text-anchor" "middle", at "font-size" "10", at "fill" "#1d1d1b"
          ]
          (show n)
    marks = range 0 maxValue
    labelled = map (_ * labelEvery) (range 0 (maxValue / labelEvery))
    tip = onDial cx cy (r - 12.0) (sweep value)
  in
    g []
      ( [ circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num (r + 5.0)), at "fill" "#b08d57" ]
        , circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num (r + 5.0)), at "fill" "url(#brass)", at "stroke" "#5c4520", at "stroke-width" "1" ]
        , circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num r), at "fill" "#f7f3e8", at "stroke" "#3a2c14", at "stroke-width" "1" ]
        , path [ at "d" band, at "fill" "none", at "stroke" bad, at "stroke-width" "5" ]
        ]
          <> map tick marks
          <> map number labelled
          <>
            [ label
                [ at "x" (num cx), at "y" (num (cy + 42.0))
                , at "text-anchor" "middle", at "font-size" "8", at "fill" "#55534c"
                ]
                title
            , line
                [ at "x1" (num cx), at "y1" (num cy), at "x2" (num tip.x), at "y2" (num tip.y)
                , at "stroke" "#b3261e", at "stroke-width" "2.5", at "stroke-linecap" "round"
                ]
            , circle [ at "cx" (num cx), at "cy" (num cy), at "r" "5", at "fill" (if value >= redFrom then bad else "#1d1d1b") ]
            ]
      )

lamp :: forall w i. Number -> Number -> String -> Boolean -> String -> HH.HTML w i
lamp x y title lit colour =
  g []
    [ circle
        [ at "cx" (num x), at "cy" (num y), at "r" "5"
        , at "fill" (if lit then colour else "#4a4d52")
        , at "stroke" "#b08d57", at "stroke-width" "1.5"
        ]
    , label
        [ at "x" (num x), at "y" (num (y + 15.0)), at "text-anchor" "middle"
        , at "font-size" "7", at "letter-spacing" "0.8", at "fill" "#c9b99a"
        ]
        title
    ]

display :: forall w i. Number -> String -> String -> HH.HTML w i
display x title value =
  g []
    [ label [ at "x" (num (x + 42.0)), at "y" "196", at "text-anchor" "middle", at "font-size" "8", at "letter-spacing" "0.6", at "fill" "#3a3d42" ] title
    , rect [ at "x" (num x), at "y" "200", at "width" "84", at "height" "26", at "rx" "4", at "fill" "#0f1a14", at "stroke" "#5c4520", at "stroke-width" "1" ]
    , label
        [ at "x" (num (x + 42.0)), at "y" "219", at "text-anchor" "middle"
        , at "font-size" "16", at "font-family" "monospace", at "fill" "#7dffa8"
        ]
        value
    ]

stateText :: MachineState -> String
stateText = case _ of
  Sleeping -> "ASLEEP"
  Heating -> "HEATING"
  Ready -> "READY"
  Faulted -> "FAULT"

stateColour :: MachineState -> String
stateColour = case _ of
  Sleeping -> "#6b7076"
  Heating -> "#f2a33a"
  Ready -> "#4fd37c"
  Faulted -> "#ff5a4d"

faulted :: MachineState -> Boolean
faulted = case _ of
  Faulted -> true
  _ -> false

fill :: String -> Attr
fill = at "fill"

front :: forall w i. FrontInput -> HH.HTML w i
front v =
  let
    phase = v.now * 2.0 - floor (v.now * 2.0)
    paddleAngle = case v.paddle of
      0 -> -60.0
      1 -> 0.0
      _ -> 60.0
    knob = onDial 200.0 262.0 22.0 paddleAngle
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
          [ at "cx" (num (384.0 + (if n `mod` 2 == 0 then -3.0 else 3.0)))
          , at "cy" (num (378.0 + drop))
          , at "r" (num (3.0 + drop / 5.0))
          , fill "#ffffff"
          , at "stroke" "#b9bec4"
          , at "stroke-width" "0.6"
          , at "opacity" (num (0.75 - drop / 40.0))
          ]
  in
    svg [ at "viewBox" "0 0 400 444", at "class" "machine" ]
      ( [ defs
            [ texture "alu" (textures <> "bushedAlum.png") 180.0
            , texture "brass" (textures <> "brushedBrass.png") 140.0
            , texture "wood" (textures <> "woodPanel01.png") 160.0
            , texture "stone" (textures <> "brutalistTexture.png") 200.0
            , sheen "sheen"
            ]
        ]
          -- counter, cup rail and body
          <> faced "stone" "#8b8a86" [ at "x" "0", at "y" "426", at "width" "400", at "height" "18", at "rx" "3" ]
          <> faced "wood" "#7a4b2a" [ at "x" "40", at "y" "14", at "width" "320", at "height" "12", at "rx" "6" ]
          <> faced "alu" steel [ at "x" "30", at "y" "26", at "width" "340", at "height" "250", at "rx" "16" ]
          <>
            [ rect [ at "x" "30", at "y" "26", at "width" "340", at "height" "250", at "rx" "16", fill "url(#sheen)" ]
            , rect
                [ at "x" "30", at "y" "26", at "width" "340", at "height" "250", at "rx" "16", fill "none"
                , at "stroke" (if faulted v.state then bad else steelDark)
                , at "stroke-width" (if faulted v.state then "5" else "1.5")
                ]
            , rect [ at "x" "45", at "y" "40", at "width" "310", at "height" "148", at "rx" "12", fill panel, at "stroke" "#0c0a09", at "stroke-width" "2" ]
            , gauge 118.0 112.0 "BREW bar" 12 2 11.0 v.brewBar
            , gauge 282.0 112.0 "STEAM bar" 3 1 2.3 v.steamBar
            -- state and lamps
            , label
                [ at "x" "200", at "y" "66", at "text-anchor" "middle", at "font-size" "11"
                , at "font-weight" "700", at "letter-spacing" "1.5", fill (stateColour v.state)
                ]
                (stateText v.state)
            , lamp 200.0 84.0 "RESERVOIR" v.brewElementOn "#ff9a3d"
            , lamp 200.0 110.0 "STEAM" v.steamElementOn "#ff9a3d"
            , lamp 200.0 136.0 "GROUP" v.groupHeaterOn "#ff9a3d"
            , lamp 200.0 162.0 "PROBLEM" v.problem "#ff2d1f"
            -- temperature displays
            , display 48.0 "RESERVOIR C" (num v.reservoirC)
            , display 158.0 "GROUP C" (num v.groupC)
            , display 268.0 "WATER C" (num v.waterC)
            -- paddle: a wooden lever on a brass hub
            , line
                [ at "x1" "200", at "y1" "262", at "x2" (num knob.x), at "y2" (num knob.y)
                , at "stroke" "#7a4b2a", at "stroke-width" "8", at "stroke-linecap" "round"
                ]
            , circle [ at "cx" (num knob.x), at "cy" (num knob.y), at "r" "8", fill "#5b3620", at "stroke" "#2e1a0d", at "stroke-width" "1" ]
            , circle [ at "cx" "200", at "cy" "262", at "r" "7", fill "#b08d57" ]
            , circle [ at "cx" "200", at "cy" "262", at "r" "7", fill "url(#brass)", at "stroke" "#5c4520", at "stroke-width" "1" ]
            ]
          -- group and portafilter
          <> faced "alu" "#b9bec4" [ at "x" "150", at "y" "276", at "width" "100", at "height" "22", at "rx" "4" ]
          -- the portafilter: a long wooden handle, a wide ring and the
          -- underside of a bottomless basket
          <> faced "wood" "#3a2415" [ at "x" "52", at "y" "311", at "width" "96", at "height" "16", at "rx" "8" ]
          <>
            [ rect [ at "x" "142", at "y" "298", at "width" "116", at "height" "13", fill steelDark ]
            , rect [ at "x" "138", at "y" "311", at "width" "124", at "height" "17", at "rx" "4", fill "#3a3d42" ]
            , path [ at "d" "M148,328 L252,328 Q250,337 240,338 L160,338 Q150,337 148,328 Z", fill "#c9ced3", at "stroke" "#6f757b", at "stroke-width" "1" ]
            -- steam wand: out of the side of the body on a ball joint,
            -- then down beside the drip tray
            , path
                [ at "d" "M336,236 L368,236 Q384,236 384,252 L384,366", fill "none"
                , at "stroke" "#c9ced3", at "stroke-width" "7", at "stroke-linecap" "round"
                ]
            , circle [ at "cx" "338", at "cy" "236", at "r" "9", fill "#b08d57", at "stroke" "#5c4520", at "stroke-width" "1" ]
            , rect [ at "x" "379", at "y" "362", at "width" "10", at "height" "12", at "rx" "3", fill "#8b9197" ]
            ]
          -- drip tray, scale platform, cup
          <> faced "alu" steelDark [ at "x" "80", at "y" "400", at "width" "240", at "height" "26", at "rx" "4" ]
          <>
            [ rect [ at "x" "150", at "y" "394", at "width" "100", at "height" "6", fill "#c7ccd1" ]
            , path [ at "d" "M172,352 L228,352 L222,394 L178,394 Z", fill "#f4f1ea", at "stroke" "#8a8378", at "stroke-width" "1.5" ]
            , path [ at "d" coffee, fill "#5a3a22" ]
            , rect [ at "x" "170", at "y" "404", at "width" "60", at "height" "16", at "rx" "3", fill "#0f1a14" ]
            , label
                [ at "x" "200", at "y" "416", at "text-anchor" "middle", at "font-size" "11"
                , at "font-family" "monospace", fill "#7dffa8"
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
