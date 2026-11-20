-- | The machine seen from the front: two dial gauges, three temperature
-- | displays, heater lamps, the paddle, the group with its portafilter,
-- | the cup on the scale and the steam wand.
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
import Sim.View.Svg (Attr, at, circle, g, label, line, num, path, rect, svg)

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
  }

bad :: String
bad = "#d32f2f"

steel :: String
steel = "#d9dcdf"

steelDark :: String
steelDark = "#8b9197"

panel :: String
panel = "#26282c"

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
      ( [ circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num (r + 4.0)), at "fill" "#b08d57" ]
        , circle [ at "cx" (num cx), at "cy" (num cy), at "r" (num r), at "fill" "#f7f3e8" ]
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
        , at "stroke" "#111", at "stroke-width" "1"
        ]
    , label
        [ at "x" (num x), at "y" (num (y + 15.0)), at "text-anchor" "middle"
        , at "font-size" "7", at "fill" "#c9ccd1"
        ]
        title
    ]

display :: forall w i. Number -> String -> String -> HH.HTML w i
display x title value =
  g []
    [ label [ at "x" (num (x + 42.0)), at "y" "196", at "text-anchor" "middle", at "font-size" "8", at "fill" "#55534c" ] title
    , rect [ at "x" (num x), at "y" "200", at "width" "84", at "height" "26", at "rx" "4", at "fill" "#0f1a14" ]
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
          [ at "cx" (num (338.0 + (if n `mod` 2 == 0 then -3.0 else 3.0)))
          , at "cy" (num (386.0 + drop))
          , at "r" (num (3.0 + drop / 5.0))
          , fill "#ffffff"
          , at "opacity" (num (0.75 - drop / 40.0))
          ]
  in
    svg [ at "viewBox" "0 0 400 440", at "class" "machine" ]
      ( [ -- cup rail and body
          rect [ at "x" "40", at "y" "16", at "width" "320", at "height" "10", at "rx" "5", fill steelDark ]
        , rect
            [ at "x" "30", at "y" "26", at "width" "340", at "height" "250", at "rx" "14", fill steel
            , at "stroke" (if faulted v.state then bad else steelDark)
            , at "stroke-width" (if faulted v.state then "5" else "1")
            ]
        , rect [ at "x" "45", at "y" "40", at "width" "310", at "height" "148", at "rx" "10", fill panel ]
        , gauge 118.0 112.0 "BREW bar" 12 2 11.0 v.brewBar
        , gauge 282.0 112.0 "STEAM bar" 3 1 2.3 v.steamBar
        -- state and lamps
        , label
            [ at "x" "200", at "y" "66", at "text-anchor" "middle", at "font-size" "11"
            , at "font-weight" "700", fill (stateColour v.state)
            ]
            (stateText v.state)
        , lamp 200.0 84.0 "RESERVOIR" v.brewElementOn "#ff7a3d"
        , lamp 200.0 110.0 "STEAM" v.steamElementOn "#ff7a3d"
        , lamp 200.0 136.0 "GROUP" v.groupHeaterOn "#ff7a3d"
        , lamp 200.0 162.0 "PROBLEM" v.problem "#ff2d1f"
        -- temperature displays
        , display 48.0 "RESERVOIR C" (num v.reservoirC)
        , display 158.0 "GROUP C" (num v.groupC)
        , display 268.0 "WATER C" (num v.waterC)
        -- paddle
        , line
            [ at "x1" "200", at "y1" "262", at "x2" (num knob.x), at "y2" (num knob.y)
            , at "stroke" "#7a4b2a", at "stroke-width" "7", at "stroke-linecap" "round"
            ]
        , circle [ at "cx" (num knob.x), at "cy" (num knob.y), at "r" "7", fill "#5b3620" ]
        , circle [ at "cx" "200", at "cy" "262", at "r" "6", fill steelDark ]
        -- group and portafilter
        , rect [ at "x" "150", at "y" "276", at "width" "100", at "height" "22", at "rx" "4", fill "#b9bec4", at "stroke" steelDark ]
        , rect [ at "x" "160", at "y" "298", at "width" "80", at "height" "10", fill steelDark ]
        , rect [ at "x" "88", at "y" "311", at "width" "78", at "height" "10", at "rx" "5", fill "#1d1d1b" ]
        , rect [ at "x" "163", at "y" "308", at "width" "74", at "height" "16", at "rx" "3", fill "#3a3d42" ]
        , rect [ at "x" "194", at "y" "324", at "width" "12", at "height" "8", fill "#3a3d42" ]
        -- steam wand
        , path
            [ at "d" "M330,276 L338,300 L338,380", fill "none"
            , at "stroke" "#b9bec4", at "stroke-width" "5", at "stroke-linecap" "round"
            ]
        -- drip tray, scale platform, cup
        , rect [ at "x" "80", at "y" "400", at "width" "240", at "height" "18", at "rx" "3", fill steelDark ]
        , rect [ at "x" "150", at "y" "394", at "width" "100", at "height" "6", fill "#c7ccd1" ]
        , path [ at "d" coffee, fill "#5a3a22" ]
        , path [ at "d" "M172,352 L228,352 L222,394 L178,394 Z", fill "none", at "stroke" "#55534c", at "stroke-width" "2" ]
        , label
            [ at "x" "200", at "y" "413", at "text-anchor" "middle", at "font-size" "11"
            , at "font-family" "monospace", fill "#ffffff"
            ]
            (num v.cupGrams <> " g")
        , rect [ at "x" "60", at "y" "418", at "width" "24", at "height" "10", fill "#1d1d1b" ]
        , rect [ at "x" "316", at "y" "418", at "width" "24", at "height" "10", fill "#1d1d1b" ]
        ]
          <>
            ( if v.pouring then
                [ rect
                    [ at "x" (if phase < 0.5 then "198" else "198.6"), at "y" "332"
                    , at "width" "3.4", at "height" (num (top - 332.0)), fill "#5a3a22"
                    ]
                ]
              else []
            )
          <> (if v.steaming then map puff (range 0 3) else [])
      )