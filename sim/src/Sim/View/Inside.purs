-- | The machine with the panels off: every branch of docs/hydraulics.csv
-- | as a pipe that lights up when water moves through it, and every
-- | valve, pump and vessel showing its present state.
module Sim.View.Inside
  ( InsideInput
  , inside
  ) where

import Prelude

import Data.Array (elem)
import Data.Number (cos, sin)
import Halogen.HTML as HH
import Sim.Problems (Problems)
import Sim.Types (Actuators, FaultKind(..), GroupValve(..), MixValve(..), PumpMode(..), TwoWay(..))
import Sim.View.Svg (at, circle, g, label, line, num, path, rect, svg)

type InsideInput =
  { now :: Number
  , act :: Actuators
  , flowMlPerS :: Number
  , reservoirC :: Number
  , mixC :: Number
  , steamBar :: Number
  , steamWaterMl :: Number
  , steamValveOpen :: Boolean
  , problems :: Problems
  }

bad :: String
bad = "#d32f2f"

idle :: String
idle = "#c5c8cc"

cold :: String
cold = "#2f7fd1"

hot :: String
hot = "#d1452f"

mixed :: String
mixed = "#c97a2b"

steamColour :: String
steamColour = "#7d8792"

pipe :: forall w i. String -> Boolean -> String -> HH.HTML w i
pipe d flowing colour =
  path
    [ at "d" d, at "fill" "none"
    , at "stroke" (if flowing then colour else idle)
    , at "stroke-width" (if flowing then "5" else "3")
    , at "stroke-linejoin" "round"
    ]

-- | A pipe that cannot carry water: red and dashed.
deadPipe :: forall w i. String -> HH.HTML w i
deadPipe d =
  path
    [ at "d" d, at "fill" "none", at "stroke" bad, at "stroke-width" "4"
    , at "stroke-dasharray" "6 5", at "stroke-linejoin" "round"
    ]

tag :: forall w i. Number -> Number -> String -> HH.HTML w i
tag x y content =
  label [ at "x" (num x), at "y" (num y), at "text-anchor" "middle", at "font-size" "8", at "fill" "#3a3d42" ] content

alarm :: forall w i. Number -> Number -> String -> HH.HTML w i
alarm x y content =
  label
    [ at "x" (num x), at "y" (num y), at "text-anchor" "middle", at "font-size" "9"
    , at "font-weight" "700", at "fill" bad
    ]
    content

-- | A valve: a body with a round window, and in the window a gate that
-- | lies along the pipe when the valve is open and swings across it when
-- | the valve is closed. The swing is animated by the stylesheet.
valve :: forall w i. Number -> Number -> String -> Boolean -> Boolean -> HH.HTML w i
valve x y name open broken =
  g []
    [ rect
        [ at "x" (num x), at "y" (num y), at "width" "40", at "height" "22", at "rx" "4"
        , at "fill" (if broken then bad else if open then "#264a3a" else "#6b7076")
        ]
    , circle
        [ at "cx" (num (x + 11.0)), at "cy" (num (y + 11.0)), at "r" "8.5"
        , at "fill" "#f4eedf", at "stroke" "#17120f", at "stroke-width" "1.5"
        ]
    , g [ at "class" (if open then "gate open" else "gate"), at "transform" ("translate(" <> num (x + 11.0) <> "," <> num (y + 11.0) <> ")") ]
        [ line
            [ at "class" "gate-blade", at "x1" "-7", at "y1" "0", at "x2" "7", at "y2" "0"
            , at "stroke" (if broken then bad else if open then "#4d8a60" else "#bc6a14"), at "stroke-width" "4", at "stroke-linecap" "round"
            ]
        ]
    , circle [ at "cx" (num (x + 11.0)), at "cy" (num (y + 11.0)), at "r" "1.8", at "fill" "#17120f" ]
    , label
        [ at "x" (num (x + 29.5)), at "y" (num (y + 13.5)), at "text-anchor" "middle"
        , at "font-size" "5.5", at "fill" "#ffffff"
        ]
        name
    , tag (x + 20.0) (y + 32.0) (if open then "open" else "closed")
    ]

element :: forall w i. Number -> Number -> Boolean -> HH.HTML w i
element x y on =
  path
    [ at "d" ("M" <> num x <> "," <> num y <> " l6,-6 l6,6 l6,-6 l6,6 l6,-6 l6,6")
    , at "class" (if on then "element on" else "element")
    , at "fill" "none", at "stroke" (if on then "#ff5a1f" else "#8b9197")
    , at "stroke-width" (if on then "3" else "2")
    ]

inside :: forall w i. InsideInput -> HH.HTML w i
inside v =
  let
    a = v.act
    p = v.problems
    has kind = elem kind p.faults
    supplyBad = p.supplyCut || has NoInletPressure
    hotBad = has OverTemperature
    sensorBad = has SensorOpen || has SensorShort || has ScaleLost || has Watchdog
    only condition item = if condition then [ item ] else []
    brewing = v.flowMlPerS > 0.02
    pumping = a.pump /= Stopped && brewing
    bypassing = a.bypassValve == Open && brewing
    mixing = a.mixValve == Regulating && a.mixPosition > 0.005 && brewing
    filling = a.fillValve == Open
    spin = v.now * 6.0 * (if pumping then a.pumpSpeed + 0.2 else 0.0)
    spoke = { x: 160.0 + 12.0 * sin spin, y: 150.0 - 12.0 * cos spin }
    level = clamp 0.0 1.0 (v.steamWaterMl / 2000.0)
    waterTop = 292.0 - 40.0 * level
  in
    svg [ at "viewBox" "0 0 520 320", at "class" "inside" ]
      ( [ -- supply and cold manifold
        if supplyBad then deadPipe "M10,150 L90,150"
        else pipe "M10,150 L90,150" (brewing || filling) cold
      , pipe "M90,60 L90,274" false cold
      , if supplyBad then alarm 44.0 140.0 "NO WATER" else tag 40.0 140.0 "WATER IN, 3 bar"
      , tag 90.0 52.0 "MAN_COLD"
      -- bypass branch
      , pipe "M90,72 L250,72 L250,150" bypassing cold
      , valve 120.0 61.0 "SV_BYP" (a.bypassValve == Open) false
      , rect [ at "x" "190", at "y" "63", at "width" "34", at "height" "18", at "rx" "9", at "fill" "#b08d57" ]
      , tag 207.0 75.0 "NV_PRE"
      -- pump branch
      , pipe "M90,150 L250,150" pumping cold
      , circle [ at "cx" "160", at "cy" "150", at "r" "16", at "fill" (if pumping then "#2c8a57" else "#6b7076") ]
      , line
          [ at "x1" "160", at "y1" "150", at "x2" (num spoke.x), at "y2" (num spoke.y)
          , at "stroke" "#ffffff", at "stroke-width" "3", at "stroke-linecap" "round"
          ]
      , tag 160.0 180.0 ("PUMP " <> num (a.pumpSpeed * 100.0) <> "%")
      , tag 250.0 142.0 "MAN_BREW"
      -- hot branch through the reservoir
      , pipe "M250,150 L290,150" brewing cold
      , rect
          [ at "x" "290", at "y" "112", at "width" "76", at "height" "76", at "rx" "6", at "fill" "#f3d9d2"
          , at "stroke" (if hotBad then bad else hot), at "stroke-width" (if hotBad then "5" else "1")
          ]
      , element 310.0 178.0 a.brewElementOn
      , tag 328.0 132.0 "RESERVOIR"
      , label [ at "x" "328", at "y" "152", at "text-anchor" "middle", at "font-size" "13", at "fill" "#7a1f12" ] (num v.reservoirC <> " C")
      , pipe "M366,150 L420,150" brewing hot
      -- cold branch through the mix valve
      , if p.coldBlocked then deadPipe "M250,150 L250,226 L420,226 L420,150"
        else pipe "M250,150 L250,226 L420,226 L420,150" mixing cold
      , rect [ at "x" "305", at "y" "215", at "width" "60", at "height" "22", at "rx" "4", at "fill" "#6b7076" ]
      , rect
          [ at "x" "305", at "y" "215", at "width" (num (60.0 * clamp 0.0 1.0 a.mixPosition))
          , at "height" "22", at "rx" "4", at "fill" cold
          ]
      , label [ at "x" "335", at "y" "229", at "text-anchor" "middle", at "font-size" "8", at "fill" "#ffffff" ] "MV_MIX"
      , tag 335.0 247.0 ("cold valve " <> num (a.mixPosition * 100.0) <> "%")
      -- mix point, group valve, group
      , pipe "M420,150 L490,150 L490,196" brewing mixed
      , circle
          [ at "cx" "420", at "cy" "150", at "r" (if has MixOverTemperature then "10" else "7")
          , at "fill" (if has MixOverTemperature then bad else if brewing then mixed else idle)
          ]
      , if has MixOverTemperature then alarm 420.0 130.0 ("MIX " <> num v.mixC <> " C, TOO HOT")
        else tag 420.0 132.0 ("MIX " <> num v.mixC <> " C")
      , valve 438.0 139.0 "SV_GRP" (a.groupValve == ToGroup) false
      , path [ at "d" "M468,196 L512,196 L506,214 L474,214 Z", at "fill" "#3a3d42" ]
      , tag 490.0 226.0 "GROUP"
      -- steam side
      , pipe "M90,274 L180,274" filling cold
      , valve 115.0 263.0 "SV_FILL" filling (has FillTimeout)
      , rect
          [ at "x" "180", at "y" "250", at "width" "110", at "height" "44", at "rx" "8", at "fill" "#eef0f2"
          , at "stroke" (if hotBad || p.levelLow then bad else steamColour)
          , at "stroke-width" (if hotBad || p.levelLow then "5" else "1")
          ]
      , rect
          [ at "x" "182", at "y" (num waterTop), at "width" "106"
          , at "height" (num (292.0 - waterTop)), at "fill" "#a9cdf0"
          ]
      , element 200.0 290.0 a.steamElementOn
      , label [ at "x" "235", at "y" "268", at "text-anchor" "middle", at "font-size" "11", at "fill" "#26282c" ] ("STEAM " <> num v.steamBar <> " bar")
      , pipe "M290,262 L420,262" v.steamValveOpen steamColour
      , valve 330.0 251.0 "STEAM" v.steamValveOpen false
      , tag 440.0 265.0 "WAND"
      ]
          <> only p.coldBlocked (alarm 335.0 207.0 "BLOCKED")
          <> only p.hardWater (alarm 44.0 166.0 "HARD WATER")
          <> only (has OverPressure) (alarm 250.0 128.0 "OVER PRESSURE")
          <> only hotBad (alarm 328.0 104.0 "OVER LIMIT")
          <> only p.levelLow (alarm 235.0 244.0 "LEVEL LOW")
          <> only (has FillTimeout) (alarm 135.0 256.0 "FILL TIMED OUT")
          <> only sensorBad (alarm 440.0 40.0 "SENSOR FAULT")
      )
