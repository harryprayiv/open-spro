-- | The machine with the panels off: every branch of docs/hydraulics.csv
-- | as a pipe that lights up when water moves through it, and every
-- | valve, pump and vessel showing its present state.
-- |
-- | It is a diagram of how the parts are joined, and not of where they
-- | sit in the machine, so it is free to arrange them to suit the room
-- | it is given. It is drawn three ways: wide, with the brew path in a
-- | row; tall, with it in a column; and as a strip, for a window much
-- | wider than it is high. The stylesheet shows whichever suits the
-- | shape of its window. In all three the steam side is set apart from
-- | the brew path: along the bottom, down the right or off at the end.
module Sim.View.Inside
  ( InsideInput
  , inside
  ) where

import Prelude

import Data.Array (elem, uncons)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Number (cos, sin)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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

type Point = { x :: Number, y :: Number }

at' :: Number -> Number -> Point
at' x y = { x, y }

-- | A pipe's path through its corners.
through :: Array Point -> String
through points = case uncons points of
  Nothing -> ""
  Just { head, tail } ->
    "M" <> num head.x <> "," <> num head.y <> joinWith "" (map (\p -> " L" <> num p.x <> "," <> num p.y) tail)

-- | One arrangement of the diagram: how big the drawing is, where the
-- | middle of each part is, and the corners each pipe turns at.
type Layout =
  { name :: String
  , width :: Number
  , height :: Number
  -- parts, by their middles
  , cold :: Point
  , bypassValve :: Point
  , needle :: Point
  , pump :: Point
  , brew :: Point
  , reservoir :: Point
  , mixValve :: Point
  , mix :: Point
  , groupValve :: Point
  , group :: Point
  , fillValve :: Point
  , boiler :: Point
  , steamValve :: Point
  , wand :: Point
  -- pipes, by their corners
  , supply :: Array Point
  , bypass :: Array Point
  , pumped :: Array Point
  , intoReservoir :: Array Point
  , outOfReservoir :: Array Point
  , coldBranch :: Array Point
  , toGroup :: Array Point
  , fill :: Array Point
  , steamOut :: Array Point
  -- where the words about the water supply and about the sensors go
  , supplyNote :: Point
  , sensorNote :: Point
  }

-- | The brew path in a row across the top two thirds, the steam side
-- | in a row of its own along the bottom.
wide :: Layout
wide =
  { name: "wide"
  , width: 560.0
  , height: 310.0
  , cold: at' 80.0 120.0
  , bypassValve: at' 125.0 44.0
  , needle: at' 187.0 44.0
  , pump: at' 152.0 120.0
  , brew: at' 232.0 120.0
  , reservoir: at' 312.0 120.0
  , mixValve: at' 322.0 200.0
  , mix: at' 412.0 120.0
  , groupValve: at' 458.0 120.0
  , group: at' 524.0 172.0
  , fillValve: at' 135.0 268.0
  , boiler: at' 245.0 268.0
  , steamValve: at' 360.0 268.0
  , wand: at' 450.0 268.0
  , supply: [ at' 8.0 120.0, at' 80.0 120.0 ]
  , bypass: [ at' 80.0 120.0, at' 80.0 44.0, at' 232.0 44.0, at' 232.0 120.0 ]
  , pumped: [ at' 80.0 120.0, at' 232.0 120.0 ]
  , intoReservoir: [ at' 232.0 120.0, at' 274.0 120.0 ]
  , outOfReservoir: [ at' 350.0 120.0, at' 412.0 120.0 ]
  , coldBranch: [ at' 232.0 120.0, at' 232.0 200.0, at' 412.0 200.0, at' 412.0 120.0 ]
  , toGroup: [ at' 412.0 120.0, at' 524.0 120.0, at' 524.0 163.0 ]
  , fill: [ at' 80.0 120.0, at' 80.0 268.0, at' 190.0 268.0 ]
  , steamOut: [ at' 300.0 268.0, at' 430.0 268.0 ]
  , supplyNote: at' 40.0 108.0
  , sensorNote: at' 480.0 30.0
  }

-- | The brew path in a column, the bypass to the left of it, the cold
-- | branch to the right, and the steam side in a column of its own
-- | down the right edge.
tall :: Layout
tall =
  { name: "tall"
  , width: 330.0
  , height: 600.0
  , cold: at' 130.0 62.0
  , bypassValve: at' 44.0 106.0
  , needle: at' 44.0 166.0
  , pump: at' 130.0 134.0
  , brew: at' 130.0 212.0
  , reservoir: at' 130.0 300.0
  , mixValve: at' 202.0 318.0
  , mix: at' 130.0 412.0
  , groupValve: at' 130.0 462.0
  , group: at' 130.0 536.0
  , fillValve: at' 262.0 112.0
  , boiler: at' 262.0 206.0
  , steamValve: at' 262.0 300.0
  , wand: at' 262.0 372.0
  , supply: [ at' 130.0 8.0, at' 130.0 62.0 ]
  , bypass: [ at' 130.0 62.0, at' 44.0 62.0, at' 44.0 212.0, at' 130.0 212.0 ]
  , pumped: [ at' 130.0 62.0, at' 130.0 212.0 ]
  , intoReservoir: [ at' 130.0 212.0, at' 130.0 262.0 ]
  , outOfReservoir: [ at' 130.0 338.0, at' 130.0 412.0 ]
  , coldBranch: [ at' 130.0 212.0, at' 202.0 212.0, at' 202.0 412.0, at' 130.0 412.0 ]
  , toGroup: [ at' 130.0 412.0, at' 130.0 527.0 ]
  , fill: [ at' 130.0 62.0, at' 262.0 62.0, at' 262.0 184.0 ]
  , steamOut: [ at' 262.0 228.0, at' 262.0 356.0 ]
  , supplyNote: at' 196.0 26.0
  , sensorNote: at' 250.0 560.0
  }

-- | Everything on one line, for a window much wider than it is high:
-- | the brew path on the left and the steam side beyond it on the
-- | right, fed by a pipe run along the bottom.
strip :: Layout
strip =
  { name: "strip"
  , width: 880.0
  , height: 200.0
  , cold: at' 64.0 100.0
  , bypassValve: at' 104.0 32.0
  , needle: at' 158.0 32.0
  , pump: at' 124.0 100.0
  , brew: at' 190.0 100.0
  , reservoir: at' 270.0 100.0
  , mixValve: at' 276.0 166.0
  , mix: at' 362.0 100.0
  , groupValve: at' 408.0 100.0
  , group: at' 478.0 132.0
  , fillValve: at' 590.0 100.0
  , boiler: at' 696.0 100.0
  , steamValve: at' 800.0 100.0
  , wand: at' 858.0 86.0
  , supply: [ at' 6.0 100.0, at' 64.0 100.0 ]
  , bypass: [ at' 64.0 100.0, at' 64.0 32.0, at' 190.0 32.0, at' 190.0 100.0 ]
  , pumped: [ at' 64.0 100.0, at' 190.0 100.0 ]
  , intoReservoir: [ at' 190.0 100.0, at' 232.0 100.0 ]
  , outOfReservoir: [ at' 308.0 100.0, at' 362.0 100.0 ]
  , coldBranch: [ at' 190.0 100.0, at' 190.0 166.0, at' 362.0 166.0, at' 362.0 100.0 ]
  , toGroup: [ at' 362.0 100.0, at' 478.0 100.0, at' 478.0 123.0 ]
  , fill: [ at' 64.0 100.0, at' 64.0 190.0, at' 540.0 190.0, at' 540.0 100.0, at' 641.0 100.0 ]
  , steamOut: [ at' 751.0 100.0, at' 850.0 100.0 ]
  , supplyNote: at' 34.0 150.0
  , sensorNote: at' 540.0 24.0
  }

-- | The diagram, in each of its arrangements. The stylesheet shows one.
inside :: forall w i. InsideInput -> HH.HTML w i
inside v = HH.div [ HP.class_ (HH.ClassName "inside-cell") ] (map (drawn v) [ wide, tall, strip ])

drawn :: forall w i. InsideInput -> Layout -> HH.HTML w i
drawn v l =
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
    spoke = { x: l.pump.x + 12.0 * sin spin, y: l.pump.y - 12.0 * cos spin }
    level = clamp 0.0 1.0 (v.steamWaterMl / 2000.0)
    boilerFloor = l.boiler.y + 20.0
    waterTop = boilerFloor - 40.0 * level
    valveAt point = valve (point.x - 20.0) (point.y - 11.0)
    dot point colour = circle [ at "cx" (num point.x), at "cy" (num point.y), at "r" "4", at "fill" colour ]
  in
    svg [ at "viewBox" ("0 0 " <> num l.width <> " " <> num l.height), at "class" ("inside " <> l.name) ]
      ( [ -- the supply
          if supplyBad then deadPipe (through l.supply) else pipe (through l.supply) (brewing || filling) cold
        , if supplyBad then alarm l.supplyNote.x l.supplyNote.y "NO WATER" else tag l.supplyNote.x l.supplyNote.y "WATER IN, 3 bar"
        -- the steam side, set apart from the rest
        , pipe (through l.fill) filling cold
        , valveAt l.fillValve "SV_FILL" filling (has FillTimeout)
        , rect
            [ at "x" (num (l.boiler.x - 55.0)), at "y" (num (l.boiler.y - 22.0)), at "width" "110", at "height" "44", at "rx" "8", at "fill" "#eef0f2"
            , at "stroke" (if hotBad || p.levelLow then bad else steamColour)
            , at "stroke-width" (if hotBad || p.levelLow then "5" else "1")
            ]
        , rect
            [ at "x" (num (l.boiler.x - 53.0)), at "y" (num waterTop), at "width" "106"
            , at "height" (num (boilerFloor - waterTop)), at "fill" "#a9cdf0"
            ]
        , element (l.boiler.x - 35.0) (l.boiler.y + 18.0) a.steamElementOn
        , label
            [ at "x" (num l.boiler.x), at "y" (num (l.boiler.y - 4.0)), at "text-anchor" "middle", at "font-size" "11", at "fill" "#26282c" ]
            ("STEAM " <> num v.steamBar <> " bar")
        , pipe (through l.steamOut) v.steamValveOpen steamColour
        , valveAt l.steamValve "STEAM" v.steamValveOpen false
        , tag l.wand.x (l.wand.y + 3.0) "WAND"
        -- the bypass branch
        , pipe (through l.bypass) bypassing cold
        , valveAt l.bypassValve "SV_BYP" (a.bypassValve == Open) false
        , rect [ at "x" (num (l.needle.x - 17.0)), at "y" (num (l.needle.y - 9.0)), at "width" "34", at "height" "18", at "rx" "9", at "fill" "#b08d57" ]
        , tag l.needle.x (l.needle.y + 3.0) "NV_PRE"
        -- the pump branch
        , pipe (through l.pumped) pumping cold
        , circle [ at "cx" (num l.pump.x), at "cy" (num l.pump.y), at "r" "16", at "fill" (if pumping then "#2c8a57" else "#6b7076") ]
        , line
            [ at "x1" (num l.pump.x), at "y1" (num l.pump.y), at "x2" (num spoke.x), at "y2" (num spoke.y)
            , at "stroke" "#ffffff", at "stroke-width" "3", at "stroke-linecap" "round"
            ]
        , tag (l.pump.x + (if l.name == "tall" then -44.0 else 0.0)) (l.pump.y + (if l.name == "tall" then 3.0 else 29.0)) ("PUMP " <> num (a.pumpSpeed * 100.0) <> "%")
        -- the hot branch, through the reservoir
        , pipe (through l.intoReservoir) brewing cold
        , rect
            [ at "x" (num (l.reservoir.x - 38.0)), at "y" (num (l.reservoir.y - 38.0)), at "width" "76", at "height" "76", at "rx" "6", at "fill" "#f3d9d2"
            , at "stroke" (if hotBad then bad else hot), at "stroke-width" (if hotBad then "5" else "1")
            ]
        , element (l.reservoir.x - 18.0) (l.reservoir.y + 28.0) a.brewElementOn
        , tag l.reservoir.x (l.reservoir.y - 18.0) "RESERVOIR"
        , label
            [ at "x" (num l.reservoir.x), at "y" (num (l.reservoir.y + 2.0)), at "text-anchor" "middle", at "font-size" "13", at "fill" "#7a1f12" ]
            (num v.reservoirC <> " C")
        , pipe (through l.outOfReservoir) brewing hot
        -- the cold branch, through the mix valve
        , if p.coldBlocked then deadPipe (through l.coldBranch) else pipe (through l.coldBranch) mixing cold
        , rect [ at "x" (num (l.mixValve.x - 30.0)), at "y" (num (l.mixValve.y - 11.0)), at "width" "60", at "height" "22", at "rx" "4", at "fill" "#6b7076" ]
        , rect
            [ at "x" (num (l.mixValve.x - 30.0)), at "y" (num (l.mixValve.y - 11.0)), at "width" (num (60.0 * clamp 0.0 1.0 a.mixPosition))
            , at "height" "22", at "rx" "4", at "fill" cold
            ]
        , label [ at "x" (num l.mixValve.x), at "y" (num (l.mixValve.y + 3.0)), at "text-anchor" "middle", at "font-size" "8", at "fill" "#ffffff" ] "MV_MIX"
        , tag l.mixValve.x (l.mixValve.y + 21.0) ("cold " <> num (a.mixPosition * 100.0) <> "%")
        -- the mix point, the group valve and the group
        , pipe (through l.toGroup) brewing mixed
        , dot l.cold idle
        , dot l.brew idle
        , circle
            [ at "cx" (num l.mix.x), at "cy" (num l.mix.y), at "r" (if has MixOverTemperature then "10" else "7")
            , at "fill" (if has MixOverTemperature then bad else if brewing then mixed else idle)
            ]
        , if has MixOverTemperature then alarm (l.mix.x + mixAside) (l.mix.y - mixAbove) ("MIX " <> num v.mixC <> " C, TOO HOT")
          else tag (l.mix.x + mixAside) (l.mix.y - mixAbove) ("MIX " <> num v.mixC <> " C")
        , valveAt l.groupValve "SV_GRP" (a.groupValve == ToGroup) false
        , path
            [ at "d"
                ( "M" <> num (l.group.x - 22.0) <> "," <> num (l.group.y - 9.0) <> " L" <> num (l.group.x + 22.0) <> "," <> num (l.group.y - 9.0)
                    <> " L" <> num (l.group.x + 16.0) <> "," <> num (l.group.y + 9.0) <> " L" <> num (l.group.x - 16.0) <> "," <> num (l.group.y + 9.0) <> " Z"
                )
            , at "fill" "#3a3d42"
            ]
        , tag l.group.x (l.group.y + 21.0) "GROUP"
        ]
          <> only p.coldBlocked (alarm l.mixValve.x (l.mixValve.y - 17.0) "BLOCKED")
          <> only p.hardWater (alarm l.supplyNote.x (l.supplyNote.y + 12.0) "HARD WATER")
          <> only (has OverPressure) (alarm (l.brew.x + 30.0) (l.brew.y - 10.0) "OVER PRESSURE")
          <> only hotBad (alarm l.reservoir.x (l.reservoir.y - 44.0) "OVER LIMIT")
          <> only p.levelLow (alarm l.boiler.x (l.boiler.y - 28.0) "LEVEL LOW")
          <> only (has FillTimeout) (alarm l.fillValve.x (l.fillValve.y - 17.0) "FILL TIMED OUT")
          <> only sensorBad (alarm l.sensorNote.x l.sensorNote.y "SENSOR FAULT")
      )
  where
  -- In the column the mix point's words go to the left of it, clear of
  -- the pipes that meet there.
  mixAside = if l.name == "tall" then -46.0 else 0.0
  mixAbove = if l.name == "tall" then -3.0 else 18.0
