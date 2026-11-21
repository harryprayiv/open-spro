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
-- |
-- | A part that sits on a pipe is drawn along that pipe: a valve on an
-- | upright pipe stands upright, with its gate across the pipe when it
-- | is shut and along it when it is open.
-- |
-- | A pipe says what is in it. Cold water is blue, hot water is red,
-- | blended water is a colour between the two that follows its
-- | temperature, steam is a grey dashed line and a drain is a thin
-- | dotted one. A pipe at rest is drawn faint in its colour, and one
-- | with something moving through it is drawn full, with marks
-- | running along it the way the flow goes.
-- |
-- | The three mechanical safeguards are drawn where they are fitted:
-- | the expansion valve EXP on the brew side, and the safety valve
-- | SRV_STM and the vacuum breaker VB on the steam boiler. They act
-- | with no controller, and each is shown lifting when the pressure
-- | under it reaches its setting.
module Sim.View.Inside
  ( InsideInput
  , inside
  ) where

import Prelude

import Data.Array (elem, length, mapWithIndex, uncons)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin)
import Data.String (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Sim.Problems (Problems)
import Sim.Tint (blend)
import Sim.Types (Actuators, FaultKind(..), GroupValve(..), MixValve(..), PumpMode(..), TwoWay(..))
import Sim.View.Svg (at, circle, g, label, line, num, path, rect, svg)

type InsideInput =
  { now :: Number
  , act :: Actuators
  , flowMlPerS :: Number
  , reservoirC :: Number
  , mixC :: Number
  , brewBar :: Number
  , steamBar :: Number
  , steamWaterMl :: Number
  , steamValveOpen :: Boolean
  -- the pressures the two mechanical valves lift at
  , expansionBar :: Number
  , steamSafetyBar :: Number
  , problems :: Problems
  }

bad :: String
bad = "#d32f2f"

idle :: String
idle = "#c5c8cc"

cold :: String
cold = "#2f7fd1"

warm :: String
warm = "#d99a2b"

hot :: String
hot = "#d1452f"

steamColour :: String
steamColour = "#7d8792"

lifting :: String
lifting = "#e07a12"

------------------------------------------------------------
-- Pipes
------------------------------------------------------------

-- | What a pipe carries. Blended water has a warmth from 0, as cold as
-- | the supply, to 1, as hot as the reservoir.
data Carries = ColdWater | HotWater | Blended Number | Steam | Drain

-- | The colour of water this warm: blue through amber to red.
waterColour :: Number -> String
waterColour warmth =
  let
    w = clamp 0.0 1.0 warmth
  in
    if w < 0.5 then blend cold warm (w * 2.0) else blend warm hot (w * 2.0 - 1.0)

-- | A pipe along a path, at rest or with something moving through it.
pipe :: forall w i. Carries -> Boolean -> String -> HH.HTML w i
pipe carries flowing d =
  let
    colour = case carries of
      ColdWater -> cold
      HotWater -> hot
      Blended warmth -> waterColour warmth
      Steam -> steamColour
      Drain -> steamColour
    dashes = case carries of
      Steam -> [ at "stroke-dasharray" "7 4" ]
      Drain -> [ at "stroke-dasharray" "1.5 3.5" ]
      _ -> []
    thin = case carries of
      Drain -> true
      _ -> false
    body =
      path
        ( [ at "d" d, at "fill" "none", at "stroke" colour
          , at "stroke-width" (if thin then (if flowing then "3" else "1.6") else if flowing then "5" else "3")
          , at "stroke-linejoin" "round"
          , at "opacity" (if flowing then "1" else "0.42")
          ]
            <> dashes
        )
    -- marks that run along the pipe the way the flow goes; the
    -- stylesheet moves them
    marks = path [ at "class" "flow", at "d" d, at "fill" "none", at "stroke" "#ffffff", at "stroke-width" (if thin then "1.2" else "1.8"), at "stroke-linecap" "round" ]
  in
    g [] ([ body ] <> (if flowing then [ marks ] else []))

-- | A pipe that cannot carry water: red and dashed.
deadPipe :: forall w i. String -> HH.HTML w i
deadPipe d =
  path
    [ at "d" d, at "fill" "none", at "stroke" bad, at "stroke-width" "4"
    , at "stroke-dasharray" "6 5", at "stroke-linejoin" "round"
    ]

------------------------------------------------------------
-- Words
------------------------------------------------------------

tag :: forall w i. Number -> Number -> String -> HH.HTML w i
tag x y content =
  label [ at "x" (num x), at "y" (num y), at "text-anchor" "middle", at "font-size" "8", at "fill" "#3a3d42" ] content

-- | Lines of words one under another, beginning, centred or ending at
-- | a point.
wordsAt :: forall w i. String -> Number -> Number -> Array String -> HH.HTML w i
wordsAt anchor x y lines =
  g []
    ( mapWithIndex
        (\i text -> label [ at "x" (num x), at "y" (num (y + 9.0 * toNumber i)), at "text-anchor" anchor, at "font-size" "8", at "fill" "#3a3d42" ] text)
        lines
    )

alarm :: forall w i. Number -> Number -> String -> HH.HTML w i
alarm x y content =
  label
    [ at "x" (num x), at "y" (num y), at "text-anchor" "middle", at "font-size" "9"
    , at "font-weight" "700", at "fill" bad
    ]
    content

------------------------------------------------------------
-- Parts
------------------------------------------------------------

type Point = { x :: Number, y :: Number }

at' :: Number -> Number -> Point
at' x y = { x, y }

-- | A part on a pipe: where its middle is, whether the pipe it sits on
-- | is upright, and which side its words go on. For a part on a level
-- | pipe 1 is below and -1 above. For one on an upright pipe 1 is to
-- | the right and -1 to the left.
type Part = { x :: Number, y :: Number, upright :: Boolean, side :: Number }

-- | A part on a level pipe, with its words below it.
level :: Number -> Number -> Part
level x y = { x, y, upright: false, side: 1.0 }

-- | A part on an upright pipe, with its words on the given side.
standing :: Number -> Number -> Number -> Part
standing x y side = { x, y, upright: true, side }

-- | The drawing of a part, laid along its pipe.
along :: forall w i. Part -> Array (HH.HTML w i) -> HH.HTML w i
along part = g [ at "transform" ("translate(" <> num part.x <> "," <> num part.y <> ")" <> (if part.upright then " rotate(90)" else "")) ]

-- | The words that go with a part: beside it if it stands on an
-- | upright pipe, and under or over it otherwise. `clear` is how far
-- | the part reaches from the middle of its pipe.
beside :: forall w i. Part -> Number -> Array String -> HH.HTML w i
beside part clear lines =
  if part.upright then
    wordsAt (if part.side > 0.0 then "start" else "end") (part.x + part.side * (clear + 5.0)) (part.y + 3.0 - 4.5 * toNumber (length lines - 1)) lines
  else if part.side > 0.0 then wordsAt "middle" part.x (part.y + clear + 10.0) lines
  else wordsAt "middle" part.x (part.y - clear - 4.0 - 9.0 * toNumber (length lines - 1)) lines

-- | A valve: a body with a round window, and in the window a gate that
-- | lies along the pipe when the valve is open and swings across it when
-- | the valve is shut. The swing is animated by the stylesheet. On a
-- | level pipe its name is written on the body. On an upright pipe the
-- | name goes beside it, where it can be read.
valve :: forall w i. Part -> String -> Boolean -> Boolean -> HH.HTML w i
valve part name open broken =
  g []
    [ along part
        ( [ rect
              [ at "x" "-20", at "y" "-11", at "width" "40", at "height" "22", at "rx" "4"
              , at "fill" (if broken then bad else if open then "#264a3a" else "#6b7076")
              ]
          , circle [ at "cx" "-9", at "cy" "0", at "r" "8.5", at "fill" "#f4eedf", at "stroke" "#17120f", at "stroke-width" "1.5" ]
          , g [ at "class" (if open then "gate open" else "gate"), at "transform" "translate(-9,0)" ]
              [ line
                  [ at "class" "gate-blade", at "x1" "-7", at "y1" "0", at "x2" "7", at "y2" "0"
                  , at "stroke" (if broken then bad else if open then "#4d8a60" else "#bc6a14"), at "stroke-width" "4", at "stroke-linecap" "round"
                  ]
              ]
          , circle [ at "cx" "-9", at "cy" "0", at "r" "1.8", at "fill" "#17120f" ]
          ]
            <>
              if part.upright then []
              else [ label [ at "x" "9.5", at "y" "2.5", at "text-anchor" "middle", at "font-size" "5.5", at "fill" "#ffffff" ] name ]
        )
    , beside part 11.0 (if part.upright then [ name, state ] else [ state ])
    ]
  where
  state = if open then "open" else "shut"

element :: forall w i. Number -> Number -> Boolean -> HH.HTML w i
element x y on =
  path
    [ at "d" ("M" <> num x <> "," <> num y <> " l6,-6 l6,6 l6,-6 l6,6 l6,-6 l6,6")
    , at "class" (if on then "element on" else "element")
    , at "fill" "none", at "stroke" (if on then "#ff5a1f" else "#8b9197")
    , at "stroke-width" (if on then "3" else "2")
    ]

-- | A spring-loaded valve that opens by itself: where it is tapped off,
-- | where it is drawn, where its outlet runs to, and where its words
-- | begin, are centred or end.
type Relief = { from :: Point, at :: Point, drainTo :: Point, words :: Point, anchor :: String }

-- | A relief valve: a round body with its spring in it, on a short tap
-- | from the pipe or vessel it guards, and a dotted outlet. While it is
-- | lifted the body is lit and the outlet runs.
relief :: forall w i. Relief -> Carries -> Boolean -> Array String -> HH.HTML w i
relief r carries lifted lines =
  g []
    [ path [ at "d" (through [ r.from, r.at ]), at "fill" "none", at "stroke" "#3a3d42", at "stroke-width" "2" ]
    , if r.drainTo == r.at then g [] [] else pipe Drain lifted (through [ r.at, r.drainTo ])
    , circle
        [ at "class" (if lifted then "element on" else "element")
        , at "cx" (num r.at.x), at "cy" (num r.at.y), at "r" "7.5"
        , at "fill" (if lifted then lifting else "#f4eedf"), at "stroke" "#17120f", at "stroke-width" "1.5"
        ]
    -- the spring
    , path
        [ at "d" ("M" <> num (r.at.x - 4.5) <> "," <> num (r.at.y + 2.0) <> " l1.5,-4 l1.5,4 l1.5,-4 l1.5,4 l1.5,-4 l1.5,4")
        , at "fill" "none", at "stroke" "#17120f", at "stroke-width" "1"
        ]
    , wordsAt r.anchor r.words.x r.words.y lines
    , if lifted then
        label
          [ at "x" (num r.words.x), at "y" (num (r.words.y + 9.0 * toNumber (length lines))), at "text-anchor" r.anchor
          , at "font-size" "8", at "font-weight" "700", at "fill" lifting
          ]
          (case carries of
            Steam -> "VENTING"
            _ -> "RELIEVING")
      else g [] []
    ]

-- | A pipe's path through its corners.
through :: Array Point -> String
through points = case uncons points of
  Nothing -> ""
  Just { head, tail } ->
    "M" <> num head.x <> "," <> num head.y <> joinWith "" (map (\p -> " L" <> num p.x <> "," <> num p.y) tail)

------------------------------------------------------------
-- Arrangements
------------------------------------------------------------

-- | One arrangement of the diagram: how big the drawing is, where each
-- | part is and which way it lies, and the corners each pipe turns at.
type Layout =
  { name :: String
  , width :: Number
  , height :: Number
  -- the parts on pipes
  , bypassValve :: Part
  , needle :: Part
  , pump :: Part
  , mixValve :: Part
  , groupValve :: Part
  , fillValve :: Part
  , steamValve :: Part
  -- vessels and other places, by their middles
  , reservoir :: Point
  , boiler :: Point
  , mix :: Point
  , group :: Point
  , wand :: Point
  -- where pipes join
  , joints :: Array Point
  -- the mechanical safeguards
  , expansion :: Relief
  , safety :: Relief
  , breaker :: Relief
  -- pipes, by their corners, in the direction of flow
  , supply :: Array Point
  , bypass :: Array Point
  , pumped :: Array Point
  , intoReservoir :: Array Point
  , outOfReservoir :: Array Point
  , coldBranch :: Array Point
  , toGroup :: Array Point
  , fill :: Array Point
  , steamOut :: Array Point
  -- where the words about the water supply, the mix point, the brew
  -- pressure and the sensors go
  , supplyNote :: Point
  , mixNote :: { x :: Number, y :: Number, anchor :: String }
  , pressureNote :: Point
  , sensorNote :: Point
  }

-- | The brew path in a row across the top, the cold branch under it,
-- | and the steam side in a row of its own along the bottom.
wide :: Layout
wide =
  { name: "wide"
  , width: 560.0
  , height: 330.0
  , bypassValve: level 125.0 44.0
  , needle: level 187.0 44.0
  , pump: level 152.0 120.0
  , mixValve: level 322.0 200.0
  , groupValve: level 458.0 120.0
  , fillValve: level 135.0 290.0
  , steamValve: level 360.0 290.0
  , reservoir: at' 312.0 120.0
  , boiler: at' 245.0 290.0
  , mix: at' 412.0 120.0
  , group: at' 524.0 172.0
  , wand: at' 450.0 290.0
  , joints: [ at' 80.0 120.0, at' 232.0 120.0 ]
  , expansion: { from: at' 200.0 120.0, at: at' 200.0 90.0, drainTo: at' 200.0 74.0, words: at' 189.0 88.0, anchor: "end" }
  , safety: { from: at' 222.0 268.0, at: at' 222.0 247.0, drainTo: at' 222.0 232.0, words: at' 233.0 244.0, anchor: "start" }
  , breaker: { from: at' 280.0 268.0, at: at' 280.0 247.0, drainTo: at' 280.0 247.0, words: at' 291.0 250.0, anchor: "start" }
  , supply: [ at' 8.0 120.0, at' 80.0 120.0 ]
  , bypass: [ at' 80.0 120.0, at' 80.0 44.0, at' 232.0 44.0, at' 232.0 120.0 ]
  , pumped: [ at' 80.0 120.0, at' 232.0 120.0 ]
  , intoReservoir: [ at' 232.0 120.0, at' 274.0 120.0 ]
  , outOfReservoir: [ at' 350.0 120.0, at' 412.0 120.0 ]
  , coldBranch: [ at' 232.0 120.0, at' 232.0 200.0, at' 412.0 200.0, at' 412.0 120.0 ]
  , toGroup: [ at' 412.0 120.0, at' 524.0 120.0, at' 524.0 163.0 ]
  , fill: [ at' 80.0 120.0, at' 80.0 290.0, at' 190.0 290.0 ]
  , steamOut: [ at' 300.0 290.0, at' 430.0 290.0 ]
  , supplyNote: at' 40.0 108.0
  , mixNote: { x: 412.0, y: 102.0, anchor: "middle" }
  , pressureNote: at' 262.0 74.0
  , sensorNote: at' 480.0 30.0
  }

-- | The brew path in a column. The cold water is all on the left: the
-- | bypass round the pump nearest the column and the cold branch to the
-- | mix point outside it, each with its valve standing on its own
-- | upright pipe. The steam side is a column of its own down the right.
tall :: Layout
tall =
  { name: "tall"
  , width: 370.0
  , height: 620.0
  , bypassValve: standing 100.0 106.0 (-1.0)
  , needle: standing 100.0 168.0 (-1.0)
  , pump: standing 160.0 134.0 1.0
  , mixValve: standing 40.0 332.0 1.0
  , groupValve: standing 160.0 478.0 1.0
  , fillValve: standing 300.0 118.0 1.0
  , steamValve: standing 300.0 300.0 1.0
  , reservoir: at' 160.0 302.0
  , boiler: at' 300.0 206.0
  , mix: at' 160.0 426.0
  , group: at' 160.0 552.0
  , wand: at' 300.0 388.0
  , joints: [ at' 160.0 62.0, at' 160.0 212.0, at' 160.0 240.0 ]
  , expansion: { from: at' 160.0 184.0, at: at' 192.0 184.0, drainTo: at' 208.0 184.0, words: at' 192.0 202.0, anchor: "middle" }
  , safety: { from: at' 268.0 184.0, at: at' 268.0 162.0, drainTo: at' 268.0 146.0, words: at' 257.0 160.0, anchor: "end" }
  , breaker: { from: at' 334.0 184.0, at: at' 334.0 162.0, drainTo: at' 334.0 162.0, words: at' 345.0 160.0, anchor: "start" }
  , supply: [ at' 160.0 8.0, at' 160.0 62.0 ]
  , bypass: [ at' 160.0 62.0, at' 100.0 62.0, at' 100.0 212.0, at' 160.0 212.0 ]
  , pumped: [ at' 160.0 62.0, at' 160.0 212.0 ]
  , intoReservoir: [ at' 160.0 212.0, at' 160.0 264.0 ]
  , outOfReservoir: [ at' 160.0 340.0, at' 160.0 426.0 ]
  , coldBranch: [ at' 160.0 240.0, at' 40.0 240.0, at' 40.0 426.0, at' 160.0 426.0 ]
  , toGroup: [ at' 160.0 426.0, at' 160.0 543.0 ]
  , fill: [ at' 160.0 62.0, at' 300.0 62.0, at' 300.0 184.0 ]
  , steamOut: [ at' 300.0 228.0, at' 300.0 372.0 ]
  , supplyNote: at' 222.0 30.0
  , mixNote: { x: 174.0, y: 422.0, anchor: "start" }
  , pressureNote: at' 222.0 226.0
  , sensorNote: at' 280.0 596.0
  }

-- | Everything on one line, for a window much wider than it is high:
-- | the brew path on the left and the steam side beyond it on the
-- | right, fed by a pipe run along the bottom.
strip :: Layout
strip =
  { name: "strip"
  , width: 900.0
  , height: 214.0
  , bypassValve: level 104.0 32.0
  , needle: level 158.0 32.0
  , pump: level 124.0 100.0
  , mixValve: level 276.0 166.0
  , groupValve: level 408.0 100.0
  , fillValve: level 590.0 100.0
  , steamValve: level 800.0 100.0
  , reservoir: at' 270.0 100.0
  , boiler: at' 696.0 100.0
  , mix: at' 362.0 100.0
  , group: at' 478.0 132.0
  , wand: at' 872.0 100.0
  , joints: [ at' 64.0 100.0, at' 190.0 100.0 ]
  , expansion: { from: at' 162.0 100.0, at: at' 162.0 74.0, drainTo: at' 162.0 59.0, words: at' 151.0 70.0, anchor: "end" }
  , safety: { from: at' 674.0 78.0, at: at' 674.0 57.0, drainTo: at' 674.0 42.0, words: at' 685.0 54.0, anchor: "start" }
  , breaker: { from: at' 730.0 78.0, at: at' 730.0 57.0, drainTo: at' 730.0 57.0, words: at' 741.0 60.0, anchor: "start" }
  , supply: [ at' 6.0 100.0, at' 64.0 100.0 ]
  , bypass: [ at' 64.0 100.0, at' 64.0 32.0, at' 190.0 32.0, at' 190.0 100.0 ]
  , pumped: [ at' 64.0 100.0, at' 190.0 100.0 ]
  , intoReservoir: [ at' 190.0 100.0, at' 232.0 100.0 ]
  , outOfReservoir: [ at' 308.0 100.0, at' 362.0 100.0 ]
  , coldBranch: [ at' 190.0 100.0, at' 190.0 166.0, at' 362.0 166.0, at' 362.0 100.0 ]
  , toGroup: [ at' 362.0 100.0, at' 478.0 100.0, at' 478.0 123.0 ]
  , fill: [ at' 64.0 100.0, at' 64.0 202.0, at' 540.0 202.0, at' 540.0 100.0, at' 641.0 100.0 ]
  , steamOut: [ at' 751.0 100.0, at' 852.0 100.0 ]
  , supplyNote: at' 34.0 150.0
  , mixNote: { x: 362.0, y: 84.0, anchor: "middle" }
  , pressureNote: at' 228.0 58.0
  , sensorNote: at' 540.0 24.0
  }

------------------------------------------------------------
-- The drawing
------------------------------------------------------------

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
    -- how warm the blended water is, between the supply and the
    -- reservoir
    warmth = clamp 0.0 1.0 ((v.mixC - 20.0) / max 1.0 (v.reservoirC - 20.0))
    -- the mechanical valves, by the pressure under them alone
    relieving = v.brewBar >= v.expansionBar - 0.05
    venting = v.steamBar >= v.steamSafetyBar - 0.02
    -- the vacuum breaker is open to the air until the boiler makes
    -- pressure
    breathing = v.steamBar < 0.05
    spin = v.now * 6.0 * (if pumping then a.pumpSpeed + 0.2 else 0.0)
    spoke = { x: l.pump.x + 12.0 * sin spin, y: l.pump.y - 12.0 * cos spin }
    waterLevel = clamp 0.0 1.0 (v.steamWaterMl / 2000.0)
    boilerFloor = l.boiler.y + 20.0
    waterTop = boilerFloor - 40.0 * waterLevel
    joint point = circle [ at "cx" (num point.x), at "cy" (num point.y), at "r" "4", at "fill" idle ]
  in
    svg [ at "viewBox" ("0 0 " <> num l.width <> " " <> num l.height), at "class" ("inside " <> l.name) ]
      ( [ -- the supply
          if supplyBad then deadPipe (through l.supply) else pipe ColdWater (brewing || filling) (through l.supply)
        , if supplyBad then alarm l.supplyNote.x l.supplyNote.y "NO WATER" else tag l.supplyNote.x l.supplyNote.y "WATER IN, 3 bar"
        -- the steam side, set apart from the rest
        , pipe ColdWater filling (through l.fill)
        , valve l.fillValve "SV_FILL" filling (has FillTimeout)
        , relief l.safety Steam venting [ "SRV_STM", num v.steamSafetyBar <> " bar" ]
        , relief l.breaker Steam false [ "VB", if breathing then "open" else "shut" ]
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
        , pipe Steam v.steamValveOpen (through l.steamOut)
        , valve l.steamValve "STEAM" v.steamValveOpen false
        , tag l.wand.x (l.wand.y + 3.0) "WAND"
        -- the bypass branch
        , pipe ColdWater bypassing (through l.bypass)
        , valve l.bypassValve "SV_BYP" (a.bypassValve == Open) false
        , along l.needle [ rect [ at "x" "-17", at "y" "-9", at "width" "34", at "height" "18", at "rx" "9", at "fill" "#b08d57" ] ]
        , if l.needle.upright then beside l.needle 9.0 [ "NV_PRE" ] else tag l.needle.x (l.needle.y + 3.0) "NV_PRE"
        -- the pump branch, with the expansion valve tapped off it
        , pipe ColdWater pumping (through l.pumped)
        , relief l.expansion ColdWater relieving [ "EXP", num v.expansionBar <> " bar" ]
        , circle [ at "cx" (num l.pump.x), at "cy" (num l.pump.y), at "r" "16", at "fill" (if pumping then "#2c8a57" else "#6b7076") ]
        , line
            [ at "x1" (num l.pump.x), at "y1" (num l.pump.y), at "x2" (num spoke.x), at "y2" (num spoke.y)
            , at "stroke" "#ffffff", at "stroke-width" "3", at "stroke-linecap" "round"
            ]
        , beside l.pump (if l.pump.upright then 16.0 else 19.0) [ "PUMP " <> num (a.pumpSpeed * 100.0) <> "%" ]
        -- the hot branch, through the reservoir
        , pipe ColdWater brewing (through l.intoReservoir)
        , rect
            [ at "x" (num (l.reservoir.x - 38.0)), at "y" (num (l.reservoir.y - 38.0)), at "width" "76", at "height" "76", at "rx" "6", at "fill" "#f3d9d2"
            , at "stroke" (if hotBad then bad else hot), at "stroke-width" (if hotBad then "5" else "1")
            ]
        , element (l.reservoir.x - 18.0) (l.reservoir.y + 28.0) a.brewElementOn
        , tag l.reservoir.x (l.reservoir.y - 18.0) "RESERVOIR"
        , label
            [ at "x" (num l.reservoir.x), at "y" (num (l.reservoir.y + 2.0)), at "text-anchor" "middle", at "font-size" "13", at "fill" "#7a1f12" ]
            (num v.reservoirC <> " C")
        , pipe HotWater brewing (through l.outOfReservoir)
        -- the cold branch, through the mix valve
        , if p.coldBlocked then deadPipe (through l.coldBranch) else pipe ColdWater mixing (through l.coldBranch)
        , along l.mixValve
            ( [ rect [ at "x" "-30", at "y" "-11", at "width" "60", at "height" "22", at "rx" "4", at "fill" "#6b7076" ]
              , rect [ at "x" "-30", at "y" "-11", at "width" (num (60.0 * clamp 0.0 1.0 a.mixPosition)), at "height" "22", at "rx" "4", at "fill" cold ]
              ]
                <>
                  if l.mixValve.upright then []
                  else [ label [ at "x" "0", at "y" "3", at "text-anchor" "middle", at "font-size" "8", at "fill" "#ffffff" ] "MV_MIX" ]
            )
        , beside l.mixValve 11.0
            ( (if l.mixValve.upright then [ "MV_MIX" ] else [])
                <> [ "cold " <> num (a.mixPosition * 100.0) <> "%" ]
            )
        -- the mix point, the group valve and the group
        , pipe (Blended warmth) brewing (through l.toGroup)
        , g [] (map joint l.joints)
        , circle
            [ at "cx" (num l.mix.x), at "cy" (num l.mix.y), at "r" (if has MixOverTemperature then "10" else "7")
            , at "fill" (if has MixOverTemperature then bad else if brewing then waterColour warmth else idle)
            ]
        , label
            [ at "x" (num l.mixNote.x), at "y" (num l.mixNote.y), at "text-anchor" l.mixNote.anchor
            , at "font-size" (if has MixOverTemperature then "9" else "8")
            , at "font-weight" (if has MixOverTemperature then "700" else "400")
            , at "fill" (if has MixOverTemperature then bad else "#3a3d42")
            ]
            ("MIX " <> num v.mixC <> " C" <> (if has MixOverTemperature then ", TOO HOT" else ""))
        , valve l.groupValve "SV_GRP" (a.groupValve == ToGroup) false
        , path
            [ at "d"
                ( "M" <> num (l.group.x - 22.0) <> "," <> num (l.group.y - 9.0) <> " L" <> num (l.group.x + 22.0) <> "," <> num (l.group.y - 9.0)
                    <> " L" <> num (l.group.x + 16.0) <> "," <> num (l.group.y + 9.0) <> " L" <> num (l.group.x - 16.0) <> "," <> num (l.group.y + 9.0) <> " Z"
                )
            , at "fill" "#3a3d42"
            ]
        , tag l.group.x (l.group.y + 21.0) "GROUP"
        ]
          <> only p.coldBlocked (alarm l.mixValve.x (l.mixValve.y - (if l.mixValve.upright then 36.0 else 17.0)) "BLOCKED")
          <> only p.hardWater (alarm l.supplyNote.x (l.supplyNote.y + 12.0) "HARD WATER")
          <> only (has OverPressure) (alarm l.pressureNote.x l.pressureNote.y "OVER PRESSURE")
          <> only hotBad (alarm l.reservoir.x (l.reservoir.y - 44.0) "OVER LIMIT")
          <> only p.levelLow (alarm l.boiler.x (l.boiler.y + 34.0) "LEVEL LOW")
          <> only (has FillTimeout) (alarm l.fillValve.x (l.fillValve.y - (if l.fillValve.upright then 28.0 else 17.0)) "FILL TIMED OUT")
          <> only sensorBad (alarm l.sensorNote.x l.sensorNote.y "SENSOR FAULT")
      )
