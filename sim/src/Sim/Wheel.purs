-- | The coffee map from the press packet, drawn for the cup the present
-- | recipe is forecast to make. Notes the cup should show are outlined
-- | in orange, as one guest's answers are in the packet. Clicking a note
-- | marks it as the one wanted.
module Sim.View.Wheel (wheel) where

import Prelude

import Data.Array (concat, index, mapWithIndex, range)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, pi, sin)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sim.Flavor (Character, families, noteCount, notes, shows)
import Sim.Model (Action(..))
import Sim.View.Svg (at, circle, label, line, num, path, svg)

type WheelInput =
  { -- How far round the wheel the cup lands, 0 to 1.
    place :: Number
  -- Its weight and clarity.
  , character :: Character
  -- The note the barista is after, counted round from jasmine.
  , wanted :: Maybe Int
  }

ink :: String
ink = "#17120f"

-- | A point at radius `r` and `turn` of the way round, clockwise from
-- | the top.
point :: Number -> Number -> { x :: Number, y :: Number }
point r turn = { x: r * sin (turn * 2.0 * pi), y: -r * cos (turn * 2.0 * pi) }

-- | A slice of a ring.
sector :: Number -> Number -> Number -> Number -> String
sector inner outer from to =
  let
    a = point outer from
    b = point outer to
    c = point inner to
    d = point inner from
    xy p = num p.x <> "," <> num p.y
  in
    "M" <> xy a <> " A" <> num outer <> "," <> num outer <> " 0 0 1 " <> xy b
      <> " L" <> xy c <> " A" <> num inner <> "," <> num inner <> " 0 0 0 " <> xy d <> " Z"

-- | Text along a spoke. On the left half it is turned round so it never
-- | reads upside down.
spoke :: forall w i. Number -> Number -> String -> String -> String -> HH.HTML w i
spoke radius turn anchorRight klass content =
  let
    degrees = turn * 360.0 - 90.0
    right = turn < 0.5
    anchor = if anchorRight == "middle" then "middle" else if right then "start" else "end"
  in
    label
      [ at "class" klass
      , at "transform" ("rotate(" <> num (if right then degrees else degrees + 180.0) <> ")")
      , at "x" (num (if right then radius else -radius))
      , at "y" "4.5"
      , at "text-anchor" anchor
      ]
      content

roastBand :: Array String
roastBand = [ "#e8dfc6", "#cdbe98", "#b39b6e", "#8f7348", "#6b5234", "#4a3822", "#2e2316", "#17120f" ]

wheel :: forall w. WheelInput -> HH.HTML w Action
wheel v =
  let
    eighth i = toNumber i / 8.0
    slot i = toNumber i / toNumber noteCount
    band = mapWithIndex
      (\i fill -> path [ at "d" (sector 322.0 377.0 (eighth i) (eighth (i + 1))), at "fill" fill, at "stroke" "#ffffff", at "stroke-width" "2" ])
      roastBand
    family i f =
      [ path [ at "d" (sector 55.0 144.0 (eighth i) (eighth (i + 1))), at "fill" f.fill, at "stroke" "#ffffff", at "stroke-width" "2" ]
      , spoke 99.5 (eighth i + 1.0 / 16.0) "middle" (if f.lightText then "wheel-family light" else "wheel-family") f.name
      ]
    note i =
      let
        f = index families (i / 3)
        shown = shows v.character v.place i
        lit = shown >= 0.35
        isWanted = v.wanted == Just i
        klass = "wheel-note" <> (if lit then " lit" else "") <> (if isWanted then " wanted" else "")
      in
        [ HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName "path")
            [ HP.attr (AttrName "d") (sector 144.0 322.0 (slot i) (slot (i + 1)))
            , HP.attr (AttrName "class") klass
            , HP.attr (AttrName "fill") (if lit then "#ffffff" else fromMaybe "#dddddd" (map _.pale f))
            , HE.onClick \_ -> Change _ { wanted = if isWanted then Nothing else Just i }
            ]
            []
        , spoke 157.0 (slot i + 1.0 / 48.0) "edge" (if lit then "wheel-name lit" else "wheel-name") (fromMaybe "" (index notes i))
        ]
    -- where the cup lands: a tick across the roast band and a dot on it
    tickIn = point 316.0 v.place
    tickOut = point 383.0 v.place
    dot = point 349.5 v.place
    -- the cup's weight, along the spoke under the tick: heavy body near
    -- the centre, sweet and clear near the rim
    spokeIn = point 152.0 v.place
    spokeOut = point 314.0 v.place
    weightAt = point (233.0 - 75.0 * v.character.weight) v.place
  in
    svg [ at "viewBox" "-392 -392 784 784", at "class" "wheel" ]
      ( band
          <> [ spoke 349.5 (1.0 / 16.0) "middle" "wheel-band" "LIGHT", spoke 349.5 (15.0 / 16.0) "middle" "wheel-band light" "DARK" ]
          <> concat (map note (range 0 (noteCount - 1)))
          <> concat (mapWithIndex family families)
          <>
            [ circle [ at "r" "55", at "fill" ink, at "stroke" "#ffffff", at "stroke-width" "2" ]
            , label [ at "class" "wheel-family light", at "y" "4.5", at "text-anchor" "middle" ] "CUP"
            , line
                [ at "class" "wheel-tick", at "x1" (num tickIn.x), at "y1" (num tickIn.y), at "x2" (num tickOut.x), at "y2" (num tickOut.y) ]
            , circle [ at "class" "wheel-dot", at "cx" (num dot.x), at "cy" (num dot.y), at "r" "9" ]
            , line
                [ at "class" "wheel-spoke", at "x1" (num spokeIn.x), at "y1" (num spokeIn.y), at "x2" (num spokeOut.x), at "y2" (num spokeOut.y) ]
            , circle [ at "class" "wheel-weight", at "cx" (num weightAt.x), at "cy" (num weightAt.y), at "r" "8" ]
            ]
      )
