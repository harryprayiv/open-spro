-- | Colours that run from cool to warm. The night face of the machine
-- | colours its hands and figures by how far along their scale they
-- | are: cool at the bottom, warm at the top.
-- |
-- | The steps between are even to the eye, which a straight blend of
-- | red, green and blue is not: halfway between a cyan and an orange in
-- | sRGB is a dull grey that looks darker than either. The blend here is
-- | made in Oklab (Björn Ottosson, 2020), a colour space built so that
-- | equal distances look like equal changes.
module Sim.Tint
  ( cool
  , warm
  , coolToWarm
  , blend
  , lightness
  ) where

import Prelude

import Data.Int (hexadecimal, round, toNumber, toStringAs)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Number (pow)
import Data.String (drop, length, take)

-- | The cool end: the sage of the press packet, taken towards cyan.
cool :: String
cool = "#86c9c3"

-- | The warm end: the packet's orange.
warm :: String
warm = "#e88a22"

-- | The colour this far from cool (0) to warm (1).
coolToWarm :: Number -> String
coolToWarm = blend cool warm

type Lab = { l :: Number, a :: Number, b :: Number }

-- | The colour a fraction of the way from one to another, by a straight
-- | line in Oklab. Both are written as "#rrggbb". The fraction is held
-- | between 0 and 1.
blend :: String -> String -> Number -> String
blend from to fraction =
  let
    t = clamp 0.0 1.0 fraction
    p = toLab from
    q = toLab to
    mix x y = x + (y - x) * t
  in
    fromLab { l: mix p.l q.l, a: mix p.a q.a, b: mix p.b q.b }

-- | How light a colour looks, from 0 for black to 1 for white.
lightness :: String -> Number
lightness colour = (toLab colour).l

-- | One channel of "#rrggbb", from 0 to 1. Anything unreadable is 0.
channel :: Int -> String -> Number
channel i colour = toNumber (fromMaybe 0 (Int.fromStringAs hexadecimal (take 2 (drop (1 + 2 * i) colour)))) / 255.0

-- | sRGB is stored bent, so that dark shades get more of the numbers.
-- | These straighten it and bend it back.
straight :: Number -> Number
straight c = if c <= 0.04045 then c / 12.92 else pow ((c + 0.055) / 1.055) 2.4

bent :: Number -> Number
bent c = if c <= 0.0031308 then c * 12.92 else 1.055 * pow c (1.0 / 2.4) - 0.055

-- | A cube root that also takes numbers below zero.
cbrt :: Number -> Number
cbrt x = if x < 0.0 then -(pow (-x) (1.0 / 3.0)) else pow x (1.0 / 3.0)

toLab :: String -> Lab
toLab colour =
  let
    r = straight (channel 0 colour)
    g = straight (channel 1 colour)
    b = straight (channel 2 colour)
    l = cbrt (0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b)
    m = cbrt (0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b)
    s = cbrt (0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b)
  in
    { l: 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s
    , a: 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s
    , b: 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
    }

fromLab :: Lab -> String
fromLab lab =
  let
    cube x = x * x * x
    l = cube (lab.l + 0.3963377774 * lab.a + 0.2158037573 * lab.b)
    m = cube (lab.l - 0.1055613458 * lab.a - 0.0638541728 * lab.b)
    s = cube (lab.l - 0.0894841775 * lab.a - 1.2914855480 * lab.b)
    r = 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s
    g = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s
    b = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
    hex c =
      let
        text = toStringAs hexadecimal (round (clamp 0.0 1.0 (bent c) * 255.0))
      in
        if length text < 2 then "0" <> text else text
  in
    "#" <> hex r <> hex g <> hex b
