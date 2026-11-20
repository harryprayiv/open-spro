-- | What ends up in the cup, judged from how much of the coffee the
-- | water took with it and how evenly the water passed through the puck,
-- | and the colour of the stream on its way there.
-- | This is a qualitative model for the simulator. Nothing in the
-- | firmware depends on it.
module Sim.Cup
  ( Taste(..)
  , taste
  , tasteNote
  , channeled
  , cupNote
  , streamColour
  ) where

import Prelude

import Data.Int (round)
import Data.Number (pow)
import Sim.Plant (Brewed)
import Sim.Recipe (Roast, development)

data Taste = Under | Balanced | Over

derive instance eqTaste :: Eq Taste

-- | `extracted` is the share of what the coffee can give up that
-- | reached the cup.
taste :: Number -> Taste
taste extracted
  | extracted < 0.60 = Under
  | extracted > 0.75 = Over
  | otherwise = Balanced

tasteNote :: Taste -> String
tasteNote = case _ of
  Under -> "Under-extracted: sour, thin, sharp."
  Balanced -> "Balanced."
  Over -> "Over-extracted: bitter, dry, harsh."

-- | A puck this uneven has channeled: part of it was washed out too far
-- | and the rest hardly at all.
channeled :: Brewed -> Boolean
channeled b = b.evenness < 0.8

-- | The verdict on a cup in words. A channeled puck is named whatever
-- | the extraction came to, because its cup is sour and bitter at once
-- | and no change to the extraction alone will mend it.
cupNote :: Number -> Brewed -> String
cupNote extracted b =
  tasteNote (taste extracted)
    <> (if channeled b then " The puck channeled: sour and bitter together, with the notes blurred." else "")

type Rgb = { r :: Number, g :: Number, b :: Number }

-- | The stream starts the colour of the roast and pales to blond as the
-- | puck gives up what it has. A stream that runs too fast is pale and
-- | thin from the start, which is crema tailing off, and so is the
-- | stream from a channeled puck.
streamColour :: Roast -> Number -> Number -> Number -> String
streamColour roast extracted evenness flowMlPerS =
  let
    -- From the tan of a Cinnamon roast to the near black of an Italian.
    d = development roast
    pale = { r: 150.0, g: 96.0, b: 44.0 }
    black = { r: 46.0, g: 24.0, b: 12.0 }
    dark = { r: pale.r + (black.r - pale.r) * d, g: pale.g + (black.g - pale.g) * d, b: pale.b + (black.b - pale.b) * d }
    blond = { r: 232.0, g: 204.0, b: 146.0 }
    spent = pow (clamp 0.0 1.0 (extracted / 0.8)) 1.6
    rushed = clamp 0.0 0.6 ((flowMlPerS - 2.5) / 3.0)
    streaked = clamp 0.0 0.5 ((0.9 - evenness) * 1.5)
    amount = clamp 0.0 1.0 (spent + rushed + streaked)
    mix :: (Rgb -> Number) -> String
    mix part = show (round (part dark + (part blond - part dark) * amount))
  in
    "rgb(" <> mix _.r <> "," <> mix _.g <> "," <> mix _.b <> ")"
