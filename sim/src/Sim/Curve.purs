-- | The shapes a setpoint can follow on its way to a step's target.
module Sim.Curve
  ( all
  , label
  , shape
  ) where

import Prelude

import Data.Number (exp)
import Sim.Types (Curve(..))

all :: Array Curve
all = [ Jump, Linear, Ease, Exponential ]

label :: Curve -> String
label = case _ of
  Jump -> "Jump"
  Linear -> "Straight"
  Ease -> "Eased"
  Exponential -> "Fast then settling"

-- | How far along the way to the target a setpoint is when the fraction
-- | `x` of the ramp time has passed. Every curve ends at 1.
shape :: Curve -> Number -> Number
shape curve raw =
  let
    x = clamp 0.0 1.0 raw
  in
    case curve of
      Jump -> 1.0
      Linear -> x
      Ease -> x * x * (3.0 - 2.0 * x)
      Exponential -> (1.0 - exp (-4.0 * x)) / (1.0 - exp (-4.0))
