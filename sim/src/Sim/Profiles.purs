-- | Starter profiles. Each exercises a different part of the machine.
module Sim.Profiles
  ( classic
  , linePrebrew
  , declining
  , all
  ) where

import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Maybe (Maybe(..))
import Sim.Types (Curve(..), Profile, StepMode(..))

-- | Pump-driven wetting at fixed flow, then a flat nine bar.
classic :: Profile
classic =
  { name: "Classic 9 bar"
  , targetGrams: 36.0
  , groupBaseC: 92.0
  , maxTargetC: 93.0
  , targetSeconds: Nothing
  , steps: cons'
      { name: "wet"
      , mode: Flow
      , curve: Jump
      , rampS: 0.0
      , targetC: 93.0
      , targetBar: Nothing
      , targetMlPerS: Just 2.0
      , maxDuration: 15.0
      , exitAboveBar: Just 2.0
      , exitAboveGrams: Nothing
      }
      [ { name: "hold"
        , mode: Pressure
        , curve: Exponential
        , rampS: 4.0
        , targetC: 93.0
        , targetBar: Just 9.0
        , targetMlPerS: Nothing
        , maxDuration: 60.0
        , exitAboveBar: Nothing
        , exitAboveGrams: Nothing
        }
      ]
  }

-- | Pump off, regulated line pressure through the needle valve, then
-- | full pressure.
linePrebrew :: Profile
linePrebrew =
  { name: "Line pressure pre-brew"
  , targetGrams: 40.0
  , groupBaseC: 92.0
  , maxTargetC: 93.0
  , targetSeconds: Nothing
  , steps: cons'
      { name: "pre-brew"
      , mode: LinePrebrew
      , curve: Jump
      , rampS: 0.0
      , targetC: 93.0
      , targetBar: Nothing
      , targetMlPerS: Nothing
      , maxDuration: 30.0
      , exitAboveBar: Just 2.0
      , exitAboveGrams: Nothing
      }
      [ { name: "brew"
        , mode: Pressure
        , curve: Ease
        , rampS: 5.0
        , targetC: 93.0
        , targetBar: Just 9.0
        , targetMlPerS: Nothing
        , maxDuration: 60.0
        , exitAboveBar: Nothing
        , exitAboveGrams: Nothing
        }
      ]
  }

-- | Hot and gentle at the start, cooler and lower pressure toward the
-- | end. This is the profile that needs the mixing stage.
declining :: Profile
declining =
  { name: "Declining temperature and pressure"
  , targetGrams: 42.0
  , groupBaseC: 90.0
  , maxTargetC: 96.0
  , targetSeconds: Nothing
  , steps: cons'
      { name: "wet hot"
      , mode: Flow
      , curve: Jump
      , rampS: 0.0
      , targetC: 96.0
      , targetBar: Nothing
      , targetMlPerS: Just 1.5
      , maxDuration: 20.0
      , exitAboveBar: Just 3.0
      , exitAboveGrams: Nothing
      }
      [ { name: "peak"
        , mode: Pressure
        , curve: Exponential
        , rampS: 4.0
        , targetC: 94.0
        , targetBar: Just 8.5
        , targetMlPerS: Nothing
        , maxDuration: 10.0
        , exitAboveBar: Nothing
        , exitAboveGrams: Nothing
        }
      , { name: "decline"
        , mode: Pressure
        , curve: Linear
        , rampS: 10.0
        , targetC: 90.0
        , targetBar: Just 6.5
        , targetMlPerS: Nothing
        , maxDuration: 12.0
        , exitAboveBar: Nothing
        , exitAboveGrams: Nothing
        }
      , { name: "finish cool"
        , mode: Flow
        , curve: Jump
        , rampS: 0.0
        , targetC: 87.0
        , targetBar: Nothing
        , targetMlPerS: Just 1.8
        , maxDuration: 40.0
        , exitAboveBar: Nothing
        , exitAboveGrams: Nothing
        }
      ]
  }

all :: NonEmptyArray Profile
all = cons' classic [ linePrebrew, declining ]
