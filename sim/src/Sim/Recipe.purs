-- | A recipe is what the barista dials in: which profile shape, how much
-- | coffee, what ratio, how hot. It resolves to a `Profile` before it
-- | reaches the controller, so the controller and the spec know nothing
-- | about doses or ratios.
module Sim.Recipe
  ( Recipe
  , Limits
  , doseLimits
  , ratioLimits
  , targetLimits
  , tempLimits
  , targetGrams
  , withTarget
  , toProfile
  , builtIn
  , encode
  , decode
  ) where

import Prelude

import Data.Array (catMaybes, filter)
import Data.Array.NonEmpty as NEA
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split)
import Sim.Profiles as Profiles
import Sim.Types (Profile)

type Recipe =
  { name :: String
  -- Position of the profile shape in `Profiles.all`.
  , profileIndex :: Int
  , doseG :: Number
  , ratio :: Number
  -- Water temperature of the hottest step. The other steps keep their
  -- offsets from it.
  , tempC :: Number
  }

type Limits = { min :: Number, max :: Number, step :: Number }

doseLimits :: Limits
doseLimits = { min: 7.0, max: 25.0, step: 0.1 }

ratioLimits :: Limits
ratioLimits = { min: 1.0, max: 4.0, step: 0.05 }

targetLimits :: Limits
targetLimits = { min: 10.0, max: 90.0, step: 0.5 }

-- | The top of the range is the reservoir temperature less the headroom
-- | the controller insists on.
tempLimits :: Limits
tempLimits = { min: 84.0, max: 98.0, step: 0.5 }

targetGrams :: Recipe -> Number
targetGrams r = r.doseG * r.ratio

-- | Set the weight in the cup by moving the ratio, keeping the dose.
withTarget :: Number -> Recipe -> Recipe
withTarget grams r =
  r { ratio = clamp ratioLimits.min ratioLimits.max (grams / r.doseG) }

toProfile :: Recipe -> Profile
toProfile r =
  let
    base = fromMaybe (NEA.head Profiles.all) (NEA.index Profiles.all r.profileIndex)
    shift = r.tempC - base.maxTargetC
  in
    base
      { targetGrams = targetGrams r
      , groupBaseC = base.groupBaseC + shift
      , maxTargetC = r.tempC
      , steps = map (\step -> step { targetC = step.targetC + shift }) base.steps
      }

builtIn :: Array Recipe
builtIn =
  [ { name: "House espresso", profileIndex: 0, doseG: 18.0, ratio: 2.0, tempC: 93.0 }
  , { name: "Light roast, long pre-brew", profileIndex: 1, doseG: 22.0, ratio: 2.5, tempC: 95.0 }
  , { name: "Dark roast, declining", profileIndex: 2, doseG: 17.0, ratio: 2.0, tempC: 92.0 }
  ]

-- | One recipe per line, fields separated by tabs.
encode :: Array Recipe -> String
encode recipes = joinWith "\n" (map line recipes)
  where
  clean = replaceAll (Pattern "\t") (Replacement " ") <<< replaceAll (Pattern "\n") (Replacement " ")
  line r = joinWith "\t"
    [ clean r.name
    , show r.profileIndex
    , show r.doseG
    , show r.ratio
    , show r.tempC
    ]

-- | Lines that do not parse are dropped.
decode :: String -> Array Recipe
decode text = catMaybes (map line (filter (_ /= "") (split (Pattern "\n") text)))
  where
  line l = case split (Pattern "\t") l of
    [ name, index, dose, ratio, temp ] -> do
      profileIndex <- Int.fromString index
      doseG <- Number.fromString dose
      ratioValue <- Number.fromString ratio
      tempC <- Number.fromString temp
      pure
        { name
        , profileIndex
        , doseG: clamp doseLimits.min doseLimits.max doseG
        , ratio: clamp ratioLimits.min ratioLimits.max ratioValue
        , tempC: clamp tempLimits.min tempLimits.max tempC
        }
    _ -> Nothing