-- | A recipe is what the barista dials in for one bag of beans: which
-- | beans, which profile shape, how much coffee, what ratio, how hot and
-- | how long. It resolves to a `Profile` before it reaches the
-- | controller, so the controller and the spec know nothing about beans,
-- | doses or ratios.
module Sim.Recipe
  ( Recipe
  , Roast(..)
  , roasts
  , roastLabel
  , roastNote
  , development
  , withRoastDefaults
  , puckOf
  , grindLimits
  , Limits
  , doseLimits
  , ratioLimits
  , targetLimits
  , tempLimits
  , secondsLimits
  , targetGrams
  , withTarget
  , toProfile
  , builtIn
  , encode
  , decode
  ) where

import Prelude

import Data.Array (catMaybes, filter, find, index)
import Data.Array.NonEmpty as NEA
import Data.Int (round, toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (exp)
import Data.Number as Number
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split)
import Sim.Plant (Puck)
import Sim.Profiles as Profiles
import Sim.Types (Profile)

-- | Degree of roast, in the roaster's own names, from lightest to
-- | darkest. The names are trade usage and not a standard: City, City+,
-- | Full City and Full City+ follow the scale Sweet Maria's uses.
data Roast
  = Cinnamon
  | NewEngland
  | American
  | City
  | CityPlus
  | FullCity
  | FullCityPlus
  | Vienna
  | French
  | Italian

derive instance eqRoast :: Eq Roast

roasts :: Array Roast
roasts = [ Cinnamon, NewEngland, American, City, CityPlus, FullCity, FullCityPlus, Vienna, French, Italian ]

roastLabel :: Roast -> String
roastLabel = case _ of
  Cinnamon -> "Cinnamon"
  NewEngland -> "New England"
  American -> "American"
  City -> "City"
  CityPlus -> "City+"
  FullCity -> "Full City"
  FullCityPlus -> "Full City+"
  Vienna -> "Vienna"
  French -> "French"
  Italian -> "Italian"

-- | How far the roast was taken, from 0 at Cinnamon to 1 at Italian.
-- | Everything the machine works out from the roast comes from this one
-- | number, so a level between two others behaves between them.
development :: Roast -> Number
development = case _ of
  Cinnamon -> 0.0
  NewEngland -> 0.1
  American -> 0.2
  City -> 0.3
  CityPlus -> 0.42
  FullCity -> 0.55
  FullCityPlus -> 0.67
  Vienna -> 0.78
  French -> 0.9
  Italian -> 1.0

-- | Where the roast was stopped, and what that means for the shot.
roastNote :: Roast -> String
roastNote roast = stopped <> " " <> advice
  where
  stopped = case roast of
    Cinnamon -> "Dropped at the very start of first crack. Pale, dry, grassy and sharp."
    NewEngland -> "Dropped during first crack. Light brown and dry, with high acidity."
    American -> "Dropped as first crack ends. Medium-light brown, still dry."
    City -> "First crack has just finished. Origin character and acidity are at their clearest."
    CityPlus -> "A little past the end of first crack. More sweetness, acidity still bright."
    FullCity -> "On the brink of second crack. Balanced sweetness, body and acidity."
    FullCityPlus -> "The first snaps of second crack. Spots of oil, bittersweet, heavier body."
    Vienna -> "Early in second crack. Light surface oil, and roast flavour starts to cover origin."
    French -> "Well into second crack. Oily, pungent and smoky, with little acidity."
    Italian -> "Near the end of second crack. Black and oily, with a thin, burnt edge."
  d = development roast
  advice
    | d < 0.5 = "Dense and slow to give up its flavour: grind finer, brew hotter and longer, with a long pre-brew. Sour or thin means it needs more of all three."
    | d < 0.72 = "The reference point: the grind and temperature here are what the others are measured from."
    | otherwise = "Brittle and quick to extract: grind coarser, brew cooler and shorter. Bitter or dry means it needs less."

-- | A value read off three known points: City, Full City and French.
-- | Between them it runs in straight lines, and the outer lines carry on
-- | to Cinnamon and Italian.
through :: Number -> Number -> Number -> Roast -> Number
through atCity atFullCity atFrench roast =
  let
    d = development roast
  in
    if d <= 0.55 then atFullCity + (atCity - atFullCity) * (0.55 - d) / 0.25
    else atFullCity + (atFrench - atFullCity) * (d - 0.55) / 0.35

type Recipe =
  { -- The beans. The name is also the name the recipe is saved under.
    name :: String
  , roaster :: String
  -- The roast date as written on the bag.
  , roastedOn :: String
  , roast :: Roast
  -- Position of the profile shape among the built-in shapes followed
  -- by the user's own.
  , profileIndex :: Int
  , doseG :: Number
  , ratio :: Number
  -- Water temperature of the hottest step. The other steps keep their
  -- offsets from it.
  , tempC :: Number
  -- How long the shot should take, and whether the machine may adjust
  -- pressure to get there.
  , seconds :: Number
  , paced :: Boolean
  -- Grinder steps from this bag's starting grind. Positive is coarser.
  , grind :: Number
  }

-- | The puck this recipe puts in the basket. At the same grind a darker
-- | roast shatters into more fines and resists the water more, and it
-- | gives up its flavour sooner. A lighter roast is the reverse. Each
-- | step coarser lets the water through more easily and extracts a
-- | little less.
puckOf :: Recipe -> Puck
puckOf r =
  let
    roast =
      { resistance: through 0.85 1.0 1.3 r.roast
      , solubility: through 0.72 1.0 1.3 r.roast
      }
  in
    { doseG: r.doseG
    , resistance: roast.resistance * exp (-0.14 * r.grind)
    , solubility: roast.solubility * exp (-0.05 * r.grind)
    }

-- | The machine's starting point for a roast: temperature, ratio, shot
-- | time, grind and profile shape. Lighter roasts get a finer grind,
-- | hotter water, a longer ratio and time, and a line-pressure pre-brew.
-- | Darker roasts get a coarser grind, cooler water, and a shorter ratio
-- | and time.
withRoastDefaults :: Recipe -> Recipe
withRoastDefaults r =
  let
    half value = toNumber (round (value * 2.0)) / 2.0
  in
    r
      { tempC = clamp tempLimits.min tempLimits.max (half (through 95.0 93.0 90.0 r.roast))
      , ratio = clamp ratioLimits.min ratioLimits.max (toNumber (round (through 2.5 2.0 1.8 r.roast * 20.0)) / 20.0)
      , seconds = clamp secondsLimits.min secondsLimits.max (toNumber (round (through 37.0 28.0 25.0 r.roast)))
      , grind = clamp grindLimits.min grindLimits.max (half (through (-2.0) 0.0 3.0 r.roast))
      -- Anything lighter than Full City gets the line-pressure pre-brew.
      , profileIndex = if development r.roast < 0.5 then 1 else 0
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

grindLimits :: Limits
grindLimits = { min: -5.0, max: 5.0, step: 0.5 }

secondsLimits :: Limits
secondsLimits = { min: 15.0, max: 60.0, step: 1.0 }

targetGrams :: Recipe -> Number
targetGrams r = r.doseG * r.ratio

-- | Set the weight in the cup by moving the ratio, keeping the dose.
withTarget :: Number -> Recipe -> Recipe
withTarget grams r =
  r { ratio = clamp ratioLimits.min ratioLimits.max (grams / r.doseG) }

-- | Resolve a recipe against the available shapes. A recipe that names
-- | a shape which no longer exists falls back to the first built-in.
toProfile :: Array Profile -> Recipe -> Profile
toProfile shapes r =
  let
    base = fromMaybe (NEA.head Profiles.all) (index shapes r.profileIndex)
    shift = r.tempC - base.maxTargetC
  in
    base
      { targetGrams = targetGrams r
      , groupBaseC = base.groupBaseC + shift
      , maxTargetC = r.tempC
      , targetSeconds = if r.paced then Just r.seconds else Nothing
      , steps = map (\step -> step { targetC = step.targetC + shift }) base.steps
      }

builtIn :: Array Recipe
builtIn =
  [ withRoastDefaults blank { name = "House espresso", roast = FullCity }
  , withRoastDefaults blank { name = "City roast", roast = City }
  , withRoastDefaults blank { name = "French roast", roast = French }
  ]
  where
  blank =
    { name: "", roaster: "", roastedOn: "", roast: FullCity
    , profileIndex: 0, doseG: 18.0, ratio: 2.0, tempC: 93.0, seconds: 30.0, paced: true, grind: 0.0
    }

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
    , clean r.roaster
    , clean r.roastedOn
    , roastLabel r.roast
    , show r.seconds
    , if r.paced then "paced" else "free"
    , show r.grind
    ]

-- | Lines that do not parse are dropped. A line with only the first five
-- | fields was stored before recipes recorded their beans, and reads as
-- | a medium roast with no pacing.
decode :: String -> Array Recipe
decode text = catMaybes (map line (filter (_ /= "") (split (Pattern "\n") text)))
  where
  line l = case split (Pattern "\t") l of
    [ name, shape, dose, ratio, temp ] ->
      core name shape dose ratio temp <#> _ { paced = false }
    [ name, shape, dose, ratio, temp, roaster, roastedOn, roast, seconds, paced, grind ] -> do
      r <- core name shape dose ratio temp
      s <- Number.fromString seconds
      steps <- Number.fromString grind
      pure r
        { roaster = roaster
        , roastedOn = roastedOn
        , roast = roastNamed roast
        , seconds = clamp secondsLimits.min secondsLimits.max s
        , paced = paced == "paced"
        , grind = clamp grindLimits.min grindLimits.max steps
        }
    _ -> Nothing
  -- The first version stored three levels under plainer names.
  roastNamed stored = case stored of
    "Light" -> City
    "Medium" -> FullCity
    "Dark" -> French
    _ -> fromMaybe FullCity (find (\known -> roastLabel known == stored) roasts)
  core name shape dose ratio temp = do
    profileIndex <- Int.fromString shape
    doseG <- Number.fromString dose
    ratioValue <- Number.fromString ratio
    tempC <- Number.fromString temp
    pure
      { name
      , roaster: ""
      , roastedOn: ""
      , roast: FullCity
      , profileIndex
      , doseG: clamp doseLimits.min doseLimits.max doseG
      , ratio: clamp ratioLimits.min ratioLimits.max ratioValue
      , tempC: clamp tempLimits.min tempLimits.max tempC
      , seconds: 30.0
      , paced: true
      , grind: 0.0
      }
