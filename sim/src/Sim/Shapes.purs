-- | The user's own profile shapes: making one from another, editing its
-- | steps, and storing the set as text.
module Sim.Shapes
  ( fork
  , editStep
  , addStep
  , removeStep
  , tidy
  , encode
  , decode
  ) where

import Prelude

import Data.Array (catMaybes, filter, find, foldl, snoc)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split)
import Sim.Curve as Curve
import Sim.Types (Curve, Profile, Step, StepMode(..))

-- | A copy to edit, so the built-in shapes stay as they are.
fork :: Profile -> Profile
fork profile = profile { name = profile.name <> " copy" }

-- | Keep a profile's summary fields true to its steps and its numbers
-- | inside sane bounds.
tidy :: Profile -> Profile
tidy profile =
  let
    steps = map tidyStep profile.steps
    hottest = foldl (\acc step -> max acc step.targetC) 0.0 (NEA.toArray steps)
  in
    profile
      { steps = steps
      , maxTargetC = hottest
      , groupBaseC = clamp 70.0 hottest profile.groupBaseC
      }

tidyStep :: Step -> Step
tidyStep step = step
  { rampS = clamp 0.0 60.0 step.rampS
  , targetC = clamp 70.0 98.0 step.targetC
  , maxDuration = clamp 1.0 120.0 step.maxDuration
  , targetBar = map (clamp 0.5 10.5) step.targetBar
  , targetMlPerS = map (clamp 0.2 8.0) step.targetMlPerS
  , exitAboveBar = map (clamp 0.2 10.5) step.exitAboveBar
  }

editStep :: Int -> (Step -> Step) -> Profile -> Profile
editStep i change profile =
  tidy profile { steps = fromMaybe profile.steps (NEA.modifyAt i change profile.steps) }

-- | Append a copy of the last step.
addStep :: Profile -> Profile
addStep profile =
  let
    template = NEA.last profile.steps
    count = NEA.length profile.steps
  in
    tidy profile { steps = NEA.snoc profile.steps template { name = "step " <> show (count + 1), exitAboveBar = Nothing, exitAboveGrams = Nothing } }

-- | Remove a step. A profile keeps at least one.
removeStep :: Int -> Profile -> Profile
removeStep i profile =
  case NEA.deleteAt i profile.steps >>= NEA.fromArray of
    Just steps -> tidy profile { steps = steps }
    Nothing -> profile

------------------------------------------------------------
-- Stored text
------------------------------------------------------------

clean :: String -> String
clean = replaceAll (Pattern "\t") (Replacement " ") <<< replaceAll (Pattern "\n") (Replacement " ")

modeCode :: StepMode -> String
modeCode = show

modeFrom :: String -> Maybe StepMode
modeFrom = case _ of
  "line_prebrew" -> Just LinePrebrew
  "pressure" -> Just Pressure
  "flow" -> Just Flow
  _ -> Nothing

curveFrom :: String -> Maybe Curve
curveFrom text = find (\curve -> show curve == text) Curve.all

optional :: Maybe Number -> String
optional = case _ of
  Just value -> show value
  Nothing -> "-"

readOptional :: String -> Maybe (Maybe Number)
readOptional = case _ of
  "-" -> Just Nothing
  text -> map Just (Number.fromString text)

-- | A `P` line opens a profile and the `S` lines after it are its steps.
encode :: Array Profile -> String
encode profiles = joinWith "\n" (Array.concatMap profile profiles)
  where
  profile p =
    [ joinWith "\t" [ "P", clean p.name, show p.targetGrams, show p.groupBaseC ] ]
      <> map step (NEA.toArray p.steps)
  step s = joinWith "\t"
    [ "S"
    , clean s.name
    , modeCode s.mode
    , show s.curve
    , show s.rampS
    , show s.targetC
    , optional s.targetBar
    , optional s.targetMlPerS
    , show s.maxDuration
    , optional s.exitAboveBar
    , optional s.exitAboveGrams
    ]

type Draft = { name :: String, targetGrams :: Number, groupBaseC :: Number, steps :: Array Step }

-- | Lines that do not parse are dropped, and so is a profile left with
-- | no steps.
decode :: String -> Array Profile
decode text =
  catMaybes (map finish (foldl line [] (filter (_ /= "") (split (Pattern "\n") text))))
  where
  finish :: Draft -> Maybe Profile
  finish draft = NEA.fromArray draft.steps <#> \steps ->
    tidy { name: draft.name, targetGrams: draft.targetGrams, groupBaseC: draft.groupBaseC, maxTargetC: 0.0, targetSeconds: Nothing, steps }

  line :: Array Draft -> String -> Array Draft
  line drafts raw = case split (Pattern "\t") raw of
    [ "P", name, grams, base ] ->
      case Number.fromString grams, Number.fromString base of
        Just targetGrams, Just groupBaseC -> snoc drafts { name, targetGrams, groupBaseC, steps: [] }
        _, _ -> drafts
    [ "S", name, mode, curve, ramp, temp, bar, flow, longest, exitBar, exitGrams ] ->
      case parsed of
        Just step -> appendStep step drafts
        Nothing -> drafts
      where
      parsed = do
        mode' <- modeFrom mode
        curve' <- curveFrom curve
        rampS <- Number.fromString ramp
        targetC <- Number.fromString temp
        targetBar <- readOptional bar
        targetMlPerS <- readOptional flow
        maxDuration <- Number.fromString longest
        exitAboveBar <- readOptional exitBar
        exitAboveGrams <- readOptional exitGrams
        pure { name, mode: mode', curve: curve', rampS, targetC, targetBar, targetMlPerS, maxDuration, exitAboveBar, exitAboveGrams }
    _ -> drafts

  appendStep :: Step -> Array Draft -> Array Draft
  appendStep step drafts = case Array.unsnoc drafts of
    Just { init, last } -> snoc init last { steps = snoc last.steps step }
    Nothing -> drafts
