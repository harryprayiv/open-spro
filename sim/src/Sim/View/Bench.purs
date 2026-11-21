-- | The profile designer, the scope and the settings.
module Sim.View.Bench (designer, forecastLine, adviceLine, scope, settings) where

import Prelude

import Data.Array (index, length, mapWithIndex, take)
import Data.String (joinWith)
import Data.Array.NonEmpty as NEA
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sim.Cup as Cup
import Sim.Curve as Curve
import Sim.Flavor as Flavor
import Sim.Model (Action(..), Field(..), State)
import Sim.Params (Entry, Group(..))
import Sim.Params as Params
import Sim.Recipe (shownStep, targetGrams, toProfile)
import Sim.Shapes as Shapes
import Sim.Types (Curve(..), ShotState(..), Step, StepMode(..))
import Sim.Advice as Advice
import Sim.Update (allShapes, askAdvice, baseShape, changeParams, deleteShape, editGroupBase, editShape, editStepTemp, isCustom, isLocked, resetParams, takeAdvice, tempShift)
import Sim.View.Plots (forecastPlot, scopePlot)
import Sim.View.Wheel (wheel)
import Sim.View.Widgets (button, chosen, cls, legend, n1, n2, numBox, row, segmented, slider, table)

------------------------------------------------------------
-- Profile designer
------------------------------------------------------------

-- | The forecast in words: what the present recipe would do on the
-- | present machine, in red when the shot would not come out right.
forecastLine :: forall w i. State -> HH.HTML w i
forecastLine st =
  let
    f = st.forecast
    wanted = if st.recipe.paced then ", aiming for " <> n1 st.recipe.seconds <> " s" else ""
    reached = f.outcome == Just StoppedByWeight
    -- Pacing at its limit means pressure cannot make up the difference,
    -- and the grinder has to.
    held = isLocked st Seconds || isLocked st Ratio
    grind =
      -- One piece of advice at a time: the cup first, then the grind.
      -- When the machine has found a change that mends the cup, the
      -- advice line under this one names it.
      if cup /= Cup.Balanced then
        (if held && isNothing st.advice then " The held time or yield does not suit this roast: free one and turn the drum again." else "")
      else if not f.paceSpent then ""
      else if f.paceBar > 0.0 then " Pacing is adding all the pressure it may: grind coarser."
      else " Pacing is taking off all the pressure it may: grind finer."
    cup = Cup.taste f.extracted
    good = cup == Cup.Balanced && not (Cup.channeled f.brewed)
    text = case f.refused of
      Just reason -> "The machine would refuse this shot: " <> reason <> "."
      Nothing ->
        if reached then
          "Forecast: " <> n1 f.grams <> " g in " <> n1 f.seconds <> " s" <> wanted <> ", peak " <> n1 f.peakBar <> " bar, puck evenness " <> n2 f.brewed.evenness <> ". " <> Cup.cupNote f.extracted f.brewed <> grind
        else
          "Forecast: the shot does not reach its weight. It ends after " <> n1 f.seconds <> " s with " <> n1 f.grams <> " g."
  in
    HH.p [ cls (if reached && good && not f.paceSpent then "forecast" else "forecast bad") ] [ HH.text text ]

-- | A key that asks the machine what it would change to mend the
-- | forecast cup, and once asked, the answer with a key that makes the
-- | change.
adviceLine :: forall w. State -> Array (HH.HTML w Action)
adviceLine st = case st.advice of
  Nothing ->
    if Advice.needed st.forecast then [ HH.div [ cls "choices" ] [ button "Advise" (Change askAdvice) ] ]
    else []
  Just advice ->
    [ HH.p [ cls "forecast advice" ]
        [ HH.text ("Advice: " <> advice.text <> " ")
        , button "Do that" (Change takeAdvice)
        ]
    ]

-- | A forecast of the shot with each step marked on it, and a list of
-- | one-line steps. One step at a time is open for editing, and the
-- | forecast is drawn again after every change.
designer :: forall w. State -> Array (HH.HTML w Action)
designer st =
  let
    shape = baseShape st
    groupC = shape.groupBaseC + tempShift st
    steps = NEA.toArray shape.steps
    count = length steps
    open = clamp 0 (count - 1) st.stepOpen
    -- The shot as the machine runs it has the recipe's soak in it, so
    -- the plot takes its step names from there.
    run = toProfile (allShapes st) st.recipe
    made = Flavor.character st.recipe.tempC st.forecast.brewed
  in
    [ HH.div [ cls "field wide" ]
        [ HH.label_ [ HH.text "Name" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.value shape.name
            , HE.onValueInput \text -> Change (editShape _ { name = text })
            ]
        ]
    , HH.p [ cls "note" ]
        [ HH.text
            ( if isCustom st then "Yours. Changes are saved as you make them."
              else "Built in. Changing anything makes a copy and edits the copy."
            )
        ]
    -- The shot beside the cup it makes, so a change to the curve shows
    -- at once in where the cup lands.
    , HH.div [ cls "design-top" ]
        [ forecastPlot
            { forecast: st.forecast
            , names: map _.name (NEA.toArray run.steps)
            , open: shownStep st.recipe open
            , goalSeconds: if st.recipe.paced then Just st.recipe.seconds else Nothing
            , goalGrams: targetGrams st.recipe
            }
        , wheel { place: Flavor.place st.recipe.roast st.forecast.extracted, character: made, wanted: st.wanted }
        ]
    , HH.p [ cls "note" ]
        [ HH.text
            ( "This cup: " <> joinWith ", " (map _.name (take 4 (Flavor.expected st.recipe.roast st.forecast.extracted made))) <> ". "
                <> Flavor.characterNote made
                <> case st.wanted of
                  Just note -> " " <> Flavor.routeNote st.recipe.roast note (Flavor.route st.recipe.roast st.forecast.extracted note)
                  Nothing -> ""
            )
        ]
    , HH.p [ cls "legend" ]
        [ legend "bar" "pressure, bar (0 to 12)"
        , legend "target" "pressure asked for"
        , legend "flow" "flow, ml/s (0 to 12)"
        , legend "grams" "in cup, g"
        , legend "goal" "where the recipe wants to end"
        ]
    , forecastLine st
    ]
      <> adviceLine st
      <>
    [ HH.div [ cls "steps" ] (mapWithIndex (stepRow st open) steps)
    , HH.div [ cls "section" ]
        ( [ button "Add a step" (Change \s -> (editShape Shapes.addStep s) { stepOpen = count }) ]
            <> (if count > 1 then [ button "Remove this step" (Change (editShape (Shapes.removeStep open))) ] else [])
            <> (if isCustom st then [ button "Delete this profile" (Change deleteShape) ] else [])
        )
    ]
      <> case index steps open of
        Just step -> [ stepEditor st open step ]
        Nothing -> []
      <>
        [ HH.div [ cls "field" ]
            [ HH.label_ [ HH.text "Group block" ]
            , slider 70.0 98.0 0.5 groupC editGroupBase
            , numBox st "shape.group" 1 groupC editGroupBase
            , HH.span [ cls "unit" ] [ HH.text "C" ]
            ]
        ]

-- | A step in one line. Clicking it opens it for editing.
stepRow :: forall w. State -> Int -> Int -> Step -> HH.HTML w Action
stepRow st open i step =
  let
    what = case step.mode of
      LinePrebrew -> "line pressure"
      Pressure -> n1 (fromMaybe 9.0 step.targetBar) <> " bar"
      Flow -> n1 (fromMaybe 2.0 step.targetMlPerS) <> " ml/s"
    path =
      if step.curve == Jump || step.rampS <= 0.0 then "jump"
      else Curve.label step.curve <> " over " <> n1 step.rampS <> " s"
  in
    HH.button
      [ cls ("step-row" <> (if open == i then " active" else ""))
      , HE.onClick \_ -> Change _ { stepOpen = i }
      ]
      [ HH.span [ cls "step-number" ] [ HH.text (show (i + 1)) ]
      , HH.span [ cls "step-name" ] [ HH.text step.name ]
      , HH.span [ cls "step-what" ]
          [ HH.text (what <> ", " <> path <> ", " <> n1 (step.targetC + tempShift st) <> " C, up to " <> n1 step.maxDuration <> " s") ]
      ]

stepEditor :: forall w. State -> Int -> Step -> HH.HTML w Action
stepEditor st i step =
  let
    key name = "step." <> show i <> "." <> name
    edit change = editShape (Shapes.editStep i change)
    actualC = step.targetC + tempShift st
    number name title unit places lo hi stepSize value apply =
      HH.div [ cls "field" ]
        [ HH.label_ [ HH.text title ]
        , slider lo hi stepSize value apply
        , numBox st (key name) places value apply
        , HH.span [ cls "unit" ] [ HH.text unit ]
        ]
    target = case step.mode of
      Pressure ->
        [ number "bar" "Pressure" "bar" 1 0.5 10.5 0.1 (fromMaybe 9.0 step.targetBar)
            (\v -> edit _ { targetBar = Just v })
        ]
      Flow ->
        [ number "flow" "Flow" "ml/s" 1 0.2 8.0 0.1 (fromMaybe 2.0 step.targetMlPerS)
            (\v -> edit _ { targetMlPerS = Just v })
        ]
      LinePrebrew -> []
    exitBar = case step.exitAboveBar of
      Just bar ->
        [ number "exitBar" "Move on above" "bar" 1 0.2 10.5 0.1 bar (\v -> edit _ { exitAboveBar = Just v }) ]
      Nothing -> []
    exitGrams = case step.exitAboveGrams of
      Just grams ->
        [ number "exitGrams" "Move on above" "g" 1 0.5 90.0 0.5 grams (\v -> edit _ { exitAboveGrams = Just v }) ]
      Nothing -> []
  in
    HH.div [ cls "step" ]
      [ HH.div [ cls "field wide" ]
          [ HH.label_ [ HH.text ("Step " <> show (i + 1)) ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.value step.name
              , HE.onValueInput \text -> Change (edit _ { name = text })
              ]
          ]
      , HH.div [ cls "choices" ]
          [ segmented
              [ chosen (step.mode == LinePrebrew) "Line" (Change (edit _ { mode = LinePrebrew }))
              , chosen (step.mode == Pressure) "Pressure"
                  (Change (edit \s -> s { mode = Pressure, targetBar = Just (fromMaybe 9.0 s.targetBar) }))
              , chosen (step.mode == Flow) "Flow"
                  (Change (edit \s -> s { mode = Flow, targetMlPerS = Just (fromMaybe 2.0 s.targetMlPerS) }))
              ]
          , segmented
              ( map
                  ( \curve -> chosen (step.curve == curve) (Curve.label curve)
                      -- A curve needs time to show, so picking one on a
                      -- step with no ramp time gives it some.
                      (Change (edit \s -> s { curve = curve, rampS = if curve /= Jump && s.rampS <= 0.0 then 4.0 else s.rampS }))
                  )
                  Curve.all
              )
          ]
      , HH.div [ cls "fields" ]
          ( [ number "ramp" "Ramp time" "s" 1 0.0 60.0 0.5 step.rampS (\v -> edit _ { rampS = v }) ]
              <> target
              <>
                [ number "temp" "Water" "C" 1 70.0 98.0 0.5 actualC (editStepTemp i)
                , number "longest" "Longest" "s" 0 1.0 120.0 1.0 step.maxDuration (\v -> edit _ { maxDuration = v })
                ]
              <> exitBar
              <> exitGrams
          )
      , HH.div [ cls "choices" ]
          [ HH.span [ cls "unit" ] [ HH.text "Move on early" ]
          , chosen (isJust step.exitAboveBar) "on pressure"
              (Change (edit \s -> s { exitAboveBar = if isJust s.exitAboveBar then Nothing else Just 2.0 }))
          , chosen (isJust step.exitAboveGrams) "on weight"
              (Change (edit \s -> s { exitAboveGrams = if isJust s.exitAboveGrams then Nothing else Just 5.0 }))
          ]
      ]

------------------------------------------------------------
-- Scope
------------------------------------------------------------

scope :: forall w. State -> Array (HH.HTML w Action)
scope st =
  let
    m = st.machine
    c = m.status
    s = m.sensors
    cfg = st.params.config
    budget = toNumber cfg.circuitBudgetWatts
  in
    [ scopePlot budget m.scope
    , HH.p [ cls "legend" ]
        [ legend "reservoir" "reservoir, C (0 to 125)"
        , legend "group" "group block, C (0 to 125)"
        , legend "steam" "steam, bar (0 to 2.5)"
        , legend "watts" ("heater draw, W (0 to " <> show cfg.circuitBudgetWatts <> ")")
        ]
    , table
        [ row "Now" (n1 s.brewBoilerC <> " C, " <> n1 s.groupC <> " C, " <> n1 s.steamBar <> " bar, " <> show c.heaterWatts <> " W")
        , row "Window" "the last 30 minutes of the machine's clock"
        ]
    , HH.p [ cls "note" ] [ HH.text "What the present physics implies. Change the physics under Settings and these move with it." ]
    , table (map (\reading -> row reading.label reading.value) (Params.readings st.params))
    ]

------------------------------------------------------------
-- Settings
------------------------------------------------------------

-- | One group of parameters at a time.
settings :: forall w. State -> Array (HH.HTML w Action)
settings st =
  [ HH.div [ cls "choices" ]
      [ segmented
          ( map
              (\which -> chosen (st.paramGroup == which) (groupShort which) (Change _ { paramGroup = which }))
              Params.groups
          )
      , button "Reset all to defaults" (Change resetParams)
      ]
  , HH.p [ cls "note" ] [ HH.text (Params.groupNote st.paramGroup <> " Changes apply at once and are remembered.") ]
  ]
    <> map (entryRow st) (Params.inGroup st.paramGroup)

groupShort :: Group -> String
groupShort = case _ of
  Loops -> "Loops"
  Calibration -> "Calibration"
  Limits -> "Limits"
  Physics -> "Physics"

entryRow :: forall w. State -> Entry -> HH.HTML w Action
entryRow st entry =
  let
    params = st.params
    value = entry.get params
    apply v = changeParams (entry.set (clamp entry.min entry.max v))
    changed = value /= entry.get Params.defaults
  in
    HH.div [ cls ("param" <> (if changed then " changed" else "")) ]
      [ HH.label [ HP.title entry.key ] [ HH.text entry.label ]
      , slider entry.min entry.max entry.step value apply
      , numBox st ("param." <> entry.key) (Params.decimals entry) value apply
      , HH.span [ cls "unit" ] [ HH.text entry.unit ]
      ]
