-- | What the machine pane, the brew tab and the bench's problem and
-- | reading tabs show.
module Sim.View.Panels (machineView, brew, faults, readings) where

import Prelude

import Data.Array (last, mapWithIndex, null, reverse, take)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sim.Cup as Cup
import Sim.Controller (Event(..), heaterDrawWatts, shotInProgress)
import Sim.Invariants (checks)
import Sim.Model (Action(..), Field(..), State)
import Sim.Problems (Problems, anyProblem)
import Sim.Recipe (Recipe, builtIn, roastLabel, roastNote, roasts, targetGrams, withRoastDefaults)
import Sim.Types (ClientKind(..), FaultKind(..), PowerMode(..), Shot, StepMode(..))
import Sim.Update (allShapes, applyRecipe, chooseShape, fieldKey, limitsOf, setField, valueOf)
import Sim.View.Bench (forecastLine)
import Sim.View.Chart (chart)
import Sim.View.Front (front)
import Sim.View.Inside (inside)
import Sim.View.Widgets (button, chosen, cls, clock, legend, n1, n2, numBox, primary, row, rowIf, section, segmented, slider, table, toggle)
import Sim.World (World, sensors)

onOff :: Boolean -> String
onOff b = if b then "on" else "off"

------------------------------------------------------------
-- Machine pane
------------------------------------------------------------

-- | The front of the machine beside its inside, with the shot under
-- | both.
machineView :: forall w. State -> Problems -> Array (HH.HTML w Action)
machineView st wrong =
  let
    w = st.world
    c = w.ctl
    s = sensors w
  in
    [ nowPlaying st
    , HH.div [ cls "views" ]
        [ front
            { now: w.now
            , state: c.state
            , brewBar: s.brewBar
            , steamBar: s.steamBar
            , reservoirC: s.brewBoilerC
            , groupC: s.groupC
            , waterC: s.groupWaterC
            , cupGrams: s.cupGrams
            , targetGrams: targetGrams st.recipe
            , brewElementOn: c.act.brewElementOn
            , steamElementOn: c.act.steamElementOn
            , groupHeaterOn: c.act.groupHeaterOn
            , pouring: shotInProgress c && s.cupGrams > 0.05 && s.brewBar > 0.3
            , steaming: w.dist.steamValveOpen && s.steamBar > 0.2
            , paddle: case c.shot of
                Nothing -> 0
                Just shot ->
                  if shot.step.mode == LinePrebrew || (shot.stepIndex == 0 && NEA.length shot.profile.steps > 1) then 1
                  else 2
            , problem: anyProblem wrong
            , flowMlPerS: w.plant.flowMlPerS
            , streamColour: Cup.streamColour st.recipe.roast w.plant.extracted w.plant.flowMlPerS
            }
        , inside
            { now: w.now
            , act: c.act
            , flowMlPerS: w.plant.flowMlPerS
            , reservoirC: s.brewBoilerC
            , mixC: s.mixC
            , steamBar: s.steamBar
            , steamWaterMl: w.plant.steamWaterMl
            , steamValveOpen: w.dist.steamValveOpen
            , problems: wrong
            }
        ]
    , HH.p [ cls "shot-line" ] [ HH.text (shotLine w) ]
    , chart w.trace
    , HH.p [ cls "legend" ]
        [ legend "bar" "pressure, bar (0 to 12)"
        , legend "flow" "flow, ml/s (0 to 12)"
        , legend "water" "water above screen, C (80 to 100)"
        , legend "target" "target, C (80 to 100)"
        , legend "grams" "in cup, g (0 to 60)"
        ]
    ]

------------------------------------------------------------
-- Controls pane, brew tab
------------------------------------------------------------

brew :: forall w. State -> Array (HH.HTML w Action)
brew st =
  let
    w = st.world
    r = st.recipe
  in
    [ section "Paddle"
        [ primary "Start shot" (Send ShotRequested)
        , button "Next step" (Send StepAdvanceRequested)
        , button "Stop" (Send ShotStopRequested)
        , toggle "Steam valve" w.dist.steamValveOpen (Disturb \d -> d { steamValveOpen = not d.steamValveOpen })
        ]
    , section "Power"
        [ button "Wake, brew only" (Send (WakeRequested BrewReady))
        , button "Wake, brew and steam" (Send (WakeRequested Full))
        , button "Sleep" (Send SleepRequested)
        ]
    , section "Time"
        [ segmented (map (\speed -> chosen (st.speed == speed) (show speed <> "x") (SetSpeed speed)) [ 1, 10, 60 ])
        , button "Skip warm-up" SkipWarmUp
        , button "Reset" Reset
        ]
    , section "Beans" (map presetButton builtIn <> map savedPreset st.saved)
    , HH.div [ cls "fields" ]
        [ textRow "Bean" "name of the coffee" r.name (\text -> Change _ { recipe = r { name = text } })
        , textRow "Roaster" "who roasted it" r.roaster (\text -> Change _ { recipe = r { roaster = text } })
        , textRow "Roasted on" "date on the bag" r.roastedOn (\text -> Change _ { recipe = r { roastedOn = text } })
        ]
    , HH.div [ cls "choices" ]
        (map (\roast -> chosen (r.roast == roast) (roastLabel roast) (Change _ { recipe = r { roast = roast } })) roasts)
    , HH.div [ cls "choices" ]
        [ button "Use this roast's starting point" (Change \s -> applyRecipe (withRoastDefaults s.recipe) s { draft = Nothing })
        , button "Save these beans" SavePreset
        ]
    , HH.p [ cls "note" ] [ HH.text (roastNote r.roast) ]
    , HH.div [ cls "fields" ]
        [ fieldRow st Dose "Beans in" "g" 1
        , fieldRow st Ratio "Ratio, 1 to" "" 2
        , fieldRow st Target "In the cup" "g" 1
        , fieldRow st Temp "Water" "C" 1
        , fieldRow st Seconds "Shot time" "s" 0
        , fieldRow st Grind "Grind, coarser" "steps" 1
        ]
    , HH.div [ cls "choices" ]
        [ toggle "Machine paces the shot to this time" r.paced (Change \s -> applyRecipe s.recipe { paced = not s.recipe.paced } s) ]
    , forecastLine st
    , section "Profile"
        [ segmented
            ( mapWithIndex
                (\i shape -> chosen (st.recipe.profileIndex == i) shape.name (Change (chooseShape i)))
                (allShapes st)
            )
        ]
    ]

-- | The beans in use, shown over the machine while it is in use.
nowPlaying :: forall w i. State -> HH.HTML w i
nowPlaying st =
  let
    r = st.recipe
    said text = if text == "" then [] else [ text ]
    details = said r.roaster <> [ roastLabel r.roast <> " roast" ] <> (if r.roastedOn == "" then [] else [ "roasted " <> r.roastedOn ])
    cupNote =
      if shotInProgress st.world.ctl || null st.world.ctl.history then ""
      else Cup.tasteNote (Cup.taste st.world.plant.extracted)
  in
    HH.div [ cls "now-playing" ]
      [ HH.span [ cls "now-label" ] [ HH.text "Now brewing" ]
      , HH.span [ cls "now-bean" ] [ HH.text (if r.name == "" then "Unnamed beans" else r.name) ]
      , HH.span [ cls "now-details" ] [ HH.text (joinWith ", " details) ]
      , HH.span [ cls "now-cup" ] [ HH.text cupNote ]
      ]

presetButton :: forall w. Recipe -> HH.HTML w Action
presetButton recipe =
  HH.button
    [ HE.onClick \_ -> Change (\st -> applyRecipe recipe st { draft = Nothing }) ]
    [ HH.text recipe.name ]

savedPreset :: forall w. Recipe -> HH.HTML w Action
savedPreset recipe =
  HH.span [ cls "saved" ]
    [ presetButton recipe
    , HH.button [ HE.onClick \_ -> DeletePreset recipe.name, cls "remove" ] [ HH.text "remove" ]
    ]

-- | A line of text about the beans.
textRow :: forall w. String -> String -> String -> (String -> Action) -> HH.HTML w Action
textRow title hint value act =
  HH.div [ cls "field wide" ]
    [ HH.label_ [ HH.text title ]
    , HH.input [ HP.type_ HP.InputText, HP.placeholder hint, HP.value value, HE.onValueInput act ]
    ]

-- | One recipe setting: a slider for coarse moves and a box for an exact
-- | number. Both write the same value.
fieldRow :: forall w. State -> Field -> String -> String -> Int -> HH.HTML w Action
fieldRow st which title unit places =
  let
    limits = limitsOf which
    value = valueOf which st.recipe
  in
    HH.div [ cls "field" ]
      [ HH.label_ [ HH.text title ]
      , slider limits.min limits.max limits.step value (setField which)
      , numBox st (fieldKey which) places value (setField which)
      , HH.span [ cls "unit" ] [ HH.text unit ]
      ]

------------------------------------------------------------
-- Bench pane, problems tab
------------------------------------------------------------

-- | What is wrong now, the switches that make things go wrong, the
-- | service buttons, the machine's own log and the spec invariants.
faults :: forall w. State -> Problems -> Array (HH.HTML w Action)
faults st wrong =
  let
    w = st.world
    c = w.ctl
    s = sensors w
  in
    [ if anyProblem wrong then HH.ul [ cls "problems" ] (map (\text -> HH.li_ [ HH.text text ]) wrong.lines)
      else HH.p [ cls "note" ] [ HH.text "Nothing is wrong." ]
    , section "Make something go wrong"
        [ toggle "Water supply cut off" (not w.dist.inletOpen) (Disturb \d -> d { inletOpen = not d.inletOpen })
        , toggle "Cold branch blocked" w.dist.coldBranchBlocked (Disturb \d -> d { coldBranchBlocked = not d.coldBranchBlocked })
        , toggle "Hard water at inlet" w.dist.hardWater (Disturb \d -> d { hardWater = not d.hardWater })
        , button "Break a sensor" (Send (SensorFailed SensorOpen))
        ]
    , section "Service"
        [ button "Clear faults" (Send (FaultsCleared Service))
        , button "Confirm blowdown" (Send BlowdownConfirmed)
        , button "Acknowledge water warning" (Send WaterWarningAcknowledged)
        ]
    , section "What is happening" []
    , if null w.happenings then HH.p [ cls "note" ] [ HH.text "Nothing yet. Wake the machine to begin." ]
      else HH.ul [ cls "log" ]
        ( map
            (\h -> HH.li [ cls (if h.bad then "bad" else "") ] [ HH.text (clock h.at <> ": " <> h.text) ])
            (reverse w.happenings)
        )
    , table
        [ row "Fills since blowdown" (show c.fillsSinceBlowdown)
        , row "Idle sleep after" (clock c.cfg.idleSleepAfter <> " without use")
        , rowIf (not (null c.faults)) "Faults"
            ( if null c.faults then "none"
              else joinWith ", " (map (\f -> show f.kind <> maybe " (active)" (const " (cleared)") f.clearedAt) c.faults)
            )
        ]
    , section "Shots" []
    , if null c.history then HH.p [ cls "note" ] [ HH.text "No shot yet." ]
      else HH.ul [ cls "log" ] (map shotItem (take 5 (reverse c.history)))
    , section "Spec invariants" []
    , table (map (\k -> rowIf (not k.holds) k.name (if k.holds then "holds" else "BROKEN")) (checks s c))
    , HH.p [ cls (if null w.violations then "note" else "bad") ]
        [ HH.text
            ( if null w.violations then "No invariant has been broken in this session."
              else "Broken at some point: " <> joinWith ", " w.violations
            )
        ]
    ]

------------------------------------------------------------
-- Bench pane, readings tab
------------------------------------------------------------

readings :: forall w. State -> Array (HH.HTML w Action)
readings st =
  let
    w = st.world
    c = w.ctl
    s = sensors w
  in
    [ section "Sensors" []
    , table
        [ row "Brew reservoir" (n1 s.brewBoilerC <> " C, target " <> n1 c.cfg.reservoirTargetC)
        , row "Group block" (n1 s.groupC <> " C, target " <> n1 c.groupTargetC)
        , row "Mix outlet" (n1 s.mixC <> " C" <> maybe "" (\t -> ", target " <> n1 t) c.mixTargetC)
        , row "Water above screen" (n1 s.groupWaterC <> " C")
        , row "Inlet" (n1 s.inletC <> " C, " <> n1 s.inletBar <> " bar, " <> n1 s.inletConductivityUs <> " uS/cm")
        , row "Brew pressure" (n1 s.brewBar <> " bar" <> maybe "" (\t -> ", target " <> n1 t) c.pressureTargetBar)
        , row "Flow into group" (n2 w.plant.flowMlPerS <> " ml/s" <> maybe "" (\t -> ", target " <> n2 t) c.flowTargetMlPerS)
        , rowIf (not s.steamLevelOk) "Steam" (n2 s.steamBar <> " bar, level " <> (if s.steamLevelOk then "ok" else "low"))
        , row "Cup" (n1 s.cupGrams <> " g")
        ]
    , section "Actuators" []
    , table
        [ row "Heat enable" (onOff c.act.heatEnabled)
        , row "Brew element" (onOff c.act.brewElementOn)
        , row "Steam element" (onOff c.act.steamElementOn)
        , row "Group heaters" (onOff c.act.groupHeaterOn)
        , row "Heater draw" (show (heaterDrawWatts c.cfg c.act) <> " W of " <> show c.cfg.circuitBudgetWatts)
        , row "Pump" (show c.act.pump <> ", speed " <> n2 c.act.pumpSpeed)
        , row "Bypass valve SV_BYP" (show c.act.bypassValve)
        , row "Fill valve SV_FILL" (show c.act.fillValve)
        , row "Group valve SV_GRP" (show c.act.groupValve)
        , row "Mix valve MV_MIX" (show c.act.mixValve <> ", position " <> n2 c.act.mixPosition)
        ]
    ]

------------------------------------------------------------
-- Shots in words
------------------------------------------------------------

shotLine :: World -> String
shotLine w = case w.ctl.shot of
  Just shot ->
    "Running " <> shot.profile.name
      <> ", step " <> show (shot.stepIndex + 1) <> " of " <> show (NEA.length shot.profile.steps)
      <> " (" <> shot.step.name <> ", " <> show shot.step.mode <> ", " <> n1 shot.step.targetC <> " C)"
      <> ", " <> n1 (w.now - shot.startedAt) <> " s"
  Nothing -> case last w.ctl.history of
    Just shot -> "Last shot: " <> describeShot shot
    Nothing -> "No shot yet."

describeShot :: Shot -> String
describeShot shot =
  shot.profile.name
    <> ", " <> show shot.state
    <> ", " <> n1 (fromMaybe shot.startedAt shot.endedAt - shot.startedAt) <> " s"
    <> maybe "" (\grams -> ", " <> n1 grams <> " g") shot.finalGrams

shotItem :: forall w i. Shot -> HH.HTML w i
shotItem shot = HH.li_ [ HH.text (describeShot shot) ]
