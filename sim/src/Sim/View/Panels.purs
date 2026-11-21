-- | What the machine pane, the brew tab and the bench's problem and
-- | reading tabs show.
module Sim.View.Panels (machineView, brew, cup, faults, readings) where

import Prelude

import Data.Array (findIndex, index, last, length, mapWithIndex, null, reverse, take)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number as Number
import Data.Number.Format (fixed, toStringWith)
import Data.String (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sim.Cup as Cup
import Sim.Flavor as Flavor
import Sim.Link (Event(..), FaderMode(..), faderScale)
import Sim.Machine (Machine, silence)
import Sim.Invariants (checks)
import Sim.Layout (Edge(..), upright)
import Sim.Model (Action(..), Field(..), Play(..), State, Theme(..))
import Sim.Problems (Problems, anyProblem)
import Sim.Recipe (Recipe, Roast, roastLabel, roastNote, roasts, samples, targetGrams)
import Sim.Types (ClientKind(..), FaultKind(..), PowerMode(..), Shot)
import Sim.Update (allShapes, applyRecipe, chooseRoast, chooseShape, fieldKey, findGrind, isLocked, limitsOf, faderReading, playheadAt, setField, simView, toggleLock, valueOf)
import Sim.View.Bench (adviceLine, forecastLine)
import Sim.View.Chart (chart)
import Sim.View.Front (front)
import Sim.View.Playhead (inkOf, playhead)
import Sim.View.Inside (inside)
import Sim.View.Wheel (wheel)
import Sim.View.Widgets (button, chosen, cls, clock, legend, n1, n2, numBox, primary, row, rowIf, section, segmented, slider, table, toggle)

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
    m = st.machine
    c = m.status
    s = m.sensors
    sim = simView st
    scale = faderScale c.faderMode
    held = isJust (c.shot >>= _.manual) || isJust (c.shot >>= _.manualC)
    recent = Array.takeEnd 6 m.trace
  in
    [ nowPlaying st
    , HH.div [ cls "views" ]
        [ HH.div [ cls "front-cell" ]
            [ HH.div [ cls "fader-wrap" ]
            [ front
            { now: m.at
            , night: st.theme == Night
            , arcade: st.play == Arcade
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
            , pouring: isJust c.shot && s.cupGrams > 0.05 && s.brewBar > 0.3
            , steaming: sim.dist.steamValveOpen && s.steamBar > 0.2
            , steamOpen: sim.dist.steamValveOpen
            , fader: (faderReading st - scale.low) / (scale.high - scale.low)
            , mode: c.faderMode
            , faderValue: case c.faderMode of
                OnPressure -> n1 (faderReading st) <> " bar"
                OnTemperature -> n1 (faderReading st) <> " C"
                OnTime -> n1 (playheadAt st) <> " s"
            , ink: inkOf st
            , running: isJust c.shot
            , manual: held
            , problem: anyProblem wrong
            -- How fast coffee is reaching the cup, in grams a second,
            -- over the last few samples of the shot.
            , outFlow: case c.shot, Array.head recent, Array.last recent of
                Just _, Just first, Just latest | latest.t > first.t -> max 0.0 ((latest.grams - first.grams) / (latest.t - first.t))
                _, _, _ -> 0.0
            -- The colour of the stream is read off the simulated puck.
            -- A real machine cannot see it.
            , streamColour: Cup.streamColour st.recipe.roast sim.puck.extracted sim.puck.evenness c.flowMlPerS
            }
            -- The fader itself: a slider laid over the slot in the
            -- drawing, on the scale the selector has chosen. Left alone
            -- it shows where the machine's motor has put it. On time it
            -- cannot be taken in hand.
            , HH.input
                [ HP.type_ HP.InputRange
                , cls ("fader" <> (if held then " by-hand" else "") <> (if c.faderMode == OnTime then " shows-only" else ""))
                , HP.title case c.faderMode of
                    OnPressure -> "The fader: pressure, in bar"
                    OnTemperature -> "The fader: water temperature, in degrees C"
                    OnTime -> "The fader: how far the shot has run. It only shows."
                , HP.disabled (c.faderMode == OnTime)
                , HP.min scale.low
                , HP.max scale.high
                , HP.step (HP.Step ((scale.high - scale.low) / 200.0))
                , HP.value (show (faderReading st))
                , HE.onValueInput \text -> FaderDrag (fromMaybe 0.0 (Number.fromString text))
                , HE.onValueChange \_ -> FaderDrop
                , HE.onMouseUp \_ -> FaderDrop
                ]
            ]
            ]
        , edge FrontEdge
        , inside
            { now: m.at
            , act: c.act
            , flowMlPerS: c.flowMlPerS
            , reservoirC: s.brewBoilerC
            , mixC: s.mixC
            , steamBar: s.steamBar
            , steamWaterMl: sim.steamWaterMl
            , steamValveOpen: sim.dist.steamValveOpen
            , problems: wrong
            }
        ]
    , edge DrawingsEdge
    , playhead st
    , HH.p [ cls "shot-line" ] [ HH.text (shotLine m) ]
    , chart m.trace
    , edge ChartEdge
    , HH.p [ cls "legend" ]
        [ legend "bar" "pressure, bar (0 to 12)"
        , legend "flow" "flow, ml/s (0 to 12)"
        , legend "water" "water above screen, C (80 to 100)"
        , legend "target" "target, C (80 to 100)"
        , legend "grams" "in cup, g (0 to 60)"
        ]
    ]

-- | An edge between two things in the machine pane, to drag.
edge :: forall w. Edge -> HH.HTML w Action
edge which =
  HH.div
    [ cls ("gutter" <> (if upright which then "" else " flat"))
    , HP.title "Drag to resize"
    , HE.onMouseDown (GutterDown which)
    ]
    []

------------------------------------------------------------
-- Controls pane, brew tab
------------------------------------------------------------

brew :: forall w. State -> Array (HH.HTML w Action)
brew st =
  let
    sim = simView st
    r = st.recipe
  in
    [ HH.div [ cls "choices" ]
        [ primary "Start shot" (Send ShotRequested)
        , button "Next step" (Send StepAdvanceRequested)
        , button "Stop" (Send ShotStopRequested)
        , toggle "Steam" sim.dist.steamValveOpen (Disturb \d -> d { steamValveOpen = not d.steamValveOpen })
        ]
    , section "Beans" (map presetButton samples <> map savedPreset st.saved)
    , HH.div [ cls "bean" ]
        [ roastDrum r.roast
        , HH.div [ cls "fields" ]
            [ textRow "Bean" "name of the coffee" r.name (\text -> Change _ { recipe = r { name = text } })
            , textRow "Roaster" "who roasted it" r.roaster (\text -> Change _ { recipe = r { roaster = text } })
            , textRow "Roasted" "date on the bag" r.roastedOn (\text -> Change _ { recipe = r { roastedOn = text } })
            ]
        ]
    , HH.p [ cls "note" ] [ HH.text (roastNote r.roast) ]
    , section "Recipe" []
    , HH.div [ cls "fields" ]
        [ fieldRow st Dose "Beans in" "g" 1
        , fieldRow st Target "In the cup" "g" 1
        , fieldRow st Seconds "Shot time" "s" 0
        , fieldRow st Temp "Water" "C" 1
        , fieldRow st Grind "Grind" "coarser" 1
        , fieldRow st Soak "Soak" "s" 0
        ]
    , forecastLine st
    ]
      <> adviceLine st
      <>
    [ HH.div [ cls "choices" ]
        ( [ toggle "Pace to time" r.paced (Change \s -> applyRecipe s.recipe { paced = not s.recipe.paced } s) ]
            <> (if isLocked st Grind then [] else [ button "Find the grind" (Change \s -> applyRecipe (findGrind s s.recipe) s { draft = Nothing }) ])
            <> [ button "Save these beans" SavePreset ]
        )
    , section "Profile"
        [ segmented
            ( mapWithIndex
                (\i shape -> chosen (st.recipe.profileIndex == i) shape.name (Change (chooseShape i)))
                (allShapes st)
            )
        ]
    , section "Machine"
        ( [ button "Wake, brew" (Send (WakeRequested BrewReady))
          , button "Wake, brew and steam" (Send (WakeRequested Full))
          , button "Sleep" (Send SleepRequested)
          , segmented (map (\speed -> chosen (sim.speed == speed) (show speed <> "x") (SetSpeed speed)) [ 1, 10, 60 ])
          ]
            -- Only the simulator can skip a warm-up, so realistic play
            -- does not offer it.
            <> (if st.play == Arcade then [ button "Skip warm-up" SkipWarmUp ] else [])
            <> [ button "Reset" Reset ]
        )
    ]

-- | The beans in use, shown over the machine while it is in use.
nowPlaying :: forall w i. State -> HH.HTML w i
nowPlaying st =
  let
    r = st.recipe
    said text = if text == "" then [] else [ text ]
    details = said r.roaster <> [ roastLabel r.roast <> " roast" ] <> (if r.roastedOn == "" then [] else [ "roasted " <> r.roastedOn ])
    cupNote =
      if isJust st.machine.shot || null st.machine.history then ""
      else Cup.cupNote sim.puck.extracted sim.brewed
    sim = simView st
  in
    HH.div [ cls "now-playing" ]
      [ HH.span [ cls "now-label" ] [ HH.text "Now brewing" ]
      , HH.span [ cls "now-bean" ] [ HH.text (if r.name == "" then "Unnamed beans" else r.name) ]
      , HH.span [ cls "now-details" ] [ HH.text (joinWith ", " details) ]
      , HH.span [ cls "now-cup" ] [ HH.text cupNote ]
      ]

-- | The roast as a drum: the chosen level in the window with its
-- | neighbours turning away above and below. Clicking a neighbour turns
-- | the drum to it.
roastDrum :: forall w. Roast -> HH.HTML w Action
roastDrum now =
  let
    here = fromMaybe 0 (findIndex (_ == now) roasts)
    face offset = case index roasts (here + offset) of
      Just roast ->
        HH.button
          [ cls ("drum-face at" <> show (offset + 2))
          , HE.onClick \_ -> Change (chooseRoast roast)
          ]
          [ HH.text (roastLabel roast) ]
      Nothing -> HH.span [ cls "drum-face blank" ] []
  in
    HH.div [ cls "drum", HP.title "Degree of roast" ] (map face [ -2, -1, 0, 1, 2 ])

------------------------------------------------------------
-- Controls pane, cup tab
------------------------------------------------------------

-- | The coffee map: where the forecast cup lands, the notes it should
-- | show, and how to get to a note that is wanted.
cup :: forall w. State -> Array (HH.HTML w Action)
cup st =
  let
    r = st.recipe
    extracted = st.forecast.extracted
    made = Flavor.character r.tempC st.forecast.brewed
    shows = take 4 (Flavor.expected r.roast extracted made)
    b = st.forecast.brewed
  in
    [ section "The coffee map" []
    , wheel { place: Flavor.place r.roast extracted, character: made, wanted: st.wanted }
    , HH.p [ cls "note" ]
        [ HH.text "Outlined notes are what the forecast cup should show. The mark on the outer band is where it lands between light and dark. The mark on the spoke under it is the cup's weight: towards the rim is sweet and clear, towards the centre is heavy in body. An uneven puck outlines fewer notes. Click a note to ask how to reach it." ]
    , section "This cup" []
    , HH.p_
        [ HH.text
            ( (if r.name == "" then "These beans" else r.name) <> ", " <> roastLabel r.roast <> " roast: "
                <> joinWith ", " (map _.name shows) <> ". " <> Cup.cupNote extracted b
            )
        ]
    , HH.p_ [ HH.text (Flavor.characterNote made) ]
    , table
        [ row "Extracted" (n2 extracted <> " of what the coffee can give (balanced from 0.60 to 0.75)")
        , row "Puck evenness" (n2 b.evenness <> " (1 is even, under 0.80 has channeled)")
        , row "Dissolved at" (n1 b.atC <> " C in the puck, " <> n1 b.atBar <> " bar, on average")
        ]
    , section "The cup you want" []
    , HH.p_
        [ HH.text case st.wanted of
            Just note -> Flavor.routeNote r.roast note (Flavor.route r.roast extracted note)
            Nothing -> "No note chosen."
        ]
    , forecastLine st
    ]
      <> adviceLine st

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

-- | One recipe setting: a slider for coarse moves, a box for an exact
-- | number, and a key that holds it. A held setting shows as plain text
-- | and the machine's suggestions leave it alone.
fieldRow :: forall w. State -> Field -> String -> String -> Int -> HH.HTML w Action
fieldRow st which title unit places =
  let
    limits = limitsOf which
    value = valueOf which st.recipe
    held = isLocked st which
    hold =
      HH.button
        [ cls ("hold" <> (if held then " active" else ""))
        , HP.title (if held then "Held. Click to let the machine suggest it." else "Free. Click to hold it.")
        , HE.onClick \_ -> Change (toggleLock which)
        ]
        [ HH.text (if held then "held" else "free") ]
  in
    if held then
      HH.div [ cls "field" ]
        [ HH.label_ [ HH.text title ]
        , HH.span [ cls "held-rule" ] []
        , HH.span [ cls "exact held-value" ] [ HH.text (toStringWith (fixed places) value) ]
        , HH.span [ cls "unit" ] [ HH.text unit ]
        , hold
        ]
    else
      HH.div [ cls "field" ]
        [ HH.label_ [ HH.text title ]
        , slider limits.min limits.max limits.step value (setField which)
        , numBox st (fieldKey which) places value (setField which)
        , HH.span [ cls "unit" ] [ HH.text unit ]
        , hold
        ]

------------------------------------------------------------
-- Bench pane, problems tab
------------------------------------------------------------

-- | What is wrong now, the switches that make things go wrong, the
-- | service buttons, the machine's own log and the spec invariants.
faults :: forall w. State -> Problems -> Array (HH.HTML w Action)
faults st wrong =
  let
    m = st.machine
    c = m.status
    s = m.sensors
    sim = simView st
    link = sim.link
  in
    [ if anyProblem wrong then HH.ul [ cls "problems" ] (map (\text -> HH.li_ [ HH.text text ]) wrong.lines)
      else HH.p [ cls "note" ] [ HH.text "Nothing is wrong." ]
    , section "Make something go wrong"
        [ toggle "Water supply cut off" (not sim.dist.inletOpen) (Disturb \d -> d { inletOpen = not d.inletOpen })
        , toggle "Cold branch blocked" sim.dist.coldBranchBlocked (Disturb \d -> d { coldBranchBlocked = not d.coldBranchBlocked })
        , toggle "Hard water at inlet" sim.dist.hardWater (Disturb \d -> d { hardWater = not d.hardWater })
        , button "Break a sensor" (Send (SensorFailed SensorOpen))
        ]
    -- The link between the page and the machine. Each way takes the
    -- delay, so an answer takes twice as long.
    , section "The link to the machine"
        [ toggle "Unplugged" link.cut (SetLink \l -> l { cut = not l.cut })
        , segmented
            ( map
                (\ms -> chosen (link.delayMs == ms) (toStringWith (fixed 0) ms <> " ms") (SetLink _ { delayMs = ms }))
                [ 0.0, 60.0, 250.0, 1000.0 ]
            )
        , toggle "Drop every third report" (link.dropEvery == 3) (SetLink \l -> l { dropEvery = if l.dropEvery == 3 then 0 else 3 })
        ]
    , table
        [ row "Last heard" (if m.heard then n1 (silence st.clock m / 1000.0) <> " s ago" else "never")
        , row "The machine's clock, as last heard" (clock m.at)
        , row "The machine's clock, in truth" (clock sim.now <> " (simulation only)")
        , row "Requests awaiting an answer" (show (length m.pending))
        , row "Messages in flight" (show sim.inFlight)
        ]
    , section "Service"
        [ button "Clear faults" (Send (FaultsCleared Service))
        , button "Confirm blowdown" (Send BlowdownConfirmed)
        , button "Acknowledge water warning" (Send WaterWarningAcknowledged)
        ]
    , section "What is happening" []
    , if null m.happenings then HH.p [ cls "note" ] [ HH.text "Nothing yet. Wake the machine to begin." ]
      else HH.ul [ cls "log" ]
        ( map
            (\h -> HH.li [ cls (if h.bad then "bad" else "") ] [ HH.text (clock h.at <> ": " <> h.text) ])
            (reverse m.happenings)
        )
    , table
        [ row "Fills since blowdown" (show c.fillsSinceBlowdown)
        , row "Idle sleep after" (clock st.params.config.idleSleepAfter <> " without use")
        , rowIf (not (null c.faults)) "Faults"
            ( if null c.faults then "none"
              else joinWith ", " (map (\f -> show f.kind <> maybe " (active)" (const " (cleared)") f.clearedAt) c.faults)
            )
        ]
    , section "Shots" []
    , if null m.history then HH.p [ cls "note" ] [ HH.text "No shot yet." ]
      else HH.ul [ cls "log" ] (map shotItem (take 5 (reverse m.history)))
    , section "Spec invariants" []
    , table (map (\k -> rowIf (not k.holds) k.name (if k.holds then "holds" else "BROKEN")) (checks st.params.config s c))
    , HH.p [ cls "note" ] [ HH.text "Checked here on each report the page hears. The simulator also checks them on every controller cycle:" ]
    , HH.p [ cls (if null sim.violations then "note" else "bad") ]
        [ HH.text
            ( if null sim.violations then "No invariant has been broken in this session."
              else "Broken at some point: " <> joinWith ", " sim.violations
            )
        ]
    ]

------------------------------------------------------------
-- Bench pane, readings tab
------------------------------------------------------------

readings :: forall w. State -> Array (HH.HTML w Action)
readings st =
  let
    m = st.machine
    c = m.status
    s = m.sensors
    sim = simView st
    cfg = st.params.config
  in
    [ section "Sensors" []
    , table
        [ row "Brew reservoir" (n1 s.brewBoilerC <> " C, target " <> n1 cfg.reservoirTargetC)
        , row "Group block" (n1 s.groupC <> " C, target " <> n1 c.groupTargetC)
        , row "Mix outlet" (n1 s.mixC <> " C" <> maybe "" (\t -> ", target " <> n1 t) c.mixTargetC)
        , row "Water above screen" (n1 s.groupWaterC <> " C")
        , row "Inlet" (n1 s.inletC <> " C, " <> n1 s.inletBar <> " bar, " <> n1 s.inletConductivityUs <> " uS/cm")
        , row "Brew pressure" (n1 s.brewBar <> " bar" <> maybe "" (\t -> ", target " <> n1 t) c.pressureTargetBar)
        , row "Flow into group, estimated" (n2 c.flowMlPerS <> " ml/s" <> maybe "" (\t -> ", target " <> n2 t) c.flowTargetMlPerS)
        , rowIf (not s.steamLevelOk) "Steam" (n2 s.steamBar <> " bar, level " <> (if s.steamLevelOk then "ok" else "low"))
        , row "Cup" (n1 s.cupGrams <> " g")
        ]
    -- The puck has no sensors. These rows read the simulated puck
    -- directly, and the real machine will not have them.
    , section "Puck (simulation only)" []
    , table
        [ row "Wetted" (n1 sim.puck.wetMl <> " ml held, settled " <> n2 sim.puck.settled)
        , row "Bed temperature" (n1 sim.puck.bedC <> " C")
        , row "Evenness" (n2 sim.puck.evenness)
        , row "Extracted" (n2 sim.puck.extracted <> " in the cup, " <> n2 sim.puck.dissolved <> " still held in the puck")
        , row "True flow into group" (n2 sim.flowMlPerS <> " ml/s")
        ]
    , section "Actuators" []
    , table
        [ row "Heat enable" (onOff c.act.heatEnabled)
        , row "Brew element" (onOff c.act.brewElementOn)
        , row "Steam element" (onOff c.act.steamElementOn)
        , row "Group heaters" (onOff c.act.groupHeaterOn)
        , row "Heater draw" (show c.heaterWatts <> " W of " <> show cfg.circuitBudgetWatts)
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

shotLine :: Machine -> String
shotLine m = case m.shot of
  Just shot ->
    "Running " <> shot.profile.name
      <> ", step " <> show (shot.stepIndex + 1) <> " of " <> show (NEA.length shot.profile.steps)
      <> " (" <> shot.step.name <> ", " <> show shot.step.mode <> ", " <> n1 shot.step.targetC <> " C)"
      <> ", " <> n1 (m.at - shot.startedAt) <> " s"
  Nothing -> case last m.history of
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
