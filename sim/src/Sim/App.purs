-- | The page: a board of panels over the simulated world. Each panel can
-- | be moved earlier or later and folded away, and the arrangement is
-- | remembered. Anything that is wrong is listed at the top in red and
-- | marked in red on the drawings.
module Sim.App (component) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (filter, last, null, reverse, snoc, take)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.Number.Format (fixed, toStringWith)
import Data.String (joinWith, trim)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Sim.Controller (Event(..), heaterDrawWatts, shotInProgress)
import Sim.Invariants (checks)
import Sim.Layout (Layout, Panel(..), Slot, defaultLayout, moveEarlier, moveLater)
import Sim.Layout as Layout
import Sim.Plant (Disturbance)
import Sim.Problems (Problems, anyProblem, problems)
import Sim.Profiles as Profiles
import Sim.Recipe (Limits, Recipe, builtIn, decode, doseLimits, encode, ratioLimits, targetGrams, targetLimits, tempLimits, toProfile, withTarget)
import Sim.Store as Store
import Sim.Types (ClientKind(..), FaultKind(..), MachineState(..), PowerMode(..), Shot, StepMode(..), config)
import Sim.View.Chart (chart)
import Sim.View.Front (front)
import Sim.View.Inside (inside)
import Sim.World (World, initialWorld, send, sensors, stepMany, warmUp)

data Field = Dose | Ratio | Target | Temp

derive instance eqField :: Eq Field

type State =
  { world :: World
  -- Simulated seconds per real second.
  , speed :: Int
  , notice :: String
  , recipe :: Recipe
  , saved :: Array Recipe
  , presetName :: String
  -- The number box being typed in, so a re-render does not overwrite it.
  , draft :: Maybe { field :: Field, text :: String }
  , layout :: Layout
  }

data Action
  = Initialize
  | Tick
  | Send Event
  | SetSpeed Int
  | Disturb (Disturbance -> Disturbance)
  | SkipWarmUp
  | Reset
  | Slide Field String
  | Draft Field String
  | EndDraft
  | ChooseShape Int
  | LoadPreset Recipe
  | SetPresetName String
  | SavePreset
  | DeletePreset String
  | Arrange (Layout -> Layout)

recipesKey :: String
recipesKey = "espresso-sim.recipes"

layoutKey :: String
layoutKey = "espresso-sim.layout"

defaultRecipe :: Recipe
defaultRecipe =
  { name: "House espresso", profileIndex: 0, doseG: 18.0, ratio: 2.0, tempC: 93.0 }

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { world: initialWorld
        , speed: 1
        , notice: ""
        , recipe: defaultRecipe
        , saved: []
        , presetName: ""
        , draft: Nothing
        , layout: defaultLayout
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

-- | Real milliseconds between frames. Two world steps of 25 simulated
-- | milliseconds per frame is real time.
frameMs :: Number
frameMs = 50.0

------------------------------------------------------------
-- Recipe editing
------------------------------------------------------------

limitsOf :: Field -> Limits
limitsOf = case _ of
  Dose -> doseLimits
  Ratio -> ratioLimits
  Target -> targetLimits
  Temp -> tempLimits

valueOf :: Field -> Recipe -> Number
valueOf field r = case field of
  Dose -> r.doseG
  Ratio -> r.ratio
  Target -> targetGrams r
  Temp -> r.tempC

setField :: Field -> Number -> Recipe -> Recipe
setField field raw r =
  let
    limits = limitsOf field
    value = clamp limits.min limits.max raw
  in
    case field of
      Dose -> r { doseG = value }
      Ratio -> r { ratio = value }
      Target -> withTarget value r
      Temp -> r { tempC = value }

format :: Field -> Number -> String
format = case _ of
  Ratio -> toStringWith (fixed 2)
  _ -> toStringWith (fixed 1)

-- | Hand a changed recipe to the controller as a profile. The controller
-- | refuses during a shot or a fault, and then the recipe stays as it was.
applyRecipe :: Recipe -> State -> State
applyRecipe recipe st =
  case send (ProfileSelected (toProfile recipe)) st.world of
    Left reason -> st { notice = "Rejected: " <> reason }
    Right world -> st
      { recipe = recipe
      , world = world { doseG = recipe.doseG }
      , notice = ""
      }

typed :: Field -> String -> State -> State
typed field text st = case Number.fromString (trim text) of
  Just value -> applyRecipe (setField field value st.recipe) st
  Nothing -> st

------------------------------------------------------------
-- Actions
------------------------------------------------------------

-- | Advance the world one frame. When the machine changes state by
-- | itself (ready, asleep, faulted) the clock drops back to real time,
-- | so a fast-forwarded session does not race past the moment.
frame :: State -> State
frame st =
  let
    world = stepMany (st.speed * 2) st.world
    before = st.world.ctl.state
    after = world.ctl.state
    settled = after /= before && (after == Ready || after == Sleeping || after == Faulted)
  in
    st { world = world, speed = if settled then 1 else st.speed }

-- | Put the plant at operating temperature. A sleeping machine is woken
-- | first, because a hot machine that is asleep still refuses a shot.
skipWarmUp :: State -> State
skipWarmUp st = case st.world.ctl.state of
  Faulted -> st { notice = "Clear the fault under Service first." }
  Sleeping -> case send (WakeRequested Full) st.world of
    Right world -> st { world = warmUp world, notice = "" }
    Left reason -> st { notice = "Rejected: " <> reason }
  _ -> st { world = warmUp st.world, notice = "" }

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    recipes <- H.liftEffect (Store.load recipesKey)
    arrangement <- H.liftEffect (Store.load layoutKey)
    H.modify_ _
      { saved = maybe [] decode recipes
      , layout = maybe defaultLayout Layout.decode arrangement
      }
    { emitter, listener } <- H.liftEffect HS.create
    _ <- H.liftAff $ Aff.forkAff $ forever do
      Aff.delay (Milliseconds frameMs)
      H.liftEffect (HS.notify listener Tick)
    void (H.subscribe emitter)
  Tick ->
    H.modify_ frame
  Send event ->
    H.modify_ \st -> case send event st.world of
      Left reason -> st { notice = "Rejected: " <> reason }
      Right world -> st
        { world = world
        , notice = ""
        , speed = if startsShot event then 1 else st.speed
        }
  SetSpeed speed ->
    H.modify_ _ { speed = speed }
  Disturb change ->
    H.modify_ \st -> st { world = st.world { dist = change st.world.dist } }
  SkipWarmUp ->
    H.modify_ skipWarmUp
  Reset ->
    H.modify_ \st -> applyRecipe st.recipe st { world = initialWorld, speed = 1 }
  Slide field text ->
    H.modify_ (typed field text)
  Draft field text ->
    H.modify_ \st -> typed field text st { draft = Just { field, text } }
  EndDraft ->
    H.modify_ _ { draft = Nothing }
  ChooseShape index ->
    H.modify_ \st -> applyRecipe st.recipe { profileIndex = index } st
  LoadPreset recipe ->
    H.modify_ \st -> applyRecipe recipe st { draft = Nothing }
  SetPresetName name ->
    H.modify_ _ { presetName = name }
  SavePreset -> do
    st <- H.get
    let name = trim st.presetName
    if name == "" then H.modify_ _ { notice = "Give the preset a name first." }
    else do
      let recipe = st.recipe { name = name }
      let saved = snoc (filter (\r -> r.name /= name) st.saved) recipe
      H.liftEffect (Store.save recipesKey (encode saved))
      H.modify_ _ { saved = saved, recipe = recipe, presetName = "", notice = "" }
  DeletePreset name -> do
    st <- H.get
    let saved = filter (\r -> r.name /= name) st.saved
    H.liftEffect (Store.save recipesKey (encode saved))
    H.modify_ _ { saved = saved }
  Arrange change -> do
    st <- H.get
    let layout = change st.layout
    H.liftEffect (Store.save layoutKey (Layout.encode layout))
    H.modify_ _ { layout = layout }

startsShot :: Event -> Boolean
startsShot = case _ of
  ShotRequested -> true
  _ -> false

------------------------------------------------------------
-- Formatting
------------------------------------------------------------

n1 :: Number -> String
n1 = toStringWith (fixed 1)

n2 :: Number -> String
n2 = toStringWith (fixed 2)

clock :: Number -> String
clock seconds = n1 (seconds / 60.0) <> " min"

onOff :: Boolean -> String
onOff b = if b then "on" else "off"

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls name = HP.class_ (HH.ClassName name)

------------------------------------------------------------
-- Rendering
------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  let
    w = st.world
    c = w.ctl
    wrong = problems w
  in
    HH.div [ cls "page" ]
      [ HH.div [ cls "top" ]
          [ HH.h1_ [ HH.text "Espresso machine simulator" ]
          , HH.p [ cls "status" ]
              [ HH.text
                  ( "State " <> show c.state
                      <> " | mode " <> show c.requestedMode
                      <> " | simulated time " <> clock w.now
                      <> " | " <> show st.speed <> "x"
                  )
              ]
          , HH.p [ cls "notice" ] [ HH.text st.notice ]
          ]
      , if anyProblem wrong then HH.ul [ cls "problems" ] (map (\text -> HH.li_ [ HH.text text ]) wrong.lines)
        else HH.text ""
      , HH.div [ cls "board" ] (map (panel st wrong) st.layout)
      , HH.p [ cls "foot" ] [ button "Restore the default arrangement" (Arrange (const defaultLayout)) ]
      ]

panel :: forall w. State -> Problems -> Slot -> HH.HTML w Action
panel st wrong slot =
  HH.div [ cls "panel" ]
    ( [ HH.div [ cls "panel-head" ]
          [ HH.h2_ [ HH.text (Layout.title slot.panel) ]
          , small "earlier" (Arrange (moveEarlier slot.panel))
          , small "later" (Arrange (moveLater slot.panel))
          , small (if slot.open then "fold" else "open") (Arrange (Layout.toggle slot.panel))
          ]
      ]
        <> (if slot.open then body st wrong slot.panel else [])
    )

body :: forall w. State -> Problems -> Panel -> Array (HH.HTML w Action)
body st wrong which =
  let
    w = st.world
    c = w.ctl
    s = sensors w
  in
    case which of
      Machine ->
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
            }
        , section "Paddle"
            [ button "Start shot" (Send ShotRequested)
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
            ( map (speedButton st.speed) [ 1, 10, 60 ]
                <> [ button "Skip warm-up" SkipWarmUp, button "Reset" Reset ]
            )
        ]

      RecipePanel ->
        [ section "Beans" (map presetButton builtIn <> map savedPreset st.saved)
        , HH.div [ cls "fields" ]
            [ fieldRow st Dose "Beans in" "g"
            , fieldRow st Ratio "Ratio, 1 to" ""
            , fieldRow st Target "In the cup" "g"
            , fieldRow st Temp "Water" "C"
            ]
        , section "Shape" (map (shapeButton st.recipe.profileIndex) (indexed (NEA.toArray Profiles.all)))
        , section "Save these settings as"
            [ HH.input
                [ HP.type_ HP.InputText
                , HP.placeholder "bean name"
                , HP.value st.presetName
                , HE.onValueInput SetPresetName
                ]
            , button "Save" SavePreset
            ]
        ]

      ShotPanel ->
        [ HH.p_ [ HH.text (shotLine w) ]
        , chart w.trace
        , HH.p [ cls "legend" ]
            [ legend "bar" "pressure, bar (0 to 12)"
            , legend "flow" "flow, ml/s (0 to 12)"
            , legend "water" "water above screen, C (80 to 100)"
            , legend "target" "target, C (80 to 100)"
            , legend "grams" "in cup, g (0 to 60)"
            ]
        , HH.ul_ (map shotItem (take 5 (reverse c.history)))
        ]

      InsidePanel ->
        [ inside
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
        , section "Things that go wrong"
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
        ]

      Log ->
        [ if null w.happenings then HH.p_ [ HH.text "Nothing yet. Wake the machine to begin." ]
          else HH.ul [ cls "log" ]
            ( map
                (\h -> HH.li [ cls (if h.bad then "bad" else "") ] [ HH.text (clock h.at <> ": " <> h.text) ])
                (reverse w.happenings)
            )
        , table
            [ row "Fills since blowdown" (show c.fillsSinceBlowdown)
            , row "Idle sleep after" (clock config.idleSleepAfter <> " without use")
            , rowIf (not (null c.faults)) "Faults"
                ( if null c.faults then "none"
                  else joinWith ", " (map (\f -> show f.kind <> maybe " (active)" (const " (cleared)") f.clearedAt) c.faults)
                )
            ]
        ]

      SensorsPanel ->
        [ table
            [ row "Brew reservoir" (n1 s.brewBoilerC <> " C, target " <> n1 config.reservoirTargetC)
            , row "Group block" (n1 s.groupC <> " C, target " <> n1 c.groupTargetC)
            , row "Mix outlet" (n1 s.mixC <> " C" <> maybe "" (\t -> ", target " <> n1 t) c.mixTargetC)
            , row "Water above screen" (n1 s.groupWaterC <> " C")
            , row "Inlet" (n1 s.inletC <> " C, " <> n1 s.inletBar <> " bar, " <> n1 s.inletConductivityUs <> " uS/cm")
            , row "Brew pressure" (n1 s.brewBar <> " bar" <> maybe "" (\t -> ", target " <> n1 t) c.pressureTargetBar)
            , row "Flow into group" (n2 w.plant.flowMlPerS <> " ml/s" <> maybe "" (\t -> ", target " <> n2 t) c.flowTargetMlPerS)
            , rowIf (not s.steamLevelOk) "Steam" (n2 s.steamBar <> " bar, level " <> (if s.steamLevelOk then "ok" else "low"))
            , row "Cup" (n1 s.cupGrams <> " g")
            ]
        ]

      ActuatorsPanel ->
        [ table
            [ row "Heat enable" (onOff c.act.heatEnabled)
            , row "Brew element" (onOff c.act.brewElementOn)
            , row "Steam element" (onOff c.act.steamElementOn)
            , row "Group heaters" (onOff c.act.groupHeaterOn)
            , row "Heater draw" (show (heaterDrawWatts c.act) <> " W of " <> show config.circuitBudgetWatts)
            , row "Pump" (show c.act.pump <> ", speed " <> n2 c.act.pumpSpeed)
            , row "Bypass valve SV_BYP" (show c.act.bypassValve)
            , row "Fill valve SV_FILL" (show c.act.fillValve)
            , row "Group valve SV_GRP" (show c.act.groupValve)
            , row "Mix valve MV_MIX" (show c.act.mixValve <> ", position " <> n2 c.act.mixPosition)
            ]
        ]

      InvariantsPanel ->
        [ table (map (\k -> rowIf (not k.holds) k.name (if k.holds then "holds" else "BROKEN")) (checks s c))
        , HH.p [ cls (if null w.violations then "" else "bad") ]
            [ HH.text
                ( if null w.violations then "No invariant has been broken in this session."
                  else "Broken at some point: " <> joinWith ", " w.violations
                )
            ]
        ]

indexed :: forall a. Array a -> Array { index :: Int, item :: a }
indexed items = (foldl step { next: 0, out: [] } items).out
  where
  step acc item = { next: acc.next + 1, out: snoc acc.out { index: acc.next, item } }

section :: forall w. String -> Array (HH.HTML w Action) -> HH.HTML w Action
section title children =
  HH.div [ cls "section" ]
    ([ HH.span [ cls "section-title" ] [ HH.text title ] ] <> children)

button :: forall w. String -> Action -> HH.HTML w Action
button text action =
  HH.button [ HE.onClick \_ -> action ] [ HH.text text ]

small :: forall w. String -> Action -> HH.HTML w Action
small text action =
  HH.button [ HE.onClick \_ -> action, cls "small" ] [ HH.text text ]

-- | A switch. An active one is a problem being injected or a valve held
-- | open, so it is drawn filled.
toggle :: forall w. String -> Boolean -> Action -> HH.HTML w Action
toggle text active action =
  HH.button
    [ HE.onClick \_ -> action
    , cls (if active then "active" else "")
    ]
    [ HH.text (text <> ": " <> (if active then "yes" else "no")) ]

speedButton :: forall w. Int -> Int -> HH.HTML w Action
speedButton current speed =
  HH.button
    [ HE.onClick \_ -> SetSpeed speed
    , cls (if current == speed then "active" else "")
    ]
    [ HH.text (show speed <> "x") ]

shapeButton :: forall w r. Int -> { index :: Int, item :: { name :: String | r } } -> HH.HTML w Action
shapeButton current entry =
  HH.button
    [ HE.onClick \_ -> ChooseShape entry.index
    , cls (if current == entry.index then "active" else "")
    ]
    [ HH.text entry.item.name ]

presetButton :: forall w. Recipe -> HH.HTML w Action
presetButton recipe =
  HH.button [ HE.onClick \_ -> LoadPreset recipe ] [ HH.text recipe.name ]

savedPreset :: forall w. Recipe -> HH.HTML w Action
savedPreset recipe =
  HH.span [ cls "saved" ]
    [ presetButton recipe
    , HH.button [ HE.onClick \_ -> DeletePreset recipe.name, cls "remove" ] [ HH.text "remove" ]
    ]

-- | One recipe setting: a slider for coarse moves and a box for an exact
-- | number. Both write the same value.
fieldRow :: forall w. State -> Field -> String -> String -> HH.HTML w Action
fieldRow st which title unit =
  let
    limits = limitsOf which
    value = valueOf which st.recipe
    boxText = case st.draft of
      Just draft | draft.field == which -> draft.text
      _ -> format which value
  in
    HH.div [ cls "field" ]
      [ HH.label_ [ HH.text title ]
      , HH.input
          [ HP.type_ HP.InputRange
          , HP.min limits.min
          , HP.max limits.max
          , HP.step (HP.Step limits.step)
          , HP.value (show value)
          , HE.onValueInput (Slide which)
          ]
      , HH.input
          [ HP.type_ HP.InputText
          , HP.attr (AttrName "inputmode") "decimal"
          , cls "exact"
          , HP.value boxText
          , HE.onValueInput (Draft which)
          , HE.onBlur \_ -> EndDraft
          ]
      , HH.span [ cls "unit" ] [ HH.text unit ]
      ]

table :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
table rows = HH.table_ [ HH.tbody_ rows ]

row :: forall w i. String -> String -> HH.HTML w i
row = rowIf false

-- | A table row, in red when the first argument says it is a problem.
rowIf :: forall w i. Boolean -> String -> String -> HH.HTML w i
rowIf wrong title value =
  HH.tr [ cls (if wrong then "bad" else "") ] [ HH.th_ [ HH.text title ], HH.td_ [ HH.text value ] ]

legend :: forall w i. String -> String -> HH.HTML w i
legend name text = HH.span [ cls ("key " <> name) ] [ HH.text text ]

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