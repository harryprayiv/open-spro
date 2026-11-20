-- | The page: a machine pane, a controls pane and a bench pane that can
-- | be put away, over the simulated world. The view, the theme, saved
-- | recipes, the user's profile shapes and every parameter are
-- | remembered. When something is wrong the top bar says so in red and
-- | the drawings mark it in red.
module Sim.App (component) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (filter, length, snoc)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number as Number
import Data.String (Pattern(..), joinWith, split, trim)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Sim.Controller (Event(..))
import Sim.Layout (BenchTab(..), Tab(..), benchTabs, benchTitle, defaultView, tabTitle, tabs)
import Sim.Layout as Layout
import Sim.Forecast (forecast)
import Sim.Model (Action(..), State, Theme(..))
import Sim.Params (Group(..))
import Sim.Params as Params
import Sim.Problems (anyProblem, problems)
import Sim.Recipe (Recipe, puckOf, toProfile)
import Sim.Recipe as Recipe
import Sim.Profiles as Profiles
import Sim.Shapes as Shapes
import Sim.Store as Store
import Sim.Update (applyRecipe, frame, refreshForecast, skipWarmUp, defaultLocks)
import Sim.View.Bench (designer, scope, settings)
import Sim.View.Panels (brew, cup, faults, machineView, readings)
import Sim.View.Widgets (chosen, cls, clock, segmented)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (clientX, toEvent)
import Sim.World (initialWorld, paramsOf, send, worldWith)

recipesKey :: String
recipesKey = "espresso-sim.recipes"

viewKey :: String
viewKey = "espresso-sim.view"

themeKey :: String
themeKey = "espresso-sim.theme"

shapesKey :: String
shapesKey = "espresso-sim.shapes"

paramsKey :: String
paramsKey = "espresso-sim.params"

defaultRecipe :: Recipe
defaultRecipe = Recipe.house

sizesKey :: String
sizesKey = "espresso-sim.sizes"

defaultSizes :: { controls :: Number, bench :: Number }
defaultSizes = { controls: 560.0, bench: 420.0 }

readSizes :: String -> Maybe { controls :: Number, bench :: Number }
readSizes text = case split (Pattern ",") text of
  [ a, b ] -> do
    controls <- Number.fromString a
    bench <- Number.fromString b
    pure { controls: clamp 320.0 1200.0 controls, bench: clamp 260.0 900.0 bench }
  _ -> Nothing

locksKey :: String
locksKey = "espresso-sim.locks"

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { world: initialWorld
        , speed: 1
        , notice: ""
        , recipe: defaultRecipe
        , saved: []
        , shapes: []
        , draft: Nothing
        , view: defaultView
        , theme: Day
        , stepOpen: 0
        , paramGroup: Loops
        , wanted: Nothing
        , locks: defaultLocks
        , sizes: defaultSizes
        , drag: Nothing
        , phone: 1
        , forecast: forecast Params.defaults (toProfile (NEA.toArray Profiles.all) defaultRecipe) (puckOf defaultRecipe)
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
-- Actions
------------------------------------------------------------

-- | Apply a change, work out the forecast again, and remember what the
-- | change may have touched: the view, the user's shapes, the saved
-- | recipes and the parameters.
changeAndStore :: forall output m. MonadAff m => (State -> State) -> H.HalogenM State Action () output m Unit
changeAndStore change = do
  st <- H.modify (refreshForecast <<< change)
  H.liftEffect do
    Store.save viewKey (Layout.encode st.view)
    Store.save shapesKey (Shapes.encode st.shapes)
    Store.save recipesKey (Recipe.encode st.saved)
    Store.save paramsKey (Params.encode (paramsOf st.world))
    Store.save locksKey (joinWith "," st.locks)

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    recipes <- H.liftEffect (Store.load recipesKey)
    view <- H.liftEffect (Store.load viewKey)
    theme <- H.liftEffect (Store.load themeKey)
    shapes <- H.liftEffect (Store.load shapesKey)
    params <- H.liftEffect (Store.load paramsKey)
    locks <- H.liftEffect (Store.load locksKey)
    sizes <- H.liftEffect (Store.load sizesKey)
    H.modify_ \st -> refreshForecast $ applyRecipe st.recipe st
      { saved = maybe [] Recipe.decode recipes
      , view = maybe defaultView Layout.decode view
      , theme = if theme == Just "night" then Night else Day
      , shapes = maybe [] Shapes.decode shapes
      , sizes = fromMaybe defaultSizes (sizes >>= readSizes)
      , locks = maybe defaultLocks (filter (_ /= "") <<< split (Pattern ",")) locks
      , world = worldWith (maybe Params.defaults Params.decode params)
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
    -- A cold machine with the present parameters and recipe.
    H.modify_ \st -> applyRecipe st.recipe st { world = worldWith (paramsOf st.world), speed = 1 }
  Typed key text apply ->
    changeAndStore \st ->
      let
        drafted = st { draft = Just { key, text } }
      in
        case Number.fromString (trim text) of
          Just value -> apply value drafted
          Nothing -> drafted
  EndDraft ->
    H.modify_ _ { draft = Nothing }
  Change change ->
    changeAndStore change
  SavePreset -> do
    st <- H.get
    let name = trim st.recipe.name
    if name == "" then H.modify_ _ { notice = "Name the beans first." }
    else do
      let recipe = st.recipe { name = name }
      changeAndStore _
        { saved = snoc (filter (\r -> r.name /= name) st.saved) recipe
        , recipe = recipe
        , notice = "Saved " <> name <> "."
        }
  DeletePreset name ->
    changeAndStore \st -> st { saved = filter (\r -> r.name /= name) st.saved }
  ToggleTheme -> do
    st <- H.get
    let theme = if st.theme == Day then Night else Day
    H.liftEffect (Store.save themeKey (if theme == Night then "night" else "day"))
    H.modify_ _ { theme = theme }
  GutterDown gutter event -> do
    -- Cancelling the press stops the browser selecting text as it drags.
    H.liftEffect (preventDefault (toEvent event))
    H.modify_ _ { drag = Just { gutter, x: clientX event } }
  GutterMove event -> do
    st <- H.get
    case st.drag of
      Nothing -> pure unit
      Just d ->
        let
          x = clientX event
          moved = toNumber (x - d.x)
          sizes =
            if d.gutter == 0 then st.sizes { controls = clamp 320.0 1200.0 (st.sizes.controls - moved) }
            else st.sizes { bench = clamp 260.0 900.0 (st.sizes.bench - moved) }
        in
          H.modify_ _ { sizes = sizes, drag = Just d { x = x } }
  GutterUp -> do
    st <- H.get
    when (isJust st.drag) do
      H.liftEffect (Store.save sizesKey (show st.sizes.controls <> "," <> show st.sizes.bench))
      H.modify_ _ { drag = Nothing }

-- | The pane widths as CSS variables. The stylesheet uses them on a wide
-- | screen and ignores them on a phone.
sizeStyle :: State -> String
sizeStyle st = "--controls: " <> show st.sizes.controls <> "px; --bench: " <> show st.sizes.bench <> "px"

-- | Shown only on a narrow screen: one pane at a time, chosen here.
phoneBar :: forall m. State -> H.ComponentHTML Action () m
phoneBar st =
  HH.div [ cls "phone-bar" ]
    [ chosen (st.phone == 0) "Machine" (Change _ { phone = 0 })
    , chosen (st.phone == 1) "Controls" (Change _ { phone = 1 })
    , chosen (st.phone == 2) "Bench" (Change _ { phone = 2, view = st.view { benchOpen = true } })
    ]

startsShot :: Event -> Boolean
startsShot = case _ of
  ShotRequested -> true
  _ -> false

------------------------------------------------------------
-- Rendering
------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  let
    w = st.world
    c = w.ctl
    wrong = problems w
    night = st.theme == Night
    open = st.view.benchOpen
    count = length wrong.lines
  in
    HH.div [ cls ("page " <> (if night then "theme-night" else "theme-day")) ]
      [ HH.div [ cls "top" ]
          [ HH.h1 [ cls "plate" ] [ HH.text "Espresso simulator" ]
          , HH.span [ cls ("chip state-" <> show c.state) ] [ HH.text (show c.state) ]
          , HH.span [ cls "chip" ] [ HH.text ("mode " <> show c.requestedMode) ]
          , HH.span [ cls "chip" ] [ HH.text (clock w.now <> " at " <> show st.speed <> "x") ]
          -- Problems stay visible here when the bench is put away.
          , if anyProblem wrong then
              HH.button
                [ cls "alarm"
                , HP.title "Show the problems"
                , HE.onClick \_ -> Change _ { view = st.view { benchOpen = true, benchTab = Faults } }
                ]
                [ HH.text (show count <> (if count == 1 then " problem" else " problems")) ]
            else HH.text ""
          , HH.span [ cls "notice" ] [ HH.text st.notice ]
          , chosen open "Bench" (Change _ { view = st.view { benchOpen = not open } })
          , HH.button
              [ cls ("theme-switch" <> (if night then " night" else ""))
              , HP.title "Day or night"
              , HE.onClick \_ -> ToggleTheme
              ]
              [ HH.span [ cls "sun" ] [ HH.text "day" ]
              , HH.span [ cls "moon" ] [ HH.text "night" ]
              , HH.span [ cls "knob" ] []
              ]
          ]
      , HH.div
          [ cls ("desk phone-" <> show st.phone <> (if open then " with-bench" else "") <> (if isJust st.drag then " dragging" else ""))
          , HP.style (sizeStyle st)
          , HE.onMouseMove GutterMove
          , HE.onMouseUp \_ -> GutterUp
          , HE.onMouseLeave \_ -> GutterUp
          ]
          ( [ HH.div [ cls "pane machine-pane" ]
                [ HH.div [ cls "pane-body" ] (machineView st wrong) ]
            , HH.div [ cls "gutter", HP.title "Drag to resize", HE.onMouseDown (GutterDown 0) ] []
            , HH.div [ cls "pane controls-pane" ]
                [ HH.div [ cls "pane-head" ]
                    [ segmented
                        (map (\tab -> chosen (st.view.tab == tab) (tabTitle tab) (Change _ { view = st.view { tab = tab } })) tabs)
                    ]
                , HH.div [ cls "pane-body" ]
                    ( case st.view.tab of
                        Brew -> brew st
                        Cup -> cup st
                        Shape -> designer st
                        Tuning -> settings st
                    )
                ]
            ]
              <>
                if open then
                  [ HH.div [ cls "gutter", HP.title "Drag to resize", HE.onMouseDown (GutterDown 1) ] []
                  , HH.div [ cls "pane bench-pane" ]
                      [ HH.div [ cls "pane-head" ]
                          [ segmented
                              ( map
                                  (\tab -> chosen (st.view.benchTab == tab) (benchTitle tab) (Change _ { view = st.view { benchTab = tab } }))
                                  benchTabs
                              )
                          , HH.button
                              [ cls "put-away"
                              , HP.title "Put the bench away"
                              , HE.onClick \_ -> Change _ { view = st.view { benchOpen = false } }
                              ]
                              [ HH.text "Put away" ]
                          ]
                      , HH.div [ cls "pane-body" ]
                          ( case st.view.benchTab of
                              Faults -> faults st wrong
                              Readings -> readings st
                              Scope -> scope st
                          )
                      ]
                  ]
                else []
          )
      , phoneBar st
      ]
