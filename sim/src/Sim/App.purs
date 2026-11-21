-- | The page: a machine pane, a controls pane and a bench pane that can
-- | be put away. The page holds no machine. It sends requests to one,
-- | hears its reports and answers some time later, and shows what it
-- | last heard. Here the machine at the far end is a simulated one. The view, the theme, saved
-- | recipes, the user's profile shapes and every parameter are
-- | remembered. When something is wrong the top bar says so in red and
-- | the drawings mark it in red.
module Sim.App (component) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (filter, length, snoc)
import Data.Array.NonEmpty as NEA
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust, maybe)
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
import Sim.Backend as Backend
import Sim.Machine as Machine
import Sim.Layout (BenchTab(..), Edge(..), Tab(..), benchTabs, benchTitle, defaultSizes, defaultView, tabTitle, tabs)
import Sim.Layout as Layout
import Sim.Palette as Palette
import Sim.Forecast (forecast)
import Sim.Model (Action(..), Play(..), State, Theme(..))
import Sim.Params (Group(..))
import Sim.Params as Params
import Sim.Problems (anyProblem, problems)
import Sim.Recipe (Recipe, puckOf, toProfile)
import Sim.Recipe as Recipe
import Sim.Profiles as Profiles
import Sim.Shapes as Shapes
import Sim.Store as Store
import Sim.Update (applyRecipe, defaultLocks, dragFader, dropFader, refreshForecast, request, reset, sim, simView, skipWarmUp, tick)
import Sim.View.Bench (designer, scope, settings)
import Sim.View.Panels (brew, cup, faults, machineView, readings)
import Sim.View.Widgets (chosen, cls, clock, segmented)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (clientX, clientY, toEvent)

recipesKey :: String
recipesKey = "espresso-sim.recipes"

viewKey :: String
viewKey = "espresso-sim.view"

themeKey :: String
themeKey = "espresso-sim.theme"

playKey :: String
playKey = "espresso-sim.play"

shapesKey :: String
shapesKey = "espresso-sim.shapes"

paramsKey :: String
paramsKey = "espresso-sim.params"

defaultRecipe :: Recipe
defaultRecipe = Recipe.house

sizesKey :: String
sizesKey = "espresso-sim.sizes"

locksKey :: String
locksKey = "espresso-sim.locks"

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { machine: Machine.fresh (toProfile (NEA.toArray Profiles.all) defaultRecipe)
        , backend: Backend.create Params.defaults
        , clock: 0.0
        , params: Params.defaults
        , resend: false
        , hand: Nothing
        , pulled: false
        , dropped: false
        , notice: ""
        , recipe: defaultRecipe
        , saved: []
        , shapes: []
        , draft: Nothing
        , view: defaultView
        , theme: Day
        , play: Arcade
        , stepOpen: 0
        , paramGroup: Loops
        , wanted: Nothing
        , locks: defaultLocks
        , sizes: defaultSizes
        , drag: Nothing
        , warming: Nothing
        , settled: false
        , phone: 1
        , forecast: forecast Params.defaults (toProfile (NEA.toArray Profiles.all) defaultRecipe) (puckOf defaultRecipe)
        , advice: Nothing
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

-- | Real milliseconds between frames. Each frame the simulated machine
-- | is given that much time to run, and the page hears what has reached
-- | it by then.
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
    Store.save paramsKey (Params.encode st.params)
    Store.save locksKey (joinWith "," st.locks)

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    recipes <- H.liftEffect (Store.load recipesKey)
    view <- H.liftEffect (Store.load viewKey)
    theme <- H.liftEffect (Store.load themeKey)
    play <- H.liftEffect (Store.load playKey)
    shapes <- H.liftEffect (Store.load shapesKey)
    params <- H.liftEffect (Store.load paramsKey)
    locks <- H.liftEffect (Store.load locksKey)
    sizes <- H.liftEffect (Store.load sizesKey)
    let stored = maybe Params.defaults Params.decode params
    H.modify_ \st -> refreshForecast $ applyRecipe st.recipe st
      { saved = maybe [] Recipe.decode recipes
      , view = maybe defaultView Layout.decode view
      , theme = if theme == Just "night" then Night else Day
      , play = if play == Just "realistic" then Realistic else Arcade
      , shapes = maybe [] Shapes.decode shapes
      , sizes = maybe defaultSizes Layout.decodeSizes sizes
      , locks = maybe defaultLocks (filter (_ /= "") <<< split (Pattern ",")) locks
      , params = stored
      , backend = Backend.create stored
      }
    { emitter, listener } <- H.liftEffect HS.create
    _ <- H.liftAff $ Aff.forkAff $ forever do
      Aff.delay (Milliseconds frameMs)
      H.liftEffect (HS.notify listener Tick)
    void (H.subscribe emitter)
  Tick ->
    H.modify_ (tick frameMs)
  -- A request to the machine. Its answer arrives with a later tick.
  Send event ->
    H.modify_ (request event)
  SetSpeed speed ->
    H.modify_ (sim (Backend.SetSpeed speed))
  Disturb change ->
    H.modify_ (sim (Backend.Disturb change))
  SetLink change ->
    H.modify_ (sim (Backend.SetLink change))
  FaderDrag bar ->
    H.modify_ (dragFader bar)
  FaderDrop ->
    H.modify_ dropFader
  SkipWarmUp ->
    H.modify_ skipWarmUp
  Reset ->
    H.modify_ reset
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
  TogglePlay -> do
    st <- H.get
    let play = if st.play == Arcade then Realistic else Arcade
    H.liftEffect (Store.save playKey (if play == Realistic then "realistic" else "arcade"))
    H.modify_ _ { play = play }

  ToggleTheme -> do
    st <- H.get
    let theme = if st.theme == Day then Night else Day
    H.liftEffect (Store.save themeKey (if theme == Night then "night" else "day"))
    H.modify_ _ { theme = theme }
  GutterDown edge event -> do
    -- Cancelling the press stops the browser selecting text as it drags.
    H.liftEffect (preventDefault (toEvent event))
    H.modify_ _ { drag = Just { edge, x: clientX event, y: clientY event } }
  GutterMove event -> do
    st <- H.get
    case st.drag of
      Nothing -> pure unit
      Just d ->
        let
          x = clientX event
          y = clientY event
          sizes = Layout.dragEdge d.edge (toNumber (x - d.x)) (toNumber (y - d.y)) st.sizes
        in
          H.modify_ _ { sizes = sizes, drag = Just d { x = x, y = y } }
  TidyWindows -> do
    st <- H.get
    let sizes = Layout.tidied st.sizes
    H.liftEffect (Store.save sizesKey (Layout.encodeSizes sizes))
    H.modify_ _ { sizes = sizes }

  GutterUp -> do
    st <- H.get
    when (isJust st.drag) do
      H.liftEffect (Store.save sizesKey (Layout.encodeSizes st.sizes))
      H.modify_ _ { drag = Nothing }

-- | Shown only on a narrow screen: one pane at a time, chosen here.
phoneBar :: forall m. State -> H.ComponentHTML Action () m
phoneBar st =
  HH.div [ cls "phone-bar" ]
    [ chosen (st.phone == 0) "Machine" (Change _ { phone = 0 })
    , chosen (st.phone == 1) "Controls" (Change _ { phone = 1 })
    , chosen (st.phone == 2) "Bench" (Change _ { phone = 2, view = st.view { benchOpen = true } })
    ]

------------------------------------------------------------
-- Rendering
------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  let
    m = st.machine
    c = m.status
    simulated = simView st
    wrong = problems (Machine.silence st.clock m) m simulated
    night = st.theme == Night
    open = st.view.benchOpen
    dragging = case st.drag of
      Just d -> Layout.dragClass d.edge
      Nothing -> ""
    count = length wrong.lines
  in
    -- The colours come from Sim.Palette, as variables the stylesheet
    -- reads.
    HH.div [ cls ("page " <> (if night then "theme-night" else "theme-day")), HP.style (Palette.cssVariables st.theme) ]
      [ HH.div [ cls "top" ]
          [ HH.h1 [ cls "plate" ] [ HH.text "Espresso simulator" ]
          , HH.span [ cls ("chip state-" <> show c.state) ] [ HH.text (show c.state) ]
          , HH.span [ cls "chip" ] [ HH.text ("mode " <> show c.requestedMode) ]
          , HH.span [ cls "chip" ] [ HH.text (clock m.at <> " at " <> show simulated.speed <> "x") ]
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
              [ cls ("play-switch" <> (if st.play == Realistic then " realistic" else ""))
              , HP.title
                  ( if st.play == Realistic then "Realistic: nothing smoothed, nothing a real machine could not do. Click for arcade."
                    else "Arcade: eased lamps, a display that can be pressed, a warm-up that can be skipped. Click for realistic."
                  )
              , HE.onClick \_ -> TogglePlay
              ]
              [ HH.text (if st.play == Realistic then "realistic" else "arcade") ]
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
          [ cls ("desk phone-" <> show st.phone <> (if open then " with-bench" else "") <> dragging)
          , HP.style (Layout.sizeStyle st.sizes)
          , HE.onMouseMove GutterMove
          , HE.onMouseUp \_ -> GutterUp
          , HE.onMouseLeave \_ -> GutterUp
          ]
          ( [ HH.div [ cls "pane machine-pane" ]
                [ HH.div [ cls "pane-body" ] (machineView st wrong) ]
            , HH.div [ cls "gutter", HP.title "Drag to resize", HE.onMouseDown (GutterDown ControlsEdge) ] []
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
                  [ HH.div [ cls "gutter", HP.title "Drag to resize", HE.onMouseDown (GutterDown BenchEdge) ] []
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
