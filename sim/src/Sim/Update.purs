-- | Pure changes to the page's state.
module Sim.Update
  ( allShapes
  , builtInCount
  , baseShape
  , tempShift
  , applyRecipe
  , limitsOf
  , valueOf
  , setField
  , fieldKey
  , chooseShape
  , chooseRoast
  , findGrind
  , askAdvice
  , takeAdvice
  , defaultLocks
  , isLocked
  , toggleLock
  , editShape
  , editStepTemp
  , editGroupBase
  , deleteShape
  , isCustom
  , changeParams
  , resetParams
  , refreshForecast
  , simView
  , sim
  , request
  , tick
  , hear
  , skipWarmUp
  , reset
  , Segment
  , segments
  , timelineSpan
  , playheadAt
  , faderReading
  , dragFader
  , dropFader
  ) where

import Prelude

import Data.Array (deleteAt, drop, elem, filter, index, length, mapWithIndex, modifyAt, range, snoc, zipWith)
import Data.Foldable (foldl)
import Data.Int (ceil, toNumber)
import Data.Number (abs)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Sim.Advice (advise)
import Sim.Backend (SimCommand(..), SimView)
import Sim.Backend as Backend
import Sim.Link (Event(..), FaderMode(..), Message(..), eventLabel, faderScale)
import Sim.Machine (Outcome(..), Pending)
import Sim.Machine as Machine
import Sim.Forecast (forecast)
import Sim.Model (Field(..), State)
import Sim.Params (Params)
import Sim.Params as Params
import Sim.Profiles as Profiles
import Sim.Recipe (Limits, Recipe, Roast, doseLimits, grindLimits, puckOf, ratioLimits, secondsLimits, soakLimits, targetGrams, targetLimits, tempLimits, toProfile, withRoastDefaults, withTarget)
import Sim.Shapes as Shapes
import Sim.Types (MachineState(..), PowerMode(..), Profile, ShotState(..))

------------------------------------------------------------
-- Shapes and recipes
------------------------------------------------------------

builtInCount :: Int
builtInCount = NEA.length Profiles.all

-- | The built-in shapes followed by the user's own.
allShapes :: State -> Array Profile
allShapes st = NEA.toArray Profiles.all <> st.shapes

isCustom :: State -> Boolean
isCustom st = st.recipe.profileIndex >= builtInCount

-- | The shape the recipe points at, before the recipe moves its
-- | temperatures.
baseShape :: State -> Profile
baseShape st = fromMaybe (NEA.head Profiles.all) (index (allShapes st) st.recipe.profileIndex)

-- | How far the recipe's water temperature moves every temperature in
-- | the shape.
tempShift :: State -> Number
tempShift st = st.recipe.tempC - (baseShape st).maxTargetC

-- | The simulator's own view of the machine. Views read this for what
-- | no machine could report, and for nothing else.
simView :: State -> SimView
simView st = Backend.view st.backend

-- | Tell the simulator to do something only a simulator can.
sim :: SimCommand -> State -> State
sim cmd st = st { backend = Backend.command cmd st.backend }

-- | Send a request to the machine and remember it until it is answered.
ask :: Event -> Maybe Recipe -> State -> State
ask event before st =
  let
    asked = Machine.ask st.clock event before st.machine
  in
    st { machine = asked.machine, backend = Backend.request asked.request st.backend }

-- | A request from a key on the page. The answer comes later: a
-- | refusal shows as a notice when it arrives.
request :: Event -> State -> State
request event st = case event of
  -- A shot is watched in real time.
  ShotRequested -> sim (SetSpeed 1) (ask event Nothing st { notice = "" })
  _ -> ask event Nothing st { notice = "" }

-- | Change the recipe and send it to the machine as a profile. The
-- | page takes the change at once, so a slider does not wait on the
-- | link. If the machine as last heard would refuse it, the page says
-- | so without asking. If the machine refuses it after all, or never
-- | answers, the recipe goes back to what it was.
applyRecipe :: Recipe -> State -> State
applyRecipe recipe st = case Machine.busy st.machine of
  Just reason -> st { notice = "Rejected: " <> reason }
  Nothing ->
    sim (LoadPuck (puckOf recipe))
      (ask (ProfileSelected (toProfile (allShapes st) recipe)) (Just st.recipe) st { recipe = recipe, notice = "", resend = false })

limitsOf :: Field -> Limits
limitsOf = case _ of
  Dose -> doseLimits
  Ratio -> ratioLimits
  Target -> targetLimits
  Temp -> tempLimits
  Seconds -> secondsLimits
  Grind -> grindLimits
  Soak -> soakLimits

valueOf :: Field -> Recipe -> Number
valueOf field r = case field of
  Dose -> r.doseG
  Ratio -> r.ratio
  Target -> targetGrams r
  Temp -> r.tempC
  Seconds -> r.seconds
  Grind -> r.grind
  Soak -> r.soakS

fieldKey :: Field -> String
fieldKey = case _ of
  Dose -> "recipe.dose"
  Ratio -> "recipe.ratio"
  Target -> "recipe.target"
  Temp -> "recipe.temp"
  Seconds -> "recipe.seconds"
  Grind -> "recipe.grind"
  Soak -> "recipe.soak"

-- | Ratio and weight in the cup are one setting seen two ways, so they
-- | share a lock.
lockKey :: Field -> String
lockKey = case _ of
  Target -> fieldKey Ratio
  field -> fieldKey field

-- | The house standard holds dose, yield and time. Temperature and grind
-- | are left for the machine to suggest.
defaultLocks :: Array String
defaultLocks = [ fieldKey Dose, fieldKey Ratio, fieldKey Seconds ]

isLocked :: State -> Field -> Boolean
isLocked st field = elem (lockKey field) st.locks

toggleLock :: Field -> State -> State
toggleLock field st =
  st { locks = if isLocked st field then filter (_ /= lockKey field) st.locks else snoc st.locks (lockKey field) }

-- | The grind, in half steps, whose unpaced shot comes nearest the
-- | recipe's time. The search runs a forecast shot for each candidate.
findGrind :: State -> Recipe -> Recipe
findGrind st recipe =
  let
    candidates = map (\i -> grindLimits.min + toNumber i * grindLimits.step) (range 0 20)
    miss grind =
      let
        r = recipe { grind = grind, paced = false }
        f = forecast st.params (toProfile (allShapes st) r) (puckOf r)
      in
        if f.outcome == Just StoppedByWeight then abs (f.seconds - recipe.seconds) else 1000.0
    best = foldl (\acc grind -> let m = miss grind in if m < acc.miss then { grind, miss: m } else acc) { grind: recipe.grind, miss: miss recipe.grind } candidates
  in
    recipe { grind = best.grind }

-- | Change the roast. Every setting that is not held moves to the
-- | roast's starting point, and an unheld grind is then worked out so the
-- | shot lands on its time. A shape of the barista's own is kept.
chooseRoast :: Roast -> State -> State
chooseRoast roast st =
  let
    now = st.recipe { roast = roast }
    start = withRoastDefaults now
    free field = not (isLocked st field)
    moved = now
      { tempC = if free Temp then start.tempC else now.tempC
      , ratio = if free Ratio then start.ratio else now.ratio
      , seconds = if free Seconds then start.seconds else now.seconds
      , grind = if free Grind then start.grind else now.grind
      , soakS = if free Soak then start.soakS else now.soakS
      , profileIndex = if now.profileIndex < builtInCount then start.profileIndex else now.profileIndex
      }
  in
    applyRecipe (if free Grind then findGrind st moved else moved) st { draft = Nothing }

setField :: Field -> Number -> State -> State
setField field raw st =
  let
    limits = limitsOf field
    value = clamp limits.min limits.max raw
    r = st.recipe
  in
    applyRecipe
      ( case field of
          Dose -> r { doseG = value }
          Ratio -> r { ratio = value }
          Target -> withTarget value r
          Temp -> r { tempC = value }
          Seconds -> r { seconds = value }
          Grind -> r { grind = value }
          Soak -> r { soakS = value }
      )
      st

chooseShape :: Int -> State -> State
chooseShape i st = applyRecipe st.recipe { profileIndex = i } st

-- | A shape cannot change under a running shot.
guarded :: (State -> State) -> State -> State
guarded change st =
  if isJust st.machine.status.shot then st { notice = "Rejected: a shot is running" }
  else change st

-- | Change the selected shape. Editing a built-in shape first makes a
-- | copy and selects it, so the built-in ones stay as they are.
editShape :: (Profile -> Profile) -> State -> State
editShape change = guarded \st ->
  let
    i = st.recipe.profileIndex
  in
    if i < builtInCount then
      let
        copy = Shapes.tidy (change (Shapes.fork (baseShape st)))
        shift = tempShift st
      in
        applyRecipe st.recipe { profileIndex = builtInCount + length st.shapes, tempC = copy.maxTargetC + shift }
          st { shapes = snoc st.shapes copy }
    else
      let
        shift = tempShift st
        shapes = fromMaybe st.shapes (modifyAt (i - builtInCount) (Shapes.tidy <<< change) st.shapes)
        next = st { shapes = shapes }
      in
        -- Keep the shift the same, so only the edited number moves.
        applyRecipe st.recipe { tempC = clamp tempLimits.min tempLimits.max ((baseShape next).maxTargetC + shift) } next

-- | Set a step's water temperature as it will actually be brewed.
editStepTemp :: Int -> Number -> State -> State
editStepTemp i actual st =
  let
    shift = tempShift st
  in
    editShape (Shapes.editStep i _ { targetC = actual - shift }) st

-- | Set the group block temperature as it will actually be held.
editGroupBase :: Number -> State -> State
editGroupBase actual st =
  let
    shift = tempShift st
  in
    editShape (_ { groupBaseC = actual - shift }) st

-- | Remove the selected shape, if it is one of the user's own.
deleteShape :: State -> State
deleteShape = guarded \st ->
  if isCustom st then
    applyRecipe st.recipe { profileIndex = 0 }
      st { shapes = fromMaybe st.shapes (deleteAt (st.recipe.profileIndex - builtInCount) st.shapes) }
  else st

------------------------------------------------------------
-- Parameters
------------------------------------------------------------

-- | Change the parameters. The page keeps them for the forecast and the
-- | simulated machine takes them at once. How a real machine is given
-- | its limits and gains is not decided (spec open question 10).
changeParams :: (Params -> Params) -> State -> State
changeParams change st =
  let
    params = change st.params
  in
    sim (SetParams params) st { params = params }

resetParams :: State -> State
resetParams = changeParams (const Params.defaults)

------------------------------------------------------------
-- Forecast
------------------------------------------------------------

-- | Run the present recipe on a model of the machine with the page's
-- | parameters. The model is the same controller core and the same
-- | simulated plant, run on the page, whatever the page is linked to.
-- | Advice that was worked out for another recipe, or for a model that
-- | forecast this one differently, is dropped.
refreshForecast :: State -> State
refreshForecast st =
  let
    now = forecast st.params (toProfile (allShapes st) st.recipe) (puckOf st.recipe)
    still advice = advice.from == st.recipe && now.seconds == st.forecast.seconds && now.extracted == st.forecast.extracted
  in
    st { forecast = now, advice = st.advice >>= \advice -> if still advice then Just advice else Nothing }

-- | Work out what to change to mend the forecast cup, leaving alone
-- | every setting the barista holds.
askAdvice :: State -> State
askAdvice st =
  let
    free =
      { grind: not (isLocked st Grind)
      , temp: not (isLocked st Temp)
      , soak: not (isLocked st Soak)
      , seconds: not (isLocked st Seconds)
      }
    found = advise st.params (allShapes st) free st.recipe st.forecast
  in
    st
      { advice = found
      , notice = if isNothing found then "No change to the grind, the water or the soak would mend this cup. Free a held setting, or change the dose, the yield or the shape." else ""
      }

-- | Make the change the advice names.
takeAdvice :: State -> State
takeAdvice st = case st.advice of
  Just advice -> applyRecipe advice.recipe st { draft = Nothing }
  Nothing -> st

------------------------------------------------------------
-- Time and the link
------------------------------------------------------------

-- | Let `ms` real milliseconds pass: the backend runs, the page hears
-- | whatever has arrived, and requests that have waited too long for an
-- | answer are given up on.
tick :: Number -> State -> State
tick ms st =
  let
    clock = st.clock + ms
    passed = Backend.advance ms st.backend
    heard = foldl (flip hear) st { clock = clock, backend = passed.backend } passed.heard
    waited = Machine.expire clock heard.machine
  in
    foldl lose heard { machine = waited.machine } waited.lost

-- | Take in one message from the machine. This is all a backend has to
-- | deliver, whether it is the simulated one or a real link.
hear :: Message -> State -> State
hear message st =
  let
    result = Machine.hear st.params.config st.clock message st.machine
    acted = foldl outcome st { machine = result.machine } result.outcomes
  in
    case message of
      -- The machine is there again after a change of recipe went
      -- unanswered: send the recipe as it now stands.
      Report _ | acted.resend -> applyRecipe acted.recipe acted
      _ -> acted

outcome :: State -> Outcome -> State
outcome st = case _ of
  Accepted _ -> st
  Refused pending reason -> case pending.event of
    -- A move of the fader that arrives after the shot has ended is
    -- refused, and that is not news.
    FaderTaken _ -> st
    FaderGiven -> st
    _ -> undo pending st { notice = "Rejected: " <> reason }
  -- The machine changing state by itself is watched in real time, so
  -- a fast-forwarded session does not race past the moment.
  CameToRest -> sim (SetSpeed 1) st

-- | A request was never answered.
lose :: State -> Pending -> State
lose st pending =
  let
    told = st { notice = "No answer from the machine to " <> eventLabel pending.event <> "." }
  in
    case pending.event of
      ProfileSelected _ -> (undo pending told) { resend = true }
      -- The fader is sent many times a second while it is dragged, and
      -- a move that is lost is followed by the next.
      FaderTaken _ -> st
      _ -> told

-- | Go back to the recipe from before a request that did not go
-- | through. Later changes still waiting for an answer were made on top
-- | of it, so they go back to the same recipe if they fail too.
undo :: Pending -> State -> State
undo pending st = case pending.undo of
  Nothing -> st
  Just recipe ->
    refreshForecast
      ( sim (LoadPuck (puckOf recipe)) st
          { recipe = recipe
          , machine = st.machine { pending = map (\p -> p { undo = p.undo $> recipe }) st.machine.pending }
          }
      )

-- | Put the simulated plant at operating temperature. A sleeping
-- | machine is asked to wake as well, because a hot machine that is
-- | asleep still refuses a shot.
skipWarmUp :: State -> State
skipWarmUp st = case st.machine.status.state of
  Faulted -> st { notice = "Clear the fault under Service first." }
  Sleeping -> sim WarmUp (ask (WakeRequested Full) Nothing st { notice = "" })
  _ -> sim WarmUp st { notice = "" }

-- | A cold simulated machine with the present parameters, nothing in
-- | flight on the link, and the present recipe sent to it afresh.
reset :: State -> State
reset st =
  applyRecipe st.recipe
    (sim (Reset st.params) st { machine = Machine.fresh (toProfile (allShapes st) st.recipe), notice = "" })

------------------------------------------------------------
-- The timeline and the fader
------------------------------------------------------------

-- | One step of the shot on the timeline: where the forecast says it
-- | begins and ends, in seconds from the start of the shot.
type Segment = { start :: Number, end :: Number }

-- | The steps of the profile the machine runs, laid out by the forecast.
-- | A step the forecast shot never reached has no length and sits at
-- | the end.
segments :: State -> Array Segment
segments st =
  let
    f = st.forecast
    -- The first step begins with the shot, whenever the forecast took
    -- its first sample.
    starts = mapWithIndex (\i start -> if i == 0 then 0.0 else fromMaybe f.seconds start) f.stepStarts
    ends = drop 1 starts <> [ f.seconds ]
  in
    zipWith (\start end -> { start, end: max start end }) starts ends

-- | How many seconds the timeline shows: whole quarter minutes, with
-- | room after the forecast end, and more if the shot runs on past it.
timelineSpan :: State -> Number
timelineSpan st = toNumber (max 2 (ceil ((max st.forecast.seconds (playheadAt st) + 2.0) / 15.0))) * 15.0

-- | How far into the shot the machine is, in seconds, or nothing when
-- | no shot is running. Between reports it runs on by the page's own
-- | clock.
playheadAt :: State -> Number
playheadAt st = case st.machine.status.shot of
  Just shot -> max 0.0 (st.machine.at - shot.startedAt + min 0.25 (Machine.silence st.clock st.machine / 1000.0))
  Nothing -> 0.0

-- | Where the fader is drawn, as a reading on the scale the selector
-- | has chosen. Under the barista's hand it is where they have dragged
-- | it. Otherwise it is where the machine says the fader is, which
-- | during a profile is where the motor has carried it.
faderReading :: State -> Number
faderReading st =
  let
    scale = faderScale st.machine.status.faderMode
    width = scale.high - scale.low
  in
    case st.hand of
      Just reading -> reading
      -- Drawn a hair to one side after each drop, so the slider is set
      -- again even when the machine's fader has not moved.
      Nothing -> scale.low + st.machine.status.fader * width + (if st.dropped then width / 10000.0 else 0.0)

-- | The fader dragged to a reading on its scale.
-- |
-- | On pressure, during a shot, this takes it in hand: the shot goes to
-- | manual mode and the pressure follows the fader, between the manual
-- | limits the machine holds it to. With no shot running, pushing the
-- | fader up starts one and takes it in hand from the first drop, so
-- | the whole shot is drawn by hand.
-- |
-- | On temperature, during a shot, the water follows the fader and the
-- | profile's steps carry on. With no shot running there is nothing for
-- | it to set.
-- |
-- | On time the fader only shows, and a hand on it does nothing.
-- |
-- | The machine has the last word on all of it: the page asks, and
-- | shows what the machine then reports.
dragFader :: Number -> State -> State
dragFader reading st =
  let
    scale = faderScale st.machine.status.faderMode
    level = clamp scale.low scale.high reading
    taken = ask (FaderTaken level) Nothing st { hand = Just level, notice = "" }
  in
    case st.machine.status.faderMode, st.machine.status.shot of
      OnTime, _ -> st { notice = "The selector is on time. The fader shows how far the shot has run, and cannot be moved." }
      _, Just _ -> taken
      OnTemperature, Nothing -> st { notice = "On temperature the fader sets the water while a shot is running." }
      OnPressure, Nothing ->
        if st.pulled then taken
        else if level >= st.params.config.manualMinBar then
          -- Start the shot, then take the fader. The two requests reach
          -- the machine in that order.
          ask (FaderTaken level) Nothing (request ShotRequested st { hand = Just level, pulled = true })
        else st { hand = Just level }

-- | The fader let go. It stays where it was left: the shot keeps the
-- | pressure or the water it was given until it ends, the fader is
-- | given back to the profile, or the selector is turned.
dropFader :: State -> State
dropFader st = st { hand = Nothing, pulled = false, dropped = not st.dropped }
