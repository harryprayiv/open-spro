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
  , frame
  , skipWarmUp
  ) where

import Prelude

import Data.Array (deleteAt, elem, filter, index, length, modifyAt, range, snoc)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Number (abs)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Sim.Controller (Event(..), shotInProgress)
import Sim.Advice (advise)
import Sim.Forecast (forecast)
import Sim.Model (Field(..), State)
import Sim.Params (Params)
import Sim.Params as Params
import Sim.Profiles as Profiles
import Sim.Recipe (Limits, Recipe, Roast, doseLimits, grindLimits, puckOf, ratioLimits, secondsLimits, soakLimits, targetGrams, targetLimits, tempLimits, toProfile, withRoastDefaults, withTarget)
import Sim.Shapes as Shapes
import Sim.Types (MachineState(..), PowerMode(..), Profile, ShotState(..))
import Sim.World (paramsOf, send, setParams, stepMany, warmUp)

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

-- | Hand a changed recipe to the controller as a profile. The controller
-- | refuses during a shot or a fault, and then the recipe stays as it was.
applyRecipe :: Recipe -> State -> State
applyRecipe recipe st =
  case send (ProfileSelected (toProfile (allShapes st) recipe)) st.world of
    Left reason -> st { notice = "Rejected: " <> reason }
    Right world -> st
      { recipe = recipe
      , world = world { puck = puckOf recipe }
      , notice = ""
      }

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
        f = forecast (paramsOf st.world) (toProfile (allShapes st) r) (puckOf r)
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
  if shotInProgress st.world.ctl then st { notice = "Rejected: a shot is running" }
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

changeParams :: (Params -> Params) -> State -> State
changeParams change st = st { world = setParams (change (paramsOf st.world)) st.world }

resetParams :: State -> State
resetParams = changeParams (const Params.defaults)

------------------------------------------------------------
-- Forecast
------------------------------------------------------------

-- | Run the present recipe on a warm copy of the present machine.
-- | Advice that was worked out for another recipe, or for a machine
-- | that forecast this one differently, is dropped.
refreshForecast :: State -> State
refreshForecast st =
  let
    now = forecast (paramsOf st.world) (toProfile (allShapes st) st.recipe) (puckOf st.recipe)
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
    found = advise (paramsOf st.world) (allShapes st) free st.recipe st.forecast
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
-- Time
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
