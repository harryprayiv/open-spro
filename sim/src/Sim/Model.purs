-- | What the page remembers and what can happen to it.
module Sim.Model
  ( Field(..)
  , Theme(..)
  , State
  , Action(..)
  ) where

import Prelude

import Data.Maybe (Maybe)
import Sim.Controller (Event)
import Sim.Forecast (Forecast)
import Sim.Layout (View)
import Sim.Params (Group)
import Sim.Plant (Disturbance)
import Sim.Recipe (Recipe)
import Sim.Types (Profile)
import Sim.World (World)

-- | The recipe settings that have a slider.
data Field = Dose | Ratio | Target | Temp | Seconds | Grind

derive instance eqField :: Eq Field

data Theme = Day | Night

derive instance eqTheme :: Eq Theme

type State =
  { world :: World
  -- Simulated seconds per real second.
  , speed :: Int
  , notice :: String
  , recipe :: Recipe
  , saved :: Array Recipe
  -- The user's own profile shapes. They follow the built-in ones.
  , shapes :: Array Profile
  -- The number box being typed in, so a re-render does not overwrite it.
  , draft :: Maybe { key :: String, text :: String }
  , view :: View
  , theme :: Theme
  -- The step the profile designer has open.
  , stepOpen :: Int
  -- The group of parameters the settings show.
  , paramGroup :: Group
  -- The note on the coffee map the barista is after, counted round the
  -- wheel from jasmine.
  , wanted :: Maybe Int
  -- What the present recipe would do on the present machine. It is
  -- worked out again whenever the recipe, a shape or a parameter
  -- changes, and not on every frame.
  , forecast :: Forecast
  }

data Action
  = Initialize
  | Tick
  | Send Event
  | SetSpeed Int
  | Disturb (Disturbance -> Disturbance)
  | SkipWarmUp
  | Reset
  -- A number box: its key, the text typed, and what to do with a number.
  | Typed String String (Number -> State -> State)
  | EndDraft
  -- A change to the page's state that should then be stored.
  | Change (State -> State)
  | SavePreset
  | DeletePreset String
  | ToggleTheme
