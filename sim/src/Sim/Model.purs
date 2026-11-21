-- | What the page remembers and what can happen to it.
module Sim.Model
  ( Field(..)
  , Theme(..)
  , Play(..)
  , State
  , Action(..)
  ) where

import Prelude

import Data.Maybe (Maybe)
import Sim.Advice (Advice)
import Sim.Backend (Backend, LinkSettings)
import Sim.Forecast (Forecast)
import Sim.Layout (Edge, Sizes, View)
import Sim.Link (Event)
import Sim.Machine (Machine)
import Sim.Params (Group, Params)
import Sim.Plant (Disturbance)
import Sim.Recipe (Recipe)
import Sim.Types (Profile)
import Web.UIEvent.MouseEvent (MouseEvent)

-- | The recipe settings that have a slider.
data Field = Dose | Ratio | Target | Temp | Seconds | Grind | Soak

derive instance eqField :: Eq Field

data Theme = Day | Night

derive instance eqTheme :: Eq Theme

-- | How faithful the simulator is being.
-- |
-- | Arcade is for playing with it. The lamps are eased so they glow and
-- | fade instead of flickering with their elements, the machine's
-- | display can be pressed to wake the machine, and the warm-up can be
-- | skipped.
-- |
-- | Realistic is for testing against. Nothing is smoothed and nothing
-- | is offered that a real machine could not do: the lamps follow
-- | their outputs exactly, the display is only a display, and the
-- | warm-up takes as long as it takes.
data Play = Arcade | Realistic

derive instance eqPlay :: Eq Play

type State =
  { -- The machine as last heard from. Everything the page shows about
    -- the machine comes from here.
    machine :: Machine
  -- The simulated machine at the far end of the link. The page sends
  -- it requests and simulator commands, lets time pass for it, and
  -- reads the simulator's own view of it. It reads nothing else.
  , backend :: Backend
  -- The page's clock: real milliseconds since it was opened.
  , clock :: Number
  -- The parameters the page last set, which the forecast runs on.
  , params :: Params
  -- A change of recipe went unanswered, so the machine may not have
  -- it. It is sent again when the machine is next heard from.
  , resend :: Boolean
  -- The fader while the barista's hand is on it: the reading, on the
  -- scale the selector has chosen, it has been dragged to. The page shows this at once and the
  -- machine's own report of the fader catches up.
  , hand :: Maybe Number
  -- While the machine heats: when, on the machine's clock, it will be
  -- ready, and when on the page's clock that was worked out. `at` is
  -- Nothing if it would not be ready within the hour.
  , warming :: Maybe { askedAt :: Number, at :: Maybe Number }
  -- The machine has been ready at least once since it was last reset.
  -- From then on a spell of heating, as after a change of profile or
  -- beans, does not take over the machine's display.
  , settled :: Boolean
  -- This drag of the fader has already asked for a shot to start.
  , pulled :: Boolean
  -- Flips each time the fader is let go, so the slider is drawn afresh
  -- and shows where the machine says the fader is.
  , dropped :: Boolean
  , notice :: String
  , recipe :: Recipe
  , saved :: Array Recipe
  -- The user's own profile shapes. They follow the built-in ones.
  , shapes :: Array Profile
  -- The number box being typed in, so a re-render does not overwrite it.
  , draft :: Maybe { key :: String, text :: String }
  , view :: View
  , theme :: Theme
  , play :: Play
  -- The step the profile designer has open.
  , stepOpen :: Int
  -- The group of parameters the settings show.
  , paramGroup :: Group
  -- The note on the coffee map the barista is after, counted round the
  -- wheel from jasmine.
  , wanted :: Maybe Int
  -- The recipe settings the barista holds fixed, by key. The machine's
  -- suggestions leave these alone.
  , locks :: Array String
  -- The sizes set by dragging the edges between things, and the drag
  -- in progress: its edge and where the pointer last was.
  , sizes :: Sizes
  , drag :: Maybe { edge :: Edge, x :: Int, y :: Int }
  -- The pane a phone shows: 0 machine, 1 controls, 2 bench.
  , phone :: Int
  -- What the present recipe would do on the present machine. It is
  -- worked out again whenever the recipe, a shape or a parameter
  -- changes, and not on every frame.
  , forecast :: Forecast
  -- What to change when the forecast cup would not come out right.
  , advice :: Maybe Advice
  }

data Action
  = Initialize
  | Tick
  | Send Event
  | SetSpeed Int
  | Disturb (Disturbance -> Disturbance)
  | SetLink (LinkSettings -> LinkSettings)
  -- The fader dragged to a reading on its scale, and let go.
  | FaderDrag Number
  | FaderDrop
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
  | TogglePlay
  | GutterDown Edge MouseEvent
  | GutterMove MouseEvent
  | GutterUp
