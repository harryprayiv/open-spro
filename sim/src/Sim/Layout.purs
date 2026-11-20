-- | How the page is arranged: three fixed panes. The machine pane shows
-- | the front, the inside and the shot. The controls pane holds every
-- | control behind tabs. The bench pane holds testing and problems and
-- | can be put away.
module Sim.Layout
  ( Tab(..)
  , BenchTab(..)
  , View
  , defaultView
  , tabs
  , tabTitle
  , benchTabs
  , benchTitle
  , encode
  , decode
  ) where

import Prelude

import Data.Array (find)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), joinWith, split)

-- | The tabs of the controls pane.
data Tab = Brew | Cup | Shape | Tuning

derive instance eqTab :: Eq Tab

-- | The tabs of the bench pane.
data BenchTab = Faults | Readings | Scope

derive instance eqBenchTab :: Eq BenchTab

type View =
  { tab :: Tab
  , benchTab :: BenchTab
  , benchOpen :: Boolean
  }

defaultView :: View
defaultView = { tab: Brew, benchTab: Faults, benchOpen: false }

tabs :: Array Tab
tabs = [ Brew, Cup, Shape, Tuning ]

tabTitle :: Tab -> String
tabTitle = case _ of
  Brew -> "Brew"
  Cup -> "Cup"
  Shape -> "Profile"
  Tuning -> "Settings"

benchTabs :: Array BenchTab
benchTabs = [ Faults, Readings, Scope ]

benchTitle :: BenchTab -> String
benchTitle = case _ of
  Faults -> "Problems"
  Readings -> "Readings"
  Scope -> "Scope"

-- | Three words separated by commas. The titles double as the stored
-- | names.
encode :: View -> String
encode view =
  joinWith ","
    [ tabTitle view.tab
    , benchTitle view.benchTab
    , if view.benchOpen then "open" else "away"
    ]

-- | Anything unreadable falls back to the default for that part.
decode :: String -> View
decode text = case split (Pattern ",") text of
  [ tab, benchTab, open ] ->
    { tab: fromMaybe defaultView.tab (find (\t -> tabTitle t == tab) tabs)
    , benchTab: fromMaybe defaultView.benchTab (find (\t -> benchTitle t == benchTab) benchTabs)
    , benchOpen: open == "open"
    }
  _ -> defaultView
