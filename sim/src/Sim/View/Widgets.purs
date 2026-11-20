-- | The small pieces every panel is built from.
module Sim.View.Widgets
  ( cls
  , n1
  , n2
  , clock
  , section
  , segmented
  , button
  , primary
  , toggle
  , chosen
  , numBox
  , slider
  , legend
  , table
  , row
  , rowIf
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Number.Format (fixed, toStringWith)
import Data.String (trim)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sim.Model (Action(..), State)

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls name = HP.class_ (HH.ClassName name)

n1 :: Number -> String
n1 = toStringWith (fixed 1)

n2 :: Number -> String
n2 = toStringWith (fixed 2)

clock :: Number -> String
clock seconds = n1 (seconds / 60.0) <> " min"

section :: forall w. String -> Array (HH.HTML w Action) -> HH.HTML w Action
section title children =
  HH.div [ cls "section" ]
    ([ HH.span [ cls "section-title" ] [ HH.text title ] ] <> children)

-- | Buttons joined into one control, for a choice of one among several.
segmented :: forall w. Array (HH.HTML w Action) -> HH.HTML w Action
segmented = HH.div [ cls "segmented" ]

button :: forall w. String -> Action -> HH.HTML w Action
button text action =
  HH.button [ HE.onClick \_ -> action ] [ HH.text text ]

-- | The one button a panel is mostly there for.
primary :: forall w. String -> Action -> HH.HTML w Action
primary text action =
  HH.button [ HE.onClick \_ -> action, cls "primary" ] [ HH.text text ]

-- | A switch. An active one is a problem being injected or a valve held
-- | open, so it is drawn lit.
toggle :: forall w. String -> Boolean -> Action -> HH.HTML w Action
toggle text active action =
  HH.button
    [ HE.onClick \_ -> action
    , cls (if active then "active" else "")
    ]
    [ HH.text (text <> ": " <> (if active then "yes" else "no")) ]

-- | One option of a segmented control, lit when it is the present one.
chosen :: forall w. Boolean -> String -> Action -> HH.HTML w Action
chosen active text action =
  HH.button
    [ HE.onClick \_ -> action
    , cls (if active then "active" else "")
    ]
    [ HH.text text ]

-- | A box for an exact number. While it is being typed in it shows the
-- | text as typed; otherwise it shows the value with the given number
-- | of decimals. Every keystroke that reads as a number is applied.
numBox :: forall w. State -> String -> Int -> Number -> (Number -> State -> State) -> HH.HTML w Action
numBox st key places value apply =
  let
    text = case st.draft of
      Just draft | draft.key == key -> draft.text
      _ -> toStringWith (fixed places) value
  in
    HH.input
      [ HP.type_ HP.InputText
      , HP.attr (AttrName "inputmode") "decimal"
      , cls "exact"
      , HP.value text
      , HE.onValueInput \typed -> Typed key typed apply
      , HE.onBlur \_ -> EndDraft
      ]

-- | A slider for coarse moves. It writes the same value as the number
-- | box beside it.
slider :: forall w. Number -> Number -> Number -> Number -> (Number -> State -> State) -> HH.HTML w Action
slider lo hi step value apply =
  HH.input
    [ HP.type_ HP.InputRange
    , HP.min lo
    , HP.max hi
    , HP.step (HP.Step step)
    , HP.value (show value)
    , HE.onValueInput \text -> Change \st -> case Number.fromString (trim text) of
        Just number -> apply number st
        Nothing -> st
    ]

-- | One entry of a chart's key.
legend :: forall w i. String -> String -> HH.HTML w i
legend name text = HH.span [ cls ("key " <> name) ] [ HH.text text ]

table :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
table rows = HH.table_ [ HH.tbody_ rows ]

row :: forall w i. String -> String -> HH.HTML w i
row = rowIf false

-- | A table row, in red when the first argument says it is a problem.
rowIf :: forall w i. Boolean -> String -> String -> HH.HTML w i
rowIf wrong title value =
  HH.tr [ cls (if wrong then "bad" else "") ] [ HH.th_ [ HH.text title ], HH.td_ [ HH.text value ] ]
