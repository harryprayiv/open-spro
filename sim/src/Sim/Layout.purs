-- | Which panels the page shows, in what order, and which are folded
-- | away. The order is the user's to change and is remembered.
module Sim.Layout
  ( Panel(..)
  , Slot
  , Layout
  , defaultLayout
  , title
  , moveEarlier
  , moveLater
  , toggle
  , encode
  , decode
  ) where

import Prelude

import Data.Array (catMaybes, elem, filter, findIndex, index, length, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)

data Panel
  = Machine
  | RecipePanel
  | ShotPanel
  | InsidePanel
  | Log
  | SensorsPanel
  | ActuatorsPanel
  | InvariantsPanel

derive instance eqPanel :: Eq Panel

type Slot = { panel :: Panel, open :: Boolean }

type Layout = Array Slot

defaultLayout :: Layout
defaultLayout =
  [ { panel: Machine, open: true }
  , { panel: Log, open: true }
  , { panel: RecipePanel, open: true }
  , { panel: ShotPanel, open: true }
  , { panel: InsidePanel, open: true }
  , { panel: SensorsPanel, open: false }
  , { panel: ActuatorsPanel, open: false }
  , { panel: InvariantsPanel, open: false }
  ]

title :: Panel -> String
title = case _ of
  Machine -> "Machine"
  RecipePanel -> "Recipe"
  ShotPanel -> "Shot"
  InsidePanel -> "Inside"
  Log -> "What is happening"
  SensorsPanel -> "Sensors"
  ActuatorsPanel -> "Actuators"
  InvariantsPanel -> "Spec invariants"

code :: Panel -> String
code = case _ of
  Machine -> "machine"
  RecipePanel -> "recipe"
  ShotPanel -> "shot"
  InsidePanel -> "inside"
  Log -> "log"
  SensorsPanel -> "sensors"
  ActuatorsPanel -> "actuators"
  InvariantsPanel -> "invariants"

fromCode :: String -> Maybe Panel
fromCode = case _ of
  "machine" -> Just Machine
  "recipe" -> Just RecipePanel
  "shot" -> Just ShotPanel
  "inside" -> Just InsidePanel
  "log" -> Just Log
  "sensors" -> Just SensorsPanel
  "actuators" -> Just ActuatorsPanel
  "invariants" -> Just InvariantsPanel
  _ -> Nothing

swap :: Int -> Int -> Layout -> Layout
swap i j layout = fromMaybe layout do
  a <- index layout i
  b <- index layout j
  updateAt i b layout >>= updateAt j a

moveEarlier :: Panel -> Layout -> Layout
moveEarlier panel layout = case findIndex (\slot -> slot.panel == panel) layout of
  Just i | i > 0 -> swap i (i - 1) layout
  _ -> layout

moveLater :: Panel -> Layout -> Layout
moveLater panel layout = case findIndex (\slot -> slot.panel == panel) layout of
  Just i | i < length layout - 1 -> swap i (i + 1) layout
  _ -> layout

toggle :: Panel -> Layout -> Layout
toggle panel = map \slot -> if slot.panel == panel then slot { open = not slot.open } else slot

encode :: Layout -> String
encode layout = joinWith "," (map (\slot -> code slot.panel <> (if slot.open then ":1" else ":0")) layout)

-- | Unknown entries are dropped and panels missing from the stored text
-- | are appended, so a layout saved by an older page still works.
decode :: String -> Layout
decode text =
  let
    entry part = case split (Pattern ":") part of
      [ name, flag ] -> fromCode name <#> \panel -> { panel, open: flag == "1" }
      _ -> Nothing
    stored = catMaybes (map entry (split (Pattern ",") text))
    present = map _.panel stored
    missing = filter (\slot -> not (elem slot.panel present)) defaultLayout
  in
    stored <> missing