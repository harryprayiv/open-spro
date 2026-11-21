-- | The colours of the whole project, in one place.
-- |
-- | `scheme` is the palette of the kissaten press packet: ink and paper,
-- | three greens, two oranges and a gold. Everything else is taken from
-- | it. `roles` says what each colour is used for by day and by night,
-- | and `cssVariables` hands those roles to the stylesheet, so the page
-- | and the drawings cannot drift apart. To change the look of the
-- | project, change `scheme`.
-- |
-- | Not everything reads from here yet. The stylesheet in
-- | sim/dev/index.html still has colours written into its rules for the
-- | charts, the coffee wheel and the top bar, and the plumbing drawing
-- | (Sim.View.Inside) has its own.
module Sim.Palette
  ( Scheme
  , scheme
  , Roles
  , roles
  , cssVariables
  , Leds
  , leds
  ) where

import Prelude

import Data.String (joinWith)
import Sim.Model (Theme(..))

-- | The press packet's palette.
type Scheme =
  { ink :: String
  , paper :: String
  -- the deep green of the packet's rules, the mid green and the sage
  , forest :: String
  , green :: String
  , sage :: String
  -- the blossom orange, and the same burnt darker for use on paper
  , orange :: String
  , burnt :: String
  , gold :: String
  -- the one colour that is not from the packet: something has gone
  -- wrong. It is darker on paper and lighter on ink.
  , alarm :: String
  , alarmOnInk :: String
  }

scheme :: Scheme
scheme =
  { ink: "#17120f"
  , paper: "#f4eedf"
  , forest: "#264a3a"
  , green: "#4d8a60"
  , sage: "#9fbba6"
  , orange: "#e88a22"
  , burnt: "#bc6a14"
  , gold: "#c9a24a"
  , alarm: "#b3261e"
  , alarmOnInk: "#ff7b70"
  }

-- | What the page uses each colour for. The names are the names of the
-- | stylesheet's variables.
type Roles =
  { paper :: String
  , card :: String
  , ink :: String
  , muted :: String
  , faint :: String
  , rule :: String
  , green :: String
  , sage :: String
  , orange :: String
  , burnt :: String
  , gold :: String
  -- a key, what is written on it, and the same for a key that is on
  , key :: String
  , keyInk :: String
  , on :: String
  , onInk :: String
  , line :: String
  , bad :: String
  }

roles :: Theme -> Roles
roles = case _ of
  Day ->
    { paper: scheme.paper
    , card: "#ffffff"
    , ink: scheme.ink
    , muted: "#625a50"
    , faint: "#9c948a"
    , rule: scheme.forest
    , green: scheme.green
    , sage: scheme.sage
    , orange: scheme.orange
    , burnt: scheme.burnt
    , gold: scheme.gold
    , key: "#ffffff"
    , keyInk: scheme.ink
    , on: scheme.forest
    , onInk: "#ffffff"
    , line: "rgba(23, 18, 15, 0.14)"
    , bad: scheme.alarm
    }
  Night ->
    { paper: scheme.ink
    , card: "#211a15"
    , ink: scheme.paper
    , muted: "#b9b0a3"
    , faint: "#8a8176"
    , rule: scheme.sage
    , green: scheme.green
    , sage: scheme.sage
    , orange: scheme.orange
    , burnt: scheme.orange
    , gold: scheme.gold
    , key: "#211a15"
    , keyInk: scheme.paper
    , on: scheme.green
    , onInk: "#ffffff"
    , line: "rgba(244, 238, 223, 0.16)"
    , bad: scheme.alarmOnInk
    }

-- | The roles as CSS variables, for the `style` of the page's outermost
-- | element. Every rule in the stylesheet that says `var(--ink)` and
-- | the like reads these.
cssVariables :: Theme -> String
cssVariables theme =
  let
    r = roles theme
  in
    joinWith "; "
      [ "--paper: " <> r.paper
      , "--card: " <> r.card
      , "--ink: " <> r.ink
      , "--muted: " <> r.muted
      , "--faint: " <> r.faint
      , "--rule: " <> r.rule
      , "--green: " <> r.green
      , "--sage: " <> r.sage
      , "--orange: " <> r.orange
      , "--burnt: " <> r.burnt
      , "--gold: " <> r.gold
      , "--key: " <> r.key
      , "--key-ink: " <> r.keyInk
      , "--on: " <> r.on
      , "--on-ink: " <> r.onInk
      , "--line: " <> r.line
      , "--bad: " <> r.bad
      ]

-- | The lamps on the front of the machine. Each meaning has one colour,
-- | and it is the same by day and by night: a lamp is a light, and does
-- | not change with the room.
type Leds =
  -- something is drawing power or is under way: a heater on, the
  -- machine heating, a shot running
  { working :: String
  -- ready, and waiting to be asked
  , ready :: String
  -- something is wrong
  , fault :: String
  -- a hand has taken over from the profile
  , byHand :: String
  }

leds :: Leds
leds =
  { working: scheme.orange
  , ready: scheme.green
  , fault: scheme.alarmOnInk
  , byHand: scheme.gold
  }
