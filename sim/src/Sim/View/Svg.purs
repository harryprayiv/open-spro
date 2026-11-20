-- | A small SVG vocabulary over Halogen's namespaced elements.
module Sim.View.Svg
  ( Attr
  , at
  , num
  , svg
  , g
  , rect
  , circle
  , line
  , path
  , label
  , polyline
  ) where

import Prelude

import Data.Number.Format (fixed, toStringWith)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Properties as HP

type Attr = { name :: String, value :: String }

at :: String -> String -> Attr
at name value = { name, value }

-- | A coordinate with one decimal.
num :: Number -> String
num = toStringWith (fixed 1)

ns :: Namespace
ns = Namespace "http://www.w3.org/2000/svg"

node :: forall w i. String -> Array Attr -> Array (HH.HTML w i) -> HH.HTML w i
node name attrs children =
  HH.elementNS ns (ElemName name) (map (\a -> HP.attr (AttrName a.name) a.value) attrs) children

svg :: forall w i. Array Attr -> Array (HH.HTML w i) -> HH.HTML w i
svg = node "svg"

g :: forall w i. Array Attr -> Array (HH.HTML w i) -> HH.HTML w i
g = node "g"

rect :: forall w i. Array Attr -> HH.HTML w i
rect attrs = node "rect" attrs []

circle :: forall w i. Array Attr -> HH.HTML w i
circle attrs = node "circle" attrs []

line :: forall w i. Array Attr -> HH.HTML w i
line attrs = node "line" attrs []

path :: forall w i. Array Attr -> HH.HTML w i
path attrs = node "path" attrs []

polyline :: forall w i. Array Attr -> HH.HTML w i
polyline attrs = node "polyline" attrs []

label :: forall w i. Array Attr -> String -> HH.HTML w i
label attrs content = node "text" attrs [ HH.text content ]