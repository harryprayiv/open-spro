-- | A small SVG vocabulary over Halogen's namespaced elements.
module Sim.View.Svg
  ( Attr
  , at
  , num
  , svg
  , g
  , defs
  , rect
  , circle
  , line
  , path
  , label
  , polyline
  , texture
  , fitted
  , sheen
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

defs :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
defs = node "defs" []

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

-- | A repeating image fill, for use inside `defs`. Shapes refer to it
-- | with `fill="url(#name)"`.
texture :: forall w i. String -> String -> Number -> HH.HTML w i
texture name file tile =
  node "pattern"
    [ at "id" name, at "patternUnits" "userSpaceOnUse"
    , at "width" (num tile), at "height" (num tile)
    ]
    [ node "image"
        [ at "href" file, at "width" (num tile), at "height" (num tile)
        , at "preserveAspectRatio" "xMidYMid slice"
        ]
        []
    ]

-- | One image shrunk to cover a single rectangle, for use inside
-- | `defs`. The rectangle is the one the fill is meant for: its corner,
-- | width and height. The image keeps its proportions and is cropped
-- | where it overhangs.
fitted :: forall w i. String -> String -> Number -> Number -> Number -> Number -> HH.HTML w i
fitted name file x y w h =
  node "pattern"
    [ at "id" name, at "patternUnits" "userSpaceOnUse"
    , at "x" (num x), at "y" (num y), at "width" (num w), at "height" (num h)
    ]
    [ node "image"
        [ at "href" file, at "width" (num w), at "height" (num h)
        , at "preserveAspectRatio" "xMidYMid slice"
        ]
        []
    ]

-- | A top-to-bottom light falloff, for use inside `defs`, that gives a
-- | flat textured face some depth.
sheen :: forall w i. String -> HH.HTML w i
sheen name =
  node "linearGradient"
    [ at "id" name, at "x1" "0", at "y1" "0", at "x2" "0", at "y2" "1" ]
    [ node "stop" [ at "offset" "0", at "stop-color" "#ffffff", at "stop-opacity" "0.45" ] []
    , node "stop" [ at "offset" "0.5", at "stop-color" "#ffffff", at "stop-opacity" "0" ] []
    , node "stop" [ at "offset" "1", at "stop-color" "#000000", at "stop-opacity" "0.28" ] []
    ]