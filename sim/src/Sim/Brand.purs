-- | Whose machine this is: the mark in the corner of the e-ink display.
-- |
-- | By default it is the two characters for shokunin, the craftsman,
-- | set as text. To put a logo there instead, save it as an image in
-- | sim/imgs/screen/ and name it in `picture`, or give a whole address
-- | beginning https:// or http://, exactly as for the frames in
-- | `Sim.Mascot`. Nothing else has to change.
-- |
-- | The mark sits on the black bar along the top of the display, in a
-- | space about twice as wide as it is high. A picture is fitted into
-- | that space without being stretched and is shown in greys, so a
-- | light logo on a clear background reads best.
module Sim.Brand
  ( Logo
  , logo
  ) where

import Data.Maybe (Maybe(..))

type Logo =
  -- the mark as text, used when there is no picture
  { mark :: String
  -- a file name inside sim/imgs/screen/, or a whole address
  , picture :: Maybe String
  }

logo :: Logo
logo =
  { mark: "職人"
  , picture: Nothing
  }
