-- | The character on the e-ink display of a sleeping machine.
-- |
-- | There is none by default, and the display shows a plain snore. To
-- | put one there, draw it, save its frames as images in
-- | sim/imgs/screen/, and name them here in `asleep`. Nothing else has
-- | to change: the display shows the frames in turn, in greys whatever
-- | their colours, with the lines of `says` in a speech bubble beside
-- | them, and pressing the display still wakes the machine.
-- |
-- | For example, with asleep-1.png and asleep-2.png in that folder:
-- |
-- |     asleep = Just
-- |       { frames: [ "asleep-1.png", "asleep-2.png" ]
-- |       , secondsPerFrame: 0.8
-- |       , says: [ "press here", "to wake", "the machine" ]
-- |       }
-- |
-- | The picture is fitted into a square on the left of the display
-- | without being stretched, so a square image fills it. The display is
-- | small: about three short lines of speech fit beside the picture.
module Sim.Mascot
  ( Mascot
  , asleep
  , folder
  , frameAt
  ) where

import Prelude

import Data.Array (index, length)
import Data.Int (floor)
import Data.Maybe (Maybe(..))

type Mascot =
  -- the file names of the frames, in the order they are shown, inside
  -- sim/imgs/screen/
  { frames :: Array String
  -- how long each frame stays up
  , secondsPerFrame :: Number
  -- what the character says, a line at a time
  , says :: Array String
  }

-- | The character for the sleeping display, if there is one.
asleep :: Maybe Mascot
asleep = Nothing

-- asleep = Just
--   { frames: [ "https://raw.githubusercontent.com/OWNER/REPO/COMMIT/path/asleep.png" ]
--   , secondsPerFrame: 0.8
--   , says: [ "press here", "to wake", "the machine" ]
--   }

-- asleep = Just
--   { frames: [ "https://raw.githubusercontent.com/markhjorth/nedry/master/nedr-screenshot.png" ]
--   , secondsPerFrame: 0.8
--   , says: [ "press here", "to wake", "the machine" ]
--   }

-- | Where the frames are, as the page sees it.
folder :: String
folder = "../imgs/screen/"

-- | The frame to show at a time in seconds: each in turn, for
-- | `secondsPerFrame`, and round again. Nothing if there are no frames.
frameAt :: Number -> Mascot -> Maybe String
frameAt seconds mascot =
  let
    count = length mascot.frames
    step = floor (seconds / max 0.05 mascot.secondsPerFrame)
  in
    if count == 0 then Nothing else index mascot.frames (step `mod` count)
