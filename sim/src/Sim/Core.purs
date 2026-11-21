-- | The controller core as PureScript sees it: an opaque state and one
-- | function that runs an operation on it. The core is the Rust crate
-- | in firmware/spro-core, built for wasm32. The same crate is what the
-- | microcontroller runs.
-- |
-- | A `State` is a value. Running an operation gives a new one and
-- | leaves the old one usable, so the page, every forecast shot and
-- | every scenario can each hold a machine of their own.
module Sim.Core
  ( State
  , Reply
  , noState
  , call
  , bufferLen
  , rejectText
  ) where

import Sim.CoreWasm (base64)

-- | The whole of a controller's memory, as bytes only the core reads.
foreign import data State :: Type

-- | What an operation gives back: 0 or the number of the reason it was
-- | refused, the controller's state afterwards, and its status as a
-- | flat run of numbers.
type Reply = { code :: Int, state :: State, status :: Array Number }

-- | The state to hand to the operation that makes a controller.
foreign import noState :: State

foreign import callImpl :: String -> State -> Int -> Number -> Number -> Array Number -> Reply

foreign import bufferLenImpl :: String -> Int

foreign import rejectTextImpl :: String -> Int -> String

-- | Run the operation numbered `op` with its two plain arguments and
-- | the rest of its message.
call :: State -> Int -> Number -> Number -> Array Number -> Reply
call = callImpl base64

-- | The most numbers a message may hold.
bufferLen :: Int
bufferLen = bufferLenImpl base64

rejectText :: Int -> String
rejectText = rejectTextImpl base64
