-- | Browser storage for saved recipes. Storage can be unavailable (a
-- | page opened from a file in some browsers, private windows), so both
-- | operations swallow the failure and the page carries on without it.
module Sim.Store
  ( load
  , save
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (try)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

load :: String -> Effect (Maybe String)
load key = do
  result <- try (window >>= localStorage >>= getItem key)
  pure case result of
    Right found -> found
    Left _ -> Nothing

save :: String -> String -> Effect Unit
save key value = void (try (window >>= localStorage >>= setItem key value))