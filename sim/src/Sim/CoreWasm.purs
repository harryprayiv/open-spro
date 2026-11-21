-- | The controller core's wasm module, as base64 text. The text is in
-- | CoreWasm.js, which is generated from the Rust crate by
-- | `nix run .#core-wasm` and is not edited by hand.
module Sim.CoreWasm (base64) where

foreign import base64 :: String
