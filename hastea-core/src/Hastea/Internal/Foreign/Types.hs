{- | JavaScript value types used throughout Hastea.

This module used to be provided by Asterius (@Asterius.Types@). It is now a
thin layer over 'GHC.Wasm.Prim' from @ghc-experimental@, which ships with
GHC's WebAssembly backend.
-}
module Hastea.Internal.Foreign.Types
  ( JSVal
  , JSString (..)
  , JSObject (..)
  , JSFunction (..)
  , JSArray (..)
  , toJSString
  , fromJSString
  , fromJSArray
  )
where

import GHC.Wasm.Prim (JSString (..), JSVal, fromJSString, toJSString)


-- | A JavaScript object.
newtype JSObject
  = JSObject JSVal


-- | A JavaScript function, e.g. a callback created from a Haskell closure.
newtype JSFunction
  = JSFunction JSVal


-- | A JavaScript array.
newtype JSArray
  = JSArray JSVal


foreign import javascript unsafe "$1.length" js_arrayLength :: JSArray -> Int
foreign import javascript unsafe "$1[$2]" js_arrayIndex :: JSArray -> Int -> JSVal


-- | Read every element of a JavaScript array.
fromJSArray :: JSArray -> [JSVal]
fromJSArray arr =
  [js_arrayIndex arr i | i <- [0 .. js_arrayLength arr - 1]]
