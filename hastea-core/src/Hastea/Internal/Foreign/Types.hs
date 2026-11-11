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

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign qualified as Foreign
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Wasm.Prim (JSString (..), JSVal)
import GHC.Wasm.Prim qualified as Prim
import System.IO.Unsafe (unsafeDupablePerformIO)


-- Text is UTF-8 in memory, so JavaScript can decode it where it lies. The
-- decoder is made once and kept.
foreign import javascript unsafe "(globalThis.__hasteaDecoder || (globalThis.__hasteaDecoder = new TextDecoder())).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_decode :: Ptr Word8 -> Int -> IO JSString


-- | Text as a JavaScript string.
toJSString :: Text -> JSString
toJSString text =
  unsafeDupablePerformIO (Foreign.useAsPtr text (\bytes size -> js_decode bytes (fromIntegral size)))


-- | A JavaScript string as text.
fromJSString :: JSString -> Text
fromJSString = T.pack . Prim.fromJSString


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
