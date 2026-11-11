{- | Run the decoders of "Hastea.Html.Decode" against JavaScript values.

This module re-exports "Hastea.Html.Decode", so it is all you need to import
to both build and run decoders.
-}
module Hastea.Decode
  ( module Hastea.Html.Decode
  , Decode
  , runDecode

    -- * Inspecting JavaScript values
  , truthy
  , notNull
  , isNull
  )
where

import Data.Text (Text)
import Hastea.Html.Decode
import Hastea.Internal.Foreign.Types


-- | The former name of 'Decoder'.
type Decode = Decoder


-- FFI type-checking

-- NOTE: This ***assumes*** that the objects are pure (i.e. doesn't return IO)
-- This behavior is potentially dangerous.

foreign import javascript unsafe "typeof ($1) == 'string'" ffi_isString :: JSVal -> Bool
foreign import javascript unsafe "typeof ($1) == 'number'" ffi_isNumber :: JSVal -> Bool
foreign import javascript unsafe "typeof ($1) == 'boolean'" ffi_isBoolean :: JSVal -> Bool
foreign import javascript unsafe "($1) instanceof Array" ffi_isArray :: JSVal -> Bool
foreign import javascript unsafe "$1 instanceof Object" ffi_isObject :: JSVal -> Bool
foreign import javascript unsafe "$1 in $2" ffi_keyIsIn :: JSString -> JSVal -> Bool
foreign import javascript unsafe "$1[$2]" ffi_keyGet :: JSVal -> JSString -> JSVal


-- Loose equality on purpose: also treats `undefined` as null
-- (e.g. `childNodes[i]` past the end).
foreign import javascript unsafe "($1) == null" ffi_isNull :: JSVal -> Bool
foreign import javascript unsafe "$1" ffi_toNumber :: JSVal -> Double
foreign import javascript unsafe "$1" ffi_toString :: JSVal -> JSString
foreign import javascript unsafe "!!($1)" ffi_truthy :: JSVal -> Bool


-- FFI wrappers

notNull :: JSVal -> Bool
notNull = not . isNull


isNull :: JSVal -> Bool
isNull = ffi_isNull


truthy :: JSVal -> Bool
truthy = ffi_truthy


-- | Run a decoder against a JavaScript value.
runDecode :: Decoder a -> JSVal -> Either Text a
runDecode = decodeWith jsSource


jsSource :: Source JSVal
jsSource =
  Source
    { string = \val ->
        if ffi_isString val then Just (fromJSString (ffi_toString val)) else Nothing
    , number = \val ->
        if ffi_isNumber val then Just (ffi_toNumber val) else Nothing
    , bool = \val ->
        if ffi_isBoolean val then Just (ffi_truthy val) else Nothing
    , isNull = ffi_isNull
    , list = \val ->
        if ffi_isArray val then Just (fromJSArray (JSArray val)) else Nothing
    , field = \key val ->
        let jsKey = toJSString key
         in if ffi_isObject val && jsKey `ffi_keyIsIn` val
              then Just (val `ffi_keyGet` jsKey)
              else Nothing
    }
