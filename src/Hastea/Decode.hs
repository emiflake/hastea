{-# LANGUAGE FlexibleInstances #-}
module Hastea.Decode
  ( Decode
  , runDecode

  -- * Decoders
  , string
  , number
  , list
  , field


  , truthy
  , notNull
  , isNull
  )
  where

import           Control.Applicative

import           Asterius.Types
import           Data.Coerce
import           Unsafe.Coerce



-- TODO: improve error messages


newtype Decode' e a =
  Decode { runDecode :: JSVal -> Either e a }


type Decode = Decode' String


instance Show (Decode' e a) where
  show _ = "Decode{...}"

instance Functor (Decode' String) where
  fmap f (Decode g) =
    Decode $ fmap f . g


instance Applicative (Decode' String) where
  pure x = Decode . const . Right $ x
  fd <*> ad = Decode $ \vf -> liftA2 ($) (runDecode fd vf) (runDecode ad vf)



-- FFI type-checking


-- NOTE: This ***assumes*** that the objects are pure (i.e. doesn't return IO)
-- This behavior is potentially dangerous.


foreign import javascript "typeof (${1}) == 'string'" ffi_isString :: JSVal -> Bool
foreign import javascript "typeof (${1}) == 'number'" ffi_isNumber :: JSVal -> Bool
foreign import javascript "(${1}) instanceof Array" ffi_isArray :: JSVal -> Bool
foreign import javascript "${1} instanceof Object" ffi_isObject :: JSVal -> Bool
foreign import javascript "${1} in ${2}" ffi_keyIsIn :: JSVal -> JSObject -> Bool
foreign import javascript "${1}[${2}]" ffi_keyGet :: JSObject -> JSString -> JSVal
foreign import javascript "(${1}) === null" ffi_isNull :: JSVal -> Bool
foreign import javascript "!!(${1})" ffi_truthy :: JSVal -> Bool



-- FFI wrappers


notNull :: JSVal -> Bool
notNull = not . isNull


isNull :: JSVal -> Bool
isNull = ffi_isNull


truthy :: JSVal -> Bool
truthy = ffi_truthy



-- Primitives


string :: Decode String
string =
  Decode $ \val ->
    if ffi_isString val
    then Right (fromJSString (coerce val))
    else Left "was not string"


number :: Decode Double
number =
  Decode $ \val ->
    if ffi_isNumber val
    then Right (unsafeCoerce val)
    else Left "was not number"


list :: Decode a -> Decode [a]
list elem =
  Decode $ \val ->
    if ffi_isArray val
    then traverse (runDecode elem) (fromJSArray (coerce val))
    else Left "was not array"


field :: String -> Decode a -> Decode a
field key elem =
  Decode $ \val ->
    if ffi_isObject val && (coerce $ toJSString key) `ffi_keyIsIn` (coerce val)
    then runDecode elem . coerce $ (coerce val) `ffi_keyGet` (coerce $ toJSString key)
    else Left "was not object, or object didn't have key"
