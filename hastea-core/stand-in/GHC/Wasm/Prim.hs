{- | A stand-in for the @GHC.Wasm.Prim@ of GHC's wasm backend, which an
ordinary GHC does not have.

It is only part of the package when that is built with an ordinary GHC and
the @editor@ flag, which is how HLS loads it (see @hie.yaml@). It has the
types and the names, so that code typechecks; nothing in it can run.
-}
module GHC.Wasm.Prim
  ( JSVal
  , freeJSVal
  , JSString (..)
  , fromJSString
  , toJSString
  , JSException (..)
  )
where

import Control.Exception (Exception)


-- | A JavaScript value.
data JSVal


freeJSVal :: JSVal -> IO ()
freeJSVal = standIn


-- | A JavaScript string.
newtype JSString = JSString JSVal


fromJSString :: JSString -> String
fromJSString = standIn


toJSString :: String -> JSString
toJSString = standIn


-- | An exception thrown by JavaScript.
newtype JSException = JSException JSVal


instance Show JSException where
  show _ = "JSException"


instance Exception JSException


standIn :: a
standIn = error "This is a stand-in for GHC.Wasm.Prim, which only exists when compiled to WebAssembly."
