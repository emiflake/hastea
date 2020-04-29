module Hastea.Internal.Foreign.FFI
  ( makeHaskellCallback1 )
  where

import Asterius.Types

foreign import javascript "wrapper"
  makeHaskellCallback1 :: (JSObject -> IO ()) -> IO JSFunction
