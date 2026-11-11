module Hastea.Internal.Foreign.FFI
  ( makeHaskellCallback
  , makeHaskellCallback1
  )
where

import Hastea.Internal.Foreign.Types


-- "wrapper sync" turns a Haskell closure into a JavaScript function that runs
-- synchronously when called (e.g. by the DOM for an event), rather than
-- returning a Promise.

foreign import javascript "wrapper sync"
  makeHaskellCallback :: IO () -> IO JSFunction


foreign import javascript "wrapper sync"
  makeHaskellCallback1 :: (JSObject -> IO ()) -> IO JSFunction
