module Hastea.Async
  ( setTimeout
  , defer
  , delay
  )
  where

import           Control.Concurrent
import           Hastea.Internal.Foreign.FFI   (makeHaskellCallback)
import           Hastea.Internal.Foreign.Types

foreign import javascript unsafe "setTimeout($1, $2)" ffi_setTimeout :: JSFunction -> Int -> IO ()

setTimeout :: IO () -> Int -> IO ()
setTimeout action delay = do
  callback <- makeHaskellCallback action
  ffi_setTimeout callback delay

defer :: IO () -> IO ()
defer = (`setTimeout` 0)


delay :: Int -> IO ()
delay = threadDelay
