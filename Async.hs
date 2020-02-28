module Async
  ( setTimeout
  , defer
  , delay
  )
  where

import           Asterius.Prim
import           Asterius.Types
import           Control.Concurrent
import           Data.Coerce

foreign import javascript "setTimeout(${1}, ${2})" ffi_setTimeout :: JSFunction -> Int -> IO ()

setTimeout :: IO () -> Int -> IO ()
setTimeout action delay = do
  callback <- makeHaskellCallback $ coerce action
  ffi_setTimeout callback delay

defer :: IO () -> IO ()
defer = (`setTimeout` 0)


delay :: Int -> IO ()
delay = threadDelay
