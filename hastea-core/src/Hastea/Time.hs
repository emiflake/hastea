-- | Commands and subscriptions to do with time.
module Hastea.Time
  ( now
  , after
  , every
  )
where

import Data.Text qualified as T

import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Internal.Foreign.FFI (makeHaskellCallback)
import Hastea.Internal.Foreign.Types
import Hastea.Sub (Sub)
import Hastea.Sub qualified as Sub


foreign import javascript unsafe "Date.now()" ffi_now :: IO Double
foreign import javascript unsafe "setTimeout($1, $2)" ffi_setTimeout :: JSFunction -> Int -> IO ()
foreign import javascript unsafe "setInterval($1, $2)" ffi_setInterval :: JSFunction -> Int -> IO Int
foreign import javascript unsafe "clearInterval($1)" ffi_clearInterval :: Int -> IO ()


-- | Ask for the current time, in milliseconds since the Unix epoch.
now :: (Double -> msg) -> Cmd msg
now toMsg = Cmd.perform (toMsg <$> ffi_now)


-- | Send a message after the given number of milliseconds.
after :: Int -> msg -> Cmd msg
after milliseconds msg = Cmd.custom $ \dispatch -> do
  callback <- makeHaskellCallback (dispatch msg)
  ffi_setTimeout callback milliseconds


{- | Send a message with the current time (in milliseconds since the Unix
epoch) repeatedly, with the given number of milliseconds in between.
-}
every :: Int -> (Double -> msg) -> Sub msg
every milliseconds toMsg =
  Sub.custom ("Hastea.Time.every " <> T.pack (show milliseconds)) start (Just . toMsg)
 where
  start emit = do
    callback <- makeHaskellCallback (ffi_now >>= emit)
    interval <- ffi_setInterval callback milliseconds
    pure (ffi_clearInterval interval)
