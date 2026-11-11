module Hastea.Internal.Foreign.Effects
  ( alert
  , putStrLn
  , warn
  , reportError
  )
where

import Data.Text (Text)
import Prelude hiding (putStrLn)

import Hastea.Internal.Foreign.Types


foreign import javascript unsafe "alert($1)" ffi_alert :: JSString -> IO ()
foreign import javascript unsafe "console.log($1)" ffi_putStrLn :: JSString -> IO ()
foreign import javascript unsafe "console.warn($1)" ffi_warn :: JSString -> IO ()
foreign import javascript unsafe "console.error($1)" ffi_error :: JSString -> IO ()


alert :: Text -> IO ()
alert = ffi_alert . toJSString


putStrLn :: Text -> IO ()
putStrLn = ffi_putStrLn . toJSString


-- | Write a warning to the browser console.
warn :: Text -> IO ()
warn = ffi_warn . toJSString


-- | Write an error to the browser console.
reportError :: Text -> IO ()
reportError = ffi_error . toJSString
