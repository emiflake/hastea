module Hastea.Internal.Foreign.Effects
  ( alert
  , putStrLn
  )
where

import Prelude hiding (putStrLn)

import Hastea.Internal.Foreign.Types


foreign import javascript unsafe "alert($1)" ffi_alert :: JSString -> IO ()
foreign import javascript unsafe "console.log($1)" ffi_putStrLn :: JSString -> IO ()


alert :: String -> IO ()
alert = ffi_alert . toJSString


putStrLn :: String -> IO ()
putStrLn = ffi_putStrLn . toJSString
