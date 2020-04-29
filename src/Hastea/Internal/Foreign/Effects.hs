module Hastea.Internal.Foreign.Effects
  ( alert
  , putStrLn
  )
  where

import           Prelude        hiding (alert, putStrLn)


import           Asterius.Types


foreign import javascript "alert($1)" ffi_alert :: JSString -> IO ()
foreign import javascript "console.log($1)" ffi_putStrLn :: JSString -> IO ()


alert :: String -> IO ()
alert = ffi_alert . toJSString


putStrLn :: String -> IO ()
putStrLn = ffi_putStrLn . toJSString
