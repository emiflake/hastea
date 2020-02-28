module Hastea.Internal.Foreign.DOM
  ( DOMNode
  , setInnerHTML
  , getElementById
  , createElement
  , createTextNode
  , setAttribute
  , addEventListener
  , appendChild
  )
  where


import           Asterius.Prim
import           Asterius.Types
import           Data.Coerce
import           Foreign.StablePtr


foreign import javascript "document.getElementById(${1})" ffi_getElementById :: JSString -> IO JSVal
foreign import javascript "(${1}).innerHTML = ${2}" ffi_setInnerHTML :: JSVal -> JSVal -> IO ()
foreign import javascript "(${1}).setAttribute(${2}, ${3})" ffi_setAttribute :: JSVal -> JSString -> JSString -> IO ()
foreign import javascript "document.createElement(${1})" ffi_createElement :: JSString -> IO JSVal
foreign import javascript "document.createTextNode(${1})" ffi_createTextNode :: JSString -> IO JSVal
foreign import javascript "(${1}).appendChild(${2})" ffi_appendChild :: JSVal -> JSVal -> IO ()
foreign import javascript "(${1}).addEventListener(${2},${3})" ffi_addEventListener :: JSVal -> JSString -> JSFunction -> IO ()
foreign import javascript "console.log(${1})" ffi_print :: JSVal -> IO ()



-- FFI Wrappers


-- Represent DOM node


newtype DOMNode
  = DOMNode JSVal


getElementById :: String -> IO DOMNode
getElementById elemId =
  DOMNode <$> ffi_getElementById (toJSString elemId)


setInnerHTML :: DOMNode -> String -> IO ()
setInnerHTML node text =
  ffi_setInnerHTML (coerce node) (coerce (toJSString text))


createElement :: String -> IO DOMNode
createElement string =
  DOMNode <$> ffi_createElement (toJSString string)


createTextNode :: String -> IO DOMNode
createTextNode string =
  DOMNode <$> ffi_createTextNode (toJSString string)


appendChild :: DOMNode -> DOMNode -> IO ()
appendChild parent child =
  ffi_appendChild (coerce parent) (coerce child)


setAttribute :: DOMNode -> String -> String -> IO ()
setAttribute node attrKey attrValue =
  ffi_setAttribute (coerce node) (toJSString attrKey) (toJSString attrValue)


addEventListener :: DOMNode -> String -> (JSVal -> IO ()) -> IO ()
addEventListener target event handler = do
  callback <- makeHaskellCallback1 $ coerce handler
  ffi_addEventListener (coerce target) (toJSString event) callback
