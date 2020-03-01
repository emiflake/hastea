{-# LANGUAGE LambdaCase #-}
module Hastea.Internal.Foreign.DOM
  ( DOMNode
  , setInnerHTML
  , getElementById
  , createElement
  , createTextNode
  , setAttribute
  , removeAttribute
  , addEventListener
  , removeEventListener
  , appendChild
  , childAt
  , removeChild
  , removeChildIndex
  , setTextContent
  , childCount
  , replaceChild
  )
  where


import           Asterius.Prim
import           Asterius.Types
import           Data.Coerce
import           Foreign.StablePtr
import           Hastea.Decode

foreign import javascript "document.getElementById(${1})" ffi_getElementById :: JSString -> IO JSVal
foreign import javascript "(${1}).innerHTML = ${2}" ffi_setInnerHTML :: JSVal -> JSVal -> IO ()
foreign import javascript "(${1}).setAttribute(${2}, ${3})" ffi_setAttribute :: JSVal -> JSString -> JSString -> IO ()
foreign import javascript "(${1}).removeAttribute(${2})" ffi_removeAttribute :: JSVal -> JSString -> IO ()
foreign import javascript "document.createElement(${1})" ffi_createElement :: JSString -> IO JSVal
foreign import javascript "document.createTextNode(${1})" ffi_createTextNode :: JSString -> IO JSVal
foreign import javascript "(${1}).appendChild(${2})" ffi_appendChild :: JSVal -> JSVal -> IO ()
foreign import javascript "(${1}).addEventListener(${2},${3})" ffi_addEventListener :: JSVal -> JSString -> JSFunction -> IO ()
foreign import javascript "(${1}).removeEventListener(${2},${3})" ffi_removeEventListener :: JSVal -> JSString -> JSFunction -> IO ()
foreign import javascript "console.log(${1})" ffi_print :: JSVal -> IO ()
foreign import javascript "(${1}).childNodes[${2}]" ffi_childAt :: JSVal -> Int -> IO JSVal
foreign import javascript "(${1}).removeChild(${2})" ffi_removeChild :: JSVal -> JSVal -> IO ()
foreign import javascript "(${1}).textContent = (${2})" ffi_setTextContent :: JSVal -> JSString -> IO ()
foreign import javascript "(${1}).childElementCount" ffi_childElementCount :: JSVal -> IO Int
foreign import javascript "(${1}).replaceChild(${2}, ${3})" ffi_replaceChild :: JSVal -> JSVal -> JSVal -> IO ()


-- FFI Wrappers


-- Represent DOM node


newtype DOMNode
  = DOMNode JSVal


getElementById :: String -> IO DOMNode
getElementById elemId =
  DOMNode <$> ffi_getElementById (toJSString elemId)


childAt :: DOMNode -> Int -> IO (Maybe DOMNode)
childAt parent index =
  ffi_childAt (coerce parent) index >>= \maybeChild ->
    if notNull maybeChild
    then pure . Just . DOMNode $ maybeChild
    else pure Nothing


childCount :: DOMNode -> IO Int
childCount node =
  ffi_childElementCount (coerce node)


removeChild :: DOMNode -> DOMNode -> IO ()
removeChild parent child =
  ffi_removeChild (coerce parent) (coerce child)


replaceChild :: DOMNode -> DOMNode -> DOMNode -> IO ()
replaceChild parent old new =
  ffi_replaceChild (coerce parent) (coerce old) (coerce new)


removeChildIndex :: DOMNode -> Int -> IO ()
removeChildIndex parent index =
  childAt parent index >>= \case
    Nothing -> pure ()
    Just childNode -> removeChild parent childNode


setInnerHTML :: DOMNode -> String -> IO ()
setInnerHTML node text =
  ffi_setInnerHTML (coerce node) (coerce (toJSString text))

setTextContent :: DOMNode -> String -> IO ()
setTextContent node text =
  ffi_setTextContent (coerce node) (coerce (toJSString text))



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

removeAttribute :: DOMNode -> String -> IO ()
removeAttribute node attrKey =
  ffi_removeAttribute (coerce node) (toJSString attrKey)


addEventListener :: DOMNode -> String -> (JSVal -> IO ()) -> IO ()
addEventListener target event handler = do
  callback <- makeHaskellCallback1 $ coerce handler
  ffi_addEventListener (coerce target) (toJSString event) callback

removeEventListener :: DOMNode -> String -> (JSVal -> IO ()) -> IO ()
removeEventListener target event handler = do
  callback <- makeHaskellCallback1 $ coerce handler
  ffi_removeEventListener (coerce target) (toJSString event) callback
