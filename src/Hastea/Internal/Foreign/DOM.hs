{-# LANGUAGE LambdaCase #-}

module Hastea.Internal.Foreign.DOM
  ( DOMNode
  , setInnerHTML
  , getElementById
  , createElement
  , createTextNode
  , setAttribute
  , setProperty
  , setBoolProperty
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
  , requestAnimationFrame
  , preventDefault
  , stopPropagation
  )
where

import Data.Coerce
import Hastea.Decode (notNull)
import Hastea.Internal.Foreign.FFI
import Hastea.Internal.Foreign.Types


foreign import javascript unsafe "document.getElementById($1)"
  ffi_getElementById :: JSString -> IO JSVal
foreign import javascript unsafe "($1).innerHTML = $2" ffi_setInnerHTML :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "($1).setAttribute($2, $3)"
  ffi_setAttribute :: JSVal -> JSString -> JSString -> IO ()
foreign import javascript unsafe "($1)[$2] = ($3)"
  ffi_setProperty :: JSVal -> JSString -> JSString -> IO ()


-- `!!` because a Haskell Bool may arrive in JavaScript as a number.
foreign import javascript unsafe "($1)[$2] = !!($3)"
  ffi_setBoolProperty :: JSVal -> JSString -> Bool -> IO ()
foreign import javascript unsafe "($1).removeAttribute($2)"
  ffi_removeAttribute :: JSVal -> JSString -> IO ()
foreign import javascript unsafe "document.createElement($1)"
  ffi_createElement :: JSString -> IO JSVal
foreign import javascript unsafe "document.createTextNode($1)"
  ffi_createTextNode :: JSString -> IO JSVal
foreign import javascript unsafe "($1).appendChild($2)" ffi_appendChild :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "($1).addEventListener($2,$3)"
  ffi_addEventListener :: JSVal -> JSString -> JSFunction -> IO ()
foreign import javascript unsafe "($1).removeEventListener($2,$3)"
  ffi_removeEventListener :: JSVal -> JSString -> JSFunction -> IO ()
foreign import javascript unsafe "($1).childNodes[$2]" ffi_childAt :: JSVal -> Int -> IO JSVal
foreign import javascript unsafe "($1).removeChild($2)" ffi_removeChild :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "($1).textContent = ($2)"
  ffi_setTextContent :: JSVal -> JSString -> IO ()
foreign import javascript unsafe "($1).childElementCount" ffi_childElementCount :: JSVal -> IO Int
foreign import javascript unsafe "($1).replaceChild($2, $3)"
  ffi_replaceChild :: JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe "window.requestAnimationFrame($1)"
  ffi_requestAnimationFrame :: JSFunction -> IO ()
foreign import javascript unsafe "($1).preventDefault()" ffi_preventDefault :: JSVal -> IO ()
foreign import javascript unsafe "($1).stopPropagation()" ffi_stopPropagation :: JSVal -> IO ()


-- FFI Wrappers

-- Represent DOM node

newtype DOMNode
  = DOMNode JSVal


getElementById :: String -> IO DOMNode
getElementById elemId =
  DOMNode <$> ffi_getElementById (toJSString elemId)


requestAnimationFrame :: IO () -> IO ()
requestAnimationFrame handler = do
  callback <- makeHaskellCallback1 $ \_ -> coerce handler
  ffi_requestAnimationFrame callback


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
createElement tag =
  DOMNode <$> ffi_createElement (toJSString tag)


createTextNode :: String -> IO DOMNode
createTextNode content =
  DOMNode <$> ffi_createTextNode (toJSString content)


appendChild :: DOMNode -> DOMNode -> IO ()
appendChild parent child =
  ffi_appendChild (coerce parent) (coerce child)


setAttribute :: DOMNode -> String -> String -> IO ()
setAttribute node attrKey attrValue =
  ffi_setAttribute (coerce node) (toJSString attrKey) (toJSString attrValue)


-- | Assign a string to a property of a node, e.g. @node.value = "..."@.
setProperty :: DOMNode -> String -> String -> IO ()
setProperty node key value =
  ffi_setProperty (coerce node) (toJSString key) (toJSString value)


-- | Assign a boolean to a property of a node, e.g. @node.checked = true@.
setBoolProperty :: DOMNode -> String -> Bool -> IO ()
setBoolProperty node key value =
  ffi_setBoolProperty (coerce node) (toJSString key) value


-- | Prevent the default action of an event.
preventDefault :: JSVal -> IO ()
preventDefault = ffi_preventDefault


-- | Stop an event from propagating to ancestors.
stopPropagation :: JSVal -> IO ()
stopPropagation = ffi_stopPropagation


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
