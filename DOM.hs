{-# LANGUAGE NamedFieldPuns #-}
module DOM
  ( DOMNode
  , getElementById
  , setInnerHTML
  , createElement
  , appendChild

  -- * DOM Manipulation
  , Html
  , Attribute

  , text
  , span
  , div
  , strong
  , a


  -- * Attributes
  , attrib
  , className

  -- * DOM Rendering
  , renderRootNode
  )
  where

import           Prelude          hiding (div, span)

import           Asterius.Types
import           Control.Monad
import           Data.Coerce
import           System.IO.Unsafe


newtype DOMNode
  = DOMNode JSVal


foreign import javascript "document.getElementById(${1})" ffi_getElementById :: JSString -> IO JSVal
foreign import javascript "(${1}).innerHTML = ${2}" ffi_setInnerHTML :: JSVal -> JSVal -> IO ()
foreign import javascript "(${1}).setAttribute(${2}, ${3})" ffi_setAttribute :: JSVal -> JSString -> JSString -> IO ()
foreign import javascript "document.createElement(${1})" ffi_createElement :: JSString -> IO JSVal
foreign import javascript "document.createTextNode(${1})" ffi_createTextNode :: JSString -> IO JSVal
foreign import javascript "(${1}).appendChild(${2})" ffi_appendChild :: JSVal -> JSVal -> IO ()



-- FFI Wrappers

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



-- Virtual DOM


data Attrib
  = Attrib
  { key   :: String
  , value :: String
  }
  deriving Show


setAttrib :: DOMNode -> Attrib -> IO ()
setAttrib node Attrib{key, value} = setAttribute node key value


data VAttribute a
  = VAttribute { unVAttribute :: Attrib }
  deriving Show


data VNode a
  = VNode
  { tagName    :: String
  , attributes :: [VAttribute a]
  , children   :: [VNode a]
  }
  | VText String
  deriving Show


renderVNode :: VNode a -> IO DOMNode
renderVNode VNode{tagName, attributes, children} = do
  el <- createElement tagName
  forM_ attributes $ \attrib -> el `setAttrib` (unVAttribute attrib)
  forM_ children $ \childVirtualNode -> do
    child <- renderVNode childVirtualNode
    el `appendChild` child
  pure el
renderVNode (VText text) = createTextNode text



-- HTML DSL


type Html a = VNode a
type Attribute a = VAttribute a


attrib :: String -> String -> Attribute a
attrib k v = VAttribute (Attrib k v)


node :: String -> [Attribute a] -> [Html a] -> Html a
node = VNode


className :: String -> Attribute a
className = attrib "class"


text :: String -> Html a
text = VText


div :: [Attribute a] -> [Html a] -> Html a
div = node "div"


span :: [Attribute a] -> [Html a] -> Html a
span = node "span"


a :: [Attribute a] -> [Html a] -> Html a
a = node "a"


strong :: [Attribute a] -> [Html a] -> Html a
strong = node "strong"



-- RENDER


renderRootNode :: Html a -> IO ()
renderRootNode vRoot = do
  mainNode <- getElementById "main-node"
  root <- renderVNode vRoot
  mainNode `appendChild` root
