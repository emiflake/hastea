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
  , p
  , ul
  , li
  , br
  , button


  -- * Attributes
  , attrib
  , className
  , on
  , onClick

  -- * DOM Rendering
  , runApp
  )
  where

import           Prelude           hiding (div, span)

import           Asterius.Prim
import           Asterius.Types

import           Foreign.StablePtr


import           Control.Monad
import           Data.Coerce
import           Data.IORef
import           Decode
import           System.IO.Unsafe


newtype DOMNode
  = DOMNode JSVal


foreign import javascript "document.getElementById(${1})" ffi_getElementById :: JSString -> IO JSVal
foreign import javascript "(${1}).innerHTML = ${2}" ffi_setInnerHTML :: JSVal -> JSVal -> IO ()
foreign import javascript "(${1}).setAttribute(${2}, ${3})" ffi_setAttribute :: JSVal -> JSString -> JSString -> IO ()
foreign import javascript "document.createElement(${1})" ffi_createElement :: JSString -> IO JSVal
foreign import javascript "document.createTextNode(${1})" ffi_createTextNode :: JSString -> IO JSVal
foreign import javascript "(${1}).appendChild(${2})" ffi_appendChild :: JSVal -> JSVal -> IO ()
foreign import javascript "(${1}).addEventListener(${2},${3})" ffi_addEventListener :: JSVal -> JSString -> JSFunction -> IO ()
foreign import javascript "console.log(${1})" ffi_print :: JSVal -> IO ()

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


addEventListener :: DOMNode -> String -> (JSVal -> IO ()) -> IO ()
addEventListener target event handler = do
  callback <- makeHaskellCallback1 $ coerce handler
  ffi_addEventListener (coerce target) (toJSString event) callback

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
  | VEventListener
  { eventName    :: String
  , eventDecoder :: Decode a
  }
  deriving Show


instance Functor VAttribute where
  fmap f att =
    case att of
      VAttribute attrib -> VAttribute attrib
      listener@VEventListener{eventDecoder} -> listener{eventDecoder = fmap f eventDecoder}


applyAttribute :: (a -> IO ()) -> DOMNode -> VAttribute a -> IO ()
applyAttribute percolate node attribute = case attribute of
  VAttribute attrib                       -> node `setAttrib` attrib
  VEventListener{eventName, eventDecoder} -> addEventListener node eventName cb
    where cb = \val -> do
            case runDecode eventDecoder val of
              Left err -> ffi_print (coerce (toJSString (show ("Err", err))))
              Right a  -> percolate a


data VNode a
  = VNode
  { tagName    :: String
  , attributes :: [VAttribute a]
  , children   :: [VNode a]
  }
  | VText String
  deriving Show


instance Functor VNode where
  fmap f node =
    case node of
      node@VNode { attributes, children } ->
        node { attributes = (fmap . fmap) f attributes, children = (fmap . fmap) f children }
      VText t -> VText t


renderVNode :: (a -> IO ()) -> VNode a -> IO DOMNode
renderVNode percolate VNode{tagName, attributes, children} = do
  el <- createElement tagName
  forM_ attributes $ \attrib -> applyAttribute percolate el attrib
  forM_ children $ \childVirtualNode -> do
    child <- renderVNode percolate childVirtualNode
    el `appendChild` child
  pure el
renderVNode _ (VText text) = createTextNode text



-- HTML DSL


type Html a = VNode a
type Attribute a = VAttribute a



-- Primitives


attrib :: String -> String -> Attribute a
attrib k v =
  VAttribute (Attrib k v)


on :: String -> Decode msg -> Attribute msg
on evt decoder =
  VEventListener evt decoder


node :: String -> [Attribute a] -> [Html a] -> Html a
node = VNode


-- Attributes


className :: String -> Attribute a
className = attrib "class"



-- Events


onClick :: msg -> Attribute msg
onClick msg = on "click" (pure msg)



-- Tags


text :: String -> Html a
text = VText


div :: [Attribute a] -> [Html a] -> Html a
div = node "div"


span :: [Attribute a] -> [Html a] -> Html a
span = node "span"


a :: [Attribute a] -> [Html a] -> Html a
a = node "a"


p :: [Attribute a] -> [Html a] -> Html a
p = node "p"


ul :: [Attribute a] -> [Html a] -> Html a
ul = node "ul"


ol :: [Attribute a] -> [Html a] -> Html a
ol = node "ol"


li :: [Attribute a] -> [Html a] -> Html a
li = node "li"


br :: Html a
br = node "br" [] []


strong :: [Attribute a] -> [Html a] -> Html a
strong = node "strong"


button :: [Attribute a] -> [Html a] -> Html a
button = node "button"



-- RENDER


runApp :: Show msg => (msg -> s -> s) -> s -> (s -> Html msg) -> IO ()
runApp update init view = do
  mainNode <- getElementById "main-node"
  stateRef <- newIORef init

  let render s = do
        let handleEvent evtMsg = do
              ffi_print (coerce (toJSString (show evtMsg)))
              modifyIORef' stateRef (update evtMsg)
              state <- readIORef stateRef
              render state
        mainNode `setInnerHTML` "" -- clears all children, TODO: should diff instead of this
        let rootNodeToRender = view s
        rootNodeRendered <- renderVNode handleEvent rootNodeToRender
        mainNode `appendChild` rootNodeRendered

  render init
