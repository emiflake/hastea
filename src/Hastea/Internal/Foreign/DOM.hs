-- | The DOM operations Hastea needs.
module Hastea.Internal.Foreign.DOM
  ( DOMNode

    -- * Finding and creating nodes
  , getElementById
  , createElement
  , createTextNode

    -- * Attributes and properties
  , setAttribute
  , removeAttribute
  , setProperty
  , setBoolProperty
  , setText

    -- * The tree
  , appendChild
  , removeAllChildren
  , childAt
  , childNodes
  , placeChildAt
  , replaceWith
  , remove

    -- * Events
  , Event
  , addCaptureListener
  , eventBubbles
  , eventPath
  , preventDefault

    -- * Scheduling
  , newCallback
  , requestAnimationFrame
  )
where

import Data.Coerce
import Hastea.Internal.Foreign.FFI
import Hastea.Internal.Foreign.Types


-- | A node in the DOM.
newtype DOMNode
  = DOMNode JSVal


-- | A DOM event object.
type Event = JSVal


foreign import javascript unsafe "document.getElementById($1)"
  ffi_getElementById :: JSString -> IO JSVal
foreign import javascript unsafe "document.createElement($1)"
  ffi_createElement :: JSString -> IO JSVal
foreign import javascript unsafe "document.createTextNode($1)"
  ffi_createTextNode :: JSString -> IO JSVal
foreign import javascript unsafe "($1).setAttribute($2, $3)"
  ffi_setAttribute :: JSVal -> JSString -> JSString -> IO ()
foreign import javascript unsafe "($1).removeAttribute($2)"
  ffi_removeAttribute :: JSVal -> JSString -> IO ()
foreign import javascript unsafe "($1)[$2] = ($3)"
  ffi_setProperty :: JSVal -> JSString -> JSString -> IO ()


-- `!!` because a Haskell Bool may arrive in JavaScript as a number.
foreign import javascript unsafe "($1)[$2] = !!($3)"
  ffi_setBoolProperty :: JSVal -> JSString -> Bool -> IO ()
foreign import javascript unsafe "($1).data = ($2)" ffi_setText :: JSVal -> JSString -> IO ()
foreign import javascript unsafe "($1).appendChild($2)" ffi_appendChild :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "($1).replaceChildren()" ffi_removeAllChildren :: JSVal -> IO ()
foreign import javascript unsafe "($1).childNodes[$2]" ffi_childAt :: JSVal -> Int -> IO JSVal
foreign import javascript unsafe "Array.from(($1).childNodes)" ffi_childNodes :: JSVal -> IO JSArray


-- Does nothing if the node is already in place, so that an unmoved node keeps
-- its state. Inserting before `undefined` appends.
foreign import javascript unsafe "(() => { const parent = $1, node = $3, current = parent.childNodes[$2]; if (current !== node) parent.insertBefore(node, current ?? null); })()"
  ffi_placeChildAt :: JSVal -> Int -> JSVal -> IO ()
foreign import javascript unsafe "($1).replaceWith($2)" ffi_replaceWith :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "($1).remove()" ffi_remove :: JSVal -> IO ()
foreign import javascript unsafe "($1).addEventListener($2, $3, true)"
  ffi_addCaptureListener :: JSVal -> JSString -> JSFunction -> IO ()
foreign import javascript unsafe "!!($1).bubbles" ffi_eventBubbles :: JSVal -> IO Bool


-- The position of the event's target below a root node: the index of each
-- ancestor among its siblings, from just below the root down to the target.
-- Empty if the target is the root itself or lies outside it.
foreign import javascript unsafe "(() => { const root = $1, path = []; let node = ($2).target; while (node && node !== root) { const parent = node.parentNode; if (!parent) return []; path.push(Array.prototype.indexOf.call(parent.childNodes, node)); node = parent; } return node ? path.reverse() : []; })()"
  ffi_eventPath :: JSVal -> JSVal -> IO JSArray
foreign import javascript unsafe "$1" ffi_toInt :: JSVal -> Int
foreign import javascript unsafe "($1).preventDefault()" ffi_preventDefault :: JSVal -> IO ()
foreign import javascript unsafe "window.requestAnimationFrame($1)"
  ffi_requestAnimationFrame :: JSFunction -> IO ()
foreign import javascript unsafe "($1) == null" ffi_isNull :: JSVal -> Bool


-- FINDING AND CREATING NODES

getElementById :: String -> IO (Maybe DOMNode)
getElementById elemId = nullable <$> ffi_getElementById (toJSString elemId)


createElement :: String -> IO DOMNode
createElement tag =
  DOMNode <$> ffi_createElement (toJSString tag)


createTextNode :: String -> IO DOMNode
createTextNode content =
  DOMNode <$> ffi_createTextNode (toJSString content)


nullable :: JSVal -> Maybe DOMNode
nullable val = if ffi_isNull val then Nothing else Just (DOMNode val)


-- ATTRIBUTES AND PROPERTIES

setAttribute :: DOMNode -> String -> String -> IO ()
setAttribute node attrKey attrValue =
  ffi_setAttribute (coerce node) (toJSString attrKey) (toJSString attrValue)


removeAttribute :: DOMNode -> String -> IO ()
removeAttribute node attrKey =
  ffi_removeAttribute (coerce node) (toJSString attrKey)


-- | Assign a string to a property of a node, e.g. @node.value = "..."@.
setProperty :: DOMNode -> String -> String -> IO ()
setProperty node key value =
  ffi_setProperty (coerce node) (toJSString key) (toJSString value)


-- | Assign a boolean to a property of a node, e.g. @node.checked = true@.
setBoolProperty :: DOMNode -> String -> Bool -> IO ()
setBoolProperty node key value =
  ffi_setBoolProperty (coerce node) (toJSString key) value


-- | Change the content of a text node.
setText :: DOMNode -> String -> IO ()
setText node content =
  ffi_setText (coerce node) (toJSString content)


-- THE TREE

appendChild :: DOMNode -> DOMNode -> IO ()
appendChild parent child =
  ffi_appendChild (coerce parent) (coerce child)


removeAllChildren :: DOMNode -> IO ()
removeAllChildren node = ffi_removeAllChildren (coerce node)


childAt :: DOMNode -> Int -> IO (Maybe DOMNode)
childAt parent index = nullable <$> ffi_childAt (coerce parent) index


-- | The children of a node as they are now; later changes do not affect the list.
childNodes :: DOMNode -> IO [DOMNode]
childNodes parent = map DOMNode . fromJSArray <$> ffi_childNodes (coerce parent)


{- | Make a node the child at the given position, moving or inserting it
unless it is already there.
-}
placeChildAt :: DOMNode -> Int -> DOMNode -> IO ()
placeChildAt parent index child =
  ffi_placeChildAt (coerce parent) index (coerce child)


-- | Put a new node in the place of an old one.
replaceWith :: DOMNode -> DOMNode -> IO ()
replaceWith old new = ffi_replaceWith (coerce old) (coerce new)


-- | Take a node out of the tree.
remove :: DOMNode -> IO ()
remove node = ffi_remove (coerce node)


-- EVENTS

{- | Listen for an event on a node in the capture phase, which also sees
events on its descendants that do not bubble, such as @focus@.
-}
addCaptureListener :: DOMNode -> String -> (Event -> IO ()) -> IO ()
addCaptureListener target event handler = do
  callback <- makeHaskellCallback1 (coerce handler)
  ffi_addCaptureListener (coerce target) (toJSString event) callback


eventBubbles :: Event -> IO Bool
eventBubbles = ffi_eventBubbles


{- | Where the target of an event is below a root node: the index of each
ancestor among its siblings, from just below the root down to the target.
Empty if the target is the root itself or is not inside it.
-}
eventPath :: DOMNode -> Event -> IO [Int]
eventPath root event =
  map ffi_toInt . fromJSArray <$> ffi_eventPath (coerce root) event


preventDefault :: Event -> IO ()
preventDefault = ffi_preventDefault


-- SCHEDULING

-- | Turn an action into a JavaScript function that can be scheduled repeatedly.
newCallback :: IO () -> IO JSFunction
newCallback = makeHaskellCallback


-- | Run a callback before the browser next paints.
requestAnimationFrame :: JSFunction -> IO ()
requestAnimationFrame = ffi_requestAnimationFrame
