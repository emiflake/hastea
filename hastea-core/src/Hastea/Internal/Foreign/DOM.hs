-- | The DOM operations Hastea needs.
module Hastea.Internal.Foreign.DOM
  ( DOMNode

    -- * Setting up
  , install
  , getElementById
  , unmountPrevious
  , setUnmount
  , clearUnmount

    -- * Building nodes
  , Namespace
  , namespaceInside
  , build

    -- * Attributes and properties
  , setAttribute
  , removeAttribute
  , setProperty
  , setBoolProperty
  , setText

    -- * The tree
  , removeAllChildren
  , childAt
  , parentNode
  , replaceWith
  , Nodes
  , snapshotChildren
  , nodeAt
  , setNodeAt
  , arrange

    -- * Events
  , Event
  , addCaptureListener
  , removeCaptureListener
  , eventBubbles
  , eventPath
  , preventDefault

    -- * Scheduling
  , newCallback
  , freeCallback
  , requestAnimationFrame
  )
where

import Data.Coerce
import Data.Text (Text)
import GHC.Wasm.Prim (freeJSVal)
import Hastea.Internal.Foreign.FFI
import Hastea.Internal.Foreign.Types


-- | A node in the DOM.
newtype DOMNode
  = DOMNode JSVal


-- | A DOM event object.
type Event = JSVal


foreign import javascript unsafe "document.getElementById($1)"
  ffi_getElementById :: JSString -> IO JSVal


-- Inside <foreignObject>, SVG gives way to HTML again.
foreign import javascript unsafe "(($1).localName === 'foreignObject' ? null : ($1).namespaceURI) ?? 'http://www.w3.org/1999/xhtml'"
  ffi_namespaceInside :: JSVal -> IO JSString
foreign import javascript unsafe "($1).parentNode" ffi_parentNode :: JSVal -> IO JSVal
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
foreign import javascript unsafe "($1).replaceChildren()" ffi_removeAllChildren :: JSVal -> IO ()
foreign import javascript unsafe "($1).childNodes[$2]" ffi_childAt :: JSVal -> Int -> IO JSVal
foreign import javascript unsafe "Array.from(($1).childNodes)" ffi_childNodes :: JSVal -> IO JSArray


foreign import javascript unsafe "($1).replaceWith($2)" ffi_replaceWith :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "($1).addEventListener($2, $3, true)"
  ffi_addCaptureListener :: JSVal -> JSString -> JSFunction -> IO ()
foreign import javascript unsafe "($1).removeEventListener($2, $3, true)"
  ffi_removeCaptureListener :: JSVal -> JSString -> JSFunction -> IO ()
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


-- How to take Hastea off an element is kept on the element itself, so that
-- whatever mounts there next can find it: another run of the program in the
-- same page, as in GHCi, included.
foreign import javascript unsafe "($1).__hasteaUnmount?.()" ffi_unmountPrevious :: JSVal -> IO ()
foreign import javascript unsafe "($1).__hasteaUnmount = ($2)" ffi_setUnmount :: JSVal -> JSFunction -> IO ()
foreign import javascript unsafe "if (($1).__hasteaUnmount === ($2)) delete ($1).__hasteaUnmount"
  ffi_clearUnmount :: JSVal -> JSFunction -> IO ()


-- SETTING UP

{- The part of building the page that is done in JavaScript, to keep the
number of calls from Haskell down. It is defined once, by 'install'.

build(namespace, script) creates nodes from a description: records that end
in \\x1e, each starting with a letter that says what it is.

  E tag           start an element
  A key\\x1f value  set an attribute of the element
  S key\\x1f value  assign text to a property of the element
  B key\\x1f 0|1    assign a boolean to a property of the element
  T text          a text node
  C               end the element

Properties are assigned when the element ends: the value of a select can
only be set once its options are there. As in an HTML document, svg and
math start their own namespace, which their descendants share, and inside
foreignObject it is HTML again.

arrange(parent, olds, created, order) makes the children of a node be the
ones listed in order: a number is a position in olds, the children as they
were, and -1 is the next of the nodes in created. Old children that are not
listed are removed.
-}
foreign import javascript unsafe "(() => { \
  \const HTML = 'http://www.w3.org/1999/xhtml'; \
  \const build = (namespace, script) => { \
  \  const nodes = []; const stack = []; let current = null; \
  \  for (const record of script.split('\\x1e')) { \
  \    if (record === '') continue; \
  \    const rest = record.slice(1); \
  \    switch (record[0]) { \
  \      case 'E': { \
  \        const around = current ? current.inside : namespace; \
  \        const own = rest === 'svg' ? 'http://www.w3.org/2000/svg' : rest === 'math' ? 'http://www.w3.org/1998/Math/MathML' : around; \
  \        const node = own === HTML ? document.createElement(rest) : document.createElementNS(own, rest); \
  \        if (current) current.node.appendChild(node); else nodes.push(node); \
  \        stack.push(current); \
  \        current = { node, inside: rest === 'foreignObject' ? HTML : own, properties: null }; \
  \        break; } \
  \      case 'A': { const at = rest.indexOf('\\x1f'); current.node.setAttribute(rest.slice(0, at), rest.slice(at + 1)); break; } \
  \      case 'S': case 'B': { if (!current.properties) current.properties = []; current.properties.push(record); break; } \
  \      case 'T': { const node = document.createTextNode(rest); if (current) current.node.appendChild(node); else nodes.push(node); break; } \
  \      case 'C': { \
  \        if (current.properties) for (const property of current.properties) { \
  \          const at = property.indexOf('\\x1f'); const value = property.slice(at + 1); \
  \          current.node[property.slice(1, at)] = property[0] === 'S' ? value : value === '1'; } \
  \        current = stack.pop(); break; } \
  \    } \
  \  } \
  \  return nodes; }; \
  \const arrange = (parent, olds, created, order) => { \
  \  const wanted = []; const used = new Set(); let next = 0; \
  \  if (order !== '') for (const part of order.split(',')) { \
  \    const index = Number(part); \
  \    if (index < 0) wanted.push(created[next++]); else { wanted.push(olds[index]); used.add(index); } } \
  \  for (let index = 0; index < olds.length; index++) if (!used.has(index)) olds[index].remove(); \
  \  let cursor = parent.firstChild; \
  \  for (const node of wanted) { if (node === cursor) cursor = cursor.nextSibling; else parent.insertBefore(node, cursor); } }; \
  \globalThis.__hastea = { build, arrange }; })()"
  ffi_install :: IO ()
foreign import javascript unsafe "globalThis.__hastea.build($1, $2)"
  ffi_build :: JSString -> JSString -> IO JSArray
foreign import javascript unsafe "globalThis.__hastea.arrange($1, $2, $3, $4)"
  ffi_arrange :: JSVal -> JSArray -> JSArray -> JSString -> IO ()
foreign import javascript unsafe "($1)[$2]" ffi_nodeAt :: JSArray -> Int -> IO JSVal
foreign import javascript unsafe "($1)[$2] = ($3)" ffi_setNodeAt :: JSArray -> Int -> JSVal -> IO ()


-- | Define the JavaScript that 'build' and 'arrange' call. Do this first.
install :: IO ()
install = ffi_install


getElementById :: Text -> IO (Maybe DOMNode)
getElementById elemId = nullable <$> ffi_getElementById (toJSString elemId)


{- | Call the function that 'setUnmount' left on an element, if there is one.
It runs synchronously, so whatever it takes down is gone when this returns.
-}
unmountPrevious :: DOMNode -> IO ()
unmountPrevious node = ffi_unmountPrevious (coerce node)


-- | Leave on an element the function that takes Hastea off it again.
setUnmount :: DOMNode -> JSFunction -> IO ()
setUnmount node callback = ffi_setUnmount (coerce node) callback


-- | Forget the function that 'setUnmount' left on an element, if it is still this one.
clearUnmount :: DOMNode -> JSFunction -> IO ()
clearUnmount node callback = ffi_clearUnmount (coerce node) callback


-- BUILDING NODES

-- | The namespace an element is created in: HTML, SVG or MathML.
type Namespace = Text


-- | The namespace that the children of a node are in, unless they say otherwise.
namespaceInside :: DOMNode -> IO Namespace
namespaceInside node = fromJSString <$> ffi_namespaceInside (coerce node)


{- | Create nodes from a description of them, in a single call. See the
comment on the JavaScript above for what a description looks like.
-}
build :: Namespace -> Text -> IO Nodes
build namespace script = Nodes <$> ffi_build (toJSString namespace) (toJSString script)


nullable :: JSVal -> Maybe DOMNode
nullable val = if ffi_isNull val then Nothing else Just (DOMNode val)


-- ATTRIBUTES AND PROPERTIES

setAttribute :: DOMNode -> Text -> Text -> IO ()
setAttribute node attrKey attrValue =
  ffi_setAttribute (coerce node) (toJSString attrKey) (toJSString attrValue)


removeAttribute :: DOMNode -> Text -> IO ()
removeAttribute node attrKey =
  ffi_removeAttribute (coerce node) (toJSString attrKey)


-- | Assign a string to a property of a node, e.g. @node.value = "..."@.
setProperty :: DOMNode -> Text -> Text -> IO ()
setProperty node key value =
  ffi_setProperty (coerce node) (toJSString key) (toJSString value)


-- | Assign a boolean to a property of a node, e.g. @node.checked = true@.
setBoolProperty :: DOMNode -> Text -> Bool -> IO ()
setBoolProperty node key value =
  ffi_setBoolProperty (coerce node) (toJSString key) value


-- | Change the content of a text node.
setText :: DOMNode -> Text -> IO ()
setText node content =
  ffi_setText (coerce node) (toJSString content)


removeAllChildren :: DOMNode -> IO ()
removeAllChildren node = ffi_removeAllChildren (coerce node)


childAt :: DOMNode -> Int -> IO (Maybe DOMNode)
childAt parent index = nullable <$> ffi_childAt (coerce parent) index


parentNode :: DOMNode -> IO (Maybe DOMNode)
parentNode node = nullable <$> ffi_parentNode (coerce node)


-- | Some nodes, kept on the JavaScript side.
newtype Nodes = Nodes JSArray


-- | The children of a node as they are now; later changes do not affect it.
snapshotChildren :: DOMNode -> IO Nodes
snapshotChildren parent = Nodes <$> ffi_childNodes (coerce parent)


nodeAt :: Nodes -> Int -> IO DOMNode
nodeAt (Nodes nodes) index = DOMNode <$> ffi_nodeAt nodes index


setNodeAt :: Nodes -> Int -> DOMNode -> IO ()
setNodeAt (Nodes nodes) index node = ffi_setNodeAt nodes index (coerce node)


{- | Make the children of a node be the ones listed, in that order. Each
entry of the list is separated by a comma: a number is a position among the
children as they were, and @-1@ stands for the next of the created nodes.
Children that are not listed are removed.
-}
arrange :: DOMNode -> Nodes -> Nodes -> Text -> IO ()
arrange parent (Nodes olds) (Nodes created) order =
  ffi_arrange (coerce parent) olds created (toJSString order)


-- | Put a new node in the place of an old one.
replaceWith :: DOMNode -> DOMNode -> IO ()
replaceWith old new = ffi_replaceWith (coerce old) (coerce new)


-- EVENTS

{- | Listen for an event on a node in the capture phase, which also sees
events on its descendants that do not bubble, such as @focus@.
-}
addCaptureListener :: DOMNode -> Text -> (Event -> IO ()) -> IO JSFunction
addCaptureListener target event handler = do
  callback <- makeHaskellCallback1 (coerce handler)
  ffi_addCaptureListener (coerce target) (toJSString event) callback
  pure callback


-- | Stop listening, with the function that 'addCaptureListener' returned.
removeCaptureListener :: DOMNode -> Text -> JSFunction -> IO ()
removeCaptureListener target event callback =
  ffi_removeCaptureListener (coerce target) (toJSString event) callback


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


{- | Let go of a callback made from Haskell, and of everything it refers to.
JavaScript must not call it afterwards.
-}
freeCallback :: JSFunction -> IO ()
freeCallback (JSFunction val) = freeJSVal val


-- | Run a callback before the browser next paints.
requestAnimationFrame :: JSFunction -> IO ()
requestAnimationFrame = ffi_requestAnimationFrame
