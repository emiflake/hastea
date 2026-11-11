{-# LANGUAGE LambdaCase #-}

module Hastea.Internal.VirtualDOM
  ( renderVNode
  , patch
  )
where

import Prelude

import Control.Monad
import Data.Maybe (mapMaybe)

import Hastea.Decode (runDecode)
import Hastea.Html.Internal
import Hastea.Internal.Foreign.DOM (DOMNode)
import Hastea.Internal.Foreign.DOM qualified as DOM


type Percolate a = a -> IO ()


applyAttribute :: Percolate a -> DOMNode -> Attribute a -> IO ()
applyAttribute percolate node = \case
  Attribute key value -> DOM.setAttribute node key value
  BoolAttribute key True -> DOM.setAttribute node key ""
  BoolAttribute key False -> DOM.removeAttribute node key
  Property key (PropertyString value) -> DOM.setProperty node key value
  Property key (PropertyBool value) -> DOM.setBoolProperty node key value
  Listener event options decoder ->
    DOM.addEventListener node event $ \val -> do
      when (preventDefault options) (DOM.preventDefault val)
      when (stopPropagation options) (DOM.stopPropagation val)
      case runDecode decoder val of
        Left _ -> pure () -- TODO somehow throw error
        Right a -> percolate a


-- | Undo an attribute that is no longer present on a node.
clearAttribute :: DOMNode -> Attribute a -> IO ()
clearAttribute node = \case
  Attribute key _ -> DOM.removeAttribute node key
  BoolAttribute key _ -> DOM.removeAttribute node key
  Property key (PropertyString _) -> DOM.setProperty node key ""
  Property key (PropertyBool _) -> DOM.setBoolProperty node key False
  -- TODO: remove event listeners
  Listener{} -> pure ()


updateAttributes :: Percolate a -> DOMNode -> [Attribute a] -> [Attribute a] -> IO ()
updateAttributes percolate node old new = do
  let newKeys = mapMaybe attributeKey new
  forM_ old $ \attribute ->
    case attributeKey attribute of
      Just key | key `notElem` newKeys -> clearAttribute node attribute
      _ -> pure ()
  forM_ new $ \case
    -- FIXME: diffing event handlers
    Listener{} -> pure ()
    attribute -> applyAttribute percolate node attribute


renderVNode :: Percolate a -> Html a -> IO DOMNode
renderVNode percolate (Element tagName attributes children) = do
  el <- DOM.createElement tagName
  forM_ attributes $ \attrib -> applyAttribute percolate el attrib
  forM_ children $ \childVirtualNode -> do
    child <- renderVNode percolate childVirtualNode
    el `DOM.appendChild` child
  pure el
renderVNode _ (Text text) = DOM.createTextNode text


-- VIRTUAL DOM DIFFING

patch :: Percolate a -> DOMNode -> Maybe (Html a) -> Maybe (Html a) -> IO ()
patch percolate target old new = patchIndexed percolate target old new 0


-- NON-NODES
patchIndexed :: Percolate a -> DOMNode -> Maybe (Html a) -> Maybe (Html a) -> Int -> IO ()
patchIndexed _ _ Nothing Nothing _ = pure ()
-- NEW NODE
patchIndexed percolate parent Nothing (Just new) _ = do
  -- Effects.putStrLn ("Creating node " <> show new)
  renderVNode percolate new >>= DOM.appendChild parent

-- DELETE NODE
patchIndexed _ parent (Just _) Nothing index = do
  -- Effects.putStrLn ("Removing node " <> show index)
  parent `DOM.removeChildIndex` index

-- TEXT CHANGE
patchIndexed _ parent (Just (Text oldText)) (Just (Text newText)) index = do
  -- Effects.putStrLn ("Text change from: \"" <> show oldText <> "\" to \"" <> show newText <> "\"")
  when (oldText /= newText) $
    if index == 0
      then parent `DOM.setTextContent` newText
      else pure ()

-- REPLACE TEXT-NODE WITH TREE-NODE
patchIndexed percolate parent (Just (Text _)) (Just new@Element{}) _ = do
  parent `DOM.setTextContent` ""
  renderVNode percolate new >>= DOM.appendChild parent

-- REPLACE TREE-NODE WITH TEXT-NODE
patchIndexed percolate parent (Just Element{}) (Just new@Text{}) _ = do
  cs <- DOM.childCount parent
  forM_ [cs - 1 .. 0] $ \ix -> parent `DOM.removeChildIndex` ix
  renderVNode percolate new >>= DOM.appendChild parent
patchIndexed
  percolate
  parent
  (Just old@(Element oldTag oldAttribs _))
  (Just new@(Element newTag newAttribs _))
  index = do
    parent `DOM.childAt` index >>= \case
      Nothing -> pure ()
      Just me ->
        if oldTag /= newTag
          then do
            -- Effects.putStrLn ("Two nodes changed " <> show index)
            renderVNode percolate new >>= \node -> DOM.replaceChild parent node me
          else do
            -- Effects.putStrLn ("Two nodes were the same " <> show index <> " were their attributes the same?")
            updateAttributes percolate me oldAttribs newAttribs
            walkChildren percolate me old new


-- Seriously remove this
tryLookup :: [a] -> Int -> Maybe a
tryLookup xs i =
  case xs of
    (_ : rest) | i > 0 -> tryLookup rest (pred i)
    (x : _) | i == 0 -> Just x
    _ -> Nothing


-- Prefer Vector
walkChildren :: Percolate a -> DOMNode -> Html a -> Html a -> IO ()
walkChildren percolate target old new = do
  -- Effects.putStrLn ("Walking children, old length: " <> show oldLength <> " new length: " <> show newLength)
  if oldLength > newLength
    then do
      walkIndices [0 .. (newLength - 1)]
      walkIndices (reverse [newLength .. (oldLength - 1)])
    else
      walkIndices [0 .. (newLength - 1)]
 where
  walkIndices ixs = do
    forM_ ixs $ \ix -> do
      -- Effects.putStrLn (show (ix, children old `tryLookup` ix, children new `tryLookup` ix))
      patchIndexed percolate target (oldChildren `tryLookup` ix) (newChildren `tryLookup` ix) ix

  oldChildren = childrenOf old
  newChildren = childrenOf new
  oldLength = length oldChildren
  newLength = length newChildren


-- | The children of a node; text nodes have none.
childrenOf :: Html a -> [Html a]
childrenOf (Element _ _ children) = children
childrenOf (Text _) = []
