{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hastea.Internal.VirtualDOM
  ( Attrib (..)
  , VAttribute (..)
  , VNode (..)
  , renderVNode
  , patch
  )
where

import Prelude

import Control.Monad

import Hastea.Decode
import Hastea.Internal.Foreign.DOM (DOMNode)
import Hastea.Internal.Foreign.DOM qualified as DOM


type Percolate a = a -> IO ()


data Attrib
  = Attrib
  { key :: String
  , value :: String
  }
  deriving (Show)


setAttrib :: DOMNode -> Attrib -> IO ()
setAttrib node Attrib{key = "value", value} = DOM.setValue node value -- \*angry mumbling*
setAttrib node Attrib{key, value} = DOM.setAttribute node key value


removeAttrib :: DOMNode -> Attrib -> IO ()
removeAttrib node Attrib{key} = DOM.removeAttribute node key


data VAttribute a
  = VAttribute {unVAttribute :: Attrib}
  | VEventListener
      { eventName :: String
      , eventDecoder :: Decode a
      }
  deriving (Show)


instance Functor VAttribute where
  fmap f att =
    case att of
      VAttribute attrib -> VAttribute attrib
      listener@VEventListener{eventDecoder} -> listener{eventDecoder = fmap f eventDecoder}


data VNode a
  = VNode
      { tagName :: String
      , attributes :: [VAttribute a]
      , children :: [VNode a]
      }
  | VText String
  deriving (Show)


instance Functor VNode where
  fmap f vnode =
    case vnode of
      VNode{attributes, children} ->
        vnode{attributes = (fmap . fmap) f attributes, children = (fmap . fmap) f children}
      VText t -> VText t


applyAttribute :: Percolate a -> DOMNode -> VAttribute a -> IO ()
applyAttribute percolate node attribute =
  case attribute of
    VAttribute attrib -> node `setAttrib` attrib
    VEventListener{eventName, eventDecoder} -> DOM.addEventListener node eventName cb
     where
      cb = \val -> do
        case runDecode eventDecoder val of
          Left _ -> pure () -- TODO somehow throw error
          Right a -> percolate a


removeAttribute :: DOMNode -> VAttribute a -> IO ()
removeAttribute node attribute =
  case attribute of
    VAttribute attrib -> node `removeAttrib` attrib
    -- TODO: remove event listeners, e.g.
    VEventListener{} -> pure ()


-- DOM.removeEventListener node eventName cb
--   where cb = \val ->
--           case runDecode eventDecoder val of
--             Left err -> pure () -- TODO somehow throw error
--             Right a  -> percolate a

updateAttribute :: Percolate a -> DOMNode -> VAttribute a -> IO ()
updateAttribute percolate node attribute =
  -- FIXME: diffing event handlers
  case attribute of
    attribute'@(VAttribute _) -> do
      -- Effects.putStrLn ("Updating attribute " <> show attrib)
      removeAttribute node attribute'
      applyAttribute percolate node attribute'
    _ -> pure ()


updateAttributes :: Percolate a -> DOMNode -> [VAttribute a] -> IO ()
updateAttributes percolate node = mapM_ (updateAttribute percolate node)


renderVNode :: Percolate a -> VNode a -> IO DOMNode
renderVNode percolate VNode{tagName, attributes, children} = do
  el <- DOM.createElement tagName
  forM_ attributes $ \attrib -> applyAttribute percolate el attrib
  forM_ children $ \childVirtualNode -> do
    child <- renderVNode percolate childVirtualNode
    el `DOM.appendChild` child
  pure el
renderVNode _ (VText text) = DOM.createTextNode text


-- VIRTUAL DOM DIFFING

changed :: VNode a -> VNode a -> Bool
changed (VText a) (VText b) = a /= b
changed VNode{tagName = a} VNode{tagName = b} = a /= b
changed _ _ = True


patch :: Percolate a -> DOMNode -> Maybe (VNode a) -> Maybe (VNode a) -> IO ()
patch percolate target old new = patchIndexed percolate target old new 0


-- NON-NODES
patchIndexed :: Percolate a -> DOMNode -> Maybe (VNode a) -> Maybe (VNode a) -> Int -> IO ()
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
patchIndexed _ parent (Just (VText oldText)) (Just (VText newText)) index = do
  -- Effects.putStrLn ("Text change from: \"" <> show oldText <> "\" to \"" <> show newText <> "\"")
  when (oldText /= newText) $
    if index == 0
      then parent `DOM.setTextContent` newText
      else pure ()

-- REPLACE TEXT-NODE WITH TREE-NODE
patchIndexed percolate parent (Just (VText _)) (Just new@VNode{}) _ = do
  parent `DOM.setTextContent` ""
  renderVNode percolate new >>= DOM.appendChild parent

-- REPLACE TREE-NODE WITH TEXT-NODE
patchIndexed percolate parent (Just VNode{}) (Just new@VText{}) _ = do
  cs <- DOM.childCount parent
  forM_ [cs - 1 .. 0] $ \ix -> parent `DOM.removeChildIndex` ix
  renderVNode percolate new >>= DOM.appendChild parent
patchIndexed percolate parent (Just old) (Just new@VNode{attributes = newAttribs}) index = do
  parent `DOM.childAt` index >>= \case
    Nothing -> pure ()
    Just me ->
      if changed old new
        then do
          -- Effects.putStrLn ("Two nodes changed " <> show index)
          renderVNode percolate new >>= \node -> DOM.replaceChild parent node me
        else do
          -- Effects.putStrLn ("Two nodes were the same " <> show index <> " were their attributes the same?")
          updateAttributes percolate me newAttribs
          walkChildren percolate me old new


-- Seriously remove this
tryLookup :: [a] -> Int -> Maybe a
tryLookup xs i =
  case xs of
    (_ : rest) | i > 0 -> tryLookup rest (pred i)
    (x : _) | i == 0 -> Just x
    _ -> Nothing


-- Prefer Vector
walkChildren :: Percolate a -> DOMNode -> VNode a -> VNode a -> IO ()
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
childrenOf :: VNode a -> [VNode a]
childrenOf VNode{children} = children
childrenOf (VText _) = []
