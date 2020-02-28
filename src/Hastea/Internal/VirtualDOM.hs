{-# LANGUAGE NamedFieldPuns #-}
module Hastea.Internal.VirtualDOM
  ( Attrib(..)
  , VAttribute(..)
  , VNode(..)
  , renderVNode
  )
  where

import           Prelude

import           Control.Monad

import           Asterius.Prim
import           Asterius.Types

import           Hastea.Decode
import           Hastea.Internal.Foreign.DOM


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
              Left err -> pure () -- TODO somehow throw error
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
