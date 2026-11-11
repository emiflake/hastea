{-# LANGUAGE LambdaCase #-}

{- | The effectful half of rendering: carry out a 'Patch' against the DOM.

No event listeners are attached to the nodes created here. Events are
handled by "Hastea.Root", which only needs to be told which events occur in
the tree.
-}
module Hastea.Internal.Commit
  ( create
  , commit
  )
where

import Control.Monad

import Hastea.Html.Diff
import Hastea.Html.Internal
import Hastea.Internal.Foreign.DOM (DOMNode)
import Hastea.Internal.Foreign.DOM qualified as DOM


-- | Called with the name of each event that some element listens for.
type Listen = String -> IO ()


-- | Build the DOM node for a tree.
create :: Listen -> Html msg -> IO DOMNode
create listen tree = case unwrap tree of
  Text content -> DOM.createTextNode content
  Element tag attributes children -> do
    node <- DOM.createElement tag
    forM_ attributes $ \case
      Listener event _ _ -> listen event
      attribute -> set node attribute
    forM_ children (create listen >=> DOM.appendChild node)
    pure node
  -- 'unwrap' leaves only the two cases above.
  _ -> DOM.createTextNode ""


{- | Apply a patch to a DOM node. Returns the node that is now in its place:
the same one, unless it had to be replaced.
-}
commit :: Listen -> DOMNode -> Patch msg -> IO DOMNode
commit listen node = \case
  Keep -> pure node
  Replace tree -> do
    new <- create listen tree
    DOM.replaceWith node new
    pure new
  SetText content -> node <$ DOM.setText node content
  Update changes children -> do
    forM_ changes $ \case
      Set attribute -> set node attribute
      Clear attribute -> clear node attribute
      Listen event -> listen event
    commitChildren listen node children
    pure node


commitChildren :: Listen -> DOMNode -> Children msg -> IO ()
commitChildren listen parent = \case
  InPlace patches ->
    forM_ (zip [0 ..] patches) $ \case
      -- Most children are usually unchanged: do not even look them up.
      (_, Keep) -> pure ()
      (index, patch) -> DOM.childAt parent index >>= mapM_ (\child -> commit listen child patch)
  Rearrange ops -> do
    olds <- DOM.childNodes parent
    news <- forM ops $ \case
      Reuse index patch -> commit listen (olds !! index) patch
      Create tree -> create listen tree
    let reused = [index | Reuse index _ <- ops]
    forM_ (zip [0 ..] olds) $ \(index, old) ->
      when (index `notElem` reused) (DOM.remove old)
    forM_ (zip [0 ..] news) $ \(index, new) -> DOM.placeChildAt parent index new


set :: DOMNode -> Attribute msg -> IO ()
set node = \case
  Attribute key value -> DOM.setAttribute node key value
  BoolAttribute key True -> DOM.setAttribute node key ""
  BoolAttribute key False -> DOM.removeAttribute node key
  Property key (PropertyString value) -> DOM.setProperty node key value
  Property key (PropertyBool value) -> DOM.setBoolProperty node key value
  Listener{} -> pure ()


clear :: DOMNode -> Attribute msg -> IO ()
clear node = \case
  Attribute key _ -> DOM.removeAttribute node key
  BoolAttribute key _ -> DOM.removeAttribute node key
  Property key (PropertyString _) -> DOM.setProperty node key ""
  Property key (PropertyBool _) -> DOM.setBoolProperty node key False
  Listener{} -> pure ()
