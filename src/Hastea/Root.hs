{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | A place on the page that shows a view and reports what the user does
with it.

This is the layer underneath "Hastea.App". It knows nothing about models or
update functions: you give it a tree to show, as often as you like, and it
gives you the messages that the tree's event handlers produce. Use it to
build other ways of structuring an application.

== How it works

'render' compares the new tree with the one on the page
("Hastea.Html.Diff") and changes only what differs.

No event listener is ever attached to the elements of the view. Instead the
root listens for each kind of event once. When an event arrives, the root
works out where its target is, looks up the same position in the tree it
last rendered, and runs the handlers it finds there and on the ancestors.
Handlers are therefore always those of the latest view.
-}
module Hastea.Root
  ( Root
  , mount
  , render
  )
where

import Control.Monad
import Data.IORef

import Hastea.Decode (runDecode)
import Hastea.Html.Diff (diff)
import Hastea.Html.Internal
import Hastea.Internal.Commit
import Hastea.Internal.Foreign.DOM (DOMNode)
import Hastea.Internal.Foreign.DOM qualified as DOM


-- | A mounted view that produces messages of type @msg@.
data Root msg = Root
  { container :: DOMNode
  , current :: IORef (Maybe (Html msg))
  -- ^ The tree that is on the page.
  , listening :: IORef [String]
  -- ^ The events the container already listens for.
  , dispatch :: msg -> IO ()
  }


{- | Take over the element with the given id. Whatever it contains is removed
at the first 'render'. Messages from event handlers are passed to the given
function.

Fails if there is no such element.
-}
mount :: String -> (msg -> IO ()) -> IO (Root msg)
mount elementId dispatch =
  DOM.getElementById elementId >>= \case
    Nothing -> ioError (userError ("Hastea: there is no element with id " <> show elementId))
    Just container -> do
      current <- newIORef Nothing
      listening <- newIORef []
      pure Root{container, current, listening, dispatch}


-- | Show a tree, changing as little of the page as possible.
render :: Root msg -> Html msg -> IO ()
render root new =
  readIORef (current root) >>= \case
    Nothing -> do
      DOM.removeAllChildren (container root)
      create (listen root) new >>= DOM.appendChild (container root)
      writeIORef (current root) (Just new)
    Just old -> do
      let (patch, new') = diff old new
      DOM.childAt (container root) 0 >>= mapM_ (\node -> commit (listen root) node patch)
      writeIORef (current root) (Just new')


-- Make sure the container listens for an event.
listen :: Root msg -> String -> IO ()
listen root event = do
  events <- readIORef (listening root)
  unless (event `elem` events) $ do
    writeIORef (listening root) (event : events)
    DOM.addCaptureListener (container root) event (handle root event)


-- Run the handlers for an event that happened somewhere inside the container.
handle :: Root msg -> String -> DOM.Event -> IO ()
handle root event val = do
  path <- DOM.eventPath (container root) val
  tree <- readIORef (current root)
  bubbles <- DOM.eventBubbles val
  -- The elements from the target outwards. An event that does not bubble
  -- only concerns its target.
  let targets = (if bubbles then id else take 1) (reverse (along path tree))
  run [[(options, decoder) | Listener name options decoder <- attributes, name == event] | attributes <- targets]
 where
  run [] = pure ()
  run (handlers : outer) = do
    forM_ handlers $ \(options, decoder) -> do
      when (preventDefault options) (DOM.preventDefault val)
      -- A handler whose decoder fails does not apply to this event.
      mapM_ (dispatch root) (runDecode decoder val)
    unless (any (stopPropagation . fst) handlers) (run outer)


{- | The attributes of the elements along a path of child positions,
outermost first. The first position is that of the tree itself, which is the
only child of the container.
-}
along :: [Int] -> Maybe (Html msg) -> [[Attribute msg]]
along (0 : path) (Just tree) = go path (unwrap tree)
 where
  go rest (Element _ attributes children) =
    attributes : case rest of
      index : deeper | child : _ <- drop index children -> go deeper (unwrap child)
      _ -> []
  go _ _ = []
along _ _ = []
