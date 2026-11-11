{-# LANGUAGE LambdaCase #-}

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

The root also keeps the state of the components in the view
("Hastea.Component"). When a component changes its state, the root shows
the last tree it was given again, by itself.
-}
module Hastea.Root
  ( Root
  , mount
  , render
  )
where

import Control.Monad
import Data.IORef

import Hastea.Cmd (runCmd)
import Hastea.Component.Internal
import Hastea.Decode (runDecode)
import Hastea.Html.Diff (diff)
import Hastea.Html.Internal
import Hastea.Internal.Commit
import Hastea.Internal.Foreign.DOM (DOMNode)
import Hastea.Internal.Foreign.DOM qualified as DOM
import Hastea.Internal.Foreign.Types (JSFunction)
import Hastea.Internal.Sub (Manager, newManager, sync)


-- | A mounted view that produces messages of type @msg@.
data Root msg = Root
  { container :: DOMNode
  , dispatch :: msg -> IO ()
  , given :: IORef (Maybe (Html msg))
  -- ^ The tree that was last asked for.
  , current :: IORef (Maybe (Html (Routed msg)))
  -- ^ The tree that is on the page: the one asked for, with each component
  -- replaced by its view.
  , components :: IORef (Store msg)
  -- ^ The state of the components in the tree.
  , subscriptions :: Manager (Routed msg)
  -- ^ The running subscriptions of the components.
  , listening :: IORef [String]
  -- ^ The events the container already listens for.
  , refreshScheduled :: IORef Bool
  , onFrame :: JSFunction
  -- ^ Shows the tree again, after a component has changed.
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
      given <- newIORef Nothing
      current <- newIORef Nothing
      components <- newIORef emptyStore
      subscriptions <- newManager
      listening <- newIORef []
      refreshScheduled <- newIORef False
      -- The callback needs the root, which needs the callback.
      refresh <- newIORef (pure ())
      onFrame <- DOM.newCallback (join (readIORef refresh))
      let root =
            Root
              { container
              , dispatch
              , given
              , current
              , components
              , subscriptions
              , listening
              , refreshScheduled
              , onFrame
              }
      writeIORef refresh $ do
        writeIORef refreshScheduled False
        readIORef given >>= mapM_ (render root)
      pure root


-- | Show a tree, changing as little of the page as possible.
render :: Root msg -> Html msg -> IO ()
render root tree = do
  writeIORef root.given (Just tree)
  (new, store) <- expand tree <$> readIORef root.components
  writeIORef root.components store
  readIORef root.current >>= \case
    Nothing -> do
      DOM.removeAllChildren root.container
      create (listen root) new >>= DOM.appendChild root.container
      writeIORef root.current (Just new)
    Just old -> do
      let (patch, new') = diff old new
      DOM.childAt root.container 0 >>= mapM_ (\node -> commit (listen root) node patch)
      writeIORef root.current (Just new')
  sync (route root) root.subscriptions (subscriptionsOf store)


-- Pass a message on: out of the root, or to the component it is for.
route :: Root msg -> Routed msg -> IO ()
route root = \case
  Out msg -> root.dispatch msg
  Local path msg -> do
    store <- readIORef root.components
    -- A message for a component that is gone is dropped.
    forM_ (deliver path msg store) $ \(store', cmd) -> do
      writeIORef root.components store'
      -- The component may want other subscriptions in its new state.
      sync (route root) root.subscriptions (subscriptionsOf store')
      scheduled <- readIORef root.refreshScheduled
      unless scheduled $ do
        writeIORef root.refreshScheduled True
        DOM.requestAnimationFrame root.onFrame
      runCmd (route root) cmd


-- Make sure the container listens for an event.
listen :: Root msg -> String -> IO ()
listen root event = do
  events <- readIORef root.listening
  unless (event `elem` events) $ do
    writeIORef root.listening (event : events)
    DOM.addCaptureListener root.container event (handle root event)


-- Run the handlers for an event that happened somewhere inside the container.
handle :: Root msg -> String -> DOM.Event -> IO ()
handle root event val = do
  path <- DOM.eventPath root.container val
  tree <- readIORef root.current
  bubbles <- DOM.eventBubbles val
  -- The elements from the target outwards. An event that does not bubble
  -- only concerns its target.
  let targets = (if bubbles then id else take 1) (reverse (along path tree))
  run [[(options, decoder) | Listener name options decoder <- attributes, name == event] | attributes <- targets]
 where
  run [] = pure ()
  run (handlers : outer) = do
    forM_ handlers $ \(options, decoder) -> do
      when options.preventDefault (DOM.preventDefault val)
      -- A handler whose decoder fails does not apply to this event.
      mapM_ (route root) (runDecode decoder val)
    unless (any (\(options, _) -> options.stopPropagation) handlers) (run outer)


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
