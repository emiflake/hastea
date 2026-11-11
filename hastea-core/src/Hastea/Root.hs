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

== When something goes wrong

A view or an update function that fails does not take the page down. The
failure is written to the browser console as an error, and the page stays
as it was: a view is computed completely before anything on the page is
changed. Likely mistakes, such as two children with the same key or an
event that its handler could not make sense of, are written to the console
as warnings, each one once.
-}
module Hastea.Root
  ( Root
  , mount
  , render
  )
where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad
import Data.IORef

import Hastea.Cmd (runCmd)
import Hastea.Component.Internal
import Hastea.Decode (runDecode)
import Hastea.Html.Diff (diff, force)
import Hastea.Html.Internal
import Hastea.Internal.Commit
import Hastea.Internal.Foreign.DOM (DOMNode)
import Hastea.Internal.Foreign.DOM qualified as DOM
import Hastea.Internal.Foreign.Effects qualified as Console
import Hastea.Internal.Foreign.Types (JSFunction)
import Hastea.Internal.Sub (Manager, newManager, sync)


-- | A mounted view that produces messages of type @msg@.
data Root msg = Root
  { container :: DOMNode
  , dispatch :: msg -> IO ()
  , given :: IORef (Maybe (Html msg))
  -- ^ The tree that was last asked for.
  , current :: IORef (Maybe (Html (Routed msg)))
  -- ^ What is on the page: an element standing for the container, whose
  -- children are the tree asked for with each component replaced by its
  -- view.
  , components :: IORef (Store msg)
  -- ^ The state of the components in the tree.
  , subscriptions :: Manager (Routed msg)
  -- ^ The running subscriptions of the components.
  , listening :: IORef [String]
  -- ^ The events the container already listens for.
  , warned :: IORef [String]
  -- ^ The warnings already given.
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
      warned <- newIORef []
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
              , warned
              , refreshScheduled
              , onFrame
              }
      writeIORef refresh $ do
        writeIORef refreshScheduled False
        readIORef given >>= mapM_ (render root)
      pure root


-- | Show a tree, changing as little of the page as possible.
render :: Root msg -> Html msg -> IO ()
render root tree = guarded root "The view could not be shown" $ do
  writeIORef root.given (Just tree)
  expansion <- expand tree <$> readIORef root.components
  old <-
    readIORef root.current >>= \case
      Just old -> pure old
      Nothing -> containerWith [] <$ DOM.removeAllChildren root.container
  let (patch, new) = diff old (containerWith [expansion.tree])
  -- Compute the view completely first. If it fails, it does so here, with
  -- the page and the state of the components still as they were.
  evaluate (force patch)
  writeIORef root.components expansion.store
  void (commit (listen root) root.container patch)
  writeIORef root.current (Just new)
  mapM_ (warn root) expansion.warnings
  sync (route root) root.subscriptions (subscriptionsOf expansion.store)
  runCmd (route root) expansion.effects
 where
  -- The tree may be a fragment, so it is treated as the children of an
  -- element that stands for the container.
  containerWith = Element "" [] . flatten


-- Pass a message on: out of the root, or to the component it is for.
route :: Root msg -> Routed msg -> IO ()
route root message = guarded root "A message could not be handled" $ case message of
  Out msg -> root.dispatch msg
  Local path msg -> do
    store <- readIORef root.components
    -- A message for a component that is gone is dropped.
    delivered <- evaluate (deliver path msg store)
    forM_ delivered $ \(store', cmd) -> do
      writeIORef root.components store'
      -- The component may want other subscriptions in its new state.
      sync (route root) root.subscriptions (subscriptionsOf store')
      scheduled <- readIORef root.refreshScheduled
      unless scheduled $ do
        writeIORef root.refreshScheduled True
        DOM.requestAnimationFrame root.onFrame
      runCmd (route root) cmd


-- Run an action, and if it fails, say so in the console and carry on.
guarded :: Root msg -> String -> IO () -> IO ()
guarded _ what action =
  try action >>= \case
    Right () -> pure ()
    Left failure -> Console.reportError ("Hastea: " <> what <> ": " <> displayException (failure :: SomeException))


-- Point out a likely mistake, once.
warn :: Root msg -> String -> IO ()
warn root warning = do
  given <- readIORef root.warned
  unless (warning `elem` given) $ do
    writeIORef root.warned (warning : given)
    Console.warn ("Hastea: " <> warning)


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
      case runDecode decoder val of
        Right message -> route root message
        Left problem ->
          warn root ("A handler for the " <> show event <> " event ignored it, because it could not read the event: " <> problem)
    unless (any (\(options, _) -> options.stopPropagation) handlers) (run outer)


{- | The attributes of the elements along a path of child positions below
the container, outermost first.
-}
along :: [Int] -> Maybe (Html msg) -> [[Attribute msg]]
along path (Just tree) = drop 1 (go path (unwrap tree))
 where
  go rest (Element _ attributes children) =
    attributes : case rest of
      index : deeper | child : _ <- drop index children -> go deeper (unwrap child)
      _ -> []
  go _ _ = []
along _ Nothing = []
