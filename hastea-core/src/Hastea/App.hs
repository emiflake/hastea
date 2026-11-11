{- | Applications in the style of The Elm Architecture: a model, a function
that updates it in response to messages, and a view of it.
-}
module Hastea.App
  ( runApp
  )
where

import Control.Monad
import Data.IORef

import Hastea.Cmd
import Hastea.Html
import Hastea.Internal.Foreign.DOM qualified as DOM
import Hastea.Root


{- | Run an application in the element with id @main-node@.

The model only changes through @update@, in response to the messages
produced by the view's event handlers and by commands. The view is shown
again after the model has changed, at most once per frame: when several
messages arrive in quick succession, only the latest model is rendered.
-}
runApp :: (msg -> s -> (s, Cmd msg)) -> s -> (s -> Html msg) -> IO ()
runApp update initial view = do
  model <- newIORef initial
  renderScheduled <- newIORef False
  -- Filled in once the root exists; the root in turn needs 'dispatch'.
  scheduleRender <- newIORef (pure ())

  let dispatch msg = do
        (new, cmd) <- update msg <$> readIORef model
        new `seq` writeIORef model new
        scheduled <- readIORef renderScheduled
        unless scheduled $ do
          writeIORef renderScheduled True
          join (readIORef scheduleRender)
        runCmd cmd >>= mapM_ dispatch

  root <- mount "main-node" dispatch
  let renderNow = do
        writeIORef renderScheduled False
        readIORef model >>= render root . view

  onFrame <- DOM.newCallback renderNow
  writeIORef scheduleRender (DOM.requestAnimationFrame onFrame)
  renderNow
