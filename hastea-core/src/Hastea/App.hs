{- | Applications in the style of The Elm Architecture: a model, a function
that updates it in response to messages, and a view of it.

'runApp' is all a simple application needs. 'App' and 'run' add the
optional parts: an initial command, subscriptions, and where on the page to
go.
-}
module Hastea.App
  ( runApp
  , runView

    -- * Applications with more parts
  , App (..)
  , app
  , run
  )
where

import Control.Monad
import Data.IORef
import Data.Void (Void, absurd)

import Hastea.Cmd
import Hastea.Html
import Hastea.Internal.Foreign.DOM qualified as DOM
import Hastea.Internal.Sub (newManager, sync)
import Hastea.Root
import Hastea.Sub (Sub)
import Hastea.Sub qualified as Sub


-- | Everything that makes up an application.
data App model msg = App
  { initial :: (model, Cmd msg)
  -- ^ The model to start with, and a command to run at the start.
  , update :: msg -> model -> (model, Cmd msg)
  -- ^ The next model when a message arrives, and a command to run.
  , view :: model -> Html msg
  -- ^ What to show for a model.
  , subscriptions :: model -> Sub msg
  -- ^ What to listen to, besides the view, while the model is like this.
  , container :: String
  -- ^ The id of the element to show the view in.
  }


{- | An application with only the required parts: no subscriptions, nothing
to do at the start, shown in the element with id @main-node@. Use record
syntax to add the rest:

@
run (app update initial view){subscriptions = subscriptions}
@
-}
app :: (msg -> model -> (model, Cmd msg)) -> model -> (model -> Html msg) -> App model msg
app update initial view =
  App
    { initial = (initial, none)
    , update = update
    , view = view
    , subscriptions = const Sub.none
    , container = "main-node"
    }


{- | Run an application in the element with id @main-node@.

The model only changes through @update@, in response to the messages
produced by the view's event handlers and by commands. The view is shown
again after the model has changed, at most once per frame: when several
messages arrive in quick succession, only the latest model is rendered.
-}
runApp :: (msg -> model -> (model, Cmd msg)) -> model -> (model -> Html msg) -> IO ()
runApp update initial view = run (app update initial view)


{- | Show a view that has no model behind it, in the element with id
@main-node@. This is all it takes when the state of the page lives in
components ("Hastea.Component"), which look after themselves.
-}
runView :: Html Void -> IO ()
runView view = do
  root <- mount "main-node" absurd
  render root view


-- | Run an application.
run :: App model msg -> IO ()
run application = do
  let (initial, initialCmd) = application.initial
  model <- newIORef initial
  running <- newManager
  renderScheduled <- newIORef False
  -- Filled in once the root exists; the root in turn needs 'dispatch'.
  scheduleRender <- newIORef (pure ())

  let dispatch msg = do
        (new, cmd) <- application.update msg <$> readIORef model
        new `seq` writeIORef model new
        scheduled <- readIORef renderScheduled
        unless scheduled $ do
          writeIORef renderScheduled True
          join (readIORef scheduleRender)
        sync dispatch running (application.subscriptions new)
        runCmd dispatch cmd

  root <- mount application.container dispatch
  let renderNow = do
        writeIORef renderScheduled False
        readIORef model >>= render root . application.view

  onFrame <- DOM.newCallback renderNow
  writeIORef scheduleRender (DOM.requestAnimationFrame onFrame)
  renderNow
  sync dispatch running (application.subscriptions initial)
  runCmd dispatch initialCmd
