{- | Applications in the style of The Elm Architecture: a model, a function
that updates it in response to messages, and a view of it.

'runApp' is all a simple application needs. 'App' and 'run' add the
optional parts: an initial command, subscriptions, and where on the page to
go.
-}
module Hastea.App
  ( runApp

    -- * Applications with more parts
  , App (..)
  , app
  , run
  )
where

import Control.Monad
import Data.IORef

import Hastea.Cmd
import Hastea.Html
import Hastea.Internal.Foreign.DOM qualified as DOM
import Hastea.Internal.Sub (newManager, sync)
import Hastea.Root
import Hastea.Sub (Sub)
import Hastea.Sub qualified as Sub


-- | Everything that makes up an application.
data App model msg = App
  { appInitial :: (model, Cmd msg)
  -- ^ The model to start with, and a command to run at the start.
  , appUpdate :: msg -> model -> (model, Cmd msg)
  -- ^ The next model when a message arrives, and a command to run.
  , appView :: model -> Html msg
  -- ^ What to show for a model.
  , appSubscriptions :: model -> Sub msg
  -- ^ What to listen to, besides the view, while the model is like this.
  , appContainer :: String
  -- ^ The id of the element to show the view in.
  }


{- | An application with only the required parts: no subscriptions, nothing
to do at the start, shown in the element with id @main-node@. Use record
syntax to add the rest:

@
run (app update initial view){appSubscriptions = subscriptions}
@
-}
app :: (msg -> model -> (model, Cmd msg)) -> model -> (model -> Html msg) -> App model msg
app update initial view =
  App
    { appInitial = (initial, none)
    , appUpdate = update
    , appView = view
    , appSubscriptions = const Sub.none
    , appContainer = "main-node"
    }


{- | Run an application in the element with id @main-node@.

The model only changes through @update@, in response to the messages
produced by the view's event handlers and by commands. The view is shown
again after the model has changed, at most once per frame: when several
messages arrive in quick succession, only the latest model is rendered.
-}
runApp :: (msg -> model -> (model, Cmd msg)) -> model -> (model -> Html msg) -> IO ()
runApp update initial view = run (app update initial view)


-- | Run an application.
run :: App model msg -> IO ()
run application = do
  let (initial, initialCmd) = appInitial application
  model <- newIORef initial
  subscriptions <- newManager
  renderScheduled <- newIORef False
  -- Filled in once the root exists; the root in turn needs 'dispatch'.
  scheduleRender <- newIORef (pure ())

  let dispatch msg = do
        (new, cmd) <- appUpdate application msg <$> readIORef model
        new `seq` writeIORef model new
        scheduled <- readIORef renderScheduled
        unless scheduled $ do
          writeIORef renderScheduled True
          join (readIORef scheduleRender)
        sync dispatch subscriptions (appSubscriptions application new)
        runCmd dispatch cmd

  root <- mount (appContainer application) dispatch
  let renderNow = do
        writeIORef renderScheduled False
        readIORef model >>= render root . appView application

  onFrame <- DOM.newCallback renderNow
  writeIORef scheduleRender (DOM.requestAnimationFrame onFrame)
  renderNow
  sync dispatch subscriptions (appSubscriptions application initial)
  runCmd dispatch initialCmd
