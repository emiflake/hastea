module Hastea.App
  ( runApp
  )
  where


import           Asterius.Types
import           Data.IORef
import           Hastea.Cmd
import           Hastea.Html
import           Hastea.Internal.Foreign.DOM
import           Hastea.Internal.VirtualDOM

-- RUN THE APP

runApp :: Show msg => (msg -> s -> (s, Cmd msg)) -> s -> (s -> Html msg) -> IO ()
runApp update init view = do
  mainNode <- getElementById "main-node"
  stateRef <- newIORef init

  let render s = do
        let handleEvent evtMsg = do
              currentState <- readIORef stateRef
              let (newState, cmd) = update evtMsg currentState
              res <- runCmd cmd
              writeIORef stateRef newState
              case res of
                Nothing  -> render newState
                Just msg -> handleEvent msg

        mainNode `setInnerHTML` "" -- clears all children, TODO: should diff instead of this
        let rootNodeToRender = view s
        rootNodeRendered <- renderVNode handleEvent rootNodeToRender
        mainNode `appendChild` rootNodeRendered

  render init
