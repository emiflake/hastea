module Hastea.App
  ( runApp
  )
  where


import           Asterius.Types
import           Data.IORef
import           Hastea.Html
import           Hastea.Internal.Foreign.DOM
import           Hastea.Internal.VirtualDOM

-- RUN THE APP


runApp :: Show msg => (msg -> s -> s) -> s -> (s -> Html msg) -> IO ()
runApp update init view = do
  mainNode <- getElementById "main-node"
  stateRef <- newIORef init

  let render s = do
        let handleEvent evtMsg = do
              modifyIORef' stateRef (update evtMsg)
              state <- readIORef stateRef
              render state
        mainNode `setInnerHTML` "" -- clears all children, TODO: should diff instead of this
        let rootNodeToRender = view s
        rootNodeRendered <- renderVNode handleEvent rootNodeToRender
        mainNode `appendChild` rootNodeRendered

  render init
