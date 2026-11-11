module Hastea.App
  ( runApp
  )
where

import Data.IORef
import Hastea.Cmd
import Hastea.Html
import Hastea.Internal.Foreign.DOM
import Hastea.Internal.Foreign.Effects qualified as Effects
import Hastea.Internal.VirtualDOM


-- RUN THE APP

runApp ::
  (Show s, Show msg) => -- Temporary for debugging
  (msg -> s -> (s, Cmd msg)) -> s -> (s -> Html msg) -> IO ()
runApp update initial view = do
  mainNode <- getElementById "main-node"
  msgRef <- newIORef []

  let collapseState = foldr (\acc v -> fst (update acc v)) initial

  let render oldDOM = do
        let handleEvent evtMsg = do
              modifyIORef msgRef (evtMsg :)
              msgs <- readIORef msgRef
              let latest = collapseState msgs
              let previous = collapseState (drop 1 msgs)
              let cmd = snd $ update evtMsg previous
              maybeMsg <- runCmd cmd
              Effects.putStrLn ("Got message: " <> show evtMsg)
              -- Effects.putStrLn ("now, latest state:" <> show latest)
              case maybeMsg of
                Just msg ->
                  handleEvent msg
                Nothing ->
                  requestAnimationFrame $
                    patch handleEvent mainNode (Just (view previous)) (Just (view latest))

        msgs <- readIORef msgRef
        let latest = collapseState msgs
        patch handleEvent mainNode oldDOM (Just (view latest))

  render Nothing
