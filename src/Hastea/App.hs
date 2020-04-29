module Hastea.App
  ( runApp
  )
  where


import           Asterius.Types
import           Data.IORef
import           Hastea.Cmd
import           Hastea.Html
import           Hastea.Internal.Foreign.DOM
import qualified Hastea.Internal.Foreign.Effects as Effects
import           Hastea.Internal.VirtualDOM
import           Control.Concurrent


-- RUN THE APP


runApp ::
  (Show s, Show msg) => -- Temporary for debugging
  (msg -> s -> (s, Cmd msg)) -> s -> (s -> Html msg) -> IO ()
runApp update init view = do
  mainNode <- getElementById "main-node"
  msgRef <- newIORef []

  let collapseState = foldr (\acc v -> fst (update acc v)) init

  let render oldDOM = do
        let handleEvent evtMsg = do
              modifyIORef msgRef (evtMsg:)
              msgs <- readIORef msgRef
              let latest = collapseState msgs
              let last = collapseState (drop 1 msgs)
              let cmd = snd $ update evtMsg last 
              maybeMsg <- runCmd cmd
              Effects.putStrLn ("Got message: " <> show evtMsg)
              -- Effects.putStrLn ("now, latest state:" <> show latest)
              case maybeMsg of
                Just msg ->
                  handleEvent msg
                Nothing ->
                  requestAnimationFrame $
                    patch handleEvent mainNode (Just (view last)) (Just (view latest))

        msgs <- readIORef msgRef
        let latest = collapseState msgs
        patch handleEvent mainNode oldDOM (Just (view latest))

  render Nothing
