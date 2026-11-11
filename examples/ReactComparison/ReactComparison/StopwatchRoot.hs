{- | React: state set from event handlers, in React's own style.
<https://react.dev/learn/referencing-values-with-refs>

"ReactComparison.Stopwatch" is this example written the usual way. This
version shows what is underneath: a "Hastea.Root" only shows trees and
hands back their messages, whatever they are. Here the messages are 'IO'
actions, which the root simply runs, state lives in an 'IORef', and setting
it renders again, as in the React original.
-}
module ReactComparison.StopwatchRoot
  ( run
  )
where

import Control.Monad (when)
import Data.IORef
import Numeric (showFFloat)

import Hastea.Async (setTimeout)
import Hastea.Html qualified as H
import Hastea.Html.Events (onClick)
import Hastea.Root (mount, render)


foreign import javascript unsafe "Date.now()" dateNow :: IO Double


-- | Times are in milliseconds.
data Stopwatch = Stopwatch
  { startTime :: Double
  , now :: Double
  }


-- | Show a stopwatch in the element with id @stopwatch-root-node@.
run :: IO ()
run = do
  -- An event handler is an action; to dispatch it is to run it.
  root <- mount "stopwatch-root-node" id
  state <- newIORef Stopwatch{startTime = 0, now = 0}
  -- Like a ref, this is not shown, so changing it does not render. It
  -- counts how often the timer was started or stopped.
  timer <- newIORef (0 :: Int)

  let setState change = do
        modifyIORef' state change
        readIORef state >>= render root . view

      handleStart = do
        time <- dateNow
        setState (const Stopwatch{startTime = time, now = time})
        modifyIORef' timer (+ 1)
        readIORef timer >>= tick

      handleStop = modifyIORef' timer (+ 1)

      -- Keeps going until the timer is started or stopped again.
      tick started = do
        current <- readIORef timer
        when (current == started) $ do
          time <- dateNow
          setState (\watch -> watch{now = time})
          setTimeout (tick started) 10

      view watch =
        H.fragment
          [ H.h1 [] [H.text ("Time passed: " <> seconds (watch.now - watch.startTime))]
          , H.button [onClick handleStart] [H.text "Start"]
          , H.button [onClick handleStop] [H.text "Stop"]
          ]

  setState id


-- | Milliseconds as seconds, to three decimal places.
seconds :: Double -> String
seconds milliseconds = showFFloat (Just 3) (milliseconds / 1000) ""
