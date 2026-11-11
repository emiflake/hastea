{- | React: state set from event handlers, in React's own style.
<https://react.dev/learn/referencing-values-with-refs>

"ReactComparison.Stopwatch" is this example written the usual way. This
version shows what is underneath "Hastea.App": a "Hastea.Root" only shows
trees and hands back their messages, whatever they are. Here the messages
are 'IO' actions, which the root simply runs, state lives in an 'IORef', and
setting it renders again, as in the React original.
-}
module ReactComparison.StopwatchRoot
  ( run
  )
where

import Prelude hiding (div)

import Control.Monad (when)
import Data.IORef
import Numeric (showFFloat)

import Hastea.Async (setTimeout)
import Hastea.Html
import Hastea.Html.Events (onClick)
import Hastea.Root (mount, render)


foreign import javascript unsafe "Date.now()" dateNow :: IO Double


data State = State
  { startTime :: Maybe Double
  , now :: Maybe Double
  }


-- | Show a stopwatch in the element with id @stopwatch-root-node@.
run :: IO ()
run = do
  -- An event handler is an action; to dispatch it is to run it.
  root <- mount "stopwatch-root-node" id
  state <- newIORef State{startTime = Nothing, now = Nothing}
  -- Like a ref, this is not shown, so changing it does not render. It
  -- counts how often the timer was started or stopped.
  timer <- newIORef (0 :: Int)

  let setState change = do
        modifyIORef' state change
        readIORef state >>= render root . view

      handleStart = do
        timestamp <- dateNow
        setState (const State{startTime = Just timestamp, now = Just timestamp})
        modifyIORef' timer (+ 1)
        readIORef timer >>= tick

      handleStop = modifyIORef' timer (+ 1)

      -- Keeps going until the timer is started or stopped again.
      tick started = do
        current <- readIORef timer
        when (current == started) $ do
          timestamp <- dateNow
          setState (\st -> st{now = Just timestamp})
          setTimeout (tick started) 10

      view st =
        div
          []
          [ h1 [] [text ("Time passed: " <> showFFloat (Just 3) (secondsPassed st) "")]
          , button [onClick handleStart] [text "Start"]
          , button [onClick handleStop] [text "Stop"]
          ]

  setState id


secondsPassed :: State -> Double
secondsPassed State{startTime = Just start, now = Just current} = (current - start) / 1000
secondsPassed _ = 0
