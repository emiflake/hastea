{- | React: effects and refs.
<https://react.dev/learn/referencing-values-with-refs>

The React version starts an interval in an event handler and keeps its id
in a ref, in order to clear it later. Here the state says whether the
stopwatch is running, and @subscriptions@ says that a running stopwatch
wants ticks. Starting and stopping the timer is left to the runtime.
-}
module ReactComparison.Stopwatch
  ( stopwatch
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showFFloat)

import Hastea.Component
import Hastea.Html qualified as H
import Hastea.Html.Events (onClick)
import Hastea.Sub qualified as Sub
import Hastea.Time qualified as Time


-- | Times are in milliseconds.
data Stopwatch = Stopwatch
  { startTime :: Double
  , now :: Double
  , running :: Bool
  }


data Msg
  = StartPressed
  | Started Double
  | StopPressed
  | Ticked Double


stopwatch :: Component () Stopwatch Msg out
stopwatch = (component initial update view){subscriptions}
 where
  initial () = Stopwatch{startTime = 0, now = 0, running = False}

  update msg watch = case msg of
    -- Reading the time is an effect: ask for the time and wait for the answer.
    StartPressed -> (watch, [Run (Time.now Started)])
    Started time -> (Stopwatch{startTime = time, now = time, running = True}, [])
    StopPressed -> (watch{running = False}, [])
    Ticked time -> (watch{now = time}, [])

  subscriptions watch
    | watch.running = Time.every 10 Ticked
    | otherwise = Sub.none

  view () watch =
    H.fragment
      [ H.h1 [] [H.text ("Time passed: " <> seconds (watch.now - watch.startTime))]
      , H.button [onClick StartPressed] [H.text "Start"]
      , H.button [onClick StopPressed] [H.text "Stop"]
      ]


-- | Milliseconds as seconds, to three decimal places.
seconds :: Double -> Text
seconds milliseconds = T.pack (showFFloat (Just 3) (milliseconds / 1000) "")
