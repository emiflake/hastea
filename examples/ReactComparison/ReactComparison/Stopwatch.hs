{- | React: effects and refs.
<https://react.dev/learn/referencing-values-with-refs>

The React version starts an interval in an event handler and keeps its id
in a ref, in order to clear it later. Here the model says whether the
stopwatch is running, and @subscriptions@ says that a running stopwatch
wants ticks. Starting and stopping the timer is left to the runtime.
-}
module ReactComparison.Stopwatch
  ( stopwatch
  )
where

import Prelude hiding (div)

import Numeric (showFFloat)

import Hastea.Component
import Hastea.Html
import Hastea.Html.Events (onClick)
import Hastea.Sub qualified as Sub
import Hastea.Time qualified as Time


data Model = Model
  { startTime :: Maybe Double
  , now :: Maybe Double
  , running :: Bool
  }


data Msg
  = StartPressed
  | Started Double
  | StopPressed
  | Ticked Double


stopwatch :: Component () Model Msg out
stopwatch =
  Component
    { initial = \() -> Model{startTime = Nothing, now = Nothing, running = False}
    , update = \msg model -> case msg of
        -- Reading the clock is an effect: ask for the time and wait for the answer.
        StartPressed -> (model, [Run (Time.now Started)])
        Started timestamp ->
          (Model{startTime = Just timestamp, now = Just timestamp, running = True}, [])
        StopPressed -> (model{running = False}, [])
        Ticked timestamp -> (model{now = Just timestamp}, [])
    , view = \() model ->
        div
          []
          [ h1 [] [text ("Time passed: " <> showFFloat (Just 3) (secondsPassed model) "")]
          , button [onClick StartPressed] [text "Start"]
          , button [onClick StopPressed] [text "Stop"]
          ]
    , subscriptions = \model ->
        if model.running then Time.every 10 Ticked else Sub.none
    }


secondsPassed :: Model -> Double
secondsPassed model = case (model.startTime, model.now) of
  (Just start, Just current) -> (current - start) / 1000
  _ -> 0
