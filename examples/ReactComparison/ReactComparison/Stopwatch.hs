{- | React: effects and refs.
<https://react.dev/learn/referencing-values-with-refs>

The React version starts an interval in an event handler and keeps its id
in a ref, in order to clear it later. Here the model says whether the
stopwatch is running, and 'subscriptions' says that a running stopwatch
wants ticks. Starting and stopping the timer is left to the runtime.
-}
module ReactComparison.Stopwatch
  ( Model
  , Msg
  , initial
  , update
  , subscriptions
  , view
  )
where

import Prelude hiding (div)

import Numeric (showFFloat)

import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Html
import Hastea.Html.Events (onClick)
import Hastea.Sub (Sub)
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


initial :: Model
initial = Model{startTime = Nothing, now = Nothing, running = False}


update :: Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  -- Reading the clock is an effect, so ask for the time and wait for the answer.
  StartPressed -> (model, Time.now Started)
  Started timestamp ->
    (Model{startTime = Just timestamp, now = Just timestamp, running = True}, Cmd.none)
  StopPressed -> (model{running = False}, Cmd.none)
  Ticked timestamp -> (model{now = Just timestamp}, Cmd.none)


subscriptions :: Model -> Sub Msg
subscriptions model
  | model.running = Time.every 10 Ticked
  | otherwise = Sub.none


view :: Model -> Html Msg
view model =
  div
    []
    [ h1 [] [text ("Time passed: " <> showFFloat (Just 3) (secondsPassed model) "")]
    , button [onClick StartPressed] [text "Start"]
    , button [onClick StopPressed] [text "Stop"]
    ]


secondsPassed :: Model -> Double
secondsPassed model = case (model.startTime, model.now) of
  (Just start, Just current) -> (current - start) / 1000
  _ -> 0
