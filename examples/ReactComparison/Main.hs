{- | Examples from the React documentation, written with Hastea.

Each module in "ReactComparison" is one example. This module puts them on
one page, which is itself an example of composing components: the page's
model holds the model of each part, and its messages wrap theirs.
-}
module Main where

import Hastea.App (App (..), app, run)
import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Html
import Hastea.Html.Attributes (className)
import Hastea.Sub (Sub)

import ReactComparison.Accordion qualified as Accordion
import ReactComparison.Counter qualified as Counter
import ReactComparison.Focus qualified as Focus
import ReactComparison.Greeting qualified as Greeting
import ReactComparison.Headings qualified as Headings
import ReactComparison.ProductTable qualified as ProductTable
import ReactComparison.Quiz qualified as Quiz
import ReactComparison.Stopwatch qualified as Stopwatch
import ReactComparison.StopwatchRoot qualified as StopwatchRoot
import ReactComparison.Tasks qualified as Tasks


data Model = Model
  { firstCounter :: Counter.Model
  , secondCounter :: Counter.Model
  , accordion :: Accordion.Model
  , productTable :: ProductTable.Model
  , tasks :: Tasks.Model
  , greeting :: Greeting.Model
  , stopwatch :: Stopwatch.Model
  , quiz :: Quiz.Model
  }


data Msg
  = FirstCounterMsg Counter.Msg
  | SecondCounterMsg Counter.Msg
  | AccordionMsg Accordion.Msg
  | ProductTableMsg ProductTable.Msg
  | TasksMsg Tasks.Msg
  | GreetingMsg Greeting.Msg
  | StopwatchMsg Stopwatch.Msg
  | FocusMsg Focus.Msg
  | QuizMsg Quiz.Msg


initial :: Model
initial =
  Model
    { firstCounter = Counter.initial
    , secondCounter = Counter.initial
    , accordion = Accordion.initial
    , productTable = ProductTable.initial
    , tasks = Tasks.initial
    , greeting = Greeting.initial
    , stopwatch = Stopwatch.initial
    , quiz = Quiz.initial
    }


update :: Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  FirstCounterMsg m -> noCmd model{firstCounter = Counter.update m (firstCounter model)}
  SecondCounterMsg m -> noCmd model{secondCounter = Counter.update m (secondCounter model)}
  AccordionMsg m -> noCmd model{accordion = Accordion.update m (accordion model)}
  ProductTableMsg m -> noCmd model{productTable = ProductTable.update m (productTable model)}
  TasksMsg m -> noCmd model{tasks = Tasks.update m (tasks model)}
  GreetingMsg m -> noCmd model{greeting = Greeting.update m (greeting model)}
  -- Parts with effects also return a command, whose messages are wrapped
  -- like those of their view.
  StopwatchMsg m ->
    let (new, cmd) = Stopwatch.update m (stopwatch model)
     in (model{stopwatch = new}, StopwatchMsg <$> cmd)
  FocusMsg m -> (model, FocusMsg <$> Focus.update m)
  QuizMsg m ->
    let (new, cmd) = Quiz.update m (quiz model)
     in (model{quiz = new}, QuizMsg <$> cmd)
 where
  noCmd new = (new, Cmd.none)


subscriptions :: Model -> Sub Msg
subscriptions model = StopwatchMsg <$> Stopwatch.subscriptions (stopwatch model)


view :: Model -> Html Msg
view model =
  main_
    []
    [ example
        "useState"
        [ -- The same component twice, each with its own state.
          FirstCounterMsg <$> Counter.view (firstCounter model)
        , SecondCounterMsg <$> Counter.view (secondCounter model)
        ]
    , example "Lifting state up" [AccordionMsg <$> lazy Accordion.view (accordion model)]
    , example "Thinking in React" [ProductTableMsg <$> lazy ProductTable.view (productTable model)]
    , example "useReducer" [TasksMsg <$> lazy Tasks.view (tasks model)]
    , example "memo" [GreetingMsg <$> Greeting.view (greeting model)]
    , example "Context" [Headings.view]
    , example "Effects and refs" [StopwatchMsg <$> Stopwatch.view (stopwatch model)]
    , example "Refs to DOM nodes" [FocusMsg <$> Focus.view]
    , example "Asynchronous work" [QuizMsg <$> lazy Quiz.view (quiz model)]
    ]


example :: String -> [Html msg] -> Html msg
example name content =
  article [className "example"] (h2 [className "example-title"] [text name] : content)


main :: IO ()
main = do
  run (app update initial view){appSubscriptions = subscriptions}
  StopwatchRoot.run


-- The wasm module is linked as a WASI reactor, so there is no implicit entry
-- point: index.js calls this export once the module is initialised.
foreign export javascript "hs_start" main :: IO ()
