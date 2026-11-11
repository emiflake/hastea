{- | Examples from the React documentation, written with Hastea.

Each module in "ReactComparison" is one example. This module puts them on
one page, which is itself an example of composing components: the page's
model holds the model of each part, and its messages wrap theirs.
-}
module Main where

import Hastea.App (runApp)
import Hastea.Cmd qualified as Cmd
import Hastea.Html
import Hastea.Html.Attributes (className)

import ReactComparison.Accordion qualified as Accordion
import ReactComparison.Counter qualified as Counter
import ReactComparison.Greeting qualified as Greeting
import ReactComparison.Headings qualified as Headings
import ReactComparison.ProductTable qualified as ProductTable
import ReactComparison.Stopwatch qualified as Stopwatch
import ReactComparison.Tasks qualified as Tasks


data Model = Model
  { firstCounter :: Counter.Model
  , secondCounter :: Counter.Model
  , accordion :: Accordion.Model
  , productTable :: ProductTable.Model
  , tasks :: Tasks.Model
  , greeting :: Greeting.Model
  }


data Msg
  = FirstCounterMsg Counter.Msg
  | SecondCounterMsg Counter.Msg
  | AccordionMsg Accordion.Msg
  | ProductTableMsg ProductTable.Msg
  | TasksMsg Tasks.Msg
  | GreetingMsg Greeting.Msg


initial :: Model
initial =
  Model
    { firstCounter = Counter.initial
    , secondCounter = Counter.initial
    , accordion = Accordion.initial
    , productTable = ProductTable.initial
    , tasks = Tasks.initial
    , greeting = Greeting.initial
    }


update :: Msg -> Model -> Model
update msg model = case msg of
  FirstCounterMsg m -> model{firstCounter = Counter.update m (firstCounter model)}
  SecondCounterMsg m -> model{secondCounter = Counter.update m (secondCounter model)}
  AccordionMsg m -> model{accordion = Accordion.update m (accordion model)}
  ProductTableMsg m -> model{productTable = ProductTable.update m (productTable model)}
  TasksMsg m -> model{tasks = Tasks.update m (tasks model)}
  GreetingMsg m -> model{greeting = Greeting.update m (greeting model)}


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
    ]


example :: String -> [Html msg] -> Html msg
example title content =
  article [className "example"] (h2 [className "example-title"] [text title] : content)


main :: IO ()
main = do
  runApp (\msg model -> (update msg model, Cmd.none)) initial view
  Stopwatch.run


-- The wasm module is linked as a WASI reactor, so there is no implicit entry
-- point: index.js calls this export once the module is initialised.
foreign export javascript "hs_start" main :: IO ()
