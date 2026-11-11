{- | Examples from the React documentation, written with Hastea.

Each module in "ReactComparison" is one example. This module puts them on
one page. Each example is a component that keeps its own state, so using
one takes a single line, as in React.
-}
module Main where

import Hastea.App (runView)
import Hastea.Component (use)
import Hastea.Html
import Hastea.Html.Attributes (className)

import ReactComparison.Accordion (accordion)
import ReactComparison.Counter (counter)
import ReactComparison.Focus (focusForm)
import ReactComparison.Greeting (greetingForm)
import ReactComparison.Headings qualified as Headings
import ReactComparison.ProductTable (filterableProductTable, products)
import ReactComparison.Quiz (quiz)
import ReactComparison.Stopwatch (stopwatch)
import ReactComparison.StopwatchRoot qualified as StopwatchRoot
import ReactComparison.Tasks (taskApp)


main :: IO ()
main = do
  runView page
  StopwatchRoot.run


page :: Html msg
page =
  main_
    []
    [ -- The same component twice, each with its own state.
      example "useState" [use counter (), use counter ()]
    , example "Lifting state up" [use accordion ()]
    , example "Thinking in React" [use filterableProductTable products]
    , example "useReducer" [use taskApp ()]
    , example "memo" [use greetingForm ()]
    , example "Context" [Headings.view]
    , example "Effects and refs" [use stopwatch ()]
    , example "Refs to DOM nodes" [use focusForm ()]
    , example "Asynchronous work" [use quiz ()]
    ]


-- | A titled box around one example.
example :: String -> [Html msg] -> Html msg
example name content =
  article [className "example"] (h2 [className "example-title"] [text name] : content)


-- The wasm module is linked as a WASI reactor, so there is no implicit entry
-- point: index.js calls this export once the module is initialised.
foreign export javascript "hs_start" main :: IO ()
