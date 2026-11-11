{- | Examples from the React documentation, written with Hastea.

Each module in "ReactComparison" is one example. This module puts them on
one page. Each example is a component that keeps its own state, so using
one takes a single line, as in React.
-}
module Main where

import Data.Void (Void, absurd)

import Hastea.Component (use)
import Hastea.Html
import Hastea.Html.Attributes (className)
import Hastea.Root (mount, render)

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


example :: String -> [Html msg] -> Html msg
example name content =
  article [className "example"] (h2 [className "example-title"] [text name] : content)


main :: IO ()
main = do
  -- The page has no state of its own and sends no messages, so there is
  -- nothing for an application loop to do: show it and let the components
  -- look after themselves.
  root <- mount "main-node" (absurd :: Void -> IO ())
  render root page
  StopwatchRoot.run


-- The wasm module is linked as a WASI reactor, so there is no implicit entry
-- point: index.js calls this export once the module is initialised.
foreign export javascript "hs_start" main :: IO ()
