{- | React: @useState@.
<https://react.dev/reference/react/useState#counter-number>

A React component is a function that calls @useState@. Here it is a value
that says what the state is at first, how messages change it, and what it
looks like.
-}
module ReactComparison.Counter
  ( counter
  )
where

import Hastea.Component
import Hastea.Html
import Hastea.Html.Events (onClick)
import Hastea.Sub qualified as Sub


data Msg = Pressed


counter :: Component () Int Msg out
counter =
  Component
    { initial = \() -> 0
    , update = \Pressed count -> (count + 1, [])
    , view = \() count ->
        button [onClick Pressed] [text ("You pressed me " <> show count <> " times")]
    , subscriptions = const Sub.none
    }
