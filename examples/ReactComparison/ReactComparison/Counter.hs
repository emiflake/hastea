{- | React: @useState@.
<https://react.dev/reference/react/useState#counter-number>

A React component is a function that calls @useState@. Here it is made of
three functions: what the state is at first, how a message changes it, and
what it looks like.
-}
module ReactComparison.Counter
  ( counter
  )
where

import Hastea.Component
import Hastea.Html qualified as H
import Hastea.Html.Events (onClick)


data Msg = Pressed


counter :: Component () Int Msg out
counter = component initial update view
 where
  initial () = 0

  update Pressed count = (count + 1, [])

  view () count =
    H.button [onClick Pressed] [H.text ("You pressed me " <> show count <> " times")]
