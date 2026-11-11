{- | React: @useState@.
<https://react.dev/reference/react/useState#counter-number>

A React component keeps its own state. Here a component is a model, the
messages that can change it, and two pure functions; whoever uses it stores
the model.
-}
module ReactComparison.Counter
  ( Model
  , Msg
  , initial
  , update
  , view
  )
where

import Hastea.Html
import Hastea.Html.Events (onClick)


type Model = Int


data Msg = Pressed


initial :: Model
initial = 0


update :: Msg -> Model -> Model
update Pressed count = count + 1


view :: Model -> Html Msg
view count =
  button [onClick Pressed] [text ("You pressed me " <> show count <> " times")]
