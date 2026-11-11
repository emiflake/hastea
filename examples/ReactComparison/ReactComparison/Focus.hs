{- | React: refs to DOM nodes.
<https://react.dev/learn/manipulating-the-dom-with-refs>

React hands the component the DOM node (a ref) and the event handler calls
@focus()@ on it. Here the view never sees DOM nodes: the input gets an id,
and @update@ asks for a command that names it.
-}
module ReactComparison.Focus
  ( focusForm
  )
where

import Prelude hiding (div)

import Hastea.Component
import Hastea.Dom qualified as Dom
import Hastea.Html
import Hastea.Html.Attributes (id_)
import Hastea.Html.Events (onClick)
import Hastea.Sub qualified as Sub


data Msg = FocusPressed


focusForm :: Component () () Msg out
focusForm =
  Component
    { initial = \() -> ()
    , update = \FocusPressed () -> ((), [Run (Dom.focus inputId)])
    , view = \() () ->
        div
          []
          [ input [id_ inputId]
          , button [onClick FocusPressed] [text "Focus the input"]
          ]
    , subscriptions = const Sub.none
    }


inputId :: String
inputId = "focus-example-input"
