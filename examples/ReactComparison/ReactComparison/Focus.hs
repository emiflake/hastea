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

import Hastea.Component
import Hastea.Dom qualified as Dom
import Hastea.Html
import Hastea.Html.Attributes (id_)
import Hastea.Html.Events (onClick)


data Msg = FocusPressed


-- | There is nothing to remember, so the state is empty.
focusForm :: Component () () Msg out
focusForm = component initial update view
 where
  initial () = ()

  update FocusPressed () = ((), [Run (Dom.focus inputId)])

  view () () =
    fragment
      [ input [id_ inputId]
      , button [onClick FocusPressed] [text "Focus the input"]
      ]

  inputId = "focus-example-input"
