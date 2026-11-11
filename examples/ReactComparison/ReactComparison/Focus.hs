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
import Hastea.Html qualified as H
import Hastea.Html.Attributes qualified as A
import Hastea.Html.Events (onClick)


data Msg = FocusPressed


-- | There is nothing to remember, so the state is empty.
focusForm :: Component () () Msg out
focusForm = component initial update view
 where
  initial () = ()

  update FocusPressed () = ((), [Run (Dom.focus inputId)])

  view () () =
    H.fragment
      [ H.input [A.id_ inputId]
      , H.button [onClick FocusPressed] [H.text "Focus the input"]
      ]

  inputId = "focus-example-input"
