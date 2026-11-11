{- | React: refs to DOM nodes.
<https://react.dev/learn/manipulating-the-dom-with-refs>

React hands the component the DOM node (a ref) and the event handler calls
@focus()@ on it. Here the view never sees DOM nodes: the input gets an id,
and 'update' returns a command that names it.
-}
module ReactComparison.Focus
  ( Msg
  , update
  , view
  )
where

import Prelude hiding (div)

import Hastea.Cmd (Cmd)
import Hastea.Dom qualified as Dom
import Hastea.Html
import Hastea.Html.Attributes (id_)
import Hastea.Html.Events (onClick)


data Msg = FocusPressed


update :: Msg -> Cmd Msg
update FocusPressed = Dom.focus inputId


view :: Html Msg
view =
  div
    []
    [ input [id_ inputId]
    , button [onClick FocusPressed] [text "Focus the input"]
    ]


inputId :: String
inputId = "focus-example-input"
