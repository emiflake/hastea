{- | React: @memo@.
<https://react.dev/reference/react/memo>

@memo@ skips rendering a component when its props are unchanged. 'lazy' does
the same for a function and its arguments. React has to trust that the
component is pure; here the type of 'greeting' guarantees it.
-}
module ReactComparison.Greeting
  ( greetingForm
  )
where

import Debug.Trace (trace)

import Hastea.Component
import Hastea.Html (Html)
import Hastea.Html qualified as H
import Hastea.Html.Attributes qualified as A
import Hastea.Html.Events (onInput)


data Form = Form
  { name :: String
  , address :: String
  }


data Msg
  = NameChanged String
  | AddressChanged String


greetingForm :: Component () Form Msg out
greetingForm = component initial update view
 where
  initial () = Form{name = "", address = ""}

  update msg fields = case msg of
    NameChanged newName -> (fields{name = newName}, [])
    AddressChanged newAddress -> (fields{address = newAddress}, [])

  view () fields =
    H.fragment
      [ H.label [] [H.text "Name: ", H.input [A.value fields.name, onInput NameChanged]]
      , H.label [] [H.text "Address: ", H.input [A.value fields.address, onInput AddressChanged]]
      , -- Typing an address leaves the name untouched, so the greeting is
        -- neither computed nor compared with the page.
        H.lazy greeting fields.name
      ]


{- | Like the React example, this reports when it is rendered (in the
browser console), to show that it is not when only the address changes.
-}
greeting :: String -> Html msg
greeting name =
  trace "Greeting was rendered" $
    H.h3 [] [H.text (if null name then "Hello!" else "Hello, " <> name <> "!")]
