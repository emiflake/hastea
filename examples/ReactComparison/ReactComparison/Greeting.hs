{- | React: @memo@.
<https://react.dev/reference/react/memo>

@memo@ skips rendering a component when its props are unchanged. 'lazy' does
the same for a function and its arguments. React has to trust that the
component is pure; here the type of 'greeting' guarantees it.
-}
module ReactComparison.Greeting
  ( Model
  , Msg
  , initial
  , update
  , view
  )
where

import Prelude hiding (div)

import Debug.Trace (trace)

import Hastea.Html hiding (address)
import Hastea.Html.Attributes (value)
import Hastea.Html.Events (onInput)


data Model = Model
  { name :: String
  , address :: String
  }


data Msg
  = NameChanged String
  | AddressChanged String


initial :: Model
initial = Model{name = "", address = ""}


update :: Msg -> Model -> Model
update msg model = case msg of
  NameChanged newName -> model{name = newName}
  AddressChanged newAddress -> model{address = newAddress}


view :: Model -> Html Msg
view model =
  div
    []
    [ label [] [text "Name: ", input [value (name model), onInput NameChanged]]
    , label [] [text "Address: ", input [value (address model), onInput AddressChanged]]
    , -- Typing an address leaves the name untouched, so the greeting is
      -- neither computed nor compared with the page.
      lazy greeting (name model)
    ]


{- | Like the React example, this reports when it is rendered (in the
browser console), to show that it is not when only the address changes.
-}
greeting :: String -> Html msg
greeting who =
  trace "Greeting was rendered" $
    h3 [] [text ("Hello" <> (if null who then "" else ", " <> who) <> "!")]
