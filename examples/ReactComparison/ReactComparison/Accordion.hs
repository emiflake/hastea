{- | React: lifting state up.
<https://react.dev/learn/sharing-state-between-components>

In React the two panels each had an @isActive@ state, which was then moved
to their parent so that only one is open. Here there was never anywhere else
to put it.
-}
module ReactComparison.Accordion
  ( Model
  , Msg
  , initial
  , update
  , view
  )
where

import Prelude hiding (div, span)

import Hastea.Html
import Hastea.Html.Attributes (className, lang)
import Hastea.Html.Events (onClick)


-- | The index of the open panel.
type Model = Int


newtype Msg = Shown Int


initial :: Model
initial = 0


update :: Msg -> Model -> Model
update (Shown index) _ = index


view :: Model -> Html Msg
view activeIndex =
  div
    []
    [ h2 [] [text "Almaty, Kazakhstan"]
    , panel
        "About"
        (activeIndex == 0)
        (Shown 0)
        [ text
            "With a population of about 2 million, Almaty is Kazakhstan's largest \
            \city. From 1929 to 1997, it was its capital city."
        ]
    , panel
        "Etymology"
        (activeIndex == 1)
        (Shown 1)
        [ text "The name comes from "
        , span [lang "kk-KZ"] [text "алма"]
        , text
            ", the Kazakh word for \"apple\" and is often translated as \"full of \
            \apples\". In fact, the region surrounding Almaty is thought to be the \
            \ancestral home of the apple, and the wild "
        , i [lang "la"] [text "Malus sieversii"]
        , text " is considered a likely candidate for the ancestor of the modern domestic apple."
        ]
    ]


{- | A React component with props is a function with arguments. The @onShow@
callback prop becomes the message to send, so the panel works for any
message type.
-}
panel :: String -> Bool -> msg -> [Html msg] -> Html msg
panel panelTitle isActive onShow children =
  section
    [className "panel"]
    [ h3 [] [text panelTitle]
    , if isActive
        then p [] children
        else button [onClick onShow] [text "Show"]
    ]
