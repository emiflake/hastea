{- | React: lifting state up.
<https://react.dev/learn/sharing-state-between-components>

As in React, the panels have no state of their own: the accordion knows
which one is open and tells each panel whether that is it.
-}
module ReactComparison.Accordion
  ( accordion
  )
where

import Prelude hiding (div, span)

import Hastea.Component
import Hastea.Html
import Hastea.Html.Attributes (className, lang)
import Hastea.Html.Events (onClick)
import Hastea.Sub qualified as Sub


newtype Msg = Shown Int


-- | The state is the index of the open panel.
accordion :: Component () Int Msg out
accordion =
  Component
    { initial = \() -> 0
    , update = \(Shown index) _ -> (index, [])
    , view = \() activeIndex ->
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
    , subscriptions = const Sub.none
    }


{- | A React component without state is a function. The @onShow@ callback
prop becomes the message to send, so the panel works for any message type.
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
