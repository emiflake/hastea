{- | React: lifting state up.
<https://react.dev/learn/sharing-state-between-components>

As in React, the panels have no state of their own: the accordion knows
which one is open and tells each panel whether that is it.
-}
module ReactComparison.Accordion
  ( accordion
  )
where

import Data.Text (Text)

import Hastea.Component
import Hastea.Html (Html)
import Hastea.Html qualified as H
import Hastea.Html.Attributes qualified as A
import Hastea.Html.Events (onClick)


newtype Msg = Shown Int


-- | The state is the index of the open panel.
accordion :: Component () Int Msg out
accordion = component initial update view
 where
  initial () = 0

  update (Shown index) _ = (index, [])

  view () activeIndex =
    H.fragment
      [ H.h2 [] [H.text "Almaty, Kazakhstan"]
      , panel "About" (activeIndex == 0) (Shown 0) about
      , panel "Etymology" (activeIndex == 1) (Shown 1) etymology
      ]


{- | A React component without state is a function. The @onShow@ callback
prop becomes the message to send, so the panel works for any message type.
-}
panel :: Text -> Bool -> msg -> [Html msg] -> Html msg
panel title isActive onShow children =
  H.section
    [A.className "panel"]
    [ H.h3 [] [H.text title]
    , if isActive
        then H.p [] children
        else H.button [onClick onShow] [H.text "Show"]
    ]


about :: [Html msg]
about =
  [ H.text
      "With a population of about 2 million, Almaty is Kazakhstan's largest city. \
      \From 1929 to 1997, it was its capital city."
  ]


etymology :: [Html msg]
etymology =
  [ H.text "The name comes from "
  , H.span [A.lang "kk-KZ"] [H.text "алма"]
  , H.text
      ", the Kazakh word for \"apple\" and is often translated as \"full of apples\". \
      \In fact, the region surrounding Almaty is thought to be the ancestral home of \
      \the apple, and the wild "
  , H.i [A.lang "la"] [H.text "Malus sieversii"]
  , H.text " is considered a likely candidate for the ancestor of the modern domestic apple."
  ]
