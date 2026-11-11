{- | React: context.
<https://react.dev/learn/passing-data-deeply-with-context>

In React a @Heading@ learns its level from the nearest @Section@ through a
context, because passing it down as a prop through every component in
between is tedious. Here a section passes the level to its children as an
ordinary argument.
-}
module ReactComparison.Headings
  ( view
  )
where

import Hastea.Html hiding (section)
import Hastea.Html qualified as Html
import Hastea.Html.Attributes (className)


view :: Html msg
view =
  section 0 $ \level ->
    [ heading level "Title"
    , section level $ \level2 ->
        [ heading level2 "Heading"
        , heading level2 "Heading"
        , section level2 $ \level3 ->
            [ heading level3 "Sub-heading"
            , heading level3 "Sub-heading"
            ]
        ]
    ]


-- | A section one level deeper than its surroundings.
section :: Int -> (Int -> [Html msg]) -> Html msg
section level children =
  Html.section [className "section"] (children (level + 1))


heading :: Int -> String -> Html msg
heading level content = tag [] [text content]
 where
  tag = case level of
    1 -> h1
    2 -> h2
    3 -> h3
    4 -> h4
    5 -> h5
    _ -> h6
