{- | React: context.
<https://react.dev/learn/passing-data-deeply-with-context>

In React a @Heading@ learns its level from the nearest @Section@ through a
context, so that the level does not have to be passed down by hand. Here
the parts of the page are functions of the level they are at ('Leveled'),
and 'section' calls its children with a level one deeper.
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
  page 0
 where
  page =
    section
      [ heading "Title"
      , section
          [ heading "Heading"
          , heading "Heading"
          , section
              [ heading "Sub-heading"
              , heading "Sub-heading"
              ]
          ]
      ]


-- | A piece of the page that depends on how deeply it is nested.
type Leveled msg = Int -> Html msg


-- | Puts its children one level deeper than itself.
section :: [Leveled msg] -> Leveled msg
section children level =
  Html.section [className "section"] [child (level + 1) | child <- children]


-- | A heading of the size that fits its level.
heading :: String -> Leveled msg
heading content level = tag [] [text content]
 where
  tag = case level of
    1 -> h1
    2 -> h2
    3 -> h3
    4 -> h4
    5 -> h5
    _ -> h6
