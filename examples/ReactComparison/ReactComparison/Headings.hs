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

import Data.Text (Text)

import Hastea.Html (Html)
import Hastea.Html qualified as H
import Hastea.Html.Attributes qualified as A


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
  H.section [A.className "section"] [child (level + 1) | child <- children]


-- | A heading of the size that fits its level.
heading :: Text -> Leveled msg
heading title level = tag [] [H.text title]
 where
  tag = case level of
    1 -> H.h1
    2 -> H.h2
    3 -> H.h3
    4 -> H.h4
    5 -> H.h5
    _ -> H.h6
