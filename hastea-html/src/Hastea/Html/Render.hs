{- | Render 'Html' to a string of HTML markup.

Useful for tests, for rendering on a server and for static pages. Event
listeners have no textual form and are left out.
-}
module Hastea.Html.Render
  ( renderHtml
  , renderDocument
  , escapeText
  , escapeAttribute
  )
where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder

import Hastea.Html.Internal


-- | Render a tree as HTML markup.
renderHtml :: Html msg -> Text
renderHtml = Lazy.toStrict . Builder.toLazyText . render


-- | Like 'renderHtml', but starts with @\<!DOCTYPE html\>@.
renderDocument :: Html msg -> Text
renderDocument tree = "<!DOCTYPE html>\n" <> renderHtml tree


render :: Html msg -> Builder
render (Text content) = Builder.fromText (escapeText content)
render (Lazy _ tree) = render tree
render (Keyed _ tree) = render tree
render (Extend _ fallback) = render fallback
render (Fragment trees) = foldMap render trees
render (Element tag attributes children)
  | tag `elem` voidElements = open
  | tag `elem` rawTextElements = open <> rawChildren <> close
  | otherwise = open <> foldMap render children <> close
 where
  open = "<" <> Builder.fromText tag <> renderAttributes attributes <> ">"
  close = "</" <> Builder.fromText tag <> ">"
  rawChildren = foldMap Builder.fromText [content | Text content <- map unwrap children]


renderAttributes :: [Attribute msg] -> Builder
renderAttributes attributes =
  foldMap (\attribute -> " " <> Builder.fromText attribute) (mapMaybe renderAttribute (lastWins attributes))


{- | When a name is given more than once, keep only the last occurrence,
like repeatedly setting it on a DOM node would.
-}
lastWins :: [Attribute msg] -> [Attribute msg]
lastWins = foldr keep []
 where
  keep attribute rest = case attributeKey attribute of
    Just name | any ((== Just name) . attributeKey) rest -> rest
    _ -> attribute : rest


renderAttribute :: Attribute msg -> Maybe Text
renderAttribute attribute = case attribute of
  Attribute name value -> Just (pair name value)
  BoolAttribute name enabled -> flag name enabled
  Property name (PropertyString value) -> Just (pair name value)
  Property name (PropertyBool enabled) -> flag name enabled
  Listener{} -> Nothing
 where
  pair name value = name <> "=\"" <> escapeAttribute value <> "\""
  flag name enabled = if enabled then Just name else Nothing


-- | Escape text for use between tags.
escapeText :: Text -> Text
escapeText = T.concatMap escape
 where
  escape '&' = "&amp;"
  escape '<' = "&lt;"
  escape '>' = "&gt;"
  escape c = T.singleton c


-- | Escape text for use inside a double-quoted attribute value.
escapeAttribute :: Text -> Text
escapeAttribute = T.concatMap escape
 where
  escape '&' = "&amp;"
  escape '"' = "&quot;"
  escape '<' = "&lt;"
  escape '>' = "&gt;"
  escape c = T.singleton c


-- Elements that have no closing tag and no children.
voidElements :: [Text]
voidElements =
  ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr"]


-- Elements whose text content is not escaped.
rawTextElements :: [Text]
rawTextElements = ["script", "style"]
