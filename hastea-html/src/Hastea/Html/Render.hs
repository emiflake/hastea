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

import Hastea.Html.Internal


-- | Render a tree as HTML markup.
renderHtml :: Html msg -> String
renderHtml tree = render tree ""


-- | Like 'renderHtml', but starts with @\<!DOCTYPE html\>@.
renderDocument :: Html msg -> String
renderDocument tree = "<!DOCTYPE html>\n" <> renderHtml tree


render :: Html msg -> ShowS
render (Text content) = showString (escapeText content)
render (Lazy _ tree) = render tree
render (Keyed _ tree) = render tree
render (Element tag attributes children)
  | tag `elem` voidElements = open
  | tag `elem` rawTextElements = open . rawChildren . close
  | otherwise = open . foldr (\child rest -> render child . rest) id children . close
 where
  open = showChar '<' . showString tag . renderAttributes attributes . showChar '>'
  close = showString "</" . showString tag . showChar '>'
  rawChildren = showString (concat [content | Text content <- map unwrap children])


renderAttributes :: [Attribute msg] -> ShowS
renderAttributes attributes =
  showString (concatMap (' ' :) (mapMaybe renderAttribute (lastWins attributes)))


{- | When a name is given more than once, keep only the last occurrence,
like repeatedly setting it on a DOM node would.
-}
lastWins :: [Attribute msg] -> [Attribute msg]
lastWins = foldr keep []
 where
  keep attribute rest = case attributeKey attribute of
    Just name | any ((== Just name) . attributeKey) rest -> rest
    _ -> attribute : rest


renderAttribute :: Attribute msg -> Maybe String
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
escapeText :: String -> String
escapeText = concatMap escape
 where
  escape '&' = "&amp;"
  escape '<' = "&lt;"
  escape '>' = "&gt;"
  escape c = [c]


-- | Escape text for use inside a double-quoted attribute value.
escapeAttribute :: String -> String
escapeAttribute = concatMap escape
 where
  escape '&' = "&amp;"
  escape '"' = "&quot;"
  escape '<' = "&lt;"
  escape '>' = "&gt;"
  escape c = [c]


-- Elements that have no closing tag and no children.
voidElements :: [String]
voidElements =
  ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr"]


-- Elements whose text content is not escaped.
rawTextElements :: [String]
rawTextElements = ["script", "style"]
