{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Hastea.Html
  ( Html
  , Attribute

  -- * Attributes
  , attrib
  , on
  , node

  , className
  , value
  , style
  , type_
  , placeholder
  , href
  , for
  , id_
  , checked

  -- * Events
  , onClick
  , onKeypress
  , onInput

  -- * Tags
  , text
  , div
  , span
  , a
  , strong
  , br
  , ol
  , li
  , ul
  , button
  , input
  , p
  , h1
  , h2
  , h3
  , h4
  , h5
  , section
  , footer
  , header
  , label
  )
  where

import           Prelude                    hiding (div, span)


import           Hastea.Decode              (Decode)
import qualified Hastea.Decode              as Decode
import           Hastea.Internal.VirtualDOM hiding (value)

type Html a = VNode a
type Attribute a = VAttribute a



-- Primitives


attrib :: String -> String -> Attribute a
attrib k v =
  VAttribute (Attrib k v)


on :: String -> Decode msg -> Attribute msg
on evt decoder =
  VEventListener evt decoder


node :: String -> [Attribute a] -> [Html a] -> Html a
node = VNode


-- Attributes


toHTMLBool :: Bool -> String
toHTMLBool = \case
  True -> "1"
  False -> "0"

className :: String -> Attribute a
className = attrib "class"


style :: String -> Attribute a
style = attrib "style"


value :: String -> Attribute a
value = attrib "value"


type_ :: String -> Attribute a
type_ = attrib "type"


placeholder :: String -> Attribute a
placeholder = attrib "placeholder"


href :: String -> Attribute a
href = attrib "href"


id_ :: String -> Attribute a
id_ = attrib "id"

  
checked :: Bool -> Attribute a
checked = attrib "checked" . toHTMLBool


for :: String -> Attribute a
for = attrib "for"



-- Events


onClick :: msg -> Attribute msg
onClick msg = on "click" (pure msg)


onKeypress :: (String -> msg) -> Attribute msg
onKeypress toMsg = on "keypress" (toMsg <$> Decode.field "key" Decode.string)


onInput :: (String -> msg) -> Attribute msg
onInput toMsg = on "input" (toMsg <$> Decode.field "target" (Decode.field "value" Decode.string))


-- Tags


text :: String -> Html a
text = VText


div :: [Attribute a] -> [Html a] -> Html a
div = node "div"


span :: [Attribute a] -> [Html a] -> Html a
span = node "span"


a :: [Attribute a] -> [Html a] -> Html a
a = node "a"


h1 :: [Attribute a] -> [Html a] -> Html a
h1 = node "h1"


h2 :: [Attribute a] -> [Html a] -> Html a
h2 = node "h2"


h3 :: [Attribute a] -> [Html a] -> Html a
h3 = node "h3"


h4 :: [Attribute a] -> [Html a] -> Html a
h4 = node "h4"


h5 :: [Attribute a] -> [Html a] -> Html a
h5 = node "h4"


p :: [Attribute a] -> [Html a] -> Html a
p = node "p"


ul :: [Attribute a] -> [Html a] -> Html a
ul = node "ul"


ol :: [Attribute a] -> [Html a] -> Html a
ol = node "ol"


li :: [Attribute a] -> [Html a] -> Html a
li = node "li"


br :: Html a
br = node "br" [] []


strong :: [Attribute a] -> [Html a] -> Html a
strong = node "strong"


button :: [Attribute a] -> [Html a] -> Html a
button = node "button"


input :: [Attribute a] -> [Html a] -> Html a
input = node "input"


section :: [Attribute a] -> [Html a] -> Html a
section = node "section"


header :: [Attribute a] -> [Html a] -> Html a
header = node "header"


footer :: [Attribute a] -> [Html a] -> Html a
footer = node "footer"


label :: [Attribute a] -> [Html a] -> Html a
label = node "label"
