{-# LANGUAGE NamedFieldPuns #-}
module Hastea.Html
  ( Html
  , Attribute

  -- * Attributes
  , attrib
  , on
  , node

  , className
  , value

  -- * Events
  , onClick
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


className :: String -> Attribute a
className = attrib "class"


value :: String -> Attribute a
value = attrib "value"



-- Events


onClick :: msg -> Attribute msg
onClick msg = on "click" (pure msg)


onInput :: (String -> msg) -> Attribute msg
onInput toMsg = on "input" (fmap toMsg $ Decode.field "target" (Decode.field "value" Decode.string))


-- Tags


text :: String -> Html a
text = VText


div :: [Attribute a] -> [Html a] -> Html a
div = node "div"


span :: [Attribute a] -> [Html a] -> Html a
span = node "span"


a :: [Attribute a] -> [Html a] -> Html a
a = node "a"


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
