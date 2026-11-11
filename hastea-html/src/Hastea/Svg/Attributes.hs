{- | Attributes for the elements in "Hastea.Svg".

All of them take text, since SVG accepts values such as @\"50%\"@ and
@\"10 5\"@ where one might expect a number. For @class@, @id@ and @style@,
use "Hastea.Html.Attributes"; for anything missing here,
'Hastea.Html.Attributes.attrib'.

'clipPathRef' and 'maskRef' are the @clip-path@ and @mask@ attributes, named
so as not to clash with the elements.
-}
module Hastea.Svg.Attributes
  ( -- * Coordinates and sizes
    viewBox
  , x
  , y
  , width
  , height
  , x1
  , y1
  , x2
  , y2
  , cx
  , cy
  , r
  , rx
  , ry
  , transform
  , preserveAspectRatio

    -- * Shapes
  , d
  , points

    -- * Paint
  , fill
  , fillOpacity
  , fillRule
  , stroke
  , strokeWidth
  , strokeOpacity
  , strokeLinecap
  , strokeLinejoin
  , strokeDasharray
  , strokeDashoffset
  , opacity
  , clipPathRef
  , maskRef

    -- * Gradients
  , offset
  , stopColor
  , stopOpacity
  , gradientUnits
  , gradientTransform

    -- * Text
  , textAnchor
  , dominantBaseline
  , fontSize
  , fontFamily
  , fontWeight
  , dx
  , dy

    -- * Links
  , href
  )
where

import Hastea.Html.Internal (Attribute (..))


-- COORDINATES AND SIZES

viewBox :: String -> Attribute msg
viewBox = Attribute "viewBox"


x :: String -> Attribute msg
x = Attribute "x"


y :: String -> Attribute msg
y = Attribute "y"


width :: String -> Attribute msg
width = Attribute "width"


height :: String -> Attribute msg
height = Attribute "height"


x1 :: String -> Attribute msg
x1 = Attribute "x1"


y1 :: String -> Attribute msg
y1 = Attribute "y1"


x2 :: String -> Attribute msg
x2 = Attribute "x2"


y2 :: String -> Attribute msg
y2 = Attribute "y2"


cx :: String -> Attribute msg
cx = Attribute "cx"


cy :: String -> Attribute msg
cy = Attribute "cy"


r :: String -> Attribute msg
r = Attribute "r"


rx :: String -> Attribute msg
rx = Attribute "rx"


ry :: String -> Attribute msg
ry = Attribute "ry"


transform :: String -> Attribute msg
transform = Attribute "transform"


preserveAspectRatio :: String -> Attribute msg
preserveAspectRatio = Attribute "preserveAspectRatio"


-- SHAPES

d :: String -> Attribute msg
d = Attribute "d"


points :: String -> Attribute msg
points = Attribute "points"


-- PAINT

fill :: String -> Attribute msg
fill = Attribute "fill"


fillOpacity :: String -> Attribute msg
fillOpacity = Attribute "fill-opacity"


fillRule :: String -> Attribute msg
fillRule = Attribute "fill-rule"


stroke :: String -> Attribute msg
stroke = Attribute "stroke"


strokeWidth :: String -> Attribute msg
strokeWidth = Attribute "stroke-width"


strokeOpacity :: String -> Attribute msg
strokeOpacity = Attribute "stroke-opacity"


strokeLinecap :: String -> Attribute msg
strokeLinecap = Attribute "stroke-linecap"


strokeLinejoin :: String -> Attribute msg
strokeLinejoin = Attribute "stroke-linejoin"


strokeDasharray :: String -> Attribute msg
strokeDasharray = Attribute "stroke-dasharray"


strokeDashoffset :: String -> Attribute msg
strokeDashoffset = Attribute "stroke-dashoffset"


opacity :: String -> Attribute msg
opacity = Attribute "opacity"


clipPathRef :: String -> Attribute msg
clipPathRef = Attribute "clip-path"


maskRef :: String -> Attribute msg
maskRef = Attribute "mask"


-- GRADIENTS

offset :: String -> Attribute msg
offset = Attribute "offset"


stopColor :: String -> Attribute msg
stopColor = Attribute "stop-color"


stopOpacity :: String -> Attribute msg
stopOpacity = Attribute "stop-opacity"


gradientUnits :: String -> Attribute msg
gradientUnits = Attribute "gradientUnits"


gradientTransform :: String -> Attribute msg
gradientTransform = Attribute "gradientTransform"


-- TEXT

textAnchor :: String -> Attribute msg
textAnchor = Attribute "text-anchor"


dominantBaseline :: String -> Attribute msg
dominantBaseline = Attribute "dominant-baseline"


fontSize :: String -> Attribute msg
fontSize = Attribute "font-size"


fontFamily :: String -> Attribute msg
fontFamily = Attribute "font-family"


fontWeight :: String -> Attribute msg
fontWeight = Attribute "font-weight"


dx :: String -> Attribute msg
dx = Attribute "dx"


dy :: String -> Attribute msg
dy = Attribute "dy"


-- LINKS

href :: String -> Attribute msg
href = Attribute "href"
