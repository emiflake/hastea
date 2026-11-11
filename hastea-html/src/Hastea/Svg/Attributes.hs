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

import Data.Text (Text)
import Hastea.Html.Internal (Attribute (..))


-- COORDINATES AND SIZES

viewBox :: Text -> Attribute msg
viewBox = Attribute "viewBox"


x :: Text -> Attribute msg
x = Attribute "x"


y :: Text -> Attribute msg
y = Attribute "y"


width :: Text -> Attribute msg
width = Attribute "width"


height :: Text -> Attribute msg
height = Attribute "height"


x1 :: Text -> Attribute msg
x1 = Attribute "x1"


y1 :: Text -> Attribute msg
y1 = Attribute "y1"


x2 :: Text -> Attribute msg
x2 = Attribute "x2"


y2 :: Text -> Attribute msg
y2 = Attribute "y2"


cx :: Text -> Attribute msg
cx = Attribute "cx"


cy :: Text -> Attribute msg
cy = Attribute "cy"


r :: Text -> Attribute msg
r = Attribute "r"


rx :: Text -> Attribute msg
rx = Attribute "rx"


ry :: Text -> Attribute msg
ry = Attribute "ry"


transform :: Text -> Attribute msg
transform = Attribute "transform"


preserveAspectRatio :: Text -> Attribute msg
preserveAspectRatio = Attribute "preserveAspectRatio"


-- SHAPES

d :: Text -> Attribute msg
d = Attribute "d"


points :: Text -> Attribute msg
points = Attribute "points"


-- PAINT

fill :: Text -> Attribute msg
fill = Attribute "fill"


fillOpacity :: Text -> Attribute msg
fillOpacity = Attribute "fill-opacity"


fillRule :: Text -> Attribute msg
fillRule = Attribute "fill-rule"


stroke :: Text -> Attribute msg
stroke = Attribute "stroke"


strokeWidth :: Text -> Attribute msg
strokeWidth = Attribute "stroke-width"


strokeOpacity :: Text -> Attribute msg
strokeOpacity = Attribute "stroke-opacity"


strokeLinecap :: Text -> Attribute msg
strokeLinecap = Attribute "stroke-linecap"


strokeLinejoin :: Text -> Attribute msg
strokeLinejoin = Attribute "stroke-linejoin"


strokeDasharray :: Text -> Attribute msg
strokeDasharray = Attribute "stroke-dasharray"


strokeDashoffset :: Text -> Attribute msg
strokeDashoffset = Attribute "stroke-dashoffset"


opacity :: Text -> Attribute msg
opacity = Attribute "opacity"


clipPathRef :: Text -> Attribute msg
clipPathRef = Attribute "clip-path"


maskRef :: Text -> Attribute msg
maskRef = Attribute "mask"


-- GRADIENTS

offset :: Text -> Attribute msg
offset = Attribute "offset"


stopColor :: Text -> Attribute msg
stopColor = Attribute "stop-color"


stopOpacity :: Text -> Attribute msg
stopOpacity = Attribute "stop-opacity"


gradientUnits :: Text -> Attribute msg
gradientUnits = Attribute "gradientUnits"


gradientTransform :: Text -> Attribute msg
gradientTransform = Attribute "gradientTransform"


-- TEXT

textAnchor :: Text -> Attribute msg
textAnchor = Attribute "text-anchor"


dominantBaseline :: Text -> Attribute msg
dominantBaseline = Attribute "dominant-baseline"


fontSize :: Text -> Attribute msg
fontSize = Attribute "font-size"


fontFamily :: Text -> Attribute msg
fontFamily = Attribute "font-family"


fontWeight :: Text -> Attribute msg
fontWeight = Attribute "font-weight"


dx :: Text -> Attribute msg
dx = Attribute "dx"


dy :: Text -> Attribute msg
dy = Attribute "dy"


-- LINKS

href :: Text -> Attribute msg
href = Attribute "href"
