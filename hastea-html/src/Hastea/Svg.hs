{- | SVG elements, for drawing inside a view.

@
import Hastea.Svg qualified as Svg
import Hastea.Svg.Attributes qualified as Svg

icon :: Html msg
icon =
  Svg.svg
    [Svg.viewBox "0 0 24 24", Svg.width "24", Svg.height "24"]
    [Svg.circle [Svg.cx "12", Svg.cy "12", Svg.r "10", Svg.fill "tomato"] []]
@

These are ordinary 'Html' nodes and can go anywhere in a view. Everything
inside 'svg' is created as SVG, also elements made with 'Hastea.Html.node'.
Several names are the same as in "Hastea.Html", so import this module
qualified. 'text_' and 'use_' have an underscore to keep them apart from
'Hastea.Html.text' and @Hastea.Component.use@.
-}
module Hastea.Svg
  ( -- * Structure
    svg
  , g
  , defs
  , symbol
  , use_
  , foreignObject

    -- * Shapes
  , path
  , rect
  , circle
  , ellipse
  , line
  , polyline
  , polygon
  , image

    -- * Text
  , text_
  , tspan
  , title
  , desc

    -- * Paint
  , linearGradient
  , radialGradient
  , stop
  , pattern_
  , clipPath
  , mask
  , marker
  )
where

import Hastea.Html.Internal (Attribute, Html (..))


svgNode :: String -> [Attribute msg] -> [Html msg] -> Html msg
svgNode = Element


-- STRUCTURE

-- | The outermost element of a drawing.
svg :: [Attribute msg] -> [Html msg] -> Html msg
svg = svgNode "svg"


-- | A group of elements.
g :: [Attribute msg] -> [Html msg] -> Html msg
g = svgNode "g"


-- | Definitions to refer to elsewhere, such as gradients.
defs :: [Attribute msg] -> [Html msg] -> Html msg
defs = svgNode "defs"


-- | A drawing to show with 'use_'.
symbol :: [Attribute msg] -> [Html msg] -> Html msg
symbol = svgNode "symbol"


-- | Show another element here, named by @href@.
use_ :: [Attribute msg] -> [Html msg] -> Html msg
use_ = svgNode "use"


-- | HTML inside a drawing.
foreignObject :: [Attribute msg] -> [Html msg] -> Html msg
foreignObject = svgNode "foreignObject"


-- SHAPES

path :: [Attribute msg] -> [Html msg] -> Html msg
path = svgNode "path"


rect :: [Attribute msg] -> [Html msg] -> Html msg
rect = svgNode "rect"


circle :: [Attribute msg] -> [Html msg] -> Html msg
circle = svgNode "circle"


ellipse :: [Attribute msg] -> [Html msg] -> Html msg
ellipse = svgNode "ellipse"


line :: [Attribute msg] -> [Html msg] -> Html msg
line = svgNode "line"


polyline :: [Attribute msg] -> [Html msg] -> Html msg
polyline = svgNode "polyline"


polygon :: [Attribute msg] -> [Html msg] -> Html msg
polygon = svgNode "polygon"


image :: [Attribute msg] -> [Html msg] -> Html msg
image = svgNode "image"


-- TEXT

-- | Text in a drawing. Its content is given with 'Hastea.Html.text'.
text_ :: [Attribute msg] -> [Html msg] -> Html msg
text_ = svgNode "text"


tspan :: [Attribute msg] -> [Html msg] -> Html msg
tspan = svgNode "tspan"


-- | A description of an element, shown as a tooltip.
title :: [Attribute msg] -> [Html msg] -> Html msg
title = svgNode "title"


-- | A longer description, for assistive technology.
desc :: [Attribute msg] -> [Html msg] -> Html msg
desc = svgNode "desc"


-- PAINT

linearGradient :: [Attribute msg] -> [Html msg] -> Html msg
linearGradient = svgNode "linearGradient"


radialGradient :: [Attribute msg] -> [Html msg] -> Html msg
radialGradient = svgNode "radialGradient"


-- | One colour of a gradient.
stop :: [Attribute msg] -> [Html msg] -> Html msg
stop = svgNode "stop"


pattern_ :: [Attribute msg] -> [Html msg] -> Html msg
pattern_ = svgNode "pattern"


clipPath :: [Attribute msg] -> [Html msg] -> Html msg
clipPath = svgNode "clipPath"


mask :: [Attribute msg] -> [Html msg] -> Html msg
mask = svgNode "mask"


marker :: [Attribute msg] -> [Html msg] -> Html msg
marker = svgNode "marker"
