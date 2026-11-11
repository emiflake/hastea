{- | Attributes for the elements in "Hastea.Html".

Attribute names follow the DOM's camel case: 'tabIndex', 'readOnly',
'colSpan'. Names that are Haskell keywords or clash with the "Prelude" get a
trailing underscore: 'id_', 'type_', 'min_', 'max_', 'span_', 'data_',
'default_', 'as_'. The @class@ attribute is 'className'.

A few names are also element names in "Hastea.Html" ('title', 'style',
'form', 'label', 'cite', 'abbr', 'slot', 'data_', 'span_'); import one of the
two modules qualified, or hide the ones you do not need.

When an attribute is given more than once on an element, the last one wins.
-}
module Hastea.Html.Attributes
  ( -- * Primitives
    attrib
  , boolAttrib
  , property
  , boolProperty

    -- * Helpers
  , classList
  , styles
  , dataAttr
  , aria

    -- * Global attributes
  , accessKey
  , autocapitalize
  , autofocus
  , className
  , contentEditable
  , dir
  , draggable
  , enterKeyHint
  , hidden
  , id_
  , inert
  , inputMode
  , is
  , itemProp
  , lang
  , nonce
  , popover
  , role
  , slot
  , spellcheck
  , style
  , tabIndex
  , title
  , translate

    -- * Links and navigation
  , href
  , target
  , download
  , hreflang
  , ping
  , rel
  , referrerPolicy

    -- * Form controls
  , value
  , checked
  , selected
  , type_
  , name
  , placeholder
  , disabled
  , readOnly
  , required
  , multiple
  , autocomplete
  , for
  , form
  , list
  , min_
  , max_
  , step
  , minLength
  , maxLength
  , pattern
  , size
  , accept
  , capture
  , dirname
  , rows
  , cols
  , wrap
  , label
  , low
  , high
  , optimum
  , popoverTarget
  , popoverTargetAction

    -- * Forms
  , action
  , method
  , enctype
  , acceptCharset
  , noValidate
  , formAction
  , formMethod
  , formEnctype
  , formTarget
  , formNoValidate

    -- * Images and embedded content
  , src
  , alt
  , width
  , height
  , srcset
  , sizes
  , loading
  , decoding
  , fetchPriority
  , crossOrigin
  , useMap
  , isMap
  , coords
  , shape
  , srcdoc
  , sandbox
  , allow
  , allowFullscreen
  , data_

    -- * Audio and video
  , autoplay
  , controls
  , loop
  , muted
  , playsInline
  , poster
  , preload
  , kind
  , srclang
  , default_

    -- * Tables
  , colSpan
  , rowSpan
  , headers
  , scope
  , span_

    -- * Lists
  , reversed
  , start

    -- * Document metadata and scripts
  , charset
  , content
  , httpEquiv
  , media
  , integrity
  , as_
  , blocking
  , async
  , defer
  , noModule

    -- * Miscellaneous
  , cite
  , dateTime
  , open
  , abbr
  )
where

import Data.List (intercalate)

import Hastea.Html.Internal (Attribute (..), PropertyValue (..))


-- PRIMITIVES

-- | An attribute with the given name and value.
attrib :: String -> String -> Attribute msg
attrib = Attribute


-- | A boolean attribute: present when 'True', absent when 'False'.
boolAttrib :: String -> Bool -> Attribute msg
boolAttrib = BoolAttribute


{- | A string-valued DOM property, assigned directly on the node rather than
set as an attribute.
-}
property :: String -> String -> Attribute msg
property key = Property key . PropertyString


-- | A boolean-valued DOM property.
boolProperty :: String -> Bool -> Attribute msg
boolProperty key = Property key . PropertyBool


-- An attribute whose value is an integer.
intAttrib :: String -> Int -> Attribute msg
intAttrib key = Attribute key . show


-- An enumerated attribute that takes the keywords "true" and "false".
trueFalseAttrib :: String -> Bool -> Attribute msg
trueFalseAttrib key enabled = Attribute key (if enabled then "true" else "false")


-- HELPERS

{- | The @class@ attribute, built from the class names whose condition holds.

@
classList [("todo", True), ("completed", isDone)]
@
-}
classList :: [(String, Bool)] -> Attribute msg
classList classes = className (unwords [name_ | (name_, True) <- classes])


{- | The @style@ attribute, built from property-value pairs.

@
styles [("display", "block"), ("color", "red")]
@
-}
styles :: [(String, String)] -> Attribute msg
styles declarations =
  style (intercalate "; " [key <> ": " <> val | (key, val) <- declarations])


-- | A custom data attribute: @dataAttr "id" "5"@ is @data-id="5"@.
dataAttr :: String -> String -> Attribute msg
dataAttr key = Attribute ("data-" <> key)


-- | An ARIA attribute: @aria "label" "Close"@ is @aria-label="Close"@.
aria :: String -> String -> Attribute msg
aria key = Attribute ("aria-" <> key)


-- PROPERTIES

{- | The current value of a form control. This is a property, so it stays in
sync with what the user has typed.
-}
value :: String -> Attribute msg
value = property "value"


-- | Whether a checkbox or radio button is checked. This is a property.
checked :: Bool -> Attribute msg
checked = boolProperty "checked"


-- | Whether an 'Hastea.Html.option' is selected. This is a property.
selected :: Bool -> Attribute msg
selected = boolProperty "selected"



-- GLOBAL ATTRIBUTES


-- | A keyboard shortcut for the element.
accessKey :: String -> Attribute msg
accessKey = Attribute "accesskey"


-- | How typed text is capitalised: @"off"@, @"sentences"@, @"words"@ or @"characters"@.
autocapitalize :: String -> Attribute msg
autocapitalize = Attribute "autocapitalize"


-- | Focus the element when the page loads.
autofocus :: Bool -> Attribute msg
autofocus = BoolAttribute "autofocus"


-- | The element's CSS classes, separated by spaces.
className :: String -> Attribute msg
className = Attribute "class"


-- | Whether the user can edit the element's content.
contentEditable :: Bool -> Attribute msg
contentEditable = trueFalseAttrib "contenteditable"


-- | Text direction: @"ltr"@, @"rtl"@ or @"auto"@.
dir :: String -> Attribute msg
dir = Attribute "dir"


-- | Whether the element can be dragged.
draggable :: Bool -> Attribute msg
draggable = trueFalseAttrib "draggable"


-- | The label of the enter key on virtual keyboards.
enterKeyHint :: String -> Attribute msg
enterKeyHint = Attribute "enterkeyhint"


-- | Hide the element.
hidden :: Bool -> Attribute msg
hidden = BoolAttribute "hidden"


-- | The element's unique identifier.
id_ :: String -> Attribute msg
id_ = Attribute "id"


-- | Make the element and its descendants non-interactive.
inert :: Bool -> Attribute msg
inert = BoolAttribute "inert"


-- | Which virtual keyboard to show, for example @"numeric"@.
inputMode :: String -> Attribute msg
inputMode = Attribute "inputmode"


-- | The name of the custom element this element should behave as.
is :: String -> Attribute msg
is = Attribute "is"


-- | A microdata property name.
itemProp :: String -> Attribute msg
itemProp = Attribute "itemprop"


-- | The language of the element's content.
lang :: String -> Attribute msg
lang = Attribute "lang"


-- | A cryptographic nonce for Content Security Policy.
nonce :: String -> Attribute msg
nonce = Attribute "nonce"


-- | Make the element a popover: @"auto"@ or @"manual"@.
popover :: String -> Attribute msg
popover = Attribute "popover"


-- | The element's ARIA role.
role :: String -> Attribute msg
role = Attribute "role"


-- | The web component slot the element is assigned to.
slot :: String -> Attribute msg
slot = Attribute "slot"


-- | Whether to check the element's spelling.
spellcheck :: Bool -> Attribute msg
spellcheck = trueFalseAttrib "spellcheck"


-- | Inline CSS. See also 'styles'.
style :: String -> Attribute msg
style = Attribute "style"


-- | The element's position in the tab order.
tabIndex :: Int -> Attribute msg
tabIndex = intAttrib "tabindex"


-- | Advisory text, usually shown as a tooltip.
title :: String -> Attribute msg
title = Attribute "title"


-- | Whether the element's content should be translated.
translate :: Bool -> Attribute msg
translate enabled = Attribute "translate" (if enabled then "yes" else "no")


-- LINKS AND NAVIGATION


-- | The URL a link points to.
href :: String -> Attribute msg
href = Attribute "href"


-- | Where to open a link or submit a form, for example @"_blank"@.
target :: String -> Attribute msg
target = Attribute "target"


-- | Download the link target, using this file name.
download :: String -> Attribute msg
download = Attribute "download"


-- | The language of the linked resource.
hreflang :: String -> Attribute msg
hreflang = Attribute "hreflang"


-- | URLs to notify when a link is followed.
ping :: String -> Attribute msg
ping = Attribute "ping"


-- | The relationship between the document and the linked resource.
rel :: String -> Attribute msg
rel = Attribute "rel"


-- | Which referrer to send when fetching the resource.
referrerPolicy :: String -> Attribute msg
referrerPolicy = Attribute "referrerpolicy"


-- FORM CONTROLS


-- | The type of an 'Hastea.Html.input', 'Hastea.Html.button', 'Hastea.Html.script' and others.
type_ :: String -> Attribute msg
type_ = Attribute "type"


-- | The name of a form control, submitted with the form.
name :: String -> Attribute msg
name = Attribute "name"


-- | A hint shown while a field is empty.
placeholder :: String -> Attribute msg
placeholder = Attribute "placeholder"


-- | Disable a form control.
disabled :: Bool -> Attribute msg
disabled = BoolAttribute "disabled"


-- | Prevent the user from editing a field.
readOnly :: Bool -> Attribute msg
readOnly = BoolAttribute "readonly"


-- | Require a value before the form can be submitted.
required :: Bool -> Attribute msg
required = BoolAttribute "required"


-- | Allow more than one value.
multiple :: Bool -> Attribute msg
multiple = BoolAttribute "multiple"


-- | A hint for form autofill, such as @"off"@ or @"email"@.
autocomplete :: String -> Attribute msg
autocomplete = Attribute "autocomplete"


-- | The id of the control a 'Hastea.Html.label' or 'Hastea.Html.output' belongs to.
for :: String -> Attribute msg
for = Attribute "for"


-- | The id of the form a control belongs to.
form :: String -> Attribute msg
form = Attribute "form"


-- | The id of a 'Hastea.Html.datalist' with suggestions.
list :: String -> Attribute msg
list = Attribute "list"


-- | The minimum value.
min_ :: String -> Attribute msg
min_ = Attribute "min"


-- | The maximum value.
max_ :: String -> Attribute msg
max_ = Attribute "max"


-- | The granularity of the value, or @"any"@.
step :: String -> Attribute msg
step = Attribute "step"


-- | The minimum number of characters.
minLength :: Int -> Attribute msg
minLength = intAttrib "minlength"


-- | The maximum number of characters.
maxLength :: Int -> Attribute msg
maxLength = intAttrib "maxlength"


-- | A regular expression the value must match.
pattern :: String -> Attribute msg
pattern = Attribute "pattern"


-- | The width of a field in characters, or the number of visible options.
size :: Int -> Attribute msg
size = intAttrib "size"


-- | The file types a file input accepts.
accept :: String -> Attribute msg
accept = Attribute "accept"


-- | Which camera or microphone to capture a file from.
capture :: String -> Attribute msg
capture = Attribute "capture"


-- | The name under which to submit the field's text direction.
dirname :: String -> Attribute msg
dirname = Attribute "dirname"


-- | The number of visible lines of a 'Hastea.Html.textarea'.
rows :: Int -> Attribute msg
rows = intAttrib "rows"


-- | The width of a 'Hastea.Html.textarea' in characters.
cols :: Int -> Attribute msg
cols = intAttrib "cols"


-- | How a 'Hastea.Html.textarea' wraps its value: @"soft"@ or @"hard"@.
wrap :: String -> Attribute msg
wrap = Attribute "wrap"


-- | The label of an 'Hastea.Html.option', 'Hastea.Html.optgroup' or 'Hastea.Html.track'.
label :: String -> Attribute msg
label = Attribute "label"


-- | The upper bound of the low range of a 'Hastea.Html.meter'.
low :: String -> Attribute msg
low = Attribute "low"


-- | The lower bound of the high range of a 'Hastea.Html.meter'.
high :: String -> Attribute msg
high = Attribute "high"


-- | The optimal value of a 'Hastea.Html.meter'.
optimum :: String -> Attribute msg
optimum = Attribute "optimum"


-- | The id of the popover a button controls.
popoverTarget :: String -> Attribute msg
popoverTarget = Attribute "popovertarget"


-- | What a button does to its popover: @"show"@, @"hide"@ or @"toggle"@.
popoverTargetAction :: String -> Attribute msg
popoverTargetAction = Attribute "popovertargetaction"


-- FORMS


-- | The URL a form is submitted to.
action :: String -> Attribute msg
action = Attribute "action"


-- | The HTTP method a form is submitted with.
method :: String -> Attribute msg
method = Attribute "method"


-- | How form data is encoded.
enctype :: String -> Attribute msg
enctype = Attribute "enctype"


-- | The character encodings a form is submitted in.
acceptCharset :: String -> Attribute msg
acceptCharset = Attribute "accept-charset"


-- | Skip validation when a form is submitted.
noValidate :: Bool -> Attribute msg
noValidate = BoolAttribute "novalidate"


-- | Override the form's 'action' for one submit button.
formAction :: String -> Attribute msg
formAction = Attribute "formaction"


-- | Override the form's 'method' for one submit button.
formMethod :: String -> Attribute msg
formMethod = Attribute "formmethod"


-- | Override the form's 'enctype' for one submit button.
formEnctype :: String -> Attribute msg
formEnctype = Attribute "formenctype"


-- | Override the form's 'target' for one submit button.
formTarget :: String -> Attribute msg
formTarget = Attribute "formtarget"


-- | Skip validation for one submit button.
formNoValidate :: Bool -> Attribute msg
formNoValidate = BoolAttribute "formnovalidate"


-- IMAGES AND EMBEDDED CONTENT


-- | The URL of an embedded resource.
src :: String -> Attribute msg
src = Attribute "src"


-- | Text to use when an image cannot be shown.
alt :: String -> Attribute msg
alt = Attribute "alt"


-- | The width in CSS pixels.
width :: Int -> Attribute msg
width = intAttrib "width"


-- | The height in CSS pixels.
height :: Int -> Attribute msg
height = intAttrib "height"


-- | Candidate images for different screen sizes and densities.
srcset :: String -> Attribute msg
srcset = Attribute "srcset"


-- | The layout width of an image for different media conditions.
sizes :: String -> Attribute msg
sizes = Attribute "sizes"


-- | When to load the resource: @"lazy"@ or @"eager"@.
loading :: String -> Attribute msg
loading = Attribute "loading"


-- | How to decode an image: @"sync"@, @"async"@ or @"auto"@.
decoding :: String -> Attribute msg
decoding = Attribute "decoding"


-- | The priority of the fetch: @"high"@, @"low"@ or @"auto"@.
fetchPriority :: String -> Attribute msg
fetchPriority = Attribute "fetchpriority"


-- | How to handle cross-origin requests.
crossOrigin :: String -> Attribute msg
crossOrigin = Attribute "crossorigin"


-- | The name of the image map to use.
useMap :: String -> Attribute msg
useMap = Attribute "usemap"


-- | Mark an image as a server-side image map.
isMap :: Bool -> Attribute msg
isMap = BoolAttribute "ismap"


-- | The coordinates of an 'Hastea.Html.area'.
coords :: String -> Attribute msg
coords = Attribute "coords"


-- | The shape of an 'Hastea.Html.area'.
shape :: String -> Attribute msg
shape = Attribute "shape"


-- | The HTML content of an 'Hastea.Html.iframe'.
srcdoc :: String -> Attribute msg
srcdoc = Attribute "srcdoc"


-- | Restrictions on the content of an 'Hastea.Html.iframe'.
sandbox :: String -> Attribute msg
sandbox = Attribute "sandbox"


-- | The permissions policy of an 'Hastea.Html.iframe'.
allow :: String -> Attribute msg
allow = Attribute "allow"


-- | Let an 'Hastea.Html.iframe' enter fullscreen.
allowFullscreen :: Bool -> Attribute msg
allowFullscreen = BoolAttribute "allowfullscreen"


-- | The URL of an 'Hastea.Html.object' resource. For @data-*@ attributes see 'dataAttr'.
data_ :: String -> Attribute msg
data_ = Attribute "data"


-- AUDIO AND VIDEO


-- | Start playback as soon as possible.
autoplay :: Bool -> Attribute msg
autoplay = BoolAttribute "autoplay"


-- | Show playback controls.
controls :: Bool -> Attribute msg
controls = BoolAttribute "controls"


-- | Restart playback when it ends.
loop :: Bool -> Attribute msg
loop = BoolAttribute "loop"


-- | Mute the audio by default.
muted :: Bool -> Attribute msg
muted = BoolAttribute "muted"


-- | Play video inline rather than in fullscreen.
playsInline :: Bool -> Attribute msg
playsInline = BoolAttribute "playsinline"


-- | An image to show before a video plays.
poster :: String -> Attribute msg
poster = Attribute "poster"


-- | How much to preload: @"none"@, @"metadata"@ or @"auto"@.
preload :: String -> Attribute msg
preload = Attribute "preload"


-- | The kind of a text 'Hastea.Html.track'.
kind :: String -> Attribute msg
kind = Attribute "kind"


-- | The language of a text 'Hastea.Html.track'.
srclang :: String -> Attribute msg
srclang = Attribute "srclang"


-- | Enable a text 'Hastea.Html.track' by default.
default_ :: Bool -> Attribute msg
default_ = BoolAttribute "default"


-- TABLES


-- | The number of columns a cell spans.
colSpan :: Int -> Attribute msg
colSpan = intAttrib "colspan"


-- | The number of rows a cell spans.
rowSpan :: Int -> Attribute msg
rowSpan = intAttrib "rowspan"


-- | The ids of the header cells for a cell.
headers :: String -> Attribute msg
headers = Attribute "headers"


-- | Which cells a header cell applies to.
scope :: String -> Attribute msg
scope = Attribute "scope"


-- | The number of columns a 'Hastea.Html.col' or 'Hastea.Html.colgroup' spans.
span_ :: Int -> Attribute msg
span_ = intAttrib "span"


-- LISTS


-- | Number an 'Hastea.Html.ol' in descending order.
reversed :: Bool -> Attribute msg
reversed = BoolAttribute "reversed"


-- | The number of the first item of an 'Hastea.Html.ol'.
start :: Int -> Attribute msg
start = intAttrib "start"


-- DOCUMENT METADATA AND SCRIPTS


-- | The character encoding of the document.
charset :: String -> Attribute msg
charset = Attribute "charset"


-- | The value of a 'Hastea.Html.meta' element.
content :: String -> Attribute msg
content = Attribute "content"


-- | A pragma directive for a 'Hastea.Html.meta' element.
httpEquiv :: String -> Attribute msg
httpEquiv = Attribute "http-equiv"


-- | The media the resource applies to.
media :: String -> Attribute msg
media = Attribute "media"


-- | Subresource integrity metadata.
integrity :: String -> Attribute msg
integrity = Attribute "integrity"


-- | The kind of content a preload 'Hastea.Html.link' fetches.
as_ :: String -> Attribute msg
as_ = Attribute "as"


-- | Which operations wait for the resource, for example @"render"@.
blocking :: String -> Attribute msg
blocking = Attribute "blocking"


-- | Run a script as soon as it is available, without blocking parsing.
async :: Bool -> Attribute msg
async = BoolAttribute "async"


-- | Run a script after the document has been parsed.
defer :: Bool -> Attribute msg
defer = BoolAttribute "defer"


-- | Skip a script in browsers that support modules.
noModule :: Bool -> Attribute msg
noModule = BoolAttribute "nomodule"


-- MISCELLANEOUS


-- | The URL of the source of a quotation or edit.
cite :: String -> Attribute msg
cite = Attribute "cite"


-- | A machine-readable date or time.
dateTime :: String -> Attribute msg
dateTime = Attribute "datetime"


-- | Whether a 'Hastea.Html.details' or 'Hastea.Html.dialog' is open.
open :: Bool -> Attribute msg
open = BoolAttribute "open"


-- | A short form of a header cell's content.
abbr :: String -> Attribute msg
abbr = Attribute "abbr"
