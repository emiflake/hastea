{- | Build HTML trees.

Every element of the HTML standard has a function named after its tag.
Ordinary elements take a list of attributes and a list of children:

@
import Hastea.Html (Html)
import Hastea.Html qualified as H
import Hastea.Html.Attributes qualified as A

view :: Html msg
view =
  H.div
    [A.className "greeting"]
    [ H.h1 [] [H.text "Hello!"]
    , H.img [A.src "wave.png", A.alt "A waving hand"]
    ]
@

Void elements such as 'img', 'input' and 'br' cannot have children, so they
only take attributes.

Attributes live in "Hastea.Html.Attributes" and event handlers in
"Hastea.Html.Events".

This module is meant to be imported qualified, as above. There is an
element for every tag, so unqualified, names such as 'a', 'i', 'title' and
'time' would not be free for your own variables, and 'div', 'span' and
'head' would clash with the "Prelude". Three names have a trailing
underscore because they are keywords or too common to give up even so:
'main_', 'map_' and 'data_'.

Use 'fmap' to change the type of message a tree produces.
-}
module Hastea.Html
  ( Html
  , Attribute

    -- * Primitives
  , text
  , node
  , fragment

    -- * Performance
  , keyed
  , lazy
  , lazy2
  , lazy3

    -- * Document
  , html
  , head
  , title
  , base
  , link
  , meta
  , style
  , body

    -- * Sections
  , article
  , section
  , nav
  , aside
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , hgroup
  , header
  , footer
  , address
  , main_
  , search

    -- * Grouping content
  , p
  , hr
  , pre
  , blockquote
  , ol
  , ul
  , menu
  , li
  , dl
  , dt
  , dd
  , figure
  , figcaption
  , div

    -- * Text
  , a
  , em
  , strong
  , small
  , s
  , cite
  , q
  , dfn
  , abbr
  , ruby
  , rt
  , rp
  , data_
  , time
  , code
  , var
  , samp
  , kbd
  , sub
  , sup
  , i
  , b
  , u
  , mark
  , bdi
  , bdo
  , span
  , br
  , wbr

    -- * Edits
  , ins
  , del

    -- * Embedded content
  , picture
  , source
  , img
  , iframe
  , embed
  , object
  , video
  , audio
  , track
  , map_
  , area
  , canvas

    -- * Tables
  , table
  , caption
  , colgroup
  , col
  , thead
  , tbody
  , tfoot
  , tr
  , th
  , td

    -- * Forms
  , form
  , label
  , input
  , button
  , select
  , datalist
  , optgroup
  , option
  , textarea
  , output
  , progress
  , meter
  , fieldset
  , legend

    -- * Interactive elements
  , details
  , summary
  , dialog

    -- * Scripting
  , script
  , noscript
  , template
  , slot
  )
where

import Prelude hiding (div, head, span)

import Hastea.Html.Internal (Attribute, Html (..), Ref (..))


-- | A text node. The text is escaped when rendered.
text :: String -> Html msg
text = Text


{- | An element with a custom tag name, for anything this module does not
cover, such as custom elements.
-}
node :: String -> [Attribute msg] -> [Html msg] -> Html msg
node = Element


{- | Several nodes where one is expected, without an element around them.

@
view = fragment [h1 [] [text "Title"], p [] [text "Body"]]
@

The nodes become children of whatever element the fragment is in, next to
its other children. Use it for a view that has no single outer element.

Do not put a fragment directly inside 'lazy': only its first node would be
shown. Put 'lazy' around each node, or an element around the fragment.
-}
fragment :: [Html msg] -> Html msg
fragment = Fragment


{- | Give a node an identity among its siblings.

When a list of children changes, a keyed child is matched with the previous
child of the same key, wherever that was. Its DOM node is kept and moved, so
it retains its state (focus, scroll position, a playing video) and is not
rebuilt. Without keys, children are matched by position. Keys must be unique
among siblings.

@
ul [] [keyed (show (todoId todo)) (viewTodo todo) | todo <- todos]
@
-}
keyed :: String -> Html msg -> Html msg
keyed = Keyed


{- | Put off building a subtree until it is needed, and skip it altogether
when nothing it depends on has changed.

@lazy view model@ is the same tree as @view model@. But when the page is
rendered again and both @view@ and @model@ are the very same values as last
time, the subtree is known to be unchanged, because @view@ is pure: it is
not computed and not compared with the page.

"The very same value" means the same object in memory, not an equal one.
So pass a top-level function rather than a lambda or a partial application
(use 'lazy2' and 'lazy3' for more arguments), and pass parts of your model as
they are rather than values rebuilt on each render. Getting this wrong is
harmless: the subtree is then simply rendered as usual.

The arguments are evaluated to weak head normal form when compared. To give
a lazy node a key, apply 'keyed' on the outside.
-}
lazy :: (a -> Html msg) -> a -> Html msg
lazy view x = Lazy [Ref view, Ref x] (view x)


-- | 'lazy' for a function of two arguments.
lazy2 :: (a -> b -> Html msg) -> a -> b -> Html msg
lazy2 view x y = Lazy [Ref view, Ref x, Ref y] (view x y)


-- | 'lazy' for a function of three arguments.
lazy3 :: (a -> b -> c -> Html msg) -> a -> b -> c -> Html msg
lazy3 view x y z = Lazy [Ref view, Ref x, Ref y, Ref z] (view x y z)


-- An element that cannot have children.
void :: String -> [Attribute msg] -> Html msg
void tag attributes = Element tag attributes []



-- DOCUMENT


-- | The root element of a document.
html :: [Attribute msg] -> [Html msg] -> Html msg
html = node "html"


-- | Container for document metadata.
head :: [Attribute msg] -> [Html msg] -> Html msg
head = node "head"


-- | The document's title.
title :: [Attribute msg] -> [Html msg] -> Html msg
title = node "title"


-- | The base URL for relative links in the document. A void element: it has no children.
base :: [Attribute msg] -> Html msg
base = void "base"


-- | A link to an external resource such as a stylesheet. A void element: it has no children.
link :: [Attribute msg] -> Html msg
link = void "link"


-- | Document metadata that no other element can express. A void element: it has no children.
meta :: [Attribute msg] -> Html msg
meta = void "meta"


-- | Embedded CSS. Its text is not escaped when rendered.
style :: [Attribute msg] -> [Html msg] -> Html msg
style = node "style"


-- | The content of a document.
body :: [Attribute msg] -> [Html msg] -> Html msg
body = node "body"


-- SECTIONS


-- | A self-contained piece of content.
article :: [Attribute msg] -> [Html msg] -> Html msg
article = node "article"


-- | A thematic grouping of content.
section :: [Attribute msg] -> [Html msg] -> Html msg
section = node "section"


-- | A section with navigation links.
nav :: [Attribute msg] -> [Html msg] -> Html msg
nav = node "nav"


-- | Content only loosely related to what surrounds it.
aside :: [Attribute msg] -> [Html msg] -> Html msg
aside = node "aside"


-- | A top-level heading.
h1 :: [Attribute msg] -> [Html msg] -> Html msg
h1 = node "h1"


-- | A second-level heading.
h2 :: [Attribute msg] -> [Html msg] -> Html msg
h2 = node "h2"


-- | A third-level heading.
h3 :: [Attribute msg] -> [Html msg] -> Html msg
h3 = node "h3"


-- | A fourth-level heading.
h4 :: [Attribute msg] -> [Html msg] -> Html msg
h4 = node "h4"


-- | A fifth-level heading.
h5 :: [Attribute msg] -> [Html msg] -> Html msg
h5 = node "h5"


-- | A sixth-level heading.
h6 :: [Attribute msg] -> [Html msg] -> Html msg
h6 = node "h6"


-- | A heading together with related content such as a subtitle.
hgroup :: [Attribute msg] -> [Html msg] -> Html msg
hgroup = node "hgroup"


-- | Introductory content for a page or section.
header :: [Attribute msg] -> [Html msg] -> Html msg
header = node "header"


-- | A footer for a page or section.
footer :: [Attribute msg] -> [Html msg] -> Html msg
footer = node "footer"


-- | Contact information.
address :: [Attribute msg] -> [Html msg] -> Html msg
address = node "address"


-- | The main content of the document. Named @main_@ so that it does not clash with a program's @main@.
main_ :: [Attribute msg] -> [Html msg] -> Html msg
main_ = node "main"


-- | A section used for searching or filtering.
search :: [Attribute msg] -> [Html msg] -> Html msg
search = node "search"


-- GROUPING CONTENT


-- | A paragraph.
p :: [Attribute msg] -> [Html msg] -> Html msg
p = node "p"


-- | A thematic break. A void element: it has no children.
hr :: [Attribute msg] -> Html msg
hr = void "hr"


-- | Preformatted text.
pre :: [Attribute msg] -> [Html msg] -> Html msg
pre = node "pre"


-- | An extended quotation.
blockquote :: [Attribute msg] -> [Html msg] -> Html msg
blockquote = node "blockquote"


-- | An ordered list.
ol :: [Attribute msg] -> [Html msg] -> Html msg
ol = node "ol"


-- | An unordered list.
ul :: [Attribute msg] -> [Html msg] -> Html msg
ul = node "ul"


-- | A list of commands.
menu :: [Attribute msg] -> [Html msg] -> Html msg
menu = node "menu"


-- | A list item.
li :: [Attribute msg] -> [Html msg] -> Html msg
li = node "li"


-- | A description list.
dl :: [Attribute msg] -> [Html msg] -> Html msg
dl = node "dl"


-- | A term in a description list.
dt :: [Attribute msg] -> [Html msg] -> Html msg
dt = node "dt"


-- | A description in a description list.
dd :: [Attribute msg] -> [Html msg] -> Html msg
dd = node "dd"


-- | Self-contained content such as an illustration, optionally with a caption.
figure :: [Attribute msg] -> [Html msg] -> Html msg
figure = node "figure"


-- | The caption of a 'figure'.
figcaption :: [Attribute msg] -> [Html msg] -> Html msg
figcaption = node "figcaption"


-- | A generic block container.
div :: [Attribute msg] -> [Html msg] -> Html msg
div = node "div"


-- TEXT


-- | A hyperlink.
a :: [Attribute msg] -> [Html msg] -> Html msg
a = node "a"


-- | Stress emphasis.
em :: [Attribute msg] -> [Html msg] -> Html msg
em = node "em"


-- | Strong importance.
strong :: [Attribute msg] -> [Html msg] -> Html msg
strong = node "strong"


-- | Side comments and small print.
small :: [Attribute msg] -> [Html msg] -> Html msg
small = node "small"


-- | Text that is no longer accurate or relevant.
s :: [Attribute msg] -> [Html msg] -> Html msg
s = node "s"


-- | The title of a creative work.
cite :: [Attribute msg] -> [Html msg] -> Html msg
cite = node "cite"


-- | An inline quotation.
q :: [Attribute msg] -> [Html msg] -> Html msg
q = node "q"


-- | The defining instance of a term.
dfn :: [Attribute msg] -> [Html msg] -> Html msg
dfn = node "dfn"


-- | An abbreviation or acronym.
abbr :: [Attribute msg] -> [Html msg] -> Html msg
abbr = node "abbr"


-- | A ruby annotation.
ruby :: [Attribute msg] -> [Html msg] -> Html msg
ruby = node "ruby"


-- | The text of a ruby annotation.
rt :: [Attribute msg] -> [Html msg] -> Html msg
rt = node "rt"


-- | Fallback parentheses around a ruby annotation.
rp :: [Attribute msg] -> [Html msg] -> Html msg
rp = node "rp"


-- | Content paired with a machine-readable value. Named @data_@ because @data@ is a keyword.
data_ :: [Attribute msg] -> [Html msg] -> Html msg
data_ = node "data"


-- | A date or time.
time :: [Attribute msg] -> [Html msg] -> Html msg
time = node "time"


-- | A fragment of computer code.
code :: [Attribute msg] -> [Html msg] -> Html msg
code = node "code"


-- | A variable.
var :: [Attribute msg] -> [Html msg] -> Html msg
var = node "var"


-- | Sample output from a program.
samp :: [Attribute msg] -> [Html msg] -> Html msg
samp = node "samp"


-- | User input, typically from a keyboard.
kbd :: [Attribute msg] -> [Html msg] -> Html msg
kbd = node "kbd"


-- | Subscript.
sub :: [Attribute msg] -> [Html msg] -> Html msg
sub = node "sub"


-- | Superscript.
sup :: [Attribute msg] -> [Html msg] -> Html msg
sup = node "sup"


-- | Text in an alternate voice or mood.
i :: [Attribute msg] -> [Html msg] -> Html msg
i = node "i"


-- | Text to draw attention to.
b :: [Attribute msg] -> [Html msg] -> Html msg
b = node "b"


-- | An unarticulated annotation, such as a misspelling.
u :: [Attribute msg] -> [Html msg] -> Html msg
u = node "u"


-- | Highlighted text.
mark :: [Attribute msg] -> [Html msg] -> Html msg
mark = node "mark"


-- | Text isolated from its surroundings for bidirectional formatting.
bdi :: [Attribute msg] -> [Html msg] -> Html msg
bdi = node "bdi"


-- | Text with an explicit direction.
bdo :: [Attribute msg] -> [Html msg] -> Html msg
bdo = node "bdo"


-- | A generic inline container.
span :: [Attribute msg] -> [Html msg] -> Html msg
span = node "span"


-- | A line break. A void element: it has no children.
br :: [Attribute msg] -> Html msg
br = void "br"


-- | A line break opportunity. A void element: it has no children.
wbr :: [Attribute msg] -> Html msg
wbr = void "wbr"


-- EDITS


-- | Content added to the document.
ins :: [Attribute msg] -> [Html msg] -> Html msg
ins = node "ins"


-- | Content removed from the document.
del :: [Attribute msg] -> [Html msg] -> Html msg
del = node "del"


-- EMBEDDED CONTENT


-- | A container offering several sources for an image.
picture :: [Attribute msg] -> [Html msg] -> Html msg
picture = node "picture"


-- | One of several media resources for 'picture', 'audio' or 'video'. A void element: it has no children.
source :: [Attribute msg] -> Html msg
source = void "source"


-- | An image. A void element: it has no children.
img :: [Attribute msg] -> Html msg
img = void "img"


-- | A nested browsing context.
iframe :: [Attribute msg] -> [Html msg] -> Html msg
iframe = node "iframe"


-- | An integration point for external content. A void element: it has no children.
embed :: [Attribute msg] -> Html msg
embed = void "embed"


-- | An external resource.
object :: [Attribute msg] -> [Html msg] -> Html msg
object = node "object"


-- | A video player.
video :: [Attribute msg] -> [Html msg] -> Html msg
video = node "video"


-- | An audio player.
audio :: [Attribute msg] -> [Html msg] -> Html msg
audio = node "audio"


-- | A timed text track for 'audio' or 'video'. A void element: it has no children.
track :: [Attribute msg] -> Html msg
track = void "track"


-- | An image map. Named @map_@ so that it does not clash with 'Prelude.map'.
map_ :: [Attribute msg] -> [Html msg] -> Html msg
map_ = node "map"


-- | A clickable area in an image map. A void element: it has no children.
area :: [Attribute msg] -> Html msg
area = void "area"


-- | A bitmap surface for scripted drawing.
canvas :: [Attribute msg] -> [Html msg] -> Html msg
canvas = node "canvas"


-- TABLES


-- | A table.
table :: [Attribute msg] -> [Html msg] -> Html msg
table = node "table"


-- | The title of a table.
caption :: [Attribute msg] -> [Html msg] -> Html msg
caption = node "caption"


-- | A group of table columns.
colgroup :: [Attribute msg] -> [Html msg] -> Html msg
colgroup = node "colgroup"


-- | A table column. A void element: it has no children.
col :: [Attribute msg] -> Html msg
col = void "col"


-- | The header rows of a table.
thead :: [Attribute msg] -> [Html msg] -> Html msg
thead = node "thead"


-- | The body rows of a table.
tbody :: [Attribute msg] -> [Html msg] -> Html msg
tbody = node "tbody"


-- | The footer rows of a table.
tfoot :: [Attribute msg] -> [Html msg] -> Html msg
tfoot = node "tfoot"


-- | A table row.
tr :: [Attribute msg] -> [Html msg] -> Html msg
tr = node "tr"


-- | A table header cell.
th :: [Attribute msg] -> [Html msg] -> Html msg
th = node "th"


-- | A table data cell.
td :: [Attribute msg] -> [Html msg] -> Html msg
td = node "td"


-- FORMS


-- | A form.
form :: [Attribute msg] -> [Html msg] -> Html msg
form = node "form"


-- | The caption of a form control.
label :: [Attribute msg] -> [Html msg] -> Html msg
label = node "label"


-- | A form control for entering data. A void element: it has no children.
input :: [Attribute msg] -> Html msg
input = void "input"


-- | A button.
button :: [Attribute msg] -> [Html msg] -> Html msg
button = node "button"


-- | A control for choosing among options.
select :: [Attribute msg] -> [Html msg] -> Html msg
select = node "select"


-- | Predefined options for another control.
datalist :: [Attribute msg] -> [Html msg] -> Html msg
datalist = node "datalist"


-- | A group of options.
optgroup :: [Attribute msg] -> [Html msg] -> Html msg
optgroup = node "optgroup"


-- | An option in a 'select' or 'datalist'.
option :: [Attribute msg] -> [Html msg] -> Html msg
option = node "option"


-- | A multi-line text field.
textarea :: [Attribute msg] -> [Html msg] -> Html msg
textarea = node "textarea"


-- | The result of a calculation or user action.
output :: [Attribute msg] -> [Html msg] -> Html msg
output = node "output"


-- | The progress of a task.
progress :: [Attribute msg] -> [Html msg] -> Html msg
progress = node "progress"


-- | A measurement within a known range.
meter :: [Attribute msg] -> [Html msg] -> Html msg
meter = node "meter"


-- | A group of form controls.
fieldset :: [Attribute msg] -> [Html msg] -> Html msg
fieldset = node "fieldset"


-- | The caption of a 'fieldset'.
legend :: [Attribute msg] -> [Html msg] -> Html msg
legend = node "legend"


-- INTERACTIVE ELEMENTS


-- | A disclosure widget.
details :: [Attribute msg] -> [Html msg] -> Html msg
details = node "details"


-- | The summary of a 'details' element.
summary :: [Attribute msg] -> [Html msg] -> Html msg
summary = node "summary"


-- | A dialog box.
dialog :: [Attribute msg] -> [Html msg] -> Html msg
dialog = node "dialog"


-- SCRIPTING


-- | An embedded or external script. Its text is not escaped when rendered.
script :: [Attribute msg] -> [Html msg] -> Html msg
script = node "script"


-- | Fallback content for when scripting is unavailable.
noscript :: [Attribute msg] -> [Html msg] -> Html msg
noscript = node "noscript"


-- | Content that is not rendered but can be instantiated by script.
template :: [Attribute msg] -> [Html msg] -> Html msg
template = node "template"


-- | A placeholder in a web component.
slot :: [Attribute msg] -> [Html msg] -> Html msg
slot = node "slot"
