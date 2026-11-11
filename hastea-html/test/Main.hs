module Main (main) where

import Prelude hiding (div, head, span)

import Control.Applicative ((<|>))
import Control.Exception (SomeException, evaluate, try)
import Data.List (nub, sortOn, (\\))

import Hastea.Html
import Hastea.Html.Attributes qualified as A
import Hastea.Html.Decode (Decoder, Value (..), decodeValue)
import Hastea.Html.Decode qualified as D
import Hastea.Html.Diff
import Hastea.Html.Events
import Hastea.Html.Internal qualified as I
import Hastea.Html.Render

import Support
import Tables qualified


main :: IO ()
main =
  runSuites
    [ elementTests
    , attributeTests
    , renderTests
    , eventTests
    , functorTests
    , decodeTests
    , diffTests
    ]



-- ELEMENTS

elementTests :: Suite -> IO ()
elementTests suite = do
  mapM_
    ( \(tag, el) ->
        equal suite ("element " <> tag) ("<" <> tag <> "></" <> tag <> ">") (renderHtml (el [] []))
    )
    Tables.elements
  mapM_
    (\(tag, el) -> equal suite ("void element " <> tag) ("<" <> tag <> ">") (renderHtml (el [])))
    Tables.voidElements

  let tags = map fst Tables.elements <> map fst Tables.voidElements
  equal suite "no tag is defined twice" [] (tags \\ nub tags)
  equal suite "every element of the HTML standard is covered" [] (standardElements \\ tags)
  equal suite "no element outside the HTML standard" [] (tags \\ standardElements)

  equal suite "elements keep attributes and children" "<p id=\"x\"><b>hi</b> there</p>" $
    renderHtml (p [A.id_ "x"] [b [] [text "hi"], text " there"])
  equal suite "void elements keep attributes" "<img src=\"a.png\" alt=\"A\">" $
    renderHtml (img [A.src "a.png", A.alt "A"])
  equal suite "custom elements" "<my-widget mode=\"x\">hi</my-widget>" $
    renderHtml (node "my-widget" [A.attrib "mode" "x"] [text "hi"])


{- | The elements of the HTML Living Standard, written out independently of
the library, minus obsolete ones and the SVG and MathML roots.
-}
standardElements :: [String]
standardElements =
  words
    "html head title base link meta style body article section nav aside h1 h2 h3 h4 h5 h6 \
    \hgroup header footer address p hr pre blockquote ol ul menu li dl dt dd figure figcaption \
    \main search div a em strong small s cite q dfn abbr ruby rt rp data time code var samp kbd \
    \sub sup i b u mark bdi bdo span br wbr ins del picture source img iframe embed object video \
    \audio track map area table caption colgroup col tbody thead tfoot tr td th form label input \
    \button select datalist optgroup option textarea output progress meter fieldset legend \
    \details summary dialog script noscript template slot canvas"


-- ATTRIBUTES

attributeTests :: Suite -> IO ()
attributeTests suite = do
  mapM_
    ( \(expected, attribute) ->
        equal
          suite
          ("attribute " <> describe attribute)
          ("<div" <> (if null expected then "" else " " <> expected) <> "></div>")
          (renderHtml (div [attribute] []))
    )
    Tables.attributes

  equal suite "attrib" "attribute x-foo=\"bar\"" (describe (A.attrib "x-foo" "bar"))
  equal suite "boolAttrib" "boolean attribute x-foo=True" (describe (A.boolAttrib "x-foo" True))
  equal suite "dataAttr" "attribute data-id=\"5\"" (describe (A.dataAttr "id" "5"))
  equal suite "aria" "attribute aria-label=\"Close\"" (describe (A.aria "label" "Close"))

  equal suite "classList keeps enabled classes" "attribute class=\"a c\"" $
    describe (A.classList [("a", True), ("b", False), ("c", True)])
  equal suite "classList with nothing enabled" "attribute class=\"\"" $
    describe (A.classList [("a", False)])
  equal suite "styles" "attribute style=\"display: block; color: red\"" $
    describe (A.styles [("display", "block"), ("color", "red")])
  equal suite "styles with no declarations" "attribute style=\"\"" (describe (A.styles []))

  -- Form state must be set as DOM properties: the attributes of the same
  -- name only provide the initial value.
  equal suite "value is a property" "property value=PropertyString \"hi\"" (describe (A.value "hi"))
  equal suite "checked is a property" "property checked=PropertyBool True" (describe (A.checked True))
  equal suite "selected is a property" "property selected=PropertyBool False" $
    describe (A.selected False)
  equal suite "disabled is a boolean attribute" "boolean attribute disabled=True" $
    describe (A.disabled True)

  equal suite "properties render like attributes" "<input value=\"hi\" checked>" $
    renderHtml (input [A.value "hi", A.checked True, A.selected False])
  equal suite "negative and zero integers" "<div tabindex=\"-1\" width=\"0\"></div>" $
    renderHtml (div [A.tabIndex (-1), A.width 0] [])


-- RENDERING

renderTests :: Suite -> IO ()
renderTests suite = do
  equal suite "text is escaped" "<p>1 &lt; 2 &amp;&amp; 3 &gt; 2 \"ok\"</p>" $
    renderHtml (p [] [text "1 < 2 && 3 > 2 \"ok\""])
  equal suite "attribute values are escaped" "<a title=\"&quot;a&quot; &amp; &lt;b&gt;\"></a>" $
    renderHtml (a [A.title "\"a\" & <b>"] [])
  equal suite "text cannot inject markup" "<div>&lt;script&gt;alert(1)&lt;/script&gt;</div>" $
    renderHtml (div [] [text "<script>alert(1)</script>"])
  equal suite "attributes cannot break out of quotes" "<div class=\"&quot; onclick=&quot;x\"></div>" $
    renderHtml (div [A.className "\" onclick=\"x"] [])

  equal suite "script content is raw" "<script>if (a < b && c) {}</script>" $
    renderHtml (script [] [text "if (a < b && c) {}"])
  equal suite "style content is raw" "<style>a > b { color: red }</style>" $
    renderHtml (style [] [text "a > b { color: red }"])

  equal suite "void elements drop children" "<br>" $
    renderHtml (node "br" [] [text "ignored"])
  equal suite "empty elements get a closing tag" "<textarea></textarea>" $
    renderHtml (textarea [] [])
  equal suite "a lone text node" "hi" (renderHtml (text "hi" :: Html ()))

  equal suite "event listeners are not rendered" "<button class=\"b\">ok</button>" $
    renderHtml (button [onClick (), A.className "b"] [text "ok"])
  equal suite "false boolean attributes are omitted" "<input>" $
    renderHtml (input [A.disabled False, A.checked False])

  equal suite "the last duplicate attribute wins" "<div id=\"x\" class=\"b\"></div>" $
    renderHtml (div [A.className "a", A.id_ "x", A.className "b"] [])
  equal suite "a later false turns a boolean attribute off" "<input>" $
    renderHtml (input [A.disabled True, A.disabled False])
  equal suite "attributes keep their order" "<a href=\"/\" target=\"_blank\" rel=\"noopener\"></a>" $
    renderHtml (a [A.href "/", A.target "_blank", A.rel "noopener"] [])

  equal
    suite
    "a whole document"
    "<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"utf-8\"><title>Hi</title></head>\
    \<body><main><h1>Hi</h1><ul><li>a</li><li>b</li></ul></main></body></html>"
    $ renderDocument
    $ html
      [A.lang "en"]
      [ head [] [meta [A.charset "utf-8"], title [] [text "Hi"]]
      , body [] [main_ [] [h1 [] [text "Hi"], ul [] [li [] [text "a"], li [] [text "b"]]]]
      ]

  equal suite "escapeText" "&amp;&lt;&gt;\"'" (escapeText "&<>\"'")
  equal suite "escapeAttribute" "&amp;&lt;&gt;&quot;'" (escapeAttribute "&<>\"'")


-- EVENTS


eventTests :: Suite -> IO ()
eventTests suite = do
  let names el = [(e, o.preventDefault, o.stopPropagation) | (e, o, _) <- listeners el]
      plain event = [(event, False, False)]
      preventing event = [(event, True, False)]

  -- Handlers that carry no data: each must listen for the right DOM event
  -- and produce its message whatever the event object looks like.
  mapM_
    ( \(event, handler) -> do
        equal suite ("event name of " <> event) (plain event) (names (div [handler Clicked] []))
        equal suite ("message of " <> event) [Right Clicked] (fire event Null (div [handler Clicked] []))
    )
    [ ("click", onClick)
    , ("dblclick", onDoubleClick)
    , ("contextmenu", onContextMenu)
    , ("mousedown", onMouseDown)
    , ("mouseup", onMouseUp)
    , ("mouseenter", onMouseEnter)
    , ("mouseleave", onMouseLeave)
    , ("mouseover", onMouseOver)
    , ("mouseout", onMouseOut)
    , ("mousemove", onMouseMove)
    , ("wheel", onWheel)
    , ("reset", onReset)
    , ("focus", onFocus)
    , ("blur", onBlur)
    , ("select", onSelect)
    , ("invalid", onInvalid)
    , ("pointerdown", onPointerDown)
    , ("pointerup", onPointerUp)
    , ("pointermove", onPointerMove)
    , ("pointerenter", onPointerEnter)
    , ("pointerleave", onPointerLeave)
    , ("pointercancel", onPointerCancel)
    , ("touchstart", onTouchStart)
    , ("touchmove", onTouchMove)
    , ("touchend", onTouchEnd)
    , ("touchcancel", onTouchCancel)
    , ("dragstart", onDragStart)
    , ("drag", onDrag)
    , ("dragend", onDragEnd)
    , ("dragenter", onDragEnter)
    , ("dragleave", onDragLeave)
    , ("copy", onCopy)
    , ("cut", onCut)
    , ("paste", onPaste)
    , ("scroll", onScroll)
    , ("load", onLoad)
    , ("error", onError)
    , ("toggle", onToggle)
    ]

  -- Handlers that must prevent the browser's default action.
  mapM_
    ( \(event, handler) -> do
        equal suite (event <> " prevents default") (preventing event) (names (div [handler Clicked] []))
        equal suite ("message of " <> event) [Right Clicked] (fire event Null (div [handler Clicked] []))
    )
    [("submit", onSubmit), ("dragover", onDragOver), ("drop", onDrop)]

  let inputEvent v = obj [("target", obj [("value", StringValue v)])]
      checkEvent c = obj [("target", obj [("checked", BoolValue c)])]
      keyEvent k = obj [("key", StringValue k)]

  equal suite "onInput reads target.value" [Right (Typed "hello")] $
    fire "input" (inputEvent "hello") (input [onInput Typed])
  equal suite "onChange reads target.value" [Right (Typed "x")] $
    fire "change" (inputEvent "x") (select [onChange Typed] [])
  equal suite "onCheck reads target.checked" [Right (Checked True)] $
    fire "change" (checkEvent True) (input [onCheck Checked])
  equal suite "onKeyDown reads key" [Right (Key "Enter")] $
    fire "keydown" (keyEvent "Enter") (input [onKeyDown Key])
  equal suite "onKeyUp reads key" [Right (Key "a")] $
    fire "keyup" (keyEvent "a") (input [onKeyUp Key])
  equal suite "onKeypress reads key" [Right (Key "b")] $
    fire "keypress" (keyEvent "b") (input [onKeypress Key])

  equal suite "onInput fails without a target" [Left "expected an object with a field named \"target\""] $
    fire "input" (obj []) (input [onInput Typed])
  equal suite "onInput fails on a non-string value" [Left ".target.value: expected a string"] $
    fire "input" (obj [("target", obj [("value", NumberValue 1)])]) (input [onInput Typed])

  equal suite "listeners only fire for their event" [] $
    fire "keydown" (keyEvent "a") (input [onInput Typed, onClick Clicked])
  equal suite "several listeners for one event all fire" [Right Clicked, Right (Key "a")] $
    fire "x" (keyEvent "a") (div [on "x" (pure Clicked), on "x" (Key <$> key)] [])

  equal suite "on uses default options" [("custom", False, False)] $
    names (div [on "custom" (pure Clicked)] [])
  equal suite "onWith keeps its options" [("custom", True, True)] $
    names (div [onWith (EventOptions True True) "custom" (pure Clicked)] [])
  equal suite "defaultEventOptions" (EventOptions False False) defaultEventOptions


-- FUNCTOR

functorTests :: Suite -> IO ()
functorTests suite = do
  let inner = button [A.className "b", onClick Clicked] [span [onClick Clicked] [text "x"]]
      mapped = fmap Wrapped inner
      child el = case el of
        I.Element _ _ (c : _) -> c
        _ -> el

  equal suite "fmap maps messages" [Right (Wrapped Clicked)] (fire "click" Null mapped)
  equal suite "fmap maps messages of descendants" [Right (Wrapped Clicked)] $
    fire "click" Null (child mapped)
  equal suite "fmap keeps the structure" (renderHtml inner) (renderHtml mapped)
  equal suite "fmap on an attribute" [Right (Wrapped Clicked)] $
    fire "click" Null (div [fmap Wrapped (onClick Clicked)] [])
  equal suite "fmap leaves plain attributes alone" (describe (A.id_ "x" :: Attribute Msg)) $
    describe (fmap Wrapped (A.id_ "x"))


-- DECODERS

decodeTests :: Suite -> IO ()
decodeTests suite = do
  let run :: Decoder a -> Value -> Either String a
      run = decodeValue

  equal suite "string" (Right "a") (run D.string (StringValue "a"))
  equal suite "string rejects numbers" (Left "expected a string") (run D.string (NumberValue 1))
  equal suite "number" (Right 1.5) (run D.number (NumberValue 1.5))
  equal suite "number rejects strings" (Left "expected a number") (run D.number (StringValue "1"))
  equal suite "bool" (Right True) (run D.bool (BoolValue True))
  equal suite "bool rejects null" (Left "expected a boolean") (run D.bool Null)

  equal suite "int" (Right 3) (run D.int (NumberValue 3))
  equal suite "int accepts negatives" (Right (-3)) (run D.int (NumberValue (-3)))
  equal suite "int rejects fractions" (Left "expected an integer") (run D.int (NumberValue 3.5))
  equal suite "int rejects NaN" (Left "expected an integer") (run D.int (NumberValue (0 / 0)))
  equal suite "int rejects infinity" (Left "expected an integer") (run D.int (NumberValue (1 / 0)))

  equal suite "nullable on null" (Right Nothing) (run (D.nullable D.string) Null)
  equal suite "nullable on a value" (Right (Just "a")) (run (D.nullable D.string) (StringValue "a"))
  equal suite "nullable still checks the type" (Left "expected a string") $
    run (D.nullable D.string) (NumberValue 1)

  equal suite "list" (Right [1, 2]) (run (D.list D.number) (ArrayValue [NumberValue 1, NumberValue 2]))
  equal suite "empty list" (Right []) (run (D.list D.number) (ArrayValue []))
  equal suite "list rejects objects" (Left "expected an array") (run (D.list D.number) (obj []))
  equal suite "list reports the failing index" (Left "[1]: expected a number") $
    run (D.list D.number) (ArrayValue [NumberValue 1, StringValue "x"])

  let nested = obj [("a", obj [("b", ArrayValue [obj [("c", NumberValue 7)]])])]
  equal suite "field" (Right 1) (run (D.field "x" D.number) (obj [("x", NumberValue 1)]))
  equal suite "missing field" (Left "expected an object with a field named \"x\"") $
    run (D.field "x" D.number) (obj [("y", NumberValue 1)])
  equal suite "field of a non-object" (Left "expected an object with a field named \"x\"") $
    run (D.field "x" D.number) (StringValue "x")
  equal suite "at" (Right [7]) (run (D.at ["a", "b"] (D.list (D.field "c" D.number))) nested)
  equal suite "at with an empty path" (Right "a") (run (D.at [] D.string) (StringValue "a"))
  equal suite "errors carry the full path" (Left ".a.b[0].c: expected a string") $
    run (D.at ["a", "b"] (D.list (D.field "c" D.string))) nested
  equal suite "missing nested field" (Left ".a: expected an object with a field named \"z\"") $
    run (D.at ["a", "z"] D.string) nested

  equal suite "pure ignores the value" (Right 'x') (run (pure 'x') Null)
  equal suite "fmap" (Right 2) (run ((+ 1) <$> D.number) (NumberValue 1))
  equal suite "applicative reads several fields" (Right ("a", 1)) $
    run
      ((,) <$> D.field "s" D.string <*> D.field "n" D.number)
      (obj [("s", StringValue "a"), ("n", NumberValue 1)])
  equal suite "applicative fails if any part fails" (Left ".n: expected a number") $
    run
      ((,) <$> D.field "s" D.string <*> D.field "n" D.number)
      (obj [("s", StringValue "a"), ("n", Null)])

  let stringOrNumber = (Left <$> D.string) <|> (Right <$> D.number)
  equal suite "<|> takes the first success" (Right (Left "a")) (run stringOrNumber (StringValue "a"))
  equal suite "<|> falls back" (Right (Right 1)) (run stringOrNumber (NumberValue 1))
  equal suite "<|> reports the last failure" (Left "expected a number") (run stringOrNumber Null)
  equal suite "oneOf" (Right "b") $
    run (D.oneOf [D.field "a" D.string, D.field "b" D.string]) (obj [("b", StringValue "b")])
  equal suite "oneOf of nothing fails" (Left "no decoder matched") (run (D.oneOf [] :: Decoder ()) Null)

  let shape =
        D.field "type" D.string `D.andThen` \t -> case t of
          "circle" -> D.field "r" D.number
          "square" -> D.field "side" D.number
          _ -> D.failWith ("unknown shape " <> t)
  equal suite "andThen picks a decoder" (Right 2) $
    run shape (obj [("type", StringValue "square"), ("side", NumberValue 2)])
  equal suite "andThen can fail" (Left "unknown shape blob") $
    run shape (obj [("type", StringValue "blob")])
  equal suite "failWith" (Left "nope") (run (D.failWith "nope" :: Decoder ()) Null)


-- DIFFING
--
-- 'diff' is checked against a small model of the DOM: applying the patch to
-- the page built from the old tree must give the page built from the new one.

-- | A DOM node: its attributes and properties are kept sorted by name.
data Dom
  = DomText String
  | DomElement String [(String, String)] [(String, I.PropertyValue)] [Dom]
  deriving (Eq, Show)


-- | What the browser builds for a tree.
build :: Html msg -> Dom
build tree = case I.unwrap tree of
  I.Element tag attributes children ->
    foldl (flip setOn) (DomElement tag [] [] (map build (I.flatten children))) attributes
  I.Text content -> DomText content
  _ -> DomText ""


setOn :: Attribute msg -> Dom -> Dom
setOn attribute dom = case (attribute, dom) of
  (I.Attribute k v, DomElement t as ps cs) -> DomElement t (put k v as) ps cs
  (I.BoolAttribute k True, DomElement t as ps cs) -> DomElement t (put k "" as) ps cs
  (I.BoolAttribute k False, DomElement t as ps cs) -> DomElement t (without k as) ps cs
  (I.Property k v, DomElement t as ps cs) -> DomElement t as (putProperty k v ps) cs
  _ -> dom


clearOn :: Attribute msg -> Dom -> Dom
clearOn attribute dom = case (attribute, dom) of
  (I.Attribute k _, DomElement t as ps cs) -> DomElement t (without k as) ps cs
  (I.BoolAttribute k _, DomElement t as ps cs) -> DomElement t (without k as) ps cs
  (I.Property k _, DomElement t as ps cs) -> DomElement t as (without k ps) cs
  _ -> dom


put :: Ord k => k -> v -> [(k, v)] -> [(k, v)]
put k v kvs = sortOn fst ((k, v) : without k kvs)


without :: Eq k => k -> [(k, v)] -> [(k, v)]
without k = filter ((/= k) . fst)


-- A property that is empty or false is the same as one that was never set.
putProperty :: String -> I.PropertyValue -> [(String, I.PropertyValue)] -> [(String, I.PropertyValue)]
putProperty k v ps
  | v `elem` [I.PropertyString "", I.PropertyBool False] = without k ps
  | otherwise = put k v ps


-- | Carry out a patch on the model, the way "Hastea.Internal.Commit" does on the page.
applyPatch :: Patch msg -> Dom -> Dom
applyPatch patch dom = case (patch, dom) of
  (Keep, _) -> dom
  (Replace tree, _) -> build tree
  (SetText content, DomText _) -> DomText content
  (Update changes children, DomElement{}) ->
    case foldl (flip change) dom changes of
      DomElement t as ps cs -> DomElement t as ps (applyChildren children cs)
      other -> other
  _ -> error ("patch does not fit the node: " <> show patch)
 where
  change (Set attribute) = setOn attribute
  change (Clear attribute) = clearOn attribute
  change (Listen _) = id

  applyChildren (InPlace patches) cs
    | length patches == length cs = zipWith applyPatch patches cs
    | otherwise = error "InPlace with the wrong number of children"
  applyChildren (Rearrange ops) cs
    | reused /= nub reused = error "a DOM node cannot be in two places"
    | otherwise = rearranged
   where
    reused = [index | Reuse index _ <- ops]
    rearranged = rearrange ops cs

  rearrange ops cs =
    [ case op of
        Reuse index childPatch -> applyPatch childPatch (cs !! index)
        Create tree -> build tree
    | op <- ops
    ]


item :: String -> Html Msg
item name = li [] [text name]


keyedItem :: String -> Html Msg
keyedItem name = keyed name (item name)


-- Must be a top-level function for 'lazy' to recognise it again.
viewNumbers :: [Int] -> Html Msg
viewNumbers ns = ul [] [li [] [text (show n)] | n <- ns]


diffTests :: Suite -> IO ()
diffTests suite = do
  let patchOf old new = outline (fst (diff old new))
      list = ul []

  equal suite "identical trees need no change" "keep" $
    patchOf (div [A.id_ "a"] [p [] [text "x"]]) (div [A.id_ "a"] [p [] [text "x"]])
  equal suite "changed text" "update{}[text:b]" (patchOf (p [] [text "a"]) (p [] [text "b"]))
  equal suite "only the changed child is touched" "update{}[keep,update{}[text:c]]" $
    patchOf (div [] [p [] [text "a"], p [] [text "b"]]) (div [] [p [] [text "a"], p [] [text "c"]])
  equal suite "a different tag replaces the node" "replace:<span>a</span>" $
    patchOf (p [] [text "a"]) (span [] [text "a"])
  equal suite "text replaced by an element" "update{}[replace:<b></b>]" $
    patchOf (p [] [text "a"]) (p [] [b [] []])

  equal suite "changed attribute" "update{set attribute class=\"b\"}[]" $
    patchOf (div [A.className "a", A.id_ "x"] []) (div [A.className "b", A.id_ "x"] [])
  equal suite "removed attribute" "update{clear attribute class=\"a\"}[]" $
    patchOf (div [A.className "a"] []) (div [] [])
  equal suite "added attribute" "update{set attribute class=\"a\"}[]" $
    patchOf (div [] []) (div [A.className "a"] [])
  equal suite "boolean attribute turned off" "update{clear boolean attribute disabled=True}[]" $
    patchOf (button [A.disabled True] []) (button [A.disabled False] [])
  equal suite "boolean attribute staying off" "keep" $
    patchOf (button [A.disabled False] []) (button [] [])
  equal suite "only the last duplicate counts" "keep" $
    patchOf (div [A.className "b"] []) (div [A.className "a", A.className "b"] [])
  -- The user may have typed since the last render.
  equal suite "properties are always set again" "update{set property value=PropertyString \"a\"}[]" $
    patchOf (input [A.value "a"]) (input [A.value "a"])
  equal suite "removed property" "update{clear property checked=PropertyBool True}[]" $
    patchOf (input [A.checked True]) (input [])

  equal suite "new listeners are announced" "update{listen click}[]" $
    patchOf (button [] []) (button [onClick Clicked] [])
  equal suite "existing listeners are not announced again" "keep" $
    patchOf (button [onClick Clicked] []) (button [onClick (Wrapped Clicked)] [])

  equal suite "appended child" "update{}<0:keep,new:<li>b</li>>" $
    patchOf (list [item "a"]) (list [item "a", item "b"])
  equal suite "removed last child" "update{}<0:keep>" $
    patchOf (list [item "a", item "b"]) (list [item "a"])
  equal suite "without keys, children are matched by position" "update{}<0:update{}[text:b]>" $
    patchOf (list [item "a", item "b"]) (list [item "b"])

  equal suite "with keys, a removed child leaves the others alone" "update{}<1:keep>" $
    patchOf (list [keyedItem "a", keyedItem "b"]) (list [keyedItem "b"])
  equal suite "keyed children are moved, not rebuilt" "update{}<2:keep,0:keep,1:keep>" $
    patchOf
      (list [keyedItem "a", keyedItem "b", keyedItem "c"])
      (list [keyedItem "c", keyedItem "a", keyedItem "b"])
  equal suite "keyed child inserted at the front" "update{}<new:<li>z</li>,0:keep,1:keep>" $
    patchOf (list [keyedItem "a", keyedItem "b"]) (list [keyedItem "z", keyedItem "a", keyedItem "b"])
  equal suite "a keyed child is still updated" "update{}[update{}[text:new]]" $
    patchOf (list [keyed "a" (item "old")]) (list [keyed "a" (item "new")])
  equal suite "keyed and unkeyed children mix" "update{}<0:keep,2:keep,1:keep>" $
    patchOf (list [item "x", keyedItem "a", item "y"]) (list [item "x", item "y", keyedItem "a"])
  equal suite "a duplicate key creates a new node" "update{}<0:keep,new:<li>a</li>>" $
    patchOf (list [keyedItem "a"]) (list [keyedItem "a", keyedItem "a"])

  -- Fragments
  let ab = [item "a", item "b"]
  equal suite "a fragment renders as its children" "<ul><li>a</li><li>b</li><li>c</li></ul>" $
    renderHtml (list [fragment ab, item "c"])
  equal suite "an empty fragment renders as nothing" "<ul></ul>" (renderHtml (list [fragment []] :: Html Msg))
  equal suite "a fragment is the same as its children in place" "keep" $
    patchOf (list (ab <> [item "c"])) (list [fragment ab, item "c"])
  equal suite "fragments within fragments" "keep" $
    patchOf (list (ab <> [item "c"])) (list [fragment [fragment [item "a"], item "b"], fragment [], item "c"])
  -- Without keys, children are matched by position, fragment or not.
  equal suite "a fragment that grows shifts what follows" "update{}<0:keep,1:update{}[text:b],new:<li>c</li>>" $
    patchOf (list [fragment [item "a"], item "c"]) (list [fragment ab, item "c"])
  equal suite "unless what follows has a key" "update{}<0:keep,new:<li>b</li>,1:keep>" $
    patchOf (list [fragment [item "a"], keyedItem "c"]) (list [fragment ab, keyedItem "c"])
  equal suite "the page for a fragment" (build (list (ab <> [item "c"]))) $
    applyPatch (fst (diff (list [item "c"]) (list [fragment ab, item "c"]))) (build (list [item "c"]))
  equal suite "a keyed fragment moves as a whole" "update{}<2:keep,0:keep,1:keep>" $
    patchOf (list [keyed "f" (fragment ab), keyedItem "c"]) (list [keyedItem "c", keyed "f" (fragment ab)])
  equal suite "fmap reaches into fragments" [Right (Wrapped Clicked)] $
    fire "click" Null (unwrapFirst (fmap Wrapped (fragment [button [onClick Clicked] []])))

  -- Finding failures before touching the page
  let failing = div [] [p [] [text ("a" <> error "this view fails")]] :: Html Msg
      forced patch = either (const "failed") (const "fine") <$> (try (evaluate (force patch)) :: IO (Either SomeException ()))
  equal suite "force finds a failing view in a new node" "failed" =<< forced (fst (diff (text "x") failing))
  equal suite "force finds a failing view in a changed text" "failed" =<< forced (fst (diff (div [] [p [] [text "a"]]) failing))
  equal suite "force accepts a view that is fine" "fine" =<< forced (fst (diff (text "x") (div [] [p [] [text "a"]] :: Html Msg)))

  -- Lazy nodes
  let numbers = [1, 2, 3] :: [Int]
      others = [1, 2, 3] :: [Int]
      refsOf tree = case tree of
        I.Lazy refs _ -> refs
        _ -> []
      old = lazy viewNumbers numbers
      -- The same function and argument as 'old', but its tree must never be
      -- looked at.
      untouchable = I.Lazy (refsOf old) (error "a skipped lazy tree was evaluated")
  equal suite "lazy renders like the function it wraps" (renderHtml (viewNumbers numbers)) $
    renderHtml (lazy viewNumbers numbers)
  equal suite "lazy with the same argument is skipped unevaluated" "keep" (patchOf old untouchable)
  equal suite "a skipped lazy tree is carried over from the old tree" (renderHtml old) $
    renderHtml (snd (diff old untouchable))
  equal suite "force does not look at a skipped lazy tree" "fine" =<< forced (fst (diff old untouchable))
  equal suite "lazy below an unchanged parent" "keep" $
    patchOf (div [] [old, text "a"]) (div [] [untouchable, text "a"])
  equal suite "keyed lazy with the same argument is skipped" "keep" $
    patchOf (list [keyed "k" old]) (list [keyed "k" untouchable])
  equal suite "lazy with another argument is diffed" True $
    patchOf old (lazy viewNumbers [1, 2, 4]) /= "keep"
  equal suite "lazy with an equal but separate argument still gives the right page" (build old) $
    applyPatch (fst (diff old (lazy viewNumbers others))) (build old)
  equal suite "sameRefs: same values" True $
    I.sameRefs [I.Ref viewNumbers, I.Ref numbers] [I.Ref viewNumbers, I.Ref numbers]
  equal suite "sameRefs: different lengths" False $
    I.sameRefs [I.Ref numbers] [I.Ref numbers, I.Ref numbers]
  equal suite "sameRefs: different values" False (I.sameRefs [I.Ref numbers] [I.Ref "abc"])
  equal suite "fmap over lazy with the same function is skipped" "keep" $
    -- One function for both: without optimisation, each mention of a
    -- constructor can be a separate function in memory.
    let wrap = Wrapped in patchOf (fmap wrap old) (fmap wrap untouchable)
  equal suite "lazy2 and lazy3 render like their functions" "<p>ab</p><p>abc</p>" $
    renderHtml (lazy2 (\x y -> p [] [text (x <> y)]) "a" "b" :: Html ())
      <> renderHtml (lazy3 (\x y z -> p [] [text (x <> y <> z)]) "a" "b" "c" :: Html ())

  -- Random trees: whatever the two trees are, the patch must turn the page
  -- for the first into the page for the second.
  let trees = randomTrees 600
      pairs = zip trees (drop 1 trees)
      wrong =
        [ (renderHtml from, renderHtml to)
        | (from, to) <- pairs
        , applyPatch (fst (diff from to)) (build from) /= build to
        ]
      wrongTrees = [renderHtml to | (from, to) <- pairs, build (snd (diff from to)) /= build to]
      -- Properties are always set again, and of two siblings with the same
      -- key only one can be reused.
      plain = [tree | tree <- trees, hasNoProperty tree, hasUniqueKeys tree]
      notKept = [renderHtml tree | tree <- plain, patchOf tree tree /= "keep"]
  equal suite "random trees: the patch gives the new page" [] (take 3 wrong)
  equal suite "random trees: the resulting tree is the new tree" [] (take 3 wrongTrees)
  equal suite "random trees: a tree does not differ from itself" [] (take 3 notKept)
  equal suite "random trees are varied" True (length (nub (map renderHtml trees)) > 300)
  equal suite "random trees: enough of them can be compared with themselves" True (length plain > 100)


-- | The first node of a tree that may be a fragment.
unwrapFirst :: Html msg -> Html msg
unwrapFirst = I.unwrap


hasNoProperty :: Html msg -> Bool
hasNoProperty tree = case I.unwrap tree of
  I.Element _ attributes children ->
    null [() | I.Property{} <- attributes] && all hasNoProperty children
  _ -> True


hasUniqueKeys :: Html msg -> Bool
hasUniqueKeys tree = case I.unwrap tree of
  I.Element _ _ children ->
    let keys = [k | I.Keyed k _ <- children]
     in keys == nub keys && all hasUniqueKeys children
  _ -> True


-- | Trees drawn from a small vocabulary, so that neighbours in the list have
-- a lot in common, generated from a fixed seed.
randomTrees :: Int -> [Html Msg]
randomTrees count = take count (go 20240607)
 where
  go seed = let (tree, seed') = randomTree 3 seed in tree : go seed'


-- A linear congruential generator: a number below the bound, and the next seed.
pick :: Int -> Int -> (Int, Int)
pick bound seed = ((next `quot` 65536) `mod` bound, next)
 where
  -- Via Integer: Int has only 32 bits on wasm.
  next = fromInteger ((toInteger seed * 1103515245 + 12345) `mod` 2147483648)


randomTree :: Int -> Int -> (Html Msg, Int)
randomTree depth seed0
  | depth == 0 || kind == 0 = (text (["a", "b", ""] !! choice), seed2)
  | otherwise =
      let (attributeCount, seed3) = pick 4 seed2
          (attributes, seed4) = times attributeCount randomAttribute seed3
          (childCount, seed5) = pick 5 seed4
          (children, seed6) = times childCount randomChild seed5
       in (node (["div", "p", "ul"] !! choice) attributes children, seed6)
 where
  (kind, seed1) = pick 4 seed0
  (choice, seed2) = pick 3 seed1

  randomChild seed =
    let (child, seed') = randomTree (depth - 1) seed
        (wrapper, seed'') = pick 6 seed'
     in (if wrapper < 3 then keyed (["k1", "k2", "k3"] !! wrapper) child else child, seed'')


randomAttribute :: Int -> (Attribute Msg, Int)
randomAttribute seed0 = (attributes !! which, seed1)
 where
  (which, seed1) = pick (length attributes) seed0
  attributes =
    [ A.className "a"
    , A.className "b"
    , A.id_ "x"
    , A.disabled True
    , A.disabled False
    , A.value "v"
    , A.value "w"
    , A.checked True
    , A.checked False
    , A.attrib "value" "attr"
    , onClick Clicked
    ]


times :: Int -> (Int -> (a, Int)) -> Int -> ([a], Int)
times 0 _ seed = ([], seed)
times n step seed =
  let (x, seed') = step seed
      (xs, seed'') = times (n - 1) step seed'
   in (x : xs, seed'')
