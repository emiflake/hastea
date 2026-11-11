module Main (main) where

import Prelude hiding (div, head, span)

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.IORef
import Data.List (intercalate, nub, sortOn, (\\))
import System.Exit (exitFailure)

import Hastea.Cmd (runCmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Component
import Hastea.Component.Internal
import Hastea.Html
import Hastea.Html.Attributes qualified as A
import Hastea.Html.Decode (Decoder, Value (..), decodeValue)
import Hastea.Html.Decode qualified as D
import Hastea.Html.Diff
import Hastea.Html.Events
import Hastea.Html.Internal qualified as I
import Hastea.Html.Render
import Hastea.Internal.Sub qualified as I.Sub
import Hastea.Sub qualified as Sub

import Tables qualified


-- A MINIMAL TEST HARNESS
--
-- Only depends on base, so that the suite runs the same natively and as a
-- wasm module under wasmtime.

type Suite = IORef (Int, [String])


-- | Record a check that passes when both values are equal.
equal :: (Eq a, Show a) => Suite -> String -> a -> a -> IO ()
equal suite name expected actual =
  modifyIORef' suite $ \(count, failures) ->
    if expected == actual
      then (count + 1, failures)
      else
        ( count + 1
        , (name <> "\n    expected: " <> show expected <> "\n    actual:   " <> show actual) : failures
        )


main :: IO ()
main = do
  suite <- newIORef (0, [])
  mapM_
    ($ suite)
    [ elementTests
    , attributeTests
    , renderTests
    , eventTests
    , functorTests
    , decodeTests
    , diffTests
    , componentTests
    ]
  (count, failures) <- readIORef suite
  mapM_ (putStrLn . ("FAIL: " <>)) (reverse failures)
  putStrLn (show (count - length failures) <> " of " <> show count <> " checks passed")
  unless (null failures) exitFailure


-- HELPERS

-- | A stand-in for a JavaScript object.
obj :: [(String, Value)] -> Value
obj = ObjectValue


-- | The listeners on an element, as (event name, options, decoder).
listeners :: Html msg -> [(String, EventOptions, Decoder msg)]
listeners el = case I.unwrap el of
  I.Element _ attributes _ -> [(e, o, d) | I.Listener e o d <- attributes]
  _ -> []


{- | Simulate the browser firing an event on an element: the messages
produced by every listener for that event.
-}
fire :: String -> Value -> Html msg -> [Either String msg]
fire event payload el = [decodeValue d payload | (e, _, d) <- listeners el, e == event]


-- | What a single attribute is, in terms that can be compared.
describe :: Attribute msg -> String
describe attribute = case attribute of
  I.Attribute k v -> "attribute " <> k <> "=" <> show v
  I.BoolAttribute k v -> "boolean attribute " <> k <> "=" <> show v
  I.Property k v -> "property " <> k <> "=" <> show v
  I.Listener e o _ -> "listener " <> e <> " " <> show o


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

data Msg
  = Clicked
  | Typed String
  | Checked Bool
  | Key String
  | Wrapped Msg
  deriving (Eq, Show)


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
    foldl (flip setOn) (DomElement tag [] [] (map build children)) attributes
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


-- | The outline of a patch, for comparing with what is expected.
outline :: Patch msg -> String
outline patch = case patch of
  Keep -> "keep"
  Replace tree -> "replace:" <> renderHtml tree
  SetText content -> "text:" <> content
  Update changes children ->
    "update{" <> intercalate "," (map changeOutline changes) <> "}" <> childrenOutline children
 where
  changeOutline (Set attribute) = "set " <> describe attribute
  changeOutline (Clear attribute) = "clear " <> describe attribute
  changeOutline (Listen event) = "listen " <> event

  childrenOutline (InPlace patches) = "[" <> intercalate "," (map outline patches) <> "]"
  childrenOutline (Rearrange ops) = "<" <> intercalate "," (map opOutline ops) <> ">"

  opOutline (Reuse index childPatch) = show index <> ":" <> outline childPatch
  opOutline (Create tree) = "new:" <> renderHtml tree


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


-- COMPONENTS
--
-- A stand-in for "Hastea.Root": it keeps the store, expands trees, and
-- routes messages, but there is no page.

data Sim msg = Sim
  { store :: IORef (Store msg)
  , received :: IORef [msg]
  -- ^ The messages that left the tree, latest first.
  }


newSim :: IO (Sim msg)
newSim = Sim <$> newIORef emptyStore <*> newIORef []


-- | What "Hastea.Root.render" does, minus the page.
showTree :: Sim msg -> Html msg -> IO (Html (Routed msg))
showTree sim tree = do
  (expanded, new) <- expand tree <$> readIORef sim.store
  writeIORef sim.store new
  pure expanded


route :: Sim msg -> Routed msg -> IO ()
route sim (Out msg) = modifyIORef' sim.received (msg :)
route sim (Local path msg) = do
  current <- readIORef sim.store
  case deliver path msg current of
    Nothing -> pure ()
    Just (new, cmd) -> writeIORef sim.store new >> runCmd (route sim) cmd


-- | Click the element found by following child positions down a tree.
click :: Sim msg -> [Int] -> Html (Routed msg) -> IO ()
click sim path tree = mapM_ (route sim) [msg | Right msg <- fire "click" Null (descend path tree)]
 where
  descend [] el = el
  descend (index : rest) el = case I.unwrap el of
    I.Element _ _ children | child : _ <- drop index children -> descend rest child
    _ -> text ""


newtype CounterMsg = Add Int


-- | Counts clicks, in steps given as its props.
counter :: Component Int Int CounterMsg out
counter =
  Component
    { initial = const 0
    , update = \(Add n) count -> (count + n, [])
    , view = \step count -> button [onClick (Add step)] [text (show count)]
    , subscriptions = const Sub.none
    }


data ReporterMsg = Report | Again


-- | Tells its surroundings how often it was clicked, and can send itself a message.
reporter :: Component () Int ReporterMsg Int
reporter =
  Component
    { initial = const 0
    , update = \msg count -> case msg of
        Report -> (count + 1, [Emit (count + 1)])
        Again -> (count, [Run (Cmd.perform (pure Report))])
    , view = \() count ->
        div [] [button [onClick Report] [text (show count)], button [onClick Again] [text "again"]]
    , subscriptions = \count -> if count > 0 then Sub.custom "ticks" neverStarts Just else Sub.none
    }
 where
  neverStarts :: (ReporterMsg -> IO ()) -> IO (IO ())
  neverStarts _ = pure (pure ())


newtype TotalMsg = Reported Int


-- | Contains a 'reporter' and shows the last number it reported.
total :: Component () Int TotalMsg out
total =
  Component
    { initial = const 0
    , update = \(Reported n) _ -> (n, [])
    , view = \() latest -> div [] [Reported <$> use reporter (), span [] [text (show latest)]]
    , subscriptions = const Sub.none
    }


-- Must be top-level functions for 'lazy' to recognise them again.
lazyCounter :: Int -> Html Msg
lazyCounter step = div [] [use counter step]


componentTests :: Suite -> IO ()
componentTests suite = do
  let shown = renderHtml :: Html (Routed Msg) -> String

  equal suite "a component without a state shows its initial view" "<p><button>0</button></p>" $
    renderHtml (p [] [use counter 1] :: Html Msg)

  -- Two uses of one component have separate states.
  sim <- newSim
  let two = div [] [use counter 1, use counter 10] :: Html Msg
  first <- showTree sim two
  equal suite "components start in their initial state" "<div><button>0</button><button>0</button></div>" (shown first)
  click sim [0] first
  click sim [0] first
  click sim [1] first
  second <- showTree sim two
  equal suite "each use of a component has its own state" "<div><button>2</button><button>10</button></div>" (shown second)
  equal suite "a component's own messages stay inside" [] =<< readIORef sim.received

  -- Props are passed in each time; the state stays.
  third <- showTree sim (div [] [use counter 5, use counter 10])
  click sim [0] third
  fourth <- showTree sim (div [] [use counter 5, use counter 10])
  equal suite "new props keep the state" "<div><button>7</button><button>10</button></div>" (shown fourth)

  -- State belongs to a place in the tree.
  fifth <- showTree sim (div [] [use counter 10])
  equal suite "without keys, state goes by position" "<div><button>7</button></div>" (shown fifth)
  _ <- showTree sim (div [] [])
  sixth <- showTree sim two
  equal suite "a component that was removed starts afresh" "<div><button>0</button><button>0</button></div>" (shown sixth)
  click sim [0] first
  seventh <- showTree sim (p [] [use counter 1])
  equal suite "state does not carry over to a different element" "<p><button>0</button></p>" (shown seventh)

  -- Keys make the state follow the item.
  keyedSim <- newSim
  let list names = ul [] [keyed name (li [] [use counter 1]) | name <- names] :: Html Msg
  ab <- showTree keyedSim (list ["a", "b"])
  click keyedSim [0, 0] ab
  ba <- showTree keyedSim (list ["b", "a"])
  equal suite "with keys, state follows the item" "<ul><li><button>0</button></li><li><button>1</button></li></ul>" (shown ba)
  onlyB <- showTree keyedSim (list ["b"])
  equal suite "removing a keyed item leaves the others' state" "<ul><li><button>0</button></li></ul>" (shown onlyB)
  back <- showTree keyedSim (list ["a", "b"])
  equal suite "a keyed item that returns starts afresh" "<ul><li><button>0</button></li><li><button>0</button></li></ul>" (shown back)

  -- A message for a component that is gone.
  goneSim <- newSim
  present <- showTree goneSim (div [] [use counter 1] :: Html Msg)
  _ <- showTree goneSim (div [] [])
  click goneSim [0] present
  after <- showTree goneSim (div [] [use counter 1])
  equal suite "a message for a removed component is dropped" "<div><button>0</button></div>" (shown after)

  -- Outputs become messages of the surrounding view.
  outSim <- newSim
  let reporting = div [] [Typed . show <$> use reporter ()] :: Html Msg
  r1 <- showTree outSim reporting
  click outSim [0, 0] r1
  click outSim [0, 0] r1
  equal suite "outputs reach the surrounding view, mapped by fmap" [Typed "2", Typed "1"] =<< readIORef outSim.received
  click outSim [0, 1] r1
  equal suite "commands send their messages back to the component" [Typed "3", Typed "2", Typed "1"] =<< readIORef outSim.received
  r2 <- showTree outSim reporting
  equal suite "the view shows the state after several messages" "<div><div><button>3</button><button>again</button></div></div>" (shown r2)

  -- Components inside components.
  nestedSim <- newSim
  let nesting = div [] [use total ()] :: Html Msg
  n1 <- showTree nestedSim nesting
  click nestedSim [0, 0, 0] n1
  click nestedSim [0, 0, 0] n1
  n2 <- showTree nestedSim nesting
  equal suite "a nested component's output goes to the component around it" "<div><div><div><button>2</button><button>again</button></div><span>2</span></div></div>" (shown n2)
  equal suite "and no further" [] =<< readIORef nestedSim.received

  -- Subscriptions
  let names (I.Sub.Sub sources) = map fst sources
  subSim <- newSim
  let twoReporters = div [] [Typed . show <$> use reporter (), Typed . show <$> use reporter ()] :: Html Msg
  s1 <- showTree subSim twoReporters
  equal suite "no subscriptions to begin with" [] . names . subscriptionsOf =<< readIORef subSim.store
  click subSim [0, 0] s1
  equal suite "a component's subscriptions follow its state at once" 1 . length . names . subscriptionsOf =<< readIORef subSim.store
  _ <- showTree subSim twoReporters
  equal suite "and stay when the tree is shown again" 1 . length . names . subscriptionsOf =<< readIORef subSim.store
  click subSim [1, 0] s1
  _ <- showTree subSim twoReporters
  subscribed <- names . subscriptionsOf <$> readIORef subSim.store
  equal suite "each component has its own subscriptions" 2 (length (nub subscribed))
  _ <- showTree subSim (div [] [])
  equal suite "a removed component's subscriptions end" [] . names . subscriptionsOf =<< readIORef subSim.store

  -- Lazy subtrees with components in them
  lazySim <- newSim
  let lazily = div [] [lazy lazyCounter 1, text "x"] :: Html Msg
      patchBetween old new = outline (fst (diff old new))
  l1 <- showTree lazySim lazily
  l2 <- showTree lazySim lazily
  equal suite "an unchanged lazy subtree with a component is skipped" "keep" (patchBetween l1 l2)
  click lazySim [0, 0] l2
  l3 <- showTree lazySim lazily
  equal suite "a component's new state gets through a lazy subtree" "<div><div><button>1</button></div>x</div>" (shown l3)
  equal suite "and is seen by the diff" True (patchBetween l2 l3 /= "keep")
  l4 <- showTree lazySim lazily
  equal suite "after which the subtree is skipped again" "keep" (patchBetween l3 l4)
  click lazySim [0, 0] l4
  l5 <- showTree lazySim lazily
  equal suite "handlers in a skipped subtree still reach their component" "<div><div><button>2</button></div>x</div>" (shown l5)
