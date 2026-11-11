module Main (main) where

import Prelude hiding (div, head, span)

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.IORef
import Data.List (nub, (\\))
import System.Exit (exitFailure)

import Hastea.Html
import Hastea.Html.Attributes qualified as A
import Hastea.Html.Decode (Decoder, Value (..), decodeValue)
import Hastea.Html.Decode qualified as D
import Hastea.Html.Events
import Hastea.Html.Internal qualified as I
import Hastea.Html.Render

import Tables qualified


-- A MINIMAL TEST HARNESS
--
-- Only depends on base, so that the suite runs the same natively and as a
-- wasm module under wasmtime.

type Suite = IORef (Int, [String])


-- | Record a check that passes when both values are equal.
equal :: (Eq a, Show a) => Suite -> String -> a -> a -> IO ()
equal suite name expected actual =
  modifyIORef' suite $ \(total, failures) ->
    if expected == actual
      then (total + 1, failures)
      else
        ( total + 1
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
    ]
  (total, failures) <- readIORef suite
  mapM_ (putStrLn . ("FAIL: " <>)) (reverse failures)
  putStrLn (show (total - length failures) <> " of " <> show total <> " checks passed")
  unless (null failures) exitFailure


-- HELPERS

-- | A stand-in for a JavaScript object.
obj :: [(String, Value)] -> Value
obj = ObjectValue


-- | The listeners on an element, as (event name, options, decoder).
listeners :: Html msg -> [(String, EventOptions, Decoder msg)]
listeners (I.Element _ attributes _) = [(e, o, d) | I.Listener e o d <- attributes]
listeners (I.Text _) = []


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
  let names el = [(e, I.preventDefault o, I.stopPropagation o) | (e, o, _) <- listeners el]
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
