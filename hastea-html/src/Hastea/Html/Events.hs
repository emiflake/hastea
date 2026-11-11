{- | Event handlers for the elements in "Hastea.Html".

Most handlers simply produce a fixed message:

@
button [onClick Increment] [text "+"]
@

Handlers that carry information about the event take a function instead:

@
input [onInput NameChanged, onKeyDown KeyPressed]
@

For anything else, build a handler with 'on' and a
'Hastea.Html.Decode.Decoder' that reads what you need from the event object.
-}
module Hastea.Html.Events
  ( -- * Custom handlers
    on
  , onWith
  , EventOptions (..)
  , defaultEventOptions

    -- * Event decoders
  , targetValue
  , targetChecked
  , key

    -- * Mouse
  , onClick
  , onDoubleClick
  , onContextMenu
  , onMouseDown
  , onMouseUp
  , onMouseEnter
  , onMouseLeave
  , onMouseOver
  , onMouseOut
  , onMouseMove
  , onWheel

    -- * Forms
  , onInput
  , onChange
  , onCheck
  , onSubmit
  , onReset
  , onFocus
  , onBlur
  , onSelect
  , onInvalid

    -- * Keyboard
  , onKeyDown
  , onKeyUp
  , onKeypress

    -- * Pointer
  , onPointerDown
  , onPointerUp
  , onPointerMove
  , onPointerEnter
  , onPointerLeave
  , onPointerCancel

    -- * Touch
  , onTouchStart
  , onTouchMove
  , onTouchEnd
  , onTouchCancel

    -- * Drag and drop
  , onDragStart
  , onDrag
  , onDragEnd
  , onDragEnter
  , onDragLeave
  , onDragOver
  , onDrop

    -- * Clipboard
  , onCopy
  , onCut
  , onPaste

    -- * Miscellaneous
  , onScroll
  , onLoad
  , onError
  , onToggle
  )
where

import Hastea.Html.Decode (Decoder)
import Hastea.Html.Decode qualified as Decode
import Hastea.Html.Internal (Attribute (..), EventOptions (..), defaultEventOptions)


-- CUSTOM HANDLERS

{- | Listen for the named event and decode the event object into a message.
If the decoder fails, no message is produced.
-}
on :: String -> Decoder msg -> Attribute msg
on event = Listener event defaultEventOptions


-- | Like 'on', but can also prevent the default action or stop propagation.
onWith :: EventOptions -> String -> Decoder msg -> Attribute msg
onWith options event = Listener event options


-- Listen for an event and always produce the same message.
simple :: String -> msg -> Attribute msg
simple event msg = on event (pure msg)


-- Like 'simple', but also prevents the event's default action.
preventing :: String -> msg -> Attribute msg
preventing event msg = onWith defaultEventOptions{preventDefault = True} event (pure msg)


-- EVENT DECODERS

-- | Decode @event.target.value@: the current text of a form control.
targetValue :: Decoder String
targetValue = Decode.at ["target", "value"] Decode.string


-- | Decode @event.target.checked@: whether a checkbox is checked.
targetChecked :: Decoder Bool
targetChecked = Decode.at ["target", "checked"] Decode.bool


-- | Decode @event.key@ of a keyboard event, for example @\"Enter\"@ or @\"a\"@.
key :: Decoder String
key = Decode.field "key" Decode.string


-- MOUSE

onClick :: msg -> Attribute msg
onClick = simple "click"


onDoubleClick :: msg -> Attribute msg
onDoubleClick = simple "dblclick"


-- | The context menu was requested, usually with a right click.
onContextMenu :: msg -> Attribute msg
onContextMenu = simple "contextmenu"


onMouseDown :: msg -> Attribute msg
onMouseDown = simple "mousedown"


onMouseUp :: msg -> Attribute msg
onMouseUp = simple "mouseup"


-- | The pointer entered the element. Does not bubble.
onMouseEnter :: msg -> Attribute msg
onMouseEnter = simple "mouseenter"


-- | The pointer left the element. Does not bubble.
onMouseLeave :: msg -> Attribute msg
onMouseLeave = simple "mouseleave"


-- | The pointer entered the element or one of its descendants.
onMouseOver :: msg -> Attribute msg
onMouseOver = simple "mouseover"


-- | The pointer left the element or one of its descendants.
onMouseOut :: msg -> Attribute msg
onMouseOut = simple "mouseout"


onMouseMove :: msg -> Attribute msg
onMouseMove = simple "mousemove"


onWheel :: msg -> Attribute msg
onWheel = simple "wheel"


-- FORMS

-- | The value of a form control changed; receives the new value as it is typed.
onInput :: (String -> msg) -> Attribute msg
onInput toMsg = on "input" (toMsg <$> targetValue)


{- | The value of a form control was committed, for example when a text field
loses focus or an option is chosen; receives the new value.
-}
onChange :: (String -> msg) -> Attribute msg
onChange toMsg = on "change" (toMsg <$> targetValue)


-- | A checkbox or radio button was toggled; receives whether it is now checked.
onCheck :: (Bool -> msg) -> Attribute msg
onCheck toMsg = on "change" (toMsg <$> targetChecked)


{- | A form was submitted. The default action is prevented, so the browser
does not navigate away from the page.
-}
onSubmit :: msg -> Attribute msg
onSubmit = preventing "submit"


onReset :: msg -> Attribute msg
onReset = simple "reset"


onFocus :: msg -> Attribute msg
onFocus = simple "focus"


onBlur :: msg -> Attribute msg
onBlur = simple "blur"


-- | Text was selected in a text field.
onSelect :: msg -> Attribute msg
onSelect = simple "select"


-- | A form control failed validation.
onInvalid :: msg -> Attribute msg
onInvalid = simple "invalid"


-- KEYBOARD

-- | A key was pressed; receives the 'key'.
onKeyDown :: (String -> msg) -> Attribute msg
onKeyDown toMsg = on "keydown" (toMsg <$> key)


-- | A key was released; receives the 'key'.
onKeyUp :: (String -> msg) -> Attribute msg
onKeyUp toMsg = on "keyup" (toMsg <$> key)


{- | A key that produces a character was pressed; receives the 'key'. The
@keypress@ event is deprecated in browsers: prefer 'onKeyDown'.
-}
onKeypress :: (String -> msg) -> Attribute msg
onKeypress toMsg = on "keypress" (toMsg <$> key)


-- POINTER

onPointerDown :: msg -> Attribute msg
onPointerDown = simple "pointerdown"


onPointerUp :: msg -> Attribute msg
onPointerUp = simple "pointerup"


onPointerMove :: msg -> Attribute msg
onPointerMove = simple "pointermove"


onPointerEnter :: msg -> Attribute msg
onPointerEnter = simple "pointerenter"


onPointerLeave :: msg -> Attribute msg
onPointerLeave = simple "pointerleave"


onPointerCancel :: msg -> Attribute msg
onPointerCancel = simple "pointercancel"


-- TOUCH

onTouchStart :: msg -> Attribute msg
onTouchStart = simple "touchstart"


onTouchMove :: msg -> Attribute msg
onTouchMove = simple "touchmove"


onTouchEnd :: msg -> Attribute msg
onTouchEnd = simple "touchend"


onTouchCancel :: msg -> Attribute msg
onTouchCancel = simple "touchcancel"


-- DRAG AND DROP

onDragStart :: msg -> Attribute msg
onDragStart = simple "dragstart"


onDrag :: msg -> Attribute msg
onDrag = simple "drag"


onDragEnd :: msg -> Attribute msg
onDragEnd = simple "dragend"


onDragEnter :: msg -> Attribute msg
onDragEnter = simple "dragenter"


onDragLeave :: msg -> Attribute msg
onDragLeave = simple "dragleave"


{- | Something is being dragged over the element. The default action is
prevented, which is what marks the element as a valid drop target.
-}
onDragOver :: msg -> Attribute msg
onDragOver = preventing "dragover"


{- | Something was dropped on the element. The default action is prevented,
so the browser does not open the dropped item.
-}
onDrop :: msg -> Attribute msg
onDrop = preventing "drop"


-- CLIPBOARD

onCopy :: msg -> Attribute msg
onCopy = simple "copy"


onCut :: msg -> Attribute msg
onCut = simple "cut"


onPaste :: msg -> Attribute msg
onPaste = simple "paste"


-- MISCELLANEOUS

onScroll :: msg -> Attribute msg
onScroll = simple "scroll"


-- | A resource such as an image finished loading.
onLoad :: msg -> Attribute msg
onLoad = simple "load"


-- | A resource such as an image failed to load.
onError :: msg -> Attribute msg
onError = simple "error"


-- | A 'Hastea.Html.details' element or popover was opened or closed.
onToggle :: msg -> Attribute msg
onToggle = simple "toggle"
