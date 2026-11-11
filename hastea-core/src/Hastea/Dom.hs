{- | Commands that act on elements of the page.

A view describes what the page looks like, which does not cover everything:
which element has the focus is not a property of the tree. These commands
reach an element by its @id@ attribute.
-}
module Hastea.Dom
  ( focus
  , blur
  )
where

import Data.Text (Text)
import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Internal.Foreign.Types


-- Wait for the next frame, so that a view rendered because of the same
-- message is on the page by then.
foreign import javascript unsafe "requestAnimationFrame(() => document.getElementById($1)?.focus())"
  ffi_focus :: JSString -> IO ()
foreign import javascript unsafe "requestAnimationFrame(() => document.getElementById($1)?.blur())"
  ffi_blur :: JSString -> IO ()


{- | Give the focus to the element with this id, once the page shows the
latest view. Does nothing if there is no such element.
-}
focus :: Text -> Cmd msg
focus = Cmd.effect . ffi_focus . toJSString


-- | Take the focus away from the element with this id.
blur :: Text -> Cmd msg
blur = Cmd.effect . ffi_blur . toJSString
