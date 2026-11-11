{- | Subscriptions: messages from outside the view.

A view produces messages when the user does something with it. A
subscription produces messages on its own: as time passes, for example. An
application says which subscriptions it wants as a function of its model
(see the @subscriptions@ field of 'Hastea.App.App'); the runtime starts and stops them as
that answer changes.

@
subscriptions model
  | running model = Time.every 1000 Tick
  | otherwise = Sub.none
@

Ready-made subscriptions are in "Hastea.Time".
-}
module Hastea.Sub
  ( Sub
  , none
  , batch
  , custom
  )
where

import Data.Typeable (Typeable)

import Hastea.Internal.Sub


-- | Listen to nothing.
none :: Sub msg
none = mempty


-- | Several subscriptions as one.
batch :: [Sub msg] -> Sub msg
batch = mconcat


{- | A subscription to a source of your own.

The name identifies the source: while the application keeps asking for a
subscription of this name, the source keeps running, so include in it
whatever the source is set up with (an interval, an event name). The second
argument starts the source, given a function to call with each value, and
returns the action that stops it. The last argument turns a value into a
message, or 'Nothing' to ignore it.
-}
custom :: Typeable a => String -> ((a -> IO ()) -> IO (IO ())) -> (a -> Maybe msg) -> Sub msg
custom name start handler = Sub [(name, Source start handler)]
