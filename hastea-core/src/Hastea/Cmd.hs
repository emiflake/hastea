{- | Commands: effects for the runtime to carry out.

'Hastea.App.runApp' keeps @update@ pure. When something has to happen
outside the model (waiting, asking for the time, moving the focus), @update@
returns a command describing it next to the new model. The runtime runs the
command, and any message it produces comes back through @update@.

Ready-made commands are in "Hastea.Time" and "Hastea.Dom".
-}
module Hastea.Cmd
  ( Cmd
  , none
  , batch

    -- * Making commands
  , perform
  , effect
  , custom
  , log

    -- * Running commands
  , runCmd
  )
where

import Prelude hiding (log)

import Hastea.Internal.Foreign.Effects qualified as Effs


{- | An effect that may send messages of type @msg@, now or later, any
number of times.
-}
newtype Cmd msg
  = Cmd ((msg -> IO ()) -> IO ())


-- | Change the messages a command sends, to embed it in a larger application.
instance Functor Cmd where
  fmap f (Cmd run) = Cmd (\dispatch -> run (dispatch . f))


-- | '<>' runs one command, then the other.
instance Semigroup (Cmd msg) where
  Cmd a <> Cmd b = Cmd (\dispatch -> a dispatch >> b dispatch)


instance Monoid (Cmd msg) where
  mempty = none


-- | Do nothing.
none :: Cmd msg
none = Cmd (\_ -> pure ())


-- | Several commands as one. They are started in order.
batch :: [Cmd msg] -> Cmd msg
batch = mconcat


-- | Run an action and send its result as a message.
perform :: IO msg -> Cmd msg
perform action = Cmd (action >>=)


-- | Run an action that produces no message.
effect :: IO () -> Cmd msg
effect action = Cmd (const action)


{- | A command with full control: it is given the function that sends a
message to the application, and may call it whenever it likes, for example
from a callback.
-}
custom :: ((msg -> IO ()) -> IO ()) -> Cmd msg
custom = Cmd


-- | Write a line to the browser console.
log :: String -> Cmd msg
log = effect . Effs.putStrLn


-- | Carry out a command, sending its messages to the given function.
runCmd :: (msg -> IO ()) -> Cmd msg -> IO ()
runCmd dispatch (Cmd run) = run dispatch
