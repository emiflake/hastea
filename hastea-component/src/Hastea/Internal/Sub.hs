{-# LANGUAGE GADTs #-}

-- | Subscriptions and the bookkeeping that starts and stops them.
module Hastea.Internal.Sub
  ( Sub (..)
  , Source (..)
  , prefixed
  , Manager
  , newManager
  , sync
  )
where

import Control.Monad
import Data.IORef
import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable, eqT, (:~:) (Refl))


{- | Sources of messages that an application listens to for as long as it
asks for them: each with a name. See "Hastea.Sub".
-}
newtype Sub msg = Sub [(Text, Source msg)]


{- | A way to start a source of values, which returns the way to stop it, and
what to do with the values.
-}
data Source msg = forall a. Typeable a => Source ((a -> IO ()) -> IO (IO ())) (a -> Maybe msg)


-- | Change the messages a subscription sends, to embed it in a larger application.
instance Functor Sub where
  fmap f (Sub sources) =
    Sub [(name, Source start (fmap f . handler)) | (name, Source start handler) <- sources]


-- | '<>' subscribes to both.
instance Semigroup (Sub msg) where
  Sub a <> Sub b = Sub (a <> b)


instance Monoid (Sub msg) where
  mempty = Sub []


-- | Put a prefix before the name of every source, to tell apart the same
-- subscription made in different places.
prefixed :: Text -> Sub msg -> Sub msg
prefixed prefix (Sub sources) = Sub [(prefix <> name, source) | (name, source) <- sources]


-- A source that has been started: what currently happens to its values, and
-- how to stop it.
data Running msg = forall a. Typeable a => Running (IORef (a -> Maybe msg)) (IO ())


-- | The sources that are currently running.
newtype Manager msg = Manager (IORef (Map Text (Running msg)))


newManager :: IO (Manager msg)
newManager = Manager <$> newIORef Map.empty


{- | Make the running sources match what the application now asks for.

A source that is already running under the same name keeps running, and only
what happens to its values is replaced. So a subscription always uses the
function it was last given, like an event handler in a view, and is not
restarted because the model changed.
-}
sync :: forall msg. (msg -> IO ()) -> Manager msg -> Sub msg -> IO ()
sync dispatch (Manager runningRef) sub = do
  running <- readIORef runningRef
  let wanted = named sub
  -- Stop what is no longer asked for.
  forM_ (running `Map.withoutKeys` Map.keysSet wanted) $ \(Running _ stop) -> stop
  Map.traverseWithKey (ensure running) wanted >>= writeIORef runningRef
 where
  ensure :: Map Text (Running msg) -> Text -> Source msg -> IO (Running msg)
  ensure running name (Source start handler) = case Map.lookup name running of
    Just existing | Just update <- retarget handler existing -> existing <$ update
    existing -> do
      forM_ existing $ \(Running _ stop) -> stop
      ref <- newIORef handler
      stop <- start (\value -> readIORef ref >>= \current -> mapM_ dispatch (current value))
      pure (Running ref stop)


-- Point a running source at a new handler, if it emits the same type of value.
retarget :: forall a msg. Typeable a => (a -> Maybe msg) -> Running msg -> Maybe (IO ())
retarget handler (Running (ref :: IORef (b -> Maybe msg)) _) =
  case eqT @a @b of
    Just Refl -> Just (writeIORef ref handler)
    Nothing -> Nothing


{- | The sources of a subscription by name. When a name occurs more than once,
the occurrences are numbered, so that each one runs separately.
-}
named :: Sub msg -> Map Text (Source msg)
named (Sub sources) = Map.fromList (snd (mapAccumL number Map.empty sources))
 where
  number seen (name, source) =
    let count = Map.findWithDefault (0 :: Int) name seen
     in (Map.insert name (count + 1) seen, (name <> "#" <> T.pack (show count), source))
