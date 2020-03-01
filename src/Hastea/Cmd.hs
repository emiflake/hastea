{-# LANGUAGE LambdaCase #-}
module Hastea.Cmd
  ( Cmd
  , none
  , unsafeLiftCmd
  , runCmd


  -- * Helper Cmds
  , batch
  , log
  )
  where

import           Control.Applicative
import           Data.Coerce
import           Prelude                         hiding (log)


import qualified Hastea.Internal.Foreign.Effects as Effs


newtype Cmd a
  = Cmd (IO (Maybe a))


instance Functor Cmd where
  fmap f (Cmd action) =
      Cmd $ (fmap . fmap) f action


instance Applicative Cmd where
  pure a = Cmd $ pure (Just a)
  (Cmd fc) <*> (Cmd ac) =
    Cmd $ do
      f <- fc
      a <- ac
      pure $ liftA2 ($) f a


instance Monad Cmd where
  return = pure
  (Cmd ma) >>= afb = Cmd $ do
    a' <- ma
    case a' of
      Nothing -> pure Nothing
      Just a -> do
        mb <- runCmd (afb a)
        case mb of
          Nothing -> pure Nothing
          Just v  -> pure (Just v)


unsafeLiftCmd :: IO a -> Cmd a
unsafeLiftCmd = Cmd . fmap Just


none :: Cmd a
none = Cmd (pure Nothing)


batch :: [Cmd a] -> Cmd [a]
batch = sequence


runCmd :: Cmd a -> IO (Maybe a)
runCmd (Cmd a) = a

log :: String -> Cmd a
log = (>>none) . (unsafeLiftCmd . Effs.putStrLn)
