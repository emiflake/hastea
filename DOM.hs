{-# LANGUAGE NamedFieldPuns #-}
module DOM
  (
  -- * Attributes
  , attrib
  , className
  , on
  , onClick

  -- * DOM Rendering
  , runApp
  )
  where

import           Prelude           hiding (div, span)

import           Asterius.Prim
import           Asterius.Types

import           Foreign.StablePtr


import           Control.Monad
import           Data.Coerce
import           Data.IORef
import           Decode
import           System.IO.Unsafe




-- Virtual DOM



-- HTML DSL
