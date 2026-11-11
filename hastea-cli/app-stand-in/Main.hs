{- | @hastea-ffi-stand-in@: a preprocessor for an ordinary GHC, which turns
JavaScript imports into stand-ins. See "Hastea.Cli.StandIn".

GHC runs it as @hastea-ffi-stand-in ORIGINAL INPUT OUTPUT@ when given
@-F -pgmF hastea-ffi-stand-in@.
-}
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.Environment (getArgs)
import System.Exit (die)

import Hastea.Cli.StandIn (standIn)


main :: IO ()
main =
  getArgs >>= \case
    original : input : output : _ -> do
      source <- decodeUtf8Lenient <$> BS.readFile input
      -- So that GHC reports problems in the file you edit, not in the copy
      -- it reads.
      let header = "{-# LINE 1 " <> T.pack (show original) <> " #-}\n"
      BS.writeFile output (encodeUtf8 (header <> standIn source))
    _ -> die "hastea-ffi-stand-in is a preprocessor, run by GHC: -F -pgmF hastea-ffi-stand-in"
