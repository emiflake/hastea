{- | @hastea build@: compile an application and put its page together in a
folder that any static file server can serve.

The folder holds:

* @hastea.wasm@, the compiled module;
* @ghc_wasm_jsffi.js@, the JavaScript its FFI imports, from GHC's post-linker;
* @browser_wasi_shim.js@ and @index.js@, which load and start it;
* the application's own files from @www@, including @index.html@ (a default
  page if it has none).
-}
module Hastea.Cli.Build
  ( BuildOptions (..)
  , build
  )
where

import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Numeric (showFFloat)
import System.Directory
import System.FilePath

import Hastea.Cli.Config
import Hastea.Cli.Dev.Page (defaultPage)
import Hastea.Cli.Log qualified as Log
import Hastea.Cli.Runtime qualified as Runtime
import Hastea.Cli.Toolchain


data BuildOptions = BuildOptions
  { out :: Maybe FilePath
  -- ^ Instead of the application's @out@.
  , optimize :: Bool
  -- ^ Run wasm-opt even if the application does not ask for it.
  }


build :: Config -> BuildOptions -> App -> IO ()
build config options app = do
  let toolchain = config.toolchain
      optimizing = options.optimize || app.optimize
  requireTools ([toolchain.cabal, toolchain.ghc, toolchain.node] <> [toolchain.wasmOpt | optimizing])
  Log.info ("Building " <> app.name <> " (exe:" <> app.executable <> ")")
  cabalBuild config app
  compiled <- listBin config app

  out <- makeAbsolute (config.root </> fromMaybe app.out options.out)
  clearOutput config.root out
  forM_ app.www $ \www -> copyTree (config.root </> www) out
  let wasm = out </> "hastea.wasm"
  copyFile compiled wasm
  postLink toolchain wasm (out </> "ghc_wasm_jsffi.js")
  when optimizing (wasmOptimize toolchain wasm)
  BS.writeFile (out </> "browser_wasi_shim.js") Runtime.wasiShim
  -- An application may bring its own loader or page.
  writeUnlessThere (out </> "index.js") Runtime.loader
  writeUnlessThere (out </> "index.html") (encodeUtf8 defaultPage)

  size <- getFileSize wasm
  let shown = makeRelative config.root out
  Log.success ("Built " <> T.pack shown <> "/ (hastea.wasm is " <> T.pack (showFFloat (Just 1) (fromIntegral size / 1048576 :: Double) "") <> " MB)")
  Log.info ("Serve it with e.g.: python3 -m http.server -d " <> T.pack shown <> " 8080")


{- Empty the output folder, or create it. Refuse to empty one that holds the
project, or that has files in it but no sign of an earlier build: it was
most likely given by mistake.
-}
clearOutput :: FilePath -> FilePath -> IO ()
clearOutput root out = do
  rootAbsolute <- makeAbsolute root
  when (splitDirectories out `isPrefixOf` splitDirectories rootAbsolute) $
    Log.dieWith ("Refusing to build into " <> T.pack out <> ", which holds the project")
  exists <- doesDirectoryExist out
  when exists $ do
    contents <- listDirectory out
    unless (null contents || "hastea.wasm" `elem` contents) $
      Log.dieWith (T.pack out <> " has files in it but is not an earlier build; not deleting it")
    removeDirectoryRecursive out
  createDirectoryIfMissing True out


copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
  exists <- doesDirectoryExist from
  unless exists (Log.dieWith ("The www folder " <> T.pack from <> " does not exist"))
  createDirectoryIfMissing True to
  entries <- listDirectory from
  forM_ entries $ \entry -> do
    isFolder <- doesDirectoryExist (from </> entry)
    if isFolder then copyTree (from </> entry) (to </> entry) else copyFile (from </> entry) (to </> entry)


writeUnlessThere :: FilePath -> ByteString -> IO ()
writeUnlessThere path contents = do
  exists <- doesFileExist path
  unless exists (BS.writeFile path contents)
