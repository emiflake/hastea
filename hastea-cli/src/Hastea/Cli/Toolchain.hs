{- | Running the programs of the wasm toolchain (ghc-wasm-meta): cabal and
GHC's cross-compiler, node for GHC's post-linker, and wasm-opt.
-}
module Hastea.Cli.Toolchain
  ( prepareEnvironment
  , requireTools
  , runIn
  , cabalBuild
  , listBin
  , postLink
  , wasmOptimize
  , ghcWasmPrefix
  )
where

import Control.Monad (filterM, forM_, unless, when)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory
import System.Environment (lookupEnv, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process

import Hastea.Cli.Config
import Hastea.Cli.Log qualified as Log


-- | Where ghc-wasm-meta installs itself, if it is there.
ghcWasmPrefix :: IO (Maybe FilePath)
ghcWasmPrefix = do
  prefix <- (</> ".ghc-wasm") <$> getHomeDirectory
  exists <- doesDirectoryExist prefix
  pure (if exists then Just prefix else Nothing)


{- | Make the toolchain reachable for this process and the programs it runs,
so that @source ~\/.ghc-wasm\/env@ is not needed: if the cabal it should use
is not on the PATH, put ghc-wasm-meta's programs there, and point NODE_PATH
at its node modules (GHCi's browser mode needs @ws@ from there).

Only the PATH and NODE_PATH are changed. The rest of ghc-wasm-meta's env file
points CC and LD at the wasm C toolchain, which would break anything else
that compiles C.
-}
prepareEnvironment :: Toolchain -> IO ()
prepareEnvironment toolchain =
  ghcWasmPrefix >>= mapM_ \prefix -> do
    onPath <- findExecutable toolchain.cabal
    when (isNothing onPath) $ do
      folders <- filterM doesDirectoryExist (map (prefix </>) ["wasm32-wasi-cabal/bin", "wasm32-wasi-ghc/bin", "nodejs/bin", "binaryen/bin", "wasmtime/bin"])
      path <- lookupEnv "PATH"
      setEnv "PATH" (intercalate ":" (folders <> maybe [] pure path))
    nodePath <- lookupEnv "NODE_PATH"
    let modules = prefix </> "nodejs/lib/node_modules"
    haveModules <- doesDirectoryExist modules
    when (maybe True null nodePath && haveModules) (setEnv "NODE_PATH" modules)


-- | Stop with a helpful message unless all these programs can be found.
requireTools :: [FilePath] -> IO ()
requireTools tools = do
  missing <- filterM (fmap isNothing . findExecutable) tools
  unless (null missing) $
    Log.dieWith $
      "Cannot find " <> T.intercalate ", " (map T.pack missing) <> ". These come with ghc-wasm-meta "
        <> "(https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta), which hastea looks for in ~/.ghc-wasm, "
        <> "or set them under toolchain: in hastea.yaml."


-- | Run a program in a folder, and stop if it fails.
runIn :: FilePath -> FilePath -> [String] -> IO ()
runIn folder program arguments = do
  (_, _, _, process) <- createProcess (proc program arguments){cwd = Just folder, delegate_ctlc = True}
  waitForProcess process >>= \case
    ExitSuccess -> pure ()
    ExitFailure code -> Log.dieWith (T.pack program <> " failed (exit code " <> T.pack (show code) <> ")")


-- Run a program in a folder and return what it prints, stopping if it fails.
readIn :: FilePath -> FilePath -> [String] -> IO Text
readIn folder program arguments = do
  (code, out, err) <- readCreateProcessWithExitCode (proc program arguments){cwd = Just folder} ""
  case code of
    ExitSuccess -> pure (T.strip (T.pack out))
    ExitFailure _ -> do
      forM_ (lines err) (Log.passThrough . T.pack)
      Log.dieWith (T.pack (unwords (program : arguments)) <> " failed")


-- | Compile the executable of an application.
cabalBuild :: Config -> App -> IO ()
cabalBuild config app = runIn config.root config.toolchain.cabal ["build", "exe:" <> T.unpack app.executable]


-- | Where cabal put the compiled module of an application.
listBin :: Config -> App -> IO FilePath
listBin config app = do
  listed <- readIn config.root config.toolchain.cabal ["list-bin", "-v0", "exe:" <> T.unpack app.executable]
  let path = T.unpack (last ("" : T.lines listed))
  -- Older versions of cabal leave the .wasm extension off.
  candidates <- filterM doesFileExist [path, path <> ".wasm"]
  case candidates of
    found : _ -> pure found
    [] -> Log.dieWith ("cabal says the module is at " <> T.pack path <> ", but there is nothing there")


{- | Generate the JavaScript that a module's JavaScript FFI imports, with the
post-linker that comes with GHC.
-}
postLink :: Toolchain -> FilePath -> FilePath -> IO ()
postLink toolchain wasm js = do
  libdir <- readIn "." toolchain.ghc ["--print-libdir"]
  runIn "." toolchain.node [T.unpack libdir </> "post-link.mjs", "-i", wasm, "-o", js]


-- | Make a module smaller with wasm-opt, in place.
wasmOptimize :: Toolchain -> FilePath -> IO ()
wasmOptimize toolchain wasm = do
  let optimized = wasm <> ".opt"
  runIn "." toolchain.wasmOpt ["-Oz", wasm, "-o", optimized]
  renameFile optimized wasm
