{- | The settings in @hastea.yaml@.

@
default: todomvc          # the application to use when none is named
apps:
  todomvc:
    executable: hastea-todomvc       # the cabal executable (required)
    www: examples\/todomvc\/www        # files to put next to it (optional)
    out: dist\/todomvc                # where hastea build puts the page
    run: main                        # what hastea dev runs after a reload
    optimize: false                  # run wasm-opt in hastea build
toolchain:                           # the programs used, if not these
  cabal: wasm32-wasi-cabal
  ghc: wasm32-wasi-ghc
  node: node
  wasm-opt: wasm-opt
dev:
  port: 8080
  watch: [.]                         # where to look for changes
  ignore: [dist-newstyle, dist, node_modules, .git]
@

Paths are relative to the folder that holds @hastea.yaml@.
-}
module Hastea.Cli.Config
  ( Config (..)
  , App (..)
  , Toolchain (..)
  , DevSettings (..)
  , configFileName
  , findConfig
  , loadConfig
  , decodeConfig
  , selectApp
  )
where

import Control.Monad (forM_, unless, when)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))


-- | Everything in @hastea.yaml@.
data Config = Config
  { root :: FilePath
  -- ^ The folder that holds @hastea.yaml@, which other paths are relative to.
  , apps :: Map Text App
  , defaultApp :: Maybe Text
  , toolchain :: Toolchain
  , dev :: DevSettings
  }
  deriving (Eq, Show)


-- | One application: an executable and the files of its page.
data App = App
  { name :: Text
  , executable :: Text
  -- ^ The name of the executable in the cabal package.
  , www :: Maybe FilePath
  -- ^ Files to serve next to the application: index.html, styles, images.
  , out :: FilePath
  -- ^ Where @hastea build@ puts the page.
  , run :: Text
  -- ^ The expression @hastea dev@ runs in GHCi after each reload.
  , optimize :: Bool
  -- ^ Whether @hastea build@ shrinks the module with wasm-opt.
  }
  deriving (Eq, Show)


-- | The programs the hastea command runs.
data Toolchain = Toolchain
  { cabal :: FilePath
  , ghc :: FilePath
  , node :: FilePath
  , wasmOpt :: FilePath
  }
  deriving (Eq, Show)


-- | Settings for @hastea dev@.
data DevSettings = DevSettings
  { port :: Int
  , watch :: [FilePath]
  -- ^ Folders to look for changes in.
  , ignore :: [FilePath]
  -- ^ Names of folders not to look into.
  }
  deriving (Eq, Show)


configFileName :: FilePath
configFileName = "hastea.yaml"


-- | Look for @hastea.yaml@ in a folder and the folders above it.
findConfig :: FilePath -> IO (Maybe FilePath)
findConfig folder = do
  let candidate = folder </> configFileName
  exists <- doesFileExist candidate
  if exists
    then pure (Just candidate)
    else
      let parent = takeDirectory folder
       in if parent == folder then pure Nothing else findConfig parent


-- | Read a @hastea.yaml@.
loadConfig :: FilePath -> IO (Either Text Config)
loadConfig file = do
  absolute <- makeAbsolute file
  decodeConfig (takeDirectory absolute) <$> BS.readFile absolute


-- | Make sense of the contents of a @hastea.yaml@ found in a given folder.
decodeConfig :: FilePath -> ByteString -> Either Text Config
decodeConfig root bytes = case Yaml.decodeEither' bytes of
  Left failure -> Left (T.pack (Yaml.prettyPrintParseException failure))
  Right value -> first T.pack (Aeson.parseEither (parseConfig root) value)


parseConfig :: FilePath -> Aeson.Value -> Aeson.Parser Config
parseConfig root = Aeson.withObject "hastea.yaml" $ \o -> do
  only "hastea.yaml" ["default", "apps", "toolchain", "dev"] o
  apps <- Aeson.explicitParseField (Aeson.withObject "apps" (fmap Map.fromList . traverse parseApp . KeyMap.toList)) o "apps"
  when (Map.null apps) (fail "apps: name at least one application")
  defaultApp <- o .:? "default"
  forM_ defaultApp $ \name ->
    unless (Map.member name apps) (fail ("default: there is no application called " <> T.unpack name))
  toolchain <- Aeson.explicitParseFieldMaybe parseToolchain o "toolchain" >>= maybe (parseToolchain (Aeson.object [])) pure
  dev <- Aeson.explicitParseFieldMaybe parseDev o "dev" >>= maybe (parseDev (Aeson.object [])) pure
  pure Config{root, apps, defaultApp, toolchain, dev}


parseApp :: (Aeson.Key, Aeson.Value) -> Aeson.Parser (Text, App)
parseApp (key, value) = do
  let name = Key.toText key
      context = "apps." <> T.unpack name
  app <- (Aeson.<?> Aeson.Key key) $ flip (Aeson.withObject context) value $ \o -> do
    only context ["executable", "www", "out", "run", "optimize"] o
    executable <- o .: "executable"
    www <- o .:? "www"
    out <- o .:? "out" .!= ("dist" </> T.unpack name)
    run <- o .:? "run" .!= "main"
    optimize <- o .:? "optimize" .!= False
    pure App{name, executable, www, out, run, optimize}
  pure (name, app)


parseToolchain :: Aeson.Value -> Aeson.Parser Toolchain
parseToolchain = Aeson.withObject "toolchain" $ \o -> do
  only "toolchain" ["cabal", "ghc", "node", "wasm-opt"] o
  cabal <- o .:? "cabal" .!= "wasm32-wasi-cabal"
  ghc <- o .:? "ghc" .!= "wasm32-wasi-ghc"
  node <- o .:? "node" .!= "node"
  wasmOpt <- o .:? "wasm-opt" .!= "wasm-opt"
  pure Toolchain{cabal, ghc, node, wasmOpt}


parseDev :: Aeson.Value -> Aeson.Parser DevSettings
parseDev = Aeson.withObject "dev" $ \o -> do
  only "dev" ["port", "watch", "ignore"] o
  port <- o .:? "port" .!= 8080
  watch <- o .:? "watch" .!= ["."]
  ignore <- o .:? "ignore" .!= ["dist-newstyle", "dist", "node_modules", ".git"]
  pure DevSettings{port, watch, ignore}


-- Fail on a key that means nothing here, which is most likely a typo.
only :: String -> [Text] -> Aeson.Object -> Aeson.Parser ()
only context known o =
  forM_ (KeyMap.keys o) $ \key ->
    unless (Key.toText key `elem` known) $
      fail (context <> ": unknown setting " <> show (Key.toText key) <> " (expected one of " <> T.unpack (T.intercalate ", " known) <> ")")


-- | The application with a given name, or the default one.
selectApp :: Config -> Maybe Text -> Either Text App
selectApp config = \case
  Just name -> maybe (Left ("There is no application called " <> name <> " in hastea.yaml. " <> available)) Right (Map.lookup name config.apps)
  Nothing -> case (config.defaultApp, Map.elems config.apps) of
    (Just name, _) -> selectApp config (Just name)
    (Nothing, [app]) -> Right app
    _ -> Left ("Name the application to use, or set default in hastea.yaml. " <> available)
 where
  available = "It has: " <> T.intercalate ", " (Map.keys config.apps) <> "."
