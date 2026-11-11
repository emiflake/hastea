-- | The hastea command.
module Main (main) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.IO (hSetEncoding, stderr, stdout, utf8)

import Hastea.Cli.Build
import Hastea.Cli.Config
import Hastea.Cli.Dev
import Hastea.Cli.Log qualified as Log
import Hastea.Cli.Toolchain (prepareEnvironment)


data Options = Options
  { config :: Maybe FilePath
  , command :: Command
  }


data Command
  = Build [Text] BuildOptions
  | Dev (Maybe Text) DevOptions


main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  options <- execParser (info (parseOptions <**> helper) (fullDesc <> progDesc "Build Hastea applications, and develop them with live reload."))
  file <- case options.config of
    Just file -> pure file
    Nothing ->
      getCurrentDirectory >>= findConfig >>= \case
        Just file -> pure file
        Nothing -> Log.dieWith "There is no hastea.yaml in this folder or above it."
  config <- loadConfig file >>= either (\failure -> Log.dieWith (T.pack file <> ": " <> failure)) pure
  prepareEnvironment
  let select name = either Log.dieWith pure (selectApp config name)
  case options.command of
    Build names buildOptions -> do
      apps <- if null names then pure <$> select Nothing else mapM (select . Just) names
      forM_ apps (build config buildOptions)
    Dev name devOptions -> select name >>= \app -> dev config app devOptions


parseOptions :: Parser Options
parseOptions =
  Options
    <$> optional (strOption (long "config" <> metavar "FILE" <> help "The hastea.yaml to use (by default, the nearest one)"))
    <*> hsubparser
      ( command "build" (info parseBuild (progDesc "Compile applications and put their pages together, ready to be served"))
          <> command "dev" (info parseDev (progDesc "Run an application in GHCi's browser mode, reloading it whenever its source changes"))
      )
 where
  parseBuild =
    Build
      <$> many (strArgument (metavar "APP..." <> help "Applications from hastea.yaml (by default, the default one)"))
      <*> ( BuildOptions
              <$> optional (strOption (long "out" <> metavar "DIR" <> help "Where to put the page, instead of the application's out"))
              <*> switch (long "optimize" <> help "Shrink the module with wasm-opt")
          )
  parseDev =
    Dev
      <$> optional (strArgument (metavar "APP" <> help "An application from hastea.yaml (by default, the default one)"))
      <*> (DevOptions <$> optional (option auto (long "port" <> metavar "PORT" <> help "Where to serve the page (by default, dev.port, or 8080)")))
