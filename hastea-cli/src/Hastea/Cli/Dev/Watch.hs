{- | Noticing that files changed, for @hastea dev@.

The watched folders are looked through a few times a second. That is cheap
for a project of this size, needs no help from the operating system, and
behaves the same everywhere, under WSL included.
-}
module Hastea.Cli.Dev.Watch
  ( Change (..)
  , classify
  , watchFiles
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Time (UTCTime)
import System.Directory
import System.FilePath

import Hastea.Cli.Config


-- | What a changed file means for the page.
data Change
  = SourceChanged
  -- ^ Haskell source: reload it in GHCi.
  | ProjectChanged
  -- ^ The cabal package, the project or hastea.yaml: start GHCi afresh.
  | StyleChanged
  -- ^ A stylesheet of the page: replace it.
  | PageChanged
  -- ^ Any other file of the page: load the page again.
  deriving (Eq, Ord, Show)


-- | What a change to a file, given relative to the project, means.
classify :: App -> FilePath -> Maybe Change
classify app path
  | Just www <- app.www
  , splitDirectories (normalise www) `isPrefixOf` splitDirectories (normalise path) =
      Just (if takeExtension path == ".css" then StyleChanged else PageChanged)
  | takeExtension path `elem` [".hs", ".lhs", ".hs-boot", ".hsc"] = Just SourceChanged
  | takeExtension path == ".cabal" = Just ProjectChanged
  | takeFileName path `elem` ["cabal.project", "cabal.project.local", configFileName] = Just ProjectChanged
  | otherwise = Nothing


-- The modification time of every file that matters, relative to the project.
snapshot :: Config -> App -> IO (Map FilePath UTCTime)
snapshot config app = Map.unions <$> mapM (walk . normalise) config.dev.watch
 where
  walk folder = do
    listed <- try (listDirectory (config.root </> folder))
    case listed of
      Left (_ :: IOException) -> pure Map.empty
      Right entries -> Map.unions <$> mapM (visit folder) entries
  visit folder entry
    | entry `elem` config.dev.ignore = pure Map.empty
    | otherwise = do
        let path = normalise (folder </> entry)
        isFolder <- doesDirectoryExist (config.root </> path)
        if isFolder
          then walk path
          else
            if isJust (classify app path)
              then either (\(_ :: IOException) -> Map.empty) (Map.singleton path) <$> try (getModificationTime (config.root </> path))
              else pure Map.empty


{- | Look for changes until the end of time, and report each batch of them,
once files have stopped changing for a moment (an editor may write several).
-}
watchFiles :: Config -> App -> ([(FilePath, Change)] -> IO ()) -> IO ()
watchFiles config app report = snapshot config app >>= loop
 where
  loop before = do
    threadDelay 250000
    now <- snapshot config app
    if now == before
      then loop before
      else do
        settled <- settle now
        let changed = [path | path <- Map.keys (Map.union settled before), Map.lookup path settled /= Map.lookup path before]
        report (mapMaybe (\path -> (path,) <$> classify app path) changed)
        loop settled
  settle current = do
    threadDelay 100000
    next <- snapshot config app
    if next == current then pure next else settle next
