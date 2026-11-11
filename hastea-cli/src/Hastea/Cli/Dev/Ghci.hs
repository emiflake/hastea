{- | A GHCi session in browser mode (@-fghci-browser@), for @hastea dev@.

GHCi itself runs here, but the interpreter and the program run in a page:
GHCi prints the address of a script that boots them, and waits for a page to
import it before it loads any module. Each session serves one page; a
second page that connects to it brings it down, so @hastea dev@ starts a new
session for every page load.

To tell where the output of one command ends, the prompt is set to a line
that nothing else prints.
-}
module Hastea.Cli.Dev.Ghci
  ( Session
  , start
  , stop
  , alive
  , origin
  , exitedSTM
  , awaitPrompt
  , command

    -- * Reading GHCi's output
  , promptMarker
  , parseOrigin
  , LoadResult (..)
  , loadResult
  , threwException
  , isQuiet
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Data.IORef
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.IO
import System.Posix.Signals (sigKILL, sigTERM, signalProcessGroup)
import System.Process
import System.Timeout (timeout)

import Hastea.Cli.Config
import Hastea.Cli.Log qualified as Log


data Session = Session
  { input :: Handle
  , process :: ProcessHandle
  , originVar :: TMVar (Maybe Text)
  -- ^ Where to boot GHCi from, once it says; Nothing if it stopped first.
  , prompts :: TQueue Text
  -- ^ The output that came before each prompt.
  , exited :: TMVar ()
  , lock :: MVar ()
  -- ^ Held while a command runs.
  , stopping :: IORef Bool
  -- ^ Set by 'stop', after which nothing more is shown: GHCi and cabal
  -- saying goodbye at once only garble the terminal.
  }


-- | Start @cabal repl@ for the executable of an application, in browser mode.
start :: Config -> App -> IO Session
start config app = do
  (readEnd, writeEnd) <- createPipe
  (Just input, _, _, process) <-
    createProcess
      (proc config.toolchain.cabal ["repl", "exe:" <> T.unpack app.executable, "--repl-options=-fghci-browser"])
        { cwd = Just config.root
        , std_in = CreatePipe
        , std_out = UseHandle writeEnd
        , std_err = UseHandle writeEnd
        , -- Its own process group, to stop cabal, GHC and node together, and
          -- to keep the terminal's Ctrl-C from reaching them directly.
          create_group = True
        }
  mapM_ (`hSetEncoding` utf8) [readEnd, input]
  hSetBuffering input LineBuffering
  -- Read once the modules are loaded, before anything else is sent.
  T.hPutStrLn input (":set prompt " <> T.pack (show (T.unpack promptMarker <> "\n")))
  session <- Session input process <$> newEmptyTMVarIO <*> newTQueueIO <*> newEmptyTMVarIO <*> newMVar () <*> newIORef False
  void (forkIO (readOutput session readEnd))
  pure session


readOutput :: Session -> Handle -> IO ()
readOutput session handle = loop []
 where
  loop lines' =
    try (T.hGetLine handle) >>= \case
      Left (_ :: IOException) -> atomically $ do
        void (tryPutTMVar session.originVar Nothing)
        void (tryPutTMVar session.exited ())
      Right line
        -- The prompt comes right after the last output, which may not end in
        -- a new line (the first prompt, "ghci> ", before ours took its place).
        | (before, marker) <- T.breakOn promptMarker line
        , not (T.null marker) -> do
            let lines'' = if T.null before || T.strip before == "ghci>" then lines' else before : lines'
            atomically (writeTQueue session.prompts (T.unlines (reverse lines'')))
            loop []
        | Just address <- parseOrigin line -> do
            atomically (void (tryPutTMVar session.originVar (Just address)))
            loop lines'
        -- The first prompt, before ours took its place.
        | T.strip line == "ghci>" -> loop lines'
        | otherwise -> do
            quiet <- readIORef session.stopping
            unless (quiet || isQuiet line) (Log.passThrough line)
            loop (line : lines')


-- | Stop the session: cabal, GHC and the node process that talks to the page.
stop :: Session -> IO ()
stop session = do
  writeIORef session.stopping True
  void (try @IOException (hClose session.input))
  getPid session.process >>= mapM_ \group -> do
    void (try @IOException (signalProcessGroup sigTERM group))
    finished <- timeout 3000000 (waitForProcess session.process)
    when (isNothing finished) $ do
      void (try @IOException (signalProcessGroup sigKILL group))
      void (waitForProcess session.process)


alive :: Session -> IO Bool
alive session = isNothing <$> getProcessExitCode session.process


{- | The origin to boot GHCi from (@http:\/\/127.0.0.1:port@), once GHCi has
said it. Nothing if it stopped before.
-}
origin :: Session -> IO (Maybe Text)
origin session = atomically (readTMVar session.originVar)


-- | Wait until the session has stopped.
exitedSTM :: Session -> STM ()
exitedSTM session = readTMVar session.exited


{- | Wait for the next prompt, and return what was printed before it. Nothing
if GHCi stopped.
-}
awaitPrompt :: Session -> IO (Maybe Text)
awaitPrompt session =
  atomically ((Just <$> readTQueue session.prompts) `orElse` (Nothing <$ readTMVar session.exited))


-- | Run a command, and return what it printed. Nothing if GHCi stopped.
command :: Session -> Text -> IO (Maybe Text)
command session line = withMVar session.lock $ \() -> do
  sent <- try (T.hPutStrLn session.input line)
  case sent of
    Left (_ :: IOException) -> pure Nothing
    Right () -> awaitPrompt session


-- READING GHCI'S OUTPUT

promptMarker :: Text
promptMarker = "<<<hastea-ghci-prompt>>>"


{- | The origin in GHCi's line @Open http:\/\/127.0.0.1:37517\/main.html or
import(...) to boot ghci@.
-}
parseOrigin :: Text -> Maybe Text
parseOrigin line = do
  rest <- T.stripPrefix "Open " (T.strip line)
  let url = T.takeWhile (/= ' ') rest
  address <- T.stripSuffix "/main.html" url
  if "http" `T.isPrefixOf` address then Just address else Nothing


data LoadResult = Loaded | LoadFailed
  deriving (Eq, Show)


-- | Whether loading or reloading modules worked, from what GHCi printed.
loadResult :: Text -> Maybe LoadResult
loadResult output = listToMaybe (mapMaybe result (reverse (T.lines output)))
 where
  result line
    | "Failed," `T.isPrefixOf` line = Just LoadFailed
    | "Ok," `T.isPrefixOf` line = Just Loaded
    | otherwise = Nothing


-- | Whether running an expression ended in an exception.
threwException :: Text -> Bool
threwException = ("*** Exception" `T.isInfixOf`)


{- | Lines not worth showing on the terminal: modules being loaded into the
interpreter, and the summary that @hastea dev@ replaces with its own.
-}
isQuiet :: Text -> Bool
isQuiet line =
  ("Compiling" `T.isInfixOf` line && ", interpreted )" `T.isInfixOf` line)
    || isJust (loadResult line)
