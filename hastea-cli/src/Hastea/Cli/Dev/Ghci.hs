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
  , complete

    -- * Reading GHCi's output
  , promptMarker
  , parseOrigin
  , LoadResult (..)
  , loadResult
  , threwException
  , isQuiet
  , parseCompletions
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (unless, void)
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
  , silent :: IORef Bool
  -- ^ Set while a command runs whose output is for the hastea command only.
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
      ( proc
          config.toolchain.cabal
          [ "repl"
          , "exe:" <> T.unpack app.executable
          , "--repl-options=-fghci-browser"
          , -- What the program in the page writes (results included) comes
            -- here, rather than to the console of the page.
            "--repl-options=-fghci-browser-redirect-wasi-console"
          ]
      )
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
  -- Read once the modules are loaded, before anything else is sent. Our
  -- prompt goes last, so that the first one GHCi shows marks the end of
  -- loading; the prompts before it are the plain "ghci> ".
  --
  -- For what is typed at the prompt only: `1 + 1` need not warn that it
  -- defaults to Integer, whatever warnings the package asks for.
  T.hPutStrLn input ":seti -Wno-type-defaults"
  T.hPutStrLn input (":set prompt " <> T.pack (show (T.unpack promptMarker <> "\n")))
  session <- Session input process <$> newEmptyTMVarIO <*> newTQueueIO <*> newEmptyTMVarIO <*> newMVar () <*> newIORef False <*> newIORef False
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
        -- a new line (the plain prompts, before ours took their place).
        | (before, marker) <- T.breakOn promptMarker line
        , not (T.null marker) -> do
            let lines'' = if plainPrompts before then lines' else before : lines'
            atomically (writeTQueue session.prompts (T.unlines (reverse lines'')))
            loop []
        | Just address <- parseOrigin line -> do
            atomically (void (tryPutTMVar session.originVar (Just address)))
            loop lines'
        | plainPrompts line -> loop lines'
        | otherwise -> do
            quiet <- (||) <$> readIORef session.stopping <*> readIORef session.silent
            unless (quiet || isQuiet line) (Log.passThrough line)
            loop (line : lines')


-- Nothing but GHCi's plain prompts, "ghci> ", before ours took their place.
plainPrompts :: Text -> Bool
plainPrompts = all (== "ghci>") . T.words


-- | Stop the session: cabal, GHC and the node process that talks to the page.
stop :: Session -> IO ()
stop session = do
  writeIORef session.stopping True
  void (try @IOException (hClose session.input))
  getPid session.process >>= mapM_ \group -> do
    void (try @IOException (signalProcessGroup sigTERM group))
    void (timeout 3000000 (waitForProcess session.process))
    -- GHC catches SIGTERM, and may go on waiting for the page after cabal
    -- has gone: whatever is left of the group goes too.
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


{- | What GHCi would complete the end of a line with: the part of the line
that stays, and the ways to finish it. Nothing if GHCi stopped or could not
say.
-}
complete :: Session -> String -> IO (Maybe (String, [String]))
complete session line = withMVar session.lock $ \() -> do
  writeIORef session.silent True
  sent <- try (T.hPutStrLn session.input (":complete repl 1-200 " <> T.pack (show line)))
  answer <- case sent of
    Left (_ :: IOException) -> pure Nothing
    Right () -> awaitPrompt session
  writeIORef session.silent False
  pure (answer >>= parseCompletions)


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


{- | The answer to @:complete repl@: a line with how many completions there
are and the part of the line they leave alone, then one completion a line,
each a Haskell string literal.

@
2 2 "map "
"mapM"
"mapM_"
@
-}
parseCompletions :: Text -> Maybe (String, [String])
parseCompletions output = case filter (not . T.null . T.strip) (T.lines output) of
  header : rest -> do
    (_ :: Int, afterShown) <- first (reads (T.unpack header))
    (_ :: Int, afterTotal) <- first (reads afterShown)
    (kept, _) <- first (reads afterTotal)
    candidates <- traverse (fmap fst . first . reads . T.unpack) rest
    pure (kept, candidates)
  [] -> Nothing
 where
  first = listToMaybe


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
