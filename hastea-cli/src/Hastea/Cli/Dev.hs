{- | @hastea dev@: run an application in GHCi's browser mode, and reload it
in the page whenever its source changes.

The hastea command serves the page. The page asks it for a GHCi session,
and boots GHCi's interpreter in itself from the address it gets back. Once
the modules are loaded, the hastea command runs the application's @run@
expression (@main@ unless hastea.yaml says otherwise). From then on:

* a change to Haskell source is loaded with @:reload@, and @run@ runs again,
  which replaces the application on the page ("Hastea.Root" unmounts
  whatever was mounted in the same place);
* a change to a stylesheet replaces it in the page;
* a change to any other file of the page, or to the cabal package, loads
  the page again, with a new GHCi session.

Compilation errors are shown on the terminal and over the page, until the
next reload that works.

When it runs in a terminal, @hastea dev@ also gives a GHCi prompt, for the
session of the page: expressions are evaluated in the page, and what they
print comes back to the terminal. @:reload@ (or @:r@) reloads and runs the
application, like a change to a file; @:quit@ (or Ctrl-D) stops.
-}
module Hastea.Cli.Dev
  ( DevOptions (..)
  , dev
  )
where

import Control.Concurrent (forkIO, myThreadId, threadDelay, throwTo)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (AsyncException (UserInterrupt), catch, finally, throwIO)
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isSpace)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import Numeric (showFFloat)
import System.Console.Haskeline qualified as Haskeline
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath (joinPath, splitSearchPath, takeExtension, (</>))
import System.IO (hIsTerminalDevice, stdin, stdout)
import System.IO.Error (isAlreadyInUseError)
import System.Posix.Signals (Handler (Catch), installHandler, sigHUP, sigTERM)
import System.Timeout (timeout)

import Hastea.Cli.Config
import Hastea.Cli.Dev.Ghci (Session)
import Hastea.Cli.Dev.Ghci qualified as Ghci
import Hastea.Cli.Dev.Page
import Hastea.Cli.Dev.Watch
import Hastea.Cli.Log qualified as Log
import Hastea.Cli.Runtime qualified as Runtime
import Hastea.Cli.Toolchain (requireTools)


newtype DevOptions = DevOptions
  { port :: Maybe Int
  }


-- | Something to tell the page: its name, and what goes with it.
data Event = Event Text Aeson.Value


-- | A GHCi session, whether a page is using it yet, and whether its modules
-- have been loaded there.
data Current = Current
  { session :: Session
  , attached :: IORef Bool
  , ready :: IORef Bool
  }


data Dev = Dev
  { config :: Config
  , app :: App
  , current :: MVar (Maybe Current)
  , events :: TChan Event
  , status :: TVar (Maybe Event)
  -- ^ Whether the last load worked, for pages that connect later.
  , dirty :: TVar Bool
  -- ^ Whether source changed since the last reload.
  }


dev :: Config -> App -> DevOptions -> IO ()
dev config app options = do
  requireTools [config.toolchain.cabal, config.toolchain.ghc, config.toolchain.node]
  -- Being killed, or losing the terminal, stops everything as Ctrl-C does.
  -- Otherwise GHCi, in a process group of its own, would be left running.
  main <- myThreadId
  forM_ [sigTERM, sigHUP] $ \signal -> installHandler signal (Catch (throwTo main UserInterrupt)) Nothing
  checkWs
  state <- Dev config app <$> newMVar Nothing <*> newBroadcastTChanIO <*> newTVarIO Nothing <*> newTVarIO False
  serving <- newEmptyMVar
  let port = fromMaybe config.dev.port options.port
      -- Once the port is ours: start a session before any page asks, so
      -- that the first load is quick, and watch for changes.
      ready = do
        Log.success ("hastea dev: " <> app.name <> " on http://localhost:" <> T.pack (show port))
        void (restart state False)
        void (forkIO (watchFiles config app (changed state)))
        putMVar serving Nothing
      settings = Warp.setPort port (Warp.setHost "127.0.0.1" (Warp.setBeforeMainLoop ready Warp.defaultSettings))
  -- The server runs alongside; this thread keeps the terminal.
  void . forkIO $
    Warp.runSettings settings (application state) `catch` \failure ->
      tryPutMVar serving (Just failure) >>= \first -> unless first (Log.problem ("The web server stopped: " <> T.pack (show failure)))
  ( takeMVar serving >>= \case
      Just failure
        | isAlreadyInUseError failure ->
            Log.dieWith ("Port " <> T.pack (show port) <> " is already in use. Choose another with --port, or with dev.port in hastea.yaml.")
        | otherwise -> throwIO failure
      Nothing -> do
        interactive <- (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stdout
        if interactive then prompt state else forever (threadDelay maxBound)
    )
    `finally` do
      Log.setPrinter Nothing
      readMVar state.current >>= mapM_ (Ghci.stop . (.session))


-- GHCi's browser mode needs the ws package from node.
checkWs :: IO ()
checkWs = do
  folders <- maybe [] splitSearchPath <$> lookupEnv "NODE_PATH"
  found <- or <$> mapM (\folder -> doesDirectoryExist (folder </> "ws")) folders
  unless found $
    Log.warn "GHCi's browser mode needs the ws package for node, which ghc-wasm-meta provides; NODE_PATH does not lead to it."


broadcast :: Dev -> Event -> IO ()
broadcast state event@(Event name _) = atomically $ do
  writeTChan state.events event
  when (name `elem` ["ok", "failed"]) (writeTVar state.status (Just event))


-- Stop the current session, if any, and start another.
restart :: Dev -> Bool -> IO Current
restart state attach = modifyMVar state.current $ \existing -> do
  mapM_ (Ghci.stop . (.session)) existing
  fresh <- Current <$> Ghci.start state.config state.app <*> newIORef attach <*> newIORef False
  pure (Just fresh, fresh)


-- A page asks for a session: an unused one, or a new one.
acquire :: Dev -> IO (Maybe Text)
acquire state = do
  reusable <- withMVar state.current $ \case
    Just existing -> do
      used <- readIORef existing.attached
      running <- Ghci.alive existing.session
      if used || not running then pure Nothing else Just existing <$ writeIORef existing.attached True
    Nothing -> pure Nothing
  chosen <- maybe (restart state True) pure reusable
  atomically (writeTVar state.status Nothing)
  address <- Ghci.origin chosen.session
  forM_ address $ \_ -> do
    Log.info "A page connected; loading the modules in it"
    void (forkIO (follow state chosen))
  pure address


-- Load the application in a session, and reload it whenever source changes.
follow :: Dev -> Current -> IO ()
follow state chosen = do
  began <- getCurrentTime
  Ghci.awaitPrompt session >>= mapM_ \output -> do
    writeIORef chosen.ready True
    loaded state session began output
    loop
 where
  session = chosen.session
  -- Wait for a change, unless the session stops first: then the change is
  -- left for the session that replaced it.
  loop = do
    changedSince <-
      atomically $
        (False <$ Ghci.exitedSTM session)
          `orElse` (True <$ (readTVar state.dirty >>= check >> writeTVar state.dirty False))
    when changedSince $ do
      began <- getCurrentTime
      Ghci.command session ":reload" >>= mapM_ \output -> do
        loaded state session began output
        loop


-- After loading: run the application if that worked, and say how it went.
loaded :: Dev -> Session -> UTCTime -> Text -> IO ()
loaded state session began output = case Ghci.loadResult output of
  Just Ghci.LoadFailed -> failed "Compilation failed" output
  _ ->
    Ghci.command session state.app.run >>= mapM_ \ran ->
      if Ghci.threwException ran
        then failed ("Running " <> state.app.run <> " failed") ran
        else do
          finished <- getCurrentTime
          let seconds = realToFrac (diffUTCTime finished began) :: Double
          Log.success ("Loaded in " <> T.pack (showFFloat (Just 2) seconds "") <> " s")
          broadcast state (Event "ok" Aeson.Null)
 where
  failed summary details = do
    Log.problem summary
    -- The same lines as on the terminal.
    let shown = T.strip (T.unlines (filter (not . Ghci.isQuiet) (T.lines (stripAnsi details))))
    broadcast state (Event "failed" (Aeson.object ["summary" .= summary, "details" .= shown]))


changed :: Dev -> [(FilePath, Change)] -> IO ()
changed state changes = do
  let kinds = map snd changes
  if ProjectChanged `elem` kinds
    then do
      Log.info "The project changed; starting GHCi afresh"
      void (restart state False)
      broadcast state (Event "reload" Aeson.Null)
    else do
      when (SourceChanged `elem` kinds) (atomically (writeTVar state.dirty True))
      if PageChanged `elem` kinds
        then broadcast state (Event "reload" Aeson.Null)
        else when (StyleChanged `elem` kinds) (broadcast state (Event "css" Aeson.Null))


-- THE PROMPT

-- Read GHCi commands from the terminal until told to stop.
prompt :: Dev -> IO ()
prompt state = do
  history <- getXdgDirectory XdgCache "hastea"
  createDirectoryIfMissing True history
  let settings =
        Haskeline.setComplete (completion state) $
          Haskeline.defaultSettings{Haskeline.historyFile = Just (history </> "ghci_history")}
  Haskeline.runInputT settings $ do
    external <- Haskeline.getExternalPrint
    liftIO (Log.info "GHCi runs in the page once it is open. Type commands here; :r reloads, :q quits.")
    let loop = do
          -- Lines printed meanwhile go above the prompt.
          liftIO (Log.setPrinter (Just external))
          line <- Haskeline.getInputLine "ghci> "
          liftIO (Log.setPrinter Nothing)
          case trim <$> line of
            Nothing -> pure ()
            Just command
              | command `elem` [":q", ":quit"] -> pure ()
              | null command -> loop
              | command `elem` [":r", ":reload"] -> liftIO (atomically (writeTVar state.dirty True)) >> loop
              | otherwise -> liftIO (send (T.pack command)) >> loop
    loop
 where
  trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
  send command =
    readySession state >>= \case
      Just session -> void (Ghci.command session command)
      Nothing -> Log.warn "GHCi is not ready: open the page, and wait for it to load."


-- Complete what is typed, as GHCi would.
completion :: Dev -> Haskeline.CompletionFunc IO
completion state (before, _) =
  readySession state >>= \case
    Nothing -> pure (before, [])
    Just session ->
      Ghci.complete session (reverse before) >>= \case
        Nothing -> pure (before, [])
        Just (kept, candidates) -> pure (reverse kept, map Haskeline.simpleCompletion candidates)


-- The session of the page, once its modules are loaded there.
readySession :: Dev -> IO (Maybe Session)
readySession state =
  withMVar state.current $ \case
    Just chosen -> do
      usable <- (&&) <$> readIORef chosen.attached <*> readIORef chosen.ready
      pure (if usable then Just chosen.session else Nothing)
    Nothing -> pure Nothing


-- THE WEB SERVER

application :: Dev -> Application
application state request respond = case (requestMethod request, pathInfo request) of
  ("GET", []) -> page
  ("GET", ["index.html"]) -> page
  ("GET", ["__hastea", "dev.js"]) -> respond (responseLBS status200 (headers "text/javascript") (LBS.fromStrict Runtime.devLoader))
  ("POST", ["__hastea", "session"]) ->
    acquire state >>= \case
      Just address -> respond (responseLBS status200 (headers "application/json") (Aeson.encode (Aeson.object ["origin" .= address])))
      Nothing -> respond (responseLBS status503 (headers "text/plain") "GHCi stopped before it was ready; see the terminal.")
  ("GET", ["__hastea", "events"]) -> respond (eventStream state)
  ("GET", segments) -> static segments
  _ -> notFound
 where
  page = do
    source <- maybe (pure Nothing) (readIfThere . (</> "index.html") . (state.config.root </>)) state.app.www
    respond (responseLBS status200 (headers "text/html; charset=utf-8") (LBS.fromStrict (encodeUtf8 (devPage source))))
  static segments
    | Just www <- state.app.www
    , all safe segments = do
        let path = state.config.root </> www </> joinPath (map T.unpack segments)
        exists <- doesFileExist path
        if exists then respond (responseFile status200 (headers (mimeType path)) path Nothing) else notFound
    | otherwise = notFound
  safe segment = not (T.null segment) && not ("." `T.isPrefixOf` segment) && T.all (/= '\\') segment
  notFound = respond (responseLBS status404 (headers "text/plain") "Not found")
  headers contentType = [(hContentType, contentType), (hCacheControl, "no-store")]


readIfThere :: FilePath -> IO (Maybe Text)
readIfThere path = do
  exists <- doesFileExist path
  if exists then Just . decodeUtf8Lenient <$> BS.readFile path else pure Nothing


-- Server-sent events: what the page should do, as it happens.
eventStream :: Dev -> Response
eventStream state = responseStream status200 [(hContentType, "text/event-stream"), (hCacheControl, "no-store")] $ \write flush -> do
  chan <- atomically (dupTChan state.events)
  let send (Event name payload) =
        write (Builder.byteString ("event: " <> encodeUtf8 name <> "\ndata: ") <> Builder.lazyByteString (Aeson.encode payload) <> "\n\n") >> flush
  write ": connected\n\n" >> flush
  readTVarIO state.status >>= mapM_ send
  forever $
    -- Say something now and then, so that the connection is not closed as idle.
    timeout 15000000 (atomically (readTChan chan)) >>= \case
      Just event -> send event
      Nothing -> write ": still here\n\n" >> flush


mimeType :: FilePath -> BS.ByteString
mimeType path = fromMaybe "application/octet-stream" (lookup (takeExtension path) types)
 where
  types =
    [ (".html", "text/html; charset=utf-8")
    , (".js", "text/javascript")
    , (".mjs", "text/javascript")
    , (".css", "text/css")
    , (".json", "application/json")
    , (".txt", "text/plain; charset=utf-8")
    , (".svg", "image/svg+xml")
    , (".png", "image/png")
    , (".jpg", "image/jpeg")
    , (".jpeg", "image/jpeg")
    , (".gif", "image/gif")
    , (".webp", "image/webp")
    , (".ico", "image/x-icon")
    , (".woff2", "font/woff2")
    , (".wasm", "application/wasm")
    ]


-- Terminal colour codes mean nothing in the page.
stripAnsi :: Text -> Text
stripAnsi text = case T.breakOn "\ESC[" text of
  (before, "") -> before
  (before, rest) -> before <> stripAnsi (T.drop 1 (T.dropWhile (\c -> not (c `elem` ['A' .. 'Z'] || c `elem` ['a' .. 'z'])) (T.drop 2 rest)))

