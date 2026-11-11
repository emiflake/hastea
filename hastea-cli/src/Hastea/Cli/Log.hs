{- | What the hastea command says on the terminal.

Several threads print at once in @hastea dev@ (GHCi's output, the file
watcher, the web server), so every line goes through one lock. While the
prompt of @hastea dev@ waits for input, lines go to haskeline instead, which
prints them above the prompt.
-}
module Hastea.Cli.Log
  ( info
  , success
  , warn
  , problem
  , passThrough
  , dieWith
  , setPrinter
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Exit (exitFailure)
import System.IO (Handle, hFlush, hIsTerminalDevice, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)


lock :: MVar ()
lock = unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}


printer :: IORef (Maybe (String -> IO ()))
printer = unsafePerformIO (newIORef Nothing)
{-# NOINLINE printer #-}


-- | Hand lines to this function instead of writing them, or stop doing so.
setPrinter :: Maybe (String -> IO ()) -> IO ()
setPrinter = atomicWriteIORef printer


say :: Handle -> Text -> Text -> IO ()
say handle code line =
  withMVar lock $ \() -> do
    -- Asked before writing: the handle is locked while it writes, and asking
    -- then, from inside the text being written, would wait forever.
    terminal <- hIsTerminalDevice handle
    let shown = if terminal && code /= "" then "\ESC[" <> code <> "m" <> line <> "\ESC[0m" else line
    readIORef printer >>= \case
      Just external -> external (T.unpack shown <> "\n")
      Nothing -> T.hPutStrLn handle shown >> hFlush handle


-- | Something worth knowing.
info :: Text -> IO ()
info = say stdout "36"


-- | Something that worked.
success :: Text -> IO ()
success = say stdout "32"


-- | Something that may be a mistake.
warn :: Text -> IO ()
warn = say stderr "33"


-- | Something that went wrong.
problem :: Text -> IO ()
problem = say stderr "31"


-- | A line from another program, as it was.
passThrough :: Text -> IO ()
passThrough = say stdout ""


-- | Say what went wrong, and stop.
dieWith :: Text -> IO a
dieWith message = problem message >> exitFailure
