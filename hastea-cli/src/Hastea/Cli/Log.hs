{- | What the hastea command says on the terminal.

Several threads print at once in @hastea dev@ (GHCi's output, the file
watcher, the web server), so every line goes through one lock.
-}
module Hastea.Cli.Log
  ( info
  , success
  , warn
  , problem
  , passThrough
  , dieWith
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Text (Text)
import Data.Text.IO qualified as T
import System.Exit (exitFailure)
import System.IO (Handle, hFlush, hIsTerminalDevice, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)


lock :: MVar ()
lock = unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}


say :: Handle -> Text -> Text -> IO ()
say handle code line =
  withMVar lock $ \() -> do
    -- Asked before writing: the handle is locked while it writes, and asking
    -- then, from inside the text being written, would wait forever.
    terminal <- hIsTerminalDevice handle
    T.hPutStrLn handle (if terminal && code /= "" then "\ESC[" <> code <> "m" <> line <> "\ESC[0m" else line)
    hFlush handle


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
