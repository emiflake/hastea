{- | What the test suites share: a minimal test harness, and helpers for
looking at trees and patches.
-}
module Support
  ( -- * Harness
    Suite
  , equal
  , runSuites

    -- * Helpers
  , Msg (..)
  , obj
  , listeners
  , fire
  , describe
  , outline
  )
where

import Control.Monad (unless)
import Data.IORef
import Data.List (intercalate)
import System.Exit (exitFailure)

import Hastea.Html
import Hastea.Html.Decode (Decoder, Value (..), decodeValue)
import Hastea.Html.Diff
import Hastea.Html.Events (EventOptions)
import Hastea.Html.Internal qualified as I
import Hastea.Html.Render (renderHtml)


-- A MINIMAL TEST HARNESS
--
-- Only depends on base, so that the suite runs the same natively and as a
-- wasm module under wasmtime.

type Suite = IORef (Int, [String])


-- | Record a check that passes when both values are equal.
equal :: (Eq a, Show a) => Suite -> String -> a -> a -> IO ()
equal suite name expected actual =
  modifyIORef' suite $ \(count, failures) ->
    if expected == actual
      then (count + 1, failures)
      else
        ( count + 1
        , (name <> "\n    expected: " <> show expected <> "\n    actual:   " <> show actual) : failures
        )


-- | Run groups of checks, report the failures, and exit accordingly.
runSuites :: [Suite -> IO ()] -> IO ()
runSuites groups = do
  suite <- newIORef (0, [])
  mapM_ ($ suite) groups
  (count, failures) <- readIORef suite
  mapM_ (putStrLn . ("FAIL: " <>)) (reverse failures)
  putStrLn (show (count - length failures) <> " of " <> show count <> " checks passed")
  unless (null failures) exitFailure


-- HELPERS

-- | A stand-in for a JavaScript object.
obj :: [(String, Value)] -> Value
obj = ObjectValue


-- | The listeners on an element, as (event name, options, decoder).
listeners :: Html msg -> [(String, EventOptions, Decoder msg)]
listeners el = case I.unwrap el of
  I.Element _ attributes _ -> [(e, o, d) | I.Listener e o d <- attributes]
  _ -> []


{- | Simulate the browser firing an event on an element: the messages
produced by every listener for that event.
-}
fire :: String -> Value -> Html msg -> [Either String msg]
fire event payload el = [decodeValue d payload | (e, _, d) <- listeners el, e == event]


-- | What a single attribute is, in terms that can be compared.
describe :: Attribute msg -> String
describe attribute = case attribute of
  I.Attribute k v -> "attribute " <> k <> "=" <> show v
  I.BoolAttribute k v -> "boolean attribute " <> k <> "=" <> show v
  I.Property k v -> "property " <> k <> "=" <> show v
  I.Listener e o _ -> "listener " <> e <> " " <> show o


-- | Messages for the trees in the tests.
data Msg
  = Clicked
  | Typed String
  | Checked Bool
  | Key String
  | Wrapped Msg
  deriving (Eq, Show)


-- | The outline of a patch, for comparing with what is expected.
outline :: Patch msg -> String
outline patch = case patch of
  Keep -> "keep"
  Replace tree -> "replace:" <> renderHtml tree
  SetText content -> "text:" <> content
  Update changes children ->
    "update{" <> intercalate "," (map changeOutline changes) <> "}" <> childrenOutline children
 where
  changeOutline (Set attribute) = "set " <> describe attribute
  changeOutline (Clear attribute) = "clear " <> describe attribute
  changeOutline (Listen event) = "listen " <> event

  childrenOutline (InPlace patches) = "[" <> intercalate "," (map outline patches) <> "]"
  childrenOutline (Rearrange ops) = "<" <> intercalate "," (map opOutline ops) <> ">"

  opOutline (Reuse index childPatch) = show index <> ":" <> outline childPatch
  opOutline (Create tree) = "new:" <> renderHtml tree
