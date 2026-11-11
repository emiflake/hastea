-- | Checks of the parts of the hastea command that do not run anything.
module Main (main) where

import Control.Monad (unless)
import Data.Either (isLeft)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import System.Exit (exitFailure)

import Hastea.Cli.Config
import Hastea.Cli.Dev.Ghci
import Hastea.Cli.Dev.Page
import Hastea.Cli.Dev.Watch


type Suite = IORef (Int, [Text])


equal :: (Eq a, Show a) => Suite -> Text -> a -> a -> IO ()
equal suite name expected actual =
  modifyIORef' suite $ \(count, failures) ->
    if expected == actual
      then (count + 1, failures)
      else (count + 1, (name <> "\n    expected: " <> T.pack (show expected) <> "\n    actual:   " <> T.pack (show actual)) : failures)


main :: IO ()
main = do
  suite <- newIORef (0, [])
  mapM_ ($ suite) [config, page, ghci, watch]
  (count, failures) <- readIORef suite
  mapM_ (T.putStrLn . ("FAIL " <>)) (reverse failures)
  putStrLn (show (count - length failures) <> " of " <> show count <> " checks passed")
  unless (null failures) exitFailure


example :: Text
example =
  T.unlines
    [ "default: todomvc"
    , "apps:"
    , "  todomvc:"
    , "    executable: hastea-todomvc"
    , "    www: examples/todomvc/www"
    , "    run: Example.TodoMVC.TodoMVC.runTodoMVC"
    , "  benchmark:"
    , "    executable: hastea-benchmark"
    , "    optimize: true"
    , "dev:"
    , "  port: 9000"
    ]


decode :: Text -> Either Text Config
decode = decodeConfig "/project" . encodeUtf8


config :: Suite -> IO ()
config suite = do
  let decoded = decode example
  case decoded of
    Left failure -> equal suite "the example decodes" "" failure
    Right c -> do
      equal suite "root" "/project" c.root
      equal suite "names" ["benchmark", "todomvc"] (Map.keys c.apps)
      equal suite "default app" (Right "todomvc") ((.name) <$> selectApp c Nothing)
      equal suite "named app" (Right "benchmark") ((.name) <$> selectApp c (Just "benchmark"))
      equal suite "unknown app" True (isLeft (selectApp c (Just "nope")))
      let todomvc = c.apps Map.! "todomvc"
          benchmark = c.apps Map.! "benchmark"
      equal suite "www" (Just "examples/todomvc/www") todomvc.www
      equal suite "run" "Example.TodoMVC.TodoMVC.runTodoMVC" todomvc.run
      equal suite "default run" "main" benchmark.run
      equal suite "default out" "dist/benchmark" benchmark.out
      equal suite "optimize" True benchmark.optimize
      equal suite "default optimize" False todomvc.optimize
      equal suite "port" 9000 c.dev.port
      equal suite "default watch" ["."] c.dev.watch
      equal suite "default cabal" "wasm32-wasi-cabal" c.toolchain.cabal
  equal suite "a typo is reported" True (isLeft (decode "apps:\n  a:\n    executible: x\n"))
  equal suite "an unknown default is reported" True (isLeft (decode "default: b\napps:\n  a:\n    executable: x\n"))
  equal suite "no apps is reported" True (isLeft (decode "apps: {}\n"))
  equal suite "errors say where" (Left True) (either (Left . ("apps.a.executable" `T.isInfixOf`)) (const (Right ())) (decode "apps:\n  a:\n    executable: 3\n"))
  case decode "apps:\n  only:\n    executable: x\n" of
    Left failure -> equal suite "a single app decodes" "" failure
    Right c -> equal suite "a single app is the default" (Right "only") ((.name) <$> selectApp c Nothing)
  case decode "apps:\n  a:\n    executable: first\n  b:\n    executable: second\n" of
    Left failure -> equal suite "two apps decode" "" failure
    Right c -> equal suite "two apps and no default" True (isLeft (selectApp c Nothing))


page :: Suite -> IO ()
page suite = do
  let built =
        T.unlines
          [ "<html>"
          , "<head>"
          , "<link rel=\"stylesheet\" href=\"index.css\">"
          , "<link rel=\"preload\" href=\"hastea.wasm\" as=\"fetch\" type=\"application/wasm\" crossorigin>"
          , "<link rel=\"modulepreload\" href=\"browser_wasi_shim.js\">"
          , "<link rel=\"modulepreload\" href=\"ghc_wasm_jsffi.js\">"
          , "</head>"
          , "<body><div id=\"main-node\"></div>"
          , "<script type=\"module\" src=\"index.js\"></script>"
          , "</body>"
          , "</html>"
          ]
      served = devPage (Just built)
  equal suite "the loader is replaced" True (("src=\"" <> devScript <> "\"") `T.isInfixOf` served)
  equal suite "index.js is gone" False ("index.js" `T.isInfixOf` served)
  equal suite "preloads are gone" False (any (`T.isInfixOf` served) ["hastea.wasm", "ghc_wasm_jsffi", "browser_wasi_shim"])
  equal suite "styles stay" True ("index.css" `T.isInfixOf` served)
  let bare = devPage (Just "<html><body><p>hi</p></body></html>\n")
  equal suite "a page without a loader gets one" True (("<script type=\"module\" src=\"" <> devScript <> "\"></script>\n</body>") `T.isInfixOf` bare)
  equal suite "no page" True (("src=\"" <> devScript <> "\"") `T.isInfixOf` devPage Nothing && "main-node" `T.isInfixOf` devPage Nothing)
  equal suite "the default page loads index.js" True ("src=\"index.js\"" `T.isInfixOf` defaultPage)


ghci :: Suite -> IO ()
ghci suite = do
  equal suite "origin" (Just "http://127.0.0.1:37517") (parseOrigin "Open http://127.0.0.1:37517/main.html or import(\"http://127.0.0.1:37517/main.js\") to boot ghci")
  equal suite "not an origin" Nothing (parseOrigin "Ok, three modules loaded.")
  equal suite "loaded" (Just Loaded) (loadResult "[4 of 6] Compiling X\nOk, three modules loaded.\n")
  equal suite "reloaded" (Just Loaded) (loadResult "Ok, three modules reloaded.\n")
  equal suite "failed" (Just LoadFailed) (loadResult "X.hs:3:1: error:\n    oops\nFailed, two modules loaded.\n")
  equal suite "nothing loaded" Nothing (loadResult "")
  equal suite "exception" True (threwException "*** Exception: Prelude.head: empty list\n")
  equal suite "no exception" False (threwException "")
  equal suite "quiet compiling" True (isQuiet "[4 of 6] Compiling Example.TodoMVC.TodoMVC ( examples/todomvc/Example/TodoMVC/TodoMVC.hs, interpreted )[hastea-0.1.0.0-inplace-hastea-todomvc]")
  equal suite "completions" (Just ("map ", ["mapM", "mapM_"])) (parseCompletions "2 2 \"map \"\n\"mapM\"\n\"mapM_\"\n")
  equal suite "no completions" (Just ("", [])) (parseCompletions "0 0 \"\"\n")
  equal suite "completions with escapes" (Just (":t ", ["x\"y"])) (parseCompletions "1 1 \":t \"\n\"x\\\"y\"\n")
  equal suite "not completions" Nothing (parseCompletions "<interactive>: oops\n")
  equal suite "errors are not quiet" False (isQuiet "examples/todomvc/Main.hs:7:8: error: [GHC-88464]")


watch :: Suite -> IO ()
watch suite = do
  let app = App{name = "todomvc", executable = "hastea-todomvc", www = Just "examples/todomvc/www", out = "dist/todomvc", run = "main", optimize = False}
  equal suite "source" (Just SourceChanged) (classify app "examples/todomvc/Main.hs")
  equal suite "boot file" (Just SourceChanged) (classify app "src/A.hs-boot")
  equal suite "cabal file" (Just ProjectChanged) (classify app "hastea.cabal")
  equal suite "project" (Just ProjectChanged) (classify app "cabal.project")
  equal suite "hastea.yaml" (Just ProjectChanged) (classify app "hastea.yaml")
  equal suite "style" (Just StyleChanged) (classify app "examples/todomvc/www/index.css")
  equal suite "page" (Just PageChanged) (classify app "examples/todomvc/www/index.html")
  equal suite "other" Nothing (classify app "README.md")
  equal suite "a www lookalike" Nothing (classify app "examples/todomvc/wwwx/index.html")
