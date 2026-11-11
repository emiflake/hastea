-- | The HTML of a page: as built, and as served by @hastea dev@.
module Hastea.Cli.Dev.Page
  ( defaultPage
  , devPage
  , devScript
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T


{- | The page of an application that brings no @index.html@: an element with
id @main-node@, where 'Hastea.App.runApp' shows the view, and the loader.
-}
defaultPage :: Text
defaultPage =
  T.unlines
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\">"
    , "    <head>"
    , "        <meta charset=\"utf-8\">"
    , "        <!-- Start fetching everything at once, rather than each file when the one before asks for it. -->"
    , "        <link rel=\"preload\" href=\"hastea.wasm\" as=\"fetch\" type=\"application/wasm\" crossorigin>"
    , "        <link rel=\"modulepreload\" href=\"browser_wasi_shim.js\">"
    , "        <link rel=\"modulepreload\" href=\"ghc_wasm_jsffi.js\">"
    , "    </head>"
    , "    <body>"
    , "        <div id=\"main-node\"></div>"
    , "        <script type=\"module\" src=\"index.js\"></script>"
    , "    </body>"
    , "</html>"
    ]


-- | Where @hastea dev@ serves its loader.
devScript :: Text
devScript = "/__hastea/dev.js"


{- | The page as @hastea dev@ serves it: the application's own @index.html@
(or 'defaultPage'), loading 'devScript' instead of @index.js@, and without
the hints to preload files that only a built page has.
-}
devPage :: Maybe Text -> Text
devPage source = withLoader (T.unlines (filter (not . buildOnly) (T.lines (fromMaybe defaultPage source))))
 where
  buildOnly line = any (`T.isInfixOf` line) ["\"hastea.wasm\"", "\"ghc_wasm_jsffi.js\"", "\"browser_wasi_shim.js\""]
  withLoader page = case filter (`T.isInfixOf` page) ["src=\"index.js\"", "src=\"./index.js\""] of
    attribute : _ -> T.replace attribute ("src=\"" <> devScript <> "\"") page
    []
      | "</body>" `T.isInfixOf` page -> T.replace "</body>" (tag <> "\n</body>") page
      | otherwise -> page <> tag <> "\n"
  tag = "<script type=\"module\" src=\"" <> devScript <> "\"></script>"
