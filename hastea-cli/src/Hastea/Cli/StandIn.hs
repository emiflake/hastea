{- | Stand-ins for the JavaScript FFI, so that an ordinary GHC can typecheck
code written for the wasm backend.

An ordinary GHC rejects @foreign import javascript@, and HLS can only use an
ordinary GHC. The @hastea-ffi-stand-in@ program is a preprocessor (GHC's
@-F -pgmF@) that rewrites each such import into a definition of the same
name and type that fails when it is used:

@
foreign import javascript unsafe "alert($1)" ffi_alert :: JSString -> IO ()
@

becomes

@
ffi_alert = error "..."; ffi_alert :: JSString -> IO ()
@

and removes each @foreign export javascript@. Everything else is passed on
as it is, and every line stays on its own line, so that what the editor
reports is where you expect it. Only positions on the line of an import
itself shift.

The result typechecks, and that is all it is for: nothing in it can run.
-}
module Hastea.Cli.StandIn
  ( standIn
  )
where

import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import Data.Text qualified as T


-- | Rewrite a Haskell source file. See the top of this module.
standIn :: Text -> Text
standIn source = T.intercalate "\n" (go (T.splitOn "\n" source))
 where
  go [] = []
  go rows@(row : rest)
    | declares "import" row = case importName rows of
        -- The lines up to the name are emptied; the lines after it, if the
        -- type runs on, are passed on like any other.
        Just (before, name, signature) ->
          replicate before "" <> go ((name <> " = " <> failure <> "; " <> signature) : drop (before + 1) rows)
        -- Not what was expected: leave it for GHC to report.
        Nothing -> row : go rest
    | declares "export" row =
        let (more, others) = span continues rest
         in "" : map (const "") more <> go others
    | otherwise = row : go rest

  declares kind row = case T.stripPrefix ("foreign " <> kind <> " javascript") row of
    Just after -> T.null after || isSpace (T.head after)
    Nothing -> False

  -- A line that carries on the declaration before it.
  continues row = not (T.null (T.strip row)) && isSpace (T.head row)

  failure = "error \"This is a stand-in for a JavaScript import, which only exists when compiled to WebAssembly.\""


{- | Where the name of an import is: how many lines down from the start of
the declaration, the name, and the rest of that line from the name on.

@foreign import javascript [safety] "code" name :: type@, in which the code
may run over several lines.
-}
importName :: [Text] -> Maybe (Int, Text, Text)
importName rows = do
  afterKeyword <- T.stripPrefix "foreign import javascript" text
  afterCode <- T.stripPrefix "\"" (dropSafety (T.stripStart afterKeyword)) >>= endOfString
  let signature = T.stripStart afterCode
      name = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') signature
      before = T.count "\n" (T.take (T.length text - T.length signature) text)
  if T.null name || not ("::" `T.isPrefixOf` T.stripStart (T.drop (T.length name) signature))
    then Nothing
    else Just (before, name, T.takeWhile (/= '\n') signature)
 where
  text = T.intercalate "\n" rows
  dropSafety after = case [rest | word <- ["unsafe", "safe", "interruptible"], Just rest <- [T.stripPrefix word after], startsWithSpace rest] of
    rest : _ -> T.stripStart rest
    [] -> after
  startsWithSpace rest = maybe False (isSpace . fst) (T.uncons rest)


-- The text after the end of a string literal, given the text after its
-- opening quote.
endOfString :: Text -> Maybe Text
endOfString text = case T.uncons text of
  Nothing -> Nothing
  Just ('"', rest) -> Just rest
  Just ('\\', rest) -> case T.uncons rest of
    -- A gap: backslash, white space, backslash.
    Just (c, _) | isSpace c -> T.stripPrefix "\\" (T.dropWhile isSpace rest) >>= endOfString
    Just (_, rest') -> endOfString rest'
    Nothing -> Nothing
  Just (_, rest) -> endOfString rest
