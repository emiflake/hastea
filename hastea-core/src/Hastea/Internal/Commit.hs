{-# LANGUAGE LambdaCase #-}

{- | The effectful half of rendering: carry out a 'Patch' against the DOM.

Calls from Haskell to JavaScript are what building a page costs most, so
they are kept few. Nodes to create are not made one by one: they are
described in a piece of text, and one call has JavaScript build them all.
Rearranging the children of a node is likewise a single call.

No event listeners are attached to the nodes created here. Events are
handled by "Hastea.Root", which only needs to be told which events occur in
the tree.
-}
module Hastea.Internal.Commit
  ( commit
  )
where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Builder.Int (decimal)

import Hastea.Html.Diff
import Hastea.Html.Internal
import Hastea.Internal.Foreign.DOM (DOMNode, Namespace, Nodes)
import Hastea.Internal.Foreign.DOM qualified as DOM


-- | Called with the name of each event that some element listens for.
type Listen = Text -> IO ()


{- | Apply a patch to a DOM node. Returns the node that is now in its place:
the same one, unless it had to be replaced.
-}
commit :: Listen -> DOMNode -> Patch msg -> IO DOMNode
commit listen node = \case
  Keep -> pure node
  Replace tree -> do
    namespace <- DOM.parentNode node >>= maybe (pure htmlNamespace) DOM.namespaceInside
    new <- create listen namespace [tree] >>= (`DOM.nodeAt` 0)
    DOM.replaceWith node new
    pure new
  SetText content -> node <$ DOM.setText node content
  Update changes children -> do
    forM_ changes $ \case
      Set attribute -> set node attribute
      Clear attribute -> clear node attribute
      Listen event -> listen event
    commitChildren listen node children
    pure node


commitChildren :: Listen -> DOMNode -> Children msg -> IO ()
commitChildren listen parent = \case
  InPlace patches ->
    forM_ (zip [0 ..] patches) $ \case
      -- Most children are usually unchanged: do not even look them up.
      (_, Keep) -> pure ()
      (index, patch) -> DOM.childAt parent index >>= mapM_ (\child -> commit listen child patch)
  Rearrange ops -> do
    olds <- DOM.snapshotChildren parent
    -- Bring the children that stay up to date, where they are.
    forM_ ops $ \case
      Reuse _ Keep -> pure ()
      Reuse index patch -> do
        old <- DOM.nodeAt olds index
        new <- commit listen old patch
        case patch of
          Replace _ -> DOM.setNodeAt olds index new
          _ -> pure ()
      Create _ -> pure ()
    namespace <- DOM.namespaceInside parent
    created <- create listen namespace [tree | Create tree <- ops]
    DOM.arrange parent olds created (order ops)
 where
  order ops = build (mconcat (zipWith (<>) ("" : repeat ",") (map position ops)))
  position (Reuse index _) = decimal index
  position (Create _) = "-1"


-- | Create the DOM nodes for some trees, in the namespace of the place they go in.
create :: Listen -> Namespace -> [Html msg] -> IO Nodes
create listen namespace trees = do
  script <- mconcat <$> mapM (describe listen) trees
  DOM.build namespace (build script)


build :: Builder -> Text
build = Lazy.toStrict . Builder.toLazyText


{- | Describe a tree for 'DOM.build', and report the events its elements
listen for.
-}
describe :: Listen -> Html msg -> IO Builder
describe listen tree = case unwrap tree of
  Element tag attributes children -> do
    described <- mapM attribute attributes
    inside <- mapM (describe listen) (flatten children)
    pure (record 'E' (Builder.fromText tag) <> mconcat described <> mconcat inside <> record 'C' mempty)
  Text content -> pure (record 'T' (plain content))
  -- 'unwrap' leaves only the two cases above.
  _ -> pure (record 'T' mempty)
 where
  attribute = \case
    Attribute key value -> pure (record 'A' (pair key value))
    BoolAttribute key True -> pure (record 'A' (pair key ""))
    BoolAttribute _ False -> pure mempty
    Property key (PropertyString value) -> pure (record 'S' (pair key value))
    Property key (PropertyBool value) -> pure (record 'B' (pair key (if value then "1" else "0")))
    Listener event _ _ -> mempty <$ listen event

  record letter content = Builder.singleton letter <> content <> Builder.singleton '\x1e'
  pair key value = plain key <> Builder.singleton '\x1f' <> plain value


{- | Text for a description. The two characters that structure a description
are control characters with no place in a page; should they occur in the
text, they are left out.
-}
plain :: Text -> Builder
plain content
  | T.any structural content = Builder.fromText (T.filter (not . structural) content)
  | otherwise = Builder.fromText content
 where
  structural c = c == '\x1e' || c == '\x1f'


htmlNamespace :: Namespace
htmlNamespace = "http://www.w3.org/1999/xhtml"


set :: DOMNode -> Attribute msg -> IO ()
set node = \case
  Attribute key value -> DOM.setAttribute node key value
  BoolAttribute key True -> DOM.setAttribute node key ""
  BoolAttribute key False -> DOM.removeAttribute node key
  Property key (PropertyString value) -> DOM.setProperty node key value
  Property key (PropertyBool value) -> DOM.setBoolProperty node key value
  Listener{} -> pure ()


clear :: DOMNode -> Attribute msg -> IO ()
clear node = \case
  Attribute key _ -> DOM.removeAttribute node key
  BoolAttribute key _ -> DOM.removeAttribute node key
  Property key (PropertyString _) -> DOM.setProperty node key ""
  Property key (PropertyBool _) -> DOM.setBoolProperty node key False
  Listener{} -> pure ()
