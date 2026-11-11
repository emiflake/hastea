{- | Keeping the state of the components in a tree.

Everything here is pure. A 'Store' holds the state of every component that
is in the view; 'expand' uses it to turn a view with components into an
ordinary tree; 'deliver' gives a message to the component it is for. What
remains for "Hastea.Root" is to hold the store and call these.
-}
module Hastea.Component.Internal
  ( -- * Components in a tree
    Mount (..)
  , Embedded (..)
  , Effect (..)
  , embed

    -- * Keeping their state
  , Routed (..)
  , Path
  , Segment (..)
  , Store
  , emptyStore
  , Expansion (..)
  , expand
  , deliver
  , subscriptionsOf
  , unmountAll
  )
where

import Data.Bifunctor (second)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)

import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Html.Internal
import Hastea.Internal.Sub (Sub, prefixed)


-- | A component placed in a tree, with its state and message types hidden.
data Mount msg
  = forall state inner. (Typeable state, Typeable inner) => Mount (Embedded state inner msg)


-- | Only what the component tells its surroundings is changed: its own
-- messages stay inside.
instance Functor Mount where
  fmap f (Mount embedded) =
    Mount
      Embedded
        { initial = embedded.initial
        , view = embedded.view
        , update = \msg state -> second (map (fmap f)) (embedded.update msg state)
        , subscriptions = embedded.subscriptions
        , onMount = map (fmap f) embedded.onMount
        , onUnmount = map (fmap f) . embedded.onUnmount
        }


{- | A component as it sits in a tree of messages @msg@: its own state is
@state@ and its own messages are @inner@.
-}
data Embedded state inner msg = Embedded
  { initial :: state
  -- ^ The state to start with.
  , view :: state -> Html inner
  , update :: inner -> state -> (state, [Effect inner msg])
  , subscriptions :: state -> Sub inner
  , onMount :: [Effect inner msg]
  , onUnmount :: state -> [Effect inner msg]
  }


-- | Something a component's @update@ asks for besides a new state.
data Effect msg out
  = -- | Run a command. Its messages come back to the component.
    Run (Cmd msg)
  | -- | Tell whoever uses the component that something happened.
    Emit out


instance Functor (Effect msg) where
  fmap _ (Run cmd) = Run cmd
  fmap f (Emit out) = Emit (f out)


{- | A component as a node of a tree. "Hastea.Html" does not know about
components, so where nothing keeps their state (in "Hastea.Html.Render",
for example) the node is shown as the component's initial view, without its
event handlers.
-}
embed :: (Typeable state, Typeable inner) => Embedded state inner msg -> Html msg
embed embedded = Extend (Extension (Mount embedded)) (inert (embedded.view embedded.initial))
 where
  inert :: Html a -> Html b
  inert (Element tag attributes children) = Element tag (mapMaybe plain attributes) (map inert children)
  inert (Text content) = Text content
  inert (Lazy _ tree) = inert tree
  inert (Keyed key tree) = Keyed key (inert tree)
  inert (Fragment trees) = Fragment (map inert trees)
  inert (Extend _ fallback) = inert fallback

  plain :: Attribute a -> Maybe (Attribute b)
  plain (Attribute key value) = Just (Attribute key value)
  plain (BoolAttribute key value) = Just (BoolAttribute key value)
  plain (Property key value) = Just (Property key value)
  plain Listener{} = Nothing


{- | A message from an expanded tree: either one for whoever shows the tree,
or one for the component at some place in it.
-}
data Routed msg
  = Out msg
  | Local Path Dynamic


-- | A place in a tree, from the root down.
type Path = [Segment]


{- | One step down a tree: from an element with the given tag to one of its
children. Children are told apart the way "Hastea.Html.Diff" matches them,
so a node that the diff keeps is in the same place here, and a node that it
replaces (because the tag is different) is not.
-}
data Segment
  = -- | The child with this key.
    Named Text Text
  | -- | The child at this position, counting only children without a key.
    At Text Int
  | -- | From a component to its view.
    Inside
  deriving (Eq, Ord, Show)


{- | What is known about one place in the tree and the places below it. The
store is shaped like the tree, so that the part belonging to an unchanged
subtree is itself unchanged.
-}
data Store msg = Store
  { state :: Maybe Dynamic
  -- ^ The state of the component here.
  , mounted :: Maybe (Mounted msg)
  -- ^ The component here.
  , lazyRefs :: Maybe [Ref]
  -- ^ What the lazy node here was computed from, if nothing below it has
  -- changed since.
  , children :: Map Segment (Store msg)
  }


-- A component in the tree, with its types hidden.
data Mounted msg = Mounted
  { handle :: Dynamic -> Maybe Dynamic -> Maybe (Dynamic, Cmd (Routed msg), Sub (Routed msg))
  -- ^ Given a message and the current state: the next state, what else
  -- should happen, and the subscriptions in the next state. 'Nothing' if the
  -- message is not of the component's type.
  , sub :: Sub (Routed msg)
  -- ^ The subscriptions in the current state.
  , unmount :: Maybe Dynamic -> Cmd (Routed msg)
  -- ^ Given the last state: what the component asks for as it goes away.
  }


-- | No component has a state yet.
emptyStore :: Store msg
emptyStore = Store{state = Nothing, mounted = Nothing, lazyRefs = Nothing, children = Map.empty}


isEmpty :: Store msg -> Bool
isEmpty store =
  null store.state && null store.mounted && null store.lazyRefs && Map.null store.children


childStore :: Segment -> Store msg -> Store msg
childStore segment store = fromMaybe emptyStore (Map.lookup segment store.children)


-- | What 'expand' makes of a tree.
data Expansion msg = Expansion
  { tree :: Html (Routed msg)
  -- ^ The tree with every component replaced by its view.
  , store :: Store msg
  -- ^ The store to use from now on.
  , effects :: Cmd (Routed msg)
  -- ^ What the components that appeared or went away ask for.
  , warnings :: [Text]
  -- ^ Likely mistakes in the tree.
  }


{- | Replace every component in a tree by its view in its current state.

In the new store, components that appear for the first time have their
initial state, and components that are no longer in the tree have none.

A lazy subtree is not looked at if neither what it was computed from nor
the state of any component in it has changed.
-}
expand :: Html msg -> Store msg -> Expansion msg
expand = go Out []


{- The first argument says how a message of the tree being expanded reaches
whoever shows the whole tree. The path is kept in reverse.

Whatever case applies has to account for all of the old store: the
components in the parts it does not carry over have gone away.
-}
go :: forall m top. (m -> Routed top) -> Path -> Html m -> Store top -> Expansion top
go send path tree store = case tree of
  Text content ->
    Expansion{tree = Text content, store = emptyStore, effects = unmountAll store, warnings = []}
  Keyed key inner ->
    let inside = go send path inner store
     in Expansion{tree = Keyed key inside.tree, store = inside.store, effects = inside.effects, warnings = inside.warnings}
  Lazy refs inner ->
    let inside = go send path inner store
        -- The store is part of what the expanded tree is computed from.
        store' = inside.store{lazyRefs = Just refs}
     in case store.lazyRefs of
          Just oldRefs
            | sameRefs oldRefs refs ->
                Expansion{tree = Lazy (Ref store : refs) inside.tree, store, effects = mempty, warnings = []}
          _ ->
            Expansion
              { tree = Lazy (Ref store' : refs) inside.tree
              , store = store'
              , effects = inside.effects
              , warnings = [fragmentInLazy | isFragment inner] <> inside.warnings
              }
  Element tag attributes children ->
    let below = contents tag children
     in Expansion{tree = Element tag (map (fmap send) attributes) (flatten below.trees), store = below.store, effects = below.effects, warnings = below.warnings}
  Fragment children ->
    let below = contents "" children
     in Expansion{tree = Fragment below.trees, store = below.store, effects = below.effects, warnings = below.warnings}
  -- An extension of some other kind is shown as what it falls back to.
  Extend extension fallback -> case fromExtension extension of
    Nothing -> go send path fallback store
    Just mount -> component mount
 where
  -- The children of an element or a fragment.
  contents :: Text -> [Html m] -> Contents top
  contents tag children =
    let segments = snd (mapAccumL (segment tag) 0 children)
        results = [(key, go send (key : path) child (childStore key store)) | (key, child) <- zip segments children]
        keys = [key | Named _ key <- segments]
        gone = store.children `Map.withoutKeys` Set.fromList segments
     in Contents
          { trees = [result.tree | (_, result) <- results]
          , store = emptyStore{children = Map.fromList [(key, result.store) | (key, result) <- results, not (isEmpty result.store)]}
          , effects = unmountHere store <> foldMap unmountAll gone <> foldMap (\(_, result) -> result.effects) results
          , warnings =
              [duplicateKey tag key | (key, count) <- Map.toList (Map.fromListWith (+) [(key, 1 :: Int) | key <- keys]), count > 1]
                <> concatMap (\(_, result) -> result.warnings) results
          }

  segment :: Text -> Int -> Html m -> (Int, Segment)
  segment tag unkeyed (Keyed key _) = (unkeyed, Named tag key)
  segment tag unkeyed _ = (unkeyed + 1, At tag unkeyed)

  component :: Mount m -> Expansion top
  component (Mount embedded) =
    let here = reverse path
        -- A state of another type belongs to a component that was here before.
        current = store.state >>= \stored -> (,) stored <$> fromDynamic stored
        (dynamic, state) = fromMaybe (toDyn embedded.initial, embedded.initial) current
        -- What was here before a component that is new has gone away.
        (before, appearing) = case current of
          Just _ -> (store{mounted = Nothing, children = Map.delete Inside store.children}, [])
          Nothing -> (store, embedded.onMount)
        inside =
          go sendInner (Inside : path) (embedded.view state) $
            if null current then emptyStore else childStore Inside store
        sendInner inner = Local here (toDyn inner)
        toCmd (Run cmd) = sendInner <$> cmd
        toCmd (Emit out) = Cmd.perform (pure (send out))
        handle message stored = do
          inner <- fromDynamic message
          let (next, asked) = embedded.update inner (fromMaybe embedded.initial (stored >>= fromDynamic))
          -- A state that cannot be computed fails here, not when it is next used.
          next `seq` pure (toDyn next, foldMap toCmd asked, subscriptionsIn next)
        unmount stored = foldMap toCmd (embedded.onUnmount (fromMaybe embedded.initial (stored >>= fromDynamic)))
        -- Named after the place, to tell apart the subscriptions of several
        -- uses of one component.
        subscriptionsIn at = prefixed (T.pack (show here) <> " ") (sendInner <$> embedded.subscriptions at)
     in Expansion
          { tree = inside.tree
          , store =
              Store
                { state = Just dynamic
                , mounted = Just Mounted{handle, unmount, sub = subscriptionsIn state}
                , lazyRefs = Nothing
                , children = Map.fromList [(Inside, inside.store) | not (isEmpty inside.store)]
                }
          , effects = unmountAll before <> foldMap toCmd appearing <> inside.effects
          , warnings = inside.warnings
          }

  isFragment (Keyed _ inner) = isFragment inner
  isFragment (Fragment _) = True
  isFragment _ = False


-- What 'go' makes of the children of a node.
data Contents msg = Contents
  { trees :: [Html (Routed msg)]
  , store :: Store msg
  , effects :: Cmd (Routed msg)
  , warnings :: [Text]
  }


-- What the component at a place asks for when it goes away.
unmountHere :: Store msg -> Cmd (Routed msg)
unmountHere store = foldMap (\gone -> gone.unmount store.state) store.mounted


-- | What the components at a place and everything below it ask for when
-- they go away.
unmountAll :: Store msg -> Cmd (Routed msg)
unmountAll store = unmountHere store <> foldMap unmountAll store.children


duplicateKey :: Text -> Text -> Text
duplicateKey tag key =
  "Two children of <" <> tag <> "> have the key " <> T.pack (show key)
    <> ". Keys must be unique among siblings: one of the two is treated as new each time, and components in them share their state."


fragmentInLazy :: Text
fragmentInLazy =
  "A fragment is directly inside lazy, so only its first node is shown. Put lazy around each node instead, or an element around the fragment."


{- | Give a message to the component at a path. Returns the store with the
component's next state, and what else should happen. 'Nothing' if there is
no such component any more.
-}
deliver :: Path -> Dynamic -> Store msg -> Maybe (Store msg, Cmd (Routed msg))
deliver [] message store = do
  component <- store.mounted
  (next, cmd, sub) <- component.handle message store.state
  pure
    ( store{state = Just next, mounted = Just Mounted{handle = component.handle, unmount = component.unmount, sub}, lazyRefs = Nothing}
    , cmd
    )
deliver (segment : rest) message store = do
  child <- Map.lookup segment store.children
  (child', cmd) <- deliver rest message child
  -- Whatever was computed from this part of the store is now out of date.
  pure (store{children = Map.insert segment child' store.children, lazyRefs = Nothing}, cmd)


-- | The subscriptions of all components in the tree.
subscriptionsOf :: Store msg -> Sub (Routed msg)
subscriptionsOf store =
  foldMap (.sub) store.mounted <> foldMap subscriptionsOf store.children
