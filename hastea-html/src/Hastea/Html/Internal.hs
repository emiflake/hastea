{-# LANGUAGE MagicHash #-}

{- | The data types behind "Hastea.Html".

This module exposes the constructors of 'Html' and 'Attribute' for code that
needs to interpret a tree, such as a virtual DOM or a renderer. Application
code should build trees with the functions in "Hastea.Html",
"Hastea.Html.Attributes" and "Hastea.Html.Events" instead.
-}
module Hastea.Html.Internal
  ( Html (..)
  , Ref (..)
  , sameRefs
  , unwrap
  , Mount (..)
  , Embedded (..)
  , Effect (..)
  , static
  , Attribute (..)
  , PropertyValue (..)
  , EventOptions (..)
  , defaultEventOptions
  , attributeKey
  )
where

import Data.Bifunctor (second)
import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import Unsafe.Coerce (unsafeCoerce)

import Hastea.Cmd (Cmd)
import Hastea.Html.Decode (Decoder)
import Hastea.Sub (Sub)


-- | A tree of HTML that can produce messages of type @msg@.
data Html msg
  = -- | An element with a tag name, attributes and children.
    Element String [Attribute msg] [Html msg]
  | -- | A text node.
    Text String
  | {- | A subtree that is only computed when needed. The references identify
    what it was computed from: if they are the same as last time, so is the
    subtree, and it is neither computed nor diffed. See 'Hastea.Html.lazy'.
    -}
    Lazy [Ref] (Html msg)
  | {- | A node with an identity among its siblings, so that it is moved
    rather than rebuilt when a list is reordered. See 'Hastea.Html.keyed'.
    -}
    Keyed String (Html msg)
  | {- | A component with state of its own, which is kept by whatever shows
    the tree. See "Hastea.Component".
    -}
    Embed (Mount msg)
  deriving (Show)


instance Functor Html where
  fmap f (Element tag attributes children) =
    Element tag (fmap (fmap f) attributes) (fmap (fmap f) children)
  fmap _ (Text content) = Text content
  -- The mapped subtree also depends on the mapping function.
  fmap f (Lazy refs tree) = Lazy (Ref f : refs) (fmap f tree)
  fmap f (Keyed key tree) = Keyed key (fmap f tree)
  -- Only what the component tells its surroundings is mapped: its own
  -- messages stay inside.
  fmap f (Embed (Mount embedded)) =
    Embed . Mount $
      Embedded
        { initial = embedded.initial
        , view = embedded.view
        , update = \msg state -> second (map (fmap f)) (embedded.update msg state)
        , subscriptions = embedded.subscriptions
        }


{- | The element or text node inside any 'Lazy' and 'Keyed' wrappers. A
component that has not been given a state is shown as in 'static'.
-}
unwrap :: Html msg -> Html msg
unwrap (Lazy _ tree) = unwrap tree
unwrap (Keyed _ tree) = unwrap tree
unwrap (Embed mount) = unwrap (static mount)
unwrap tree = tree


-- | A component placed in a tree, with its state and message types hidden.
data Mount msg
  = forall state inner. (Typeable state, Typeable inner) => Mount (Embedded state inner msg)


instance Show (Mount msg) where
  show _ = "<component>"


{- | A component as it sits in a tree of messages @msg@: its own state is
@state@ and its own messages are @inner@.
-}
data Embedded state inner msg = Embedded
  { initial :: state
  -- ^ The state to start with.
  , view :: state -> Html inner
  , update :: inner -> state -> (state, [Effect inner msg])
  , subscriptions :: state -> Sub inner
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


{- | What a component looks like in its initial state, without its event
handlers. This is how a component appears where nothing keeps its state,
such as in "Hastea.Html.Render".
-}
static :: Mount a -> Html b
static (Mount embedded) = inert (embedded.view embedded.initial)
 where
  inert :: Html c -> Html d
  inert (Element tag attributes children) = Element tag (mapMaybe plain attributes) (map inert children)
  inert (Text content) = Text content
  inert (Lazy _ tree) = inert tree
  inert (Keyed key tree) = Keyed key (inert tree)
  inert (Embed mount) = static mount

  plain :: Attribute c -> Maybe (Attribute d)
  plain (Attribute key value) = Just (Attribute key value)
  plain (BoolAttribute key value) = Just (BoolAttribute key value)
  plain (Property key value) = Just (Property key value)
  plain Listener{} = Nothing


-- | A reference to a value of any type, compared by identity.
data Ref = forall a. Ref a


instance Show Ref where
  show _ = "<ref>"


{- | Whether two lists refer to the very same values in memory.

Because Haskell values are immutable, the same reference always means the
same value, so anything computed purely from one is equal to what was
computed from the other. 'False' does not mean the values differ: it only
means the work has to be done again.

The values are evaluated to weak head normal form first, so that an
unevaluated field access is recognised as the value it refers to.
-}
sameRefs :: [Ref] -> [Ref] -> Bool
sameRefs (a : as) (b : bs) = sameRef a b && sameRefs as bs
sameRefs [] [] = True
sameRefs _ _ = False


sameRef :: Ref -> Ref -> Bool
sameRef (Ref a) (Ref b) =
  a `seq` b `seq` isTrue# (reallyUnsafePtrEquality# a (unsafeCoerce b))


-- | Something attached to an element: an attribute, a property or an event listener.
data Attribute msg
  = -- | A plain attribute, set with @setAttribute@.
    Attribute String String
  | {- | A boolean attribute such as @disabled@: present when 'True',
    absent when 'False'.
    -}
    BoolAttribute String Bool
  | {- | A DOM property such as @value@ or @checked@, assigned directly on
    the node. Unlike the attribute of the same name, this keeps tracking
    the live state of a form control after the user has interacted with it.
    -}
    Property String PropertyValue
  | -- | An event listener that decodes the event object into a message.
    Listener String EventOptions (Decoder msg)
  deriving (Show)


instance Functor Attribute where
  fmap f (Listener event options decoder) = Listener event options (fmap f decoder)
  fmap _ (Attribute key value) = Attribute key value
  fmap _ (BoolAttribute key value) = BoolAttribute key value
  fmap _ (Property key value) = Property key value


-- | The value assigned to a DOM property.
data PropertyValue
  = PropertyString String
  | PropertyBool Bool
  deriving (Eq, Show)


-- | What to do with an event besides decoding it.
data EventOptions = EventOptions
  { preventDefault :: Bool
  -- ^ Call @event.preventDefault()@.
  , stopPropagation :: Bool
  -- ^ Call @event.stopPropagation()@.
  }
  deriving (Eq, Show)


-- | Neither prevent the default action nor stop propagation.
defaultEventOptions :: EventOptions
defaultEventOptions = EventOptions{preventDefault = False, stopPropagation = False}


{- | The attribute or property name of an 'Attribute', or 'Nothing' for an
event listener.
-}
attributeKey :: Attribute msg -> Maybe String
attributeKey (Attribute key _) = Just key
attributeKey (BoolAttribute key _) = Just key
attributeKey (Property key _) = Just key
attributeKey Listener{} = Nothing
