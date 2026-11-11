{-# LANGUAGE GADTs #-}
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
  , flatten
  , Extension (..)
  , fromExtension
  , Attribute (..)
  , PropertyValue (..)
  , EventOptions (..)
  , defaultEventOptions
  , attributeKey
  )
where

import Data.Typeable (Typeable, eqT, (:~:) (Refl))
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import Unsafe.Coerce (unsafeCoerce)

import Hastea.Html.Decode (Decoder)


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
  | {- | Several nodes in a row, in a place where one is expected. See
    'Hastea.Html.fragment'.
    -}
    Fragment [Html msg]
  | {- | A node that another library gives a meaning to, such as a component
    with state. This package does not know what it is: it only carries the
    value along, and treats the node as the tree given with it, which is
    what the node looks like when nothing gives it a meaning.
    -}
    Extend (Extension msg) (Html msg)
  deriving (Show)


instance Functor Html where
  fmap f (Element tag attributes children) =
    Element tag (fmap (fmap f) attributes) (fmap (fmap f) children)
  fmap _ (Text content) = Text content
  -- The mapped subtree also depends on the mapping function.
  fmap f (Lazy refs tree) = Lazy (Ref f : refs) (fmap f tree)
  fmap f (Keyed key tree) = Keyed key (fmap f tree)
  fmap f (Fragment trees) = Fragment (map (fmap f) trees)
  fmap f (Extend extension fallback) = Extend (fmap f extension) (fmap f fallback)


{- | The element or text node inside any 'Lazy' and 'Keyed' wrappers. An
extension counts as the tree it falls back to.

This is for places that hold exactly one node. A fragment that is still
there (see 'flatten') counts as its first node, or as nothing.
-}
unwrap :: Html msg -> Html msg
unwrap (Lazy _ tree) = unwrap tree
unwrap (Keyed _ tree) = unwrap tree
unwrap (Extend _ fallback) = unwrap fallback
unwrap (Fragment (tree : _)) = unwrap tree
unwrap (Fragment []) = Text ""
unwrap tree = tree


{- | Replace the fragments in a list of children by what they contain, so
that each entry is one node.

A fragment with a key passes it on: its children get keys that start with
it. A fragment inside a 'Lazy' node is left alone, because looking for it
would mean computing the lazy tree.
-}
flatten :: [Html msg] -> [Html msg]
flatten = concatMap spread
 where
  spread (Fragment trees) = flatten trees
  spread (Keyed key (Fragment trees)) = zipWith (under key) [0 :: Int ..] (flatten trees)
  spread tree = [tree]

  under key _ (Keyed inner tree) = Keyed (key <> "/" <> inner) tree
  under key index tree = Keyed (key <> "/" <> show index) tree


{- | A value of some other library's type, which must say how the messages
inside it are changed ('Functor') and be recognisable again ('Typeable').
-}
data Extension msg = forall f. (Typeable f, Functor f) => Extension (f msg)


instance Show (Extension msg) where
  show _ = "<extension>"


instance Functor Extension where
  fmap f (Extension value) = Extension (fmap f value)


-- | Get the value out of an extension, if it is of the type asked for.
fromExtension :: forall g msg. Typeable g => Extension msg -> Maybe (g msg)
fromExtension (Extension (value :: f msg)) = case eqT @f @g of
  Just Refl -> Just value
  Nothing -> Nothing


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
