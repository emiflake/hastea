{- | The data types behind "Hastea.Html".

This module exposes the constructors of 'Html' and 'Attribute' for code that
needs to interpret a tree, such as a virtual DOM or a renderer. Application
code should build trees with the functions in "Hastea.Html",
"Hastea.Html.Attributes" and "Hastea.Html.Events" instead.
-}
module Hastea.Html.Internal
  ( Html (..)
  , Attribute (..)
  , PropertyValue (..)
  , EventOptions (..)
  , defaultEventOptions
  , attributeKey
  )
where

import Hastea.Html.Decode (Decoder)


-- | A tree of HTML that can produce messages of type @msg@.
data Html msg
  = -- | An element with a tag name, attributes and children.
    Element String [Attribute msg] [Html msg]
  | -- | A text node.
    Text String
  deriving (Show)


instance Functor Html where
  fmap f (Element tag attributes children) =
    Element tag (fmap (fmap f) attributes) (fmap (fmap f) children)
  fmap _ (Text content) = Text content


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
