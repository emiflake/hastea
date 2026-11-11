{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{- | Decoders turn untyped JavaScript values, such as DOM events, into
Haskell values.

A 'Decoder' is only a /description/ of how to read a value; it does not
depend on JavaScript. The @hastea@ package runs decoders against real
JavaScript values, while 'decodeValue' runs them against the pure 'Value'
type, which is handy for testing event handlers.
-}
module Hastea.Html.Decode
  ( Decoder

    -- * Primitives
  , string
  , number
  , int
  , bool
  , nullable

    -- * Structure
  , field
  , at
  , list

    -- * Combining
  , oneOf
  , andThen
  , failWith

    -- * Running decoders
  , Value (..)
  , decodeValue
  , Source (..)
  , decodeWith
  )
where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (first)


-- | A description of how to turn a JavaScript value into an @a@.
data Decoder a where
  Succeed :: a -> Decoder a
  Fail :: String -> Decoder a
  Ap :: Decoder (b -> a) -> Decoder b -> Decoder a
  Alt :: Decoder a -> Decoder a -> Decoder a
  AndThen :: Decoder b -> (b -> Decoder a) -> Decoder a
  String :: Decoder String
  Number :: Decoder Double
  Bool :: Decoder Bool
  Nullable :: Decoder b -> Decoder (Maybe b)
  List :: Decoder b -> Decoder [b]
  Field :: String -> Decoder a -> Decoder a


instance Show (Decoder a) where
  show _ = "<decoder>"


instance Functor Decoder where
  fmap f = Ap (Succeed f)


instance Applicative Decoder where
  pure = Succeed
  (<*>) = Ap


-- | 'empty' always fails; @a '<|>' b@ tries @b@ when @a@ fails.
instance Alternative Decoder where
  empty = Fail "no decoder matched"
  (<|>) = Alt


-- | Decode a JavaScript string.
string :: Decoder String
string = String


-- | Decode a JavaScript number.
number :: Decoder Double
number = Number


-- | Decode a JavaScript number that is a whole number.
int :: Decoder Int
int =
  number `andThen` \n ->
    let rounded = round n :: Int
     in if isNaN n || isInfinite n || fromIntegral rounded /= n
          then failWith "expected an integer"
          else pure rounded


-- | Decode a JavaScript boolean.
bool :: Decoder Bool
bool = Bool


-- | Decode @null@ or @undefined@ as 'Nothing', and anything else with the given decoder.
nullable :: Decoder a -> Decoder (Maybe a)
nullable = Nullable


-- | Decode a property of an object.
field :: String -> Decoder a -> Decoder a
field = Field


-- | Decode a nested property: @at ["target", "value"] string@.
at :: [String] -> Decoder a -> Decoder a
at path decoder = foldr field decoder path


-- | Decode an array, using the given decoder for each element.
list :: Decoder a -> Decoder [a]
list = List


-- | Try each decoder in turn and keep the first success.
oneOf :: [Decoder a] -> Decoder a
oneOf = foldr (<|>) empty


-- | Choose how to continue decoding the same value based on an earlier result.
andThen :: Decoder a -> (a -> Decoder b) -> Decoder b
andThen = AndThen


-- | A decoder that always fails with the given message.
failWith :: String -> Decoder a
failWith = Fail


{- | How to inspect values of type @v@. This is what lets one 'Decoder' run
against both real JavaScript values and the pure 'Value' type.
-}
data Source v = Source
  { sourceString :: v -> Maybe String
  , sourceNumber :: v -> Maybe Double
  , sourceBool :: v -> Maybe Bool
  , sourceIsNull :: v -> Bool
  -- ^ Whether the value is @null@ or @undefined@.
  , sourceList :: v -> Maybe [v]
  , sourceField :: String -> v -> Maybe v
  -- ^ Look up a property; 'Nothing' if the value has no such property.
  }


-- | Run a decoder against any kind of value that has a 'Source'.
decodeWith :: forall v a. Source v -> Decoder a -> v -> Either String a
decodeWith source = go
 where
  go :: forall b. Decoder b -> v -> Either String b
  go decoder v = case decoder of
    Succeed a -> Right a
    Fail message -> Left message
    Ap df da -> go df v <*> go da v
    Alt a b -> either (const (go b v)) Right (go a v)
    AndThen a f -> go a v >>= \x -> go (f x) v
    String -> expect "a string" (sourceString source v)
    Number -> expect "a number" (sourceNumber source v)
    Bool -> expect "a boolean" (sourceBool source v)
    Nullable inner
      | sourceIsNull source v -> Right Nothing
      | otherwise -> Just <$> go inner v
    List inner -> do
      items <- expect "an array" (sourceList source v)
      sequence
        [first (within ("[" <> show i <> "]")) (go inner item) | (i, item) <- zip [0 :: Int ..] items]
    Field key inner -> do
      child <- expect ("an object with a field named " <> show key) (sourceField source key v)
      first (within ("." <> key)) (go inner child)

  expect what = maybe (Left ("expected " <> what)) Right

  -- Prefix an error with the path segment it happened in.
  within segment message = case message of
    c : _ | c `elem` ".[" -> segment <> message
    _ -> segment <> ": " <> message


{- | A pure stand-in for a JavaScript value, for running decoders without a
JavaScript engine.
-}
data Value
  = Null
  | BoolValue Bool
  | NumberValue Double
  | StringValue String
  | ArrayValue [Value]
  | ObjectValue [(String, Value)]
  deriving (Eq, Show)


-- | Run a decoder against a pure 'Value'.
decodeValue :: Decoder a -> Value -> Either String a
decodeValue = decodeWith valueSource


valueSource :: Source Value
valueSource =
  Source
    { sourceString = \case StringValue s -> Just s; _ -> Nothing
    , sourceNumber = \case NumberValue n -> Just n; _ -> Nothing
    , sourceBool = \case BoolValue b -> Just b; _ -> Nothing
    , sourceIsNull = (== Null)
    , sourceList = \case ArrayValue vs -> Just vs; _ -> Nothing
    , sourceField = \key -> \case ObjectValue fields -> lookup key fields; _ -> Nothing
    }
