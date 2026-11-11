{- | Work out what has to change in the DOM to turn one tree into another.

This is the pure half of rendering. 'diff' compares the tree that is on the
page with the tree that should be, and describes the difference as a
'Patch'. Carrying the patch out against the real DOM is a separate step,
done by the @hastea@ package. Since 'diff' has no effects it can be tested
without a browser.
-}
module Hastea.Html.Diff
  ( diff
  , Patch (..)
  , Children (..)
  , ChildOp (..)
  , AttributeChange (..)
  )
where

import Data.Bifunctor (second)
import Data.List (mapAccumL, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Hastea.Html.Internal


-- | What to do to one DOM node.
data Patch msg
  = -- | The node and everything below it are already right.
    Keep
  | -- | Replace the node with a newly created one.
    Replace (Html msg)
  | -- | Change the content of a text node.
    SetText String
  | -- | Change an element's attributes, then its children.
    Update [AttributeChange msg] (Children msg)
  deriving (Show)


-- | What to do to the children of an element.
data Children msg
  = {- | The same children in the same order: one patch for each existing
    child.
    -}
    InPlace [Patch msg]
  | {- | Children were added, removed or moved: how to obtain each child of
    the new list, in order. Existing children that are not reused are
    removed.
    -}
    Rearrange [ChildOp msg]
  deriving (Show)


-- | Where a child in a rearranged list comes from.
data ChildOp msg
  = {- | The existing child at this position (counted before any change),
    after applying a patch to it.
    -}
    Reuse Int (Patch msg)
  | -- | A newly created node.
    Create (Html msg)
  deriving (Show)


-- | A change to the attributes of an element.
data AttributeChange msg
  = -- | Set an attribute or property. Never a listener.
    Set (Attribute msg)
  | -- | Remove an attribute, or reset a property. Never a listener.
    Clear (Attribute msg)
  | -- | The element now has a listener for this event, and did not before.
    Listen String
  deriving (Show)


{- | Compare the tree on the page with the tree that should be there.

Returns the patch, and the tree that is on the page once the patch has been
applied. That is the new tree, except that a lazy subtree which was skipped
is taken from the old tree, where it has already been computed. Pass it as
the old tree next time.
-}
diff :: Html msg -> Html msg -> (Patch msg, Html msg)
diff old new = case new of
  Keyed key inner -> second (Keyed key) (diff old inner)
  -- Components are given their state before diffing ("Hastea.Component.Internal").
  Embed mount -> diff old (static mount)
  Lazy refs inner
    | Lazy oldRefs _ <- withoutKey old
    , sameRefs oldRefs refs ->
        (Keep, withoutKey old)
    | otherwise -> second (Lazy refs) (diff old inner)
  Text content -> case unwrap old of
    Text oldContent | oldContent == content -> (Keep, new)
    Text _ -> (SetText content, new)
    _ -> (Replace new, new)
  Element tag attributes children -> case unwrap old of
    Element oldTag oldAttributes oldChildren
      | oldTag == tag ->
          let (childrenPatch, children') = diffChildren oldChildren children
           in ( update (diffAttributes oldAttributes attributes) childrenPatch
              , Element tag attributes children'
              )
    _ -> (Replace new, new)


withoutKey :: Html msg -> Html msg
withoutKey (Keyed _ tree) = withoutKey tree
withoutKey tree = tree


-- An update that changes nothing is no update at all.
update :: [AttributeChange msg] -> Children msg -> Patch msg
update [] (InPlace patches) | all isKeep patches = Keep
update changes children = Update changes children


isKeep :: Patch msg -> Bool
isKeep Keep = True
isKeep _ = False


-- CHILDREN

{- Each new child is matched with an old one: a keyed child with the old
child of the same key, any other child with the next old child that has no
key. Matched pairs are diffed; unmatched new children are created and
unmatched old children removed.
-}
diffChildren :: [Html msg] -> [Html msg] -> (Children msg, [Html msg])
diffChildren olds news
  | inPlace = (InPlace [patch | Reuse _ patch <- ops], news')
  | otherwise = (Rearrange ops, news')
 where
  indexed = zip [0 ..] olds
  -- With duplicate keys, the first one is the one that gets matched.
  keyedOlds = Map.fromListWith (\_ first -> first) [(key, old) | old@(_, Keyed key _) <- indexed]
  unkeyedOlds = [old | old@(_, tree) <- indexed, not (isKeyed tree)]

  (_, results) = mapAccumL match (keyedOlds, unkeyedOlds) news
  (ops, news') = unzip results

  inPlace =
    length ops == length olds && and (zipWith (\i op -> reuses i op) [0 ..] ops)
  reuses :: Int -> ChildOp msg -> Bool
  reuses i (Reuse j _) = i == j
  reuses _ (Create _) = False


type Unmatched msg = (Map String (Int, Html msg), [(Int, Html msg)])


match :: Unmatched msg -> Html msg -> (Unmatched msg, (ChildOp msg, Html msg))
match (keyed, unkeyed) new = case new of
  Keyed key _
    | Just old <- Map.lookup key keyed -> ((Map.delete key keyed, unkeyed), reuse old)
    | otherwise -> ((keyed, unkeyed), (Create new, new))
  _ -> case unkeyed of
    old : rest -> ((keyed, rest), reuse old)
    [] -> ((keyed, unkeyed), (Create new, new))
 where
  reuse (index, old) = let (patch, new') = diff old new in (Reuse index patch, new')


isKeyed :: Html msg -> Bool
isKeyed (Keyed _ _) = True
isKeyed _ = False


-- ATTRIBUTES

-- Attributes and properties are set in different ways, so one of each can
-- exist under the same name.
data Slot = AttributeSlot String | PropertySlot String
  deriving (Eq)


diffAttributes :: [Attribute msg] -> [Attribute msg] -> [AttributeChange msg]
diffAttributes old new = clears <> sets <> listens
 where
  oldValues = values old
  newValues = values new

  clears = [Clear attribute | (slot, attribute) <- oldValues, slot `notElem` map fst newValues]
  sets =
    [ Set attribute
    | (slot, attribute) <- newValues
    , isProperty attribute || fmap valueOf (lookup slot oldValues) /= Just (valueOf attribute)
    ]
  listens = [Listen event | event <- nub (events new), event `notElem` events old]

  events attributes = [event | Listener event _ _ <- attributes]

  -- A property is always set again, even if it is unchanged from the old
  -- tree: the user may have changed it on the page since, by typing into a
  -- field for example.
  isProperty Property{} = True
  isProperty _ = False


{- | What a list of attributes amounts to on an element: the last attribute
given for each name, leaving out boolean attributes that are off.
-}
values :: [Attribute msg] -> [(Slot, Attribute msg)]
values = filter (isOn . snd) . foldr keepLast []
 where
  keepLast attribute rest = case slotOf attribute of
    Just slot | slot `notElem` map fst rest -> (slot, attribute) : rest
    _ -> rest

  isOn (BoolAttribute _ False) = False
  isOn _ = True


slotOf :: Attribute msg -> Maybe Slot
slotOf (Attribute key _) = Just (AttributeSlot key)
slotOf (BoolAttribute key _) = Just (AttributeSlot key)
slotOf (Property key _) = Just (PropertySlot key)
slotOf Listener{} = Nothing


-- The value of an attribute in a form that can be compared.
valueOf :: Attribute msg -> Maybe PropertyValue
valueOf (Attribute _ value) = Just (PropertyString value)
valueOf (BoolAttribute _ value) = Just (PropertyBool value)
valueOf (Property _ value) = Just value
valueOf Listener{} = Nothing
