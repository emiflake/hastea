module Hastea.Internal.Foreign where


import qualified Hastea.Internal.Foreign.DOM as DOM
import           Hastea.Internal.VirtualDOM

import           Asterius.Types
import           Data.Map                    (Map)
import qualified Data.Map                    as Map

type Props = Map String String

data EventListener l = On String (JSVal -> IO ())

data DOM l = DOM
  { createElement       :: String -> IO l
  , createElementNS     :: String -> String -> IO l
  , createTextNode      :: String -> IO l
  , replaceChild        :: l -> l -> l -> IO ()
  , removeChild         :: l -> l -> IO ()
  , appendChild         :: l -> l -> IO ()
  , childCount          :: l -> IO Int
  , childAt             :: Int -> l -> IO (Maybe l)
  , setTextContent      :: String -> l -> IO ()
  , setAttribute        :: String -> String -> l -> IO ()
  , removeAttribute     :: String -> l -> IO ()
  , addEventListener    :: String -> (JSVal -> IO ()) -> l -> IO ()
  , removeEventListener :: String -> l -> IO ()
  }

createEl :: DOM l -> VNode l -> IO l
createEl api (Text t) = createTextNode api t
createEl api node = do
    el <- createElement api (tagName node)
    mapM_ (\(k, v) -> setAttribute api k v el) (Map.toList $ props node)
    mapM_ (createEl api >=> flip (appendChild api) el) (children node)
    return el

addListener :: DOM l -> l -> EventListener l -> IO ()
addListener api target (On n handler) = addEventListener api n handler target

removeListener :: DOM l -> l -> EventListener l -> IO ()
removeListener api target (On n _) = removeEventListener api n target

changed :: VNode l -> VNode l-> Bool
changed (Text t1) (Text t2) = t1 /= t2
changed e1 e2               = name e1 /= name e2

updateListeners :: DOM l -> l -> [EventListener l] -> [EventListener l] -> IO ()
updateListeners api me oldListeners newListeners = do
  mapM_ (removeListener api me) oldListeners
  mapM_ (addListener api me) newListeners

updateProps :: DOM l -> l -> Props -> Props -> IO ()
updateProps api target old new =
    mapM_ update (Map.keys (Map.union old new))
  where
    update key =
        case (Map.lookup key old, Map.lookup key new) of
            (Nothing, Just value) ->
                setAttribute api key value target
            (Just _, Nothing) ->
                removeAttribute api key target
            (Just prev, Just next) ->
                when (prev /= next) (setAttribute api key next target)
            (Nothing, Nothing) ->
                return ()

patch :: DOM l-> l -> Maybe (VNode l) -> Maybe (VNode l) -> IO ()
patch api target' old' new' = patchIndexed api target' old' new' 0

patchIndexed :: DOM l-> l -> Maybe (VNode l) -> Maybe (VNode l) -> Int -> IO ()
patchIndexed _ _ Nothing Nothing _ = return ()

patchIndexed api parent Nothing (Just new) _ = do
  el <- createEl api new
  appendChild api el parent

patchIndexed api parent (Just _) Nothing index = do
  child <- childAt api index parent
  case child of
    Just n  -> removeChild api n parent
    Nothing -> return ()

patchIndexed api parent (Just (Text old)) (Just (Text new)) index = do
  when (old /= new) $ do
    if index == 0
      then setTextContent api new parent
      else throwJS (DOM.toJSString $ "patchIndexed text->text error: index /= 0 (" <> show index <> ")" )

patchIndexed api parent (Just (Text _)) (Just new@(Element _ _ _ _)) _ = do
  setTextContent api "" parent -- remove inner text
  n <- createEl api new
  appendChild api n parent

patchIndexed api parent (Just (Element _ _ _ _)) (Just (Text new)) _ = do
  cs <- childCount api parent
  mapM_ (f parent) (reverse [0..cs-1])
  setTextContent api new parent

  where
    f p i = do
      child <- childAt api i p
      case child of
        Just n  -> removeChild api n p
        Nothing -> return ()

patchIndexed api parent (Just old) (Just new) index = do
  me' <- childAt api index parent
  case me' of
    Nothing -> return ()
    Just me ->
      if changed old new
          then do
            n <- createEl api new
            replaceChild api n me parent
          else do
            case (old, new) of
              (Element {props = oldProps, listeners = oldListeners}, Element {props = newProps, listeners = newListeners}) -> do
                updateListeners api me oldListeners newListeners
                updateProps api me oldProps newProps
              (_, _) -> return ()
            walkChildren api me old new

walkChildren :: DOM l -> l -> VNode l -> VNode l -> IO ()
walkChildren api target old new =
    if oldLength > newLength
        then do
            walkIndexes' [0 .. (newLength - 1)] -- walk up to last child of new
            walkIndexes' (reverse [newLength .. (oldLength - 1)]) -- delete children backwards from end
        else
            walkIndexes' [0 .. (newLength - 1)]
  where
    walkIndexes' =
        mapM_ (\i' ->
            let
                i = Offset i'
            in
                patchIndexed api target (children old ! i) (children new ! i) i')
    oldLength = fromIntegral $ toInteger $ length $ children old
    newLength = fromIntegral $ toInteger $ length $ children new
