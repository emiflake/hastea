module Main (main) where

import Data.IORef
import Data.List (isInfixOf, nub)

import Hastea.Cmd (runCmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Component
import Hastea.Component.Internal (Expansion (..), Routed (..), Store, deliver, emptyStore, expand, subscriptionsOf)
import Hastea.Html (Html)
import Hastea.Html qualified as H
import Hastea.Html.Decode (Value (..))
import Hastea.Html.Diff (diff)
import Hastea.Html.Events (onClick)
import Hastea.Html.Internal qualified as I
import Hastea.Html.Render (renderHtml)
import Hastea.Internal.Sub qualified as I.Sub
import Hastea.Sub qualified as Sub

import Support


main :: IO ()
main = runSuites [componentTests]


-- COMPONENTS
--
-- A stand-in for "Hastea.Root": it keeps the store, expands trees, and
-- routes messages, but there is no page.

data Sim msg = Sim
  { store :: IORef (Store msg)
  , received :: IORef [msg]
  -- ^ The messages that left the tree, latest first.
  , warned :: IORef [String]
  }


newSim :: IO (Sim msg)
newSim = Sim <$> newIORef emptyStore <*> newIORef [] <*> newIORef []


-- | What "Hastea.Root.render" does, minus the page.
showTree :: Sim msg -> Html msg -> IO (Html (Routed msg))
showTree sim tree = do
  expansion <- expand tree <$> readIORef sim.store
  writeIORef sim.store expansion.store
  modifyIORef' sim.warned (<> expansion.warnings)
  runCmd (route sim) expansion.effects
  pure expansion.tree


route :: Sim msg -> Routed msg -> IO ()
route sim (Out msg) = modifyIORef' sim.received (msg :)
route sim (Local path msg) = do
  current <- readIORef sim.store
  case deliver path msg current of
    Nothing -> pure ()
    Just (new, cmd) -> writeIORef sim.store new >> runCmd (route sim) cmd


-- | Click the element found by following child positions down a tree.
click :: Sim msg -> [Int] -> Html (Routed msg) -> IO ()
click sim path tree = mapM_ (route sim) [msg | Right msg <- fire "click" Null (descend path tree)]
 where
  descend [] el = el
  descend (index : rest) el = case I.unwrap el of
    I.Element _ _ children | child : _ <- drop index children -> descend rest child
    _ -> H.text ""


newtype CounterMsg = Add Int


-- | Counts clicks, in steps given as its props.
counter :: Component Int Int CounterMsg out
counter = component initial update view
 where
  initial _ = 0
  update (Add n) count = (count + n, [])
  view step count = H.button [onClick (Add step)] [H.text (show count)]


data ReporterMsg = Report | Again


-- | Tells its surroundings how often it was clicked, and can send itself a message.
reporter :: Component () Int ReporterMsg Int
reporter = (component initial update view){subscriptions}
 where
  initial () = 0

  update msg count = case msg of
    Report -> (count + 1, [Emit (count + 1)])
    Again -> (count, [Run (Cmd.perform (pure Report))])

  view () count =
    H.div [] [H.button [onClick Report] [H.text (show count)], H.button [onClick Again] [H.text "again"]]

  subscriptions count = if count > 0 then Sub.custom "ticks" neverStarts Just else Sub.none

  neverStarts :: (ReporterMsg -> IO ()) -> IO (IO ())
  neverStarts _ = pure (pure ())


newtype TotalMsg = Reported Int


-- | Contains a 'reporter' and shows the last number it reported.
total :: Component () Int TotalMsg out
total = component initial update view
 where
  initial () = 0
  update (Reported n) _ = (n, [])
  view () latest = H.div [] [Reported <$> use reporter (), H.span [] [H.text (show latest)]]


-- | Says hello to its surroundings when it appears and goodbye when it goes.
greeter :: Component String () () Msg
greeter =
  (component (const ()) (\() () -> ((), [])) (\name () -> H.span [] [H.text name]))
    { onMount = \name -> [Emit (Typed ("hello " <> name))]
    , onUnmount = \() -> [Emit (Typed "goodbye")]
    }


newtype LoaderMsg = Loaded String


-- | Loads what it shows when it appears.
loader :: Component () String LoaderMsg out
loader =
  (component (const "loading") (\(Loaded content) _ -> (content, [])) (\() content -> H.p [] [H.text content]))
    { onMount = \() -> [Run (Cmd.perform (pure (Loaded "loaded")))]
    }


-- | A component without an element of its own around its two counters.
pair :: Component () () () out
pair = component (const ()) (\() () -> ((), [])) (\() () -> H.fragment [use counter 1, use counter 10])


lazyFragment :: Int -> Html Msg
lazyFragment n = H.fragment [H.text (show n), H.text "!"]


-- Must be top-level functions for 'lazy' to recognise them again.
lazyCounter :: Int -> Html Msg
lazyCounter step = H.div [] [use counter step]


componentTests :: Suite -> IO ()
componentTests suite = do
  let shown = renderHtml :: Html (Routed Msg) -> String

  equal suite "a component without a state shows its initial view" "<p><button>0</button></p>" $
    renderHtml (H.p [] [use counter 1] :: Html Msg)

  -- Two uses of one component have separate states.
  sim <- newSim
  let two = H.div [] [use counter 1, use counter 10] :: Html Msg
  first <- showTree sim two
  equal suite "components start in their initial state" "<div><button>0</button><button>0</button></div>" (shown first)
  click sim [0] first
  click sim [0] first
  click sim [1] first
  second <- showTree sim two
  equal suite "each use of a component has its own state" "<div><button>2</button><button>10</button></div>" (shown second)
  equal suite "a component's own messages stay inside" [] =<< readIORef sim.received

  -- Props are passed in each time; the state stays.
  third <- showTree sim (H.div [] [use counter 5, use counter 10])
  click sim [0] third
  fourth <- showTree sim (H.div [] [use counter 5, use counter 10])
  equal suite "new props keep the state" "<div><button>7</button><button>10</button></div>" (shown fourth)

  -- State belongs to a place in the tree.
  fifth <- showTree sim (H.div [] [use counter 10])
  equal suite "without keys, state goes by position" "<div><button>7</button></div>" (shown fifth)
  _ <- showTree sim (H.div [] [])
  sixth <- showTree sim two
  equal suite "a component that was removed starts afresh" "<div><button>0</button><button>0</button></div>" (shown sixth)
  click sim [0] first
  seventh <- showTree sim (H.p [] [use counter 1])
  equal suite "state does not carry over to a different element" "<p><button>0</button></p>" (shown seventh)

  -- Keys make the state follow the item.
  keyedSim <- newSim
  let list names = H.ul [] [H.keyed name (H.li [] [use counter 1]) | name <- names] :: Html Msg
  ab <- showTree keyedSim (list ["a", "b"])
  click keyedSim [0, 0] ab
  ba <- showTree keyedSim (list ["b", "a"])
  equal suite "with keys, state follows the item" "<ul><li><button>0</button></li><li><button>1</button></li></ul>" (shown ba)
  onlyB <- showTree keyedSim (list ["b"])
  equal suite "removing a keyed item leaves the others' state" "<ul><li><button>0</button></li></ul>" (shown onlyB)
  back <- showTree keyedSim (list ["a", "b"])
  equal suite "a keyed item that returns starts afresh" "<ul><li><button>0</button></li><li><button>0</button></li></ul>" (shown back)

  -- A message for a component that is gone.
  goneSim <- newSim
  present <- showTree goneSim (H.div [] [use counter 1] :: Html Msg)
  _ <- showTree goneSim (H.div [] [])
  click goneSim [0] present
  after <- showTree goneSim (H.div [] [use counter 1])
  equal suite "a message for a removed component is dropped" "<div><button>0</button></div>" (shown after)

  -- Outputs become messages of the surrounding view.
  outSim <- newSim
  let reporting = H.div [] [Typed . show <$> use reporter ()] :: Html Msg
  r1 <- showTree outSim reporting
  click outSim [0, 0] r1
  click outSim [0, 0] r1
  equal suite "outputs reach the surrounding view, mapped by fmap" [Typed "2", Typed "1"] =<< readIORef outSim.received
  click outSim [0, 1] r1
  equal suite "commands send their messages back to the component" [Typed "3", Typed "2", Typed "1"] =<< readIORef outSim.received
  r2 <- showTree outSim reporting
  equal suite "the view shows the state after several messages" "<div><div><button>3</button><button>again</button></div></div>" (shown r2)

  -- Components inside components.
  nestedSim <- newSim
  let nesting = H.div [] [use total ()] :: Html Msg
  n1 <- showTree nestedSim nesting
  click nestedSim [0, 0, 0] n1
  click nestedSim [0, 0, 0] n1
  n2 <- showTree nestedSim nesting
  equal suite "a nested component's output goes to the component around it" "<div><div><div><button>2</button><button>again</button></div><span>2</span></div></div>" (shown n2)
  equal suite "and no further" [] =<< readIORef nestedSim.received

  -- Subscriptions
  let names (I.Sub.Sub sources) = map fst sources
  subSim <- newSim
  let twoReporters = H.div [] [Typed . show <$> use reporter (), Typed . show <$> use reporter ()] :: Html Msg
  s1 <- showTree subSim twoReporters
  equal suite "no subscriptions to begin with" [] . names . subscriptionsOf =<< readIORef subSim.store
  click subSim [0, 0] s1
  equal suite "a component's subscriptions follow its state at once" 1 . length . names . subscriptionsOf =<< readIORef subSim.store
  _ <- showTree subSim twoReporters
  equal suite "and stay when the tree is shown again" 1 . length . names . subscriptionsOf =<< readIORef subSim.store
  click subSim [1, 0] s1
  _ <- showTree subSim twoReporters
  subscribed <- names . subscriptionsOf <$> readIORef subSim.store
  equal suite "each component has its own subscriptions" 2 (length (nub subscribed))
  _ <- showTree subSim (H.div [] [])
  equal suite "a removed component's subscriptions end" [] . names . subscriptionsOf =<< readIORef subSim.store

  -- Lazy subtrees with components in them
  lazySim <- newSim
  let lazily = H.div [] [H.lazy lazyCounter 1, H.text "x"] :: Html Msg
      patchBetween old new = outline (fst (diff old new))
  l1 <- showTree lazySim lazily
  l2 <- showTree lazySim lazily
  equal suite "an unchanged lazy subtree with a component is skipped" "keep" (patchBetween l1 l2)
  click lazySim [0, 0] l2
  l3 <- showTree lazySim lazily
  equal suite "a component's new state gets through a lazy subtree" "<div><div><button>1</button></div>x</div>" (shown l3)
  equal suite "and is seen by the diff" True (patchBetween l2 l3 /= "keep")
  l4 <- showTree lazySim lazily
  equal suite "after which the subtree is skipped again" "keep" (patchBetween l3 l4)
  click lazySim [0, 0] l4
  l5 <- showTree lazySim lazily
  equal suite "handlers in a skipped subtree still reach their component" "<div><div><button>2</button></div>x</div>" (shown l5)


  -- Appearing and going away
  lifeSim <- newSim
  let greeting people = H.ul [] [H.keyed name (H.li [] [use greeter name]) | name <- people] :: Html Msg
      heard = reverse <$> readIORef lifeSim.received
  _ <- showTree lifeSim (greeting ["a"])
  equal suite "a component that appears can say so" [Typed "hello a"] =<< heard
  _ <- showTree lifeSim (greeting ["a"])
  equal suite "but only once" [Typed "hello a"] =<< heard
  _ <- showTree lifeSim (greeting ["b", "a"])
  equal suite "another one appearing does not disturb it" [Typed "hello a", Typed "hello b"] =<< heard
  _ <- showTree lifeSim (greeting ["b"])
  equal suite "a component that goes away can say so" [Typed "hello a", Typed "hello b", Typed "goodbye"] =<< heard
  _ <- showTree lifeSim (H.text "nothing")
  equal suite "also when everything around it goes" 4 . length =<< heard
  _ <- showTree lifeSim (H.div [] [H.div [] [use greeter "deep"]])
  _ <- showTree lifeSim (H.div [] [H.span [] []])
  equal suite "also when an element above it is replaced" [Typed "hello deep", Typed "goodbye"] . drop 4 =<< heard
  _ <- showTree lifeSim (H.div [] [use greeter "x"])
  _ <- showTree lifeSim (H.div [] [use counter 1])
  equal suite "also when another component takes its place" [Typed "hello x", Typed "goodbye"] . drop 6 =<< heard

  loadSim <- newSim
  let loading = H.div [] [use loader ()] :: Html Msg
  before <- showTree loadSim loading
  equal suite "a component starts in its initial state" "<div><p>loading</p></div>" (shown before)
  loaded <- showTree loadSim loading
  equal suite "a command started on appearing reports back to it" "<div><p>loaded</p></div>" (shown loaded)

  -- Fragments
  fragmentSim <- newSim
  let pairs = H.div [] [use pair (), H.text "|", H.fragment [H.text "a", H.text "b"]] :: Html Msg
  f1 <- showTree fragmentSim pairs
  equal suite "a component's fragment joins the children around it" "<div><button>0</button><button>0</button>|ab</div>" (shown f1)
  equal suite "as separate nodes" 5 $ case I.unwrap f1 of
    I.Element _ _ children -> length children
    _ -> 0
  click fragmentSim [1] f1
  click fragmentSim [0] f1
  click fragmentSim [0] f1
  f2 <- showTree fragmentSim pairs
  equal suite "components in a fragment keep their own state" "<div><button>2</button><button>10</button>|ab</div>" (shown f2)
  topSim <- newSim
  top <- showTree topSim (use pair () :: Html Msg)
  equal suite "a fragment can be the whole tree" "<button>0</button><button>0</button>" (shown top)

  -- Warnings
  warnSim <- newSim
  _ <- showTree warnSim (H.ul [] [H.keyed "a" (H.li [] []), H.keyed "b" (H.li [] []), H.keyed "a" (H.li [] [])] :: Html Msg)
  duplicate <- readIORef warnSim.warned
  equal suite "duplicate keys are pointed out, once per key" [True] (map ("the key \"a\"" `isInfixOf`) duplicate)
  writeIORef warnSim.warned []
  _ <- showTree warnSim (H.div [] [H.lazy lazyFragment 1] :: Html Msg)
  inLazy <- readIORef warnSim.warned
  equal suite "a fragment directly inside lazy is pointed out" [True] (map ("inside lazy" `isInfixOf`) inLazy)
  writeIORef warnSim.warned []
  _ <- showTree warnSim (H.ul [] [H.keyed "a" (H.li [] []), H.keyed "b" (H.li [] []), H.fragment [H.text "x"]] :: Html Msg)
  equal suite "a tree without mistakes has no warnings" [] =<< readIORef warnSim.warned
