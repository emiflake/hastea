{- | The table of rows that web frameworks are commonly measured with: create
many rows, change some, select, swap, remove and clear them.

The same page is written with React in @react/app.js@, and @run.cjs@
measures both. See the README next to this file.
-}
module Main where

import Hastea.App (runApp)
import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Html (Html)
import Hastea.Html qualified as H
import Hastea.Html.Attributes qualified as A
import Hastea.Html.Events (onClick)
import Hastea.Svg qualified as Svg
import Hastea.Svg.Attributes qualified as Svg


-- MODEL

data Row = Row
  { id :: Int
  , label :: String
  }


data Model = Model
  { rows :: [Row]
  , selected :: Int
  -- ^ The id of the selected row, or 0.
  , nextId :: Int
  , seed :: Int
  -- ^ For choosing the words of the next label.
  }


initial :: Model
initial = Model{rows = [], selected = 0, nextId = 1, seed = 1}


-- | Make a number of new rows, and the model that knows they were made.
makeRows :: Int -> Model -> ([Row], Model)
makeRows count model =
  (new, model{nextId = model.nextId + count, seed = last (model.seed : map fst labelled)})
 where
  labelled = take count (drop 1 (iterate (\(current, _) -> makeLabel current) (model.seed, "")))
  new = zipWith (\rowId (_, rowLabel) -> Row{id = rowId, label = rowLabel}) [model.nextId ..] labelled


-- | A label of three words chosen by a seed, and the next seed.
makeLabel :: Int -> (Int, String)
makeLabel seed0 = (seed3, unwords [choose adjectives seed1, choose colours seed2, choose nouns seed3])
 where
  seed1 = next seed0
  seed2 = next seed1
  seed3 = next seed2
  -- Small enough not to overflow the 32-bit Int of wasm.
  next current = (current * 75 + 74) `mod` 65537
  choose list number = list !! (number `mod` length list)


adjectives, colours, nouns :: [String]
adjectives = words "pretty large big small tall short long handsome plain quaint clean elegant easy angry crazy helpful mushy odd unsightly adorable important inexpensive cheap expensive fancy"
colours = words "red yellow blue green pink brown purple brown white black orange"
nouns = words "table chair house bbq desk car pony cookie sandwich burger pizza mouse keyboard"


-- UPDATE

data Msg
  = Created Int
  | Appended Int
  | UpdatedEveryTenth
  | Cleared
  | Swapped
  | Selected Int
  | Removed Int


update :: Msg -> Model -> (Model, Cmd Msg)
update msg model = (step msg model, Cmd.none)


step :: Msg -> Model -> Model
step msg model = case msg of
  Created count ->
    let (new, model') = makeRows count model in model'{rows = new, selected = 0}
  Appended count ->
    let (new, model') = makeRows count model in model'{rows = model.rows <> new}
  UpdatedEveryTenth ->
    model{rows = zipWith emphasise [0 :: Int ..] model.rows}
  Cleared -> model{rows = [], selected = 0}
  Swapped -> model{rows = swap 1 998 model.rows}
  Selected rowId -> model{selected = rowId}
  Removed rowId -> model{rows = [row | row <- model.rows, row.id /= rowId]}
 where
  emphasise index row
    | index `mod` 10 == 0 = row{label = row.label <> " !!!"}
    | otherwise = row


-- | Exchange the elements at two positions, if both exist.
swap :: Int -> Int -> [item] -> [item]
swap first second items
  | second < length items = zipWith pick [0 ..] items
  | otherwise = items
 where
  pick index item
    | index == first = items !! second
    | index == second = items !! first
    | otherwise = item


-- VIEW

view :: Model -> Html Msg
view model =
  H.div
    [A.className "container"]
    [ H.div
        [A.className "jumbotron"]
        [ H.h1 [] [logo, H.text " Hastea"]
        , H.div
            []
            [ action "run" "Create 1,000 rows" (Created 1000)
            , action "runlots" "Create 10,000 rows" (Created 10000)
            , action "add" "Append 1,000 rows" (Appended 1000)
            , action "update" "Update every 10th row" UpdatedEveryTenth
            , action "clear" "Clear" Cleared
            , action "swaprows" "Swap Rows" Swapped
            ]
        ]
    , H.table
        [A.className "table"]
        [ H.tbody
            [A.id_ "tbody"]
            [ -- A row that did not change, and whose selection did not, is skipped.
              H.keyed (show row.id) (H.lazy2 viewRow (row.id == model.selected) row)
            | row <- model.rows
            ]
        ]
    ]


action :: String -> String -> Msg -> Html Msg
action buttonId caption msg =
  H.button [A.type_ "button", A.id_ buttonId, onClick msg] [H.text caption]


viewRow :: Bool -> Row -> Html Msg
viewRow isSelected row =
  H.tr
    [A.classList [("danger", isSelected)]]
    [ H.td [A.className "id"] [H.text (show row.id)]
    , H.td [] [H.a [A.className "lbl", onClick (Selected row.id)] [H.text row.label]]
    , H.td [] [H.a [A.className "remove", onClick (Removed row.id)] [H.text "x"]]
    ]


-- | A cup of tea.
logo :: Html msg
logo =
  Svg.svg
    [Svg.viewBox "0 0 24 24", Svg.width "32", Svg.height "32"]
    [ Svg.path [Svg.d "M4 9h13v6a5 5 0 0 1-5 5H9a5 5 0 0 1-5-5z", Svg.fill "#c0392b"] []
    , Svg.path [Svg.d "M17 10h1.5a2.5 2.5 0 0 1 0 5H17", Svg.fill "none", Svg.stroke "#c0392b", Svg.strokeWidth "2"] []
    , Svg.path [Svg.d "M8 6c0-1.5 1.5-1.5 1.5-3M12 6c0-1.5 1.5-1.5 1.5-3", Svg.fill "none", Svg.stroke "#888", Svg.strokeWidth "1.5", Svg.strokeLinecap "round"] []
    ]


main :: IO ()
main = runApp update initial view


-- The wasm module is linked as a WASI reactor, so there is no implicit entry
-- point: index.js calls this export once the module is initialised.
foreign export javascript "hs_start" main :: IO ()
