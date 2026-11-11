{- | React: props, lists with keys, conditional rendering, controlled inputs.
<https://react.dev/learn/thinking-in-react>
-}
module ReactComparison.ProductTable
  ( Product (..)
  , products
  , filterableProductTable
  )
where

import Data.Char (toLower)
import Data.Function (on)
import Data.List (groupBy, isInfixOf)

import Hastea.Component
import Hastea.Html (Html)
import Hastea.Html qualified as H
import Hastea.Html.Attributes qualified as A
import Hastea.Html.Events (onCheck, onInput)


data Product = Product
  { category :: String
  , price :: String
  , stocked :: Bool
  , name :: String
  }


products :: [Product]
products =
  [ Product "Fruits" "$1" True "Apple"
  , Product "Fruits" "$1" True "Dragonfruit"
  , Product "Fruits" "$2" False "Passionfruit"
  , Product "Vegetables" "$2" True "Spinach"
  , Product "Vegetables" "$4" False "Pumpkin"
  , Product "Vegetables" "$1" True "Peas"
  ]


-- | What the user is filtering by.
data Filter = Filter
  { text :: String
  , inStockOnly :: Bool
  }


data Msg
  = FilterTextChanged String
  | InStockOnlyChanged Bool


-- | The products to show are props; what to filter them by is state.
filterableProductTable :: Component [Product] Filter Msg out
filterableProductTable = component initial update view
 where
  initial _ = Filter{text = "", inStockOnly = False}

  update msg current = case msg of
    FilterTextChanged newText -> (current{text = newText}, [])
    InStockOnlyChanged only -> (current{inStockOnly = only}, [])

  view allProducts current =
    H.div
      []
      [ searchBar current
      , productTable (filter (matches current) allProducts)
      ]


matches :: Filter -> Product -> Bool
matches current item =
  map toLower current.text `isInfixOf` map toLower item.name
    && (item.stocked || not current.inStockOnly)


searchBar :: Filter -> Html Msg
searchBar current =
  H.form
    []
    [ H.input
        [A.type_ "text", A.value current.text, A.placeholder "Search...", onInput FilterTextChanged]
    , H.label
        []
        [ H.input [A.type_ "checkbox", A.checked current.inStockOnly, onCheck InStockOnlyChanged]
        , H.text " Only show products in stock"
        ]
    ]


-- | One group of rows for each category, in the order the products come in.
productTable :: [Product] -> Html msg
productTable items =
  H.table
    []
    [ H.thead [] [H.tr [] [H.th [] [H.text "Name"], H.th [] [H.text "Price"]]]
    , H.tbody [] (concatMap categoryRows (groupBy ((==) `on` (.category)) items))
    ]
 where
  categoryRows group =
    [H.keyed item.category (productCategoryRow item.category) | item <- take 1 group]
      <> [H.keyed item.name (productRow item) | item <- group]


productCategoryRow :: String -> Html msg
productCategoryRow category =
  H.tr [] [H.th [A.colSpan 2] [H.text category]]


productRow :: Product -> Html msg
productRow item =
  H.tr
    []
    [ H.td [] [if item.stocked then H.text item.name else outOfStock item.name]
    , H.td [] [H.text item.price]
    ]
 where
  outOfStock name = H.span [A.styles [("color", "red")]] [H.text name]
