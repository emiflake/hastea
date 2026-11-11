{- | React: props, lists with keys, conditional rendering, controlled inputs.
<https://react.dev/learn/thinking-in-react>
-}
module ReactComparison.ProductTable
  ( Product (..)
  , products
  , filterableProductTable
  )
where

import Prelude hiding (div, span)

import Data.Char (toLower)
import Data.Function (on)
import Data.List (groupBy, isInfixOf)

import Hastea.Component
import Hastea.Html
import Hastea.Html.Attributes (checked, colSpan, placeholder, styles, type_, value)
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
    div
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
  form
    []
    [ input [type_ "text", value current.text, placeholder "Search...", onInput FilterTextChanged]
    , label
        []
        [ input [type_ "checkbox", checked current.inStockOnly, onCheck InStockOnlyChanged]
        , text " Only show products in stock"
        ]
    ]


-- | One group of rows for each category, in the order the products come in.
productTable :: [Product] -> Html msg
productTable items =
  table
    []
    [ thead [] [tr [] [th [] [text "Name"], th [] [text "Price"]]]
    , tbody [] (concatMap categoryRows (groupBy ((==) `on` (.category)) items))
    ]
 where
  categoryRows group =
    [keyed item.category (productCategoryRow item.category) | item <- take 1 group]
      <> [keyed item.name (productRow item) | item <- group]


productCategoryRow :: String -> Html msg
productCategoryRow category =
  tr [] [th [colSpan 2] [text category]]


productRow :: Product -> Html msg
productRow item =
  tr
    []
    [ td [] [if item.stocked then text item.name else outOfStock item.name]
    , td [] [text item.price]
    ]
 where
  outOfStock name = span [styles [("color", "red")]] [text name]
