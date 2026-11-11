{- | React: props, lists with keys, conditional rendering, controlled inputs.
<https://react.dev/learn/thinking-in-react>
-}
module ReactComparison.ProductTable
  ( Model
  , Msg
  , initial
  , update
  , view
  )
where

import Prelude hiding (div, span)

import Data.Char (toLower)
import Data.List (isInfixOf)

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


data Model = Model
  { filterText :: String
  , inStockOnly :: Bool
  }


data Msg
  = FilterTextChanged String
  | InStockOnlyChanged Bool


initial :: Model
initial = Model{filterText = "", inStockOnly = False}


update :: Msg -> Model -> Model
update msg model = case msg of
  FilterTextChanged newText -> model{filterText = newText}
  InStockOnlyChanged only -> model{inStockOnly = only}


view :: Model -> Html Msg
view model =
  div
    []
    [ searchBar model
    , productTable products model
    ]


searchBar :: Model -> Html Msg
searchBar model =
  form
    []
    [ input
        [ type_ "text"
        , value model.filterText
        , placeholder "Search..."
        , onInput FilterTextChanged
        ]
    , label
        []
        [ input [type_ "checkbox", checked model.inStockOnly, onCheck InStockOnlyChanged]
        , text " Only show products in stock"
        ]
    ]


productTable :: [Product] -> Model -> Html msg
productTable allProducts model =
  table
    []
    [ thead [] [tr [] [th [] [text "Name"], th [] [text "Price"]]]
    , tbody [] (rows Nothing visible)
    ]
 where
  visible =
    [ item
    | item <- allProducts
    , map toLower model.filterText `isInfixOf` map toLower item.name
    , item.stocked || not model.inStockOnly
    ]

  -- A category row goes before the first item of each category.
  rows _ [] = []
  rows lastCategory (item : rest)
    | Just item.category /= lastCategory =
        keyed item.category (productCategoryRow item.category)
          : productRows item rest
    | otherwise = productRows item rest

  productRows item rest =
    keyed item.name (productRow item) : rows (Just item.category) rest


productCategoryRow :: String -> Html msg
productCategoryRow categoryName =
  tr [] [th [colSpan 2] [text categoryName]]


productRow :: Product -> Html msg
productRow item =
  tr
    []
    [ td
        []
        [ if item.stocked
            then text item.name
            else span [styles [("color", "red")]] [text item.name]
        ]
    , td [] [text item.price]
    ]
