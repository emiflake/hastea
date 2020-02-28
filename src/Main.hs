module Main where

import           Prelude     hiding (div, init, span)

import           Hastea.App
import           Hastea.Html

data Msg
  = Increment
  | Decrement
  deriving Show

type Model = Int

view :: Int -> Html Msg
view counter =
  div []
    [ span [] [ text (show counter) ]
    , button [ onClick Increment ] [ text "Increment" ]
    , button [ onClick Decrement ] [ text "Decrement" ]
    ]

init :: Model
init = 42

update :: Msg -> Model -> Model
update Increment = succ
update Decrement = pred


main :: IO ()
main = runApp update init view
