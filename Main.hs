module Main where


import           Asterius.Types
import           Control.Monad
import           Data.Coerce
import           Prelude        hiding (div, init, span)

import           Async
import           Data.IORef
import           DOM


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
