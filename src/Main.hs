module Main where

import           Prelude     hiding (div, init, span)

import           Hastea.App
import           Hastea.Cmd  (Cmd)
import qualified Hastea.Cmd  as Cmd
import           Hastea.Html

import Data.Char

data Msg
  = Change String
  deriving Show

type Model = String

view :: String -> Html Msg
view val  =
  div []
      [ input [ onInput Change, value val ] [ ]
      , p [] [ text val ]
      , p [] [ text (toUpper <$> val) ]
      , p [] [ text (toLower <$> val) ]
      ]


init :: Model
init = ""

update :: Msg -> Model -> (Model, Cmd Msg)
update (Change c) _ = (c, Cmd.none)

main :: IO ()
main = runApp update init view
