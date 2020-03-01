module Main where

import           Prelude     hiding (div, init, span)

import           Hastea.App
import           Hastea.Cmd  (Cmd)
import qualified Hastea.Cmd  as Cmd
import           Hastea.Html


data Msg
  = Change String
  deriving Show

type Model = String

view :: String -> Html Msg
view val  =
  div []
      [ input [ onInput Change, value val ] [ ]
      , text val
      , text val
      , text val
      , text val
      , text val
      , text val
      , text val
      ]


init :: Model
init = ""

update :: Msg -> Model -> (Model, Cmd Msg)
update (Change c) _ = (c, Cmd.none)

main :: IO ()
main = runApp update init view
