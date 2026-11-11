{- | React: asynchronous work in an event handler.
<https://react.dev/learn/reacting-to-input-with-state>

The React handler is an @async@ function that sets state, awaits the
request, then sets state again. Here that is two messages: 'Submitted'
starts the request with a command, and 'Answered' arrives when it is done.
What the form looks like in between is whatever 'view' makes of the model.
-}
module ReactComparison.Quiz
  ( Model
  , Msg
  , initial
  , update
  , view
  )
where

import Prelude hiding (div)

import Data.Char (toLower)

import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Html
import Hastea.Html.Attributes (className, disabled, value)
import Hastea.Html.Events (onInput, onSubmit)
import Hastea.Time qualified as Time


data Status = Typing | Submitting | Success
  deriving (Eq)


data Model = Model
  { answer :: String
  , problem :: Maybe String
  , status :: Status
  }


data Msg
  = AnswerChanged String
  | Submitted
  | Answered (Either String ())


initial :: Model
initial = Model{answer = "", problem = Nothing, status = Typing}


update :: Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  AnswerChanged newAnswer -> (model{answer = newAnswer}, Cmd.none)
  Submitted -> (model{status = Submitting}, submitForm model.answer)
  Answered (Right ()) -> (model{status = Success}, Cmd.none)
  Answered (Left message) -> (model{status = Typing, problem = Just message}, Cmd.none)


-- | Pretend it's hitting the network.
submitForm :: String -> Cmd Msg
submitForm given = Time.after 1500 (Answered result)
 where
  result
    | map toLower given == "lima" = Right ()
    | otherwise = Left "Good guess but a wrong answer. Try again!"


view :: Model -> Html Msg
view model
  | model.status == Success = h1 [] [text "That's right!"]
  | otherwise =
      div
        []
        [ h2 [] [text "City quiz"]
        , p [] [text "In which city is there a billboard that turns air into drinkable water?"]
        , form
            [onSubmit Submitted]
            ( [ textarea
                  [ value model.answer
                  , onInput AnswerChanged
                  , disabled (model.status == Submitting)
                  ]
                  []
              , br []
              , button
                  [disabled (null model.answer || model.status == Submitting)]
                  [text "Submit"]
              ]
                <> [p [className "Error"] [text message] | Just message <- [model.problem]]
            )
        ]
