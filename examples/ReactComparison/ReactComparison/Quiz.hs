{- | React: asynchronous work in an event handler.
<https://react.dev/learn/reacting-to-input-with-state>

The React handler is an @async@ function that sets state, awaits the
request, then sets state again. Here that is two messages: 'Submitted'
starts the request with a command, and 'Answered' arrives when it is done.
What the form looks like in between is whatever the view makes of the state.
-}
module ReactComparison.Quiz
  ( quiz
  )
where

import Prelude hiding (div)

import Data.Char (toLower)

import Hastea.Cmd (Cmd)
import Hastea.Component
import Hastea.Html
import Hastea.Html.Attributes (className, disabled, value)
import Hastea.Html.Events (onInput, onSubmit)
import Hastea.Time qualified as Time


data Quiz = Quiz
  { answer :: String
  , status :: Status
  , problem :: Maybe String
  -- ^ What was wrong with the last answer.
  }


data Status = Typing | Submitting | Success
  deriving (Eq)


data Msg
  = AnswerChanged String
  | Submitted
  | Answered (Either String ())


quiz :: Component () Quiz Msg out
quiz = component initial update view
 where
  initial () = Quiz{answer = "", status = Typing, problem = Nothing}

  update msg state = case msg of
    AnswerChanged newAnswer -> (state{answer = newAnswer}, [])
    Submitted -> (state{status = Submitting}, [Run (submitForm state.answer)])
    Answered (Right ()) -> (state{status = Success}, [])
    Answered (Left message) -> (state{status = Typing, problem = Just message}, [])

  view () state
    | state.status == Success = h1 [] [text "That's right!"]
    | otherwise = viewForm state


-- | Pretend it's hitting the network.
submitForm :: String -> Cmd Msg
submitForm answer = Time.after 1500 (Answered result)
 where
  result
    | map toLower answer == "lima" = Right ()
    | otherwise = Left "Good guess but a wrong answer. Try again!"


viewForm :: Quiz -> Html Msg
viewForm state =
  div
    []
    [ h2 [] [text "City quiz"]
    , p [] [text "In which city is there a billboard that turns air into drinkable water?"]
    , form
        [onSubmit Submitted]
        ( [ textarea [value state.answer, onInput AnswerChanged, disabled submitting] []
          , br []
          , button [disabled (null state.answer || submitting)] [text "Submit"]
          ]
            <> [p [className "Error"] [text message] | Just message <- [state.problem]]
        )
    ]
 where
  submitting = state.status == Submitting
