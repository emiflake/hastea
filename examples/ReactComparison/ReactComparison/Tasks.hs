{- | React: @useReducer@, and components with state of their own.
<https://react.dev/learn/extracting-state-logic-into-a-reducer>

React introduces a reducer as a way to tidy up a component with many event
handlers. Here it is how every component changes its state: @update@ is the
reducer and the message type the actions.

As in the React version, the text being typed belongs to 'addTask' and
whether a task is being edited belongs to its 'taskRow'. Both tell 'taskApp'
what happened by emitting one of its messages.
-}
module ReactComparison.Tasks
  ( taskApp
  )
where

import Prelude hiding (div)

import Hastea.Component
import Hastea.Html
import Hastea.Html.Attributes (checked, placeholder, type_, value)
import Hastea.Html.Events (onCheck, onClick, onInput)
import Hastea.Sub qualified as Sub


data Task = Task
  { id :: Int
  , text :: String
  , done :: Bool
  }


data Model = Model
  { tasks :: [Task]
  , nextId :: Int
  }


-- | The actions of the reducer.
data Msg
  = Added String
  | Changed Task
  | Deleted Int


taskApp :: Component () Model Msg out
taskApp =
  Component
    { initial = \() ->
        Model
          { tasks =
              [ Task 0 "Visit Kafka Museum" True
              , Task 1 "Watch a puppet show" False
              , Task 2 "Lennon Wall pic" False
              ]
          , nextId = 3
          }
    , update = \msg model -> (reducer msg model, [])
    , view = \() model ->
        div
          []
          [ h1 [] [text "Prague itinerary"]
          , Added <$> use addTask ()
          , ul [] [keyed (show task.id) (li [] [use taskRow task]) | task <- model.tasks]
          ]
    , subscriptions = const Sub.none
    }


reducer :: Msg -> Model -> Model
reducer msg model = case msg of
  Added newText ->
    model
      { tasks = model.tasks <> [Task model.nextId newText False]
      , nextId = model.nextId + 1
      }
  Changed task ->
    model{tasks = [if t.id == task.id then task else t | t <- model.tasks]}
  Deleted deletedId ->
    model{tasks = filter (\task -> task.id /= deletedId) model.tasks}


data AddTaskMsg
  = Typed String
  | AddPressed


-- | Keeps the text being typed, and emits it when "Add" is pressed.
addTask :: Component () String AddTaskMsg String
addTask =
  Component
    { initial = \() -> ""
    , update = \msg draft -> case msg of
        Typed newDraft -> (newDraft, [])
        AddPressed -> ("", [Emit draft])
    , view = \() draft ->
        div
          []
          [ input [placeholder "Add task", value draft, onInput Typed]
          , button [onClick AddPressed] [text "Add"]
          ]
    , subscriptions = const Sub.none
    }


data RowMsg
  = EditPressed
  | SavePressed
  | Edited Task
  | DeletePressed Int


-- | Shows the task it is given, and keeps whether it is being edited.
taskRow :: Component Task Bool RowMsg Msg
taskRow =
  Component
    { initial = \_ -> False
    , update = \msg isEditing -> case msg of
        EditPressed -> (True, [])
        SavePressed -> (False, [])
        Edited task -> (isEditing, [Emit (Changed task)])
        DeletePressed deletedId -> (isEditing, [Emit (Deleted deletedId)])
    , view = \task isEditing ->
        label
          []
          [ input [type_ "checkbox", checked task.done, onCheck (\isDone -> Edited task{done = isDone})]
          , if isEditing
              then input [value task.text, onInput (\newText -> Edited task{text = newText})]
              else text task.text
          , if isEditing
              then button [onClick SavePressed] [text "Save"]
              else button [onClick EditPressed] [text "Edit"]
          , button [onClick (DeletePressed task.id)] [text "Delete"]
          ]
    , subscriptions = const Sub.none
    }
