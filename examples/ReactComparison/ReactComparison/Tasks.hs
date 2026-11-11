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


data Task = Task
  { id :: Int
  , text :: String
  , done :: Bool
  }


-- THE LIST

data Tasks = Tasks
  { all :: [Task]
  , nextId :: Int
  }


-- | The actions of the reducer.
data Msg
  = Added String
  | Changed Task
  | Deleted Int


taskApp :: Component () Tasks Msg out
taskApp = component initial update view
 where
  initial () =
    Tasks
      { all =
          [ Task 0 "Visit Kafka Museum" True
          , Task 1 "Watch a puppet show" False
          , Task 2 "Lennon Wall pic" False
          ]
      , nextId = 3
      }

  update msg tasks = (reducer msg tasks, [])

  view () tasks =
    div
      []
      [ h1 [] [text "Prague itinerary"]
      , Added <$> use addTask ()
      , ul [] [keyed (show task.id) (li [] [use taskRow task]) | task <- tasks.all]
      ]


reducer :: Msg -> Tasks -> Tasks
reducer msg tasks = case msg of
  Added newText ->
    tasks
      { all = tasks.all <> [Task tasks.nextId newText False]
      , nextId = tasks.nextId + 1
      }
  Changed changed ->
    tasks{all = [if task.id == changed.id then changed else task | task <- tasks.all]}
  Deleted deletedId ->
    tasks{all = [task | task <- tasks.all, task.id /= deletedId]}


-- ADDING A TASK

data AddTaskMsg
  = Typed String
  | AddPressed


-- | Keeps the text being typed, and emits it when "Add" is pressed.
addTask :: Component () String AddTaskMsg String
addTask = component initial update view
 where
  initial () = ""

  update msg draft = case msg of
    Typed newDraft -> (newDraft, [])
    AddPressed -> ("", [Emit draft])

  view () draft =
    div
      []
      [ input [placeholder "Add task", value draft, onInput Typed]
      , button [onClick AddPressed] [text "Add"]
      ]


-- ONE TASK

data RowMsg
  = EditPressed
  | SavePressed
  | Edited Task
  | DeletePressed Int


-- | Shows the task it is given, and keeps whether it is being edited.
taskRow :: Component Task Bool RowMsg Msg
taskRow = component initial update view
 where
  initial _ = False

  update msg isEditing = case msg of
    EditPressed -> (True, [])
    SavePressed -> (False, [])
    Edited task -> (isEditing, [Emit (Changed task)])
    DeletePressed taskId -> (isEditing, [Emit (Deleted taskId)])

  view task isEditing =
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
