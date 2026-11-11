{- | React: @useReducer@.
<https://react.dev/learn/extracting-state-logic-into-a-reducer>

React introduces a reducer as a way to tidy up a component with many event
handlers. In Hastea it is the only way to change state: 'update' is the
reducer and 'Msg' the actions.

The React version also has state outside the reducer: @AddTask@ keeps the
text being typed and each @Task@ whether it is being edited. Here those live
in the model as well ('draft' and 'editing').
-}
module ReactComparison.Tasks
  ( Model
  , Msg
  , initial
  , update
  , view
  )
where

import Prelude hiding (div)

import Hastea.Html
import Hastea.Html.Attributes (checked, placeholder, type_, value)
import Hastea.Html.Events (onCheck, onClick, onInput)


data Task = Task
  { taskId :: Int
  , taskText :: String
  , done :: Bool
  }


data Model = Model
  { tasks :: [Task]
  , nextId :: Int
  , draft :: String
  -- ^ The text in the "Add task" field.
  , editing :: Maybe Int
  -- ^ The task whose text is being edited.
  }


data Msg
  = Added
  | Changed Task
  | Deleted Int
  | DraftChanged String
  | EditStarted Int
  | EditFinished


initial :: Model
initial =
  Model
    { tasks =
        [ Task 0 "Visit Kafka Museum" True
        , Task 1 "Watch a puppet show" False
        , Task 2 "Lennon Wall pic" False
        ]
    , nextId = 3
    , draft = ""
    , editing = Nothing
    }


update :: Msg -> Model -> Model
update msg model = case msg of
  Added ->
    model
      { tasks = tasks model <> [Task (nextId model) (draft model) False]
      , nextId = nextId model + 1
      , draft = ""
      }
  Changed task ->
    model{tasks = [if taskId t == taskId task then task else t | t <- tasks model]}
  Deleted deletedId ->
    model{tasks = filter ((/= deletedId) . taskId) (tasks model)}
  DraftChanged newDraft -> model{draft = newDraft}
  EditStarted editedId -> model{editing = Just editedId}
  EditFinished -> model{editing = Nothing}


view :: Model -> Html Msg
view model =
  div
    []
    [ h1 [] [text "Prague itinerary"]
    , addTask (draft model)
    , ul
        []
        [ keyed (show (taskId task)) (li [] [viewTask (editing model == Just (taskId task)) task])
        | task <- tasks model
        ]
    ]


addTask :: String -> Html Msg
addTask currentDraft =
  div
    []
    [ input [placeholder "Add task", value currentDraft, onInput DraftChanged]
    , button [onClick Added] [text "Add"]
    ]


viewTask :: Bool -> Task -> Html Msg
viewTask isEditing task =
  label
    []
    ( [input [type_ "checkbox", checked (done task), onCheck (\isDone -> Changed task{done = isDone})]]
        <> content
        <> [button [onClick (Deleted (taskId task))] [text "Delete"]]
    )
 where
  content
    | isEditing =
        [ input [value (taskText task), onInput (\newText -> Changed task{taskText = newText})]
        , button [onClick EditFinished] [text "Save"]
        ]
    | otherwise =
        [ text (taskText task)
        , button [onClick (EditStarted (taskId task))] [text "Edit"]
        ]
