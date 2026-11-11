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
  { id :: Int
  , text :: String
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
      { tasks = model.tasks <> [Task model.nextId model.draft False]
      , nextId = model.nextId + 1
      , draft = ""
      }
  Changed task ->
    model{tasks = [if t.id == task.id then task else t | t <- model.tasks]}
  Deleted deletedId ->
    model{tasks = filter (\task -> task.id /= deletedId) model.tasks}
  DraftChanged newDraft -> model{draft = newDraft}
  EditStarted editedId -> model{editing = Just editedId}
  EditFinished -> model{editing = Nothing}


view :: Model -> Html Msg
view model =
  div
    []
    [ h1 [] [text "Prague itinerary"]
    , addTask model.draft
    , ul
        []
        [ keyed (show task.id) (li [] [viewTask (model.editing == Just task.id) task])
        | task <- model.tasks
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
    ( [input [type_ "checkbox", checked task.done, onCheck (\isDone -> Changed task{done = isDone})]]
        <> content
        <> [button [onClick (Deleted task.id)] [text "Delete"]]
    )
 where
  content
    | isEditing =
        [ input [value task.text, onInput (\newText -> Changed task{text = newText})]
        , button [onClick EditFinished] [text "Save"]
        ]
    | otherwise =
        [ text task.text
        , button [onClick (EditStarted task.id)] [text "Edit"]
        ]
