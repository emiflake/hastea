{-# LANGUAGE NamedFieldPuns #-}
module Example.TodoMVC.TodoMVC
  ( runTodoMVC
  )
  where


import           Prelude     hiding (div, init, span)

import           Hastea.App
import           Hastea.Cmd  (Cmd)
import qualified Hastea.Cmd  as Cmd
import           Hastea.Html


import qualified Example.TodoMVC.Todos as Todos
import Example.TodoMVC.Todos (Todos, Todo)


data Model
  = Model
  { mTodos :: Todos
  , mTodoEntry :: String
  , mFilter :: Todos.Filter
  }
  deriving Show


addTodo :: Model -> Model
addTodo model@Model{mTodos, mTodoEntry} =
  model
  { mTodos = Todos.add mTodoEntry mTodos
  , mTodoEntry = ""
  }


init :: Model
init =
  Model
  { mTodos = Todos.empty
  , mTodoEntry = ""
  , mFilter = Todos.All
  }


data Msg
  = NoOp
  | TodosUpdate (Todos -> Todos)
  | TodoEntryChange String
  | TodoEntryKeypress String
  | SetFilter Todos.Filter
  | ClearCompleted
  | ToggleAll


-- Will get rid of this eventually.
instance Show Msg where
  show msg =
    case msg of
      NoOp -> "NoOp"
      TodosUpdate _ -> "TodosUpdate { internal }"
      TodoEntryChange s -> "TodoEntryChange " <> show s
      TodoEntryKeypress kp -> "TodoEntryKeypress " <> show kp
      SetFilter newFilter -> "SetFilter " <> show newFilter
      ToggleAll -> "ToggleAll"
      ClearCompleted -> "ClearCompleted"


update :: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TodosUpdate updateTodos ->
      ( model { mTodos = updateTodos (mTodos model) }, Cmd.none )
    TodoEntryChange s ->
      ( model { mTodoEntry = s }, Cmd.none )
    TodoEntryKeypress "Enter"
      | not . null . mTodoEntry $ model ->
      ( addTodo model, Cmd.none )
    SetFilter newFilter ->
      ( model { mFilter = newFilter }, Cmd.none )
    ClearCompleted ->
      ( model { mTodos = Todos.clearCompleted (mTodos model) }, Cmd.none )
    ToggleAll ->
      ( model { mTodos = Todos.toggleAll (mTodos model) }, Cmd.none )
    _ ->
      ( model, Cmd.none )


viewFilter :: Todos.Filter -> Todos.Filter -> Html Msg
viewFilter target current =
  let
    selected = target == current
  in
    li
      []
      [ a [ if selected then className "selected" else className ""
          , onClick (SetFilter target)
          ]
          [ text . show $ target ]
      ]

viewFilters :: Model -> Html Msg
viewFilters model =
  ul
    [ className "filters" ]
    [ viewFilter Todos.All (mFilter model)
    , viewFilter Todos.Active (mFilter model)
    , viewFilter Todos.Completed (mFilter model)
    ]


viewClearCompleted :: Model -> [Html Msg]
viewClearCompleted model =
  if Todos.anyCompleted (mTodos model)
  then
    [ button
        [ className "clear-completed"
        , onClick ClearCompleted
        ]
        [ text "Clear completed" ]
    ]
  else
    []


viewItemCount :: Int -> Html msg
viewItemCount count =
  let label =
        case count of
          1 -> show count <> " item left"
          _ -> show count <> " items left"
  in
    span [ className "todo-count" ] [ text label ]

viewFooter :: Model -> [Html Msg]
viewFooter model =
  if not . Todos.isEmpty . mTodos $ model
  then
    [ footer
        [ className "footer" ]
        ([ viewItemCount . Todos.count . mTodos $ model
        , viewFilters model
        ] <> viewClearCompleted model)
    ]
  else
    []

view :: Model -> Html Msg
view model =
  div
    [ className "todomvc-wrapper" ]
    [ section
        [ className "todoapp" ]
        ([ header
            [ className "header" ]
            [ h1 [] [ text "Todos" ]
            , input
                [ className "new-todo", placeholder "What needs to be done?"
                , onInput TodoEntryChange
                , onKeypress TodoEntryKeypress
                , value (mTodoEntry model)
                ]
                []
            ]
        , section
            [ className "main", style "display: block;" ] -- Would like to have `style n v` pairs
            [ input
                [ id_ "toggle-all"
                , className "toggle-all"
                , type_ "checkbox"
                , onClick ToggleAll
                , checked (Todos.allCompleted . mTodos $ model)
                ]
                []
            , label [ for "toggle-all" ] []
            , Todos.viewTodos (mFilter model) TodosUpdate (mTodos model)
            ]
        ]
        <> (viewFooter model))
    , footer
        [ className "info"]
        [ p [] [ text "Double-click to edit a todo" ]
        , p [] [ span [] [ text "Created with "]
               , a [ href "https://github.com/emiflake/hastea" ] [ text "Hastea" ]
               , span [] [ text " and " ]
               , a [ href "https://github.com/tweag/asterius" ] [ text "Asterius" ]
               ]
        ]
    ]


runTodoMVC :: IO ()
runTodoMVC =
  do
    putStrLn "-- TODO: MVC"
    runApp update init view
