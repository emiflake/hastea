module Example.TodoMVC.TodoMVC
  ( runTodoMVC
  )
where

import Prelude hiding (div, init, span)

import Hastea.App
import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Html
import Hastea.Html.Attributes
  ( checked
  , className
  , for
  , href
  , id_
  , placeholder
  , styles
  , type_
  , value
  )
import Hastea.Html.Events

import Example.TodoMVC.Todos (Todos)
import Example.TodoMVC.Todos qualified as Todos


data Model
  = Model
  { todos :: Todos
  , entry :: String
  , filter :: Todos.Filter
  }
  deriving (Show)


addTodo :: Model -> Model
addTodo model =
  model
    { todos = Todos.add model.entry model.todos
    , entry = ""
    }


init :: Model
init =
  Model
    { todos = Todos.empty
    , entry = ""
    , filter = Todos.All
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
      TodoEntryChange entry -> "TodoEntryChange " <> show entry
      TodoEntryKeypress kp -> "TodoEntryKeypress " <> show kp
      SetFilter newFilter -> "SetFilter " <> show newFilter
      ToggleAll -> "ToggleAll"
      ClearCompleted -> "ClearCompleted"


update :: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TodosUpdate updateTodos ->
      (model{todos = updateTodos model.todos}, Cmd.none)
    TodoEntryChange newEntry ->
      (model{entry = newEntry}, Cmd.none)
    TodoEntryKeypress "Enter"
      | not (null model.entry) ->
          (addTodo model, Cmd.none)
    SetFilter newFilter ->
      (model{filter = newFilter}, Cmd.none)
    ClearCompleted ->
      (model{todos = Todos.clearCompleted model.todos}, Cmd.none)
    ToggleAll ->
      (model{todos = Todos.toggleAll model.todos}, Cmd.none)
    _ ->
      (model, Cmd.none)


viewFilter :: Todos.Filter -> Todos.Filter -> Html Msg
viewFilter target current =
  let
    selected = target == current
   in
    li
      []
      [ a
          [ if selected then className "selected" else className ""
          , onClick (SetFilter target)
          ]
          [text . show $ target]
      ]


viewFilters :: Model -> Html Msg
viewFilters model =
  ul
    [className "filters"]
    [ viewFilter Todos.All model.filter
    , viewFilter Todos.Active model.filter
    , viewFilter Todos.Completed model.filter
    ]


viewClearCompleted :: Model -> [Html Msg]
viewClearCompleted model =
  if Todos.anyCompleted model.todos
    then
      [ button
          [ className "clear-completed"
          , onClick ClearCompleted
          ]
          [text "Clear completed"]
      ]
    else
      []


viewItemCount :: Int -> Html msg
viewItemCount count =
  let countLabel =
        case count of
          1 -> show count <> " item left"
          _ -> show count <> " items left"
   in span [className "todo-count"] [text countLabel]


viewFooter :: Model -> [Html Msg]
viewFooter model =
  if not (Todos.isEmpty model.todos)
    then
      [ footer
          [className "footer"]
          ( [ viewItemCount (Todos.count model.todos)
            , viewFilters model
            ]
              <> viewClearCompleted model
          )
      ]
    else
      []


view :: Model -> Html Msg
view model =
  div
    [className "todomvc-wrapper"]
    [ section
        [className "todoapp"]
        ( [ header
              [className "header"]
              [ h1 [] [text "Todos"]
              , input
                  [ className "new-todo"
                  , placeholder "What needs to be done?"
                  , onInput TodoEntryChange
                  , onKeypress TodoEntryKeypress
                  , value model.entry
                  ]
              ]
          , section
              [className "main", styles [("display", "block")]]
              [ input
                  [ id_ "toggle-all"
                  , className "toggle-all"
                  , type_ "checkbox"
                  , onClick ToggleAll
                  , checked (Todos.allCompleted model.todos)
                  ]
              , label [for "toggle-all"] []
              , -- Typing in the entry field changes neither the filter nor
                -- the todos, so the list is then skipped entirely.
                lazy3 Todos.viewTodos model.filter TodosUpdate model.todos
              ]
          ]
            <> (viewFooter model)
        )
    , footer
        [className "info"]
        [ p [] [text "Double-click to edit a todo"]
        , p
            []
            [ span [] [text "Created with "]
            , a [href "https://github.com/emiflake/hastea"] [text "Hastea"]
            , span [] [text " and "]
            , a
                [href "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html"]
                [text "GHC's WebAssembly backend"]
            ]
        ]
    ]


runTodoMVC :: IO ()
runTodoMVC =
  do
    putStrLn "-- TODO: MVC"
    runApp update init view
