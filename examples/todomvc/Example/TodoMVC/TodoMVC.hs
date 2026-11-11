{- | TodoMVC as one model, one update function and one view.

Everything the page shows is computed from 'Model', and the model only
changes in 'step'.
-}
module Example.TodoMVC.TodoMVC
  ( runTodoMVC
  )
where

import Prelude hiding (div, span)

import Hastea.App (runApp)
import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Html
import Hastea.Html.Attributes
  ( checked
  , classList
  , className
  , for
  , href
  , id_
  , placeholder
  , styles
  , type_
  , value
  )
import Hastea.Html.Events (onClick, onInput, onKeyDown)

import Example.TodoMVC.Todos (Filter (..), Todo, TodoId, Todos)
import Example.TodoMVC.Todos qualified as Todos


runTodoMVC :: IO ()
runTodoMVC = runApp update initial view


-- MODEL

data Model = Model
  { todos :: Todos
  , entry :: String
  -- ^ The text in the "What needs to be done?" field.
  , filter :: Filter
  }


initial :: Model
initial = Model{todos = Todos.empty, entry = "", filter = All}


-- UPDATE

-- | What the user can do.
data Msg
  = EntryChanged String
  | EntryKeyPressed String
  | Toggled TodoId
  | AllToggled
  | FilterSelected Filter
  | CompletedCleared


-- | Nothing here has effects, so there is never a command to run.
update :: Msg -> Model -> (Model, Cmd Msg)
update msg model = (step msg model, Cmd.none)


step :: Msg -> Model -> Model
step msg model = case msg of
  EntryChanged newEntry -> model{entry = newEntry}
  EntryKeyPressed "Enter"
    | not (null model.entry) -> model{todos = Todos.add model.entry model.todos, entry = ""}
  EntryKeyPressed _ -> model
  Toggled todoId -> model{todos = Todos.toggle todoId model.todos}
  AllToggled -> model{todos = Todos.toggleAll model.todos}
  FilterSelected newFilter -> model{filter = newFilter}
  CompletedCleared -> model{todos = Todos.clearCompleted model.todos}


-- VIEW

view :: Model -> Html Msg
view model =
  div
    [className "todomvc-wrapper"]
    [ section
        [className "todoapp"]
        ( [viewHeader model.entry, viewMain model]
            <> [viewFooter model | not (Todos.isEmpty model.todos)]
        )
    , viewInfo
    ]


viewHeader :: String -> Html Msg
viewHeader entry =
  header
    [className "header"]
    [ h1 [] [text "Todos"]
    , input
        [ className "new-todo"
        , placeholder "What needs to be done?"
        , value entry
        , onInput EntryChanged
        , onKeyDown EntryKeyPressed
        ]
    ]


viewMain :: Model -> Html Msg
viewMain model =
  section
    [className "main", styles [("display", "block")]]
    [ input
        [ id_ "toggle-all"
        , className "toggle-all"
        , type_ "checkbox"
        , checked (Todos.allCompleted model.todos)
        , onClick AllToggled
        ]
    , label [for "toggle-all"] []
    , -- Typing in the entry field changes neither the filter nor the todos,
      -- so the list is then skipped entirely.
      lazy2 viewTodos model.filter model.todos
    ]


viewTodos :: Filter -> Todos -> Html Msg
viewTodos current todos =
  ul
    [className "todo-list"]
    [ -- The key ties each todo to its element, whatever the filter shows.
      keyed (show todoId) (viewTodo todoId todo)
    | (todoId, todo) <- Todos.visible current todos
    ]


viewTodo :: TodoId -> Todo -> Html Msg
viewTodo todoId todo =
  li
    [classList [("completed", todo.completed)]]
    [ div
        [className "view"]
        [ input
            [ className "toggle"
            , type_ "checkbox"
            , checked todo.completed
            , onClick (Toggled todoId)
            ]
        , label [] [text todo.label]
        ]
    ]


viewFooter :: Model -> Html Msg
viewFooter model =
  footer
    [className "footer"]
    ( [viewItemCount (Todos.remaining model.todos), viewFilters model.filter]
        <> [viewClearCompleted | Todos.anyCompleted model.todos]
    )


viewItemCount :: Int -> Html msg
viewItemCount count =
  span [className "todo-count"] [text (show count <> (if count == 1 then " item left" else " items left"))]


viewFilters :: Filter -> Html Msg
viewFilters current =
  ul [className "filters"] [viewFilter current choice | choice <- [All, Active, Completed]]


viewFilter :: Filter -> Filter -> Html Msg
viewFilter current choice =
  li
    []
    [ a
        [classList [("selected", choice == current)], onClick (FilterSelected choice)]
        [text (filterName choice)]
    ]


filterName :: Filter -> String
filterName All = "All"
filterName Active = "Active"
filterName Completed = "Completed"


viewClearCompleted :: Html Msg
viewClearCompleted =
  button [className "clear-completed", onClick CompletedCleared] [text "Clear completed"]


viewInfo :: Html msg
viewInfo =
  footer
    [className "info"]
    [ p [] [text "Double-click to edit a todo"]
    , p
        []
        [ text "Created with "
        , a [href "https://github.com/emiflake/hastea"] [text "Hastea"]
        , text " and "
        , a
            [href "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html"]
            [text "GHC's WebAssembly backend"]
        ]
    ]
