{- | TodoMVC as one model, one update function and one view.

Everything the page shows is computed from 'Model', and the model only
changes in 'step'.
-}
module Example.TodoMVC.TodoMVC
  ( runTodoMVC
  )
where

import Hastea.App (runApp)
import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Html (Html)
import Hastea.Html qualified as H
import Hastea.Html.Attributes qualified as A
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
  H.div
    [A.className "todomvc-wrapper"]
    [ H.section
        [A.className "todoapp"]
        ( [viewHeader model.entry, viewMain model]
            <> [viewFooter model | not (Todos.isEmpty model.todos)]
        )
    , viewInfo
    ]


viewHeader :: String -> Html Msg
viewHeader entry =
  H.header
    [A.className "header"]
    [ H.h1 [] [H.text "Todos"]
    , H.input
        [ A.className "new-todo"
        , A.placeholder "What needs to be done?"
        , A.value entry
        , onInput EntryChanged
        , onKeyDown EntryKeyPressed
        ]
    ]


viewMain :: Model -> Html Msg
viewMain model =
  H.section
    [A.className "main", A.styles [("display", "block")]]
    [ H.input
        [ A.id_ "toggle-all"
        , A.className "toggle-all"
        , A.type_ "checkbox"
        , A.checked (Todos.allCompleted model.todos)
        , onClick AllToggled
        ]
    , H.label [A.for "toggle-all"] []
    , -- Typing in the entry field changes neither the filter nor the todos,
      -- so the list is then skipped entirely.
      H.lazy2 viewTodos model.filter model.todos
    ]


viewTodos :: Filter -> Todos -> Html Msg
viewTodos current todos =
  H.ul
    [A.className "todo-list"]
    [ -- The key ties each todo to its element, whatever the filter shows.
      H.keyed (show todoId) (viewTodo todoId todo)
    | (todoId, todo) <- Todos.visible current todos
    ]


viewTodo :: TodoId -> Todo -> Html Msg
viewTodo todoId todo =
  H.li
    [A.classList [("completed", todo.completed)]]
    [ H.div
        [A.className "view"]
        [ H.input
            [ A.className "toggle"
            , A.type_ "checkbox"
            , A.checked todo.completed
            , onClick (Toggled todoId)
            ]
        , H.label [] [H.text todo.label]
        ]
    ]


viewFooter :: Model -> Html Msg
viewFooter model =
  H.footer
    [A.className "footer"]
    ( [viewItemCount (Todos.remaining model.todos), viewFilters model.filter]
        <> [viewClearCompleted | Todos.anyCompleted model.todos]
    )


viewItemCount :: Int -> Html msg
viewItemCount count =
  H.span [A.className "todo-count"] [H.text (show count <> " " <> items <> " left")]
 where
  items = if count == 1 then "item" else "items"


viewFilters :: Filter -> Html Msg
viewFilters current =
  H.ul [A.className "filters"] [viewFilter current choice | choice <- [All, Active, Completed]]


viewFilter :: Filter -> Filter -> Html Msg
viewFilter current choice =
  H.li
    []
    [ H.a
        [A.classList [("selected", choice == current)], onClick (FilterSelected choice)]
        [H.text (filterName choice)]
    ]


filterName :: Filter -> String
filterName All = "All"
filterName Active = "Active"
filterName Completed = "Completed"


viewClearCompleted :: Html Msg
viewClearCompleted =
  H.button [A.className "clear-completed", onClick CompletedCleared] [H.text "Clear completed"]


viewInfo :: Html msg
viewInfo =
  H.footer
    [A.className "info"]
    [ H.p [] [H.text "Double-click to edit a todo"]
    , H.p
        []
        [ H.text "Created with "
        , H.a [A.href "https://github.com/emiflake/hastea"] [H.text "Hastea"]
        , H.text " and "
        , H.a
            [A.href "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html"]
            [H.text "GHC's WebAssembly backend"]
        ]
    ]
