module Example.TodoMVC.Todos
  ( add
  , viewTodo
  , viewTodos
  , empty
  , isEmpty
  , count
  , clearCompleted
  , anyCompleted
  , allCompleted
  , toggleAll
  , Todo
  , Todos
  , Filter (..)
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hastea.Html
import Hastea.Html.Attributes (checked, className, type_)
import Hastea.Html.Events
import Prelude hiding (div, span)


-- MODEL

newtype TodoId
  = TodoId Int
  deriving (Show, Eq, Ord)


data Todo
  = Todo
  { label :: String
  , completed :: Bool
  }
  deriving (Show)


data Filter
  = All
  | Active
  | Completed
  deriving (Show, Eq)


todoToggle :: Todo -> Todo
todoToggle todo =
  todo{completed = not todo.completed}


data Todos
  = Todos
  { items :: Map TodoId Todo
  , counter :: Int
  }
  deriving (Show)


add :: String -> Todos -> Todos
add newLabel todos =
  let
    newTodo =
      Todo{label = newLabel, completed = False}
    freshId =
      TodoId todos.counter
   in
    todos
      { items = Map.insert freshId newTodo todos.items
      , counter = succ todos.counter
      }


toggleTodo :: TodoId -> Todos -> Todos
toggleTodo todoId todos =
  todos
    { items =
        Map.adjust
          todoToggle
          todoId
          todos.items
    }


empty :: Todos
empty =
  Todos
    { items = Map.empty
    , counter = 0
    }


isEmpty :: Todos -> Bool
isEmpty todos =
  Map.null todos.items


count :: Todos -> Int
count todos =
  Map.size (Map.filter (\todo -> not todo.completed) todos.items)


clearCompleted :: Todos -> Todos
clearCompleted todos =
  todos
    { items = Map.filter (\todo -> not todo.completed) todos.items
    }


anyCompleted :: Todos -> Bool
anyCompleted todos =
  any (.completed) todos.items


allCompleted :: Todos -> Bool
allCompleted todos =
  all (.completed) todos.items


toggleAll :: Todos -> Todos
toggleAll todos =
  let
    setAll isCompleted =
      fmap (\todo -> todo{completed = isCompleted})
    newTodos =
      if not $ allCompleted todos
        then
          setAll True todos.items
        else
          setAll False todos.items
   in
    todos
      { items = newTodos
      }


-- VIEW

viewTodo :: msg -> Todo -> Html msg
viewTodo toggle todo =
  li
    [if todo.completed then className "completed" else className ""]
    [ div
        [className "view"]
        [ input
            [ className "toggle"
            , type_ "checkbox"
            , checked todo.completed
            , onClick toggle
            ]
        , label [] [text todo.label]
        ]
    ]


shouldView :: Filter -> Todo -> Bool
shouldView currentFilter todo =
  case currentFilter of
    Completed -> todo.completed
    All -> True
    Active -> not todo.completed


viewTodos :: Filter -> ((Todos -> Todos) -> msg) -> Todos -> Html msg
viewTodos currentFilter updateTodos todos =
  ul
    [className "todo-list"]
    [ -- The key ties each item to its DOM node, whatever the filter shows.
      keyed (show number) $
        viewTodo (updateTodos $ \currentTodos -> toggleTodo todoId currentTodos) todo
    | (todoId@(TodoId number), todo) <- Map.assocs todos.items
    , shouldView currentFilter todo
    ]
