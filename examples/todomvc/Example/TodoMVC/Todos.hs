{-# LANGUAGE NamedFieldPuns #-}

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
import Prelude hiding (div, span)


-- MODEL

newtype TodoId
  = TodoId {unTodoId :: Int}
  deriving (Show, Eq, Ord)


data Todo
  = Todo
  { todoLabel :: String
  , todoCompleted :: Bool
  }
  deriving (Show)


data Filter
  = All
  | Active
  | Completed
  deriving (Show, Eq)


todoToggle :: Todo -> Todo
todoToggle todo@Todo{todoCompleted} =
  todo{todoCompleted = not todoCompleted}


data Todos
  = Todos
  { todosMap :: Map TodoId Todo
  , todosCounter :: Int
  }
  deriving (Show)


add :: String -> Todos -> Todos
add todoLabel todos =
  let
    newTodo =
      Todo{todoLabel, todoCompleted = False}
    freshId =
      TodoId (todosCounter todos)
   in
    todos
      { todosMap = Map.insert freshId newTodo (todosMap todos)
      , todosCounter = succ $ todosCounter todos
      }


toggleTodo :: TodoId -> Todos -> Todos
toggleTodo todoId todos =
  todos
    { todosMap =
        Map.adjust
          todoToggle
          todoId
          (todosMap todos)
    }


empty :: Todos
empty =
  Todos
    { todosMap = Map.empty
    , todosCounter = 0
    }


isEmpty :: Todos -> Bool
isEmpty =
  Map.null . todosMap


count :: Todos -> Int
count =
  Map.size . Map.filter (not . todoCompleted) . todosMap


clearCompleted :: Todos -> Todos
clearCompleted todos@Todos{todosMap} =
  todos
    { todosMap = Map.filter (not . todoCompleted) todosMap
    }


anyCompleted :: Todos -> Bool
anyCompleted =
  not . Map.null . Map.filter todoCompleted . todosMap


allCompleted :: Todos -> Bool
allCompleted =
  all todoCompleted . Map.elems . todosMap


toggleAll :: Todos -> Todos
toggleAll todos =
  let
    setAll completed items =
      fmap (\todo -> todo{todoCompleted = completed}) items
    newTodos =
      if not $ allCompleted todos
        then
          setAll True (todosMap todos)
        else
          setAll False (todosMap todos)
   in
    todos
      { todosMap = newTodos
      }


-- VIEW

viewTodo :: msg -> Todo -> Html msg
viewTodo toggle todo =
  li
    [if todoCompleted todo then className "completed" else className ""]
    [ div
        [className "view"]
        [ input
            [ className "toggle"
            , type_ "checkbox"
            , onClick toggle
            ]
            []
        , label [] [text (todoLabel todo)]
        ]
    ]


shouldView :: Filter -> Todo -> Bool
shouldView currentFilter Todo{todoCompleted} =
  case currentFilter of
    Completed -> todoCompleted
    All -> True
    Active -> not todoCompleted


viewTodos :: Filter -> ((Todos -> Todos) -> msg) -> Todos -> Html msg
viewTodos currentFilter updateTodos todos =
  ul
    [className "todo-list"]
    [ viewTodo (updateTodos $ \currentTodos -> toggleTodo todoId currentTodos) todo
    | (todoId, todo) <- Map.assocs (todosMap todos)
    , shouldView currentFilter todo
    ]
