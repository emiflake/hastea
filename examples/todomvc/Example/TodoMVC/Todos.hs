-- | The list of todos and what can be done with it. No views here.
module Example.TodoMVC.Todos
  ( Todo (..)
  , TodoId
  , Todos
  , Filter (..)

    -- * Changing the list
  , empty
  , add
  , toggle
  , toggleAll
  , clearCompleted

    -- * Asking about the list
  , visible
  , isEmpty
  , remaining
  , anyCompleted
  , allCompleted
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)


data Todo = Todo
  { label :: Text
  , completed :: Bool
  }


type TodoId = Int


data Todos = Todos
  { items :: Map TodoId Todo
  , nextId :: TodoId
  }


-- | Which todos to show.
data Filter
  = All
  | Active
  | Completed
  deriving (Eq)


-- CHANGING THE LIST

empty :: Todos
empty = Todos{items = Map.empty, nextId = 0}


add :: Text -> Todos -> Todos
add newLabel todos =
  todos
    { items = Map.insert todos.nextId Todo{label = newLabel, completed = False} todos.items
    , nextId = todos.nextId + 1
    }


toggle :: TodoId -> Todos -> Todos
toggle todoId todos =
  todos{items = Map.adjust (\todo -> todo{completed = not todo.completed}) todoId todos.items}


-- | Complete every todo, or if they all are already, none.
toggleAll :: Todos -> Todos
toggleAll todos =
  todos{items = fmap (\todo -> todo{completed = not allDone}) todos.items}
 where
  allDone = allCompleted todos


clearCompleted :: Todos -> Todos
clearCompleted todos =
  todos{items = Map.filter (\todo -> not todo.completed) todos.items}


-- ASKING ABOUT THE LIST

-- | The todos that a filter lets through, oldest first.
visible :: Filter -> Todos -> [(TodoId, Todo)]
visible which todos =
  [(todoId, todo) | (todoId, todo) <- Map.toList todos.items, passes todo]
 where
  passes todo = case which of
    All -> True
    Active -> not todo.completed
    Completed -> todo.completed


isEmpty :: Todos -> Bool
isEmpty todos = Map.null todos.items


-- | How many todos are still to do.
remaining :: Todos -> Int
remaining todos = Map.size (Map.filter (\todo -> not todo.completed) todos.items)


anyCompleted :: Todos -> Bool
anyCompleted todos = any (.completed) todos.items


allCompleted :: Todos -> Bool
allCompleted todos = all (.completed) todos.items
