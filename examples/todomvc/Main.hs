module Main where

import Example.TodoMVC.TodoMVC (runTodoMVC)


main :: IO ()
main = runTodoMVC


-- The wasm module is linked as a WASI reactor, so there is no implicit entry
-- point: index.js calls this export once the module is initialised.
foreign export javascript "hs_start" main :: IO ()
