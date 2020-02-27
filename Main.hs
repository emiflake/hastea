module Main where


import           Prelude        hiding (div, span)

import           Asterius.Types
import           Data.Coerce


import           DOM




-- GENERAL JS FFI


foreign import javascript "(${1}).toString()" unsafeShowJSVal :: JSVal -> JSString



-- LIB


example :: Html a
example =
  div [] [ span [] [ text "Hello, world" ]
         , strong [] [ text "Bold!" ]
         , a [ attrib "href" "https://google.com" ] [ text "Click me!" ]
         ]


main :: IO ()
main = do
  print example
  renderRootNode example
