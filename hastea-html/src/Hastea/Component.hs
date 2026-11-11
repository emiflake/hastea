{- | Components: parts of a view that keep their own state.

A component bundles a state, the messages that change it, and a view of it.
It is placed in a view with 'use', and whatever shows the view keeps the
state, so the view that uses a component does not have to store or update
anything for it:

@
counter :: Component () Int CounterMsg out
counter =
  Component
    { initial = \\() -> 0
    , update = \\Pressed count -> (count + 1, [])
    , view = \\() count -> button [onClick Pressed] [text (show count)]
    , subscriptions = const Sub.none
    }

page :: Html msg
page = div [] [use counter (), use counter ()] -- two counters, counting separately
@

== Where the state lives

A component's state belongs to its place in the view: the same component
used in two places has two states, and a component that is no longer in the
view loses its state. In a list, give each item a key with
'Hastea.Html.keyed' so that the state follows the item when the list
changes. If a different component with the same type of state appears in
the very same place, it takes over the state.

== Talking to the surroundings

A component receives /props/ from the view that uses it, each time that view
is shown. It reports back with @'Emit'@: the type of 'use' makes those
outputs the messages of the surrounding view.
-}
module Hastea.Component
  ( Component (..)
  , Effect (..)
  , use
  )
where

import Data.Typeable (Typeable)

import Hastea.Html.Internal
import Hastea.Sub (Sub)


{- | A component that is given @props@, keeps a @state@, changes it in
response to messages @msg@, and reports @out@ to its surroundings.
-}
data Component props state msg out = Component
  { initial :: props -> state
  -- ^ The state of a component that has just appeared.
  , update :: msg -> state -> (state, [Effect msg out])
  -- ^ The next state when a message arrives, and what else should happen.
  , view :: props -> state -> Html msg
  , subscriptions :: state -> Sub msg
  -- ^ What to listen to, besides the view, while the state is like this.
  }


-- | Place a component in a view.
use :: (Typeable state, Typeable msg) => Component props state msg out -> props -> Html out
use component props =
  Embed . Mount $
    Embedded
      { initial = component.initial props
      , view = component.view props
      , update = component.update
      , subscriptions = component.subscriptions
      }
