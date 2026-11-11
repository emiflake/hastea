{- | React: @useEffect@ to fetch data.
<https://react.dev/reference/react/useEffect#fetching-data-with-effects>

The React version has an effect that runs when the component appears and
whenever @person@ changes, with a cleanup function that makes it ignore a
response that arrives too late. Here the request is started in the two
places where that happens: 'onMount', and @update@ when another person is
chosen. A response says who it is about, so a late one is recognised by
looking at it.
-}
module ReactComparison.Bio
  ( bioPage
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Hastea.Cmd (Cmd)
import Hastea.Component
import Hastea.Html qualified as H
import Hastea.Html.Attributes qualified as A
import Hastea.Html.Events (onChange)
import Hastea.Http qualified as Http


data Page = Page
  { person :: Text
  , bio :: Maybe Text
  -- ^ 'Nothing' while it is being fetched.
  }


data Msg
  = PersonChanged Text
  | BioFetched Text (Either Http.Error Text)


bioPage :: Component () Page Msg out
bioPage = (component initial update view){onMount}
 where
  initial () = Page{person = "Alice", bio = Nothing}

  onMount () = [Run (fetchBio "Alice")]

  update msg page = case msg of
    PersonChanged person -> (Page{person, bio = Nothing}, [Run (fetchBio person)])
    BioFetched person result
      | person == page.person -> (page{bio = Just (either describe id result)}, [])
      -- A response for someone who is no longer selected.
      | otherwise -> (page, [])

  view () page =
    H.fragment
      [ H.select
          [A.value page.person, onChange PersonChanged]
          [H.option [A.value name] [H.text name] | name <- ["Alice", "Bob", "Taylor"]]
      , H.hr []
      , H.p [] [H.i [] [H.text (fromMaybe "Loading..." page.bio)]]
      ]

  describe problem = "Could not fetch the bio: " <> T.pack (show problem)


fetchBio :: Text -> Cmd Msg
fetchBio person = Http.get ("bios/" <> person <> ".txt") (BioFetched person)
