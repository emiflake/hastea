{- | Commands that make HTTP requests.

@
data Msg = Loaded (Either Http.Error [String])

load :: Cmd Msg
load = Http.getJson "/names.json" (Decode.list Decode.string) Loaded
@
-}
module Hastea.Http
  ( -- * Simple requests
    get
  , getJson

    -- * Any request
  , Request (..)
  , request
  , requestJson
  , Error (..)
  )
where

import Hastea.Cmd (Cmd)
import Hastea.Cmd qualified as Cmd
import Hastea.Decode (Decoder, runDecode)
import Hastea.Internal.Foreign.FFI (makeHaskellCallback1)
import Hastea.Internal.Foreign.Types


-- | A request to make.
data Request = Request
  { method :: String
  -- ^ For example @\"GET\"@ or @\"POST\"@.
  , url :: String
  , headers :: [(String, String)]
  , body :: Maybe String
  }


-- | Why a request did not give a result.
data Error
  = -- | The server could not be reached, or the browser refused the request.
    NetworkError String
  | -- | The server answered with a status other than 2xx. Holds the status
    -- and the body of the response.
    BadStatus Int String
  | -- | The response was not what was expected: not JSON, or not of the
    -- right shape.
    BadBody String
  deriving (Eq, Show)


foreign import javascript unsafe "new Headers()" ffi_newHeaders :: IO JSVal
foreign import javascript unsafe "($1).append($2, $3)"
  ffi_appendHeader :: JSVal -> JSString -> JSString -> IO ()


-- An empty body is passed as null.
foreign import javascript unsafe "fetch($1, { method: $2, headers: $3, body: $4 ?? undefined }).then(async (response) => ($5)({ status: response.status, body: await response.text() })).catch((error) => ($6)(String(error)))"
  ffi_fetch :: JSString -> JSString -> JSVal -> JSVal -> JSFunction -> JSFunction -> IO ()
foreign import javascript unsafe "null" ffi_null :: JSVal
foreign import javascript unsafe "($1).status" ffi_status :: JSObject -> Int
foreign import javascript unsafe "($1).body" ffi_body :: JSObject -> JSString
foreign import javascript unsafe "$1" ffi_asString :: JSObject -> JSString


-- Either the parsed value in an array, or the reason it is not JSON.
foreign import javascript unsafe "(() => { try { return [JSON.parse($1)]; } catch (error) { return String(error); } })()"
  ffi_parseJson :: JSString -> JSVal
foreign import javascript unsafe "Array.isArray($1)" ffi_isArray :: JSVal -> Bool
foreign import javascript unsafe "($1)[0]" ffi_first :: JSVal -> JSVal
foreign import javascript unsafe "$1" ffi_toString :: JSVal -> JSString


-- | Fetch a URL and pass on the body of the response as text.
get :: String -> (Either Error String -> msg) -> Cmd msg
get = request . simple


-- | Fetch a URL and decode the response as JSON.
getJson :: String -> Decoder a -> (Either Error a -> msg) -> Cmd msg
getJson = requestJson . simple


simple :: String -> Request
simple url = Request{method = "GET", url, headers = [], body = Nothing}


-- | Make a request and pass on the body of the response as text.
request :: Request -> (Either Error String -> msg) -> Cmd msg
request details toMsg = Cmd.custom $ \dispatch -> do
  headers <- ffi_newHeaders
  mapM_ (\(name, value) -> ffi_appendHeader headers (toJSString name) (toJSString value)) details.headers
  onResponse <- makeHaskellCallback1 $ \response ->
    let status = ffi_status response
        text = fromJSString (ffi_body response)
     in dispatch . toMsg $
          if status >= 200 && status < 300 then Right text else Left (BadStatus status text)
  onFailure <- makeHaskellCallback1 $ \reason ->
    dispatch (toMsg (Left (NetworkError (fromJSString (ffi_asString reason)))))
  ffi_fetch
    (toJSString details.url)
    (toJSString details.method)
    headers
    (maybe ffi_null (\text -> let JSString val = toJSString text in val) details.body)
    onResponse
    onFailure


-- | Make a request and decode the response as JSON.
requestJson :: Request -> Decoder a -> (Either Error a -> msg) -> Cmd msg
requestJson details decoder toMsg = request details (toMsg . (>>= decode))
 where
  decode text =
    let parsed = ffi_parseJson (toJSString text)
     in if ffi_isArray parsed
          then either (Left . BadBody) Right (runDecode decoder (ffi_first parsed))
          else Left (BadBody (fromJSString (ffi_toString parsed)))
