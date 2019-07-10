{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Functor.Identity
import Data.Kind (Type)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Trasa.Core
import Trasa.Server
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as B
import qualified Trasa.Method as Method

-- Our route data type. We define this ourselves.
data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where
  HelloWorld :: Route 
    '[ByteString] -- ^ now the path captures the first piece as a ByeString
    '[('Optional ByteString)] -- ^ there is an optional query parameter, decoded as a ByteString
    'Bodyless -- ^ the route does not have a request body
    ByteString -- ^ the response body will be `ByteString`

bodyText :: BodyCodec ByteString
bodyText = BodyCodec 
  (pure "text/html; charset=utf-8") -- ^ NonEmpty list of the HTTP media type names.
  id -- ^ encode from ByteString to ByteString
  Right -- ^ decode from ByteString to (Either Text ByteString)

bytestring :: CaptureCodec ByteString
bytestring = CaptureCodec (TE.decodeUtf8 . B.toStrict) (Just . B.fromStrict . TE.encodeUtf8)

-- | metadata about our routes: value level functions and data for constructing
--   and decoding paths
meta :: Route captures queries request response -> MetaCodec captures queries request response
meta route = case route of
  HelloWorld -> Meta 
    (capture bytestring ./ end) -- ^ match "/hello"
    (optional "b" bytestring .& qend) -- ^ no query parameters
    bodyless -- ^ no request body
    (resp (one bodyText)) -- ^ response body is one BodyCodec: our bodyText function above
    Method.get -- ^ http method: GET

-- | this function defines how we handle routes with our web server:
--   what actions we perform based on the route and its captures & queries
routes
  :: forall captures queries request response.
     Route captures queries request response -- ^ our route GADT, polymorphic over its type variables
  -> Rec Identity captures -- ^ an extensible record of the captures for this route
  -> Rec Parameter queries -- ^ an extensible record of the captures for this route
  -> RequestBody Identity request -- ^ the request body
  -> TrasaT IO response -- ^ our response
routes route captures queries reqBody = case route of
  HelloWorld -> go helloWorld
  where
  -- | this helper function uses the `handler` function to unwrap the `Arguments` type family.
  go :: Arguments captures queries request (TrasaT IO response) -> TrasaT IO response
  go f = handler captures queries reqBody f

helloWorld :: ByteString -> Maybe ByteString -> TrasaT IO ByteString
helloWorld a (Just b) = pure $ a <> ", " <> b <> "!"
helloWorld a Nothing = pure a

-- | We define a list of all the routes for our server for wai
allRoutes :: [Constructed Route]
allRoutes = [Constructed HelloWorld]

-- | Another implimentaiton detail: this creates the data structure used to do routing
router :: Router Route
router = routerWith (mapMeta captureDecoding captureDecoding id id . meta) allRoutes

-- | `wai` application
application :: Application
application = serveWith
  (metaCodecToMetaServer . meta) -- ^ implimentaiton detail: this just marshals some types
  routes -- ^ routes function defined above
  router -- ^ router function defined above

main :: IO ()
main = run 8080 (logStdoutDev application)

