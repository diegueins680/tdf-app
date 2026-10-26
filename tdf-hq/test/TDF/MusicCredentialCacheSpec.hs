{-# LANGUAGE OverloadedStrings #-}

module TDF.MusicCredentialCacheSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value(..), object, (.=))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Network.HTTP.Types
  ( Method, Status, hCacheControl, status200, status201, status400, status401
  , status403, status404, status409, status429, status500, status503
  )
import Network.HTTP.Types.URI (decodePathSegments)
import Network.Wai
  ( Application, Middleware, Request, Response, defaultRequest, mapResponseHeaders
  , pathInfo, queryString, rawPathInfo, rawQueryString, requestMethod, responseHeaders
  , responseLBS, responseStatus, responseToStream
  )
import Network.Wai.Internal (ResponseReceived(..))
import Servant (NoContent(..), Proxy(..), serve, (:<|>)(..))
import Test.Hspec

import TDF.API.MusicRelease (MusicReleasePublicAPI)
import TDF.App.FailureBoundary (requestExceptionBoundary)
import TDF.App.StartupResponse (startupApp)
import TDF.MusicRelease.CredentialCache (musicCredentialCache)

-- Independent operation inventory from TDF.API.MusicRelease. GET operations
-- also accept HEAD; captures deliberately differ so position mistakes surface.
credentialRoutes :: [(Method, [Text])]
credentialRoutes =
  [ assetRoute
  , ("POST", ["music", "entitlements", "entitlement-id", "download"])
  , ("POST", ["music", "downloads", "free"])
  , ("POST", ["music", "releases", "release-id", "versions", "version-id", "uploads"])
  , ("GET", ["music", "uploads", "upload-id", "parts", "2"])
  , ("GET", ["music", "uploads", "upload-id", "completion"])
  , ("DELETE", ["music", "uploads", "upload-id"])
  , ("GET", ["music", "ddex", "exports", "export-id", "download"])
  ]

assetRoute :: (Method, [Text])
assetRoute = ("GET", ["music", "assets", "asset-id", "access"])

readRoutes :: [(Method, [Text])]
readRoutes = [("HEAD", path) | ("GET", path) <- credentialRoutes]

requestFor :: (Method, [Text]) -> Request
requestFor (method, path) = defaultRequest { requestMethod = method, pathInfo = path }

body :: BL.ByteString
body = "{\"url\":\"https://storage.example.test/synthetic?grant=TEST_ONLY\","
  <> "\"expiresAt\":\"2026-10-26T00:05:00Z\"}"

upstream :: Status -> Application
upstream status _ send = send $ responseLBS status
  [ ("cache-control", "public, max-age=3600")
  , ("CACHE-CONTROL", "s-maxage=86400")
  , ("Content-Type", "application/json")
  , ("Set-Cookie", "synthetic-a=1; HttpOnly")
  , ("Set-Cookie", "synthetic-b=2; HttpOnly")
  , ("Access-Control-Allow-Origin", "https://ui.example.test")
  , ("Access-Control-Allow-Credentials", "true")
  , ("Vary", "Origin, Cookie")
  , ("ETag", "synthetic-etag")
  , ("Location", "/synthetic-result")
  ] body

capture :: Application -> Request -> IO Response
capture app request = do
  result <- newIORef Nothing
  _ <- app request $ \response -> do
    writeIORef result (Just response)
    pure ResponseReceived
  readIORef result >>= maybe (fail "Application did not send a response") pure

responseBody :: Response -> IO BL.ByteString
responseBody response = do
  chunks <- newIORef mempty
  let (_, _, withBody) = responseToStream response
  withBody $ \stream -> stream (\chunk -> modifyIORef' chunks (<> chunk)) (pure ())
  toLazyByteString <$> readIORef chunks

cacheHeaders :: Response -> [BL.ByteString]
cacheHeaders = map (BL.fromStrict . snd) . filter ((== hCacheControl) . fst) . responseHeaders

-- Exercise the actual public API router. Only the handler bodies are fixtures;
-- route parsing, GET/HEAD and decoded path semantics are the shipped Servant API.
publicRouter :: Application
publicRouter = serve (Proxy :: Proxy MusicReleasePublicAPI)
  ((\_ _ _ _ _ -> pure [])
    :<|> (\_ _ -> pure Null)
    :<|> (\_ _ -> pure (object ["url" .= ("https://storage.example.test/test" :: Text)]))
    :<|> (\_ _ -> pure NoContent))

spec :: Spec
spec = describe "music credential cache boundary" $ do
  forM_ (credentialRoutes <> readRoutes) $ \route ->
    it ("protects credential operation " <> show route) $ do
      original <- capture (upstream status201) (requestFor route)
      response <- capture (musicCredentialCache (upstream status201)) (requestFor route)
      cacheHeaders response `shouldBe` ["private, no-store"]
      responseStatus response `shouldBe` responseStatus original
      filter ((/= hCacheControl) . fst) (responseHeaders response)
        `shouldBe` filter ((/= hCacheControl) . fst) (responseHeaders original)
      responseBody response `shouldReturn` body

  it "protects successful and denied responses without changing status or body" $
    forM_ [status200, status201, status400, status401, status403, status404,
           status409, status429, status500, status503] $ \status -> do
      response <- capture (musicCredentialCache (upstream status))
        (requestFor assetRoute)
      cacheHeaders response `shouldBe` ["private, no-store"]
      responseStatus response `shouldBe` status
      responseBody response `shouldReturn` body

  it "remains idempotent around middleware that adds response headers" $ do
    let addOuterHeaders :: Middleware
        addOuterHeaders next request send = next request $ send . mapResponseHeaders
          (("Cache-Control", "public") :)
        app = musicCredentialCache (addOuterHeaders (musicCredentialCache (upstream status200)))
    response <- capture app (requestFor assetRoute)
    original <- capture (upstream status200) (requestFor assetRoute)
    cacheHeaders response `shouldBe` ["private, no-store"]
    filter ((/= hCacheControl) . fst) (responseHeaders response)
      `shouldBe` filter ((/= hCacheControl) . fst) (responseHeaders original)
    responseBody response `shouldReturn` body

  it "protects canonical startup failures without changing startup semantics" $ do
    let request = requestFor assetRoute
    original <- capture startupApp request
    response <- capture (musicCredentialCache startupApp) request
    cacheHeaders response `shouldBe` ["private, no-store"]
    responseStatus response `shouldBe` responseStatus original
    originalBody <- responseBody original
    responseBody response `shouldReturn` originalBody

  it "protects errors synthesized outside the inner application" $ do
    let broken _ _ = ioError (userError "TEST_ONLY_PRIVATE_DIAGNOSTIC")
        app = musicCredentialCache
          (requestExceptionBoundary (const (pure ())) (musicCredentialCache broken))
    response <- capture app (requestFor assetRoute)
    responseStatus response `shouldBe` status500
    cacheHeaders response `shouldBe` ["private, no-store"]
    responseBody response `shouldReturn` "Internal server error"

  it "retains the policy with trailing slashes and query parameters" $
    forM_ (credentialRoutes <> readRoutes) $ \(method, path) ->
      forM_ [[""], ["", "", ""]] $ \trailing -> do
        let request = (requestFor (method, path <> trailing))
              { rawQueryString = "?cache=public&route=/music/releases"
              , queryString = [("cache", Just "public"), ("route", Just "/music/releases")]
              }
        response <- capture (musicCredentialCache (upstream status200)) request
        cacheHeaders response `shouldBe` ["private, no-store"]

  it "uses WAI decoded static path segments without rewriting the request" $ do
    let encoded = "/%6dusic/assets/asset-id/%61ccess"
        request = (requestFor assetRoute)
          { rawPathInfo = encoded, pathInfo = decodePathSegments encoded }
    observed <- newIORef Nothing
    let inspect next incoming send = do
          writeIORef observed (Just (rawPathInfo incoming, pathInfo incoming))
          next incoming send
    response <- capture (musicCredentialCache (inspect (upstream status200))) request
    cacheHeaders response `shouldBe` ["private, no-store"]
    readIORef observed `shouldReturn` Just (encoded, decodePathSegments encoded)

  it "protects accepted asset-access spellings through the actual public API router" $
    forM_ ["GET", "HEAD"] $ \method ->
      forM_ [ "/music/assets/00000000-0000-4000-8000-000000000001/access"
            , "/music/assets/00000000-0000-4000-8000-000000000001/access/"
            , "/%6dusic/assets/00000000-0000-4000-8000-000000000001/%61ccess"
            ] $ \encoded -> do
        let request = (requestFor assetRoute)
              { requestMethod = method, rawPathInfo = encoded
              , pathInfo = decodePathSegments encoded }
        original <- capture publicRouter request
        response <- capture (musicCredentialCache publicRouter) request
        responseStatus original `shouldBe` status200
        responseStatus response `shouldBe` responseStatus original
        cacheHeaders response `shouldBe` ["private, no-store"]
        originalBody <- responseBody original
        responseBody response `shouldReturn` originalBody

  it "also protects repeated trailing empty segments without changing router decisions" $ do
    let encoded = "/music/assets/00000000-0000-4000-8000-000000000001/access///"
        request = (requestFor assetRoute)
          { rawPathInfo = encoded, pathInfo = decodePathSegments encoded }
    original <- capture publicRouter request
    response <- capture (musicCredentialCache publicRouter) request
    responseStatus response `shouldBe` responseStatus original
    cacheHeaders response `shouldBe` ["private, no-store"]
    originalBody <- responseBody original
    responseBody response `shouldReturn` originalBody

  forM_ unrelatedRoutes $ \route ->
    it ("leaves non-credential operation unchanged " <> show route) $ do
      original <- capture (upstream status200) (requestFor route)
      response <- capture (musicCredentialCache (upstream status200)) (requestFor route)
      responseHeaders response `shouldBe` responseHeaders original
      responseStatus response `shouldBe` responseStatus original
      responseBody response `shouldReturn` body

  it "does not interpret credential-like query text as a credential route" $ do
    let request = (requestFor ("GET", ["music", "releases"]))
          { rawQueryString = "?q=/music/assets/a/access"
          , queryString = [("q", Just "/music/assets/a/access")]
          }
    response <- capture (musicCredentialCache (upstream status200)) request
    cacheHeaders response `shouldBe` ["public, max-age=3600", "s-maxage=86400"]

unrelatedRoutes :: [(Method, [Text])]
unrelatedRoutes =
  [ ("GET", ["music", "releases"])
  , ("GET", ["music", "releases", "album-slug"])
  , ("GET", ["music", "favorites"])
  , ("GET", ["music", "playlists"])
  , ("GET", ["music", "history"])
  , ("GET", ["music", "entitlements"])
  , ("GET", ["music", "studio", "releases"])
  , ("GET", ["music", "assets", "asset-id", "access", "extra"])
  , ("GET", ["api", "music", "assets", "asset-id", "access"])
  , ("GET", ["musical", "assets", "asset-id", "access"])
  , ("GET", ["music", "assets", "asset-id", "accessibility"])
  , ("POST", ["music", "assets", "asset-id", "access"])
  , ("GET", ["music", "entitlements", "entitlement-id", "download"])
  , ("GET", ["music", "downloads", "free"])
  , ("GET", ["music", "releases", "release-id", "versions", "version-id", "uploads"])
  , ("PUT", ["music", "uploads", "upload-id", "parts", "2"])
  , ("POST", ["music", "uploads", "upload-id", "completion"])
  , ("GET", ["music", "uploads", "upload-id"])
  , ("POST", ["music", "ddex", "exports", "export-id", "download"])
  , ("POST", ["music", "playback-events"])
  , ("GET", ["version"])
  ]
