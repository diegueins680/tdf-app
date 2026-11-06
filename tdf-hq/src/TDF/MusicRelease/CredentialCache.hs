{-# LANGUAGE OverloadedStrings #-}

module TDF.MusicRelease.CredentialCache (musicCredentialCache) where

import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Network.HTTP.Types (Method, hCacheControl, methodGet, methodHead)
import Network.Wai (Middleware, Request, mapResponseHeaders, pathInfo, requestMethod)

-- Signed delivery and multipart URLs are bearer credentials, including on
-- otherwise public endpoints. Apply the policy before authentication/routing
-- and to failures too, without changing ordinary catalog or library caching.
-- The outer boot boundary also covers startup and exception responses. This
-- operation is idempotent because mkApp uses it for direct application users.
musicCredentialCache :: Middleware
musicCredentialCache next request respond
  | isCredentialRequest request = next request (respond . mapResponseHeaders protect)
  | otherwise = next request respond
  where
    protect headers = (hCacheControl, "private, no-store")
      : filter ((/= hCacheControl) . fst) headers

isCredentialRequest :: Request -> Bool
isCredentialRequest request =
  -- Servant accepts a trailing slash at terminal routes. Do not let that
  -- spelling omit the response policy; retain internal path segments exactly.
  case dropWhileEnd T.null (pathInfo request) of
    ["music", "assets", _, "access"] -> readMethod method
    ["music", "entitlements", _, "download"] -> method == "POST"
    ["music", "downloads", "free"] -> method == "POST"
    ["music", "releases", _, "versions", _, "uploads"] -> method == "POST"
    ["music", "uploads", _, "parts", _] -> readMethod method
    ["music", "uploads", _, "completion"] -> readMethod method
    ["music", "uploads", _] -> method == "DELETE"
    ["music", "ddex", "exports", _, "download"] -> readMethod method
    _ -> False
  where
    method = requestMethod request

readMethod :: Method -> Bool
readMethod method = method == methodGet || method == methodHead
