{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_)
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Test (runSession, request, simpleStatus)
import System.Environment (setEnv, unsetEnv)
import Test.Hspec
import TDF.Cors (corsPolicy)

main :: IO ()
main = do
  -- Independent process: never change environment of a running API.
  forM_ ["ALLOW_ORIGINS", "ALLOW_ORIGIN", "CORS_ALLOW_ORIGINS", "CORS_ALLOW_ORIGIN",
         "CORS_ALLOW_ALL_ORIGINS", "DISABLE_DEFAULT_CORS", "HQ_APP_URL"] unsetEnv
  setEnv "ALLOWED_ORIGINS" "http://127.0.0.1:4187"
  setEnv "ALLOW_ALL_ORIGINS" "false"
  setEnv "CORS_DISABLE_DEFAULTS" "true"
  policy <- corsPolicy
  let app _ respond = respond (responseLBS status200 [] "ok")
      preflight origin headers = runSession (request defaultRequest
        { requestMethod = "OPTIONS"
        , requestHeaders = [("Origin", origin), ("Access-Control-Request-Method", "POST"),
                            ("Access-Control-Request-Headers", headers)]
        }) (policy app)
  hspec $ describe "Music browser CORS" $ do
    it "allows canonical idempotent mutations from an authorized frontend" $ do
      response <- preflight "http://127.0.0.1:4187" "authorization,content-type,idempotency-key"
      simpleStatus response `shouldBe` status200
    it "still refuses an untrusted frontend" $ do
      response <- preflight "https://attacker.example" "authorization,content-type,idempotency-key"
      simpleStatus response `shouldNotBe` status200
    it "does not grant browser control of trusted-edge country headers" $ do
      response <- preflight "http://127.0.0.1:4187" "cf-ipcountry"
      simpleStatus response `shouldNotBe` status200
