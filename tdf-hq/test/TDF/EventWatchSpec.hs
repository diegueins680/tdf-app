{-# LANGUAGE OverloadedStrings #-}
module TDF.EventWatchSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.Either (isLeft)
import qualified Data.Text as T
import Data.Time (UTCTime(..), fromGregorian, addUTCTime)
import Test.Hspec
import Test.QuickCheck (property, Positive(..))
import Network.Wai (defaultRequest, requestHeaders)
import Servant (errHTTPCode)
import Servant.Server.Experimental.Auth (unAuthHandler)
import Servant.Server.Internal.Handler (runHandler)
import TDF.EventWatch
import TDF.Auth (isAuthenticatableApiTokenLabel)

spec :: Spec
spec = describe "isolated event-watch credentials" $ do
  let now = UTCTime (fromGregorian 2026 10 21) 0
      valid = sessionValid now (addUTCTime (-1) now) (addUTCTime 899 now)
  it "admits an unexpired session, with exact expiry rejected" $ do
    valid (addUTCTime 1000 now) `shouldBe` True
    sessionValid now now now (addUTCTime 1000 now) `shouldBe` False
    valid now `shouldBe` False
  it "rejects a future issue time and a lifetime longer than 15 minutes" $ do
    sessionValid now (addUTCTime 1 now) (addUTCTime 900 now)
      (addUTCTime 1000 now) `shouldBe` False
    sessionValid now now (addUTCTime 901 now) (addUTCTime 1000 now) `shouldBe` False
  it "cannot admit an expired client even with an unexpired session" $ property $
    \(Positive seconds) -> not (valid (addUTCTime (negate (fromInteger seconds)) now))
  it "hashes secrets deterministically without retaining them" $ do
    secretDigest "abc" `shouldBe`
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    T.length (secretDigest "abc") `shouldBe` 64
  it "rejects ambiguous client identifiers" $ do
    map validClientId ["ab", "UPPER", "../watch", "watch:admin", T.replicate 49 "a"]
      `shouldBe` replicate 5 False
    validClientId "quito-watch" `shouldBe` True
  it "does not extend the interactive token purpose allowlist" $
    isAuthenticatableApiTokenLabel (Just "event-watch:quito-watch") `shouldBe` False
  it "rejects cookies, duplicate headers and malformed tokens before any database access" $ do
    let headers = [[], [("Cookie", "tdf_session=human-session")],
          [("Authorization", "Bearer short")],
          [("Authorization", "Bearer short"), ("Authorization", "Bearer other")],
          [("Authorization", "Basic x")], [("Authorization", "Bearer \255")]]
    mapM_ (\hs -> do
      outcome <- runHandler $ unAuthHandler
        (authHandler (error "must not access Env before rejecting malformed credentials"))
        defaultRequest{requestHeaders=hs}
      either errHTTPCode (const 200) outcome `shouldBe` 401) headers
  it "rejects unknown token exchange inputs and caller-selected scopes" $ do
    isLeft (eitherDecode "{\"grant_type\":\"client_credentials\",\"client_id\":\"quito-watch\",\"client_secret\":\"x\",\"scope\":\"admin\"}"
      :: Either String TokenRequest) `shouldBe` True
