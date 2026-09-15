{-# LANGUAGE OverloadedStrings #-}

module TDF.EventOperations.TypesSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (isInfixOf)
import Servant.Server (errHTTPCode)
import Test.Hspec

import TDF.EventOperations.Server (eventOperationDomainError)
import TDF.EventOperations.Types

spec :: Spec
spec = describe "event operations executable API contracts" $ do
  it "round-trips every canonical lifecycle state" $ do
    map (eitherDecode . encode) allEventLifecycleStates
      `shouldBe` map Right allEventLifecycleStates

  it "rejects unknown lifecycle states" $ do
    (eitherDecode "\"finished\"" :: Either String EventLifecycleState)
      `shouldSatisfy` either (const True) (const False)

  it "accepts a versioned transition command and omits an absent reason" $ do
    let raw =
          "{\"expectedVersion\":3,\"targetState\":\"planning\",\"correlationId\":\"web:transition-3\"}"
        decoded = eitherDecode raw :: Either String EventTransitionCommand
    decoded `shouldBe` Right EventTransitionCommand
      { etcExpectedVersion = 3
      , etcTargetState = Planning
      , etcReason = Nothing
      , etcCorrelationId = "web:transition-3"
      }
    case decoded of
      Left message -> expectationFailure message
      Right command ->
        "\"reason\"" `isInfixOf` BL8.unpack (encode command) `shouldBe` False

  it "rejects null optional fields and unknown command fields" $ do
    let decodeCommand = eitherDecode :: BL8.ByteString -> Either String EventTransitionCommand
    decodeCommand "{\"expectedVersion\":1,\"targetState\":\"planning\",\"reason\":null,\"correlationId\":\"test\"}"
      `shouldSatisfy` either (const True) (const False)
    decodeCommand "{\"expectedVersion\":1,\"targetState\":\"planning\",\"correlationId\":\"test\",\"admin\":true}"
      `shouldSatisfy` either (const True) (const False)

  it "maps stable domain failures without turning them into success" $ do
    let statusFor = errHTTPCode . eventOperationDomainError
    map statusFor ["invalid_request", "reason_required"] `shouldBe` [400, 400]
    statusFor "forbidden" `shouldBe` 403
    map statusFor ["feature_disabled", "not_found"] `shouldBe` [404, 404]
    map statusFor
      [ "idempotency_conflict"
      , "version_conflict"
      , "transition_invalid"
      , "transition_effects_not_ready"
      , "separation_of_duties"
      ] `shouldBe` replicate 5 409
    statusFor "invalid_database_response" `shouldBe` 500
