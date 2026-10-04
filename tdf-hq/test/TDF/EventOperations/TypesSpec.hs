{-# LANGUAGE OverloadedStrings #-}

module TDF.EventOperations.TypesSpec (spec) where

import Data.Aeson (Value(..), eitherDecode, encode, toJSON, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (isInfixOf)
import Servant.Server (errHTTPCode)
import Test.Hspec
import Test.QuickCheck (choose, forAll)

import TDF.EventOperations.Server (eventOperationDomainError)
import TDF.EventOperations.Types

spec :: Spec
spec = describe "event operations executable API contracts" $ do
  it "accepts only exact completion fields, retaining text and BIGINT precision" $ do
    let base = object ["expectedRevision" .= ("9223372036854775806" :: T.Text),
          "reason" .= (" razón " :: T.Text), "correlationId" .= ("correlation" :: T.Text)]
        patch key value = case base of Object fields -> Object (KM.insert key value fields); _ -> Null
        decodeCommand value = eitherDecode (encode value) :: Either String EventTaskCompletionCommand
        invalid = [patch "actorPartyId" (Number 1), patch "override" (Bool True),
          patch "expectedRevision" (Number 4), patch "expectedRevision" (String "04"),
          patch "expectedRevision" (String "9223372036854775808"), patch "reason" Null,
          patch "reason" (String "\t"), patch "reason" (String (T.replicate 2001 "x")),
          patch "correlationId" Null, patch "correlationId" (String " "),
          patch "correlationId" (String (T.replicate 201 "x")), object []]
    fmap toJSON (decodeCommand base) `shouldBe` Right base
    map decodeCommand invalid `shouldSatisfy` all (either (const True) (const False))

  it "accepts only strict RACI command fields and preserves exact request identity" $ do
    let base = object ["expectedRevision" .= ("4" :: T.Text), "role" .= ("responsible" :: T.Text),
          "fromPartyId" .= (2 :: Int), "toPartyId" .= (3 :: Int), "reason" .= (" reason " :: T.Text),
          "correlationId" .= ("correlation" :: T.Text)]
        patch key value = case base of Object fields -> Object (KM.insert key value fields); _ -> Null
        decodeCommand value = eitherDecode (encode value) :: Either String EventRaciReassignmentCommand
        invalid = [patch "actorPartyId" (Number 1), patch "expectedRevision" (Number 4),
          patch "toPartyId" (Number 2), patch "fromPartyId" (Number 0), patch "toPartyId" (Number 9007199254740992),
          patch "role" (String "Responsible"), patch "reason" Null, patch "reason" (String "\t"),
          patch "reason" (String (T.replicate 2001 "x")), patch "correlationId" (String (T.replicate 201 "x"))]
    fmap toJSON (decodeCommand base) `shouldBe` Right base
    map decodeCommand invalid `shouldSatisfy` all (either (const True) (const False))

  it "round-trips generated positive signed BIGINT revisions as exact decimal strings" $
    forAll (choose (1, maxBound :: Int64)) $ \n ->
      let raw = T.pack (show n)
      in case parseEventTaskAggregateRevision raw of
        Nothing -> False
        Just revision -> toJSON revision == String raw && eitherDecode (encode revision) == Right revision

  it "preserves revision boundaries above JavaScript's numeric precision" $ do
    let valid = ["1", "9007199254740991", "9007199254740992", "9223372036854775807"]
    map (fmap toJSON . parseEventTaskAggregateRevision) valid `shouldBe` map (Just . String) valid

  it "rejects noncanonical, numeric, null and overflowing revision tokens" $ do
    let invalid = ["", "0", "-1", "+1", "01", " 1", "1 ", "1\n", "1\r", "1e2", "1.0", "١", "１",
                   "9223372036854775808", "10000000000000000000"]
    map parseEventTaskAggregateRevision invalid `shouldBe` replicate (length invalid) Nothing
    map (eitherDecode :: BL8.ByteString -> Either String EventTaskAggregateRevision) ["1", "null", "{}", "true"]
      `shouldSatisfy` all (either (const True) (const False))

  it "keeps both exact numeric transport endpoints and rejects their outside neighbors" $
    map isSafePositiveInteger [-1, 0, 1, 9007199254740991, 9007199254740992]
      `shouldBe` [False, False, True, True, False]

  it "round-trips every canonical logistics status and RACI role" $ do
    map (eitherDecode . encode) ([minBound..maxBound] :: [EventTaskStatus])
      `shouldBe` map Right ([minBound..maxBound] :: [EventTaskStatus])
    map (eitherDecode . encode) ([minBound..maxBound] :: [EventRaciRole])
      `shouldBe` map Right ([minBound..maxBound] :: [EventRaciRole])

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
      , "dependencies_not_ready"
      , "accountability_not_ready"
      ] `shouldBe` replicate 7 409
    statusFor "invalid_database_response" `shouldBe` 500
