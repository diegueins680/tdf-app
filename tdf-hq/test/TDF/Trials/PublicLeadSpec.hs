{-# LANGUAGE OverloadedStrings #-}

module TDF.Trials.PublicLeadSpec (spec) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (..), getJustEntity, selectList, (==.))
import Database.Persist.Sql (SqlPersistT, rawExecute, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Servant (ServerError (errBody, errHTTPCode))
import Test.Hspec

import TDF.Trials.DTO (PreferredSlot (..))
import TDF.Trials.Server
  ( createOrFetchParty
  , ensurePublicLeadParty
  , validatePreferredSlots
  , validatePublicSubjectSelection
  , validatePublicTrialPartyId
  )
import qualified TDF.Models as Models
import TDF.Trials.Models (Subject (..))

spec :: Spec
spec = do
  describe "Public trials lead party resolution" $ do
    it "creates/reuses signup party by email" $ do
      (firstId, secondId, storedEmail, storedName, storedPhone) <- runInMemory $ do
        now <- liftIO getCurrentTime
        firstId <- createOrFetchParty (Just "Test User") (Just " User@Example.com ") (Just "+593 99 123 4567") now
        secondId <- createOrFetchParty (Just "Another Name") (Just "user@example.com") Nothing now
        Entity _ party <- getJustEntity firstId
        pure (firstId, secondId, Models.partyPrimaryEmail party, Models.partyDisplayName party, Models.partyPrimaryPhone party)

      firstId `shouldBe` secondId
      storedEmail `shouldBe` Just "user@example.com"
      storedName `shouldBe` "Test User"
      storedPhone `shouldBe` Just "+593991234567"

    it "falls back to the normalized email when the provided name is blank" $ do
      storedName <- runInMemory $ do
        now <- liftIO getCurrentTime
        partyId <- createOrFetchParty (Just "   ") (Just " Student@Example.com ") Nothing now
        Models.partyDisplayName . entityVal <$> getJustEntity partyId

      storedName `shouldBe` "student@example.com"

    it "rejects explicitly invalid nonblank phones instead of silently discarding them" $ do
      result <- tryCreateOrFetchParty (Just "Test User") (Just "user@example.com") (Just "---")
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "phone"
        Right _ ->
          expectationFailure "Expected invalid phone input to be rejected"

    it "rejects malformed emails instead of creating unusable parties" $ do
      result <- tryCreateOrFetchParty (Just "Test User") (Just "not-an-email") Nothing
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "email"
        Right _ ->
          expectationFailure "Expected invalid email input to be rejected"

    it "rejects free-form text that merely contains digits instead of extracting a misleading partial phone" $ do
      result <- tryCreateOrFetchParty (Just "Test User") (Just "user@example.com") (Just "call me at 099 123 4567")
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "phone"
        Right _ ->
          expectationFailure "Expected mixed text phone input to be rejected"

    it "keeps a single fallback party for anonymous interests" $ do
      (firstId, secondId, total) <- runInMemory $ do
        now <- liftIO getCurrentTime
        firstId <- ensurePublicLeadParty now
        secondId <- ensurePublicLeadParty now
        rows <- selectList [Models.PartyPrimaryEmail ==. Just "public-interest@tdf.local"] []
        pure (firstId, secondId, length rows)

      firstId `shouldBe` secondId
      total `shouldBe` 1

  describe "validatePublicTrialPartyId" $ do
    it "accepts requests that omit partyId" $
      validatePublicTrialPartyId Nothing `shouldBe` Right ()

    it "rejects caller-supplied party ids on the public endpoint" $
      case validatePublicTrialPartyId (Just 42) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "partyId is not allowed on public trial requests"
        Right _ ->
          expectationFailure "Expected public partyId to be rejected"

  describe "validatePublicSubjectSelection" $ do
    it "accepts active public subjects" $
      validatePublicSubjectSelection (Just (Subject "Piano" True)) `shouldBe` Right ()

    it "rejects missing or inactive subjects instead of reporting a misleading availability error" $ do
      let assertRejected candidate =
            case validatePublicSubjectSelection candidate of
              Left err -> do
                errHTTPCode err `shouldBe` 422
                BL8.unpack (errBody err) `shouldContain` "La materia solicitada no está disponible"
              Right value ->
                expectationFailure ("Expected unavailable public subject to be rejected, got " <> show value)
      assertRejected Nothing
      assertRejected (Just (Subject "Piano" False))

  describe "validatePreferredSlots" $ do
    it "rejects requests with more than three preferred slots" $ do
      let slots = replicate 4 validSlot
      case validatePreferredSlots slots of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "At most three preferred slots are allowed"
        Right _ ->
          expectationFailure "Expected too many slots to be rejected"

    it "rejects slots whose endAt is not after startAt" $ do
      let reversedSlot = PreferredSlot slotEnd slotStart
      case validatePreferredSlots [reversedSlot] of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Preferred slot endAt must be after startAt"
        Right _ ->
          expectationFailure "Expected reversed slot to be rejected"

    it "rejects overlapping or duplicate preferred slots" $ do
      let overlappingSlot = PreferredSlot (addUTCTime 1800 slotStart) (addUTCTime 5400 slotStart)
          assertRejected slots =
            case validatePreferredSlots slots of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "Preferred slots must be distinct non-overlapping windows"
              Right value ->
                expectationFailure ("Expected overlapping slots to be rejected, got " <> show value)
      assertRejected [validSlot, overlappingSlot]
      assertRejected [validSlot, validSlot]

    it "preserves valid slots without truncation or mutation" $
      validatePreferredSlots [validSlot, laterValidSlot] `shouldBe` Right [validSlot, laterValidSlot]

runInMemory :: SqlPersistT IO a -> IO a
runInMemory action =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializePartySchema pool
    liftIO $ runSqlPool action pool

tryCreateOrFetchParty
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> IO (Either ServerError Models.PartyId)
tryCreateOrFetchParty mName mEmail mPhone =
  try $ runInMemory $ do
    now <- liftIO getCurrentTime
    createOrFetchParty mName mEmail mPhone now

initializePartySchema :: (MonadIO m) => SqlPersistT m ()
initializePartySchema = do
  rawExecute "PRAGMA foreign_keys = ON" []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"party\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"legal_name\" VARCHAR NULL,\
    \\"display_name\" VARCHAR NOT NULL,\
    \\"is_org\" BOOLEAN NOT NULL,\
    \\"tax_id\" VARCHAR NULL,\
    \\"primary_email\" VARCHAR NULL,\
    \\"primary_phone\" VARCHAR NULL,\
    \\"whatsapp\" VARCHAR NULL,\
    \\"instagram\" VARCHAR NULL,\
    \\"emergency_contact\" VARCHAR NULL,\
    \\"notes\" VARCHAR NULL,\
    \\"created_at\" TIMESTAMP NOT NULL\
    \)"
    []

slotStart :: UTCTime
slotStart = UTCTime (fromGregorian 2026 4 1) (secondsToDiffTime 36000)

slotEnd :: UTCTime
slotEnd = addUTCTime 3600 slotStart

validSlot :: PreferredSlot
validSlot = PreferredSlot slotStart slotEnd

laterValidSlot :: PreferredSlot
laterValidSlot =
  let laterStart = addUTCTime 7200 slotStart
  in PreferredSlot laterStart (addUTCTime 3600 laterStart)
