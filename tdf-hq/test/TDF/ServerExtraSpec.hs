{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerExtraSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Time (utctDay)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist hiding (Active)
import Database.Persist.Sql (SqlBackend, SqlPersistT, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool, runSqlite)
import Servant (ServerError (errBody, errHTTPCode), ServerT, (:<|>) (..))
import Test.Hspec
import Web.PathPieces (fromPathPiece)

import qualified TDF.Models as M
import TDF.API.Inventory (InventoryAPI)
import TDF.API.Pipelines (PipelinesAPI)
import TDF.API.Rooms (RoomsAPI)
import TDF.API.Payments (PaymentCreate (..))
import TDF.API.Types
  ( AssetCheckinRequest (..)
  , AssetCreate (..)
  , AssetCheckoutDTO
  , AssetCheckoutRequest (..)
  , AssetUpdate (..)
  , PipelineCardCreate (..)
  , PipelineCardDTO (..)
  , PipelineCardUpdate (..)
  , RoomCreate (..)
  , RoomDTO
  , RoomUpdate (..)
  )
import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.DB (Env (..))
import qualified TDF.ModelsExtra as ME
import TDF.ModelsExtra
  ( Asset (..)
  , AssetCondition (Good)
  , AssetStatus (Active, Booked, OutForMaintenance, Retired)
  , Band (..)
  , CheckoutTarget (TargetParty, TargetRoom, TargetSession)
  , MaintenancePolicy (None)
  , Room (..)
  , Session (..)
  , SessionStatus (InPrep, InSession)
  )
import TDF.ServerExtra (
    IGInbound (..),
    IGInboundDeleted (..),
    MetaChannel (..),
    MetaInboundEvent (..),
    assetMatchesSearchQuery,
    extractMetaInbound,
    normalizeAssetSearchQuery,
    normalizeAssetCategory,
    normalizeAssetCategoryUpdate,
    normalizeAssetCheckinFields,
    normalizeAssetNotesUpdate,
    normalizeAssetName,
    normalizeAssetNameUpdate,
    validateAssetPhotoUrl,
    validateAssetPhotoUrlUpdate,
    normalizeRoomName,
    normalizeRoomNameUpdate,
    validateSocialLimit,
    parseSocialBoolParam,
    parseSocialDirectionParam,
    validateSocialReplyExternalId,
    socialReplyOutcomeFields,
    validateSocialReplySenderId,
    validateInventoryPageParams,
    validatePaymentAmountCents,
    validatePaymentAttachmentUrl,
    parseCheckoutTargetKind,
    parseOptionalKeyField,
    validatePaymentReferences,
    validatePaymentCurrency,
    validatePaymentConcept,
    validatePositivePaymentReferenceId,
    validateOptionalPositivePaymentReferenceId,
    normalizeServiceCatalogNameUpdate,
    persistMetaInbound,
    validatePaymentMethod,
    validateDistinctSessionRooms,
    validateDistinctBandMemberIds,
    validateSessionStatusInput,
    validateSessionTimeRange,
    validateCheckoutTargets,
    validateCheckoutTargetReferences,
    validateServiceCatalogCurrency,
    validateServiceCatalogCurrencyUpdate,
    validateServiceCatalogTaxBps,
    validateServiceCatalogTaxBpsUpdate,
    validateAssetCheckoutStatus,
    validateAssetStatusUpdate,
    validatePageParams,
    validateSessionReferences,
    inventoryServer,
    pipelinesServer,
    roomsServer,
 )

type InventoryTestM = ReaderT Env (ExceptT ServerError IO)

spec :: Spec
spec = do
  describe "PaymentCreate FromJSON" $ do
    it "accepts canonical payment create fields for payment ingestion" $
      case A.eitherDecode
        "{\"pcPartyId\":42,\"pcOrderId\":7,\"pcInvoiceId\":9,\"pcAmountCents\":12500,\"pcCurrency\":\"USD\",\"pcMethod\":\"cash\",\"pcReference\":\"REC-42\",\"pcPaidAt\":\"2026-04-13\",\"pcConcept\":\"Studio booking\",\"pcPeriod\":\"2026-04\",\"pcAttachmentUrl\":\"https://files.example.com/receipt.pdf\"}" of
        Left err ->
          expectationFailure ("Expected canonical payment create payload to decode, got: " <> err)
        Right payload -> do
          pcPartyId payload `shouldBe` 42
          pcOrderId payload `shouldBe` Just 7
          pcInvoiceId payload `shouldBe` Just 9
          pcAmountCents payload `shouldBe` 12500
          pcCurrency payload `shouldBe` "USD"
          pcMethod payload `shouldBe` "cash"
          pcReference payload `shouldBe` Just "REC-42"
          pcPaidAt payload `shouldBe` "2026-04-13"
          pcConcept payload `shouldBe` "Studio booking"
          pcPeriod payload `shouldBe` Just "2026-04"
          pcAttachmentUrl payload `shouldBe` Just "https://files.example.com/receipt.pdf"

    it "rejects unexpected payment keys so typoed or over-posted writes fail explicitly" $ do
      (A.eitherDecode
        "{\"pcPartyId\":42,\"pcAmountCents\":12500,\"pcCurrency\":\"USD\",\"pcMethod\":\"cash\",\"pcPaidAt\":\"2026-04-13\",\"pcConcept\":\"Studio booking\",\"unexpected\":true}"
          :: Either String PaymentCreate)
        `shouldSatisfy` isLeft

  describe "inventory checkout/check-in request JSON" $ do
    it "accepts canonical asset create and patch keys used by current clients" $ do
      case A.eitherDecode
        "{\"cName\":\"Roland Juno-106\",\"cCategory\":\"Synth\",\"cPhotoUrl\":\"https://cdn.example.com/juno.jpg\"}" of
        Left err ->
          expectationFailure ("Expected canonical asset create payload to decode, got: " <> err)
        Right payload -> do
          cName payload `shouldBe` "Roland Juno-106"
          cCategory payload `shouldBe` "Synth"
          cPhotoUrl payload `shouldBe` Just "https://cdn.example.com/juno.jpg"

      case A.eitherDecode
        "{\"uName\":\"Roland Juno-60\",\"uNotes\":\"Freshly serviced\"}" of
        Left err ->
          expectationFailure ("Expected canonical asset patch payload to decode, got: " <> err)
        Right payload -> do
          uName payload `shouldBe` Just "Roland Juno-60"
          uNotes payload `shouldBe` Just "Freshly serviced"
          uCategory payload `shouldBe` Nothing

    it "rejects response-shaped or unexpected asset create and patch keys so typoed inventory writes fail explicitly" $ do
      (A.eitherDecode
        "{\"cName\":\"Roland Juno-106\",\"cCategory\":\"Synth\",\"status\":\"Active\"}"
          :: Either String AssetCreate)
        `shouldSatisfy` isLeft
      (A.eitherDecode
        "{\"name\":\"Roland Juno-106\",\"category\":\"Synth\"}"
          :: Either String AssetUpdate)
        `shouldSatisfy` isLeft
      (A.eitherDecode
        "{\"uName\":\"Roland Juno-106\",\"unexpected\":true}"
          :: Either String AssetUpdate)
        `shouldSatisfy` isLeft

    it "accepts canonical inventory checkout keys used by current clients" $
      case A.eitherDecode
        "{\"coTargetKind\":\"room\",\"coTargetRoom\":\"00000000-0000-0000-0000-000000000042\",\"coConditionOut\":\"Excelente\",\"coNotes\":\"Cableado completo\"}" of
        Left err ->
          expectationFailure ("Expected canonical asset checkout payload to decode, got: " <> err)
        Right payload -> do
          coTargetKind payload `shouldBe` Just "room"
          coTargetRoom payload `shouldBe` Just "00000000-0000-0000-0000-000000000042"
          coConditionOut payload `shouldBe` Just "Excelente"
          coNotes payload `shouldBe` Just "Cableado completo"

    it "rejects response-shaped or unexpected checkout keys so typos cannot silently fall back to party checkout" $ do
      (A.eitherDecode
        "{\"targetKind\":\"room\",\"targetRoom\":\"00000000-0000-0000-0000-000000000042\"}"
          :: Either String AssetCheckoutRequest)
        `shouldSatisfy` isLeft
      (A.eitherDecode
        "{\"coTargetKind\":\"room\",\"coTargetRoom\":\"00000000-0000-0000-0000-000000000042\",\"unexpected\":true}"
          :: Either String AssetCheckoutRequest)
        `shouldSatisfy` isLeft

    it "accepts canonical inventory check-in keys used by current clients" $
      case A.eitherDecode
        "{\"ciConditionIn\":\"Returned OK\",\"ciNotes\":\"Cables verified\"}" of
        Left err ->
          expectationFailure ("Expected canonical asset check-in payload to decode, got: " <> err)
        Right payload -> do
          ciConditionIn payload `shouldBe` Just "Returned OK"
          ciNotes payload `shouldBe` Just "Cables verified"

    it "rejects response-shaped or unexpected check-in keys so metadata typos fail explicitly" $ do
      (A.eitherDecode
        "{\"conditionIn\":\"Returned OK\",\"notes\":\"Cables verified\"}"
          :: Either String AssetCheckinRequest)
        `shouldSatisfy` isLeft
      (A.eitherDecode
        "{\"ciConditionIn\":\"Returned OK\",\"unexpected\":true}"
          :: Either String AssetCheckinRequest)
        `shouldSatisfy` isLeft

  describe "inventory asset query filtering" $ do
    it "normalizes missing or blank queries to no filter" $ do
      normalizeAssetSearchQuery Nothing `shouldBe` Nothing
      normalizeAssetSearchQuery (Just "   ") `shouldBe` Nothing
      normalizeAssetSearchQuery (Just "  SYNTH  ") `shouldBe` Just "synth"

    it "matches name/category/brand/model/owner/notes case-insensitively once q is provided" $ do
      let synthAsset = fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" (Just "Analog poly")
          drumAsset = fixtureAsset "Ludwig Supraphonic" "Drum" (Just "Ludwig") Nothing "Backline" (Just "Studio snare")
      assetMatchesSearchQuery "juno" synthAsset `shouldBe` True
      assetMatchesSearchQuery "synth" synthAsset `shouldBe` True
      assetMatchesSearchQuery "roland" synthAsset `shouldBe` True
      assetMatchesSearchQuery "tdf" synthAsset `shouldBe` True
      assetMatchesSearchQuery "analog" synthAsset `shouldBe` True
      assetMatchesSearchQuery "juno" drumAsset `shouldBe` False

  describe "shared list pagination validation" $ do
    it "defaults omitted params and preserves explicit supported values" $ do
      validatePageParams Nothing Nothing `shouldBe` Right (1, 50)
      validatePageParams (Just 3) (Just 25) `shouldBe` Right (3, 25)
      validatePageParams (Just 1) (Just 100) `shouldBe` Right (1, 100)
      validateInventoryPageParams (Just 2) (Just 40) `shouldBe` Right (2, 40)

    it "rejects invalid explicit pagination instead of silently clamping list endpoints" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid list pagination error, got " <> show value)
      assertInvalid "page must be greater than or equal to 1" (validatePageParams (Just 0) Nothing)
      assertInvalid "page must be greater than or equal to 1" (validatePageParams (Just (-2)) Nothing)
      assertInvalid "pageSize must be between 1 and 100" (validatePageParams Nothing (Just 0))
      assertInvalid "pageSize must be between 1 and 100" (validatePageParams Nothing (Just 101))

  describe "asset name/category normalization" $ do
    it "trims meaningful asset names and categories on create and update" $ do
      normalizeAssetName "  Roland Juno-106  " `shouldBe` Right "Roland Juno-106"
      normalizeAssetNameUpdate (Just "  Tape Echo  ") `shouldBe` Right (Just "Tape Echo")
      normalizeAssetNameUpdate Nothing `shouldBe` Right Nothing
      normalizeAssetCategory "  Synth  " `shouldBe` Right "Synth"
      normalizeAssetCategoryUpdate (Just "  Outboard  ") `shouldBe` Right (Just "Outboard")
      normalizeAssetCategoryUpdate Nothing `shouldBe` Right Nothing

    it "rejects explicit blank asset names and categories instead of storing whitespace-only records" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid asset input error, got " <> show value)
      assertInvalid "Asset name is required" (normalizeAssetName "   ")
      assertInvalid "Asset name is required" (normalizeAssetNameUpdate (Just "   "))
      assertInvalid "Asset category is required" (normalizeAssetCategory "   ")
      assertInvalid "Asset category is required" (normalizeAssetCategoryUpdate (Just "   "))

  describe "validateAssetPhotoUrl" $ do
    it "treats omitted or blank asset photo inputs as absent and canonicalizes supported URL shapes" $ do
      validateAssetPhotoUrl Nothing `shouldBe` Right Nothing
      validateAssetPhotoUrl (Just "   ") `shouldBe` Right Nothing
      validateAssetPhotoUrl (Just "  https://cdn.example.com/roland.jpg  ")
        `shouldBe` Right (Just "https://cdn.example.com/roland.jpg")
      validateAssetPhotoUrl (Just " inventory/roland-juno.jpg ")
        `shouldBe` Right (Just "inventory/roland-juno.jpg")
      validateAssetPhotoUrl (Just "assets/inventory/roland-juno.jpg")
        `shouldBe` Right (Just "inventory/roland-juno.jpg")
      validateAssetPhotoUrl (Just "/assets/serve/inventory/roland-juno.jpg")
        `shouldBe` Right (Just "inventory/roland-juno.jpg")

    it "rejects malformed or unsupported asset photo inputs instead of storing opaque strings" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "photoUrl must be an absolute http(s) URL or an inventory asset path"
            Right value ->
              expectationFailure ("Expected invalid asset photo URL error, got " <> show value)
      assertInvalid (validateAssetPhotoUrl (Just "roland-juno.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "ftp://cdn.example.com/roland.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "https://cdn/roland.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "https://2130706433/roland.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "assets/serve/roland.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "inventory/../roland.jpg"))

  describe "validateAssetPhotoUrlUpdate" $ do
    it "keeps omitted photo updates untouched, trims valid values, and treats blank strings as clear intents" $ do
      validateAssetPhotoUrlUpdate Nothing `shouldBe` Right Nothing
      validateAssetPhotoUrlUpdate (Just "   ") `shouldBe` Right (Just Nothing)
      validateAssetPhotoUrlUpdate (Just "  https://cdn.example.com/roland.jpg  ")
        `shouldBe` Right (Just (Just "https://cdn.example.com/roland.jpg"))
      validateAssetPhotoUrlUpdate (Just " inventory/roland-juno.jpg ")
        `shouldBe` Right (Just (Just "inventory/roland-juno.jpg"))

    it "rejects malformed explicit photo updates instead of silently ignoring them" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "photoUrl must be an absolute http(s) URL or an inventory asset path"
            Right value ->
              expectationFailure ("Expected invalid asset photo update error, got " <> show value)
      assertInvalid (validateAssetPhotoUrlUpdate (Just "roland-juno.jpg"))
      assertInvalid (validateAssetPhotoUrlUpdate (Just "ftp://cdn.example.com/roland.jpg"))

  describe "normalizeAssetNotesUpdate" $ do
    it "preserves omitted notes, trims meaningful updates, and maps blanks to an explicit clear" $ do
      normalizeAssetNotesUpdate Nothing `shouldBe` Nothing
      normalizeAssetNotesUpdate (Just "  Analog poly synth  ")
        `shouldBe` Just (Just "Analog poly synth")
      normalizeAssetNotesUpdate (Just "   ") `shouldBe` Just Nothing

  describe "normalizeRoomName" $ do
    it "trims and collapses meaningful room names on create and update" $ do
      normalizeRoomName "  Sala   A  " `shouldBe` Right "Sala A"
      normalizeRoomNameUpdate (Just "  Control \t Room  ") `shouldBe` Right (Just "Control Room")
      normalizeRoomNameUpdate Nothing `shouldBe` Right Nothing

    it "rejects explicit blank room names instead of storing whitespace-only records" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "Room name is required"
            Right value ->
              expectationFailure ("Expected invalid room name error, got " <> show value)
      assertInvalid (normalizeRoomName "   ")
      assertInvalid (normalizeRoomNameUpdate (Just "   "))

  describe "room write request JSON" $ do
    it "accepts canonical room create and patch keys used by current clients" $ do
      case A.eitherDecode
        "{\"rcName\":\"Sala A\"}" of
        Left err ->
          expectationFailure ("Expected canonical room create payload to decode, got: " <> err)
        Right payload ->
          rcName payload `shouldBe` "Sala A"

      case A.eitherDecode
        "{\"ruName\":\"Control Room\",\"ruIsBookable\":false}" of
        Left err ->
          expectationFailure ("Expected canonical room patch payload to decode, got: " <> err)
        Right payload -> do
          ruName payload `shouldBe` Just "Control Room"
          ruIsBookable payload `shouldBe` Just False

    it "rejects response-shaped or unexpected room keys so typoed patches fail explicitly" $ do
      (A.eitherDecode
        "{\"name\":\"Sala A\"}"
          :: Either String RoomCreate)
        `shouldSatisfy` isLeft
      (A.eitherDecode
        "{\"rcName\":\"Sala A\",\"unexpected\":true}"
          :: Either String RoomCreate)
        `shouldSatisfy` isLeft
      (A.eitherDecode
        "{\"name\":\"Control Room\",\"isBookable\":false}"
          :: Either String RoomUpdate)
        `shouldSatisfy` isLeft
      (A.eitherDecode
        "{\"ruName\":\"Control Room\",\"unexpected\":true}"
          :: Either String RoomUpdate)
        `shouldSatisfy` isLeft

  describe "roomsServer duplicate name handling" $ do
    let existingRoomId = "00000000-0000-0000-0000-000000000701"
        otherRoomId = "00000000-0000-0000-0000-000000000702"

    it "rejects duplicate room creates with a 409 instead of bubbling a DB uniqueness failure" $ do
      existingKey <- case (fromPathPiece existingRoomId :: Maybe (Key Room)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing room fixture key" >> fail "unreachable"
      result <- runRoomCreateHandler
        (insertKey existingKey (fixtureRoom "Sala A"))
        (RoomCreate "  Sala A  ")
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "A room with this name already exists"
        Right value ->
          expectationFailure ("Expected duplicate room create to fail, got " <> show value)

    it "rejects room creates that only differ by case or repeated whitespace" $ do
      existingKey <- case (fromPathPiece existingRoomId :: Maybe (Key Room)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing room fixture key" >> fail "unreachable"
      result <- runRoomCreateHandler
        (insertKey existingKey (fixtureRoom "Sala A"))
        (RoomCreate "  sala   a  ")
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "A room with this name already exists"
        Right value ->
          expectationFailure ("Expected canonical duplicate room create to fail, got " <> show value)

    it "rejects room renames that collide with another room instead of failing ambiguously on update" $ do
      existingKey <- case (fromPathPiece existingRoomId :: Maybe (Key Room)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing room fixture key" >> fail "unreachable"
      otherKey <- case (fromPathPiece otherRoomId :: Maybe (Key Room)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid other room fixture key" >> fail "unreachable"
      result <- runRoomPatchHandler
        (do
            insertKey existingKey (fixtureRoom "Sala A")
            insertKey otherKey (fixtureRoom "Control Room"))
        otherRoomId
        (RoomUpdate (Just " Sala A ") Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "A room with this name already exists"
        Right value ->
          expectationFailure ("Expected duplicate room patch to fail, got " <> show value)

    it "rejects room renames that collide after case-and-spacing normalization" $ do
      existingKey <- case (fromPathPiece existingRoomId :: Maybe (Key Room)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing room fixture key" >> fail "unreachable"
      otherKey <- case (fromPathPiece otherRoomId :: Maybe (Key Room)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid other room fixture key" >> fail "unreachable"
      result <- runRoomPatchHandler
        (do
            insertKey existingKey (fixtureRoom "Sala A")
            insertKey otherKey (fixtureRoom "Control Room"))
        otherRoomId
        (RoomUpdate (Just "  sala   a  ") Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "A room with this name already exists"
        Right value ->
          expectationFailure ("Expected canonical duplicate room patch to fail, got " <> show value)

  describe "pipeline card write normalization" $ do
    let pipelineType = "recording"
        existingPipelineCardId = "00000000-0000-0000-0000-000000000801"

    it "trims create fields before persistence so CRM cards do not store padded text" $ do
      result <- runPipelineCreateHandler
        (pure ())
        pipelineType
        (PipelineCardCreate "  Demo Lead  " (Just "  Ada  ") Nothing (Just 2) (Just "  Needs quote  "))
      case result of
        Left err ->
          expectationFailure ("Expected trimmed pipeline card create to succeed, got " <> show err)
        Right card -> do
          pcTitle card `shouldBe` "Demo Lead"
          pcArtist card `shouldBe` Just "Ada"
          pcStage card `shouldBe` "Inquiry"
          pcSortOrder card `shouldBe` 2
          pcNotes card `shouldBe` Just "Needs quote"

    it "maps blank optional patch text to explicit clears instead of persisting whitespace-only CRM metadata" $ do
      existingKey <- case (fromPathPiece existingPipelineCardId :: Maybe (Key ME.PipelineCard)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing pipeline card fixture key" >> fail "unreachable"
      result <- runPipelinePatchHandler
        (do
            now <- liftIO getCurrentTime
            insertKey existingKey ME.PipelineCard
              { ME.pipelineCardServiceKind = M.Recording
              , ME.pipelineCardTitle = "Initial lead"
              , ME.pipelineCardArtist = Just "Ada"
              , ME.pipelineCardStage = "Inquiry"
              , ME.pipelineCardSortOrder = 3
              , ME.pipelineCardNotes = Just "Needs quote"
              , ME.pipelineCardCreatedAt = now
              , ME.pipelineCardUpdatedAt = now
              })
        pipelineType
        existingPipelineCardId
        (PipelineCardUpdate (Just "  Final Quote  ") (Just (Just "   ")) Nothing Nothing (Just (Just "   ")))
      case result of
        Left err ->
          expectationFailure ("Expected pipeline card patch normalization to succeed, got " <> show err)
        Right card -> do
          pcTitle card `shouldBe` "Final Quote"
          pcArtist card `shouldBe` Nothing
          pcNotes card `shouldBe` Nothing
          pcSortOrder card `shouldBe` 3

    it "rejects blank titles on create and patch instead of creating unlabeled pipeline cards" $ do
      createResult <- runPipelineCreateHandler
        (pure ())
        pipelineType
        (PipelineCardCreate "   " Nothing Nothing Nothing Nothing)
      case createResult of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Pipeline card title is required"
        Right value ->
          expectationFailure ("Expected blank pipeline card create title to fail, got " <> show value)

      existingKey <- case (fromPathPiece existingPipelineCardId :: Maybe (Key ME.PipelineCard)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing pipeline card fixture key" >> fail "unreachable"
      patchResult <- runPipelinePatchHandler
        (do
            now <- liftIO getCurrentTime
            insertKey existingKey ME.PipelineCard
              { ME.pipelineCardServiceKind = M.Recording
              , ME.pipelineCardTitle = "Initial lead"
              , ME.pipelineCardArtist = Nothing
              , ME.pipelineCardStage = "Inquiry"
              , ME.pipelineCardSortOrder = 0
              , ME.pipelineCardNotes = Nothing
              , ME.pipelineCardCreatedAt = now
              , ME.pipelineCardUpdatedAt = now
              })
        pipelineType
        existingPipelineCardId
        (PipelineCardUpdate (Just "   ") Nothing Nothing Nothing Nothing)
      case patchResult of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Pipeline card title is required"
        Right value ->
          expectationFailure ("Expected blank pipeline card patch title to fail, got " <> show value)

  describe "validateAssetStatusUpdate" $ do
    it "accepts supported asset status variants" $ do
      validateAssetStatusUpdate (Just " booked ") `shouldBe` Right (Just Booked)
      validateAssetStatusUpdate (Just "out_for_maintenance") `shouldBe` Right (Just OutForMaintenance)

    it "rejects blank or unknown asset statuses instead of silently ignoring them" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "Allowed values: active, booked, out_for_maintenance, retired"
            Right value ->
              expectationFailure ("Expected invalid status error, got " <> show value)
      assertInvalid (validateAssetStatusUpdate (Just "   "))
      assertInvalid (validateAssetStatusUpdate (Just "on-loan"))

  describe "validateAssetCheckoutStatus" $ do
    it "allows checkout only when the asset is in the active inventory state" $
      validateAssetCheckoutStatus Active `shouldBe` Right ()

    it "rejects booked, maintenance, or retired assets instead of creating impossible checkout rows" $ do
      let assertInvalid expectedMessage statusValue =
            case validateAssetCheckoutStatus statusValue of
              Left err -> do
                errHTTPCode err `shouldBe` 409
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected checkout status validation error, got " <> show value)
      assertInvalid "resolve the existing checkout state" Booked
      assertInvalid "out for maintenance" OutForMaintenance
      assertInvalid "retired" Retired

  describe "parseCheckoutTargetKind" $ do
    it "defaults omitted target kinds to party and normalizes supported values" $ do
      parseCheckoutTargetKind Nothing `shouldBe` Right TargetParty
      parseCheckoutTargetKind (Just " room ") `shouldBe` Right TargetRoom
      parseCheckoutTargetKind (Just "SESSION") `shouldBe` Right TargetSession

    it "rejects blank or unknown target kinds instead of silently treating them as party checkouts" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "targetKind must be one of: party, room, session"
            Right value ->
              expectationFailure ("Expected invalid checkout target kind error, got " <> show value)
      assertInvalid (parseCheckoutTargetKind (Just "   "))
      assertInvalid (parseCheckoutTargetKind (Just "locker"))

  describe "parseOptionalKeyField" $ do
    it "treats missing or blank optional ids as absent and trims valid identifiers" $ do
      (parseOptionalKeyField "targetRoom" Nothing :: Either ServerError (Maybe (Key M.Party)))
        `shouldBe` Right Nothing
      (parseOptionalKeyField "targetRoom" (Just "   ") :: Either ServerError (Maybe (Key M.Party)))
        `shouldBe` Right Nothing
      (parseOptionalKeyField "targetRoom" (Just " 42 ") :: Either ServerError (Maybe (Key M.Party)))
        `shouldBe` Right (Just (toSqlKey 42))

    it "rejects malformed optional ids instead of silently treating them as missing" $
      case (parseOptionalKeyField "targetSession" (Just "abc") :: Either ServerError (Maybe (Key M.Party))) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "targetSession must be a valid identifier"
        Right value ->
          expectationFailure ("Expected invalid optional key input error, got " <> show value)

  describe "validateCheckoutTargets" $ do
    let roomId = case (fromPathPiece "00000000-0000-0000-0000-000000000042" :: Maybe (Key Room)) of
          Just key -> key
          Nothing -> error "invalid room fixture key"
        sessionId = case (fromPathPiece "00000000-0000-0000-0000-000000000084" :: Maybe (Key Session)) of
          Just key -> key
          Nothing -> error "invalid session fixture key"

    it "accepts target fields that exactly match the declared checkout target kind" $ do
      validateCheckoutTargets TargetParty (Just "  Backline Crew  ") Nothing Nothing
        `shouldBe` Right (Just "Backline Crew", Nothing, Nothing)
      validateCheckoutTargets TargetRoom Nothing (Just roomId) Nothing
        `shouldBe` Right (Nothing, Just roomId, Nothing)
      validateCheckoutTargets TargetSession Nothing Nothing (Just sessionId)
        `shouldBe` Right (Nothing, Nothing, Just sessionId)

    it "rejects contradictory or destination-less checkout targets instead of creating ambiguous checkout rows" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected contradictory checkout target to be rejected, got " <> show value)
      assertInvalid "targetParty required for party checkout" (validateCheckoutTargets TargetParty Nothing Nothing Nothing)
      assertInvalid "targetParty required for party checkout" (validateCheckoutTargets TargetParty (Just "   ") Nothing Nothing)
      assertInvalid "targetRoom is only allowed for room checkout" (validateCheckoutTargets TargetParty (Just "Crew") (Just roomId) Nothing)
      assertInvalid "targetSession is only allowed for session checkout" (validateCheckoutTargets TargetParty (Just "Crew") Nothing (Just sessionId))
      assertInvalid "targetParty is only allowed for party checkout" (validateCheckoutTargets TargetRoom (Just "Crew") (Just roomId) Nothing)
      assertInvalid "targetParty is only allowed for party checkout" (validateCheckoutTargets TargetSession (Just "Crew") Nothing (Just sessionId))
      assertInvalid "targetSession is only allowed for session checkout" (validateCheckoutTargets TargetRoom Nothing (Just roomId) (Just sessionId))
      assertInvalid "targetRoom is only allowed for room checkout" (validateCheckoutTargets TargetSession Nothing (Just roomId) (Just sessionId))

  describe "validateCheckoutTargetReferences" $ do
    let existingRoomId = case (fromPathPiece "00000000-0000-0000-0000-000000000511" :: Maybe (Key Room)) of
          Just key -> key
          Nothing -> error "invalid existing checkout room fixture key"
        missingRoomId = case (fromPathPiece "00000000-0000-0000-0000-000000000512" :: Maybe (Key Room)) of
          Just key -> key
          Nothing -> error "invalid missing checkout room fixture key"
        existingSessionId = case (fromPathPiece "00000000-0000-0000-0000-000000000611" :: Maybe (Key Session)) of
          Just key -> key
          Nothing -> error "invalid existing checkout session fixture key"
        missingSessionId = case (fromPathPiece "00000000-0000-0000-0000-000000000612" :: Maybe (Key Session)) of
          Just key -> key
          Nothing -> error "invalid missing checkout session fixture key"

    it "accepts existing room and session checkout targets" $
      runSessionReferenceValidationSql $ do
        seedCheckoutTargetReferenceFixture existingRoomId existingSessionId
        existingRoomResult <- validateCheckoutTargetReferences (Just existingRoomId) Nothing
        existingSessionResult <- validateCheckoutTargetReferences Nothing (Just existingSessionId)
        noTargetResult <- validateCheckoutTargetReferences Nothing Nothing
        liftIO $ do
          existingRoomResult `shouldBe` Right ()
          existingSessionResult `shouldBe` Right ()
          noTargetResult `shouldBe` Right ()

    it "rejects unknown room or session checkout targets before the asset checkout insert can fail ambiguously" $
      runSessionReferenceValidationSql $ do
        seedCheckoutTargetReferenceFixture existingRoomId existingSessionId
        let assertInvalid expectedMessage result = case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected invalid checkout target reference error, got " <> show value)
        missingRoomResult <- validateCheckoutTargetReferences (Just missingRoomId) Nothing
        missingSessionResult <- validateCheckoutTargetReferences Nothing (Just missingSessionId)
        liftIO $ do
          assertInvalid "targetRoom references an unknown room" missingRoomResult
          assertInvalid "targetSession references an unknown session" missingSessionResult

  describe "normalizeAssetCheckinFields" $ do
    it "trims meaningful condition and notes before persisting a check-in" $ do
      normalizeAssetCheckinFields (AssetCheckinRequest (Just "  Returned OK  ") (Just "  Cables verified  "))
        `shouldBe` (Just "Returned OK", Just "Cables verified")

    it "drops omitted or blank check-in text so existing checkout context is not silently erased" $ do
      normalizeAssetCheckinFields (AssetCheckinRequest Nothing Nothing)
        `shouldBe` (Nothing, Nothing)
      normalizeAssetCheckinFields (AssetCheckinRequest (Just "   ") (Just "   "))
        `shouldBe` (Nothing, Nothing)

  describe "checkinAssetH" $ do
    let missingAssetId = "00000000-0000-0000-0000-000000000901"
        existingAssetId = "00000000-0000-0000-0000-000000000902"
        request = AssetCheckinRequest Nothing Nothing

    it "rejects unknown asset ids before collapsing them into missing checkout errors" $ do
      result <- runInventoryCheckinHandler (pure ()) missingAssetId request
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 404
          BL8.unpack (errBody err) `shouldContain` "Asset not found"
        Right value ->
          expectationFailure ("Expected missing asset check-in to fail, got " <> show value)

    it "keeps the active-checkout 404 for real assets that are not currently checked out" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing asset fixture key" >> fail "unreachable"
      result <- runInventoryCheckinHandler
        (insertKey assetKey (fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing))
        existingAssetId
        request
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 404
          BL8.unpack (errBody err) `shouldContain` "No active checkout"
        Right value ->
          expectationFailure ("Expected idle asset check-in to fail, got " <> show value)

  describe "validateSessionStatusInput" $ do
    it "preserves omitted values and normalizes supported session statuses" $ do
      validateSessionStatusInput Nothing `shouldBe` Right Nothing
      validateSessionStatusInput (Just " in_prep ") `shouldBe` Right (Just InPrep)
      validateSessionStatusInput (Just "In Session") `shouldBe` Right (Just InSession)

    it "rejects blank or unknown session statuses instead of silently defaulting them" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "Allowed values: in_prep, in_session, break, editing, approved, delivered, closed"
            Right value ->
              expectationFailure ("Expected invalid session status error, got " <> show value)
      assertInvalid (validateSessionStatusInput (Just "   "))
      assertInvalid (validateSessionStatusInput (Just "live"))

  describe "validateSessionTimeRange" $ do
    it "accepts sessions whose end time is strictly after the start time" $ do
      startAt <- getCurrentTime
      let endAt = addUTCTime 3600 startAt
      validateSessionTimeRange startAt endAt `shouldBe` Right ()

    it "rejects zero-length or backwards sessions before they can be created or patched" $ do
      startAt <- getCurrentTime
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "sessionEndAt must be after sessionStartAt"
            Right value ->
              expectationFailure ("Expected invalid session time range error, got " <> show value)
      assertInvalid (validateSessionTimeRange startAt startAt)
      assertInvalid (validateSessionTimeRange startAt (addUTCTime (-60) startAt))

  describe "validateDistinctSessionRooms" $ do
    let roomIdA = case (fromPathPiece "00000000-0000-0000-0000-000000000042" :: Maybe (Key Room)) of
          Just key -> key
          Nothing -> error "invalid room fixture key A"
        roomIdB = case (fromPathPiece "00000000-0000-0000-0000-000000000084" :: Maybe (Key Room)) of
          Just key -> key
          Nothing -> error "invalid room fixture key B"

    it "keeps empty and unique room selections unchanged" $ do
      validateDistinctSessionRooms [] `shouldBe` Right []
      validateDistinctSessionRooms [roomIdA, roomIdB] `shouldBe` Right [roomIdA, roomIdB]

    it "rejects duplicate room selections before session writes hit uniqueness constraints" $
      case validateDistinctSessionRooms [roomIdA, roomIdA] of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "roomIds must not contain duplicates"
        Right value ->
          expectationFailure ("Expected duplicate session room ids to be rejected, got " <> show value)

  describe "validateSessionReferences" $ do
    let existingBandId = case (fromPathPiece "00000000-0000-0000-0000-000000000111" :: Maybe (Key Band)) of
          Just key -> key
          Nothing -> error "invalid band fixture key"
        missingBandId = case (fromPathPiece "00000000-0000-0000-0000-000000000222" :: Maybe (Key Band)) of
          Just key -> key
          Nothing -> error "invalid missing band fixture key"
        existingRoomId = case (fromPathPiece "00000000-0000-0000-0000-000000000333" :: Maybe (Key Room)) of
          Just key -> key
          Nothing -> error "invalid room fixture key"
        missingRoomId = case (fromPathPiece "00000000-0000-0000-0000-000000000444" :: Maybe (Key Room)) of
          Just key -> key
          Nothing -> error "invalid missing room fixture key"

    it "accepts existing optional band refs and room refs used by session writes" $
      runSessionReferenceValidationSql $ do
        seedSessionReferenceFixture existingBandId existingRoomId
        noBandResult <- validateSessionReferences Nothing [existingRoomId]
        withBandResult <- validateSessionReferences (Just existingBandId) [existingRoomId]
        noRoomResult <- validateSessionReferences (Just existingBandId) []
        liftIO $ do
          noBandResult `shouldBe` Right ()
          withBandResult `shouldBe` Right ()
          noRoomResult `shouldBe` Right ()

    it "rejects unknown band or room references before session writes fall through to DB errors" $
      runSessionReferenceValidationSql $ do
        seedSessionReferenceFixture existingBandId existingRoomId
        let assertInvalid expectedMessage result = case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected invalid session reference error, got " <> show value)
        unknownBandResult <- validateSessionReferences (Just missingBandId) [existingRoomId]
        unknownRoomResult <- validateSessionReferences (Just existingBandId) [missingRoomId]
        liftIO $ do
          assertInvalid "bandId references an unknown band" unknownBandResult
          assertInvalid "roomIds reference one or more unknown rooms" unknownRoomResult

  describe "validateDistinctBandMemberIds" $ do
    let partyIdA = toSqlKey 42 :: Key M.Party
        partyIdB = toSqlKey 84 :: Key M.Party

    it "keeps empty and unique band member selections unchanged" $ do
      validateDistinctBandMemberIds [] `shouldBe` Right []
      validateDistinctBandMemberIds [partyIdA, partyIdB] `shouldBe` Right [partyIdA, partyIdB]

    it "rejects duplicate band members before band creation can persist ambiguous memberships" $
      case validateDistinctBandMemberIds [partyIdA, partyIdA] of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "band members must not contain duplicates"
        Right value ->
          expectationFailure ("Expected duplicate band member ids to be rejected, got " <> show value)

  describe "validatePaymentMethod" $ do
    it "accepts supported manual-payment aliases, including persisted enum labels reused by the UI" $ do
      validatePaymentMethod " Produbanco " `shouldBe` Right M.BankTransferM
      validatePaymentMethod "bank_transfer" `shouldBe` Right M.BankTransferM
      validatePaymentMethod "BankTransferM" `shouldBe` Right M.BankTransferM
      validatePaymentMethod "Card" `shouldBe` Right M.CardPOSM
      validatePaymentMethod "PayPalM" `shouldBe` Right M.PayPalM
      validatePaymentMethod "other" `shouldBe` Right M.OtherM

    it "rejects blank or unknown manual payment methods instead of silently storing them as bank transfers" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "paymentMethod must be one of"
            Right value ->
              expectationFailure ("Expected invalid payment method error, got " <> show value)
      assertInvalid (validatePaymentMethod "   ")
      assertInvalid (validatePaymentMethod "wire-transfer")

  describe "validatePaymentCurrency" $ do
    it "normalizes supported manual payment currencies to USD" $ do
      validatePaymentCurrency "USD" `shouldBe` Right "USD"
      validatePaymentCurrency " usd " `shouldBe` Right "USD"

    it "rejects blank or unsupported payment currencies instead of silently coercing them to USD" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "Only USD manual payments are currently supported"
            Right value ->
              expectationFailure ("Expected invalid payment currency error, got " <> show value)
      assertInvalid (validatePaymentCurrency "   ")
      assertInvalid (validatePaymentCurrency "EUR")

  describe "validatePaymentAmountCents" $ do
    it "accepts positive payment amounts without rewriting them" $ do
      validatePaymentAmountCents 1 `shouldBe` Right 1
      validatePaymentAmountCents 25000 `shouldBe` Right 25000

    it "rejects zero or negative payment amounts instead of persisting impossible manual payments" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "amountCents must be greater than 0"
            Right value ->
              expectationFailure ("Expected invalid payment amount error, got " <> show value)
      assertInvalid (validatePaymentAmountCents 0)
      assertInvalid (validatePaymentAmountCents (-500))

  describe "validatePositivePaymentReferenceId" $ do
    it "accepts positive payment, party, order, and invoice identifiers without rewriting them" $ do
      validatePositivePaymentReferenceId "paymentId" 9 `shouldBe` Right 9
      validateOptionalPositivePaymentReferenceId "partyId" (Just 7) `shouldBe` Right (Just 7)
      validateOptionalPositivePaymentReferenceId "orderId" Nothing `shouldBe` Right Nothing
      validateOptionalPositivePaymentReferenceId "invoiceId" (Just 12) `shouldBe` Right (Just 12)

    it "rejects zero or negative explicit references instead of silently broadening lookups or deferring to DB failures" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid payment reference error, got " <> show value)
      assertInvalid "paymentId must be a positive integer" (validatePositivePaymentReferenceId "paymentId" 0)
      assertInvalid "partyId must be a positive integer" (validateOptionalPositivePaymentReferenceId "partyId" (Just (-1)))
      assertInvalid "orderId must be a positive integer" (validateOptionalPositivePaymentReferenceId "orderId" (Just 0))

  describe "validatePaymentAttachmentUrl" $ do
    it "treats omitted or blank attachment URLs as absent and trims valid URLs" $ do
      validatePaymentAttachmentUrl Nothing `shouldBe` Right Nothing
      validatePaymentAttachmentUrl (Just "   ") `shouldBe` Right Nothing
      validatePaymentAttachmentUrl (Just "  https://files.example.com/proof.pdf  ")
        `shouldBe` Right (Just "https://files.example.com/proof.pdf")

    it "rejects malformed payment attachment URLs instead of storing unusable links" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "attachmentUrl must be an absolute http(s) URL"
            Right value ->
              expectationFailure ("Expected invalid payment attachment URL error, got " <> show value)
      assertInvalid (validatePaymentAttachmentUrl (Just "proof.pdf"))
      assertInvalid (validatePaymentAttachmentUrl (Just "https://files.example.com/proof copy.pdf"))
      assertInvalid (validatePaymentAttachmentUrl (Just "https://files/proof.pdf"))
      assertInvalid (validatePaymentAttachmentUrl (Just "https://2130706433/proof.pdf"))
      assertInvalid (validatePaymentAttachmentUrl (Just "https://256.256.256.256/proof.pdf"))

  describe "validatePaymentConcept" $ do
    it "trims meaningful concepts before storing manual payment rows" $ do
      validatePaymentConcept "  Honorarios abril  " `shouldBe` Right "Honorarios abril"

    it "rejects blank concepts instead of storing ambiguous payment descriptions" $
      case validatePaymentConcept "   " of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "concept is required"
        Right value ->
          expectationFailure ("Expected invalid payment concept error, got " <> show value)

  describe "validatePaymentReferences" $ do
    it "accepts existing party, order, and invoice references when the invoice includes that order" $ do
      now <- getCurrentTime
      runPaymentValidationSql $ do
        (partyId, _, orderId, _, _, invoiceId, _, _) <- seedPaymentReferenceFixture now
        result <- validatePaymentReferences partyId (Just orderId) (Just invoiceId)
        liftIO $ result `shouldBe` Right ()

    it "rejects missing or cross-party references before manual payments hit ambiguous DB errors" $ do
      now <- getCurrentTime
      runPaymentValidationSql $ do
        (partyId, otherPartyId, orderId, _, otherOrderId, invoiceId, samePartyOtherInvoiceId, otherInvoiceId) <- seedPaymentReferenceFixture now
        let assertInvalid expectedMessage validation = case validation of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected invalid payment reference error, got " <> show value)
        missingParty <- validatePaymentReferences (toSqlKey 999999) Nothing Nothing
        missingOrder <- validatePaymentReferences partyId (Just (toSqlKey 999998)) Nothing
        missingInvoice <- validatePaymentReferences partyId Nothing (Just (toSqlKey 999997))
        orderMismatch <- validatePaymentReferences partyId (Just otherOrderId) (Just invoiceId)
        invoiceMismatch <- validatePaymentReferences partyId (Just orderId) (Just otherInvoiceId)
        unrelatedInvoice <- validatePaymentReferences partyId (Just orderId) (Just samePartyOtherInvoiceId)
        otherPartyMismatch <- validatePaymentReferences otherPartyId (Just orderId) (Just invoiceId)
        liftIO $ do
          assertInvalid "partyId references an unknown party" missingParty
          assertInvalid "orderId references an unknown service order" missingOrder
          assertInvalid "invoiceId references an unknown invoice" missingInvoice
          assertInvalid "orderId does not belong to partyId" orderMismatch
          assertInvalid "invoiceId does not belong to partyId" invoiceMismatch
          assertInvalid "invoiceId does not include orderId" unrelatedInvoice
          assertInvalid "orderId does not belong to partyId" otherPartyMismatch

  describe "validateSocialLimit" $ do
    it "keeps the default only when the caller omits the limit" $ do
      validateSocialLimit Nothing `shouldBe` Right 100
      validateSocialLimit (Just 1) `shouldBe` Right 1
      validateSocialLimit (Just 200) `shouldBe` Right 200

    it "rejects out-of-range explicit limits instead of silently clamping inbox queries" $ do
      let assertInvalid rawLimit =
            case validateSocialLimit (Just rawLimit) of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "limit must be between 1 and 200"
              Right value ->
                expectationFailure ("Expected invalid social inbox limit error, got " <> show value)
      assertInvalid 0
      assertInvalid 201

  describe "social list filter parsing" $ do
    it "uses defaults only when direction and repliedOnly are omitted, while still normalizing supported explicit values" $ do
      (parseSocialDirectionParam Nothing :: Either ServerError (Maybe Text))
        `shouldBe` Right Nothing
      (parseSocialDirectionParam (Just " ALL ") :: Either ServerError (Maybe Text))
        `shouldBe` Right Nothing
      (parseSocialDirectionParam (Just " Incoming ") :: Either ServerError (Maybe Text))
        `shouldBe` Right (Just "incoming")
      (parseSocialBoolParam Nothing :: Either ServerError Bool)
        `shouldBe` Right False
      (parseSocialBoolParam (Just " YES ") :: Either ServerError Bool)
        `shouldBe` Right True
      (parseSocialBoolParam (Just "0") :: Either ServerError Bool)
        `shouldBe` Right False

    it "rejects explicitly blank or unknown filter values instead of silently widening inbox queries" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid social list filter error, got " <> show value)
      assertInvalid
        "direction must be omitted or one of: all, incoming, outgoing"
        ((parseSocialDirectionParam (Just "   ")) :: Either ServerError (Maybe Text))
      assertInvalid
        "direction must be omitted or one of: all, incoming, outgoing"
        ((parseSocialDirectionParam (Just "sideways")) :: Either ServerError (Maybe Text))
      assertInvalid
        "repliedOnly must be omitted or one of: true, false, 1, 0, yes, no"
        ((parseSocialBoolParam (Just "   ")) :: Either ServerError Bool)
      assertInvalid
        "repliedOnly must be omitted or one of: true, false, 1, 0, yes, no"
        ((parseSocialBoolParam (Just "maybe")) :: Either ServerError Bool)

  describe "social reply input validation" $ do
    it "trims valid sender and external ids before reply dispatch" $ do
      validateSocialReplySenderId "  17841400000000000  "
        `shouldBe` Right "17841400000000000"
      validateSocialReplyExternalId (Just "  mid.abc123  ")
        `shouldBe` Right (Just "mid.abc123")
      validateSocialReplyExternalId Nothing `shouldBe` Right Nothing

    it "rejects blank sender ids or explicitly blank external ids instead of sending ambiguous replies" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid social reply input error, got " <> show value)
      assertInvalid "senderId is required" (validateSocialReplySenderId "   ")
      assertInvalid "externalId must be omitted or a non-empty string" (validateSocialReplyExternalId (Just "   "))

    it "rejects malformed reply identifiers before dispatching to Meta" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure
                ("Expected malformed social reply identifier error, got " <> show value)
          longSenderId = mconcat (replicate 257 ("x" :: Text))
      assertInvalid
        "senderId must not contain whitespace"
        (validateSocialReplySenderId "178414 000000")
      assertInvalid
        "externalId must not contain whitespace"
        (validateSocialReplyExternalId (Just "mid.abc 123"))
      assertInvalid
        "senderId must be 256 characters or fewer"
        (validateSocialReplySenderId longSenderId)

  describe "social reply outcome persistence" $ do
    it "marks failed manual sends as error rows instead of leaving ambiguous sent status behind" $
      socialReplyOutcomeFields (Left "Meta API timeout" :: Either Text A.Value)
        `shouldBe` ("error", Just "Meta API timeout")

    it "marks successful manual sends as sent rows without a reply error" $
      socialReplyOutcomeFields (Right (A.object ["ok" .= True]) :: Either Text A.Value)
        `shouldBe` ("sent", Nothing)

  describe "normalizeServiceCatalogNameUpdate" $ do
    it "preserves omitted names and trims meaningful updates" $ do
      normalizeServiceCatalogNameUpdate Nothing `shouldBe` Right Nothing
      normalizeServiceCatalogNameUpdate (Just "  Mezcla Full  ") `shouldBe` Right (Just "Mezcla Full")

    it "rejects explicit blank names instead of silently treating them as no-op updates" $
      case normalizeServiceCatalogNameUpdate (Just "   ") of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Nombre requerido"
        Right value ->
          expectationFailure ("Expected blank service catalog update name to be rejected, got " <> show value)

  describe "validateServiceCatalogCurrency" $ do
    it "defaults omitted values to USD and normalizes supported ISO codes" $ do
      validateServiceCatalogCurrency Nothing `shouldBe` Right "USD"
      validateServiceCatalogCurrency (Just " usd ") `shouldBe` Right "USD"
      validateServiceCatalogCurrency (Just "eur") `shouldBe` Right "EUR"

    it "rejects blank or malformed currency codes instead of storing ambiguous data" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "código ISO de 3 letras"
            Right value ->
              expectationFailure ("Expected invalid currency error, got " <> show value)
      assertInvalid (validateServiceCatalogCurrency (Just "   "))
      assertInvalid (validateServiceCatalogCurrency (Just "usdollars"))
      assertInvalid (validateServiceCatalogCurrency (Just "12$"))

  describe "validateServiceCatalogCurrencyUpdate" $ do
    it "preserves omitted updates and normalizes meaningful ones" $ do
      validateServiceCatalogCurrencyUpdate Nothing `shouldBe` Right Nothing
      validateServiceCatalogCurrencyUpdate (Just " gbp ") `shouldBe` Right (Just "GBP")

    it "rejects explicit blank updates instead of silently resetting the currency" $
      case validateServiceCatalogCurrencyUpdate (Just "   ") of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "código ISO de 3 letras"
        Right value ->
          expectationFailure ("Expected invalid currency update error, got " <> show value)

  describe "validateServiceCatalogTaxBps" $ do
    it "accepts omitted values and basis points within a 0..10000 percentage range" $ do
      validateServiceCatalogTaxBps Nothing `shouldBe` Right Nothing
      validateServiceCatalogTaxBps (Just 0) `shouldBe` Right (Just 0)
      validateServiceCatalogTaxBps (Just 850) `shouldBe` Right (Just 850)
      validateServiceCatalogTaxBps (Just 10000) `shouldBe` Right (Just 10000)

    it "rejects negative or oversized tax percentages before they can be stored" $ do
      let assertInvalid result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "entre 0 y 10000"
            Right value ->
              expectationFailure ("Expected invalid tax basis points error, got " <> show value)
      assertInvalid (validateServiceCatalogTaxBps (Just (-1)))
      assertInvalid (validateServiceCatalogTaxBps (Just 10001))

  describe "validateServiceCatalogTaxBpsUpdate" $ do
    it "preserves omitted and explicit-null updates" $ do
      validateServiceCatalogTaxBpsUpdate Nothing `shouldBe` Right Nothing
      validateServiceCatalogTaxBpsUpdate (Just Nothing) `shouldBe` Right (Just Nothing)
      validateServiceCatalogTaxBpsUpdate (Just (Just 1200)) `shouldBe` Right (Just (Just 1200))

    it "rejects invalid update values instead of storing impossible tax percentages" $
      case validateServiceCatalogTaxBpsUpdate (Just (Just 15000)) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "entre 0 y 10000"
        Right value ->
          expectationFailure ("Expected invalid service catalog tax update error, got " <> show value)

  describe "Meta inbox deletion handling" $ do
    it "parses deleted Instagram webhook events" $ do
        let payload =
                A.object
                    [ "object" .= ("instagram" :: Text)
                    , "entry"
                        .=
                            [ A.object
                                [ "id" .= ("17841400000000000" :: Text)
                                , "changes"
                                    .=
                                        [ A.object
                                            [ "field" .= ("messages" :: Text)
                                            , "value"
                                                .= A.object
                                                    [ "from" .= A.object ["id" .= ("user-1" :: Text), "username" .= ("fan-user" :: Text)]
                                                    , "timestamp" .= (1773630000 :: Int)
                                                    , "message" .= A.object ["mid" .= ("mid-1" :: Text), "is_deleted" .= True]
                                                    ]
                                            ]
                                        ]
                                ]
                            ]
                    ]
            events = extractMetaInbound payload
        case events of
            [MetaInboundDeleted deletedEvent] -> do
                igInboundDeletedExternalId deletedEvent `shouldBe` "mid-1"
                igInboundDeletedSenderId deletedEvent `shouldBe` "user-1"
                igInboundDeletedSenderName deletedEvent `shouldBe` Just "fan-user"
                igInboundDeletedMetadata deletedEvent `shouldSatisfy` (/= Nothing)
            _ -> expectationFailure ("Expected a deleted event, got " <> show events)

    it "tombstones deleted Instagram messages without resurrecting them on later upserts" $ do
        now <- getCurrentTime
        let deletedAt = addUTCTime 60 now
            replayedAt = addUTCTime 120 now
            original =
                MetaInboundMessage
                    IGInbound
                        { igInboundExternalId = "mid-1"
                        , igInboundSenderId = "user-1"
                        , igInboundSenderName = Just "Fan"
                        , igInboundText = "hola"
                        , igInboundAdExternalId = Nothing
                        , igInboundAdName = Nothing
                        , igInboundCampaignExternalId = Nothing
                        , igInboundCampaignName = Nothing
                        , igInboundMetadata = Just "{\"recipient_id\":\"biz-1\"}"
                        }
            deletedEvent =
                MetaInboundDeleted
                    IGInboundDeleted
                        { igInboundDeletedExternalId = "mid-1"
                        , igInboundDeletedSenderId = "user-1"
                        , igInboundDeletedSenderName = Just "Fan"
                        , igInboundDeletedMetadata = Just "{\"event\":\"message_deleted\"}"
                        }
            lateReplay =
                MetaInboundMessage
                    IGInbound
                        { igInboundExternalId = "mid-1"
                        , igInboundSenderId = "user-1"
                        , igInboundSenderName = Just "Fan"
                        , igInboundText = "hola de nuevo"
                        , igInboundAdExternalId = Nothing
                        , igInboundAdName = Nothing
                        , igInboundCampaignExternalId = Nothing
                        , igInboundCampaignName = Nothing
                        , igInboundMetadata = Just "{\"recipient_id\":\"biz-1\"}"
                        }
        runMetaInboxSql $ do
            persistMetaInbound MetaInstagram now [original]
            persistMetaInbound MetaInstagram deletedAt [deletedEvent]
            persistMetaInbound MetaInstagram replayedAt [lateReplay]

            stored <- getBy (M.UniqueInstagramMessage "mid-1")
            visible <- selectList [M.InstagramMessageDeletedAt ==. Nothing] []
            liftIO $ do
                case stored of
                    Nothing -> expectationFailure "Expected stored Instagram message"
                    Just (Entity _ row) -> do
                        M.instagramMessageText row `shouldBe` Just "hola de nuevo"
                        M.instagramMessageDeletedAt row `shouldBe` Just deletedAt
                length visible `shouldBe` 0

runMetaInboxSql :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runMetaInboxSql action =
    runSqlite ":memory:" $ do
        initializeMetaInboxSchema
        action

runPaymentValidationSql :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runPaymentValidationSql action =
    runSqlite ":memory:" $ do
        initializePaymentValidationSchema
        action

runSessionReferenceValidationSql :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runSessionReferenceValidationSql action =
    runSqlite ":memory:" $ do
        initializeSessionReferenceValidationSchema
        action

runInventoryCheckinHandler
  :: SqlPersistT IO ()
  -> Text
  -> AssetCheckinRequest
  -> IO (Either ServerError AssetCheckoutDTO)
runInventoryCheckinHandler setup rawId req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeInventoryCheckinSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in inventory check-in tests"
            }
    liftIO $ runExceptT (runReaderT (checkinHandlerFor inventoryUser rawId req) env)

runRoomCreateHandler
  :: SqlPersistT IO ()
  -> RoomCreate
  -> IO (Either ServerError RoomDTO)
runRoomCreateHandler setup req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeRoomSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in room tests"
            }
    liftIO $ runExceptT (runReaderT (createRoomHandlerFor inventoryUser req) env)

runRoomPatchHandler
  :: SqlPersistT IO ()
  -> Text
  -> RoomUpdate
  -> IO (Either ServerError RoomDTO)
runRoomPatchHandler setup rawId req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeRoomSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in room tests"
            }
    liftIO $ runExceptT (runReaderT (patchRoomHandlerFor inventoryUser rawId req) env)

runPipelineCreateHandler
  :: SqlPersistT IO ()
  -> Text
  -> PipelineCardCreate
  -> IO (Either ServerError PipelineCardDTO)
runPipelineCreateHandler setup rawType req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializePipelineSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in pipeline tests"
            }
    liftIO $ runExceptT (runReaderT (createPipelineHandlerFor inventoryUser rawType req) env)

runPipelinePatchHandler
  :: SqlPersistT IO ()
  -> Text
  -> Text
  -> PipelineCardUpdate
  -> IO (Either ServerError PipelineCardDTO)
runPipelinePatchHandler setup rawType rawId req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializePipelineSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in pipeline tests"
            }
    liftIO $ runExceptT (runReaderT (patchPipelineHandlerFor inventoryUser rawType rawId req) env)

inventoryUser :: AuthedUser
inventoryUser =
  AuthedUser
    { auPartyId = toSqlKey 1
    , auRoles = [M.Admin]
    , auModules = modulesForRoles [M.Admin]
    }

checkinHandlerFor :: AuthedUser -> Text -> AssetCheckinRequest -> InventoryTestM AssetCheckoutDTO
checkinHandlerFor user =
  case (inventoryServer user :: ServerT InventoryAPI InventoryTestM) of
    _listAssets
      :<|> _createAsset
      :<|> _uploadAssetPhoto
      :<|> _getAsset
      :<|> _patchAsset
      :<|> _deleteAsset
      :<|> _checkoutAsset
      :<|> checkinAsset
      :<|> _checkoutHistory
      :<|> _refreshQr
      :<|> _resolveByQr ->
          checkinAsset

createRoomHandlerFor :: AuthedUser -> RoomCreate -> InventoryTestM RoomDTO
createRoomHandlerFor user =
  case (roomsServer user :: ServerT RoomsAPI InventoryTestM) of
    _listRooms
      :<|> createRoom
      :<|> _patchRoom ->
          createRoom

patchRoomHandlerFor :: AuthedUser -> Text -> RoomUpdate -> InventoryTestM RoomDTO
patchRoomHandlerFor user =
  case (roomsServer user :: ServerT RoomsAPI InventoryTestM) of
    _listRooms
      :<|> _createRoom
      :<|> patchRoom ->
          patchRoom

createPipelineHandlerFor :: AuthedUser -> Text -> PipelineCardCreate -> InventoryTestM PipelineCardDTO
createPipelineHandlerFor user rawType =
  case (pipelinesServer user :: ServerT PipelinesAPI InventoryTestM) of
    pipelineRoutes ->
      case pipelineRoutes rawType of
        _listCards
          :<|> _listStages
          :<|> createCard
          :<|> _cardServer ->
              createCard

patchPipelineHandlerFor :: AuthedUser -> Text -> Text -> PipelineCardUpdate -> InventoryTestM PipelineCardDTO
patchPipelineHandlerFor user rawType rawId =
  case (pipelinesServer user :: ServerT PipelinesAPI InventoryTestM) of
    pipelineRoutes ->
      case pipelineRoutes rawType of
        _listCards
          :<|> _listStages
          :<|> _createCard
          :<|> cardRoutes ->
              case cardRoutes rawId of
                _getCard :<|> patchCard :<|> _deleteCard ->
                  patchCard

initializeMetaInboxSchema :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
initializeMetaInboxSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"instagram_message\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"external_id\" VARCHAR NOT NULL,\
        \\"sender_id\" VARCHAR NOT NULL,\
        \\"sender_name\" VARCHAR NULL,\
        \\"text\" VARCHAR NULL,\
        \\"direction\" VARCHAR NOT NULL,\
        \\"ad_external_id\" VARCHAR NULL,\
        \\"ad_name\" VARCHAR NULL,\
        \\"campaign_external_id\" VARCHAR NULL,\
        \\"campaign_name\" VARCHAR NULL,\
        \\"metadata\" VARCHAR NULL,\
        \\"reply_status\" VARCHAR NOT NULL,\
        \\"hold_reason\" VARCHAR NULL,\
        \\"hold_required_fields\" VARCHAR NULL,\
        \\"last_attempt_at\" TIMESTAMP NULL,\
        \\"attempt_count\" INTEGER NOT NULL,\
        \\"replied_at\" TIMESTAMP NULL,\
        \\"reply_text\" VARCHAR NULL,\
        \\"reply_error\" VARCHAR NULL,\
        \\"deleted_at\" TIMESTAMP NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \CONSTRAINT \"unique_instagram_message\" UNIQUE (\"external_id\")\
        \)"
        []

initializeSessionReferenceValidationSchema :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
initializeSessionReferenceValidationSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"band\" (\
        \\"id\" uuid PRIMARY KEY,\
        \\"party_id\" INTEGER NOT NULL,\
        \\"name\" VARCHAR NOT NULL,\
        \\"label_artist\" BOOLEAN NOT NULL,\
        \\"primary_genre\" VARCHAR NULL,\
        \\"home_city\" VARCHAR NULL,\
        \\"photo_url\" VARCHAR NULL,\
        \\"contract_flags\" VARCHAR NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"room\" (\
        \\"id\" uuid PRIMARY KEY,\
        \\"name\" VARCHAR NOT NULL,\
        \\"is_bookable\" BOOLEAN NOT NULL,\
        \\"capacity\" INTEGER NULL,\
        \\"channel_count\" INTEGER NULL,\
        \\"default_sample_rate\" INTEGER NULL,\
        \\"patchbay_notes\" VARCHAR NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"session\" (\
        \\"id\" uuid PRIMARY KEY,\
        \\"booking_ref\" VARCHAR NULL,\
        \\"band_id\" uuid NULL,\
        \\"client_party_ref\" VARCHAR NULL,\
        \\"service\" VARCHAR NOT NULL,\
        \\"start_at\" TIMESTAMP NOT NULL,\
        \\"end_at\" TIMESTAMP NOT NULL,\
        \\"engineer_ref\" VARCHAR NOT NULL,\
        \\"assistant_ref\" VARCHAR NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"sample_rate\" INTEGER NULL,\
        \\"bit_depth\" INTEGER NULL,\
        \\"daw\" VARCHAR NULL,\
        \\"session_folder_drive_id\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL\
        \)"
        []

initializeInventoryCheckinSchema :: SqlPersistT IO ()
initializeInventoryCheckinSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"asset\" (\
        \\"id\" uuid PRIMARY KEY,\
        \\"name\" VARCHAR NOT NULL,\
        \\"category\" VARCHAR NOT NULL,\
        \\"brand\" VARCHAR NULL,\
        \\"model\" VARCHAR NULL,\
        \\"serial_number\" VARCHAR NULL,\
        \\"purchase_date\" DATE NULL,\
        \\"purchase_price_usd_cents\" INTEGER NULL,\
        \\"condition\" VARCHAR NOT NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"location_id\" uuid NULL,\
        \\"owner\" VARCHAR NOT NULL,\
        \\"qr_code\" VARCHAR NULL,\
        \\"photo_url\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"warranty_expires\" DATE NULL,\
        \\"maintenance_policy\" VARCHAR NOT NULL,\
        \\"next_maintenance_due\" DATE NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"asset_checkout\" (\
        \\"id\" uuid PRIMARY KEY,\
        \\"asset_id\" uuid NOT NULL,\
        \\"target_kind\" VARCHAR NOT NULL,\
        \\"target_session_id\" uuid NULL,\
        \\"target_party_ref\" VARCHAR NULL,\
        \\"target_room_id\" uuid NULL,\
        \\"checked_out_by_ref\" VARCHAR NOT NULL,\
        \\"checked_out_at\" TIMESTAMP NOT NULL,\
        \\"due_at\" TIMESTAMP NULL,\
        \\"condition_out\" VARCHAR NULL,\
        \\"photo_drive_file_id\" VARCHAR NULL,\
        \\"returned_at\" TIMESTAMP NULL,\
        \\"condition_in\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL\
        \)"
        []

initializeRoomSchema :: SqlPersistT IO ()
initializeRoomSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"room\" (\
        \\"id\" uuid PRIMARY KEY,\
        \\"name\" VARCHAR NOT NULL,\
        \\"is_bookable\" BOOLEAN NOT NULL,\
        \\"capacity\" INTEGER NULL,\
        \\"channel_count\" INTEGER NULL,\
        \\"default_sample_rate\" INTEGER NULL,\
        \\"patchbay_notes\" VARCHAR NULL,\
        \CONSTRAINT \"unique_room_name\" UNIQUE (\"name\")\
        \)"
        []

initializePipelineSchema :: SqlPersistT IO ()
initializePipelineSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"pipeline_card\" (\
        \\"id\" uuid PRIMARY KEY NOT NULL DEFAULT (lower(hex(randomblob(4)) || '-' || hex(randomblob(2)) || '-' || '4' || substr(hex(randomblob(2)), 2) || '-' || substr('89ab', (abs(random()) % 4) + 1, 1) || substr(hex(randomblob(2)), 2) || '-' || hex(randomblob(6)))),\
        \\"service_kind\" VARCHAR NOT NULL,\
        \\"title\" VARCHAR NOT NULL,\
        \\"artist\" VARCHAR NULL,\
        \\"stage\" VARCHAR NOT NULL,\
        \\"sort_order\" INTEGER NOT NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL\
        \)"
        []

initializePaymentValidationSchema :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
initializePaymentValidationSchema = do
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
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"service_catalog\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"name\" VARCHAR NOT NULL,\
        \\"kind\" VARCHAR NOT NULL,\
        \\"pricing_model\" VARCHAR NOT NULL,\
        \\"default_rate_cents\" INTEGER NULL,\
        \\"tax_bps\" INTEGER NULL,\
        \\"currency\" VARCHAR NOT NULL,\
        \\"billing_unit\" VARCHAR NULL,\
        \\"active\" BOOLEAN NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"service_order\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"customer_id\" INTEGER NOT NULL REFERENCES \"party\" ON DELETE RESTRICT ON UPDATE RESTRICT,\
        \\"artist_id\" INTEGER NULL REFERENCES \"party\" ON DELETE RESTRICT ON UPDATE RESTRICT,\
        \\"catalog_id\" INTEGER NOT NULL REFERENCES \"service_catalog\" ON DELETE RESTRICT ON UPDATE RESTRICT,\
        \\"service_kind\" VARCHAR NOT NULL,\
        \\"title\" VARCHAR NULL,\
        \\"description\" VARCHAR NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"price_quoted_cents\" INTEGER NULL,\
        \\"quote_sent_at\" TIMESTAMP NULL,\
        \\"scheduled_start\" TIMESTAMP NULL,\
        \\"scheduled_end\" TIMESTAMP NULL,\
        \\"created_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"package_product\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"name\" VARCHAR NOT NULL,\
        \\"service_kind\" VARCHAR NOT NULL,\
        \\"units_kind\" VARCHAR NOT NULL,\
        \\"units_qty\" INTEGER NOT NULL,\
        \\"price_cents\" INTEGER NOT NULL,\
        \\"expires_days\" INTEGER NULL,\
        \\"transferable\" BOOLEAN NOT NULL,\
        \\"refund_policy\" VARCHAR NOT NULL,\
        \\"active\" BOOLEAN NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"package_purchase\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"buyer_id\" INTEGER NOT NULL REFERENCES \"party\" ON DELETE RESTRICT ON UPDATE RESTRICT,\
        \\"product_id\" INTEGER NOT NULL REFERENCES \"package_product\" ON DELETE RESTRICT ON UPDATE RESTRICT,\
        \\"purchased_at\" TIMESTAMP NOT NULL,\
        \\"price_cents\" INTEGER NOT NULL,\
        \\"expires_at\" TIMESTAMP NULL,\
        \\"remaining_units\" INTEGER NOT NULL,\
        \\"status\" VARCHAR NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"invoice\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"customer_id\" INTEGER NOT NULL REFERENCES \"party\" ON DELETE RESTRICT ON UPDATE RESTRICT,\
        \\"issue_date\" DATE NOT NULL,\
        \\"due_date\" DATE NOT NULL,\
        \\"number\" VARCHAR NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"currency\" VARCHAR NOT NULL,\
        \\"subtotal_cents\" INTEGER NOT NULL,\
        \\"tax_cents\" INTEGER NOT NULL,\
        \\"total_cents\" INTEGER NOT NULL,\
        \\"sri_document_id\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \CONSTRAINT \"unique_invoice_number\" UNIQUE (\"number\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"invoice_line\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"invoice_id\" INTEGER NOT NULL REFERENCES \"invoice\" ON DELETE RESTRICT ON UPDATE RESTRICT,\
        \\"service_order_id\" INTEGER NULL REFERENCES \"service_order\" ON DELETE RESTRICT ON UPDATE RESTRICT,\
        \\"package_purchase_id\" INTEGER NULL REFERENCES \"package_purchase\" ON DELETE RESTRICT ON UPDATE RESTRICT,\
        \\"description\" VARCHAR NOT NULL,\
        \\"quantity\" INTEGER NOT NULL,\
        \\"unit_cents\" INTEGER NOT NULL,\
        \\"tax_bps\" INTEGER NOT NULL,\
        \\"total_cents\" INTEGER NOT NULL\
        \)"
        []

seedSessionReferenceFixture
  :: Key Band
  -> Key Room
  -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
seedSessionReferenceFixture bandId roomId = do
  insertKey bandId Band
    { bandPartyId = toSqlKey 1
    , bandName = "Reference Fixture Band"
    , bandLabelArtist = False
    , bandPrimaryGenre = Nothing
    , bandHomeCity = Nothing
    , bandPhotoUrl = Nothing
    , bandContractFlags = Nothing
    }
  insertKey roomId Room
    { roomName = "Reference Fixture Room"
    , roomIsBookable = True
    , roomCapacity = Nothing
    , roomChannelCount = Nothing
    , roomDefaultSampleRate = Nothing
    , roomPatchbayNotes = Nothing
    }

seedCheckoutTargetReferenceFixture
  :: Key Room
  -> Key Session
  -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
seedCheckoutTargetReferenceFixture roomId sessionId = do
  insertKey roomId Room
    { roomName = "Checkout Fixture Room"
    , roomIsBookable = True
    , roomCapacity = Nothing
    , roomChannelCount = Nothing
    , roomDefaultSampleRate = Nothing
    , roomPatchbayNotes = Nothing
    }
  now <- liftIO getCurrentTime
  insertKey sessionId Session
    { sessionBookingRef = Nothing
    , sessionBandId = Nothing
    , sessionClientPartyRef = Nothing
    , sessionService = "Tracking"
    , sessionStartAt = now
    , sessionEndAt = addUTCTime 3600 now
    , sessionEngineerRef = "Engineer"
    , sessionAssistantRef = Nothing
    , sessionStatus = InPrep
    , sessionSampleRate = Nothing
    , sessionBitDepth = Nothing
    , sessionDaw = Nothing
    , sessionSessionFolderDriveId = Nothing
    , sessionNotes = Nothing
    }

seedPaymentReferenceFixture
  :: UTCTime
  -> SqlPersistT (NoLoggingT (ResourceT IO))
       ( Key M.Party
       , Key M.Party
       , Key M.ServiceOrder
       , Key M.ServiceOrder
       , Key M.ServiceOrder
       , Key M.Invoice
       , Key M.Invoice
       , Key M.Invoice
       )
seedPaymentReferenceFixture now = do
  partyId <- insert M.Party
    { M.partyLegalName = Nothing
    , M.partyDisplayName = "Payment Fixture Party"
    , M.partyIsOrg = False
    , M.partyTaxId = Nothing
    , M.partyPrimaryEmail = Nothing
    , M.partyPrimaryPhone = Nothing
    , M.partyWhatsapp = Nothing
    , M.partyInstagram = Nothing
    , M.partyEmergencyContact = Nothing
    , M.partyNotes = Nothing
    , M.partyCreatedAt = now
    }
  otherPartyId <- insert M.Party
    { M.partyLegalName = Nothing
    , M.partyDisplayName = "Other Payment Fixture Party"
    , M.partyIsOrg = False
    , M.partyTaxId = Nothing
    , M.partyPrimaryEmail = Nothing
    , M.partyPrimaryPhone = Nothing
    , M.partyWhatsapp = Nothing
    , M.partyInstagram = Nothing
    , M.partyEmergencyContact = Nothing
    , M.partyNotes = Nothing
    , M.partyCreatedAt = now
    }
  catalogId <- insert M.ServiceCatalog
    { M.serviceCatalogName = "Tracking"
    , M.serviceCatalogKind = M.Recording
    , M.serviceCatalogPricingModel = M.Hourly
    , M.serviceCatalogDefaultRateCents = Just 10000
    , M.serviceCatalogTaxBps = Nothing
    , M.serviceCatalogCurrency = "USD"
    , M.serviceCatalogBillingUnit = Just "hour"
    , M.serviceCatalogActive = True
    }
  orderId <- insert M.ServiceOrder
    { M.serviceOrderCustomerId = partyId
    , M.serviceOrderArtistId = Nothing
    , M.serviceOrderCatalogId = catalogId
    , M.serviceOrderServiceKind = M.Recording
    , M.serviceOrderTitle = Just "EP tracking"
    , M.serviceOrderDescription = Nothing
    , M.serviceOrderStatus = "quoted"
    , M.serviceOrderPriceQuotedCents = Just 10000
    , M.serviceOrderQuoteSentAt = Nothing
    , M.serviceOrderScheduledStart = Nothing
    , M.serviceOrderScheduledEnd = Nothing
    , M.serviceOrderCreatedAt = now
    }
  samePartyOtherOrderId <- insert M.ServiceOrder
    { M.serviceOrderCustomerId = partyId
    , M.serviceOrderArtistId = Nothing
    , M.serviceOrderCatalogId = catalogId
    , M.serviceOrderServiceKind = M.Recording
    , M.serviceOrderTitle = Just "Mix revisions"
    , M.serviceOrderDescription = Nothing
    , M.serviceOrderStatus = "quoted"
    , M.serviceOrderPriceQuotedCents = Just 12000
    , M.serviceOrderQuoteSentAt = Nothing
    , M.serviceOrderScheduledStart = Nothing
    , M.serviceOrderScheduledEnd = Nothing
    , M.serviceOrderCreatedAt = now
    }
  otherOrderId <- insert M.ServiceOrder
    { M.serviceOrderCustomerId = otherPartyId
    , M.serviceOrderArtistId = Nothing
    , M.serviceOrderCatalogId = catalogId
    , M.serviceOrderServiceKind = M.Recording
    , M.serviceOrderTitle = Just "Other session"
    , M.serviceOrderDescription = Nothing
    , M.serviceOrderStatus = "quoted"
    , M.serviceOrderPriceQuotedCents = Just 8000
    , M.serviceOrderQuoteSentAt = Nothing
    , M.serviceOrderScheduledStart = Nothing
    , M.serviceOrderScheduledEnd = Nothing
    , M.serviceOrderCreatedAt = now
    }
  let today = utctDay now
  invoiceId <- insert M.Invoice
    { M.invoiceCustomerId = partyId
    , M.invoiceIssueDate = today
    , M.invoiceDueDate = today
    , M.invoiceNumber = Nothing
    , M.invoiceStatus = M.Draft
    , M.invoiceCurrency = "USD"
    , M.invoiceSubtotalCents = 10000
    , M.invoiceTaxCents = 0
    , M.invoiceTotalCents = 10000
    , M.invoiceSriDocumentId = Nothing
    , M.invoiceNotes = Nothing
    , M.invoiceCreatedAt = now
    }
  samePartyOtherInvoiceId <- insert M.Invoice
    { M.invoiceCustomerId = partyId
    , M.invoiceIssueDate = today
    , M.invoiceDueDate = today
    , M.invoiceNumber = Nothing
    , M.invoiceStatus = M.Draft
    , M.invoiceCurrency = "USD"
    , M.invoiceSubtotalCents = 12000
    , M.invoiceTaxCents = 0
    , M.invoiceTotalCents = 12000
    , M.invoiceSriDocumentId = Nothing
    , M.invoiceNotes = Nothing
    , M.invoiceCreatedAt = now
    }
  otherInvoiceId <- insert M.Invoice
    { M.invoiceCustomerId = otherPartyId
    , M.invoiceIssueDate = today
    , M.invoiceDueDate = today
    , M.invoiceNumber = Nothing
    , M.invoiceStatus = M.Draft
    , M.invoiceCurrency = "USD"
    , M.invoiceSubtotalCents = 8000
    , M.invoiceTaxCents = 0
    , M.invoiceTotalCents = 8000
    , M.invoiceSriDocumentId = Nothing
    , M.invoiceNotes = Nothing
    , M.invoiceCreatedAt = now
    }
  _ <- insert M.InvoiceLine
    { M.invoiceLineInvoiceId = invoiceId
    , M.invoiceLineServiceOrderId = Just orderId
    , M.invoiceLinePackagePurchaseId = Nothing
    , M.invoiceLineDescription = "EP tracking"
    , M.invoiceLineQuantity = 1
    , M.invoiceLineUnitCents = 10000
    , M.invoiceLineTaxBps = 0
    , M.invoiceLineTotalCents = 10000
    }
  _ <- insert M.InvoiceLine
    { M.invoiceLineInvoiceId = samePartyOtherInvoiceId
    , M.invoiceLineServiceOrderId = Just samePartyOtherOrderId
    , M.invoiceLinePackagePurchaseId = Nothing
    , M.invoiceLineDescription = "Mix revisions"
    , M.invoiceLineQuantity = 1
    , M.invoiceLineUnitCents = 12000
    , M.invoiceLineTaxBps = 0
    , M.invoiceLineTotalCents = 12000
    }
  _ <- insert M.InvoiceLine
    { M.invoiceLineInvoiceId = otherInvoiceId
    , M.invoiceLineServiceOrderId = Just otherOrderId
    , M.invoiceLinePackagePurchaseId = Nothing
    , M.invoiceLineDescription = "Other session"
    , M.invoiceLineQuantity = 1
    , M.invoiceLineUnitCents = 8000
    , M.invoiceLineTaxBps = 0
    , M.invoiceLineTotalCents = 8000
    }
  pure
    ( partyId
    , otherPartyId
    , orderId
    , samePartyOtherOrderId
    , otherOrderId
    , invoiceId
    , samePartyOtherInvoiceId
    , otherInvoiceId
    )

fixtureAsset :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Maybe Text -> Asset
fixtureAsset name category brand model owner notes =
  Asset
    { assetName = name
    , assetCategory = category
    , assetBrand = brand
    , assetModel = model
    , assetSerialNumber = Nothing
    , assetPurchaseDate = Nothing
    , assetPurchasePriceUsdCents = Nothing
    , assetCondition = Good
    , assetStatus = Active
    , assetLocationId = Nothing
    , assetOwner = owner
    , assetQrCode = Nothing
    , assetPhotoUrl = Nothing
    , assetNotes = notes
    , assetWarrantyExpires = Nothing
    , assetMaintenancePolicy = None
    , assetNextMaintenanceDue = Nothing
    }

fixtureRoom :: Text -> Room
fixtureRoom name =
  Room
    { roomName = name
    , roomIsBookable = True
    , roomCapacity = Nothing
    , roomChannelCount = Nothing
    , roomDefaultSampleRate = Nothing
    , roomPatchbayNotes = Nothing
    }
