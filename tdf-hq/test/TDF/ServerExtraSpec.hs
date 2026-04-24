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
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, utctDay)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.Persist hiding (Active)
import Database.Persist.Sql (SqlBackend, SqlPersistT, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool, runSqlite)
import Servant (ServerError (errBody, errHTTPCode), ServerT, (:<|>) (..))
import Servant.Multipart
  ( FileData (..)
  , FromMultipart (fromMultipart)
  , Input (..)
  , MultipartData (..)
  , Tmp
  )
import Test.Hspec
import Web.PathPieces (fromPathPiece)

import qualified TDF.Models as M
import TDF.API.Bands (BandsAPI)
import TDF.API.Inventory (AssetUploadForm (..), InventoryAPI, InventoryPublicAPI)
import TDF.API.Pipelines (PipelinesAPI)
import TDF.API.Rooms (RoomsAPI)
import TDF.API.Payments (PaymentCreate (..))
import TDF.API.Types
  ( AssetCheckinRequest (..)
  , AssetCreate (..)
  , BandCreate (..)
  , BandDTO (bName)
  , AssetDTO
      ( assetId
      , currentCheckoutAt
      , currentCheckoutDisposition
      , currentCheckoutDueAt
      , currentCheckoutKind
      , currentCheckoutHolderEmail
      , currentCheckoutHolderPhone
      , currentCheckoutPaymentAmountCents
      , currentCheckoutPaymentCurrency
      , currentCheckoutPaymentInstallments
      , currentCheckoutPaymentOutstandingCents
      , currentCheckoutPhotoUrl
      , currentCheckoutPaymentType
      , currentCheckoutTarget
      , location
      , qrToken
      )
  , AssetCheckoutDTO
      ( AssetCheckoutDTO
      , checkedOutBy
      , conditionIn
      , conditionOut
      , disposition
      , dueAt
      , holderEmail
      , holderPhone
      , notes
      , paymentAmountCents
      , paymentCurrency
      , paymentInstallments
      , paymentOutstandingCents
      , paymentReference
      , paymentType
      , photoInUrl
      , photoOutUrl
      , returnedAt
      , targetKind
      , targetPartyRef
      , termsAndConditions
      )
  , AssetCheckoutRequest (..)
  , AssetQrDTO
  , AssetUpdate (..)
  , PipelineCardCreate (..)
  , PipelineCardDTO (..)
  , PipelineCardUpdate (..)
  , RoomCreate (..)
  , RoomDTO
  , RoomUpdate (..)
  , SessionInputRow (SessionInputRow)
  )
import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.Config (AppConfig (..))
import qualified TDF.Config as Config
import TDF.DB (Env (..))
import qualified TDF.ModelsExtra as ME
import TDF.ModelsExtra
  ( Asset (..)
  , AssetCondition (Good)
  , CheckoutDisposition (Loan, Rental, Sale)
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
    NormalizedCheckoutRequest (..),
    assetMatchesSearchQuery,
    extractMetaInbound,
    normalizeAssetSearchQuery,
    normalizeAssetCategory,
    normalizeAssetCategoryUpdate,
    normalizeAssetCheckinFields,
    normalizeAssetNotesUpdate,
    normalizeAssetName,
    normalizeAssetNameUpdate,
    normalizeBandName,
    validateAssetPhotoUrl,
    validateAssetPhotoUrlUpdate,
    normalizeRoomName,
    normalizeRoomNameUpdate,
    validateSocialLimit,
    metaWebhookVerifyTokenCandidates,
    verifyMetaWebhook,
    validateMetaWebhookChannel,
    validateMetaWebhookVerifyRequest,
    parseSocialBoolParam,
    parseSocialDirectionParam,
    resolveInstagramReplyContext,
    validateSocialReplyExternalId,
    socialReplyOutcomeFields,
    validateSocialReplySenderId,
    validateInventoryPageParams,
    validatePaymentAmountCents,
    validatePaymentPaidAt,
    validatePaymentAttachmentUrl,
    parseCheckoutTargetKind,
    parseOptionalKeyField,
    validatePaymentReferences,
    validatePaymentCurrency,
    validatePaymentConcept,
    validatePaymentReference,
    validatePaymentPeriod,
    validatePositivePaymentReferenceId,
    validateOptionalPositivePaymentReferenceId,
    normalizeServiceCatalogName,
    normalizeServiceCatalogNameUpdate,
    persistMetaInbound,
    validatePaymentMethod,
    parseUTCTimeText,
    parseCheckoutDisposition,
    validateDistinctSessionRooms,
    validateDistinctBandMemberIds,
    validateSessionStatusInput,
    validateSessionTimeRange,
    validateSessionInputRowsWrite,
    validatePublicQrUploadContext,
    normalizeCheckoutRequest,
    validateCheckoutTargets,
    validateCheckoutDueAt,
    validateCheckoutTargetReferences,
    validateServiceCatalogCurrency,
    validateServiceCatalogCurrencyUpdate,
    validateServiceCatalogBillingUnit,
    validateServiceCatalogBillingUnitUpdate,
    validateServiceCatalogTaxBps,
    validateServiceCatalogTaxBpsUpdate,
    validateAssetCheckoutStatus,
    validateAssetStatusUpdate,
    validatePageParams,
    sanitizePublicCheckoutDTO,
    validateSessionReferences,
    bandsServer,
    inventoryPublicServer,
    inventoryServer,
    pipelinesServer,
    roomsServer,
 )

type InventoryTestM = ReaderT Env (ExceptT ServerError IO)

mkAssetUploadMultipart :: [(Text, Text)] -> [FileData Tmp] -> MultipartData Tmp
mkAssetUploadMultipart fields uploads =
  MultipartData
    { inputs = map (uncurry Input) fields
    , files = uploads
    }

mkAssetUploadFile :: Text -> FileData Tmp
mkAssetUploadFile fileName =
  FileData
    { fdInputName = "file"
    , fdFileName = fileName
    , fdFileCType = "image/jpeg"
    , fdPayload = "/tmp/mock-asset-upload"
    }

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
        "{\"coTargetKind\":\"room\",\"coTargetRoom\":\"00000000-0000-0000-0000-000000000042\",\"coDisposition\":\"rental\",\"coTermsAndConditions\":\"Devuelve con estuche y fuente.\",\"coHolderEmail\":\"ops@example.com\",\"coHolderPhone\":\"0999999999\",\"coPaymentType\":\"bank_transfer\",\"coPaymentInstallments\":3,\"coPaymentReference\":\"TRX-009\",\"coPaymentAmount\":\"1200.50\",\"coPaymentCurrency\":\"usd\",\"coPaymentOutstanding\":\"400.25\",\"coConditionOut\":\"Excelente\",\"coPhotoUrl\":\"inventory/foto.jpg\",\"coNotes\":\"Cableado completo\"}" of
        Left err ->
          expectationFailure ("Expected canonical asset checkout payload to decode, got: " <> err)
        Right payload -> do
          coTargetKind payload `shouldBe` Just "room"
          coTargetRoom payload `shouldBe` Just "00000000-0000-0000-0000-000000000042"
          coDisposition payload `shouldBe` Just "rental"
          coTermsAndConditions payload `shouldBe` Just "Devuelve con estuche y fuente."
          coHolderEmail payload `shouldBe` Just "ops@example.com"
          coHolderPhone payload `shouldBe` Just "0999999999"
          coPaymentType payload `shouldBe` Just "bank_transfer"
          coPaymentInstallments payload `shouldBe` Just 3
          coPaymentReference payload `shouldBe` Just "TRX-009"
          coPaymentAmount payload `shouldBe` Just "1200.50"
          coPaymentCurrency payload `shouldBe` Just "usd"
          coPaymentOutstanding payload `shouldBe` Just "400.25"
          coConditionOut payload `shouldBe` Just "Excelente"
          coPhotoUrl payload `shouldBe` Just "inventory/foto.jpg"
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
        "{\"ciConditionIn\":\"Returned OK\",\"ciNotes\":\"Cables verified\",\"ciPhotoUrl\":\"inventory/returned.jpg\"}" of
        Left err ->
          expectationFailure ("Expected canonical asset check-in payload to decode, got: " <> err)
        Right payload -> do
          ciConditionIn payload `shouldBe` Just "Returned OK"
          ciNotes payload `shouldBe` Just "Cables verified"
          ciPhotoUrl payload `shouldBe` Just "inventory/returned.jpg"

    it "rejects response-shaped or unexpected check-in keys so metadata typos fail explicitly" $ do
      (A.eitherDecode
        "{\"conditionIn\":\"Returned OK\",\"notes\":\"Cables verified\"}"
          :: Either String AssetCheckinRequest)
        `shouldSatisfy` isLeft
      (A.eitherDecode
        "{\"ciConditionIn\":\"Returned OK\",\"unexpected\":true}"
          :: Either String AssetCheckinRequest)
        `shouldSatisfy` isLeft

  describe "inventory asset upload multipart parsing" $ do
    it "normalizes optional upload names so blank values keep filename fallbacks" $ do
      case fromMultipart
        (mkAssetUploadMultipart [("name", "  Front Room.jpg  ")] [mkAssetUploadFile "camera.jpg"])
          :: Either String AssetUploadForm of
        Left err ->
          expectationFailure ("Expected asset upload multipart to parse, got: " <> err)
        Right payload -> do
          fdFileName (aufFile payload) `shouldBe` "camera.jpg"
          aufName payload `shouldBe` Just "Front Room.jpg"

      case fromMultipart
        (mkAssetUploadMultipart [("name", "   ")] [mkAssetUploadFile "fallback.jpg"])
          :: Either String AssetUploadForm of
        Left err ->
          expectationFailure ("Expected blank asset upload name to parse, got: " <> err)
        Right payload ->
          aufName payload `shouldBe` Nothing

    it "rejects uploads with no usable form name or browser filename" $
      case fromMultipart
        (mkAssetUploadMultipart [("name", "   ")] [mkAssetUploadFile "   "])
          :: Either String AssetUploadForm of
        Left err ->
          err `shouldContain` "Either field name or uploaded file name must be provided"
        Right payload ->
          expectationFailure
            ( "Expected unnamed asset upload multipart to be rejected, got file: "
                <> T.unpack (fdFileName (aufFile payload))
            )

    it "rejects browser filenames with control characters instead of silently rewriting them into stored asset names" $
      case fromMultipart
        (mkAssetUploadMultipart [] [mkAssetUploadFile "front-room\nshot.jpg"])
          :: Either String AssetUploadForm of
        Left err ->
          err `shouldContain` "Uploaded file name must not contain control characters"
        Right payload ->
          expectationFailure
            ( "Expected control-character browser filename to be rejected, got file: "
                <> T.unpack (fdFileName (aufFile payload))
            )

    it "rejects browser filenames with path separators instead of silently collapsing them into another stored asset name" $ do
      let assertInvalid rawFileName =
            case fromMultipart
              (mkAssetUploadMultipart [] [mkAssetUploadFile rawFileName])
                :: Either String AssetUploadForm of
              Left err ->
                err `shouldContain` "Uploaded file name must not contain path separators"
              Right payload ->
                expectationFailure
                  ( "Expected path-like browser filename to be rejected, got file: "
                      <> T.unpack (fdFileName (aufFile payload))
                  )

      assertInvalid "inventory/front-room.jpg"
      assertInvalid "inventory\\\\front-room.jpg"

    it "rejects explicit upload names that would be silently reshaped into a different stored filename" $ do
      let assertInvalid :: String -> MultipartData Tmp -> Expectation
          assertInvalid expectedMessage multipart =
            case fromMultipart multipart :: Either String AssetUploadForm of
              Left err -> err `shouldContain` expectedMessage
              Right payload ->
                expectationFailure
                  ( "Expected invalid asset upload name to be rejected, got file: "
                      <> T.unpack (fdFileName (aufFile payload))
                  )

      assertInvalid
        "Asset upload name must include a supported image extension"
        (mkAssetUploadMultipart
          [("name", "front-room")]
          [mkAssetUploadFile "camera.jpg"]
        )
      assertInvalid
        "Asset upload name must not contain path separators"
        (mkAssetUploadMultipart
          [("name", "folder/front-room.jpg")]
          [mkAssetUploadFile "camera.jpg"]
        )
      assertInvalid
        "Asset upload name must not contain path separators"
        (mkAssetUploadMultipart
          [("name", "folder\\\\front-room.jpg")]
          [mkAssetUploadFile "camera.jpg"]
        )
      assertInvalid
        "Asset upload name must not contain control characters"
        (mkAssetUploadMultipart
          [("name", "front-room\nshot.jpg")]
          [mkAssetUploadFile "camera.jpg"]
        )

    it "rejects non-image asset uploads before inventory storage can persist them" $ do
      let assertInvalid :: String -> MultipartData Tmp -> Expectation
          assertInvalid expectedMessage multipart =
            case fromMultipart multipart :: Either String AssetUploadForm of
              Left err -> err `shouldContain` expectedMessage
              Right payload ->
                expectationFailure
                  ( "Expected invalid asset upload multipart, got file: "
                      <> T.unpack (fdFileName (aufFile payload))
                  )

      assertInvalid
        "Asset upload must be a raster image"
        (mkAssetUploadMultipart
          []
          [(mkAssetUploadFile "manual.pdf") { fdFileCType = "application/pdf" }]
        )
      assertInvalid
        "Asset upload file name must end with .jpg, .jpeg, .png, .webp, or .gif"
        (mkAssetUploadMultipart [] [mkAssetUploadFile "manual.pdf"])
      assertInvalid
        "Asset upload image extension must match its MIME type"
        (mkAssetUploadMultipart
          [("name", "front-room.png")]
          [mkAssetUploadFile "front-room.jpg"]
        )

    it "rejects conflicting upload name and browser filename extensions" $
      case fromMultipart
        (mkAssetUploadMultipart
          [("name", "front-room.png")]
          [(mkAssetUploadFile "front-room.jpg") { fdFileCType = "image/png" }]
        )
          :: Either String AssetUploadForm of
        Left err ->
          err `shouldContain` "Asset upload image extension must match its MIME type"
        Right payload ->
          expectationFailure
            ( "Expected conflicting asset upload metadata to be rejected, got file: "
                <> T.unpack (fdFileName (aufFile payload))
            )

    it "rejects overlong effective upload names before storage hits filesystem limits" $ do
      let assertInvalid :: MultipartData Tmp -> Expectation
          assertInvalid multipart =
            case fromMultipart multipart :: Either String AssetUploadForm of
              Left err ->
                err `shouldContain` "Asset upload file name must be 218 characters or fewer"
              Right payload ->
                expectationFailure
                  ( "Expected overlong asset upload name to be rejected, got file: "
                      <> T.unpack (fdFileName (aufFile payload))
                  )
          longName = T.replicate 215 "a" <> ".jpg"

      assertInvalid
        (mkAssetUploadMultipart
          [("name", longName)]
          [mkAssetUploadFile "camera.jpg"]
        )
      assertInvalid
        (mkAssetUploadMultipart [] [mkAssetUploadFile longName])

    it "rejects duplicate or unexpected upload parts instead of silently choosing one" $ do
      let assertInvalid :: String -> MultipartData Tmp -> Expectation
          assertInvalid expectedMessage multipart =
            case fromMultipart multipart :: Either String AssetUploadForm of
              Left err -> err `shouldContain` expectedMessage
              Right payload ->
                expectationFailure
                  ( "Expected malformed asset upload multipart, got file: "
                      <> T.unpack (fdFileName (aufFile payload))
                  )

      assertInvalid
        "Duplicate field: name"
        (mkAssetUploadMultipart
          [ ("name", "front-room")
          , ("name", "stage-left")
          ]
          [mkAssetUploadFile "file.jpg"]
        )
      assertInvalid
        "Unexpected field: folder"
        (mkAssetUploadMultipart [("folder", "inventory")] [mkAssetUploadFile "file.jpg"])
      assertInvalid
        "Duplicate file field: file"
        (mkAssetUploadMultipart
          []
          [ mkAssetUploadFile "first.jpg"
          , mkAssetUploadFile "second.jpg"
          ]
        )
      assertInvalid
        "Unexpected file field: photo"
        (mkAssetUploadMultipart
          []
          [(mkAssetUploadFile "file.jpg") { fdInputName = "photo" }]
        )

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

  describe "validatePublicQrUploadContext" $ do
    let checkoutKey =
          case (fromPathPiece "00000000-0000-0000-0000-000000000921" :: Maybe (Key ME.AssetCheckout)) of
            Just key -> key
            Nothing -> error "invalid public QR upload checkout fixture key"
        assetKey =
          case (fromPathPiece "00000000-0000-0000-0000-000000000922" :: Maybe (Key Asset)) of
            Just key -> key
            Nothing -> error "invalid public QR upload asset fixture key"
        mkOpenCheckout target disposition =
          Entity
            checkoutKey
            ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = target
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Backline Crew"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = disposition
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Just "ops@example.com"
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "public-link"
              , ME.assetCheckoutCheckedOutAt = UTCTime (fromGregorian 2026 4 24) 0
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Just "inventory/checkout.jpg"
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Nothing
              }

    it "allows uploads only for assets that can still complete a public checkout or return flow" $ do
      validatePublicQrUploadContext Active Nothing `shouldBe` Right ()
      validatePublicQrUploadContext Active (Just (mkOpenCheckout TargetParty Loan)) `shouldBe` Right ()
      validatePublicQrUploadContext Active (Just (mkOpenCheckout TargetParty Rental)) `shouldBe` Right ()

    it "reuses checkout-status validation when an idle asset is no longer publicly checkoutable" $
      case validatePublicQrUploadContext Retired Nothing of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "Asset is retired and cannot be checked out"
        Right value ->
          expectationFailure ("Expected retired public QR upload context to be rejected, got " <> show value)

    it "rejects uploads for internal-only or terminal active checkout flows" $ do
      let assertInvalid ctx =
            case validatePublicQrUploadContext Active (Just ctx) of
              Left err -> do
                errHTTPCode err `shouldBe` 409
                BL8.unpack (errBody err)
                  `shouldContain` "available for checkout or currently checked out to a party loan or rental"
              Right value ->
                expectationFailure ("Expected invalid public QR upload context to be rejected, got " <> show value)

      assertInvalid (mkOpenCheckout TargetRoom Loan)
      assertInvalid (mkOpenCheckout TargetParty Sale)

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

    it "rejects oversized or control-character asset labels before they can corrupt inventory metadata" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid asset label error, got " <> show value)
      assertInvalid
        "Asset name must be 160 characters or fewer"
        (normalizeAssetName (T.replicate 161 "a"))
      assertInvalid
        "Asset name must not contain control characters"
        (normalizeAssetName "Roland\nJuno-106")
      assertInvalid
        "Asset category must be 120 characters or fewer"
        (normalizeAssetCategory (T.replicate 121 "a"))
      assertInvalid
        "Asset category must not contain control characters"
        (normalizeAssetCategory "Synth\NULLead")

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
              BL8.unpack (errBody err) `shouldContain` "photoUrl must be an absolute https URL or an inventory asset path"
            Right value ->
              expectationFailure ("Expected invalid asset photo URL error, got " <> show value)
      assertInvalid (validateAssetPhotoUrl (Just "roland-juno.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "http://cdn.example.com/roland.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "ftp://cdn.example.com/roland.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "https://cdn/roland.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "https://2130706433/roland.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "assets/serve/roland.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "inventory/../roland.jpg"))
      assertInvalid (validateAssetPhotoUrl (Just "inventory/manual.pdf"))
      assertInvalid (validateAssetPhotoUrl (Just "inventory/folder/no-extension"))

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
              BL8.unpack (errBody err) `shouldContain` "photoUrl must be an absolute https URL or an inventory asset path"
            Right value ->
              expectationFailure ("Expected invalid asset photo update error, got " <> show value)
      assertInvalid (validateAssetPhotoUrlUpdate (Just "roland-juno.jpg"))
      assertInvalid (validateAssetPhotoUrlUpdate (Just "ftp://cdn.example.com/roland.jpg"))
      assertInvalid (validateAssetPhotoUrlUpdate (Just "inventory/manual.pdf"))

  describe "normalizeAssetNotesUpdate" $ do
    it "preserves omitted notes, trims meaningful updates, and maps blanks to an explicit clear" $ do
      normalizeAssetNotesUpdate Nothing `shouldBe` Right Nothing
      normalizeAssetNotesUpdate (Just "  Analog poly synth  ")
        `shouldBe` Right (Just (Just "Analog poly synth"))
      normalizeAssetNotesUpdate (Just "   ") `shouldBe` Right (Just Nothing)

    it "rejects oversized or unsafe-control notes instead of silently persisting opaque asset text" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid asset notes error, got " <> show value)
      assertInvalid
        "Asset notes must be 1000 characters or fewer"
        (normalizeAssetNotesUpdate (Just (T.replicate 1001 "a")))
      assertInvalid
        "Asset notes must not contain control characters other than tabs or line breaks"
        (normalizeAssetNotesUpdate (Just "Service notes\NULhere"))

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

  describe "normalizeBandName" $ do
    it "trims and collapses meaningful CRM band names before they are stored" $ do
      normalizeBandName "  TDF   House \t Band  " `shouldBe` Right "TDF House Band"

    it "rejects explicit blank band names instead of storing whitespace-only CRM records" $
      case normalizeBandName "   " of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Band name is required"
        Right value ->
          expectationFailure ("Expected invalid band name error, got " <> show value)

  describe "bandsServer createBandH name handling" $ do
    let existingBandId = "00000000-0000-0000-0000-000000000711"

    it "normalizes band names before storing and returning the new CRM record" $ do
      result <- runBandCreateHandler
        (pure ())
        (BandCreate "  TDF   House \t Band  " Nothing Nothing Nothing Nothing Nothing [])
      case result of
        Left err ->
          expectationFailure ("Expected normalized band create to succeed, got " <> show err)
        Right band ->
          bName band `shouldBe` "TDF House Band"

    it "rejects band creates that only differ by case or repeated whitespace" $ do
      existingKey <- case (fromPathPiece existingBandId :: Maybe (Key Band)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing band fixture key" >> fail "unreachable"
      result <- runBandCreateHandler
        (insertKey existingKey (fixtureBand (toSqlKey 1) "TDF House Band"))
        (BandCreate "  tdf   house   band  " Nothing Nothing Nothing Nothing Nothing [])
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "A band with this name already exists"
        Right value ->
          expectationFailure ("Expected canonical duplicate band create to fail, got " <> show value)

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
    it "requires explicit target kinds and normalizes supported values" $ do
      parseCheckoutTargetKind (Just " room ") `shouldBe` Right TargetRoom
      parseCheckoutTargetKind (Just "SESSION") `shouldBe` Right TargetSession

    it "rejects missing, blank, or unknown target kinds instead of silently treating them as party checkouts" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid checkout target kind error, got " <> show value)
      assertInvalid "targetKind is required" (parseCheckoutTargetKind Nothing)
      assertInvalid "targetKind must be one of: party, room, session" (parseCheckoutTargetKind (Just "   "))
      assertInvalid "targetKind must be one of: party, room, session" (parseCheckoutTargetKind (Just "locker"))

  describe "parseCheckoutDisposition" $ do
    it "requires explicit dispositions and normalizes supported values" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid checkout disposition error, got " <> show value)
      assertInvalid "disposition is required" (parseCheckoutDisposition Nothing)
      parseCheckoutDisposition (Just " rent ") `shouldBe` Right Rental
      parseCheckoutDisposition (Just "SALE") `shouldBe` Right Sale

    it "rejects blank or unknown disposition values" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid checkout disposition error, got " <> show value)
      assertInvalid "disposition must be one of" (parseCheckoutDisposition (Just "   "))
      assertInvalid "disposition must be one of" (parseCheckoutDisposition (Just "borrowed-forever"))

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
      assertInvalid
        "targetParty must not contain control characters"
        (validateCheckoutTargets TargetParty (Just "Crew\nA") Nothing Nothing)
      assertInvalid
        "targetParty must be 160 characters or fewer"
        (validateCheckoutTargets TargetParty (Just (T.replicate 161 "a")) Nothing Nothing)
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

  describe "validateCheckoutDueAt" $ do
    let now = UTCTime (fromGregorian 2026 4 23) 43200

    it "accepts omitted or non-past due timestamps for new inventory checkouts" $ do
      let future = addUTCTime 3600 now
      validateCheckoutDueAt now Nothing `shouldBe` Right Nothing
      validateCheckoutDueAt now (Just now) `shouldBe` Right (Just now)
      validateCheckoutDueAt now (Just future) `shouldBe` Right (Just future)

    it "rejects already-expired due timestamps instead of creating overdue checkouts at insert time" $ do
      let past = addUTCTime (-60) now
      case validateCheckoutDueAt now (Just past) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "dueAt must not be in the past"
        Right value ->
          expectationFailure ("Expected past checkout dueAt to be rejected, got " <> show value)

  describe "normalizeCheckoutRequest" $ do
    let roomIdText = "00000000-0000-0000-0000-000000000042"
        roomId = case (fromPathPiece roomIdText :: Maybe (Key Room)) of
          Just key -> key
          Nothing -> error "invalid checkout room fixture key"

    it "normalizes actionable holder email and phone fields before writing inventory custody records" $ do
      case normalizeCheckoutRequest
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "loan")
          Nothing
          (Just " Ops@Example.com ")
          (Just " +593 99 123 4567 ")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing) of
        Left err ->
          expectationFailure ("Expected holder contact normalization to succeed, got " <> show err)
        Right normalized -> do
          ncrHolderEmail normalized `shouldBe` Just "ops@example.com"
          ncrHolderPhone normalized `shouldBe` Just "+593991234567"

    it "trims meaningful condition and notes while preserving safe line breaks for inventory checkout writes" $ do
      case normalizeCheckoutRequest
        (AssetCheckoutRequest
          (Just " room ")
          Nothing
          Nothing
          (Just roomIdText)
          (Just " rental ")
          (Just "  Devuelve con estuche y fuente.  ")
          Nothing
          Nothing
          (Just " transferencia ")
          (Just 3)
          (Just "  TRX-009  ")
          (Just " 1200.50 ")
          (Just " usd ")
          (Just " 400.25 ")
          Nothing
          Nothing
          (Just "  Returned with stand  ")
          (Just "  Cableado completo\n\tListo para sala  ")) of
        Left err ->
          expectationFailure ("Expected checkout request normalization to succeed, got " <> show err)
        Right normalized -> do
          ncrTargetKind normalized `shouldBe` TargetRoom
          ncrTargetRoom normalized `shouldBe` Just roomId
          ncrDisposition normalized `shouldBe` Rental
          ncrTermsAndConditions normalized `shouldBe` Just "Devuelve con estuche y fuente."
          ncrPaymentType normalized `shouldBe` Just "bank_transfer"
          ncrPaymentInstallments normalized `shouldBe` Just 3
          ncrPaymentReference normalized `shouldBe` Just "TRX-009"
          ncrPaymentAmountCents normalized `shouldBe` Just 120050
          ncrPaymentCurrency normalized `shouldBe` Just "USD"
          ncrPaymentOutstandingCents normalized `shouldBe` Just 40025
          ncrConditionOut normalized `shouldBe` Just "Returned with stand"
          ncrNotes normalized `shouldBe` Just "Cableado completo\n\tListo para sala"

    it "rejects oversized or unsafe-control checkout condition and notes instead of storing ambiguous movement text" $ do
      let assertInvalid expectedMessage req =
            case normalizeCheckoutRequest req of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected invalid checkout request error, got a normalized payload"

          baseRequest =
            AssetCheckoutRequest
              (Just "party")
              Nothing
              (Just "Backline Crew")
              Nothing
              (Just "loan")
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing

      assertInvalid
        "conditionOut must be 240 characters or fewer"
        baseRequest { coConditionOut = Just (T.replicate 241 "a") }
      assertInvalid
        "conditionOut must not contain control characters other than tabs or line breaks"
        baseRequest { coConditionOut = Just "Returned\NULOK" }
      assertInvalid
        "notes must be 1000 characters or fewer"
        baseRequest { coNotes = Just (T.replicate 1001 "a") }
      assertInvalid
        "notes must not contain control characters other than tabs or line breaks"
        baseRequest { coNotes = Just "Cableado\NULcompleto" }
      assertInvalid
        "holderEmail must be a valid email address"
        baseRequest { coHolderEmail = Just "ops at example.com" }
      assertInvalid
        "holderPhone must be a valid phone number"
        baseRequest { coHolderPhone = Just "call me maybe" }

    it "rejects payment references without payment types so checkout financial metadata stays unambiguous" $
      case normalizeCheckoutRequest
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "sale")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just "  TRX-009  ")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "paymentReference requires paymentType"
        Right value ->
          value `seq` expectationFailure "Expected orphan payment reference to be rejected"

    it "rejects payment metadata for non-commercial dispositions so loans and repairs cannot carry sale-only fields" $
      case normalizeCheckoutRequest
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "loan")
          Nothing
          Nothing
          Nothing
          (Just "cash")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "payment fields are only allowed for sale or rental checkout"
        Right value ->
          value `seq` expectationFailure "Expected loan checkout payment metadata to be rejected"

    it "rejects due dates on sale checkouts so sold assets cannot carry return expectations" $
      case normalizeCheckoutRequest
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "sale")
          Nothing
          (Just "ops@example.com")
          Nothing
          (Just "card")
          Nothing
          Nothing
          (Just "1200")
          (Just "USD")
          Nothing
          Nothing
          (Just (UTCTime (fromGregorian 2026 5 1) 0))
          Nothing
          Nothing) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "dueAt is not allowed for sale checkout"
        Right value ->
          value `seq` expectationFailure "Expected sale checkout dueAt to be rejected"

    it "rejects incomplete or contradictory checkout money fields before storing unusable balances" $ do
      let assertInvalid expectedMessage req =
            case normalizeCheckoutRequest req of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected invalid checkout financial metadata to fail, got a normalized payload"

          baseRequest =
            AssetCheckoutRequest
              (Just "party")
              Nothing
              (Just "Backline Crew")
              Nothing
              (Just "sale")
              Nothing
              (Just "ops@example.com")
              Nothing
              (Just "card")
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing

      assertInvalid
        "paymentInstallments requires paymentAmount"
        baseRequest { coPaymentInstallments = Just 3 }
      assertInvalid
        "paymentAmount requires paymentCurrency"
        baseRequest { coPaymentAmount = Just "1200" }
      assertInvalid
        "paymentCurrency requires paymentAmount"
        baseRequest { coPaymentCurrency = Just "USD" }
      assertInvalid
        "paymentOutstanding requires paymentAmount"
        baseRequest { coPaymentOutstanding = Just "10.00" }
      assertInvalid
        "paymentOutstanding must be less than or equal to paymentAmount"
        baseRequest
          { coPaymentAmount = Just "100.00"
          , coPaymentCurrency = Just "USD"
          , coPaymentOutstanding = Just "150.00"
          }

  describe "normalizeAssetCheckinFields" $ do
    it "trims meaningful condition and notes before persisting a check-in" $ do
      normalizeAssetCheckinFields (AssetCheckinRequest (Just "  Returned OK  ") (Just "  Cables verified  ") (Just "inventory/checkin.jpg"))
        `shouldBe` Right (Just "Returned OK", Just "Cables verified", Just "inventory/checkin.jpg")

    it "drops omitted or blank check-in text so existing checkout context is not silently erased" $ do
      normalizeAssetCheckinFields (AssetCheckinRequest Nothing Nothing Nothing)
        `shouldBe` Right (Nothing, Nothing, Nothing)
      normalizeAssetCheckinFields (AssetCheckinRequest (Just "   ") (Just "   ") (Just "   "))
        `shouldBe` Right (Nothing, Nothing, Nothing)

    it "rejects oversized or unsafe-control check-in condition and notes before closing inventory movements" $ do
      let assertInvalid expectedMessage req =
            case normalizeAssetCheckinFields req of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected invalid check-in normalization error, got " <> show value)

      assertInvalid
        "conditionIn must be 240 characters or fewer"
        (AssetCheckinRequest (Just (T.replicate 241 "a")) Nothing Nothing)
      assertInvalid
        "conditionIn must not contain control characters other than tabs or line breaks"
        (AssetCheckinRequest (Just "Returned\NULOK") Nothing Nothing)
      assertInvalid
        "notes must be 1000 characters or fewer"
        (AssetCheckinRequest Nothing (Just (T.replicate 1001 "a")) Nothing)
      assertInvalid
        "notes must not contain control characters other than tabs or line breaks"
        (AssetCheckinRequest Nothing (Just "Cableado\NULverificado") Nothing)

  describe "checkoutAssetH" $ do
    let existingAssetId = "00000000-0000-0000-0000-000000000900"
        roomIdText = "00000000-0000-0000-0000-000000000042"

    it "requires an explicit disposition so authenticated checkout writes cannot silently default to loan" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing asset fixture key" >> fail "unreachable"
      result <- runInventoryCheckoutHandler
        (insertKey assetKey (fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing))
        existingAssetId
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "disposition is required"
        Right value ->
          expectationFailure ("Expected missing inventory checkout disposition to be rejected, got " <> show value)

    it "rejects sale checkouts aimed at rooms so ownership transfers cannot be stored with internal-only targets" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing asset fixture key" >> fail "unreachable"
      result <- runInventoryCheckoutHandler
        (insertKey assetKey (fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing))
        existingAssetId
        (AssetCheckoutRequest
          (Just "room")
          Nothing
          Nothing
          (Just roomIdText)
          (Just "sale")
          Nothing
          (Just "ops@example.com")
          Nothing
          (Just "card")
          Nothing
          Nothing
          (Just "1200")
          (Just "USD")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "sale checkout only supports party targets"
        Right value ->
          expectationFailure ("Expected room-targeted sale checkout to be rejected, got " <> show value)

  describe "checkinAssetH" $ do
    let missingAssetId = "00000000-0000-0000-0000-000000000901"
        existingAssetId = "00000000-0000-0000-0000-000000000902"
        checkoutIdText = "00000000-0000-0000-0000-000000000903"
        request = AssetCheckinRequest Nothing Nothing Nothing

    it "rejects unknown asset ids before collapsing them into missing checkout errors" $ do
      result <- runInventoryCheckinHandler (pure ()) missingAssetId request
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 404
          BL8.unpack (errBody err) `shouldContain` "Asset not found"
        Right value ->
          expectationFailure ("Expected missing asset check-in to fail, got " <> show value)

    it "returns a conflict for real assets that are not currently checked out" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing asset fixture key" >> fail "unreachable"
      result <- runInventoryCheckinHandler
        (insertKey assetKey (fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing))
        existingAssetId
        request
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "Asset is not currently checked out"
        Right value ->
          expectationFailure ("Expected idle asset check-in to fail, got " <> show value)

    it "rejects sale check-ins so retired ownership records cannot be silently closed" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing checkout fixture key" >> fail "unreachable"
      result <- runInventoryCheckinHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetStatus = Retired
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Buyer"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Sale
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Just "buyer@example.com"
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Just "card"
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Just 120000
              , ME.assetCheckoutPaymentCurrency = Just "USD"
              , ME.assetCheckoutPaymentOutstandingCents = Just 0
              , ME.assetCheckoutCheckedOutByRef = "1"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Sold"
              , ME.assetCheckoutPhotoOutUrl = Just "inventory/sale-checkout.jpg"
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Just "Factura entregada"
              })
        existingAssetId
        (AssetCheckinRequest (Just "Returned by buyer") Nothing (Just "inventory/return.jpg"))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "Sale checkouts cannot be checked in"
        Right value ->
          expectationFailure ("Expected sale asset check-in to be rejected, got " <> show value)

    it "preserves checkout notes by appending labeled check-in notes instead of overwriting custody context" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing checkout fixture key" >> fail "unreachable"
      result <- runInventoryCheckinHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetStatus = Booked
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Backline Crew"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Loan
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Nothing
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "1"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Nothing
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Just "Bring hard case"
              })
        existingAssetId
        (AssetCheckinRequest (Just "Returned OK") (Just "Extra cable included") Nothing)
      case result of
        Left err ->
          expectationFailure ("Expected check-in note merge to succeed, got " <> show err)
        Right value -> do
          conditionIn value `shouldBe` Just "Returned OK"
          notes value `shouldBe` Just "Bring hard case\n\nCheck-in: Extra cable included"

    it "rejects check-in notes that would overflow the shared movement notes field after preserving checkout context" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing checkout fixture key" >> fail "unreachable"
      result <- runInventoryCheckinHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetStatus = Booked
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Backline Crew"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Loan
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Nothing
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "1"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Nothing
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Just (T.replicate 990 "a")
              })
        existingAssetId
        (AssetCheckinRequest Nothing (Just "b") Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "notes must be 1000 characters or fewer"
        Right value ->
          expectationFailure ("Expected oversized merged check-in notes to be rejected, got " <> show value)

  describe "checkoutHistoryH" $ do
    let missingAssetId = "00000000-0000-0000-0000-000000000907"

    it "rejects unknown asset ids instead of collapsing them into empty history responses" $ do
      result <- runInventoryCheckoutHistoryHandler (pure ()) missingAssetId
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 404
          BL8.unpack (errBody err) `shouldContain` "Asset not found"
        Right value ->
          expectationFailure ("Expected missing asset history lookup to fail, got " <> show value)

  describe "patchAssetH" $ do
    let existingAssetId = "00000000-0000-0000-0000-000000000908"
        request =
          AssetUpdate
            Nothing
            Nothing
            Nothing
            Nothing
            (Just "Service notes\NULhere")
            Nothing

    it "rejects invalid notes on asset patch instead of persisting unreadable inventory metadata" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid patch asset fixture key" >> fail "unreachable"
      result <- runInventoryPatchHandler
        (insertKey assetKey (fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing))
        existingAssetId
        request
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err)
            `shouldContain` "Asset notes must not contain control characters other than tabs or line breaks"
        Right value ->
          expectationFailure ("Expected invalid asset patch notes to fail, got " <> show value)

    it "rejects patch requests that try to mark an available asset as booked without creating a checkout" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid booked patch asset fixture key" >> fail "unreachable"
      result <- runInventoryPatchHandler
        (insertKey assetKey (fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing))
        existingAssetId
        (AssetUpdate Nothing Nothing (Just "booked") Nothing Nothing Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err)
            `shouldContain` "Asset status can only become booked through the checkout endpoint"
        Right value ->
          expectationFailure ("Expected impossible booked status patch to fail, got " <> show value)

    it "rejects patch status changes that would contradict an active checkout" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid active checkout patch asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece "00000000-0000-0000-0000-000000000916" :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid active checkout patch fixture key" >> fail "unreachable"
      result <- runInventoryPatchHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetStatus = Booked
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Backline Crew"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Loan
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Just "ops@example.com"
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "1"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Nothing
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Nothing
              })
        existingAssetId
        (AssetUpdate Nothing Nothing (Just "active") Nothing Nothing Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err)
            `shouldContain` "Asset status cannot change while an active checkout exists"
        Right value ->
          expectationFailure ("Expected contradictory asset status patch to fail, got " <> show value)

    it "still allows booked status no-ops on legacy rows so unrelated asset edits stay unblocked" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid legacy booked asset fixture key" >> fail "unreachable"
      result <- runInventoryPatchHandler
        (insertKey assetKey
          ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
            { assetStatus = Booked
            }))
        existingAssetId
        (AssetUpdate Nothing Nothing (Just "booked") Nothing (Just "Needs relabeling") Nothing)
      case result of
        Left err ->
          expectationFailure ("Expected legacy booked asset patch to succeed, got " <> show err)
        Right asset ->
          assetId asset `shouldBe` existingAssetId

  describe "deleteAssetH" $ do
    let deletableAssetId = "00000000-0000-0000-0000-000000000903"
        historicAssetId = "00000000-0000-0000-0000-000000000904"

    it "deletes assets that have no checkout history" $ do
      assetKey <- case (fromPathPiece deletableAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid deletable asset fixture key" >> fail "unreachable"
      (result, assetStillExists) <- runInventoryDeleteHandler
        (insertKey assetKey (fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing))
        deletableAssetId
      case result of
        Left err ->
          expectationFailure ("Expected asset delete to succeed, got " <> show err)
        Right () ->
          assetStillExists `shouldBe` False

    it "rejects assets with checkout history instead of deleting inventory movement context" $ do
      assetKey <- case (fromPathPiece historicAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid historic asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece "00000000-0000-0000-0000-000000000914" :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid historic checkout fixture key" >> fail "unreachable"
      (result, assetStillExists) <- runInventoryDeleteHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey (fixtureAsset "Ludwig Supraphonic" "Drum" (Just "Ludwig") Nothing "TDF" Nothing)
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Backline Crew"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Loan
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Just "ops@example.com"
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "1"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Nothing
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Just now
              , ME.assetCheckoutConditionIn = Just "Returned OK"
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Just "Returned without issues"
              })
        historicAssetId
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "Asset has checkout history and cannot be deleted"
          assetStillExists `shouldBe` True
        Right () ->
          expectationFailure "Expected asset delete with checkout history to be rejected"

  describe "refreshQrH" $ do
    let missingAssetId = "00000000-0000-0000-0000-000000000905"

    it "rejects unknown asset ids instead of minting orphan QR tokens" $ do
      result <- runInventoryRefreshQrHandler (pure ()) missingAssetId
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 404
          BL8.unpack (errBody err) `shouldContain` "Asset not found"
        Right value ->
          expectationFailure ("Expected missing asset QR refresh to fail, got " <> show value)

  describe "resolveByQrH" $ do
    let existingAssetId = "00000000-0000-0000-0000-000000000906"
        canonicalToken = "00000000-0000-0000-0000-00000000abcd"

    it "rejects malformed QR tokens before inventory lookup turns them into ambiguous 404s" $ do
      result <- runInventoryResolveQrHandler (pure ()) "not-a-uuid"
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Invalid asset QR token"
        Right value ->
          expectationFailure ("Expected malformed QR token lookup to fail, got " <> show value)

    it "normalizes UUID casing so copied QR links still resolve the intended asset" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid existing asset fixture key" >> fail "unreachable"
      result <- runInventoryResolveQrHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        "00000000-0000-0000-0000-00000000ABCD"
      case result of
        Left err ->
          expectationFailure ("Expected uppercase QR token lookup to resolve, got " <> show err)
        Right asset -> do
          assetId asset `shouldBe` existingAssetId
          qrToken asset `shouldBe` Just canonicalToken

  describe "inventoryPublicServer loadByQrToken" $ do
    let existingAssetId = "00000000-0000-0000-0000-000000000921"
        canonicalToken = "00000000-0000-0000-0000-00000000dcbd"
        checkoutIdText = "00000000-0000-0000-0000-000000000922"
        roomIdText = "00000000-0000-0000-0000-000000000923"

    it "redacts sensitive fields on public QR loads while keeping party checkout context readable" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public resolve asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public resolve checkout fixture key" >> fail "unreachable"
      roomKey <- case (fromPathPiece roomIdText :: Maybe (Key Room)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public resolve room fixture key" >> fail "unreachable"
      result <- runInventoryPublicResolveQrHandler
        (do
            let now = UTCTime (fromGregorian 2035 5 1) 0
                dueAt = addUTCTime (60 * 60 * 24) now
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetQrCode = Just canonicalToken
                , assetStatus = Booked
                , assetLocationId = Just roomKey
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Backline Crew"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Rental
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Just "ops@example.com"
              , ME.assetCheckoutHolderPhone = Just "+593991234567"
              , ME.assetCheckoutPaymentType = Just "bank_transfer"
              , ME.assetCheckoutPaymentInstallments = Just 3
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Just 120000
              , ME.assetCheckoutPaymentCurrency = Just "USD"
              , ME.assetCheckoutPaymentOutstandingCents = Just 40000
              , ME.assetCheckoutCheckedOutByRef = "1"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Just dueAt
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Just "inventory/public-checkout-proof.jpg"
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Nothing
              })
        canonicalToken
      case result of
        Left err ->
          expectationFailure ("Expected public QR resolve to succeed, got " <> show err)
        Right asset -> do
          assetId asset `shouldBe` existingAssetId
          location asset `shouldBe` Nothing
          qrToken asset `shouldBe` Nothing
          currentCheckoutKind asset `shouldBe` Just "party"
          currentCheckoutTarget asset `shouldBe` Just "Backline Crew"
          currentCheckoutDisposition asset `shouldBe` Just "rental"
          currentCheckoutAt asset `shouldBe` Just (UTCTime (fromGregorian 2035 5 1) 0)
          currentCheckoutDueAt asset `shouldBe` Just (UTCTime (fromGregorian 2035 5 2) 0)
          currentCheckoutHolderEmail asset `shouldBe` Nothing
          currentCheckoutHolderPhone asset `shouldBe` Nothing
          currentCheckoutPaymentType asset `shouldBe` Nothing
          currentCheckoutPaymentInstallments asset `shouldBe` Nothing
          currentCheckoutPaymentAmountCents asset `shouldBe` Nothing
          currentCheckoutPaymentCurrency asset `shouldBe` Nothing
          currentCheckoutPaymentOutstandingCents asset `shouldBe` Nothing
          currentCheckoutPhotoUrl asset `shouldBe` Nothing

    it "hides internal room or session checkout metadata on public QR loads instead of exposing partial internal movement details" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public resolve asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public resolve checkout fixture key" >> fail "unreachable"
      roomKey <- case (fromPathPiece roomIdText :: Maybe (Key Room)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public resolve room fixture key" >> fail "unreachable"
      result <- runInventoryPublicResolveQrHandler
        (do
            let now = UTCTime (fromGregorian 2035 5 3) 0
                dueAt = addUTCTime (60 * 60 * 24) now
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetQrCode = Just canonicalToken
                , assetStatus = Booked
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetRoom
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Nothing
              , ME.assetCheckoutTargetRoomId = Just roomKey
              , ME.assetCheckoutDisposition = Loan
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Nothing
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "1"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Just dueAt
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Nothing
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Nothing
              })
        canonicalToken
      case result of
        Left err ->
          expectationFailure ("Expected public QR resolve to succeed, got " <> show err)
        Right asset -> do
          assetId asset `shouldBe` existingAssetId
          currentCheckoutKind asset `shouldBe` Nothing
          currentCheckoutTarget asset `shouldBe` Nothing
          currentCheckoutDisposition asset `shouldBe` Nothing
          currentCheckoutAt asset `shouldBe` Nothing
          currentCheckoutDueAt asset `shouldBe` Nothing

    it "hides buyer identity on public QR loads for sold assets while still surfacing that the asset was sold" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public resolve asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public resolve checkout fixture key" >> fail "unreachable"
      result <- runInventoryPublicResolveQrHandler
        (do
            let now = UTCTime (fromGregorian 2035 5 4) 0
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetQrCode = Just canonicalToken
                , assetStatus = Retired
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Cliente final"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Sale
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Just "buyer@example.com"
              , ME.assetCheckoutHolderPhone = Just "+593991234567"
              , ME.assetCheckoutPaymentType = Just "bank_transfer"
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Just "SALE-001"
              , ME.assetCheckoutPaymentAmountCents = Just 250000
              , ME.assetCheckoutPaymentCurrency = Just "USD"
              , ME.assetCheckoutPaymentOutstandingCents = Just 0
              , ME.assetCheckoutCheckedOutByRef = "1"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Sold as-is"
              , ME.assetCheckoutPhotoOutUrl = Just "inventory/public-sale-proof.jpg"
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Nothing
              })
        canonicalToken
      case result of
        Left err ->
          expectationFailure ("Expected sold public QR resolve to succeed, got " <> show err)
        Right asset -> do
          assetId asset `shouldBe` existingAssetId
          currentCheckoutKind asset `shouldBe` Nothing
          currentCheckoutTarget asset `shouldBe` Nothing
          currentCheckoutDisposition asset `shouldBe` Just "sale"
          currentCheckoutAt asset `shouldBe` Nothing
          currentCheckoutDueAt asset `shouldBe` Nothing
          currentCheckoutHolderEmail asset `shouldBe` Nothing
          currentCheckoutHolderPhone asset `shouldBe` Nothing
          currentCheckoutPaymentType asset `shouldBe` Nothing
          currentCheckoutPaymentInstallments asset `shouldBe` Nothing
          currentCheckoutPaymentAmountCents asset `shouldBe` Nothing
          currentCheckoutPaymentCurrency asset `shouldBe` Nothing
          currentCheckoutPaymentOutstandingCents asset `shouldBe` Nothing
          currentCheckoutPhotoUrl asset `shouldBe` Nothing

  describe "sanitizePublicCheckoutDTO" $ do
    it "redacts private checkout metadata before public QR flows return a movement payload" $ do
      let dueAtValue = UTCTime (fromGregorian 2035 5 1) 0
          checkedOutAtValue = UTCTime (fromGregorian 2035 4 30) 0
          returnedAtValue = UTCTime (fromGregorian 2035 5 2) 0
          sanitized =
            sanitizePublicCheckoutDTO
              (AssetCheckoutDTO
                "00000000-0000-0000-0000-000000000915"
                "00000000-0000-0000-0000-000000000907"
                "party"
                Nothing
                (Just "Backline Crew")
                Nothing
                "rental"
                (Just "Devuelve con estuche y fuente.")
                (Just "ops@example.com")
                (Just "+593991234567")
                (Just "card")
                (Just 3)
                (Just "TRX-009")
                (Just 120050)
                (Just "USD")
                (Just 40025)
                "42"
                checkedOutAtValue
                (Just dueAtValue)
                (Just "Equipo completo")
                (Just "inventory/checkout.jpg")
                (Just "Returned OK")
                (Just "inventory/checkin.jpg")
                (Just returnedAtValue)
                (Just "Solo visible para ops"))
      targetKind sanitized `shouldBe` "party"
      targetPartyRef sanitized `shouldBe` Just "Backline Crew"
      disposition sanitized `shouldBe` "rental"
      dueAt sanitized `shouldBe` Just dueAtValue
      returnedAt sanitized `shouldBe` Just returnedAtValue
      termsAndConditions sanitized `shouldBe` Nothing
      holderEmail sanitized `shouldBe` Nothing
      holderPhone sanitized `shouldBe` Nothing
      paymentType sanitized `shouldBe` Nothing
      paymentInstallments sanitized `shouldBe` Nothing
      paymentReference sanitized `shouldBe` Nothing
      paymentAmountCents sanitized `shouldBe` Nothing
      paymentCurrency sanitized `shouldBe` Nothing
      paymentOutstandingCents sanitized `shouldBe` Nothing
      checkedOutBy sanitized `shouldBe` "redacted"
      conditionOut sanitized `shouldBe` Nothing
      photoOutUrl sanitized `shouldBe` Nothing
      conditionIn sanitized `shouldBe` Nothing
      photoInUrl sanitized `shouldBe` Nothing
      notes sanitized `shouldBe` Nothing

  describe "inventoryPublicServer checkoutByQrToken" $ do
    let existingAssetId = "00000000-0000-0000-0000-000000000907"
        canonicalToken = "00000000-0000-0000-0000-00000000dcba"
        request =
          AssetCheckoutRequest
            (Just "room")
            Nothing
            Nothing
            (Just "00000000-0000-0000-0000-000000000042")
            (Just "loan")
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing

    it "requires an explicit disposition on public QR links so missing fields cannot silently become loan checkouts" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          Nothing
          Nothing
          (Just "ops@example.com")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just "inventory/checkout.jpg")
          Nothing
          Nothing
          Nothing
          )
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "disposition is required"
        Right value ->
          expectationFailure ("Expected public QR checkout without explicit disposition to be rejected, got " <> show value)

    it "rejects room or session targets on public QR links before external callers can attach internal references" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        request
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Public QR checkout only supports party targets"
        Right value ->
          expectationFailure ("Expected public QR room checkout to be rejected, got " <> show value)

    it "rejects sale disposition on public QR links so anonymous scans cannot retire inventory" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "sale")
          Nothing
          (Just "ops@example.com")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just "inventory/checkout.jpg")
          Nothing
          Nothing
          Nothing
          )
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Public QR checkout does not allow sale disposition"
        Right value ->
          expectationFailure ("Expected public QR sale checkout to be rejected, got " <> show value)

    it "rejects internal-only repair-style dispositions on public QR links so anonymous scans cannot create ambiguous inventory flows" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "repair")
          Nothing
          (Just "ops@example.com")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just "inventory/checkout.jpg")
          Nothing
          Nothing
          Nothing
          )
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Public QR checkout only supports loan or rental disposition"
        Right value ->
          expectationFailure ("Expected public QR repair checkout to be rejected, got " <> show value)

    it "requires holder contact on public QR links so anonymous custody records stay actionable" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "loan")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just "inventory/checkout.jpg")
          Nothing
          )
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Public QR checkout requires holderEmail or holderPhone"
        Right value ->
          expectationFailure ("Expected public QR checkout without contact details to be rejected, got " <> show value)

    it "requires rental terms on public QR links so anonymous paid custody records do not omit the agreed conditions" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "rental")
          Nothing
          (Just "ops@example.com")
          Nothing
          (Just "card")
          Nothing
          Nothing
          (Just "1200.00")
          (Just "USD")
          Nothing
          (Just "inventory/checkout.jpg")
          (Just (UTCTime (fromGregorian 2035 5 1) 0))
          (Just "Equipo completo")
          Nothing
          )
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Public QR rental checkout requires coTermsAndConditions"
        Right value ->
          expectationFailure ("Expected public QR rental checkout without terms to be rejected, got " <> show value)

    it "rejects malformed holder contact on public QR links before creating unusable custody rows" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "loan")
          Nothing
          (Just "ops at example.com")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just "inventory/checkout.jpg")
          Nothing
          Nothing
          Nothing
          )
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "holderEmail must be a valid email address"
        Right value ->
          expectationFailure ("Expected malformed public QR checkout contact to be rejected, got " <> show value)

    it "requires an agreed return date on public QR links so anonymous custody records do not stay open-ended" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "loan")
          Nothing
          (Just "ops@example.com")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just "inventory/checkout.jpg")
          Nothing
          (Just "Equipo completo")
          Nothing
          )
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Public QR checkout requires coDueAt"
        Right value ->
          expectationFailure ("Expected public QR checkout without dueAt to be rejected, got " <> show value)

    it "requires a checkout photo on public QR links so anonymous custody records keep visual proof" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "loan")
          Nothing
          (Just "ops@example.com")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just "Sale completo")
          Nothing
          )
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Public QR checkout requires coPhotoUrl"
        Right value ->
          expectationFailure ("Expected public QR checkout without photo to be rejected, got " <> show value)

    it "requires an explicit checkout condition on public QR links so custody handoff records are not left ambiguous" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "loan")
          Nothing
          (Just "ops@example.com")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just "inventory/checkout.jpg")
          (Just (UTCTime (fromGregorian 2035 5 1) 0))
          Nothing
          Nothing
          )
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Public QR checkout requires coConditionOut"
        Right value ->
          expectationFailure ("Expected public QR checkout without condition text to be rejected, got " <> show value)

    it "rejects externally hosted checkout proof on public QR links so anonymous custody evidence stays under managed storage" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public checkout asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckoutHandler
        (insertKey assetKey ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing) { assetQrCode = Just canonicalToken }))
        canonicalToken
        (AssetCheckoutRequest
          (Just "party")
          Nothing
          (Just "Backline Crew")
          Nothing
          (Just "loan")
          Nothing
          (Just "ops@example.com")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Just "https://cdn.example.com/checkouts/juno-106.jpg")
          (Just (UTCTime (fromGregorian 2035 5 1) 0))
          (Just "Sale completo")
          Nothing
          )
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "uploaded inventory asset path"
        Right value ->
          expectationFailure ("Expected public QR checkout with external photo proof to be rejected, got " <> show value)

  describe "inventoryPublicServer checkinByQrToken" $ do
    let existingAssetId = "00000000-0000-0000-0000-000000000910"
        canonicalToken = "00000000-0000-0000-0000-00000000dcbb"
        checkoutIdText = "00000000-0000-0000-0000-000000000915"
        roomIdText = "00000000-0000-0000-0000-000000000042"
        dueAtValue = UTCTime (fromGregorian 2035 6 1) 0
        request = AssetCheckinRequest (Just "Returned OK") Nothing (Just "inventory/checkin.jpg")

    it "redacts private checkout metadata from successful public QR check-in responses" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in checkout fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckinHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetQrCode = Just canonicalToken
                , assetStatus = Booked
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Backline Crew"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Rental
              , ME.assetCheckoutTermsAndConditions = Just "Devuelve con estuche y fuente."
              , ME.assetCheckoutHolderEmail = Just "ops@example.com"
              , ME.assetCheckoutHolderPhone = Just "+593991234567"
              , ME.assetCheckoutPaymentType = Just "card"
              , ME.assetCheckoutPaymentInstallments = Just 3
              , ME.assetCheckoutPaymentReference = Just "TRX-009"
              , ME.assetCheckoutPaymentAmountCents = Just 120050
              , ME.assetCheckoutPaymentCurrency = Just "USD"
              , ME.assetCheckoutPaymentOutstandingCents = Just 40025
              , ME.assetCheckoutCheckedOutByRef = "42"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Just dueAtValue
              , ME.assetCheckoutConditionOut = Just "Equipo completo"
              , ME.assetCheckoutPhotoOutUrl = Just "inventory/checkout.jpg"
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Just "Solo visible para ops"
              })
        canonicalToken
        (AssetCheckinRequest
          (Just "Returned OK")
          (Just "Cables verificados")
          (Just "inventory/checkin.jpg"))
      case result of
        Left err ->
          expectationFailure ("Expected public QR check-in to succeed, got " <> show err)
        Right checkout -> do
          targetKind checkout `shouldBe` "party"
          targetPartyRef checkout `shouldBe` Just "Backline Crew"
          disposition checkout `shouldBe` "rental"
          dueAt checkout `shouldBe` Just dueAtValue
          returnedAt checkout `shouldSatisfy` isJust
          termsAndConditions checkout `shouldBe` Nothing
          holderEmail checkout `shouldBe` Nothing
          holderPhone checkout `shouldBe` Nothing
          paymentType checkout `shouldBe` Nothing
          paymentInstallments checkout `shouldBe` Nothing
          paymentReference checkout `shouldBe` Nothing
          paymentAmountCents checkout `shouldBe` Nothing
          paymentCurrency checkout `shouldBe` Nothing
          paymentOutstandingCents checkout `shouldBe` Nothing
          checkedOutBy checkout `shouldBe` "redacted"
          conditionOut checkout `shouldBe` Nothing
          photoOutUrl checkout `shouldBe` Nothing
          conditionIn checkout `shouldBe` Nothing
          photoInUrl checkout `shouldBe` Nothing
          notes checkout `shouldBe` Nothing

    it "returns a conflict when a valid public QR asset exists but has no active checkout" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in asset fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckinHandler
        (insertKey assetKey
          ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
            { assetQrCode = Just canonicalToken
            }))
        canonicalToken
        request
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "Asset is not currently checked out"
        Right value ->
          expectationFailure ("Expected idle public QR check-in to fail, got " <> show value)

    it "rejects public QR check-ins for internal room or session movements" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in checkout fixture key" >> fail "unreachable"
      roomKey <- case (fromPathPiece roomIdText :: Maybe (Key Room)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in room fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckinHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetQrCode = Just canonicalToken
                , assetStatus = Booked
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetRoom
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Nothing
              , ME.assetCheckoutTargetRoomId = Just roomKey
              , ME.assetCheckoutDisposition = Loan
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Nothing
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "1"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Nothing
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Nothing
              })
        canonicalToken
        request
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "Public QR check-in only supports party checkouts"
        Right value ->
          expectationFailure ("Expected public QR room check-in to be rejected, got " <> show value)

    it "rejects public QR check-ins for internal party repair movements" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in checkout fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckinHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetQrCode = Just canonicalToken
                , assetStatus = Booked
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Taller externo"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = ME.Repair
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Nothing
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "1"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Pending repair"
              , ME.assetCheckoutPhotoOutUrl = Just "inventory/repair-checkout.jpg"
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Just "Only ops should close this movement"
              })
        canonicalToken
        request
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "Public QR check-in only supports party loan or rental checkouts"
        Right value ->
          expectationFailure ("Expected public QR repair check-in to be rejected, got " <> show value)

    it "requires a return condition on public QR check-ins before closing the active movement" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in checkout fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckinHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetQrCode = Just canonicalToken
                , assetStatus = Booked
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Backline Crew"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Loan
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Nothing
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "public-link"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Just "inventory/checkout.jpg"
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Nothing
              })
        canonicalToken
        (AssetCheckinRequest Nothing (Just "Cables verified") Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Public QR check-in requires ciConditionIn"
        Right value ->
          expectationFailure ("Expected public QR check-in without condition to be rejected, got " <> show value)

    it "rejects public QR sale check-ins so sold assets stay query-only even on direct API calls" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in checkout fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckinHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetQrCode = Just canonicalToken
                , assetStatus = Retired
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Buyer"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Sale
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Just "buyer@example.com"
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Just "card"
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Just 120000
              , ME.assetCheckoutPaymentCurrency = Just "USD"
              , ME.assetCheckoutPaymentOutstandingCents = Just 0
              , ME.assetCheckoutCheckedOutByRef = "public-link"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Sold"
              , ME.assetCheckoutPhotoOutUrl = Just "inventory/sale-checkout.jpg"
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Just "Factura entregada"
              })
        canonicalToken
        (AssetCheckinRequest (Just "Returned by buyer") (Just "Should stay sold") (Just "inventory/return.jpg"))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "Sale checkouts cannot be checked in"
        Right value ->
          expectationFailure ("Expected public QR sale check-in to be rejected, got " <> show value)

    it "requires a return photo on public QR check-ins so anonymous returns keep visual proof" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in checkout fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckinHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetQrCode = Just canonicalToken
                , assetStatus = Booked
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Backline Crew"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Loan
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Nothing
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "public-link"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Just "inventory/checkout.jpg"
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Nothing
              })
        canonicalToken
        (AssetCheckinRequest (Just "Returned OK") (Just "Cables verified") Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Public QR check-in requires ciPhotoUrl"
        Right value ->
          expectationFailure ("Expected public QR check-in without photo to be rejected, got " <> show value)

    it "rejects externally hosted return proof on public QR check-ins so anonymous custody evidence cannot point at arbitrary remote URLs" $ do
      assetKey <- case (fromPathPiece existingAssetId :: Maybe (Key Asset)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in asset fixture key" >> fail "unreachable"
      checkoutKey <- case (fromPathPiece checkoutIdText :: Maybe (Key ME.AssetCheckout)) of
        Just key -> pure key
        Nothing -> expectationFailure "invalid public check-in checkout fixture key" >> fail "unreachable"
      result <- runInventoryPublicCheckinHandler
        (do
            now <- liftIO getCurrentTime
            insertKey assetKey
              ((fixtureAsset "Roland Juno-106" "Synth" (Just "Roland") (Just "Juno-106") "TDF" Nothing)
                { assetQrCode = Just canonicalToken
                , assetStatus = Booked
                })
            insertKey checkoutKey ME.AssetCheckout
              { ME.assetCheckoutAssetId = assetKey
              , ME.assetCheckoutTargetKind = TargetParty
              , ME.assetCheckoutTargetSessionId = Nothing
              , ME.assetCheckoutTargetPartyRef = Just "Backline Crew"
              , ME.assetCheckoutTargetRoomId = Nothing
              , ME.assetCheckoutDisposition = Loan
              , ME.assetCheckoutTermsAndConditions = Nothing
              , ME.assetCheckoutHolderEmail = Nothing
              , ME.assetCheckoutHolderPhone = Nothing
              , ME.assetCheckoutPaymentType = Nothing
              , ME.assetCheckoutPaymentInstallments = Nothing
              , ME.assetCheckoutPaymentReference = Nothing
              , ME.assetCheckoutPaymentAmountCents = Nothing
              , ME.assetCheckoutPaymentCurrency = Nothing
              , ME.assetCheckoutPaymentOutstandingCents = Nothing
              , ME.assetCheckoutCheckedOutByRef = "public-link"
              , ME.assetCheckoutCheckedOutAt = now
              , ME.assetCheckoutDueAt = Nothing
              , ME.assetCheckoutConditionOut = Just "Good"
              , ME.assetCheckoutPhotoOutUrl = Just "inventory/checkout.jpg"
              , ME.assetCheckoutPhotoDriveFileId = Nothing
              , ME.assetCheckoutReturnedAt = Nothing
              , ME.assetCheckoutConditionIn = Nothing
              , ME.assetCheckoutPhotoInUrl = Nothing
              , ME.assetCheckoutNotes = Nothing
              })
        canonicalToken
        (AssetCheckinRequest
          (Just "Returned OK")
          (Just "Cables verified")
          (Just "https://cdn.example.com/checkins/juno-106.jpg"))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "uploaded inventory asset path"
        Right value ->
          expectationFailure ("Expected public QR check-in with external photo proof to be rejected, got " <> show value)

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

  describe "validateSessionInputRowsWrite" $ do
    let inputRow =
          SessionInputRow
            1
            (Just "Lead vocal")
            (Just "Voice")
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            (Just True)
            Nothing
            Nothing
            Nothing
            Nothing
        assertRejected result = case result of
          Left err -> do
            errHTTPCode err `shouldBe` 400
            BL8.unpack (errBody err) `shouldContain` "input list rows are read-only"
          Right value ->
            expectationFailure ("Expected session input rows to be rejected, got " <> show value)

    it "allows omitted input rows but rejects arrays the session write path cannot persist" $ do
      validateSessionInputRowsWrite Nothing `shouldBe` Right ()
      assertRejected (validateSessionInputRowsWrite (Just []))
      assertRejected (validateSessionInputRowsWrite (Just [inputRow]))

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

    it "rejects non-positive member ids before band creation falls through to unknown-party lookups" $
      case validateDistinctBandMemberIds [toSqlKey 0 :: Key M.Party] of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "band member party ids must be positive"
        Right value ->
          expectationFailure ("Expected non-positive band member ids to be rejected, got " <> show value)

  describe "validatePaymentMethod" $ do
    it "accepts supported manual-payment aliases, including persisted enum labels reused by the UI" $ do
      validatePaymentMethod " Produbanco " `shouldBe` Right M.BankTransferM
      validatePaymentMethod "bank_transfer" `shouldBe` Right M.BankTransferM
      validatePaymentMethod "BankTransferM" `shouldBe` Right M.BankTransferM
      validatePaymentMethod "Card" `shouldBe` Right M.CardPOSM
      validatePaymentMethod "PayPalM" `shouldBe` Right M.PayPalM
      validatePaymentMethod "other" `shouldBe` Right M.OtherM

    it "rejects blank, unknown, or control-bearing manual payment methods instead of silently storing them as bank transfers" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid payment method error, got " <> show value)
      assertInvalid "paymentMethod must be one of" (validatePaymentMethod "   ")
      assertInvalid "paymentMethod must be one of" (validatePaymentMethod "wire-transfer")
      assertInvalid "paymentMethod must not contain control characters" (validatePaymentMethod "cash\n")

  describe "validatePaymentCurrency" $ do
    it "normalizes supported manual payment currencies to USD" $ do
      validatePaymentCurrency "USD" `shouldBe` Right "USD"
      validatePaymentCurrency " usd " `shouldBe` Right "USD"

    it "rejects blank, unsupported, or control-bearing payment currencies instead of silently coercing them to USD" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid payment currency error, got " <> show value)
      assertInvalid "Only USD manual payments are currently supported" (validatePaymentCurrency "   ")
      assertInvalid "Only USD manual payments are currently supported" (validatePaymentCurrency "EUR")
      assertInvalid "currency must not contain control characters" (validatePaymentCurrency "USD\n")

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

  describe "parseUTCTimeText" $ do
    it "accepts canonical manual-payment dates after trimming transport whitespace" $ do
      result <- runExceptT (parseUTCTimeText " 2026-04-13 ")
      result `shouldBe` Right (UTCTime (fromGregorian 2026 4 13) 0)

    it "rejects loosely shaped manual-payment dates before persistence" $ do
      let assertInvalid rawDate = do
            result <- runExceptT (parseUTCTimeText rawDate)
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "Invalid date format, expected YYYY-MM-DD"
              Right value ->
                expectationFailure ("Expected invalid payment date error, got " <> show value)
      assertInvalid "2026-4-13"
      assertInvalid "2026-04-1"
      assertInvalid "2026/04/13"

  describe "validatePaymentPaidAt" $ do
    let now = UTCTime (fromGregorian 2026 4 13) 0
        past = UTCTime (fromGregorian 2026 4 12) 0
        future = UTCTime (fromGregorian 2026 4 14) 0

    it "accepts same-day and historical manual payment dates without rewriting them" $ do
      validatePaymentPaidAt now now `shouldBe` Right now
      validatePaymentPaidAt now past `shouldBe` Right past

    it "rejects future manual payment dates before persisting impossible payments" $
      case validatePaymentPaidAt now future of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "paidAt must not be in the future"
        Right value ->
          expectationFailure ("Expected future payment date error, got " <> show value)

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
              BL8.unpack (errBody err) `shouldContain` "attachmentUrl must be an absolute https URL"
            Right value ->
              expectationFailure ("Expected invalid payment attachment URL error, got " <> show value)
      assertInvalid (validatePaymentAttachmentUrl (Just "proof.pdf"))
      assertInvalid (validatePaymentAttachmentUrl (Just "http://files.example.com/proof.pdf"))
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

    it "rejects oversized or control-character concepts before storing manual payment rows" $ do
      let assertInvalid expectedMessage rawConcept =
            case validatePaymentConcept rawConcept of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected invalid payment concept error, got " <> show value)
      assertInvalid "concept must be 240 characters or fewer" (T.replicate 241 "a")
      assertInvalid "concept must not contain control characters" "Honorarios\nabril"

  describe "validatePaymentReference and validatePaymentPeriod" $ do
    it "normalizes optional manual payment labels before persistence" $ do
      validatePaymentReference Nothing `shouldBe` Right Nothing
      validatePaymentReference (Just "   ") `shouldBe` Right Nothing
      validatePaymentReference (Just "  REC-42  ") `shouldBe` Right (Just "REC-42")
      validatePaymentPeriod (Just "  2026-04  ") `shouldBe` Right (Just "2026-04")

    it "rejects oversized or control-character optional payment labels" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid payment label error, got " <> show value)
      assertInvalid
        "reference must be 160 characters or fewer"
        (validatePaymentReference (Just (T.replicate 161 "a")))
      assertInvalid
        "reference must not contain control characters"
        (validatePaymentReference (Just "REC\n42"))
      assertInvalid
        "period must be 80 characters or fewer"
        (validatePaymentPeriod (Just (T.replicate 81 "a")))
      assertInvalid
        "period must not contain control characters"
        (validatePaymentPeriod (Just "2026\n04"))

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

  describe "social reply identifier validation" $ do
    it "normalizes omitted and explicit reply ids without inventing fallbacks" $ do
      validateSocialReplySenderId " ig-user-1 "
        `shouldBe` Right "ig-user-1"
      validateSocialReplyExternalId Nothing
        `shouldBe` Right Nothing
      validateSocialReplyExternalId (Just " msg-1 ")
        `shouldBe` Right (Just "msg-1")

    it "rejects blank, whitespace, control, or oversized ids before reply dispatch" $ do
      let assertInvalid expectedMessage result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure ("Expected invalid social reply identifier error, got " <> show value)
      assertInvalid "senderId is required" (validateSocialReplySenderId "   ")
      assertInvalid
        "externalId must be omitted or a non-empty string"
        (validateSocialReplyExternalId (Just "   "))
      assertInvalid
        "senderId must not contain whitespace"
        (validateSocialReplySenderId "ig user")
      assertInvalid
        "externalId must not contain control characters"
        (validateSocialReplyExternalId (Just ("msg" <> T.singleton '\NUL' <> "1")))
      assertInvalid
        "senderId must be 256 characters or fewer"
        (validateSocialReplySenderId (T.replicate 257 "a"))

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

  describe "validateMetaWebhookVerifyRequest" $ do
    it "returns the explicit challenge only for complete subscribe handshakes with a matching token" $
      validateMetaWebhookVerifyRequest
        "instagram"
        (Just " SuBsCrIbE ")
        (Just " challenge-123 ")
        (Just " secret ")
        [Just "   ", Just "secret"]
        `shouldBe` Right "challenge-123"

    it "keeps Facebook webhook verification scoped to Facebook tokens" $ do
      cfg <- Config.loadConfig
      let configured =
            cfg
              { facebookMessagingToken = Just "fb-secret"
              , instagramVerifyToken = Just "ig-secret"
              , instagramMessagingToken = Just "ig-msg-secret"
              , instagramAppToken = Just "ig-app-secret"
              }
          env =
            Env
              { envPool = error "verifyMetaWebhook test does not use the database pool"
              , envConfig = configured
              }
          verify :: Text -> Either ServerError Text
          verify token =
            runReaderT
              (verifyMetaWebhook MetaFacebook (Just "subscribe") (Just token) (Just "challenge-123"))
              env
      metaWebhookVerifyTokenCandidates MetaFacebook configured
        `shouldBe` [Just "fb-secret"]
      case verify "fb-secret" of
        Left err ->
          expectationFailure
            ("Expected Facebook verify token to be accepted, got " <> show (errHTTPCode err))
        Right challenge ->
          challenge `shouldBe` "challenge-123"
      case verify "ig-secret" of
        Left err -> do
          errHTTPCode err `shouldBe` 403
          BL8.unpack (errBody err) `shouldContain` "Meta verify token mismatch for facebook"
        Right challenge ->
          expectationFailure
            ("Expected Instagram verify token to be rejected, got " <> T.unpack challenge)

    it "rejects incomplete or ambiguous Meta webhook verification handshakes" $ do
      let assertInvalid expectedCode expectedMessage result =
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` expectedCode
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right challenge ->
                expectationFailure
                  ("Expected invalid Meta webhook verification, got " <> T.unpack challenge)
          validate = validateMetaWebhookVerifyRequest "facebook"
      assertInvalid
        400
        "hub.mode is required"
        (validate Nothing (Just "challenge-123") (Just "secret") [Just "secret"])
      assertInvalid
        400
        "hub.mode must be subscribe"
        (validate (Just "publish") (Just "challenge-123") (Just "secret") [Just "secret"])
      assertInvalid
        400
        "hub.challenge is required"
        (validate (Just "subscribe") (Just "   ") (Just "secret") [Just "secret"])
      assertInvalid
        400
        "hub.verify_token is required"
        (validate (Just "subscribe") (Just "challenge-123") Nothing [Just "secret"])
      assertInvalid
        403
        "Meta verify token mismatch for facebook"
        (validate (Just "subscribe") (Just "challenge-123") (Just "wrong") [Just "secret"])
      assertInvalid
        403
        "Meta verify token not configured"
        (validate (Just "subscribe") (Just "challenge-123") (Just "secret") [Just "   "])

    it "rejects unsafe Meta webhook verification values before echoing or falling back" $ do
      let assertInvalid expectedCode expectedMessage result =
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` expectedCode
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right challenge ->
                expectationFailure
                  ("Expected unsafe Meta webhook verification to fail, got " <> T.unpack challenge)
          validate = validateMetaWebhookVerifyRequest "instagram" (Just "subscribe")
      assertInvalid
        400
        "hub.challenge must be 512 characters or fewer"
        (validate (Just (T.replicate 513 "x")) (Just "secret") [Just "secret"])
      assertInvalid
        400
        "hub.challenge must not contain control characters"
        (validate (Just "challenge\nInjected: value") (Just "secret") [Just "secret"])
      assertInvalid
        400
        "hub.verify_token must not contain control characters"
        (validate (Just "challenge-123") (Just "secret\nInjected") [Just "secret"])
      assertInvalid
        403
        "Meta verify token is misconfigured"
        (validate (Just "challenge-123") (Just "secret") [Just "bad\nsecret", Just "secret"])

  describe "validateMetaWebhookChannel" $ do
    it "accepts only route-matching Meta webhook object values" $ do
      let instagramPayload = A.object ["object" .= (" InStAgRaM " :: Text)]
          facebookPagePayload = A.object ["object" .= ("page" :: Text)]
          facebookAliasPayload = A.object ["object" .= ("facebook" :: Text)]
      validateMetaWebhookChannel MetaInstagram instagramPayload
        `shouldBe` Right MetaInstagram
      validateMetaWebhookChannel MetaFacebook facebookPagePayload
        `shouldBe` Right MetaFacebook
      validateMetaWebhookChannel MetaFacebook facebookAliasPayload
        `shouldBe` Right MetaFacebook

    it "rejects missing or mismatched Meta webhook objects instead of falling back to the endpoint" $ do
      let assertInvalid expectedMessage result =
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right channel ->
                expectationFailure ("Expected invalid Meta webhook channel, got " <> show channel)
      assertInvalid
        "Meta webhook object must be one of: instagram, page, facebook"
        (validateMetaWebhookChannel MetaInstagram (A.object []))
      assertInvalid
        "Meta webhook object does not match the webhook endpoint"
        (validateMetaWebhookChannel MetaInstagram (A.object ["object" .= ("page" :: Text)]))
      assertInvalid
        "Meta webhook object does not match the webhook endpoint"
        (validateMetaWebhookChannel MetaFacebook (A.object ["object" .= ("instagram" :: Text)]))

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

  describe "normalizeServiceCatalogName" $ do
    it "normalizes bounded service catalog names before create/update persistence" $ do
      normalizeServiceCatalogName "  Mezcla Full  " `shouldBe` Right "Mezcla Full"
      normalizeServiceCatalogNameUpdate Nothing `shouldBe` Right Nothing
      normalizeServiceCatalogNameUpdate (Just "  Mezcla Full  ") `shouldBe` Right (Just "Mezcla Full")

    it "rejects malformed service catalog names before persistence" $ do
      let assertInvalid expected result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expected
            Right value ->
              expectationFailure ("Expected invalid service catalog name error, got " <> show value)
      assertInvalid "Nombre requerido" (normalizeServiceCatalogName "   ")
      assertInvalid
        "160 caracteres o menos"
        (normalizeServiceCatalogName (T.replicate 161 "a"))
      assertInvalid
        "caracteres de control"
        (normalizeServiceCatalogName "Podcast\nLive")
      assertInvalid "Nombre requerido" (normalizeServiceCatalogNameUpdate (Just "   "))
      assertInvalid
        "caracteres de control"
        (normalizeServiceCatalogNameUpdate (Just "Podcast\tLive"))

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

  describe "validateServiceCatalogBillingUnit" $ do
    it "normalizes bounded billing units and treats omitted create values as empty" $ do
      validateServiceCatalogBillingUnit Nothing `shouldBe` Right Nothing
      validateServiceCatalogBillingUnit (Just "   ") `shouldBe` Right Nothing
      validateServiceCatalogBillingUnit (Just "  por sesión  ")
        `shouldBe` Right (Just "por sesión")

    it "rejects malformed billing units before service catalog writes persist opaque labels" $ do
      let assertInvalid expected result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expected
            Right value ->
              expectationFailure ("Expected invalid billing unit error, got " <> show value)
      assertInvalid
        "80 caracteres o menos"
        (validateServiceCatalogBillingUnit (Just (T.replicate 81 "a")))
      assertInvalid
        "caracteres de control"
        (validateServiceCatalogBillingUnit (Just "sesión\nextra"))

  describe "validateServiceCatalogBillingUnitUpdate" $ do
    it "preserves omitted and explicit-null updates while trimming meaningful billing units" $ do
      validateServiceCatalogBillingUnitUpdate Nothing `shouldBe` Right Nothing
      validateServiceCatalogBillingUnitUpdate (Just Nothing) `shouldBe` Right (Just Nothing)
      validateServiceCatalogBillingUnitUpdate (Just (Just "  por hora  "))
        `shouldBe` Right (Just (Just "por hora"))

    it "rejects blank or malformed updates instead of silently clearing billing units" $ do
      let assertInvalid expected result = case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expected
            Right value ->
              expectationFailure ("Expected invalid billing unit update error, got " <> show value)
      assertInvalid
        "debe omitirse, ser null, o contener texto"
        (validateServiceCatalogBillingUnitUpdate (Just (Just "   ")))
      assertInvalid
        "caracteres de control"
        (validateServiceCatalogBillingUnitUpdate (Just (Just "por\nhora")))

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
    it "uses the deterministic external id fallback when Meta sends a blank message id" $ do
        let payload =
                A.object
                    [ "object" .= ("instagram" :: Text)
                    , "entry"
                        .=
                            [ A.object
                                [ "id" .= ("17841400000000000" :: Text)
                                , "messaging"
                                    .=
                                        [ A.object
                                            [ "sender" .= A.object ["id" .= ("user-1" :: Text)]
                                            , "recipient" .= A.object ["id" .= ("biz-1" :: Text)]
                                            , "timestamp" .= (1773630000 :: Int)
                                            , "message"
                                                .= A.object
                                                    [ "mid" .= ("   " :: Text)
                                                    , "text" .= ("hola" :: Text)
                                                    ]
                                            ]
                                        ]
                                ]
                            ]
                    ]
            events = extractMetaInbound payload
        case events of
            [MetaInboundMessage inbound] -> do
                igInboundExternalId inbound `shouldSatisfy` T.isPrefixOf "user-1-"
                igInboundExternalId inbound `shouldNotBe` "   "
                igInboundText inbound `shouldBe` "hola"
            _ -> expectationFailure ("Expected an inbound event, got " <> show events)

    it "keeps malformed Meta message ids out of persisted webhook events" $ do
        let payload =
                A.object
                    [ "object" .= ("instagram" :: Text)
                    , "entry"
                        .=
                            [ A.object
                                [ "id" .= ("17841400000000000" :: Text)
                                , "messaging"
                                    .=
                                        [ A.object
                                            [ "sender" .= A.object ["id" .= ("user-1" :: Text)]
                                            , "recipient" .= A.object ["id" .= ("biz-1" :: Text)]
                                            , "timestamp" .= (1773630000 :: Int)
                                            , "message"
                                                .= A.object
                                                    [ "mid" .= ("mid with space" :: Text)
                                                    , "text" .= ("hola" :: Text)
                                                    ]
                                            ]
                                        ]
                                , "changes"
                                    .=
                                        [ A.object
                                            [ "field" .= ("messages" :: Text)
                                            , "value"
                                                .= A.object
                                                    [ "from" .= A.object ["id" .= ("user-1" :: Text)]
                                                    , "timestamp" .= (1773630001 :: Int)
                                                    , "message"
                                                        .= A.object
                                                            [ "mid" .= ("bad mid" :: Text)
                                                            , "is_deleted" .= True
                                                            ]
                                                    ]
                                            ]
                                        ]
                                ]
                            ]
                    ]
            events = extractMetaInbound payload
        case events of
            [MetaInboundMessage inbound] -> do
                igInboundExternalId inbound `shouldSatisfy` T.isPrefixOf "user-1-"
                igInboundExternalId inbound `shouldNotBe` "mid with space"
                igInboundText inbound `shouldBe` "hola"
            _ -> expectationFailure ("Expected one valid inbound event, got " <> show events)

    it "normalizes actor ids and drops ambiguous Meta webhook events before persistence" $ do
        let payload =
                A.object
                    [ "object" .= ("instagram" :: Text)
                    , "entry"
                        .=
                            [ A.object
                                [ "id" .= ("17841400000000000" :: Text)
                                , "messaging"
                                    .=
                                        [ A.object
                                            [ "sender" .= A.object ["id" .= ("  user-1  " :: Text)]
                                            , "recipient" .= A.object ["id" .= ("  biz-1  " :: Text)]
                                            , "timestamp" .= (1773630000 :: Int)
                                            , "message"
                                                .= A.object
                                                    [ "mid" .= ("mid-trimmed" :: Text)
                                                    , "text" .= ("hola" :: Text)
                                                    ]
                                            ]
                                        , A.object
                                            [ "sender" .= A.object ["id" .= ("   " :: Text)]
                                            , "message"
                                                .= A.object
                                                    [ "mid" .= ("mid-blank-sender" :: Text)
                                                    , "text" .= ("ambiguous" :: Text)
                                                    ]
                                            ]
                                        , A.object
                                            [ "sender" .= A.object ["id" .= ("user 2" :: Text)]
                                            , "message"
                                                .= A.object
                                                    [ "mid" .= ("mid-whitespace-sender" :: Text)
                                                    , "text" .= ("ambiguous" :: Text)
                                                    ]
                                            ]
                                        ]
                                ]
                            ]
                    ]
            events = extractMetaInbound payload
        case events of
            [MetaInboundMessage inbound] -> do
                igInboundExternalId inbound `shouldBe` "mid-trimmed"
                igInboundSenderId inbound `shouldBe` "user-1"
                igInboundMetadata inbound `shouldSatisfy`
                    maybe False (T.isInfixOf "\"recipient_id\":\"biz-1\"")
            _ -> expectationFailure ("Expected one normalized inbound event, got " <> show events)

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

    it "keeps reply delivery bound to the incoming Instagram recipient instead of falling back to another asset" $ do
        now <- getCurrentTime
        (mAccountId, mToken) <- runMetaInboxSql $ do
            _ <- insert M.SocialSyncAccount
                { M.socialSyncAccountPartyId = Nothing
                , M.socialSyncAccountArtistProfileId = Nothing
                , M.socialSyncAccountPlatform = "instagram"
                , M.socialSyncAccountExternalUserId = "biz-other"
                , M.socialSyncAccountHandle = Just "other-account"
                , M.socialSyncAccountAccessToken = Just "other-token"
                , M.socialSyncAccountTokenExpiresAt = Nothing
                , M.socialSyncAccountStatus = "connected"
                , M.socialSyncAccountLastSyncedAt = Nothing
                , M.socialSyncAccountCreatedAt = now
                , M.socialSyncAccountUpdatedAt = Just now
                }
            persistMetaInbound MetaInstagram now
                [ MetaInboundMessage
                    IGInbound
                        { igInboundExternalId = "mid-preferred"
                        , igInboundSenderId = "user-1"
                        , igInboundSenderName = Just "Fan"
                        , igInboundText = "hola"
                        , igInboundAdExternalId = Nothing
                        , igInboundAdName = Nothing
                        , igInboundCampaignExternalId = Nothing
                        , igInboundCampaignName = Nothing
                        , igInboundMetadata = Just "{\"recipient_id\":\"biz-missing\"}"
                        }
                ]
            resolveInstagramReplyContext (Just "mid-preferred")

        mAccountId `shouldBe` Just "biz-missing"
        mToken `shouldBe` Nothing

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

runInventoryCheckoutHandler
  :: SqlPersistT IO ()
  -> Text
  -> AssetCheckoutRequest
  -> IO (Either ServerError AssetCheckoutDTO)
runInventoryCheckoutHandler setup rawId req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeInventoryCheckinSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in inventory checkout tests"
            }
    liftIO $ runExceptT (runReaderT (checkoutHandlerFor inventoryUser rawId req) env)

runInventoryPatchHandler
  :: SqlPersistT IO ()
  -> Text
  -> AssetUpdate
  -> IO (Either ServerError AssetDTO)
runInventoryPatchHandler setup rawId req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeInventoryCheckinSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in inventory patch tests"
            }
    liftIO $ runExceptT (runReaderT (patchAssetHandlerFor inventoryUser rawId req) env)

runInventoryCheckoutHistoryHandler
  :: SqlPersistT IO ()
  -> Text
  -> IO (Either ServerError [AssetCheckoutDTO])
runInventoryCheckoutHistoryHandler setup rawId =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeInventoryCheckinSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in inventory checkout history tests"
            }
    liftIO $ runExceptT (runReaderT (checkoutHistoryHandlerFor inventoryUser rawId) env)

runInventoryDeleteHandler
  :: SqlPersistT IO ()
  -> Text
  -> IO (Either ServerError (), Bool)
runInventoryDeleteHandler setup rawId =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeInventoryCheckinSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in inventory delete tests"
            }
    result <- liftIO $ runExceptT (runReaderT (deleteHandlerFor inventoryUser rawId) env)
    assetStillExists <- case (fromPathPiece rawId :: Maybe (Key Asset)) of
      Nothing -> pure False
      Just assetKey -> liftIO $ runSqlPool (maybe False (const True) <$> get assetKey) pool
    pure (result, assetStillExists)

runInventoryRefreshQrHandler
  :: SqlPersistT IO ()
  -> Text
  -> IO (Either ServerError AssetQrDTO)
runInventoryRefreshQrHandler setup rawId =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeInventoryCheckinSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in missing inventory QR tests"
            }
    liftIO $ runExceptT (runReaderT (refreshQrHandlerFor inventoryUser rawId) env)

runInventoryResolveQrHandler
  :: SqlPersistT IO ()
  -> Text
  -> IO (Either ServerError AssetDTO)
runInventoryResolveQrHandler setup token =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeInventoryCheckinSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in inventory QR resolve tests"
            }
    liftIO $ runExceptT (runReaderT (resolveByQrHandlerFor inventoryUser token) env)

runInventoryPublicResolveQrHandler
  :: SqlPersistT IO ()
  -> Text
  -> IO (Either ServerError AssetDTO)
runInventoryPublicResolveQrHandler setup token =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeInventoryCheckinSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in public inventory QR resolve tests"
            }
    liftIO $ runExceptT (runReaderT (publicResolveByQrHandlerFor token) env)

runInventoryPublicCheckoutHandler
  :: SqlPersistT IO ()
  -> Text
  -> AssetCheckoutRequest
  -> IO (Either ServerError AssetCheckoutDTO)
runInventoryPublicCheckoutHandler setup token req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeInventoryCheckinSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in public inventory checkout tests"
            }
    liftIO $ runExceptT (runReaderT (publicCheckoutHandlerFor token req) env)

runInventoryPublicCheckinHandler
  :: SqlPersistT IO ()
  -> Text
  -> AssetCheckinRequest
  -> IO (Either ServerError AssetCheckoutDTO)
runInventoryPublicCheckinHandler setup token req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeInventoryCheckinSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in public inventory check-in tests"
            }
    liftIO $ runExceptT (runReaderT (publicCheckinHandlerFor token req) env)

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

runBandCreateHandler
  :: SqlPersistT IO ()
  -> BandCreate
  -> IO (Either ServerError BandDTO)
runBandCreateHandler setup req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeBandSchema pool
    liftIO $ runSqlPool setup pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in band tests"
            }
    liftIO $ runExceptT (runReaderT (createBandHandlerFor inventoryUser req) env)

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

checkoutHandlerFor :: AuthedUser -> Text -> AssetCheckoutRequest -> InventoryTestM AssetCheckoutDTO
checkoutHandlerFor user =
  case (inventoryServer user :: ServerT InventoryAPI InventoryTestM) of
    _listAssets
      :<|> _createAsset
      :<|> _uploadAssetPhoto
      :<|> _getAsset
      :<|> _patchAsset
      :<|> _deleteAsset
      :<|> checkoutAsset
      :<|> _checkinAsset
      :<|> _checkoutHistory
      :<|> _refreshQr
      :<|> _resolveByQr ->
          checkoutAsset

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

patchAssetHandlerFor :: AuthedUser -> Text -> AssetUpdate -> InventoryTestM AssetDTO
patchAssetHandlerFor user =
  case (inventoryServer user :: ServerT InventoryAPI InventoryTestM) of
    _listAssets
      :<|> _createAsset
      :<|> _uploadAssetPhoto
      :<|> _getAsset
      :<|> patchAsset
      :<|> _deleteAsset
      :<|> _checkoutAsset
      :<|> _checkinAsset
      :<|> _checkoutHistory
      :<|> _refreshQr
      :<|> _resolveByQr ->
          patchAsset

checkoutHistoryHandlerFor :: AuthedUser -> Text -> InventoryTestM [AssetCheckoutDTO]
checkoutHistoryHandlerFor user =
  case (inventoryServer user :: ServerT InventoryAPI InventoryTestM) of
    _listAssets
      :<|> _createAsset
      :<|> _uploadAssetPhoto
      :<|> _getAsset
      :<|> _patchAsset
      :<|> _deleteAsset
      :<|> _checkoutAsset
      :<|> _checkinAsset
      :<|> checkoutHistory
      :<|> _refreshQr
      :<|> _resolveByQr ->
          checkoutHistory

deleteHandlerFor :: AuthedUser -> Text -> InventoryTestM ()
deleteHandlerFor user =
  case (inventoryServer user :: ServerT InventoryAPI InventoryTestM) of
    _listAssets
      :<|> _createAsset
      :<|> _uploadAssetPhoto
      :<|> _getAsset
      :<|> _patchAsset
      :<|> deleteAsset
      :<|> _checkoutAsset
      :<|> _checkinAsset
      :<|> _checkoutHistory
      :<|> _refreshQr
      :<|> _resolveByQr ->
          \rawId -> () <$ deleteAsset rawId

refreshQrHandlerFor :: AuthedUser -> Text -> InventoryTestM AssetQrDTO
refreshQrHandlerFor user =
  case (inventoryServer user :: ServerT InventoryAPI InventoryTestM) of
    _listAssets
      :<|> _createAsset
      :<|> _uploadAssetPhoto
      :<|> _getAsset
      :<|> _patchAsset
      :<|> _deleteAsset
      :<|> _checkoutAsset
      :<|> _checkinAsset
      :<|> _checkoutHistory
      :<|> refreshQr
      :<|> _resolveByQr ->
          refreshQr

resolveByQrHandlerFor :: AuthedUser -> Text -> InventoryTestM AssetDTO
resolveByQrHandlerFor user =
  case (inventoryServer user :: ServerT InventoryAPI InventoryTestM) of
    _listAssets
      :<|> _createAsset
      :<|> _uploadAssetPhoto
      :<|> _getAsset
      :<|> _patchAsset
      :<|> _deleteAsset
      :<|> _checkoutAsset
      :<|> _checkinAsset
      :<|> _checkoutHistory
      :<|> _refreshQr
      :<|> resolveByQr ->
          resolveByQr

publicResolveByQrHandlerFor :: Text -> InventoryTestM AssetDTO
publicResolveByQrHandlerFor =
  case (inventoryPublicServer :: ServerT InventoryPublicAPI InventoryTestM) of
    loadByQr
      :<|> _checkoutByQr
      :<|> _checkinByQr
      :<|> _uploadByQr ->
          loadByQr

publicCheckoutHandlerFor :: Text -> AssetCheckoutRequest -> InventoryTestM AssetCheckoutDTO
publicCheckoutHandlerFor =
  case (inventoryPublicServer :: ServerT InventoryPublicAPI InventoryTestM) of
    _loadByQr
      :<|> checkoutByQr
      :<|> _checkinByQr
      :<|> _uploadByQr ->
          checkoutByQr

publicCheckinHandlerFor :: Text -> AssetCheckinRequest -> InventoryTestM AssetCheckoutDTO
publicCheckinHandlerFor =
  case (inventoryPublicServer :: ServerT InventoryPublicAPI InventoryTestM) of
    _loadByQr
      :<|> _checkoutByQr
      :<|> checkinByQr
      :<|> _uploadByQr ->
          checkinByQr

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

createBandHandlerFor :: AuthedUser -> BandCreate -> InventoryTestM BandDTO
createBandHandlerFor user =
  case (bandsServer user :: ServerT BandsAPI InventoryTestM) of
    _listBands
      :<|> createBand
      :<|> _bandOptions
      :<|> _getBand ->
          createBand

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
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"social_sync_account\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"party_id\" INTEGER NULL,\
        \\"artist_profile_id\" INTEGER NULL,\
        \\"platform\" VARCHAR NOT NULL,\
        \\"external_user_id\" VARCHAR NOT NULL,\
        \\"handle\" VARCHAR NULL,\
        \\"access_token\" VARCHAR NULL,\
        \\"token_expires_at\" TIMESTAMP NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"last_synced_at\" TIMESTAMP NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NULL,\
        \CONSTRAINT \"unique_social_sync_account\" UNIQUE (\"platform\", \"external_user_id\")\
        \)"
        []

initializeSessionReferenceValidationSchema :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
initializeSessionReferenceValidationSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"band\" (\
        \\"id\" uuid PRIMARY KEY NOT NULL DEFAULT (lower(hex(randomblob(4)) || '-' || hex(randomblob(2)) || '-' || '4' || substr(hex(randomblob(2)), 2) || '-' || substr('89ab', (abs(random()) % 4) + 1, 1) || substr(hex(randomblob(2)), 2) || '-' || hex(randomblob(6)))),\
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
        \\"disposition\" VARCHAR NOT NULL,\
        \\"terms_and_conditions\" VARCHAR NULL,\
        \\"holder_email\" VARCHAR NULL,\
        \\"holder_phone\" VARCHAR NULL,\
        \\"payment_type\" VARCHAR NULL,\
        \\"payment_installments\" INTEGER NULL,\
        \\"payment_reference\" VARCHAR NULL,\
        \\"payment_amount_cents\" INTEGER NULL,\
        \\"payment_currency\" VARCHAR NULL,\
        \\"payment_outstanding_cents\" INTEGER NULL,\
        \\"checked_out_by_ref\" VARCHAR NOT NULL,\
        \\"checked_out_at\" TIMESTAMP NOT NULL,\
        \\"due_at\" TIMESTAMP NULL,\
        \\"condition_out\" VARCHAR NULL,\
        \\"photo_out_url\" VARCHAR NULL,\
        \\"photo_drive_file_id\" VARCHAR NULL,\
        \\"returned_at\" TIMESTAMP NULL,\
        \\"condition_in\" VARCHAR NULL,\
        \\"photo_in_url\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL\
        \)"
        []

initializeBandSchema :: SqlPersistT IO ()
initializeBandSchema = do
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
        "CREATE TABLE IF NOT EXISTS \"band\" (\
        \\"id\" uuid PRIMARY KEY NOT NULL DEFAULT (lower(hex(randomblob(4)) || '-' || hex(randomblob(2)) || '-' || '4' || substr(hex(randomblob(2)), 2) || '-' || substr('89ab', (abs(random()) % 4) + 1, 1) || substr(hex(randomblob(2)), 2) || '-' || hex(randomblob(6)))),\
        \\"party_id\" INTEGER NOT NULL,\
        \\"name\" VARCHAR NOT NULL,\
        \\"label_artist\" BOOLEAN NOT NULL,\
        \\"primary_genre\" VARCHAR NULL,\
        \\"home_city\" VARCHAR NULL,\
        \\"photo_url\" VARCHAR NULL,\
        \\"contract_flags\" VARCHAR NULL,\
        \CONSTRAINT \"unique_band_name\" UNIQUE (\"name\"),\
        \CONSTRAINT \"unique_band_party\" UNIQUE (\"party_id\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"band_member\" (\
        \\"id\" uuid PRIMARY KEY NOT NULL DEFAULT (lower(hex(randomblob(4)) || '-' || hex(randomblob(2)) || '-' || '4' || substr(hex(randomblob(2)), 2) || '-' || substr('89ab', (abs(random()) % 4) + 1, 1) || substr(hex(randomblob(2)), 2) || '-' || hex(randomblob(6)))),\
        \\"band_id\" uuid NOT NULL,\
        \\"party_id\" INTEGER NOT NULL,\
        \\"role_in_band\" VARCHAR NULL,\
        \CONSTRAINT \"unique_band_member\" UNIQUE (\"band_id\", \"party_id\")\
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
fixtureAsset name category brand model owner mNotes =
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
    , assetNotes = mNotes
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

fixtureBand :: Key M.Party -> Text -> Band
fixtureBand partyKey name =
  Band
    { bandPartyId = partyKey
    , bandName = name
    , bandLabelArtist = False
    , bandPrimaryGenre = Nothing
    , bandHomeCity = Nothing
    , bandPhotoUrl = Nothing
    , bandContractFlags = Nothing
    }
