{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerExtraSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.Persist hiding (Active)
import Database.Persist.Sql (SqlBackend, SqlPersistT, rawExecute, toSqlKey)
import Database.Persist.Sqlite (runSqlite)
import Servant (ServerError (errBody, errHTTPCode))
import Test.Hspec
import Web.PathPieces (fromPathPiece)

import qualified TDF.Models as M
import TDF.API.Types (AssetCheckinRequest (..))
import TDF.ModelsExtra
  ( Asset (..)
  , AssetCondition (Good)
  , AssetStatus (Active, Booked, OutForMaintenance)
  , CheckoutTarget (TargetParty, TargetRoom, TargetSession)
  , MaintenancePolicy (None)
  , Room
  , Session
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
    normalizeAssetName,
    normalizeAssetNameUpdate,
    normalizeRoomName,
    normalizeRoomNameUpdate,
    validateSocialLimit,
    validatePaymentAmountCents,
    validatePaymentAttachmentUrl,
    parseCheckoutTargetKind,
    parseOptionalKeyField,
    validatePaymentCurrency,
    validatePaymentConcept,
    validatePositivePaymentReferenceId,
    validateOptionalPositivePaymentReferenceId,
    normalizeServiceCatalogNameUpdate,
    persistMetaInbound,
    validatePaymentMethod,
    validateSessionStatusInput,
    validateSessionTimeRange,
    validateCheckoutTargets,
    validateServiceCatalogCurrency,
    validateServiceCatalogCurrencyUpdate,
    validateServiceCatalogTaxBps,
    validateServiceCatalogTaxBpsUpdate,
    validateAssetStatusUpdate,
 )

spec :: Spec
spec = do
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

  describe "normalizeRoomName" $ do
    it "trims meaningful room names on create and update" $ do
      normalizeRoomName "  Sala A  " `shouldBe` Right "Sala A"
      normalizeRoomNameUpdate (Just "  Control Room  ") `shouldBe` Right (Just "Control Room")
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
      validateCheckoutTargets TargetRoom (Just "stale party") (Just roomId) Nothing
        `shouldBe` Right (Nothing, Just roomId, Nothing)
      validateCheckoutTargets TargetSession (Just "stale party") Nothing (Just sessionId)
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
      assertInvalid "targetSession is only allowed for session checkout" (validateCheckoutTargets TargetRoom Nothing (Just roomId) (Just sessionId))
      assertInvalid "targetRoom is only allowed for room checkout" (validateCheckoutTargets TargetSession Nothing (Just roomId) (Just sessionId))

  describe "normalizeAssetCheckinFields" $ do
    it "trims meaningful condition and notes before persisting a check-in" $ do
      normalizeAssetCheckinFields (AssetCheckinRequest (Just "  Returned OK  ") (Just "  Cables verified  "))
        `shouldBe` (Just "Returned OK", Just "Cables verified")

    it "drops omitted or blank check-in text so existing checkout context is not silently erased" $ do
      normalizeAssetCheckinFields (AssetCheckinRequest Nothing Nothing)
        `shouldBe` (Nothing, Nothing)
      normalizeAssetCheckinFields (AssetCheckinRequest (Just "   ") (Just "   "))
        `shouldBe` (Nothing, Nothing)

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
