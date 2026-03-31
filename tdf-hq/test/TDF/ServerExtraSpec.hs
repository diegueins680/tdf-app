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
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, rawExecute)
import Database.Persist.Sqlite (runSqlite)
import Servant (ServerError (errBody, errHTTPCode))
import Test.Hspec

import qualified TDF.Models as M
import TDF.ModelsExtra (AssetStatus (Booked, OutForMaintenance))
import TDF.ServerExtra (
    IGInbound (..),
    IGInboundDeleted (..),
    MetaChannel (..),
    MetaInboundEvent (..),
    extractMetaInbound,
    normalizeServiceCatalogNameUpdate,
    persistMetaInbound,
    validateAssetStatusUpdate,
 )

spec :: Spec
spec = do
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
