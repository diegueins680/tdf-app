{-# LANGUAGE OverloadedStrings #-}

module TDF.WhatsApp.HistorySpec (spec) where

import Control.Monad.Logger (NoLoggingT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson (object)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist (entityVal, get, insert)
import Database.Persist.Sql (SqlPersistT, rawExecute)
import Database.Persist.Sqlite (runSqlite)
import Test.Hspec

import qualified TDF.ModelsExtra as ME
import TDF.WhatsApp.Client (SendTextResult (..))
import TDF.WhatsApp.History (OutgoingWhatsAppRecord (..), normalizeWhatsAppPhone, recordOutgoingWhatsAppMessage)

spec :: Spec
spec = do
  describe "TDF.WhatsApp.History.normalizeWhatsAppPhone" $ do
    it "normalizes plausible WhatsApp phone inputs to an E.164-style value" $ do
      normalizeWhatsAppPhone " +593 99 123 4567 " `shouldBe` Just "+593991234567"
      normalizeWhatsAppPhone "(02) 555-0123" `shouldBe` Just "+025550123"

    it "rejects mixed-text or implausible phone inputs instead of deriving misleading matches" $ do
      normalizeWhatsAppPhone "call me at 099 123 4567" `shouldBe` Nothing
      normalizeWhatsAppPhone "12345" `shouldBe` Nothing
      normalizeWhatsAppPhone "+1234567890123456" `shouldBe` Nothing
      normalizeWhatsAppPhone "593+991234567" `shouldBe` Nothing

  describe "recordOutgoingWhatsAppMessage" $ do
    it "falls back to a generated external id when a transport success returns a blank message id" $ do
      let now = UTCTime (fromGregorian 2026 4 12) (secondsToDiffTime 0)
          sendResult =
            Right SendTextResult
              { sendTextPayload = object []
              , sendTextMessageId = Just "   "
              }
      stored <- runWhatsAppHistorySql $
        recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
          { owrRecipientPhone = "+593991234567"
          , owrRecipientPartyId = Nothing
          , owrRecipientName = Just "Ada"
          , owrRecipientEmail = Nothing
          , owrActorPartyId = Nothing
          , owrBody = "Hola"
          , owrSource = Just "history_spec"
          , owrReplyToMessageId = Nothing
          , owrReplyToExternalId = Nothing
          , owrResendOfMessageId = Nothing
          , owrMetadata = Nothing
          }
          sendResult

      let externalId = ME.whatsAppMessageExternalId (entityVal stored)
      externalId `shouldSatisfy` (\val -> not (T.null (T.strip val)))
      externalId `shouldSatisfy` (\val -> ("+593991234567-out-" :: Text) `T.isPrefixOf` val)
      externalId `shouldSatisfy` (T.all (not . isSpace))

    it "allocates distinct fallback external ids for repeated blank-id sends at the same timestamp" $ do
      let now = UTCTime (fromGregorian 2026 4 12) (secondsToDiffTime 0)
          sendResult =
            Right SendTextResult
              { sendTextPayload = object []
              , sendTextMessageId = Just "   "
              }
          outgoing body = OutgoingWhatsAppRecord
            { owrRecipientPhone = "+593991234567"
            , owrRecipientPartyId = Nothing
            , owrRecipientName = Just "Ada"
            , owrRecipientEmail = Nothing
            , owrActorPartyId = Nothing
            , owrBody = body
            , owrSource = Just "history_spec"
            , owrReplyToMessageId = Nothing
            , owrReplyToExternalId = Nothing
            , owrResendOfMessageId = Nothing
            , owrMetadata = Nothing
            }
      (firstExternalId, secondExternalId) <- runWhatsAppHistorySql $ do
        first <- recordOutgoingWhatsAppMessage now (outgoing "Hola") sendResult
        second <- recordOutgoingWhatsAppMessage now (outgoing "Hola otra vez") sendResult
        pure
          ( ME.whatsAppMessageExternalId (entityVal first)
          , ME.whatsAppMessageExternalId (entityVal second)
          )

      firstExternalId `shouldSatisfy` (\val -> ("+593991234567-out-" :: Text) `T.isPrefixOf` val)
      secondExternalId `shouldSatisfy` (\val -> ("+593991234567-out-" :: Text) `T.isPrefixOf` val)
      secondExternalId `shouldNotBe` firstExternalId
      secondExternalId `shouldBe` firstExternalId <> "-2"

    it "applies reply outcomes only to incoming reply targets when linked by message id" $ do
      let now = UTCTime (fromGregorian 2026 4 12) (secondsToDiffTime 0)
          incomingReplySendResult =
            Right SendTextResult
              { sendTextPayload = object []
              , sendTextMessageId = Just "outgoing-reply-1"
              }
          outgoingReplySendResult =
            Right SendTextResult
              { sendTextPayload = object []
              , sendTextMessageId = Just "outgoing-reply-2"
              }
      (mIncomingTarget, mOutgoingTarget) <- runWhatsAppHistorySql $ do
        incomingKey <- insert (seedWhatsAppMessage now "incoming-target" "incoming")
        outgoingKey <- insert (seedWhatsAppMessage now "outgoing-target" "outgoing")
        _ <- recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
          { owrRecipientPhone = "+593991234567"
          , owrRecipientPartyId = Nothing
          , owrRecipientName = Just "Ada"
          , owrRecipientEmail = Nothing
          , owrActorPartyId = Nothing
          , owrBody = "Gracias por escribirnos"
          , owrSource = Just "history_spec"
          , owrReplyToMessageId = Just incomingKey
          , owrReplyToExternalId = Just "incoming-target"
          , owrResendOfMessageId = Nothing
          , owrMetadata = Nothing
          }
          incomingReplySendResult
        _ <- recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
          { owrRecipientPhone = "+593991234567"
          , owrRecipientPartyId = Nothing
          , owrRecipientName = Just "Ada"
          , owrRecipientEmail = Nothing
          , owrActorPartyId = Nothing
          , owrBody = "Seguimiento interno"
          , owrSource = Just "history_spec"
          , owrReplyToMessageId = Just outgoingKey
          , owrReplyToExternalId = Just "outgoing-target"
          , owrResendOfMessageId = Nothing
          , owrMetadata = Nothing
          }
          outgoingReplySendResult
        (,) <$> get incomingKey <*> get outgoingKey

      case mIncomingTarget of
        Nothing ->
          expectationFailure "Expected seeded incoming reply target to remain readable"
        Just incomingTarget -> do
          ME.whatsAppMessageRepliedAt incomingTarget `shouldSatisfy` isJust
          ME.whatsAppMessageReplyText incomingTarget `shouldBe` Just "Gracias por escribirnos"
          ME.whatsAppMessageReplyStatus incomingTarget `shouldBe` "sent"

      case mOutgoingTarget of
        Nothing ->
          expectationFailure "Expected seeded outgoing reply target to remain readable"
        Just outgoingTarget -> do
          ME.whatsAppMessageRepliedAt outgoingTarget `shouldBe` Nothing
          ME.whatsAppMessageReplyText outgoingTarget `shouldBe` Nothing
          ME.whatsAppMessageReplyStatus outgoingTarget `shouldBe` "sent"

runWhatsAppHistorySql :: SqlPersistT IO a -> IO a
runWhatsAppHistorySql action =
  runSqlite ":memory:" $ do
    initializeWhatsAppHistorySchema
    backend <- ask
    liftIO (runReaderT action backend)

initializeWhatsAppHistorySchema :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
initializeWhatsAppHistorySchema = do
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
    "CREATE TABLE IF NOT EXISTS \"whats_app_message\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"external_id\" VARCHAR NOT NULL,\
    \\"sender_id\" VARCHAR NOT NULL,\
    \\"sender_name\" VARCHAR NULL,\
    \\"party_id\" INTEGER NULL,\
    \\"actor_party_id\" INTEGER NULL,\
    \\"phone_e164\" VARCHAR NULL,\
    \\"contact_email\" VARCHAR NULL,\
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
    \\"delivery_status\" VARCHAR NOT NULL,\
    \\"delivery_updated_at\" TIMESTAMP NULL,\
    \\"delivery_error\" VARCHAR NULL,\
    \\"transport_payload\" VARCHAR NULL,\
    \\"status_payload\" VARCHAR NULL,\
    \\"source\" VARCHAR NULL,\
    \\"resend_of_message_id\" INTEGER NULL,\
    \\"created_at\" TIMESTAMP NOT NULL,\
    \CONSTRAINT \"unique_whats_app_message\" UNIQUE (\"external_id\")\
    \)"
    []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"audit_log\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"actor_id\" INTEGER NULL,\
    \\"entity\" VARCHAR NOT NULL,\
    \\"entity_id\" VARCHAR NOT NULL,\
    \\"action\" VARCHAR NOT NULL,\
    \\"diff\" VARCHAR NULL,\
    \\"created_at\" TIMESTAMP NOT NULL\
    \)"
    []

seedWhatsAppMessage :: UTCTime -> Text -> Text -> ME.WhatsAppMessage
seedWhatsAppMessage now externalId direction =
  ME.WhatsAppMessage
    { ME.whatsAppMessageExternalId = externalId
    , ME.whatsAppMessageSenderId = "+593991234567"
    , ME.whatsAppMessageSenderName = Just "Ada"
    , ME.whatsAppMessagePartyId = Nothing
    , ME.whatsAppMessageActorPartyId = Nothing
    , ME.whatsAppMessagePhoneE164 = Just "+593991234567"
    , ME.whatsAppMessageContactEmail = Nothing
    , ME.whatsAppMessageText = Just "Original message"
    , ME.whatsAppMessageDirection = direction
    , ME.whatsAppMessageAdExternalId = Nothing
    , ME.whatsAppMessageAdName = Nothing
    , ME.whatsAppMessageCampaignExternalId = Nothing
    , ME.whatsAppMessageCampaignName = Nothing
    , ME.whatsAppMessageMetadata = Nothing
    , ME.whatsAppMessageReplyStatus = if direction == "incoming" then "pending" else "sent"
    , ME.whatsAppMessageHoldReason = Nothing
    , ME.whatsAppMessageHoldRequiredFields = Nothing
    , ME.whatsAppMessageLastAttemptAt = Nothing
    , ME.whatsAppMessageAttemptCount = 0
    , ME.whatsAppMessageRepliedAt = Nothing
    , ME.whatsAppMessageReplyText = Nothing
    , ME.whatsAppMessageReplyError = Nothing
    , ME.whatsAppMessageDeliveryStatus = if direction == "incoming" then "received" else "sent"
    , ME.whatsAppMessageDeliveryUpdatedAt = Nothing
    , ME.whatsAppMessageDeliveryError = Nothing
    , ME.whatsAppMessageTransportPayload = Nothing
    , ME.whatsAppMessageStatusPayload = Nothing
    , ME.whatsAppMessageSource = Just "history_spec_seed"
    , ME.whatsAppMessageResendOfMessageId = Nothing
    , ME.whatsAppMessageCreatedAt = now
    }
