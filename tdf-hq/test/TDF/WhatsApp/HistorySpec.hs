{-# LANGUAGE OverloadedStrings #-}

module TDF.WhatsApp.HistorySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson (object)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist (entityKey, entityVal, get, insert)
import Database.Persist.Sql (SqlPersistT, rawExecute)
import Database.Persist.Sqlite (runSqlite)
import Test.Hspec

import qualified TDF.ModelsExtra as ME
import TDF.WhatsApp.Client
  ( SendTextResult (..)
  , normalizeGraphApiVersion
  , normalizeWhatsAppAccessToken
  , normalizeWhatsAppMessageBody
  , normalizeWhatsAppPhoneNumberId
  , normalizeWhatsAppRecipientPhone
  )
import TDF.WhatsApp.History
  ( IncomingWhatsAppRecord (..)
  , OutgoingWhatsAppRecord (..)
  , normalizeWhatsAppPhone
  , recordIncomingWhatsAppMessage
  , recordOutgoingWhatsAppMessage
  )

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

  describe "TDF.WhatsApp.Client.normalizeGraphApiVersion" $ do
    it "defaults blank versions and canonicalizes supported Graph API versions" $ do
      normalizeGraphApiVersion "   " `shouldBe` Right "v20.0"
      normalizeGraphApiVersion " V20.0 " `shouldBe` Right "v20.0"
      normalizeGraphApiVersion "v21" `shouldBe` Right "v21"

    it "rejects path, query, or label values before WhatsApp URL construction" $ do
      let assertInvalid rawVersion =
            case normalizeGraphApiVersion rawVersion of
              Left err ->
                err `shouldContain` "WhatsApp Graph API version"
              Right version ->
                expectationFailure
                  ("Expected invalid Graph API version to be rejected, got " <> T.unpack version)
      assertInvalid "v20.0/messages"
      assertInvalid "v20.0?fields=id"
      assertInvalid "latest"
      assertInvalid "v20 beta"
      assertInvalid "v0"
      assertInvalid "v00.0"
      assertInvalid "v21.00"

  describe "TDF.WhatsApp.Client provider credential normalization" $ do
    it "trims send credentials before request construction" $ do
      normalizeWhatsAppAccessToken " token_123 " `shouldBe` Right "token_123"
      normalizeWhatsAppPhoneNumberId " 1234567890 " `shouldBe` Right "1234567890"

    it "rejects blank, header-shaped, or path-shaped send credentials" $ do
      normalizeWhatsAppAccessToken "   "
        `shouldBe` Left "Invalid WhatsApp access token: token is required"
      normalizeWhatsAppAccessToken "token value"
        `shouldBe` Left "Invalid WhatsApp access token: must not contain whitespace or control characters"
      normalizeWhatsAppAccessToken "token\nX-Extra: value"
        `shouldBe` Left "Invalid WhatsApp access token: must not contain whitespace or control characters"
      normalizeWhatsAppPhoneNumberId "   "
        `shouldBe` Left "Invalid WhatsApp phone number id: id is required"
      normalizeWhatsAppPhoneNumberId "123/messages"
        `shouldBe` Left "Invalid WhatsApp phone number id: expected digits only"
      normalizeWhatsAppPhoneNumberId "123?fields=id"
        `shouldBe` Left "Invalid WhatsApp phone number id: expected digits only"

  describe "TDF.WhatsApp.Client outbound payload normalization" $ do
    it "normalizes recipient phones and message bodies before Graph request construction" $ do
      normalizeWhatsAppRecipientPhone " +593 99 123 4567 "
        `shouldBe` Right "+593991234567"
      normalizeWhatsAppRecipientPhone "(02) 555-0123"
        `shouldBe` Right "+025550123"
      normalizeWhatsAppMessageBody "  Hola\nSeguimos por aqui.  "
        `shouldBe` Right "Hola\nSeguimos por aqui."

    it "rejects malformed recipients and text bodies before provider fallback handling" $ do
      let recipientShapeMessage =
            "Invalid WhatsApp recipient phone: expected 8-15 digits with optional "
              <> "leading + and phone separators"
          bodyControlMessage =
            "Invalid WhatsApp message body: message must not contain "
              <> "unsupported control characters"
      normalizeWhatsAppRecipientPhone "   "
        `shouldBe` Left "Invalid WhatsApp recipient phone: phone is required"
      normalizeWhatsAppRecipientPhone "call me at 099 123 4567"
        `shouldBe` Left recipientShapeMessage
      normalizeWhatsAppRecipientPhone "12345"
        `shouldBe` Left recipientShapeMessage
      normalizeWhatsAppRecipientPhone "+1234567890123456"
        `shouldBe` Left recipientShapeMessage
      normalizeWhatsAppRecipientPhone "593+991234567"
        `shouldBe` Left recipientShapeMessage
      normalizeWhatsAppMessageBody "   "
        `shouldBe` Left "Invalid WhatsApp message body: message is required"
      normalizeWhatsAppMessageBody (T.replicate 4097 "a")
        `shouldBe` Left "Invalid WhatsApp message body: message must be 4096 characters or fewer"
      normalizeWhatsAppMessageBody ("hola" <> T.singleton '\NUL')
        `shouldBe` Left bodyControlMessage

  describe "recordIncomingWhatsAppMessage" $ do
    it "does not overwrite immutable inbound content on duplicate webhook delivery" $ do
      let now = UTCTime (fromGregorian 2026 4 12) (secondsToDiffTime 0)
          incoming body adName metadata payload = IncomingWhatsAppRecord
            { iwrExternalId = "wamid.duplicate"
            , iwrSenderId = "+593991234567"
            , iwrSenderName = Just "Ada"
            , iwrText = body
            , iwrAdExternalId = Just "ad-1"
            , iwrAdName = Just adName
            , iwrCampaignExternalId = Nothing
            , iwrCampaignName = Nothing
            , iwrMetadata = Just metadata
            , iwrTransportPayload = Just payload
            , iwrSource = Just "history_spec"
            }
      (firstKey, secondKey, storedText, storedAdName, storedMetadata, storedPayload) <-
        runWhatsAppHistorySql $ do
          first <- recordIncomingWhatsAppMessage now
            (incoming
              "Original inbound body"
              "Original ad"
              "original-metadata"
              "original-payload")
          second <- recordIncomingWhatsAppMessage now
            (incoming
              "Mutated inbound body"
              "Mutated ad"
              "mutated-metadata"
              "mutated-payload")
          let stored = entityVal second
          pure
            ( entityKey first
            , entityKey second
            , ME.whatsAppMessageText stored
            , ME.whatsAppMessageAdName stored
            , ME.whatsAppMessageMetadata stored
            , ME.whatsAppMessageTransportPayload stored
            )

      secondKey `shouldBe` firstKey
      storedText `shouldBe` Just "Original inbound body"
      storedAdName `shouldBe` Just "Original ad"
      storedMetadata `shouldBe` Just "original-metadata"
      storedPayload `shouldBe` Just "original-payload"

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
