{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.WhatsApp.History
  ( WhatsAppContactSnapshot(..)
  , IncomingWhatsAppRecord(..)
  , OutgoingWhatsAppRecord(..)
  , WhatsAppDeliveryUpdate(..)
  , normalizeWhatsAppPhone
  , phoneLookupAliases
  , cleanMaybeText
  , resolveWhatsAppContactSnapshot
  , recordIncomingWhatsAppMessage
  , recordOutgoingWhatsAppMessage
  , applyWhatsAppDeliveryUpdate
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isDigit, isSpace)
import           Data.List (nub)
import           Data.Maybe (catMaybes, isJust, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime)
import           Database.Persist
import           Database.Persist.Sql (SqlPersistT, fromSqlKey)

import           TDF.Models
import qualified TDF.ModelsExtra as ME
import           TDF.WhatsApp.Client (SendTextResult(..))

-- Invariants:
-- 1. Every inbound/outbound WhatsApp attempt creates exactly one whatsapp_message row.
-- 2. Message content is immutable after insert; only delivery and reply summaries evolve.
-- 3. Every send/status transition writes an audit_log row to preserve the operator trail.

data WhatsAppContactSnapshot = WhatsAppContactSnapshot
  { wcsPartyId     :: Maybe PartyId
  , wcsDisplayName :: Maybe Text
  , wcsEmail       :: Maybe Text
  , wcsPhoneE164   :: Maybe Text
  } deriving (Show)

data IncomingWhatsAppRecord = IncomingWhatsAppRecord
  { iwrExternalId         :: Text
  , iwrSenderId           :: Text
  , iwrSenderName         :: Maybe Text
  , iwrText               :: Text
  , iwrAdExternalId       :: Maybe Text
  , iwrAdName             :: Maybe Text
  , iwrCampaignExternalId :: Maybe Text
  , iwrCampaignName       :: Maybe Text
  , iwrMetadata           :: Maybe Text
  , iwrTransportPayload   :: Maybe Text
  , iwrSource             :: Maybe Text
  } deriving (Show)

data OutgoingWhatsAppRecord = OutgoingWhatsAppRecord
  { owrRecipientPhone    :: Text
  , owrRecipientPartyId  :: Maybe PartyId
  , owrRecipientName     :: Maybe Text
  , owrRecipientEmail    :: Maybe Text
  , owrActorPartyId      :: Maybe PartyId
  , owrBody              :: Text
  , owrSource            :: Maybe Text
  , owrReplyToMessageId  :: Maybe (Key ME.WhatsAppMessage)
  , owrReplyToExternalId :: Maybe Text
  , owrResendOfMessageId :: Maybe (Key ME.WhatsAppMessage)
  , owrMetadata          :: Maybe Text
  } deriving (Show)

data WhatsAppDeliveryUpdate = WhatsAppDeliveryUpdate
  { wduExternalId    :: Text
  , wduStatus        :: Text
  , wduRecipientId   :: Maybe Text
  , wduOccurredAt    :: Maybe UTCTime
  , wduDeliveryError :: Maybe Text
  , wduStatusPayload :: Maybe Text
  } deriving (Show)

normalizeWhatsAppPhone :: Text -> Maybe Text
normalizeWhatsAppPhone raw =
  let trimmed = T.filter (not . isSpace) (T.strip raw)
      digits = T.filter (\c -> isDigit c || c == '+') trimmed
      withoutPlus = T.dropWhile (== '+') digits
      onlyDigits = T.filter isDigit withoutPlus
  in if T.null onlyDigits then Nothing else Just ("+" <> onlyDigits)

resolveWhatsAppContactSnapshot
  :: Maybe PartyId
  -> Maybe Text
  -> SqlPersistT IO WhatsAppContactSnapshot
resolveWhatsAppContactSnapshot mPartyId mPhone = do
  let normalizedPhone = mPhone >>= normalizeWhatsAppPhone
  mParty <- case mPartyId of
    Just partyId -> getEntity partyId
    Nothing ->
      case normalizedPhone of
        Nothing -> pure Nothing
        Just phoneVal ->
          let aliases = phoneLookupAliases phoneVal
          in if null aliases
               then pure Nothing
                 else selectFirst
                 ([PartyWhatsapp <-. map Just aliases] ||. [PartyPrimaryPhone <-. map Just aliases])
                 [Asc PartyId]
  let displayNameVal =
        case mParty of
          Nothing -> Nothing
          Just ent -> cleanMaybeText (Just (partyDisplayName (entityVal ent)))
      emailVal =
        case mParty of
          Nothing -> Nothing
          Just ent -> cleanMaybeText (partyPrimaryEmail (entityVal ent))
      storedPhone =
        case mParty of
          Nothing -> Nothing
          Just ent ->
            normalizeWhatsAppPhone =<<
              (cleanMaybeText (partyWhatsapp (entityVal ent))
                <|> cleanMaybeText (partyPrimaryPhone (entityVal ent)))
  pure WhatsAppContactSnapshot
    { wcsPartyId = entityKey <$> mParty
    , wcsDisplayName = displayNameVal
    , wcsEmail = emailVal
    , wcsPhoneE164 = normalizedPhone <|> storedPhone
    }

recordIncomingWhatsAppMessage
  :: UTCTime
  -> IncomingWhatsAppRecord
  -> SqlPersistT IO (Entity ME.WhatsAppMessage)
recordIncomingWhatsAppMessage now IncomingWhatsAppRecord{..} = do
  snapshot <- resolveWhatsAppContactSnapshot Nothing (Just iwrSenderId)
  let externalId = nonEmptyOr ("incoming-" <> T.pack (show now)) iwrExternalId
      senderId = nonEmptyOr "unknown" iwrSenderId
      senderName = cleanMaybeText iwrSenderName <|> wcsDisplayName snapshot
      bodyVal = cleanMaybeText (Just iwrText)
      metadataVal = cleanMaybeText iwrMetadata
      payloadVal = cleanMaybeText iwrTransportPayload
      sourceVal = cleanMaybeText iwrSource <|> Just "webhook_inbound"
  mExisting <- getBy (ME.UniqueWhatsAppMessage externalId)
  case mExisting of
    Just (Entity key existing) -> do
      let updates = catMaybes
            [ if isBlankMaybe (ME.whatsAppMessageSenderName existing)
                then setOptionalFieldUpdate ME.WhatsAppMessageSenderName senderName
                else Nothing
            , if isBlankMaybe (ME.whatsAppMessagePhoneE164 existing)
                then setOptionalFieldUpdate ME.WhatsAppMessagePhoneE164 (wcsPhoneE164 snapshot)
                else Nothing
            , if isBlankMaybe (ME.whatsAppMessageContactEmail existing)
                then setOptionalFieldUpdate ME.WhatsAppMessageContactEmail (wcsEmail snapshot)
                else Nothing
            , if isNothingMaybe (ME.whatsAppMessagePartyId existing)
                then setOptionalFieldUpdate ME.WhatsAppMessagePartyId (wcsPartyId snapshot)
                else Nothing
            , setOptionalFieldUpdate ME.WhatsAppMessageText bodyVal
            , setOptionalFieldUpdate ME.WhatsAppMessageAdExternalId (cleanMaybeText iwrAdExternalId)
            , setOptionalFieldUpdate ME.WhatsAppMessageAdName (cleanMaybeText iwrAdName)
            , setOptionalFieldUpdate ME.WhatsAppMessageCampaignExternalId (cleanMaybeText iwrCampaignExternalId)
            , setOptionalFieldUpdate ME.WhatsAppMessageCampaignName (cleanMaybeText iwrCampaignName)
            , setOptionalFieldUpdate ME.WhatsAppMessageMetadata metadataVal
            , setOptionalFieldUpdate ME.WhatsAppMessageTransportPayload payloadVal
            , setOptionalFieldUpdate ME.WhatsAppMessageSource sourceVal
            ]
      if null updates
        then pure ()
        else update key updates
      getJustEntity key
    Nothing -> do
      key <- insert ME.WhatsAppMessage
        { ME.whatsAppMessageExternalId = externalId
        , ME.whatsAppMessageSenderId = senderId
        , ME.whatsAppMessageSenderName = senderName
        , ME.whatsAppMessagePartyId = wcsPartyId snapshot
        , ME.whatsAppMessageActorPartyId = Nothing
        , ME.whatsAppMessagePhoneE164 = wcsPhoneE164 snapshot
        , ME.whatsAppMessageContactEmail = wcsEmail snapshot
        , ME.whatsAppMessageText = bodyVal
        , ME.whatsAppMessageDirection = "incoming"
        , ME.whatsAppMessageAdExternalId = cleanMaybeText iwrAdExternalId
        , ME.whatsAppMessageAdName = cleanMaybeText iwrAdName
        , ME.whatsAppMessageCampaignExternalId = cleanMaybeText iwrCampaignExternalId
        , ME.whatsAppMessageCampaignName = cleanMaybeText iwrCampaignName
        , ME.whatsAppMessageMetadata = metadataVal
        , ME.whatsAppMessageReplyStatus = "pending"
        , ME.whatsAppMessageHoldReason = Nothing
        , ME.whatsAppMessageHoldRequiredFields = Nothing
        , ME.whatsAppMessageLastAttemptAt = Nothing
        , ME.whatsAppMessageAttemptCount = 0
        , ME.whatsAppMessageRepliedAt = Nothing
        , ME.whatsAppMessageReplyText = Nothing
        , ME.whatsAppMessageReplyError = Nothing
        , ME.whatsAppMessageDeliveryStatus = "received"
        , ME.whatsAppMessageDeliveryUpdatedAt = Just now
        , ME.whatsAppMessageDeliveryError = Nothing
        , ME.whatsAppMessageTransportPayload = payloadVal
        , ME.whatsAppMessageStatusPayload = Nothing
        , ME.whatsAppMessageSource = sourceVal
        , ME.whatsAppMessageResendOfMessageId = Nothing
        , ME.whatsAppMessageCreatedAt = now
        }
      entity <- getJustEntity key
      writeAuditEntry now Nothing "whatsapp_message" key "received" $
        object
          [ "direction" .= ("incoming" :: Text)
          , "phoneE164" .= wcsPhoneE164 snapshot
          , "source" .= sourceVal
          ]
      pure entity

recordOutgoingWhatsAppMessage
  :: UTCTime
  -> OutgoingWhatsAppRecord
  -> Either Text SendTextResult
  -> SqlPersistT IO (Entity ME.WhatsAppMessage)
recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord{..} sendResult = do
  snapshot <- resolveWhatsAppContactSnapshot owrRecipientPartyId (Just owrRecipientPhone)
  let recipientPhone = fromMaybe (nonEmptyOr "unknown" owrRecipientPhone) (normalizeWhatsAppPhone owrRecipientPhone <|> wcsPhoneE164 snapshot)
      safeBase = if T.null (T.strip recipientPhone) then "unknown" else T.strip recipientPhone
      externalId = case sendResult of
        Left _ -> safeBase <> "-out-" <> T.pack (show now)
        Right resp -> fromMaybe (safeBase <> "-out-" <> T.pack (show now)) (sendTextMessageId resp)
      isFailure =
        case sendResult of
          Left _ -> True
          Right _ -> False
      deliveryStatus = if isFailure then "failed" else "sent"
      deliveryError =
        case sendResult of
          Left err -> cleanMaybeText (Just err)
          Right _ -> Nothing
      transportPayload =
        case sendResult of
          Left _ -> Nothing
          Right resp -> Just (encodeValueText (sendTextPayload resp))
      senderName = cleanMaybeText owrRecipientName <|> wcsDisplayName snapshot
      contactEmail = cleanMaybeText owrRecipientEmail <|> wcsEmail snapshot
      partyId = owrRecipientPartyId <|> wcsPartyId snapshot
      sourceVal = cleanMaybeText owrSource
      bodyVal = cleanMaybeText (Just owrBody)
      hasReplyTarget = isJust owrReplyToMessageId || isJust (cleanMaybeText owrReplyToExternalId)
      messageKind
        | isJust owrResendOfMessageId = "resend"
        | hasReplyTarget = "reply"
        | otherwise = "notify"
      auditAction = messageKind <> if isFailure then "_failed" else "_sent"
  key <- insert ME.WhatsAppMessage
    { ME.whatsAppMessageExternalId = externalId
    , ME.whatsAppMessageSenderId = recipientPhone
    , ME.whatsAppMessageSenderName = senderName
    , ME.whatsAppMessagePartyId = partyId
    , ME.whatsAppMessageActorPartyId = owrActorPartyId
    , ME.whatsAppMessagePhoneE164 = Just recipientPhone
    , ME.whatsAppMessageContactEmail = contactEmail
    , ME.whatsAppMessageText = bodyVal
    , ME.whatsAppMessageDirection = "outgoing"
    , ME.whatsAppMessageAdExternalId = Nothing
    , ME.whatsAppMessageAdName = Nothing
    , ME.whatsAppMessageCampaignExternalId = Nothing
    , ME.whatsAppMessageCampaignName = Nothing
    , ME.whatsAppMessageMetadata = cleanMaybeText owrMetadata
    , ME.whatsAppMessageReplyStatus = if isFailure then "error" else "sent"
    , ME.whatsAppMessageHoldReason = Nothing
    , ME.whatsAppMessageHoldRequiredFields = Nothing
    , ME.whatsAppMessageLastAttemptAt = Just now
    , ME.whatsAppMessageAttemptCount = 1
    , ME.whatsAppMessageRepliedAt = Nothing
    , ME.whatsAppMessageReplyText = Nothing
    , ME.whatsAppMessageReplyError = Nothing
    , ME.whatsAppMessageDeliveryStatus = deliveryStatus
    , ME.whatsAppMessageDeliveryUpdatedAt = Just now
    , ME.whatsAppMessageDeliveryError = deliveryError
    , ME.whatsAppMessageTransportPayload = transportPayload
    , ME.whatsAppMessageStatusPayload = Nothing
    , ME.whatsAppMessageSource = sourceVal
    , ME.whatsAppMessageResendOfMessageId = owrResendOfMessageId
    , ME.whatsAppMessageCreatedAt = now
    }
  applyReplyOutcome now bodyVal sendResult owrReplyToMessageId (cleanMaybeText owrReplyToExternalId)
  entity <- getJustEntity key
  writeAuditEntry now owrActorPartyId "whatsapp_message" key auditAction $
    object
      [ "direction" .= ("outgoing" :: Text)
      , "source" .= sourceVal
      , "deliveryStatus" .= deliveryStatus
      , "phoneE164" .= recipientPhone
      , "replyToMessageId" .= fmap (T.pack . show . fromSqlKey) owrReplyToMessageId
      , "resendOfMessageId" .= fmap (T.pack . show . fromSqlKey) owrResendOfMessageId
      ]
  pure entity

applyWhatsAppDeliveryUpdate
  :: UTCTime
  -> WhatsAppDeliveryUpdate
  -> SqlPersistT IO (Maybe (Entity ME.WhatsAppMessage))
applyWhatsAppDeliveryUpdate now WhatsAppDeliveryUpdate{..} = do
  let externalId = nonEmptyOr "" wduExternalId
      statusVal = nonEmptyOr "unknown" wduStatus
      updateTs = wduOccurredAt <|> Just now
  if T.null externalId
    then pure Nothing
    else do
      mRow <- getBy (ME.UniqueWhatsAppMessage externalId)
      case mRow of
        Nothing -> pure Nothing
        Just (Entity key row) -> do
          snapshot <- resolveWhatsAppContactSnapshot (ME.whatsAppMessagePartyId row) wduRecipientId
          let recipientPhone = (wduRecipientId >>= normalizeWhatsAppPhone) <|> wcsPhoneE164 snapshot
              updates = catMaybes
                [ Just (ME.WhatsAppMessageDeliveryStatus =. statusVal)
                , Just (ME.WhatsAppMessageDeliveryUpdatedAt =. updateTs)
                , Just (ME.WhatsAppMessageDeliveryError =. cleanMaybeText wduDeliveryError)
                , setOptionalFieldUpdate ME.WhatsAppMessageStatusPayload (cleanMaybeText wduStatusPayload)
                , if isNothingMaybe (ME.whatsAppMessagePartyId row)
                    then setOptionalFieldUpdate ME.WhatsAppMessagePartyId (wcsPartyId snapshot)
                    else Nothing
                , if isBlankMaybe (ME.whatsAppMessagePhoneE164 row)
                    then setOptionalFieldUpdate ME.WhatsAppMessagePhoneE164 recipientPhone
                    else Nothing
                , if isBlankMaybe (ME.whatsAppMessageContactEmail row)
                    then setOptionalFieldUpdate ME.WhatsAppMessageContactEmail (wcsEmail snapshot)
                    else Nothing
                , if isBlankMaybe (ME.whatsAppMessageSenderName row)
                    then setOptionalFieldUpdate ME.WhatsAppMessageSenderName (wcsDisplayName snapshot)
                    else Nothing
                ]
          update key updates
          entity <- getJustEntity key
          writeAuditEntry now Nothing "whatsapp_message" key "delivery_status_updated" $
            object
              [ "deliveryStatus" .= statusVal
              , "deliveryUpdatedAt" .= updateTs
              , "deliveryError" .= cleanMaybeText wduDeliveryError
              ]
          pure (Just entity)

applyReplyOutcome
  :: UTCTime
  -> Maybe Text
  -> Either Text SendTextResult
  -> Maybe (Key ME.WhatsAppMessage)
  -> Maybe Text
  -> SqlPersistT IO ()
applyReplyOutcome now bodyVal sendResult mReplyToKey mReplyToExternalId =
  case mReplyToKey of
    Just replyKey -> update replyKey (replyUpdates sendResult bodyVal now)
    Nothing ->
      case mReplyToExternalId of
        Nothing -> pure ()
        Just extId ->
          updateWhere
            [ ME.WhatsAppMessageExternalId ==. extId
            , ME.WhatsAppMessageDirection ==. "incoming"
            ]
            (replyUpdates sendResult bodyVal now)

replyUpdates
  :: Either Text SendTextResult
  -> Maybe Text
  -> UTCTime
  -> [Update ME.WhatsAppMessage]
replyUpdates sendResult bodyVal now =
  case sendResult of
    Left err ->
      [ ME.WhatsAppMessageReplyStatus =. "error"
      , ME.WhatsAppMessageReplyError =. cleanMaybeText (Just err)
      , ME.WhatsAppMessageLastAttemptAt =. Just now
      ]
    Right _ ->
      [ ME.WhatsAppMessageReplyStatus =. "sent"
      , ME.WhatsAppMessageRepliedAt =. Just now
      , ME.WhatsAppMessageReplyText =. bodyVal
      , ME.WhatsAppMessageReplyError =. Nothing
      , ME.WhatsAppMessageLastAttemptAt =. Just now
      ]

writeAuditEntry
  :: UTCTime
  -> Maybe PartyId
  -> Text
  -> Key ME.WhatsAppMessage
  -> Text
  -> Value
  -> SqlPersistT IO ()
writeAuditEntry now actorId entity key action diffValue =
  insert_ AuditLog
    { auditLogActorId = actorId
    , auditLogEntity = entity
    , auditLogEntityId = T.pack (show (fromSqlKey key))
    , auditLogAction = action
    , auditLogDiff = Just (encodeValueText diffValue)
    , auditLogCreatedAt = now
    }

phoneLookupAliases :: Text -> [Text]
phoneLookupAliases phoneVal =
  nub
    [ phoneVal
    , T.dropWhile (== '+') phoneVal
    ]

cleanMaybeText :: Maybe Text -> Maybe Text
cleanMaybeText Nothing = Nothing
cleanMaybeText (Just raw) =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

setOptionalFieldUpdate :: PersistField typ => EntityField record (Maybe typ) -> Maybe typ -> Maybe (Update record)
setOptionalFieldUpdate field = fmap (\value -> field =. Just value)

encodeValueText :: Value -> Text
encodeValueText = TE.decodeUtf8 . BL.toStrict . encode

isBlankMaybe :: Maybe Text -> Bool
isBlankMaybe = maybe True T.null . cleanMaybeText

isNothingMaybe :: Maybe a -> Bool
isNothingMaybe Nothing = True
isNothingMaybe (Just _) = False

nonEmptyOr :: Text -> Text -> Text
nonEmptyOr fallback raw =
  case cleanMaybeText (Just raw) of
    Just value -> value
    Nothing -> fallback
