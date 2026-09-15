{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Commerce.ProviderEventStore
  ( ProviderEventCreation(..)
  , ProviderEventReference(..)
  , ProviderEventStored(..)
  , ProviderEventClaim(..)
  , ProviderEventRecord(..)
  , ProviderEventPayload(..)
  , ProviderEventReplayError(..)
  , parseProviderEventReference
  , storeVerifiedProviderEvent
  , storeUntrustedProviderEvent
  , listProviderEvents
  , listDueProviderEventReferences
  , providerEventStaleBefore
  , loadProviderEventPayload
  , requeueDeadLetterProviderEvent
  , claimProviderEvent
  , markProviderEventProcessed
  , markProviderEventIgnored
  , markProviderEventRetry
  , markProviderEventDeadLetter
  , validateProviderEventTimestamp
  , minimizeProviderEventPayload
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Crypto.Hash (Digest, SHA256, hash)
import           Data.Aeson (FromJSON, Value(..), eitherDecodeStrict', (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteArray.Encoding as BAE
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import           Data.UUID (fromText, toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql)
import           GHC.Generics (Generic)

import           TDF.Commerce.CheckoutStore
  ( CheckoutEnvironment
  , PaymentProvider(..)
  , checkoutEnvironmentText
  , paymentProviderText
  )

newtype ProviderEventReference = ProviderEventReference
  { providerEventReferenceId :: Text
  } deriving (Eq, Show)

data ProviderEventCreation = ProviderEventCreation
  { pecProvider          :: PaymentProvider
  , pecEnvironment       :: CheckoutEnvironment
  , pecMerchantRef       :: Text
  , pecProviderEventId   :: Text
  , pecEventType         :: Text
  , pecProviderCreatedAt :: Maybe UTCTime
  , pecProviderResource  :: Maybe Text
  , pecRawPayload        :: ByteString
  , pecEncryptionKey     :: Text
  , pecReceivedAt        :: UTCTime
  }

data ProviderEventStored = ProviderEventStored
  { pesReference :: ProviderEventReference
  , pesInserted  :: Bool
  } deriving (Eq, Show)

data ProviderEventClaim
  = ProviderEventClaimed Int
  | ProviderEventAlreadyHandled Text
  | ProviderEventBusy
  deriving (Eq, Show)

data ProviderEventRecord = ProviderEventRecord
  { perId                  :: Text
  , perProvider            :: Text
  , perEnvironment         :: Text
  , perProviderEventId     :: Text
  , perEventType           :: Text
  , perEvidenceType        :: Text
  , perProviderResourceId  :: Maybe Text
  , perStatus              :: Text
  , perAttemptCount        :: Int
  , perCheckoutId          :: Maybe Text
  , perPaymentAttemptId    :: Maybe Text
  , perRefundId            :: Maybe Text
  , perReceivedAt          :: UTCTime
  , perProviderCreatedAt   :: Maybe UTCTime
  , perProcessingStartedAt :: Maybe UTCTime
  , perLastAttemptAt       :: Maybe UTCTime
  , perNextAttemptAt       :: Maybe UTCTime
  , perProcessedAt         :: Maybe UTCTime
  , perErrorSummary        :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON ProviderEventRecord

data ProviderEventPayload = ProviderEventPayload
  { pepReference          :: ProviderEventReference
  , pepProvider           :: Text
  , pepEnvironment        :: Text
  , pepMerchantRef        :: Text
  , pepProviderEventId    :: Text
  , pepEventType          :: Text
  , pepEvidenceType       :: Text
  , pepSignatureVerified  :: Bool
  , pepProviderCreatedAt  :: Maybe UTCTime
  , pepProviderResourceId :: Maybe Text
  , pepRawPayload         :: ByteString
  } deriving (Eq)

-- Never let an incidental debug/show call reveal a decrypted historical body.
instance Show ProviderEventPayload where
  show _ = "ProviderEventPayload {payload = <redacted>}"

data ProviderEventPayloadMetadata = ProviderEventPayloadMetadata
  { ppmId                  :: Text
  , ppmProvider            :: Text
  , ppmEnvironment         :: Text
  , ppmMerchantRef         :: Text
  , ppmProviderEventId     :: Text
  , ppmEventType           :: Text
  , ppmEvidenceType        :: Text
  , ppmSignatureVerified   :: Bool
  , ppmProviderCreatedAt   :: Maybe UTCTime
  , ppmProviderResourceId  :: Maybe Text
  , ppmPayloadSha256       :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON ProviderEventPayloadMetadata

data ProviderEventReplayError
  = ProviderEventNotFound
  | ProviderEventReplayConflict Text
  deriving (Eq, Show)

parseProviderEventReference :: Text -> Either Text ProviderEventReference
parseProviderEventReference rawReference =
  case fromText (T.strip rawReference) of
    Nothing -> Left "Provider event ID must be a UUID"
    Just eventId -> Right (ProviderEventReference (toText eventId))

storeVerifiedProviderEvent
  :: ProviderEventCreation
  -> SqlPersistT IO (Either Text ProviderEventStored)
storeVerifiedProviderEvent = storeProviderEvent "signature_verified" True

-- | Persist a callback that has no provider-authentication mechanism. It is a
-- query trigger only: the worker must obtain authoritative status through the
-- provider's authenticated server-to-server API before changing payment or
-- fulfillment state.
storeUntrustedProviderEvent
  :: ProviderEventCreation
  -> SqlPersistT IO (Either Text ProviderEventStored)
storeUntrustedProviderEvent = storeProviderEvent "untrusted_callback" False

storeProviderEvent
  :: Text
  -> Bool
  -> ProviderEventCreation
  -> SqlPersistT IO (Either Text ProviderEventStored)
storeProviderEvent evidenceType signatureVerified ProviderEventCreation{..}
  | not (validEncryptionKey pecEncryptionKey) =
      pure (Left "Provider event encryption key must contain 32 to 256 safe characters")
  | not (validReference 128 pecProviderEventId) =
      pure (Left "Provider event ID is invalid")
  | not (validEventType pecEventType) =
      pure (Left "Provider event type is invalid")
  | not (validReference 256 pecMerchantRef) =
      pure (Left "Provider merchant reference is invalid")
  | maybe False (not . validReference 256) pecProviderResource =
      pure (Left "Provider event resource ID is invalid")
  | BS.null pecRawPayload || BS.length pecRawPayload > maxProviderEventBytes =
      pure (Left "Provider event payload must contain 1 to 1048576 bytes")
  | otherwise = case minimizeProviderEventPayload pecProvider pecRawPayload of
    Left problem -> pure (Left problem)
    Right retainedPayload -> do
      eventId <- liftIO (toText <$> nextRandom)
      let payloadHash = sha256Hex retainedPayload
          legacyPayloadHash = sha256Hex pecRawPayload
      inserted <- (rawSql
        "INSERT INTO commerce_provider_event_inbox (\
        \ id, provider, environment, merchant_account_ref, provider_event_id,\
        \ event_type, signature_verified, evidence_type, received_at, provider_created_at,\
        \ provider_resource_id, payload_ciphertext, payload_sha256, processing_status\
        \) VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,\
        \ pgp_sym_encrypt_bytea(?::bytea, ?, 'cipher-algo=aes256,compress-algo=1'),\
        \ ?, 'pending')\
        \ ON CONFLICT (provider, environment, merchant_account_ref, provider_event_id)\
        \ DO NOTHING RETURNING id::text"
        [ PersistText eventId
        , PersistText (paymentProviderText pecProvider)
        , PersistText (checkoutEnvironmentText pecEnvironment)
        , PersistText pecMerchantRef
        , PersistText pecProviderEventId
        , PersistText pecEventType
        , PersistBool signatureVerified
        , PersistText evidenceType
        , PersistUTCTime pecReceivedAt
        , maybe PersistNull PersistUTCTime pecProviderCreatedAt
        , maybe PersistNull PersistText pecProviderResource
        , PersistByteString retainedPayload
        , PersistText pecEncryptionKey
        , PersistText payloadHash
        ] :: SqlPersistT IO [Single Text])
      case inserted of
        [Single insertedId] ->
          pure (Right (ProviderEventStored (ProviderEventReference insertedId) True))
        [] -> do
          existing <- (rawSql
            "SELECT id::text FROM commerce_provider_event_inbox\
            \ WHERE provider = ? AND environment = ? AND merchant_account_ref = ?\
            \ AND provider_event_id = ? AND event_type = ?\
            \ AND signature_verified = ? AND evidence_type = ?\
            \ AND (payload_sha256 = ? OR payload_sha256 = ?)\
            \ AND provider_resource_id IS NOT DISTINCT FROM ?\
            \ AND provider_created_at IS NOT DISTINCT FROM ?"
            [ PersistText (paymentProviderText pecProvider)
            , PersistText (checkoutEnvironmentText pecEnvironment)
            , PersistText pecMerchantRef
            , PersistText pecProviderEventId
            , PersistText pecEventType
            , PersistBool signatureVerified
            , PersistText evidenceType
            , PersistText payloadHash
            -- Exact redelivery of a pre-minimization event must still find its
            -- original row. Never rewrite that row or relax immutable metadata.
            , PersistText legacyPayloadHash
            , maybe PersistNull PersistText pecProviderResource
            , maybe PersistNull PersistUTCTime pecProviderCreatedAt
            ] :: SqlPersistT IO [Single Text])
          case existing of
            [Single existingId] ->
              pure (Right (ProviderEventStored (ProviderEventReference existingId) False))
            _ -> pure (Left "Provider event ID conflicts with different immutable evidence")
        _ -> pure (Left "Provider event insert returned an ambiguous result")

-- | Authentication always happens against the original body upstream. Only
-- fields used by the current worker may cross the SQL/encryption boundary.
-- Unrecognized card/customer/address/description/URL containers are discarded.
-- Existing inbox rows are intentionally not rewritten by this function.
minimizeProviderEventPayload :: PaymentProvider -> ByteString -> Either Text ByteString
minimizeProviderEventPayload provider rawPayload = do
  unless (not (BS.null rawPayload) && BS.length rawPayload <= maxProviderEventBytes) $
    Left "Provider event payload size is invalid"
  value <- either (const (Left invalidPayload)) Right (eitherDecodeStrict' rawPayload)
  projected <- either (const (Left invalidPayload)) Right $ parseEither parser value
  pure (BL.toStrict (A.encode projected))
  where
    invalidPayload = "Provider event payload does not match the retained evidence schema"
    parser = case provider of
      ProviderPayPal -> paypalEvidence
      ProviderPlaceToPay -> placeToPayEvidence
      ProviderPayPhone -> payPhoneEvidence
      _ -> const (fail "Unsupported provider inbox schema")

paypalEvidence :: Value -> Parser Value
paypalEvidence = A.withObject "PayPal evidence" $ \envelope -> do
  eventId <- envelope .: "id" >>= safeEvidenceText 128
  eventType <- envelope .: "event_type" >>= safeEvidenceText 100
  unless (validReference 128 eventId && validEventType eventType) (fail "Invalid identity")
  createdAt <- envelope .: "create_time" >>= evidenceTimestamp
  resource <- envelope .: "resource"
  retainedResource <- case resource of
    Object fields -> do
      identifier <- optionalEvidenceText fields "id" 128
      if eventType `elem`
          ["PAYMENT.CAPTURE.COMPLETED", "PAYMENT.CAPTURE.REFUNDED", "PAYMENT.CAPTURE.REVERSED"]
        then do
          status <- optionalEvidenceText fields "status" 32
          amount <- optionalEvidenceObject fields "amount" $ \amountFields -> do
            value <- optionalEvidenceText amountFields "value" 32
            currency <- optionalEvidenceText amountFields "currency_code" 3
            pure (value <> currency)
          payee <- optionalEvidenceObject fields "payee" $ \payeeFields ->
            optionalEvidenceText payeeFields "merchant_id" 128
          supplementary <- optionalEvidenceObject fields "supplementary_data" $ \extra ->
            optionalEvidenceObject extra "related_ids" $ \related ->
              optionalEvidenceText related "order_id" 128
          pure (Object (identifier <> status <> amount <> payee <> supplementary))
        else pure (Object identifier)
    _ -> pure (Object KM.empty) -- Unsupported event resources are never interpreted.
  pure $ A.object
    [ "id" .= eventId, "event_type" .= eventType, "create_time" .= createdAt
    , "resource" .= retainedResource ]

placeToPayEvidence :: Value -> Parser Value
placeToPayEvidence = A.withObject "PlaceToPay evidence" $ \envelope -> do
  requestId <- envelope .: "requestId" :: Parser Int64
  unless (requestId > 0) (fail "Invalid request ID")
  statusValue <- envelope .: "status"
  (status, date) <- A.withObject "PlaceToPay status" (\fields -> do
    status <- fields .: "status" >>= safeEvidenceText 32
    date <- fields .: "date" >>= evidenceTimestamp
    pure (status, date)) statusValue
  signature <- envelope .: "signature" >>= safeEvidenceText 71
  unless (T.length signature == 71 && "sha256:" `T.isPrefixOf` T.toLower signature
      && T.all (`elem` ("0123456789abcdef" :: String)) (T.toLower (T.drop 7 signature))) $
    fail "Invalid signature format"
  pure $ A.object
    [ "requestId" .= requestId, "signature" .= signature
    , "status" .= A.object ["status" .= status, "date" .= date] ]

payPhoneEvidence :: Value -> Parser Value
payPhoneEvidence = A.withObject "PayPhone evidence" $ \envelope -> do
  transactionId <- (envelope .: "TransactionId" <|> envelope .: "id") :: Parser Int64
  reference <- (envelope .: "ClientTransactionId" <|> envelope .: "clientTransactionID")
    >>= safeEvidenceText 128
  unless (transactionId > 0 && validReference 128 reference) (fail "Invalid binding")
  store <- optionalEvidenceText envelope "StoreId" 128
  -- Callback status is untrusted and unused: only the bound IDs trigger a query.
  pure $ Object (KM.fromList
    [ ("TransactionId", A.toJSON transactionId), ("ClientTransactionId", String reference) ]
    <> store)

safeEvidenceText :: Int -> Text -> Parser Text
safeEvidenceText maxLength value = do
  unless (not (T.null value) && T.length value <= maxLength
      && T.all (\character -> character >= ' ' && character <= '~') value) $
    fail "Invalid evidence text"
  pure value

evidenceTimestamp :: Text -> Parser Text
evidenceTimestamp value = do
  retained <- safeEvidenceText 64 value
  _ <- A.parseJSON (String (T.strip retained)) :: Parser UTCTime
  pure retained

optionalEvidenceText :: A.Object -> A.Key -> Int -> Parser A.Object
optionalEvidenceText fields key limit = do
  value <- fields .:? key
  case value of
    Nothing -> pure KM.empty
    Just textValue -> KM.singleton key . String <$> safeEvidenceText limit textValue

optionalEvidenceObject
  :: A.Object -> A.Key -> (A.Object -> Parser A.Object) -> Parser A.Object
optionalEvidenceObject fields key project = do
  value <- fields .:? key
  case value of
    Nothing -> pure KM.empty
    Just objectValue -> KM.singleton key . Object <$> A.withObject "Evidence" project objectValue

listProviderEvents
  :: Maybe Text
  -> Int
  -> Int
  -> SqlPersistT IO [ProviderEventRecord]
listProviderEvents statusFilter requestedLimit requestedOffset = do
  rows <- rawSql
    (providerEventRecordSelect <> "\
    \ WHERE (?::text IS NULL OR processing_status = ?::text)\
    \ ORDER BY received_at DESC, id DESC LIMIT ? OFFSET ?")
    [ maybe PersistNull PersistText statusFilter
    , maybe PersistNull PersistText statusFilter
    , PersistInt64 (fromIntegral (min 100 (max 1 requestedLimit)))
    , PersistInt64 (fromIntegral (min 10000 (max 0 requestedOffset)))
    ] :: SqlPersistT IO [Single Text]
  decodeProviderEventRecords rows

listDueProviderEventReferences
  :: UTCTime
  -> Int
  -> SqlPersistT IO [ProviderEventReference]
listDueProviderEventReferences now requestedLimit = do
  let staleBefore = providerEventStaleBefore now
  rows <- rawSql
    "SELECT event.id::text FROM commerce_provider_event_inbox event\
    \ WHERE (event.signature_verified = TRUE\
    \   OR event.evidence_type = 'untrusted_callback')\
    \ AND EXISTS (SELECT 1 FROM revenue_feature_flag flag\
    \   WHERE flag.flag_key = 'checkout.provider_event_worker'\
    \   AND flag.environment = event.environment AND flag.enabled)\
    \ AND (\
    \   event.processing_status = 'pending'\
    \   OR (event.processing_status = 'retry' AND COALESCE(event.next_attempt_at, ?) <= ?)\
    \   OR (event.processing_status = 'processing'\
    \       AND event.processing_started_at < ?)\
    \ ) ORDER BY COALESCE(event.next_attempt_at, event.received_at), event.received_at\
    \ LIMIT ?"
    [ PersistUTCTime now
    , PersistUTCTime now
    , PersistUTCTime staleBefore
    , PersistInt64 (fromIntegral (min 100 (max 1 requestedLimit)))
    ] :: SqlPersistT IO [Single Text]
  pure [ProviderEventReference eventId | Single eventId <- rows]

providerEventStaleBefore :: UTCTime -> UTCTime
providerEventStaleBefore = addUTCTime (negate (15 * 60))

loadProviderEventPayload
  :: ProviderEventReference
  -> Text
  -> SqlPersistT IO (Either Text ProviderEventPayload)
loadProviderEventPayload eventRef encryptionKey
  | not (validEncryptionKey encryptionKey) =
      pure (Left "Provider event encryption key is invalid")
  | otherwise = do
      rows <- rawSql
        "SELECT jsonb_build_object(\
        \ 'ppmId', id::text, 'ppmProvider', provider, 'ppmEnvironment', environment,\
        \ 'ppmMerchantRef', merchant_account_ref,\
        \ 'ppmProviderEventId', provider_event_id, 'ppmEventType', event_type,\
        \ 'ppmEvidenceType', evidence_type,\
        \ 'ppmSignatureVerified', signature_verified,\
        \ 'ppmProviderCreatedAt', provider_created_at,\
        \ 'ppmProviderResourceId', provider_resource_id,\
        \ 'ppmPayloadSha256', payload_sha256)::text,\
        \ pgp_sym_decrypt_bytea(payload_ciphertext, ?)\
        \ FROM commerce_provider_event_inbox\
        \ WHERE id = ?::uuid\
        \ AND processing_status = 'processing'"
        [ PersistText encryptionKey
        , PersistText (providerEventReferenceId eventRef)
        ] :: SqlPersistT IO [(Single Text, Single ByteString)]
      pure $ case rows of
        [(Single metadataJson, Single rawPayload)] -> do
          metadata <- firstText "Invalid provider event metadata" $
            eitherDecodeStrict' (TE.encodeUtf8 metadataJson)
          if ppmPayloadSha256 metadata /= sha256Hex rawPayload
            then Left "Provider event payload checksum mismatch"
            else Right ProviderEventPayload
              { pepReference = ProviderEventReference (ppmId metadata)
              , pepProvider = ppmProvider metadata
              , pepEnvironment = ppmEnvironment metadata
              , pepMerchantRef = ppmMerchantRef metadata
              , pepProviderEventId = ppmProviderEventId metadata
              , pepEventType = ppmEventType metadata
              , pepEvidenceType = ppmEvidenceType metadata
              , pepSignatureVerified = ppmSignatureVerified metadata
              , pepProviderCreatedAt = ppmProviderCreatedAt metadata
              , pepProviderResourceId = ppmProviderResourceId metadata
              , pepRawPayload = rawPayload
              }
        [] -> Left "Provider event is unavailable for processing"
        _ -> Left "Provider event payload lookup returned an ambiguous result"

requeueDeadLetterProviderEvent
  :: ProviderEventReference
  -> Int64
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either ProviderEventReplayError ProviderEventRecord)
requeueDeadLetterProviderEvent eventRef actorPartyId reason now = do
  statuses <- rawSql
    "SELECT processing_status FROM commerce_provider_event_inbox\
    \ WHERE id = ?::uuid FOR UPDATE"
    [PersistText (providerEventReferenceId eventRef)] :: SqlPersistT IO [Single Text]
  case statuses of
    [] -> pure (Left ProviderEventNotFound)
    [Single "dead_letter"] -> do
      requeued <- rawSql
        "SELECT commerce_requeue_provider_event(?::uuid, ?::bigint, ?, ?)::text"
        [ PersistText (providerEventReferenceId eventRef)
        , PersistInt64 actorPartyId
        , PersistText reason
        , PersistUTCTime now
        ] :: SqlPersistT IO [Single Text]
      case requeued of
        [Single _] -> do
          records <- loadProviderEventRecord eventRef
          pure $ case records of
            [record] -> Right record
            _ -> Left ProviderEventNotFound
        _ -> pure (Left (ProviderEventReplayConflict "dead_letter"))
    [Single currentStatus] ->
      pure (Left (ProviderEventReplayConflict currentStatus))
    _ -> pure (Left ProviderEventNotFound)

claimProviderEvent
  :: ProviderEventReference
  -> UTCTime
  -> SqlPersistT IO ProviderEventClaim
claimProviderEvent eventRef now = do
  let staleBefore = providerEventStaleBefore now
  claimed <- (rawSql
    "UPDATE commerce_provider_event_inbox\
    \ SET processing_status = 'processing', attempt_count = attempt_count + 1,\
    \ processing_started_at = ?, last_attempt_at = ?, next_attempt_at = NULL,\
    \ error_summary = NULL\
    \ WHERE id = ?::uuid AND (\
    \   processing_status = 'pending'\
    \   OR (processing_status = 'retry' AND COALESCE(next_attempt_at, ?) <= ?)\
    \   OR (processing_status = 'processing'\
    \       AND processing_started_at < ?)\
    \ ) RETURNING attempt_count"
    [ PersistUTCTime now
    , PersistUTCTime now
    , PersistText (providerEventReferenceId eventRef)
    , PersistUTCTime now
    , PersistUTCTime now
    , PersistUTCTime staleBefore
    ] :: SqlPersistT IO [Single Int])
  case claimed of
    [Single attemptCount] -> pure (ProviderEventClaimed attemptCount)
    [] -> do
      statuses <- (rawSql
        "SELECT processing_status FROM commerce_provider_event_inbox WHERE id = ?::uuid"
        [PersistText (providerEventReferenceId eventRef)] :: SqlPersistT IO [Single Text])
      pure $ case statuses of
        [Single status]
          | status `elem` ["processed", "ignored", "dead_letter"] ->
              ProviderEventAlreadyHandled status
        _ -> ProviderEventBusy
    _ -> pure ProviderEventBusy

markProviderEventProcessed
  :: ProviderEventReference
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> UTCTime
  -> SqlPersistT IO ()
markProviderEventProcessed = markProviderEventTerminal "processed"

markProviderEventIgnored
  :: ProviderEventReference
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> UTCTime
  -> SqlPersistT IO ()
markProviderEventIgnored = markProviderEventTerminal "ignored"

markProviderEventRetry
  :: ProviderEventReference
  -> Int
  -> Text
  -> UTCTime
  -> SqlPersistT IO Bool
markProviderEventRetry eventRef attemptCount rawSummary now = do
  let exhausted = attemptCount >= maxProviderEventAttempts
      nextAttempt = addUTCTime (retryDelaySeconds attemptCount) now
      status = if exhausted then "dead_letter" else "retry"
  rawExecute
    "UPDATE commerce_provider_event_inbox\
    \ SET processing_status = ?, next_attempt_at = ?, error_summary = ?,\
    \ processing_started_at = NULL\
    \ WHERE id = ?::uuid AND processing_status = 'processing'"
    [ PersistText status
    , if exhausted then PersistNull else PersistUTCTime nextAttempt
    , PersistText (safeErrorSummary rawSummary)
    , PersistText (providerEventReferenceId eventRef)
    ]
  pure exhausted

markProviderEventDeadLetter
  :: ProviderEventReference
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
markProviderEventDeadLetter eventRef checkoutId attemptId refundId rawSummary now =
  rawExecute
    "UPDATE commerce_provider_event_inbox\
    \ SET processing_status = 'dead_letter', next_attempt_at = NULL,\
    \ checkout_id = ?::uuid, payment_attempt_id = ?::uuid, refund_id = ?::uuid,\
    \ error_summary = ?, processing_started_at = NULL, last_attempt_at = ?\
    \ WHERE id = ?::uuid AND processing_status = 'processing'"
    [ maybe PersistNull PersistText checkoutId
    , maybe PersistNull PersistText attemptId
    , maybe PersistNull PersistText refundId
    , PersistText (safeErrorSummary rawSummary)
    , PersistUTCTime now
    , PersistText (providerEventReferenceId eventRef)
    ]

validateProviderEventTimestamp
  :: UTCTime
  -> UTCTime
  -> Either Text ()
validateProviderEventTimestamp now transmittedAt
  | transmittedAt > addUTCTime allowedFutureSkew now =
      Left "Provider event timestamp is too far in the future"
  | diffUTCTime now transmittedAt > maxProviderEventAge =
      Left "Provider event timestamp is outside the replay window"
  | otherwise = Right ()

markProviderEventTerminal
  :: Text
  -> ProviderEventReference
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> UTCTime
  -> SqlPersistT IO ()
markProviderEventTerminal status eventRef checkoutId attemptId refundId now =
  rawExecute
    "UPDATE commerce_provider_event_inbox\
    \ SET processing_status = ?, checkout_id = ?::uuid, payment_attempt_id = ?::uuid,\
    \ refund_id = ?::uuid, processed_at = ?, processing_started_at = NULL,\
    \ next_attempt_at = NULL, error_summary = NULL\
    \ WHERE id = ?::uuid AND processing_status = 'processing'"
    [ PersistText status
    , maybe PersistNull PersistText checkoutId
    , maybe PersistNull PersistText attemptId
    , maybe PersistNull PersistText refundId
    , PersistUTCTime now
    , PersistText (providerEventReferenceId eventRef)
    ]

sha256Hex :: ByteString -> Text
sha256Hex bytes =
  TE.decodeUtf8 (BAE.convertToBase BAE.Base16 (hash bytes :: Digest SHA256))

validEncryptionKey :: Text -> Bool
validEncryptionKey key =
  let normalized = T.strip key
  in T.length normalized >= 32
      && T.length normalized <= 256
      && T.all (\character -> character >= '!' && character <= '~') normalized

validReference :: Int -> Text -> Bool
validReference maxLength value =
  let normalized = T.strip value
  in not (T.null normalized)
      && T.length normalized <= maxLength
      && T.all (\character ->
        (character >= '0' && character <= '9')
          || (character >= 'A' && character <= 'Z')
          || (character >= 'a' && character <= 'z')
          || character `elem` ("-_.:" :: String)) normalized

validEventType :: Text -> Bool
validEventType value =
  let normalized = T.strip value
  in not (T.null normalized)
      && T.length normalized <= 100
      && T.all (\character ->
        (character >= 'A' && character <= 'Z')
          || (character >= '0' && character <= '9')
          || character `elem` ("._-" :: String)) normalized

safeErrorSummary :: Text -> Text
safeErrorSummary = T.take 500 . T.filter (\character -> character >= ' ' && character /= '\DEL')

retryDelaySeconds :: Int -> NominalDiffTime
retryDelaySeconds attemptCount =
  fromIntegral (min (3600 :: Int64) (30 * (2 ^ min 7 (max 0 (attemptCount - 1)))))

maxProviderEventBytes :: Int
maxProviderEventBytes = 1024 * 1024

maxProviderEventAttempts :: Int
maxProviderEventAttempts = 8

allowedFutureSkew :: NominalDiffTime
allowedFutureSkew = 300

maxProviderEventAge :: NominalDiffTime
maxProviderEventAge = 4 * 24 * 60 * 60

providerEventRecordSelect :: Text
providerEventRecordSelect =
  "SELECT jsonb_build_object(\
  \ 'perId', id::text, 'perProvider', provider, 'perEnvironment', environment,\
  \ 'perProviderEventId', provider_event_id, 'perEventType', event_type,\
  \ 'perEvidenceType', evidence_type,\
  \ 'perProviderResourceId', provider_resource_id, 'perStatus', processing_status,\
  \ 'perAttemptCount', attempt_count, 'perCheckoutId', checkout_id::text,\
  \ 'perPaymentAttemptId', payment_attempt_id::text, 'perRefundId', refund_id::text,\
  \ 'perReceivedAt', received_at, 'perProviderCreatedAt', provider_created_at,\
  \ 'perProcessingStartedAt', processing_started_at, 'perLastAttemptAt', last_attempt_at,\
  \ 'perNextAttemptAt', next_attempt_at, 'perProcessedAt', processed_at,\
  \ 'perErrorSummary', error_summary)::text\
  \ FROM commerce_provider_event_inbox"

loadProviderEventRecord
  :: ProviderEventReference
  -> SqlPersistT IO [ProviderEventRecord]
loadProviderEventRecord eventRef = do
  rows <- rawSql
    (providerEventRecordSelect <> " WHERE id = ?::uuid")
    [PersistText (providerEventReferenceId eventRef)] :: SqlPersistT IO [Single Text]
  decodeProviderEventRecords rows

decodeProviderEventRecords
  :: [Single Text]
  -> SqlPersistT IO [ProviderEventRecord]
decodeProviderEventRecords rows =
  case traverse decodeOne rows of
    Left message -> liftIO (ioError (userError message))
    Right records -> pure records
  where
    decodeOne (Single rawJson) =
      firstString "Invalid provider event record" $
        eitherDecodeStrict' (TE.encodeUtf8 rawJson)

firstText :: Text -> Either String a -> Either Text a
firstText prefix = either (Left . (prefix <>) . (": " <>) . T.pack) Right

firstString :: String -> Either String a -> Either String a
firstString prefix = either (Left . ((prefix <> ": ") <>)) Right
