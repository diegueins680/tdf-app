{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Commerce.ProviderEventStore
  ( ProviderEventCreation(..)
  , ProviderEventReference(..)
  , ProviderEventStored(..)
  , ProviderEventClaim(..)
  , storeVerifiedProviderEvent
  , claimProviderEvent
  , markProviderEventProcessed
  , markProviderEventIgnored
  , markProviderEventRetry
  , markProviderEventDeadLetter
  , validateProviderEventTimestamp
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray.Encoding as BAE
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql)

import           TDF.Commerce.CheckoutStore
  ( CheckoutEnvironment
  , PaymentProvider
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

storeVerifiedProviderEvent
  :: ProviderEventCreation
  -> SqlPersistT IO (Either Text ProviderEventStored)
storeVerifiedProviderEvent ProviderEventCreation{..}
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
  | otherwise = do
      eventId <- liftIO (toText <$> nextRandom)
      let payloadHash = sha256Hex pecRawPayload
      inserted <- (rawSql
        "INSERT INTO commerce_provider_event_inbox (\
        \ id, provider, environment, merchant_account_ref, provider_event_id,\
        \ event_type, signature_verified, received_at, provider_created_at,\
        \ provider_resource_id, payload_ciphertext, payload_sha256, processing_status\
        \) VALUES (?::uuid, ?, ?, ?, ?, ?, TRUE, ?, ?, ?,\
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
        , PersistUTCTime pecReceivedAt
        , maybe PersistNull PersistUTCTime pecProviderCreatedAt
        , maybe PersistNull PersistText pecProviderResource
        , PersistByteString pecRawPayload
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
            \ AND signature_verified = TRUE AND payload_sha256 = ?\
            \ AND provider_resource_id IS NOT DISTINCT FROM ?"
            [ PersistText (paymentProviderText pecProvider)
            , PersistText (checkoutEnvironmentText pecEnvironment)
            , PersistText pecMerchantRef
            , PersistText pecProviderEventId
            , PersistText pecEventType
            , PersistText payloadHash
            , maybe PersistNull PersistText pecProviderResource
            ] :: SqlPersistT IO [Single Text])
          case existing of
            [Single existingId] ->
              pure (Right (ProviderEventStored (ProviderEventReference existingId) False))
            _ -> pure (Left "Provider event ID conflicts with different immutable evidence")
        _ -> pure (Left "Provider event insert returned an ambiguous result")

claimProviderEvent
  :: ProviderEventReference
  -> UTCTime
  -> SqlPersistT IO ProviderEventClaim
claimProviderEvent eventRef now = do
  claimed <- (rawSql
    "UPDATE commerce_provider_event_inbox\
    \ SET processing_status = 'processing', attempt_count = attempt_count + 1,\
    \ processing_started_at = ?, last_attempt_at = ?, next_attempt_at = NULL,\
    \ error_summary = NULL\
    \ WHERE id = ?::uuid AND (\
    \   processing_status = 'pending'\
    \   OR (processing_status = 'retry' AND COALESCE(next_attempt_at, ?) <= ?)\
    \   OR (processing_status = 'processing'\
    \       AND processing_started_at < ? - INTERVAL '15 minutes')\
    \ ) RETURNING attempt_count"
    [ PersistUTCTime now
    , PersistUTCTime now
    , PersistText (providerEventReferenceId eventRef)
    , PersistUTCTime now
    , PersistUTCTime now
    , PersistUTCTime now
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
