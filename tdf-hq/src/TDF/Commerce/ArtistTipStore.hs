{-# LANGUAGE OverloadedStrings #-}

-- | Database side of keyed artist tips (PAY-TIP-001): reserve the tip row or
-- recognise a replay before any provider call. Pure decisions live in
-- 'TDF.Commerce.ArtistTip'; this module only orders the lookups and the
-- atomic keyed insert so a concurrent duplicate is recognised instead of
-- doubled or turned into a unique-violation error.
module TDF.Commerce.ArtistTipStore
  ( TipReservation (..)
  , reserveArtistTip
  , insertKeyedTip
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Text (Text)
import           Database.Persist (Entity (..), getBy, insert)
import           Database.Persist.Sql (Single (..), SqlPersistT, rawSql, toPersistValue, toSqlKey)

import           TDF.Commerce.ArtistTip (TipReplay (..), decideTipReplay)
import qualified TDF.ModelsExtra as ME

data TipReservation
  = TipReserved ME.ArtistTipId
    -- ^ a new pending row was inserted; create the PaymentIntent for it
  | TipReplay ME.ArtistTipId ME.ArtistTip Text
    -- ^ same key and same request: answer with the stored PaymentIntent
  | TipConflict Text
    -- ^ same key, different request (or unusable stored row)
  | TipPending
    -- ^ same key, a concurrent request holds the row without an intent yet
  deriving (Show)

-- | Reserve a tip row for the request fingerprint and optional idempotency key.
--
-- Unkeyed requests always insert a new row. Keyed requests are answered from
-- the existing row when the fingerprint matches and a PaymentIntent exists,
-- rejected when the fingerprint differs, and otherwise inserted; a concurrent
-- insert of the same key is re-read instead of creating a second row.
reserveArtistTip :: MonadIO m => Text -> Maybe Text -> ME.ArtistTip -> SqlPersistT m TipReservation
reserveArtistTip fingerprint mKey tipRow = do
  existing <- lookupKeyed
  case decideTipReplay fingerprint (storedFor <$> existing) of
    TipReplayIntent intentId | Just (Entity tipKey tip) <- existing -> pure (TipReplay tipKey tip intentId)
    TipReplayConflict message -> pure (TipConflict message)
    TipReplayInProgress -> pure TipPending
    _ -> case mKey of
      Nothing -> TipReserved <$> insert tipRow
      Just _ -> do
        inserted <- insertKeyedTip tipRow
        case inserted of
          Just tipKey -> pure (TipReserved tipKey)
          Nothing -> do
            -- A concurrent request inserted the same key between our lookup and
            -- our insert; answer from its row exactly as a sequential retry.
            raced <- lookupKeyed
            case decideTipReplay fingerprint (storedFor <$> raced) of
              TipReplayIntent intentId | Just (Entity tipKey tip) <- raced -> pure (TipReplay tipKey tip intentId)
              TipReplayConflict message -> pure (TipConflict message)
              _ -> pure TipPending
  where
    profileKey = ME.artistTipArtistProfileId tipRow
    lookupKeyed = maybe (pure Nothing)
      (\key -> getBy (ME.UniqueArtistTipIdempotency profileKey (Just key))) mKey
    storedFor (Entity _ tip) =
      ( fromMaybe "" (ME.artistTipRequestSha256 tip)
      , ME.artistTipStripePaymentIntentId tip
      , ME.artistTipStatus tip )

-- | Insert a keyed tip row unless a row with the same artist and idempotency
-- key exists, in one statement. Persistent's 'insertUnique' checks and then
-- inserts, so a concurrent duplicate surfaces as a unique-violation exception
-- that also aborts the PostgreSQL transaction; @ON CONFLICT DO NOTHING@ makes
-- the loser observe 'Nothing' and keep its transaction usable.
insertKeyedTip :: MonadIO m => ME.ArtistTip -> SqlPersistT m (Maybe ME.ArtistTipId)
insertKeyedTip tip = do
  ids <- rawSql
    "INSERT INTO artist_tip (artist_profile_id, tipper_party_id, tipper_email, tipper_name,\
    \ amount_cents, currency, platform_fee_cents, stripe_payment_intent_id, status, message,\
    \ idempotency_key, request_sha256, created_at, updated_at)\
    \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)\
    \ ON CONFLICT (artist_profile_id, idempotency_key) WHERE idempotency_key IS NOT NULL\
    \ DO NOTHING RETURNING id"
    [ toPersistValue (ME.artistTipArtistProfileId tip)
    , toPersistValue (ME.artistTipTipperPartyId tip)
    , toPersistValue (ME.artistTipTipperEmail tip)
    , toPersistValue (ME.artistTipTipperName tip)
    , toPersistValue (ME.artistTipAmountCents tip)
    , toPersistValue (ME.artistTipCurrency tip)
    , toPersistValue (ME.artistTipPlatformFeeCents tip)
    , toPersistValue (ME.artistTipStripePaymentIntentId tip)
    , toPersistValue (ME.artistTipStatus tip)
    , toPersistValue (ME.artistTipMessage tip)
    , toPersistValue (ME.artistTipIdempotencyKey tip)
    , toPersistValue (ME.artistTipRequestSha256 tip)
    , toPersistValue (ME.artistTipCreatedAt tip)
    , toPersistValue (ME.artistTipUpdatedAt tip)
    ]
  pure (toSqlKey . unSingle <$> listToMaybe ids)
