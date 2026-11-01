{-# LANGUAGE OverloadedStrings #-}

-- | Database side of keyed artist tips (PAY-TIP-001): reserve the tip row or
-- recognise a replay before any provider call. Pure decisions live in
-- 'TDF.Commerce.ArtistTip'; this module only orders the lookups and the
-- unique insert so a concurrent duplicate is recognised instead of doubled.
module TDF.Commerce.ArtistTipStore
  ( TipReservation (..)
  , reserveArtistTip
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Database.Persist (Entity (..), getBy, insertUnique)
import           Database.Persist.Sql (SqlPersistT)

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
    _ -> do
      inserted <- insertUnique tipRow
      case inserted of
        Just tipKey -> pure (TipReserved tipKey)
        Nothing -> do
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
