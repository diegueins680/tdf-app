{-# LANGUAGE OverloadedStrings #-}
-- | Pure admission and fee rules for public artist tips (PAY-TIP-001).
--
-- The route @POST /fans/artists/{artistId}/tips@ addresses the artist by the
-- same 'PartyId' as every sibling @/fans/artists/{artistId}@ route and as the
-- public 'apArtistId'. A tip is admitted only for an artist that the public
-- directory currently lists and whose payout onboarding is complete; the
-- destination account is always the addressed artist's own account.
--
-- This module performs no IO. The handler gathers the evidence, these
-- functions decide, and the handler maps each rejection to one HTTP status.
module TDF.Commerce.ArtistTip
  ( ArtistDirectoryVisibility (..)
  , TipRejection (..)
  , artistDirectoryVisibilityFromCounts
  , artistPubliclyListed
  , admitArtistTip
  , artistTipPlatformFeeBps
  , artistTipPlatformFeeCents
  , TipReplay (..)
  , validateTipIdempotencyKey
  , tipRequestFingerprint
  , decideTipReplay
  ) where

import           Crypto.Hash (Digest, SHA256, hash)
import           Data.Char (isAscii, isAlphaNum)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           TDF.Profiles.ArtistVisibility
  ( ArtistDirectoryVisibility (..)
  , artistDirectoryVisibilityFromCounts
  , artistPubliclyListed
  )

data TipRejection
  = TipArtistNotFound
    -- ^ no artist profile for the addressed party
  | TipArtistNotListed
    -- ^ the artist exists but the public directory does not list it
  | TipPayoutNotReady
    -- ^ listed artist without a completed payout account
  deriving (Eq, Show)

-- | Decide admission and the destination account.
--
-- The first argument is the addressed artist's profile evidence: 'Nothing'
-- when the party has no artist profile, otherwise its stored payout account.
-- A blank stored account is not a completed onboarding.
admitArtistTip
  :: Maybe (Maybe Text)
  -> ArtistDirectoryVisibility
  -> Either TipRejection Text
admitArtistTip Nothing _ = Left TipArtistNotFound
admitArtistTip (Just storedAccount) visibility
  | not (artistPubliclyListed visibility) = Left TipArtistNotListed
  | otherwise =
      case T.strip <$> storedAccount of
        Just account | not (T.null account) -> Right account
        _ -> Left TipPayoutNotReady

-- | Platform share in basis points (10%).
artistTipPlatformFeeBps :: Int
artistTipPlatformFeeBps = 1000

-- | Platform fee in minor units, rounded down so the artist never receives
-- less than the exact share. For every validated amount (1..99,999,999) the
-- product stays far inside 'Int', and @0 <= fee < amount@.
artistTipPlatformFeeCents :: Int -> Int
artistTipPlatformFeeCents amountCents =
  (amountCents * artistTipPlatformFeeBps) `div` 10000

-- | Request-level idempotency (PAY-TIP-001). The client may send an
-- @Idempotency-Key@; the same key with the same request replays the stored
-- tip, the same key with a different request is a conflict, and a key whose
-- earlier attempt never obtained a PaymentIntent or already failed cannot be
-- replayed into a new charge.
data TipReplay
  = TipCreateNew
    -- ^ no stored tip for this key: create one
  | TipReplayIntent Text
    -- ^ same request: answer with the stored PaymentIntent
  | TipReplayInProgress
    -- ^ same request, still waiting for the provider: retry with the same key
  | TipReplayConflict Text
    -- ^ the key cannot be honoured; the text is the public reason
  deriving (Eq, Show)

-- | Keys are 1..128 ASCII letters, digits, hyphens or underscores, so they are
-- safe in logs, indexes and provider metadata.
validateTipIdempotencyKey :: Text -> Either Text Text
validateTipIdempotencyKey raw
  | T.null raw || T.length raw > 128 =
      Left "Idempotency-Key must contain 1 to 128 characters"
  | not (T.all allowed raw) =
      Left "Idempotency-Key must contain only ASCII letters, digits, hyphens or underscores"
  | otherwise = Right raw
  where
    allowed c = isAscii c && (isAlphaNum c || c == '-' || c == '_')

-- | Canonical fingerprint of the request the key is bound to. Every field is
-- length-prefixed so no two different requests share a fingerprint text.
tipRequestFingerprint
  :: Int64 -> Int -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text
tipRequestFingerprint artistPartyId amountCents currency tipperName tipperEmail message =
  T.pack (show (hash (TE.encodeUtf8 canonical) :: Digest SHA256))
  where
    canonical = T.intercalate "\n" (map field
      [ T.pack (show artistPartyId), T.pack (show amountCents), T.toLower (T.strip currency)
      , opt tipperName, opt tipperEmail, opt message ])
    opt = maybe "-" ("+" <>)
    field value = T.pack (show (T.length value)) <> ":" <> value

-- | Decide what a keyed request does, given the stored row for that key, if
-- any: its fingerprint, PaymentIntent id and status.
decideTipReplay :: Text -> Maybe (Text, Maybe Text, Text) -> TipReplay
decideTipReplay _ Nothing = TipCreateNew
decideTipReplay fingerprint (Just (storedFingerprint, storedIntent, storedStatus))
  | storedFingerprint /= fingerprint =
      TipReplayConflict "Idempotency-Key was already used for a different tip request"
  | storedStatus == "failed" =
      TipReplayConflict "The earlier attempt with this Idempotency-Key failed; use a new key"
  | otherwise = case storedIntent of
      Just intentId | not (T.null intentId) -> TipReplayIntent intentId
      -- No PaymentIntent stored yet: the original request is still waiting
      -- for the provider and may succeed, so a new key would double-charge.
      _ -> TipReplayInProgress

