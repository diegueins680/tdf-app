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
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

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
