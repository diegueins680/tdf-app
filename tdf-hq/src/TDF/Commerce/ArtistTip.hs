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

-- | How the public directory decides whether an artist party is listed.
--
-- A directory identity (a @directory_profile@ whose subject is the artist
-- party, or one linked to the legacy @artist_profile@ row) always wins, even
-- when it is private, draft, moderated or merged: the legacy profile is then
-- never listed on its own. Without any directory identity the legacy profile
-- is listed. This mirrors the discovery projection in "TDF.Server.Directory".
data ArtistDirectoryVisibility
  = NoDirectoryIdentity
  | DirectoryIdentity
      { publicIdentityCount :: !Int
        -- ^ identities that are published, public, allowed and not merged
      }
  deriving (Eq, Show)

-- | Build the visibility evidence from the two counts the handler queries:
-- all directory identities of the artist, and the publicly listed ones.
-- A public count larger than the total is impossible evidence and is treated
-- as not listed, never as listed.
artistDirectoryVisibilityFromCounts :: Int -> Int -> ArtistDirectoryVisibility
artistDirectoryVisibilityFromCounts total public
  | total <= 0 = NoDirectoryIdentity
  | public < 0 || public > total = DirectoryIdentity 0
  | otherwise = DirectoryIdentity public

artistPubliclyListed :: ArtistDirectoryVisibility -> Bool
artistPubliclyListed NoDirectoryIdentity = True
artistPubliclyListed (DirectoryIdentity public) = public > 0

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
