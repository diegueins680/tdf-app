{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Public visibility of artist profiles (ART-PUBLIC-VISIBILITY-001).
--
-- One rule for every public artist read, artist tips and the directory:
-- a directory identity of the artist (a @directory_profile@ whose subject is
-- the artist party, or one linked to its legacy @artist_profile@ row) always
-- wins, even when it is private, draft, moderated or merged, and the artist is
-- then listed only when at least one identity is published, public, allowed
-- and not merged. An artist without any directory identity is listed through
-- its legacy profile. This mirrors the discovery projection in
-- "TDF.Server.Directory".
--
-- Administrative and owner reads do not use this module.
module TDF.Profiles.ArtistVisibility
  ( ArtistDirectoryVisibility (..)
  , artistDirectoryVisibilityFromCounts
  , artistPubliclyListed
  , loadArtistDirectoryVisibility
  , loadPubliclyListedArtistParties
  , artistPartyPubliclyListed
  ) where

import           Control.Monad.IO.Class  (MonadIO)
import           Data.Int                (Int64)
import qualified Data.Set                as Set
import qualified Data.Text               as T
import           Database.Persist        (PersistValue (..))
import           Database.Persist.Sql    (Single (..), SqlPersistT, fromSqlKey, rawSql, toSqlKey)

import           TDF.Models              (ArtistProfileId, PartyId)

data ArtistDirectoryVisibility
  = NoDirectoryIdentity
  | DirectoryIdentity
      { publicIdentityCount :: !Int
        -- ^ identities that are published, public, allowed and not merged
      }
  deriving (Eq, Show)

-- | Visibility evidence from the count of all directory identities of the
-- artist and the count of publicly listed ones. Impossible evidence (a public
-- count outside @0..total@) is treated as not listed, never as listed.
artistDirectoryVisibilityFromCounts :: Int -> Int -> ArtistDirectoryVisibility
artistDirectoryVisibilityFromCounts total public
  | total <= 0 = NoDirectoryIdentity
  | public < 0 || public > total = DirectoryIdentity 0
  | otherwise = DirectoryIdentity public

artistPubliclyListed :: ArtistDirectoryVisibility -> Bool
artistPubliclyListed NoDirectoryIdentity = True
artistPubliclyListed (DirectoryIdentity public) = public > 0

-- | Identity counts per artist profile. The predicate is the one of the
-- discovery projection; an identity reached both by subject and by legacy
-- link is counted once.
visibilitySql :: T.Text -> T.Text
visibilitySql whereClause =
  "SELECT artist.artist_party_id, \
  \count(DISTINCT profile.id)::bigint, \
  \count(DISTINCT profile.id) FILTER (WHERE profile.profile_status='published' \
  \AND profile.visibility='public' AND profile.moderation_status='allowed' \
  \AND profile.canonical_profile_id IS NULL)::bigint \
  \FROM artist_profile artist \
  \LEFT JOIN directory_profile profile \
  \ON profile.subject_party_id = artist.artist_party_id \
  \OR profile.id IN (SELECT link.profile_id FROM directory_legacy_link link \
  \WHERE link.legacy_kind='artist_profile' AND link.legacy_id = artist.id::text) "
  <> whereClause <>
  " GROUP BY artist.artist_party_id"

-- | Visibility of one artist profile row (used by tip admission, which has
-- already loaded the profile).
loadArtistDirectoryVisibility
  :: MonadIO m
  => PartyId
  -> ArtistProfileId
  -> SqlPersistT m ArtistDirectoryVisibility
loadArtistDirectoryVisibility partyKey profileKey = do
  rows <- rawSql
    (visibilitySql "WHERE artist.artist_party_id = ? AND artist.id = ?")
    [ PersistInt64 (fromSqlKey partyKey)
    , PersistInt64 (fromSqlKey profileKey)
    ]
  pure $ case rows of
    [(Single (_ :: Int64), Single total, Single public)] ->
      artistDirectoryVisibilityFromCounts (fromIntegral (total :: Int64)) (fromIntegral (public :: Int64))
    -- A missing aggregate row is impossible evidence: fail closed.
    _ -> DirectoryIdentity 0

-- | Every artist party the public may currently see.
loadPubliclyListedArtistParties :: MonadIO m => SqlPersistT m (Set.Set PartyId)
loadPubliclyListedArtistParties = do
  rows <- rawSql (visibilitySql "") []
  pure $ Set.fromList
    [ toSqlKey party
    | (Single party, Single total, Single public) <- rows
    , artistPubliclyListed
        (artistDirectoryVisibilityFromCounts (fromIntegral (total :: Int64)) (fromIntegral (public :: Int64)))
    ]

-- | Whether one artist party is publicly listed. A party without an artist
-- profile is not.
artistPartyPubliclyListed :: MonadIO m => PartyId -> SqlPersistT m Bool
artistPartyPubliclyListed partyKey = do
  rows <- rawSql
    (visibilitySql "WHERE artist.artist_party_id = ?")
    [PersistInt64 (fromSqlKey partyKey)]
  pure $ case rows of
    [(Single (_ :: Int64), Single total, Single public)] ->
      artistPubliclyListed
        (artistDirectoryVisibilityFromCounts (fromIntegral (total :: Int64)) (fromIntegral (public :: Int64)))
    _ -> False
