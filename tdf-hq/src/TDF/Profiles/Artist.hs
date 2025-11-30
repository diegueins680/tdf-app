{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Profiles.Artist
  ( upsertArtistProfileRecord
  , loadArtistProfileDTO
  , loadOrCreateArtistProfileDTO
  , loadAllArtistProfilesDTO
  , buildArtistProfileDTOs
  , fetchPartyNameMap
  , fetchFollowerCounts
  , fetchArtistProfileMap
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.Map.Strict           as Map
import qualified Data.Set                  as Set
import           Data.List                 (foldl')
import           Data.Maybe                (listToMaybe)
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime, getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql      (SqlPersistT, fromSqlKey)

import           TDF.DTO                   ( ArtistProfileDTO(..)
                                           , ArtistProfileUpsert(..)
                                           )
import           TDF.Models
import qualified TDF.Models                as M

upsertArtistProfileRecord
  :: MonadIO m
  => PartyId
  -> ArtistProfileUpsert
  -> UTCTime
  -> SqlPersistT m ArtistProfileDTO
upsertArtistProfileRecord artistKey ArtistProfileUpsert{..} now = do
  _ <- upsert
    ArtistProfile
      { artistProfileArtistPartyId    = artistKey
      , artistProfileSlug             = apuSlug
      , artistProfileBio              = apuBio
      , artistProfileCity             = apuCity
      , artistProfileHeroImageUrl     = apuHeroImageUrl
      , artistProfileSpotifyArtistId  = apuSpotifyArtistId
      , artistProfileSpotifyUrl       = apuSpotifyUrl
      , artistProfileYoutubeChannelId = apuYoutubeChannelId
      , artistProfileYoutubeUrl       = apuYoutubeUrl
      , artistProfileWebsiteUrl       = apuWebsiteUrl
      , artistProfileFeaturedVideoUrl = apuFeaturedVideoUrl
      , artistProfileGenres           = apuGenres
      , artistProfileHighlights       = apuHighlights
      , artistProfileCreatedAt        = now
      , artistProfileUpdatedAt        = Just now
      }
    [ ArtistProfileSlug             =. apuSlug
    , ArtistProfileBio              =. apuBio
    , ArtistProfileCity             =. apuCity
    , ArtistProfileHeroImageUrl     =. apuHeroImageUrl
    , ArtistProfileSpotifyArtistId  =. apuSpotifyArtistId
    , ArtistProfileSpotifyUrl       =. apuSpotifyUrl
    , ArtistProfileYoutubeChannelId =. apuYoutubeChannelId
    , ArtistProfileYoutubeUrl       =. apuYoutubeUrl
    , ArtistProfileWebsiteUrl       =. apuWebsiteUrl
    , ArtistProfileFeaturedVideoUrl =. apuFeaturedVideoUrl
    , ArtistProfileGenres           =. apuGenres
    , ArtistProfileHighlights       =. apuHighlights
    , ArtistProfileUpdatedAt        =. Just now
    ]
  mDto <- loadArtistProfileDTO artistKey
  case mDto of
    Just dto -> pure dto
    Nothing -> do
      hasAccount <- artistHasUserAccount [artistKey]
      pure (emptyDto artistKey hasAccount)

emptyDto :: PartyId -> Bool -> ArtistProfileDTO
emptyDto artistKey hasAccount = ArtistProfileDTO
  { apArtistId         = fromSqlKey artistKey
  , apDisplayName      = ""
  , apSlug             = Nothing
  , apBio              = Nothing
  , apCity             = Nothing
  , apHeroImageUrl     = Nothing
  , apSpotifyArtistId  = Nothing
  , apSpotifyUrl       = Nothing
  , apYoutubeChannelId = Nothing
  , apYoutubeUrl       = Nothing
  , apWebsiteUrl       = Nothing
  , apFeaturedVideoUrl = Nothing
  , apGenres           = Nothing
  , apHighlights       = Nothing
  , apFollowerCount    = 0
  , apHasUserAccount   = hasAccount
  }

loadAllArtistProfilesDTO :: MonadIO m => SqlPersistT m [ArtistProfileDTO]
loadAllArtistProfilesDTO = do
  profiles <- selectList [] [Asc ArtistProfileArtistPartyId]
  buildArtistProfileDTOs profiles

loadArtistProfileDTO :: MonadIO m => PartyId -> SqlPersistT m (Maybe ArtistProfileDTO)
loadArtistProfileDTO artistId = do
  mEntity <- getBy (UniqueArtistProfile artistId)
  case mEntity of
    Nothing    -> pure Nothing
    Just entity -> do
      dtos <- buildArtistProfileDTOs [entity]
      pure (listToMaybe dtos)

loadOrCreateArtistProfileDTO :: MonadIO m => PartyId -> SqlPersistT m ArtistProfileDTO
loadOrCreateArtistProfileDTO artistId = do
  _ <- ensureArtistProfileEntity artistId
  mDto <- loadArtistProfileDTO artistId
  case mDto of
    Just dto -> pure dto
    Nothing -> do
      hasAccount <- artistHasUserAccount [artistId]
      pure (emptyDto artistId hasAccount)

ensureArtistProfileEntity :: MonadIO m => PartyId -> SqlPersistT m (Entity ArtistProfile)
ensureArtistProfileEntity artistId = do
  mProfile <- getBy (UniqueArtistProfile artistId)
  case mProfile of
    Just ent -> pure ent
    Nothing -> do
      now <- liftIO getCurrentTime
      let record = ArtistProfile
            { artistProfileArtistPartyId    = artistId
            , artistProfileSlug             = Nothing
            , artistProfileBio              = Nothing
            , artistProfileCity             = Nothing
            , artistProfileHeroImageUrl     = Nothing
            , artistProfileSpotifyArtistId  = Nothing
            , artistProfileSpotifyUrl       = Nothing
            , artistProfileYoutubeChannelId = Nothing
            , artistProfileYoutubeUrl       = Nothing
            , artistProfileWebsiteUrl       = Nothing
            , artistProfileFeaturedVideoUrl = Nothing
            , artistProfileGenres           = Nothing
            , artistProfileHighlights       = Nothing
            , artistProfileCreatedAt        = now
            , artistProfileUpdatedAt        = Nothing
            }
      key <- insert record
      pure (Entity key record)

buildArtistProfileDTOs :: MonadIO m => [Entity ArtistProfile] -> SqlPersistT m [ArtistProfileDTO]
buildArtistProfileDTOs profiles = do
  let artistIds = map (artistProfileArtistPartyId . entityVal) profiles
  nameMap <- fetchPartyNameMap artistIds
  followerCounts <- fetchFollowerCounts artistIds
  accountMap <- fetchArtistAccountMap artistIds
  pure (map (artistProfileEntityToDTO nameMap followerCounts accountMap) profiles)

fetchArtistAccountMap :: MonadIO m => [PartyId] -> SqlPersistT m (Map.Map PartyId Bool)
fetchArtistAccountMap ids
  | null ids  = pure Map.empty
  | otherwise = do
      creds <- selectList [UserCredentialPartyId <-. ids] []
      let partyIds = Set.fromList (map (userCredentialPartyId . entityVal) creds)
      pure $ Map.fromList [ (pid, True) | pid <- Set.toList partyIds ]

fetchPartyNameMap :: MonadIO m => [PartyId] -> SqlPersistT m (Map.Map PartyId Text)
fetchPartyNameMap partyIds = do
  parties <- if null partyIds then pure [] else selectList [PartyId <-. partyIds] []
  pure $ Map.fromList [ (entityKey p, M.partyDisplayName (entityVal p)) | p <- parties ]

fetchFollowerCounts :: MonadIO m => [PartyId] -> SqlPersistT m (Map.Map PartyId Int)
fetchFollowerCounts ids
  | null ids  = pure Map.empty
  | otherwise = do
      follows <- selectList [FanFollowArtistPartyId <-. ids] []
      pure $ foldl' (\acc (Entity _ fol) -> Map.insertWith (+) (fanFollowArtistPartyId fol) 1 acc) Map.empty follows

fetchArtistProfileMap :: MonadIO m => [PartyId] -> SqlPersistT m (Map.Map PartyId ArtistProfile)
fetchArtistProfileMap ids
  | null ids  = pure Map.empty
  | otherwise = do
      profiles <- selectList [ArtistProfileArtistPartyId <-. ids] []
      pure $ Map.fromList [ (artistProfileArtistPartyId (entityVal ap), entityVal ap) | ap <- profiles ]

artistProfileEntityToDTO
  :: Map.Map PartyId Text
  -> Map.Map PartyId Int
  -> Map.Map PartyId Bool
  -> Entity ArtistProfile
  -> ArtistProfileDTO
artistProfileEntityToDTO nameMap followMap accountMap (Entity _ prof) =
  let artistId = artistProfileArtistPartyId prof
      displayName = Map.findWithDefault "Artista" artistId nameMap
      followerCount = Map.findWithDefault 0 artistId followMap
      hasAccount = Map.findWithDefault False artistId accountMap
  in ArtistProfileDTO
      { apArtistId         = fromSqlKey artistId
      , apDisplayName      = displayName
      , apSlug             = artistProfileSlug prof
      , apBio              = artistProfileBio prof
      , apCity             = artistProfileCity prof
      , apHeroImageUrl     = artistProfileHeroImageUrl prof
      , apSpotifyArtistId  = artistProfileSpotifyArtistId prof
      , apSpotifyUrl       = artistProfileSpotifyUrl prof
      , apYoutubeChannelId = artistProfileYoutubeChannelId prof
      , apYoutubeUrl       = artistProfileYoutubeUrl prof
      , apWebsiteUrl       = artistProfileWebsiteUrl prof
      , apFeaturedVideoUrl = artistProfileFeaturedVideoUrl prof
      , apGenres           = artistProfileGenres prof
      , apHighlights       = artistProfileHighlights prof
      , apFollowerCount    = followerCount
      , apHasUserAccount   = hasAccount
      }

artistHasUserAccount :: MonadIO m => [PartyId] -> SqlPersistT m Bool
artistHasUserAccount [] = pure False
artistHasUserAccount ids = do
  let keySet = Set.fromList ids
  creds <- selectList [UserCredentialPartyId <-. Set.toList keySet] [LimitTo 1]
  pure (not (null creds))
