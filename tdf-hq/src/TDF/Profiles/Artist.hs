{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Profiles.Artist
  ( upsertArtistProfileRecord
  , validateArtistProfileUpsert
  , loadArtistProfileDTO
  , loadArtistProfileBySlugDTO
  , loadOrCreateArtistProfileDTO
  , loadAllArtistProfilesDTO
  , searchArtistProfilesDTO
  , buildArtistProfileDTOs
  , fetchPartyNameMap
  , fetchFollowerCounts
  , fetchArtistProfileMap
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Char                 ( GeneralCategory (Format, LineSeparator, ParagraphSeparator)
                                           , generalCategory
                                           , isAsciiLower
                                           , isControl
                                           , isDigit
                                           , isSpace
                                           )
import qualified Data.Map.Strict           as Map
import qualified Data.Set                  as Set
import           Data.List                 (foldl')
import           Data.Maybe                (listToMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime, getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql      (SqlPersistT, fromSqlKey)

import           TDF.DTO                   ( ArtistProfileDTO(..)
                                           , ArtistProfileUpsert(..)
                                           )
import           TDF.Models
import qualified TDF.Models                as M
import qualified TDF.Trials.Server         as TrialsServer (isValidHttpUrl)

cleanOptionalText :: Maybe Text -> Maybe Text
cleanOptionalText = (>>= nonBlank)
  where
    nonBlank txt =
      let trimmed = T.strip txt
      in if T.null trimmed then Nothing else Just trimmed

validateArtistProfileUpsert :: ArtistProfileUpsert -> Either Text ArtistProfileUpsert
validateArtistProfileUpsert payload@ArtistProfileUpsert{..} = do
  normalizedSlug <- validateArtistProfileSlug apuSlug
  normalizedHeroImageUrl <- validateArtistProfileUrl "heroImageUrl" apuHeroImageUrl
  normalizedSpotifyUrl <- validateArtistProfileUrl "spotifyUrl" apuSpotifyUrl
  normalizedYoutubeUrl <- validateArtistProfileUrl "youtubeUrl" apuYoutubeUrl
  normalizedWebsiteUrl <- validateArtistProfileUrl "websiteUrl" apuWebsiteUrl
  normalizedFeaturedVideoUrl <- validateArtistProfileUrl "featuredVideoUrl" apuFeaturedVideoUrl
  pure payload
    { apuSlug = normalizedSlug
    , apuHeroImageUrl = normalizedHeroImageUrl
    , apuSpotifyUrl = normalizedSpotifyUrl
    , apuYoutubeUrl = normalizedYoutubeUrl
    , apuWebsiteUrl = normalizedWebsiteUrl
    , apuFeaturedVideoUrl = normalizedFeaturedVideoUrl
    }

validateArtistProfileSlug :: Maybe Text -> Either Text (Maybe Text)
validateArtistProfileSlug Nothing = Right Nothing
validateArtistProfileSlug (Just rawSlug) =
  case cleanOptionalText (Just rawSlug) of
    Nothing -> Right Nothing
    Just slugVal
      | T.length normalizedSlug > maxArtistProfileSlugChars ->
          Left "artist profile slug must be 96 characters or fewer"
      | isValidArtistProfileSlug normalizedSlug ->
          Right (Just normalizedSlug)
      | otherwise ->
          Left "artist profile slug must contain only lowercase ASCII letters, numbers, and hyphens"
      where
        normalizedSlug = T.toLower slugVal

maxArtistProfileSlugChars :: Int
maxArtistProfileSlugChars = 96

isValidArtistProfileSlug :: Text -> Bool
isValidArtistProfileSlug slugVal =
  T.any isArtistProfileSlugAtom slugVal
    && T.all isArtistProfileSlugChar slugVal
    && T.head slugVal /= '-'
    && T.last slugVal /= '-'

isArtistProfileSlugChar :: Char -> Bool
isArtistProfileSlugChar ch = isArtistProfileSlugAtom ch || ch == '-'

isArtistProfileSlugAtom :: Char -> Bool
isArtistProfileSlugAtom ch = isAsciiLower ch || isDigit ch

validateArtistProfileUrl :: Text -> Maybe Text -> Either Text (Maybe Text)
validateArtistProfileUrl _ Nothing = Right Nothing
validateArtistProfileUrl fieldName (Just rawUrl) =
  case cleanOptionalText (Just rawUrl) of
    Nothing -> Right Nothing
    Just urlVal
      | T.length urlVal > maxArtistProfileUrlChars ->
          Left (fieldName <> " must be 2048 characters or fewer")
      | T.any isUnsafeArtistProfileUrlChar urlVal ->
          Left
            ( fieldName
                <> " must not contain whitespace, control, or hidden formatting characters"
            )
      | isAbsoluteArtistProfileHttpUrl urlVal ->
          Right (Just urlVal)
      | otherwise ->
          Left (fieldName <> " must be an absolute public http or https URL")

maxArtistProfileUrlChars :: Int
maxArtistProfileUrlChars = 2048

isAbsoluteArtistProfileHttpUrl :: Text -> Bool
isAbsoluteArtistProfileHttpUrl = TrialsServer.isValidHttpUrl

isUnsafeArtistProfileUrlChar :: Char -> Bool
isUnsafeArtistProfileUrlChar ch =
  isSpace ch
    || isControl ch
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

upsertArtistProfileRecord
  :: MonadIO m
  => PartyId
  -> ArtistProfileUpsert
  -> UTCTime
  -> SqlPersistT m ArtistProfileDTO
upsertArtistProfileRecord artistKey ArtistProfileUpsert{..} now = do
  let trimmedDisplay = cleanOptionalText apuDisplayName
      normalizedSlug = cleanOptionalText apuSlug
      normalizedBio = cleanOptionalText apuBio
      normalizedCity = cleanOptionalText apuCity
      normalizedHeroImageUrl = cleanOptionalText apuHeroImageUrl
      normalizedSpotifyArtistId = cleanOptionalText apuSpotifyArtistId
      normalizedSpotifyUrl = cleanOptionalText apuSpotifyUrl
      normalizedYoutubeChannelId = cleanOptionalText apuYoutubeChannelId
      normalizedYoutubeUrl = cleanOptionalText apuYoutubeUrl
      normalizedWebsiteUrl = cleanOptionalText apuWebsiteUrl
      normalizedFeaturedVideoUrl = cleanOptionalText apuFeaturedVideoUrl
      normalizedGenres = cleanOptionalText apuGenres
      normalizedHighlights = cleanOptionalText apuHighlights
      displayUpdate =
        case trimmedDisplay of
          Just name | not (T.null name) -> Just (M.PartyDisplayName =. name)
          _                             -> Nothing
  case displayUpdate of
    Just upd -> update artistKey [upd]
    Nothing  -> pure ()
  _ <- upsert
    ArtistProfile
      { artistProfileArtistPartyId    = artistKey
      , artistProfileSlug             = normalizedSlug
      , artistProfileBio              = normalizedBio
      , artistProfileCity             = normalizedCity
      , artistProfileHeroImageUrl     = normalizedHeroImageUrl
      , artistProfileSpotifyArtistId  = normalizedSpotifyArtistId
      , artistProfileSpotifyUrl       = normalizedSpotifyUrl
      , artistProfileYoutubeChannelId = normalizedYoutubeChannelId
      , artistProfileYoutubeUrl       = normalizedYoutubeUrl
      , artistProfileWebsiteUrl       = normalizedWebsiteUrl
      , artistProfileFeaturedVideoUrl = normalizedFeaturedVideoUrl
      , artistProfileGenres           = normalizedGenres
      , artistProfileHighlights       = normalizedHighlights
      , artistProfileStripeAccountId  = Nothing
      , artistProfileCreatedAt        = now
      , artistProfileUpdatedAt        = Just now
      }
    [ ArtistProfileSlug             =. normalizedSlug
    , ArtistProfileBio              =. normalizedBio
    , ArtistProfileCity             =. normalizedCity
    , ArtistProfileHeroImageUrl     =. normalizedHeroImageUrl
    , ArtistProfileSpotifyArtistId  =. normalizedSpotifyArtistId
    , ArtistProfileSpotifyUrl       =. normalizedSpotifyUrl
    , ArtistProfileYoutubeChannelId =. normalizedYoutubeChannelId
    , ArtistProfileYoutubeUrl       =. normalizedYoutubeUrl
    , ArtistProfileWebsiteUrl       =. normalizedWebsiteUrl
    , ArtistProfileFeaturedVideoUrl =. normalizedFeaturedVideoUrl
    , ArtistProfileGenres           =. normalizedGenres
    , ArtistProfileHighlights       =. normalizedHighlights
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

loadArtistProfileBySlugDTO :: MonadIO m => Text -> SqlPersistT m (Maybe ArtistProfileDTO)
loadArtistProfileBySlugDTO rawSlug = do
  let slugVal = T.toLower (T.strip rawSlug)
  if T.null slugVal
    then pure Nothing
    else do
      mEntity <- selectFirst [ArtistProfileSlug ==. Just slugVal] [Asc ArtistProfileArtistPartyId]
      case mEntity of
        Nothing     -> pure Nothing
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
            , artistProfileStripeAccountId  = Nothing
            , artistProfileCreatedAt        = now
            , artistProfileUpdatedAt        = Nothing
            }
      key <- insert record
      pure (Entity key record)

searchArtistProfilesDTO
  :: MonadIO m
  => Maybe Text
  -> Maybe Text
  -> SqlPersistT m [ArtistProfileDTO]
searchArtistProfilesDTO rawQuery rawGenre = do
  dtos <- loadAllArtistProfilesDTO
  let mQuery = normalizeSearchTerm rawQuery
      mGenre = normalizeSearchTerm rawGenre
  pure
    [ dto
    | dto <- dtos
    , profileMatchesQuery mQuery dto
    , profileMatchesGenre mGenre dto
    ]

normalizeSearchTerm :: Maybe Text -> Maybe Text
normalizeSearchTerm raw =
  T.toCaseFold <$> cleanOptionalText raw

profileMatchesQuery :: Maybe Text -> ArtistProfileDTO -> Bool
profileMatchesQuery Nothing _ = True
profileMatchesQuery (Just needle) ArtistProfileDTO{..} =
  any (matchesText needle)
    [ Just apDisplayName
    , apSlug
    , apBio
    , apCity
    , apGenres
    , apHighlights
    ]

profileMatchesGenre :: Maybe Text -> ArtistProfileDTO -> Bool
profileMatchesGenre Nothing _ = True
profileMatchesGenre (Just needle) ArtistProfileDTO{..} =
  matchesText needle apGenres

matchesText :: Text -> Maybe Text -> Bool
matchesText needle raw =
  maybe False (T.isInfixOf needle . T.toCaseFold) raw

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
