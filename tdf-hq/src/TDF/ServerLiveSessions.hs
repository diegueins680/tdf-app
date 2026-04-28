{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.ServerLiveSessions
  ( liveSessionsServer
  , LiveSessionMusicianLookup(..)
  , buildLiveSessionUsernameCollisionCandidate
  , resolveLiveSessionMusicianLookup
  , sanitizeLiveSessionRiderFileName
  , validateLiveSessionReferencedPartyEmail
  , validateLiveSessionRiderFileSize
  , validateLiveSessionTermsAcceptance
  ) where

import           Control.Monad              (forM_, void, when)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Crypto.BCrypt              (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Data.Char                  (isAlphaNum, isAscii, isControl)
import           Data.Maybe                 (fromMaybe, mapMaybe)
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (UTCTime, getCurrentTime)
import           Data.UUID                  (toText)
import           Data.UUID.V4               (nextRandom)
import           Database.Persist
import           Database.Persist.Sql       (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant
import           Servant.Multipart          (FileData(..), Tmp)
import           System.Directory           (createDirectoryIfMissing, getFileSize)
import           System.FilePath            ((</>), takeFileName)
import qualified Data.ByteString.Lazy       as BL

import           TDF.API.LiveSessions
import           TDF.Auth                   (AuthedUser, auPartyId)
import           TDF.DB                     (Env(..))
import           TDF.Models
import qualified TDF.Models                 as M
import qualified TDF.ModelsExtra           as ME

liveSessionUsernameCollisionBudget :: Int
liveSessionUsernameCollisionBudget = 60

liveSessionTermsVersionMaxLength :: Int
liveSessionTermsVersionMaxLength = 160

maxLiveSessionRiderBytes :: Integer
maxLiveSessionRiderBytes = 10 * 1024 * 1024

liveSessionRiderFileNameMaxLength :: Int
liveSessionRiderFileNameMaxLength = 160

data LiveSessionMusicianLookup
  = LookupLiveSessionMusicianByEmail Text
  | CreateLiveSessionMusician
  deriving (Eq, Show)

liveSessionsServer
  :: forall m.
     ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT LiveSessionsAPI m
liveSessionsServer user = intakeHandler
  where
    intakeHandler payload = do
      let bandName = T.strip (lsiBandName payload)
      when (T.null bandName) $
        throwError err400 { errBody = "bandName is required" }
      acceptedTermsVersion <-
        either throwError pure $
          validateLiveSessionTermsAcceptance
            (lsiAcceptedTerms payload)
            (lsiTermsVersion payload)

      now <- liftIO getCurrentTime
      riderPath <- traverse validateAndStoreRiderFile (lsiRider payload)

      partyKeys <- mapM (ensureMusician now) (lsiMusicians payload)
      resolvedSongOrders <-
        either
          (\err ->
            throwError
              err400
                { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack err))
                }
          )
          pure
          (resolveLiveSessionSetlistSortOrders (lsiSetlist payload))

      intakeId <- withPool $ insert ME.LiveSessionIntake
        { ME.liveSessionIntakeBandName     = bandName
        , ME.liveSessionIntakeBandDescription = lsiBandDescription payload
        , ME.liveSessionIntakePrimaryGenre = lsiPrimaryGenre payload
        , ME.liveSessionIntakeInputList    = lsiInputList payload
        , ME.liveSessionIntakeContactEmail = T.strip <$> lsiContactEmail payload
        , ME.liveSessionIntakeContactPhone = T.strip <$> lsiContactPhone payload
        , ME.liveSessionIntakeSessionDate  = lsiSessionDate payload
        , ME.liveSessionIntakeAvailability = lsiAvailability payload
        , ME.liveSessionIntakeAcceptedTerms = True
        , ME.liveSessionIntakeTermsVersion = Just acceptedTermsVersion
        , ME.liveSessionIntakeRiderPath    = riderPath
        , ME.liveSessionIntakeCreatedBy    = Just (auPartyId user)
        , ME.liveSessionIntakeCreatedAt    = now
        }

      let preparedSongs =
            mapMaybe prepareSong (zip resolvedSongOrders (lsiSetlist payload))
          prepareSong (sortOrder, song) =
            let title = T.strip (lssTitle song)
            in if T.null title
                 then Nothing
                 else Just (sortOrder, title, song)

      withPool $
        forM_ (zip partyKeys (lsiMusicians payload)) $ \(partyKey, m) ->
          insert_ ME.LiveSessionMusician
            { ME.liveSessionMusicianIntakeId   = intakeId
            , ME.liveSessionMusicianPartyId    = partyKey
            , ME.liveSessionMusicianName       = lsmName m
            , ME.liveSessionMusicianEmail      = lsmEmail m
            , ME.liveSessionMusicianInstrument = lsmInstrument m
            , ME.liveSessionMusicianRole       = lsmRole m
            , ME.liveSessionMusicianNotes      = lsmNotes m
            , ME.liveSessionMusicianIsExisting = lsmIsExisting m
            }

      withPool $
        forM_ preparedSongs $ \(sortOrder, title, song) ->
          insert_ ME.LiveSessionSong
            { ME.liveSessionSongIntakeId  = intakeId
            , ME.liveSessionSongTitle     = title
            , ME.liveSessionSongBpm       = lssBpm song
            , ME.liveSessionSongSongKey   = fmap T.strip (lssSongKey song)
            , ME.liveSessionSongLyrics    = lssLyrics song
            , ME.liveSessionSongSortOrder = sortOrder
            }

      pure NoContent

    ensureMusician :: UTCTime -> LiveSessionMusicianPayload -> m (Key Party)
    ensureMusician now LiveSessionMusicianPayload{..} = do
      let mEmail = T.strip <$> lsmEmail
          trimmedName = T.strip lsmName
      (partyKey, accountEmail) <- case lsmPartyId of
        Just pidInt -> do
          let key = toSqlKey (fromIntegral pidInt)
          existingParty <- withPool $ get key
          case existingParty of
            Nothing -> throwError err400 { errBody = "Referenced party not found" }
            Just party -> do
              referencedPartyEmail <-
                either throwError pure $
                  validateLiveSessionReferencedPartyEmail
                    (M.partyPrimaryEmail party)
                    mEmail
              pure (key, referencedPartyEmail)
        Nothing -> do
          found <- case resolveLiveSessionMusicianLookup mEmail of
            LookupLiveSessionMusicianByEmail email ->
              withPool $ selectFirst [M.PartyPrimaryEmail ==. Just email] []
            CreateLiveSessionMusician ->
              pure Nothing
          case found of
            Just ent ->
              pure (entityKey ent, M.partyPrimaryEmail (entityVal ent))
            Nothing -> withPool $ do
              key <- insert Party
                { partyLegalName        = Nothing
                , partyDisplayName      = if T.null trimmedName then "Músico Live Session" else trimmedName
                , partyIsOrg            = False
                , partyTaxId            = Nothing
                , partyPrimaryEmail     = mEmail
                , partyPrimaryPhone     = Nothing
                , partyWhatsapp         = Nothing
                , partyInstagram        = Nothing
                , partyEmergencyContact = Nothing
                , partyNotes            = Just (fromMaybe "" lsmInstrument)
                , partyCreatedAt        = now
                }
              pure (key, mEmail)

      when (partyKey == toSqlKey 0) $
        throwError err400 { errBody = "Invalid party reference" }
      withPool $ ensureArtistRole partyKey
      withPool $ ensureUserAccount partyKey accountEmail
      pure partyKey

    ensureArtistRole :: PartyId -> SqlPersistT IO ()
    ensureArtistRole pid = do
      _ <- upsert (PartyRole pid Artist True) [PartyRoleActive =. True]
      pure ()

    ensureUserAccount :: PartyId -> Maybe Text -> SqlPersistT IO ()
    ensureUserAccount pid mEmail = do
      existing <- selectFirst [UserCredentialPartyId ==. pid, UserCredentialActive ==. True] []
      case existing of
        Just _  -> pure ()
        Nothing -> do
          baseUsername <- pure $ case mEmail of
            Just email | not (T.null email) -> T.toLower email
            _ -> "livesession-" <> T.pack (show (fromSqlKey pid))
          username <- generateUniqueUsername baseUsername
          tempPwd <- liftIO randomPassword
          hashed <- liftIO (hashPasswordText tempPwd)
          _ <- insert UserCredential
            { userCredentialPartyId      = pid
            , userCredentialUsername     = username
            , userCredentialPasswordHash = hashed
            , userCredentialActive       = True
            }
          applyRoles pid [Artist]

    applyRoles :: PartyId -> [RoleEnum] -> SqlPersistT IO ()
    applyRoles pid rolesList =
      forM_ rolesList $ \role ->
        void $ upsert (PartyRole pid role True) [PartyRoleActive =. True]

    generateUniqueUsername :: Text -> SqlPersistT IO Text
    generateUniqueUsername base = do
      conflict <- getBy (UniqueCredentialUsername base)
      case conflict of
        Nothing -> pure base
        Just _  -> generateCollisionUsername base

    generateCollisionUsername :: Text -> SqlPersistT IO Text
    generateCollisionUsername base = do
      suffix <- liftIO (T.pack . show . toText <$> nextRandom)
      let candidate = buildLiveSessionUsernameCollisionCandidate base suffix
      conflict <- getBy (UniqueCredentialUsername candidate)
      case conflict of
        Nothing -> pure candidate
        Just _  -> generateCollisionUsername base

    hashPasswordText :: Text -> IO Text
    hashPasswordText pwd = do
      let raw = TE.encodeUtf8 pwd
      mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy raw
      case mHash of
        Nothing   -> fail "Failed to hash password"
        Just hash -> pure (TE.decodeUtf8 hash)

    randomPassword :: IO Text
    randomPassword = toText <$> nextRandom

    validateAndStoreRiderFile :: FileData Tmp -> m Text
    validateAndStoreRiderFile file@FileData{..} = do
      size <- liftIO (getFileSize fdPayload)
      either throwError pure (validateLiveSessionRiderFileSize size)
      liftIO (storeRiderFile file)

    storeRiderFile :: FileData Tmp -> IO Text
    storeRiderFile FileData{..} = do
      let safeName = sanitizeLiveSessionRiderFileName fdFileName
      token <- toText <$> nextRandom
      let destDir  = "uploads/live-sessions"
          destPath = destDir </> T.unpack token <> "-" <> T.unpack safeName
      createDirectoryIfMissing True destDir
      BL.readFile fdPayload >>= BL.writeFile destPath
      pure (T.pack destPath)

validateLiveSessionRiderFileSize :: Integer -> Either ServerError ()
validateLiveSessionRiderFileSize size
  | size < 0 =
      Left err400 { errBody = "rider file size is invalid" }
  | size == 0 =
      Left err400 { errBody = "rider file must not be empty" }
  | size > maxLiveSessionRiderBytes =
      Left err400 { errBody = "rider file must be 10 MB or smaller" }
  | otherwise =
      Right ()

buildLiveSessionUsernameCollisionCandidate :: Text -> Text -> Text
buildLiveSessionUsernameCollisionCandidate base suffix =
  let trimmedBase = T.strip base
      trimmedSuffix = T.strip suffix
      suffixPart =
        if T.null trimmedSuffix
          then ""
          else "-" <> trimmedSuffix
      baseBudget =
        max 0 (liveSessionUsernameCollisionBudget - T.length suffixPart)
      basePrefix =
        if T.null suffixPart
          then T.take liveSessionUsernameCollisionBudget trimmedBase
          else T.take baseBudget trimmedBase
  in T.take liveSessionUsernameCollisionBudget (basePrefix <> suffixPart)

resolveLiveSessionMusicianLookup :: Maybe Text -> LiveSessionMusicianLookup
resolveLiveSessionMusicianLookup rawEmail =
  case T.toLower . T.strip <$> rawEmail of
    Just email | not (T.null email) -> LookupLiveSessionMusicianByEmail email
    _ -> CreateLiveSessionMusician

validateLiveSessionReferencedPartyEmail
  :: Maybe Text
  -> Maybe Text
  -> Either ServerError (Maybe Text)
validateLiveSessionReferencedPartyEmail rawExistingEmail rawSuppliedEmail =
  case suppliedEmail of
    Nothing ->
      Right existingEmail
    Just supplied
      | existingEmail == Just supplied ->
          Right existingEmail
      | otherwise ->
          Left err400
            { errBody =
                "Referenced musician email must match the existing party email"
            }
  where
    existingEmail = normalizeReferencedPartyEmail rawExistingEmail
    suppliedEmail = normalizeReferencedPartyEmail rawSuppliedEmail

normalizeReferencedPartyEmail :: Maybe Text -> Maybe Text
normalizeReferencedPartyEmail rawEmail =
  case T.toLower . T.strip <$> rawEmail of
    Just email | not (T.null email) -> Just email
    _ -> Nothing

validateLiveSessionTermsAcceptance :: Bool -> Maybe Text -> Either ServerError Text
validateLiveSessionTermsAcceptance acceptedTerms rawTermsVersion
  | not acceptedTerms =
      Left err400
        { errBody = "acceptedTerms must be true before submitting live session intake" }
  | otherwise =
      case T.strip <$> rawTermsVersion of
        Just termsVersion
          | T.null termsVersion ->
              missingTermsVersion
          | T.length termsVersion > liveSessionTermsVersionMaxLength ->
              Left err400
                { errBody =
                    BL.fromStrict
                      ( TE.encodeUtf8
                          ( "termsVersion must be "
                              <> T.pack (show liveSessionTermsVersionMaxLength)
                              <> " characters or fewer"
                          )
                      )
                }
          | T.any isControl termsVersion ->
              Left err400 { errBody = "termsVersion must not contain control characters" }
          | otherwise ->
              Right termsVersion
        _ ->
          missingTermsVersion
  where
    missingTermsVersion =
      Left err400 { errBody = "termsVersion is required when acceptedTerms is true" }

sanitizeLiveSessionRiderFileName :: Text -> Text
sanitizeLiveSessionRiderFileName rawName =
  let trimmed = T.strip rawName
      baseName = T.pack (takeFileName (T.unpack trimmed))
      cleaned = T.map normalizeRiderFileNameChar baseName
      stripped = T.dropWhile (== '-') (T.dropWhileEnd (== '-') cleaned)
      bounded = T.take liveSessionRiderFileNameMaxLength stripped
  in
    if T.null bounded || not (T.any isStableRiderFileNameChar bounded)
      then "rider"
      else bounded
  where
    isStableRiderFileNameChar ch = isAscii ch && isAlphaNum ch

    normalizeRiderFileNameChar ch
      | isStableRiderFileNameChar ch = ch
      | ch == '.' || ch == '-' || ch == '_' = ch
      | ch == ' ' = '-'
      | otherwise = '-'

withPool
  :: (MonadReader Env m, MonadIO m)
  => SqlPersistT IO a
  -> m a
withPool action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)
