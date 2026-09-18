{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.ServerLiveSessions
  ( liveSessionsServer
  , LiveSessionMusicianLookup(..)
  , buildLiveSessionUsernameCollisionCandidate
  , liveSessionMusicianPartyNotes
  , resolveLiveSessionMusicianLookup
  , selectUniqueLiveSessionMusicianByEmail
  , sanitizeLiveSessionRiderFileName
  , validateLiveSessionBandName
  , validateLiveSessionMusicianCount
  , validateLiveSessionOptionalEmail
  , validateLiveSessionReferencedPartyEmail
  , validateLiveSessionRiderFileName
  , validateLiveSessionRiderFileSize
  , validateLiveSessionTermsAcceptance
  ) where

import           Control.Monad              (forM_, unless, when, zipWithM)
import           Control.Exception          (throwIO, try, catch)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Data.Aeson                 (Value, encode, object, (.=))
import           Data.Char                  ( GeneralCategory
                                              ( Format
                                              , LineSeparator
                                              , ParagraphSeparator
                                              )
                                            , generalCategory
                                            , isAlphaNum
                                            , isAscii
                                            , isControl
                                            )
import           Data.Maybe                 (mapMaybe)
import           Data.Int                   (Int64)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (UTCTime, getCurrentTime)
import           Database.Persist
import           Database.PostgreSQL.Simple (SqlError(..))
import           Database.Persist.Sql       (SqlPersistT, Single(..), fromSqlKey, rawExecute, rawSql, runSqlPool, toSqlKey)
import           Servant
import           Servant.Multipart          (FileData(..), Tmp)
import           System.Directory           (createDirectoryIfMissing, doesFileExist, getFileSize)
import           System.FilePath            ((</>), takeFileName)
import qualified Data.ByteString.Lazy       as BL

import           TDF.API.LiveSessions
import           TDF.Auth                   (AuthedUser, auPartyId, ModuleAccess(..), hasModuleAccess)
import qualified TDF.Catalog.Models        as Catalog
import           TDF.DB                     (Env(..))
import           TDF.Models
import qualified TDF.Models                 as M
import qualified TDF.ModelsExtra           as ME
import           TDF.ServerAuth             (normalizeAuthEmailAddress)
import           Web.PathPieces             (fromPathPiece)

liveSessionUsernameCollisionBudget :: Int
liveSessionUsernameCollisionBudget = 60

liveSessionBandNameMaxLength :: Int
liveSessionBandNameMaxLength = 160

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

liveSessionMusicianPartyNotes :: Maybe Text -> Maybe Text
liveSessionMusicianPartyNotes rawInstrument =
  case T.strip <$> rawInstrument of
    Just instrument | not (T.null instrument) -> Just instrument
    _ -> Nothing

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
    intakeHandler requestKey payload = do
      key <- either throwError pure (validateLiveSessionRequestKey requestKey)
      bandName <- either throwError pure (validateLiveSessionBandName (lsiBandName payload))
      acceptedTermsVersion <-
        either throwError pure $
          validateLiveSessionTermsAcceptance
            (lsiAcceptedTerms payload)
            (lsiTermsVersion payload)
      contactEmail <-
        either throwError pure $
          validateLiveSessionOptionalEmail "contactEmail" (lsiContactEmail payload)
      either throwError pure $
        validateLiveSessionMusicianCount (lsiMusicians payload)
      now <- liftIO getCurrentTime
      rider <- traverse readRiderFile (lsiRider payload)
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

      pool <- asks envPool
      result <- liftIO $ try $ catch (runSqlPool (do
        _ <- rawSql "SELECT 1::bigint FROM pg_advisory_xact_lock(hashtextextended(?,0))"
          [PersistText ("live-intake:" <> T.pack (show (fromSqlKey (auPartyId user))) <> ":" <> key)] :: SqlPersistT IO [Single Int64]
        riderDigest <- traverse (digestBytes . snd) rider
        let body = TE.decodeUtf8 (BL.toStrict (encode (liveSessionRequestPayload payload (fst <$> rider) riderDigest)))
        previous <- rawSql "SELECT r.request_payload = ?::jsonb, i.rider_path FROM identity_live_intake_request r JOIN live_session_intake i ON i.id=r.intake_id WHERE r.actor_party_id=? AND r.request_key=?"
          [PersistText body, toPersistValue (auPartyId user), PersistText key]
        case previous of
          [(Single True, Single storedPath)] -> do
            restoredPath <- storeRequestRider key rider riderDigest
            unless (restoredPath == storedPath) $
              liftIO $ throwIO err409 { errBody = "The saved rider reference needs administrative review" }
            pure NoContent
          [(Single False, _)] -> liftIO $ throwIO err409 { errBody = "This submission changed after an earlier send. Review the saved intake before starting another submission." }
          [] -> do
            primaryGenreKey <- traverse resolvePublishedGenre (lsiPrimaryGenreId payload)
            musicianInstruments <- mapM (traverse resolvePublishedInstrument . lsmInstrumentId) (lsiMusicians payload)
            createIntake key body now bandName acceptedTermsVersion contactEmail primaryGenreKey musicianInstruments resolvedSongOrders rider riderDigest payload
          _ -> liftIO $ throwIO err500 { errBody = "Could not resolve intake request" }
        ) pool) (\(sqlError :: SqlError) ->
          if sqlState sqlError == "55000"
            then throwIO err409 { errBody = "An archived contact requires administrative review before this intake can be saved." }
            else throwIO sqlError)
      either (throwError :: ServerError -> m NoContent) pure result

    createIntake key body now bandName acceptedTermsVersion contactEmail primaryGenreKey musicianInstruments resolvedSongOrders rider riderDigest payload = do
      preparedMusicians <- zipWithM (ensureMusician now) musicianInstruments (lsiMusicians payload)
      riderPath <- storeRequestRider key rider riderDigest
      intakeId <- insert ME.LiveSessionIntake
        { ME.liveSessionIntakeBandName     = bandName
        , ME.liveSessionIntakeBandDescription = lsiBandDescription payload
        , ME.liveSessionIntakePrimaryGenre = Nothing
        , ME.liveSessionIntakePrimaryGenreId = primaryGenreKey
        , ME.liveSessionIntakeInputList    = lsiInputList payload
        , ME.liveSessionIntakeContactEmail = contactEmail
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

      forM_
          (zip preparedMusicians (lsiMusicians payload))
          $ \((partyKey, musicianEmail, instrumentKey), m) ->
              insert_ ME.LiveSessionMusician
                { ME.liveSessionMusicianIntakeId   = intakeId
                , ME.liveSessionMusicianPartyId    = partyKey
                , ME.liveSessionMusicianName       = lsmName m
                , ME.liveSessionMusicianEmail      = musicianEmail
                , ME.liveSessionMusicianInstrument = Nothing
                , ME.liveSessionMusicianInstrumentId = instrumentKey
                , ME.liveSessionMusicianRole       = Nothing
                , ME.liveSessionMusicianNotes      = lsmNotes m
                , ME.liveSessionMusicianIsExisting = lsmIsExisting m
                }

      forM_ preparedSongs $ \(sortOrder, title, song) ->
          insert_ ME.LiveSessionSong
            { ME.liveSessionSongIntakeId  = intakeId
            , ME.liveSessionSongTitle     = title
            , ME.liveSessionSongBpm       = lssBpm song
            , ME.liveSessionSongSongKey   = fmap T.strip (lssSongKey song)
            , ME.liveSessionSongLyrics    = lssLyrics song
            , ME.liveSessionSongSortOrder = sortOrder
            }

      rawExecute "INSERT INTO identity_live_intake_request(actor_party_id,request_key,request_payload,intake_id) VALUES (?,?,?::jsonb,?)"
        [toPersistValue (auPartyId user), PersistText key, PersistText body, toPersistValue intakeId]
      pure NoContent

    storeRequestRider key rider riderDigest =
      case (rider, riderDigest) of
        (Just (safeName, bytes), Just contentHash) -> do
          pathHash <- digestBytes (BL.fromStrict (TE.encodeUtf8 (T.pack (show (fromSqlKey (auPartyId user))) <> ":" <> key <> ":" <> contentHash)))
          Just <$> liftIO (storeRiderFile pathHash safeName bytes)
        _ -> pure Nothing

    ensureMusician
      :: UTCTime
      -> Maybe (Catalog.InstrumentId, Text)
      -> LiveSessionMusicianPayload
      -> SqlPersistT IO (Key Party, Maybe Text, Maybe Catalog.InstrumentId)
    ensureMusician now instrumentRef LiveSessionMusicianPayload{..} = do
      mEmail <- either (liftIO . throwIO) pure $
        validateLiveSessionOptionalEmail "musicians.email" lsmEmail
      let trimmedName = T.strip lsmName
      partyKey <- case lsmPartyId of
        Just pidInt -> do
          let key = toSqlKey (fromIntegral pidInt)
          unless (key == auPartyId user || hasModuleAccess ModuleCRM user) $
            liftIO $ throwIO err403 { errBody = "You cannot select this contact" }
          -- Do not expose contact information before checking access. The archive
          -- guard also revalidates this reference when the musician is inserted.
          existing <- get key
          case existing of
            Nothing -> liftIO $ throwIO err400 { errBody = "Referenced contact is unavailable" }
            Just party -> do
              _ <- either (liftIO . throwIO) pure $
                validateLiveSessionReferencedPartyEmail (M.partyPrimaryEmail party) mEmail
              pure key
        Nothing -> insert Party
          { partyLegalName = Nothing
          , partyDisplayName = if T.null trimmedName then "Músico Live Session" else trimmedName
          , partyIsOrg = False
          , partyTaxId = Nothing
          , partyPrimaryEmail = mEmail
          , partyPrimaryPhone = Nothing
          , partyWhatsapp = Nothing
          , partyInstagram = Nothing
          , partyEmergencyContact = Nothing
          , partyNotes = liveSessionMusicianPartyNotes (snd <$> instrumentRef)
          , partyStripeCustomerId = Nothing
          , partyCountryCode = Nothing
          , partyCountryId = Nothing
          , partyCreatedAt = now
          }
      -- Intake registers a contact, not authentication, privileges, or consent.
      pure (partyKey, mEmail, fst <$> instrumentRef)

    resolvePublishedGenre :: Text -> SqlPersistT IO Catalog.GenreId
    resolvePublishedGenre rawId = do
      genreKey <-
        maybe
          (liftIO $ throwIO err400 { errBody = "primaryGenreId must be a valid catalog UUID" })
          pure
          (fromPathPiece (T.strip rawId))
      valid <- do
        item <- get genreKey
        case item of
          Nothing -> pure False
          Just genre -> do
            state <- get (Catalog.genreWorkflowStateId genre)
            catalog <- get (Catalog.genreCatalogId genre)
            pure $
              Catalog.genreActive genre
                && maybe False ((== "published") . Catalog.workflowStateCode) state
                && maybe False (\definition -> Catalog.catalogDefinitionActive definition && Catalog.catalogDefinitionCode definition == "genres") catalog
      unless valid $
        liftIO $ throwIO err400 { errBody = "primaryGenreId must reference an active published genre" }
      pure genreKey

    resolvePublishedInstrument :: Text -> SqlPersistT IO (Catalog.InstrumentId, Text)
    resolvePublishedInstrument rawId = do
      instrumentKey <-
        maybe
          (liftIO $ throwIO err400 { errBody = "instrumentId must be a valid catalog UUID" })
          pure
          (fromPathPiece (T.strip rawId))
      result <- do
        item <- get instrumentKey
        case item of
          Nothing -> pure Nothing
          Just instrument -> do
            state <- get (Catalog.instrumentWorkflowStateId instrument)
            catalog <- get (Catalog.instrumentCatalogId instrument)
            pure $
              if Catalog.instrumentActive instrument
                && maybe False ((== "published") . Catalog.workflowStateCode) state
                && maybe False (\definition -> Catalog.catalogDefinitionActive definition && Catalog.catalogDefinitionCode definition == "instruments") catalog
                then Just (Catalog.instrumentNameEs instrument)
                else Nothing
      label <- maybe
        (liftIO $ throwIO err400 { errBody = "instrumentId must reference an active published instrument" })
        pure
        result
      pure (instrumentKey, label)

    readRiderFile :: FileData Tmp -> m (Text, BL.ByteString)
    readRiderFile FileData{..} = do
      safeName <- either throwError pure (validateLiveSessionRiderFileName fdFileName)
      size <- liftIO (getFileSize fdPayload)
      either throwError pure (validateLiveSessionRiderFileSize size)
      bytes <- liftIO (BL.readFile fdPayload)
      either throwError pure (validateLiveSessionRiderFileSize (fromIntegral (BL.length bytes)))
      pure (safeName, bytes)

-- A retry uses the same path; a partial write is detected rather than overwritten.
storeRiderFile :: Text -> Text -> BL.ByteString -> IO Text
storeRiderFile pathHash safeName bytes = do
  let destDir = "uploads/live-sessions"
      destPath = destDir </> T.unpack pathHash <> "-" <> T.unpack safeName
  createDirectoryIfMissing True destDir
  exists <- doesFileExist destPath
  if exists
    then do
      stored <- BL.readFile destPath
      unless (stored == bytes) $ throwIO err409 { errBody = "The previous rider upload needs review before retrying" }
    else BL.writeFile destPath bytes
  pure (T.pack destPath)

digestBytes :: BL.ByteString -> SqlPersistT IO Text
digestBytes bytes = do
  rows <- rawSql "SELECT encode(digest(?::bytea,'sha256'),'hex')" [PersistByteString (BL.toStrict bytes)]
  case rows of
    [Single digest] -> pure digest
    _ -> liftIO $ throwIO err500 { errBody = "Could not identify intake upload" }

validateLiveSessionRequestKey :: Maybe Text -> Either ServerError Text
validateLiveSessionRequestKey (Just key)
  | T.length key >= 16 && T.length key <= 128
  , T.all (\ch -> isAscii ch && (isAlphaNum ch || ch == '-' || ch == '_')) key = Right key
validateLiveSessionRequestKey _ = Left err400
  { errBody = "Refresh the Live Session form before submitting so retries can be saved safely." }

liveSessionRequestPayload :: LiveSessionIntakePayload -> Maybe Text -> Maybe Text -> Value
liveSessionRequestPayload LiveSessionIntakePayload{..} riderName riderDigest = object
  [ "bandName" .= lsiBandName, "bandDescription" .= lsiBandDescription
  , "primaryGenreId" .= lsiPrimaryGenreId, "inputList" .= lsiInputList
  , "contactEmail" .= lsiContactEmail, "contactPhone" .= lsiContactPhone
  , "sessionDate" .= lsiSessionDate, "availability" .= lsiAvailability
  , "acceptedTerms" .= lsiAcceptedTerms, "termsVersion" .= lsiTermsVersion
  , "riderName" .= riderName, "riderSha256" .= riderDigest
  , "musicians" .= map (\LiveSessionMusicianPayload{..} -> object
      [ "partyId" .= lsmPartyId, "name" .= lsmName, "email" .= lsmEmail
      , "instrumentId" .= lsmInstrumentId, "notes" .= lsmNotes, "isExisting" .= lsmIsExisting ]) lsiMusicians
  , "setlist" .= map (\LiveSessionSongPayload{..} -> object
      [ "title" .= lssTitle, "bpm" .= lssBpm, "songKey" .= lssSongKey
      , "lyrics" .= lssLyrics, "sortOrder" .= lssSortOrder ]) lsiSetlist
  ]

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

validateLiveSessionBandName :: Text -> Either ServerError Text
validateLiveSessionBandName rawBandName
  | T.null bandName =
      Left err400 { errBody = "bandName is required" }
  | T.length bandName > liveSessionBandNameMaxLength =
      Left err400
        { errBody =
            BL.fromStrict
              ( TE.encodeUtf8
                  ( "bandName must be "
                      <> T.pack (show liveSessionBandNameMaxLength)
                      <> " characters or fewer"
                  )
              )
        }
  | T.any isUnsafeLiveSessionBandNameChar bandName =
      Left err400
        { errBody =
            "bandName must not contain control characters or hidden formatting characters"
        }
  | otherwise =
      Right bandName
  where
    bandName = T.strip rawBandName

isUnsafeLiveSessionBandNameChar :: Char -> Bool
isUnsafeLiveSessionBandNameChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

resolveLiveSessionMusicianLookup :: Maybe Text -> LiveSessionMusicianLookup
resolveLiveSessionMusicianLookup rawEmail =
  case rawEmail >>= normalizeAuthEmailAddress of
    Just email -> LookupLiveSessionMusicianByEmail email
    _ -> CreateLiveSessionMusician

selectUniqueLiveSessionMusicianByEmail
  :: [Entity Party]
  -> Either ServerError (Maybe (Entity Party))
selectUniqueLiveSessionMusicianByEmail [] = Right Nothing
selectUniqueLiveSessionMusicianByEmail [partyEnt]
  | fromSqlKey (entityKey partyEnt) <= 0 =
      Left err500 { errBody = "Stored live-session musician party id is invalid" }
  | otherwise = Right (Just partyEnt)
selectUniqueLiveSessionMusicianByEmail _ =
  Left err409 { errBody = "Multiple parties match this musician email" }

validateLiveSessionMusicianCount
  :: [LiveSessionMusicianPayload]
  -> Either ServerError ()
validateLiveSessionMusicianCount [] =
  Left err400 { errBody = "At least one musician is required for live-session intake" }
validateLiveSessionMusicianCount musicians
  | length musicians > maxLiveSessionMusicians =
      Left err400
        { errBody =
            BL.fromStrict
              ( TE.encodeUtf8
                  ( "musicians must contain at most "
                      <> T.pack (show maxLiveSessionMusicians)
                      <> " entries"
                  )
              )
        }
  | any invalidPartyId musicians =
      Left err400 { errBody = "musician partyId must be a positive integer" }
  | hasDuplicates referencedPartyIds =
      Left err400 { errBody = "referenced musician partyIds must be distinct" }
  | otherwise =
      Right ()
  where
    invalidPartyId musician =
      maybe False (<= 0) (lsmPartyId musician)

    referencedPartyIds =
      mapMaybe lsmPartyId musicians


hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates = go Set.empty
  where
    go _ [] = False
    go seen (value : rest)
      | Set.member value seen = True
      | otherwise = go (Set.insert value seen) rest

validateLiveSessionOptionalEmail
  :: Text
  -> Maybe Text
  -> Either ServerError (Maybe Text)
validateLiveSessionOptionalEmail _ Nothing = Right Nothing
validateLiveSessionOptionalEmail fieldName (Just rawEmail)
  | T.null (T.strip rawEmail) =
      Right Nothing
  | Just email <- normalizeAuthEmailAddress rawEmail =
      Right (Just email)
  | otherwise =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 (fieldName <> " must be a valid email address"))
        }

validateLiveSessionReferencedPartyEmail
  :: Maybe Text
  -> Maybe Text
  -> Either ServerError (Maybe Text)
validateLiveSessionReferencedPartyEmail rawExistingEmail rawSuppliedEmail = do
  suppliedEmail <- validateLiveSessionOptionalEmail "musicians.email" rawSuppliedEmail
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
    existingEmail = rawExistingEmail >>= normalizeAuthEmailAddress

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
          | T.any isUnsafeLiveSessionTermsVersionChar termsVersion ->
              Left err400
                { errBody =
                    "termsVersion must not contain control characters or hidden formatting characters"
                }
          | otherwise ->
              Right termsVersion
        _ ->
          missingTermsVersion
  where
    missingTermsVersion =
      Left err400 { errBody = "termsVersion is required when acceptedTerms is true" }

isUnsafeLiveSessionTermsVersionChar :: Char -> Bool
isUnsafeLiveSessionTermsVersionChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateLiveSessionRiderFileName :: Text -> Either ServerError Text
validateLiveSessionRiderFileName rawName
  | T.null trimmed =
      Left err400 { errBody = "rider file name is required" }
  | T.any isUnsafeRiderFileNameChar trimmed =
      Left err400
        { errBody =
            "rider file name must not contain control characters or hidden formatting characters"
        }
  | T.any isPathSeparator trimmed =
      Left err400 { errBody = "rider file name must not contain path separators" }
  | T.length trimmed > liveSessionRiderFileNameMaxLength =
      Left err400
        { errBody =
            BL.fromStrict
              ( TE.encodeUtf8
                  ( "rider file name must be "
                      <> T.pack (show liveSessionRiderFileNameMaxLength)
                      <> " characters or fewer"
                  )
              )
        }
  | sanitized == "rider" && trimmed /= "rider" =
      Left err400 { errBody = "rider file name must include a usable name" }
  | hasDisallowedLiveSessionRiderExtension sanitized =
      Left err400 { errBody = "rider file name extension is not allowed" }
  | otherwise =
      Right sanitized
  where
    trimmed = T.strip rawName
    sanitized = sanitizeLiveSessionRiderFileName trimmed

isUnsafeRiderFileNameChar :: Char -> Bool
isUnsafeRiderFileNameChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

isPathSeparator :: Char -> Bool
isPathSeparator ch = ch == '/' || ch == '\\'

hasDisallowedLiveSessionRiderExtension :: Text -> Bool
hasDisallowedLiveSessionRiderExtension name =
  any (`elem` extensionChain) disallowedLiveSessionRiderExtensions
  where
    loweredName = T.toLower name
    extensionChain = map ("." <>) (drop 1 (T.splitOn "." loweredName))

disallowedLiveSessionRiderExtensions :: [Text]
disallowedLiveSessionRiderExtensions =
  [ ".bat"
  , ".cmd"
  , ".com"
  , ".exe"
  , ".htm"
  , ".html"
  , ".jar"
  , ".js"
  , ".mjs"
  , ".php"
  , ".ps1"
  , ".scr"
  , ".sh"
  , ".svg"
  , ".svgz"
  , ".xhtml"
  ]

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
