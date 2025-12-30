{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerAdmin
  ( adminServer
  ) where

import           Control.Exception      (SomeException, try)
import           Control.Monad          (unless, when)
import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Crypto.BCrypt          (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Data.Foldable          (for_)
import           Data.Char              (isAlphaNum)
import           Data.Maybe             (catMaybes, fromMaybe, isJust)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString.Lazy   as BL
import           Data.Time              (diffUTCTime, getCurrentTime)
import           Database.Persist       ( (==.), (!=.)
                                        , (=.)
                                        , Entity(..)
                                        , Update
                                        , SelectOpt(..)
                                        , selectFirst
                                        , selectList
                                        , get
                                        , getBy
                                        , getJust
                                        , update
                                        , getEntity
                                        , getJustEntity
                                        , insert
                                        , upsert
                                        )
import           Database.Persist.Sql   (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant
import           Web.PathPieces         (PathPiece, fromPathPiece, toPathPiece)

import           TDF.API.Admin          ( AdminAPI
                                        , BrainEntryCreate(..)
                                        , BrainEntryDTO(..)
                                        , BrainEntryUpdate(..)
                                        , EmailTestRequest(..)
                                        , EmailTestResponse(..)
                                        , RagIndexStatus(..)
                                        , RagRefreshResponse(..)
                                        )
import           TDF.API.Types          ( DropdownOptionCreate(..)
                                        , DropdownOptionDTO(..)
                                        , DropdownOptionUpdate(..)
                                        , RoleDetailDTO(..)
                                        , UserAccountCreate(..)
                                        , UserAccountDTO(..)
                                        , UserAccountUpdate(..)
                                        )
import           TDF.DTO                ( ArtistProfileUpsert(..)
                                        , ArtistReleaseDTO(..)
                                        , ArtistReleaseUpsert(..)
                                        )
import           TDF.Auth               ( AuthedUser
                                        , ModuleAccess(..)
                                        , hasModuleAccess
                                        , moduleName
                                        , modulesForRoles
                                        )
import           TDF.DB                 (Env(..))
import           TDF.Config             (ragRefreshHours)
import           TDF.Models
import           TDF.ModelsExtra (DropdownOption(..))
import qualified TDF.ModelsExtra as ME
import           TDF.Seed               (seedAll)
import qualified TDF.Email              as Email
import qualified TDF.Email.Service      as EmailSvc
import           TDF.Profiles.Artist    ( loadAllArtistProfilesDTO
                                        , upsertArtistProfileRecord
                                        )
import           TDF.LogBuffer          ( LogEntry(..), LogLevel(..), addLog, getRecentLogs, clearLogs )
import           TDF.DTO                ( LogEntryDTO(..) )
import           TDF.RagStore           (getRagIndexStats, refreshRagIndex)
import           System.IO              (hPutStrLn, stderr)

adminServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT AdminAPI m
adminServer user =
       seedHandler
  :<|> dropdownRouter
  :<|> usersRouter
  :<|> rolesHandler
  :<|> artistsRouter
  :<|> logsRouter
  :<|> emailTestHandler
  :<|> brainRouter
  :<|> ragRouter
  where
    seedHandler = do
      ensureModule ModuleAdmin user
      withPool seedAll
      pure NoContent

    dropdownRouter rawCategory =
           listOptions rawCategory
      :<|> createOption rawCategory
      :<|> updateOption rawCategory

    usersRouter =
           listUsers
      :<|> createUser
      :<|> userById

    userById userId =
           getUser userId
      :<|> updateUser userId

    rolesHandler = do
      ensureModule ModuleAdmin user
      pure (map roleDetail [minBound .. maxBound])

    artistsRouter =
      (listArtistProfilesAdmin :<|> upsertArtistProfileAdmin)
      :<|> (createArtistReleaseAdmin :<|> updateArtistReleaseAdmin)

    logsRouter =
      getLogs :<|> clearLogsHandler

    emailTestHandler EmailTestRequest{..} = do
      ensureModule ModuleAdmin user
      cfg <- asks envConfig
      let emailSvc = EmailSvc.mkEmailService cfg
          subj = fromMaybe "Correo de prueba TDF" etrSubject
          body = maybe ["Correo de prueba desde TDF HQ."] (\txt -> [txt]) etrBody
          targetName = fromMaybe "" etrName
          preMsg = "[Admin][EmailTest] Sending to " <> etrEmail <> " | subject: " <> subj
      liftIO $ addLog LogInfo preMsg
      sendResult <- liftIO $ try $
        EmailSvc.sendTestEmail
          emailSvc
          targetName
          etrEmail
          subj
          body
          etrCtaUrl
      case sendResult of
        Left (err :: SomeException) -> do
          let msg = "[Admin][EmailTest] Failed for " <> etrEmail <> ": " <> T.pack (show err)
          liftIO $ addLog LogError msg
          liftIO $ hPutStrLn stderr (T.unpack msg)
          pure EmailTestResponse { status = "error", message = Just msg }
        Right () -> do
          let msg = "[Admin][EmailTest] Sent to " <> etrEmail
          liftIO $ addLog LogInfo msg
          liftIO $ hPutStrLn stderr (T.unpack msg)
          pure EmailTestResponse { status = "sent", message = Nothing }

    brainRouter =
      brainListHandler :<|> brainCreateHandler :<|> brainUpdateHandler

    ragRouter =
      ragStatusHandler :<|> ragRefreshHandler

    brainListHandler mIncludeInactive = do
      ensureModule ModuleAdmin user
      rows <- withPool $ do
        let filters =
              case mIncludeInactive of
                Just True -> []
                _ -> [ME.StudioBrainEntryActive ==. True]
        selectList filters [Desc ME.StudioBrainEntryUpdatedAt, LimitTo 200]
      pure (map brainEntryToDTO rows)

    brainCreateHandler BrainEntryCreate{..} = do
      ensureModule ModuleAdmin user
      let title = T.strip becTitle
          body = T.strip becBody
          category = cleanMaybe becCategory
          tags = cleanTags becTags
          active = fromMaybe True becActive
      when (T.null title) $ throwError err400 { errBody = "Título requerido" }
      when (T.null body) $ throwError err400 { errBody = "Contenido requerido" }
      now <- liftIO getCurrentTime
      entryId <- withPool $ insert ME.StudioBrainEntry
        { ME.studioBrainEntryTitle = title
        , ME.studioBrainEntryBody = body
        , ME.studioBrainEntryCategory = category
        , ME.studioBrainEntryTags = tags
        , ME.studioBrainEntryActive = active
        , ME.studioBrainEntryCreatedAt = now
        , ME.studioBrainEntryUpdatedAt = now
        }
      row <- withPool $ getJust entryId
      pure (brainEntryToDTO (Entity entryId row))

    brainUpdateHandler entryId BrainEntryUpdate{..} = do
      ensureModule ModuleAdmin user
      let entryKey = toSqlKey entryId :: ME.StudioBrainEntryId
          titleUpdate = T.strip <$> beuTitle
          bodyUpdate = T.strip <$> beuBody
      for_ titleUpdate $ \t -> when (T.null t) $
        throwError err400 { errBody = "Título requerido" }
      for_ bodyUpdate $ \t -> when (T.null t) $
        throwError err400 { errBody = "Contenido requerido" }
      now <- liftIO getCurrentTime
      let tagsUpdate =
            case beuTags of
              Nothing -> Nothing
              Just tags -> Just (ME.StudioBrainEntryTags =. cleanTags (Just tags))
          updates = catMaybes
            [ (ME.StudioBrainEntryTitle =.) <$> titleUpdate
            , (ME.StudioBrainEntryBody =.) <$> bodyUpdate
            , (ME.StudioBrainEntryCategory =.) <$> fmap cleanMaybe beuCategory
            , tagsUpdate
            , (ME.StudioBrainEntryActive =.) <$> beuActive
            ]
      let updates' = updates <> [ME.StudioBrainEntryUpdatedAt =. now]
      result <- withPool $ do
        mEntry <- get entryKey
        case mEntry of
          Nothing -> pure (Left err404)
          Just _ -> do
            if null updates
              then pure (Left err400 { errBody = "Sin cambios para actualizar" })
              else do
                update entryKey updates'
                Right <$> getEntity entryKey
      case result of
        Left err -> throwError err
        Right maybeEntry -> maybe (throwError err404) (pure . brainEntryToDTO) maybeEntry

    ragStatusHandler = do
      ensureModule ModuleAdmin user
      pool <- asks envPool
      cfg <- asks envConfig
      (countVal, updatedAt) <- liftIO $ getRagIndexStats pool
      now <- liftIO getCurrentTime
      let stale = case updatedAt of
            Nothing -> True
            Just ts ->
              let hours = realToFrac (diffUTCTime now ts) / 3600 :: Double
              in hours >= fromIntegral (ragRefreshHours cfg)
      pure RagIndexStatus
        { risCount = countVal
        , risUpdatedAt = updatedAt
        , risStale = stale
        }

    ragRefreshHandler = do
      ensureModule ModuleAdmin user
      pool <- asks envPool
      cfg <- asks envConfig
      result <- liftIO $ refreshRagIndex cfg pool
      case result of
        Left err ->
          throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 err) }
        Right chunkCount ->
          pure RagRefreshResponse
            { rrrStatus = "ok"
            , rrrChunks = chunkCount
      }

    brainEntryToDTO (Entity key entry) = BrainEntryDTO
      { bedId = fromSqlKey key
      , bedTitle = ME.studioBrainEntryTitle entry
      , bedBody = ME.studioBrainEntryBody entry
      , bedCategory = ME.studioBrainEntryCategory entry
      , bedTags = fromMaybe [] (ME.studioBrainEntryTags entry)
      , bedActive = ME.studioBrainEntryActive entry
      , bedUpdatedAt = ME.studioBrainEntryUpdatedAt entry
      }

    cleanMaybe Nothing = Nothing
    cleanMaybe (Just txt) =
      let trimmed = T.strip txt
      in if T.null trimmed then Nothing else Just trimmed

    cleanTags Nothing = Nothing
    cleanTags (Just tags) =
      let cleaned = filter (not . T.null) (map T.strip tags)
      in if null cleaned then Nothing else Just cleaned

    roleDetail role = RoleDetailDTO
      { role    = role
      , label   = roleToText role
      , modules = map moduleName (Set.toList (modulesForRoles [role]))
      }

    listArtistProfilesAdmin = do
      ensureModule ModuleAdmin user
      withPool loadAllArtistProfilesDTO

    upsertArtistProfileAdmin payload@ArtistProfileUpsert{..} = do
      ensureModule ModuleAdmin user
      let artistKey = toSqlKey apuArtistId
      mParty <- withPool $ get artistKey
      case mParty of
        Nothing -> throwError err404
        Just _ -> do
          now <- liftIO getCurrentTime
          dto <- withPool $ upsertArtistProfileRecord artistKey payload now
          pure dto

    createArtistReleaseAdmin ArtistReleaseUpsert{..} = do
      ensureModule ModuleAdmin user
      now <- liftIO getCurrentTime
      let artistKey = toSqlKey aruArtistId
      dto <- withPool $ do
        mProfile <- getBy (UniqueArtistProfile artistKey)
        case mProfile of
          Nothing -> pure Nothing
          Just _ -> do
            releaseId <- insert ArtistRelease
              { artistReleaseArtistPartyId = artistKey
              , artistReleaseTitle         = aruTitle
              , artistReleaseReleaseDate   = aruReleaseDate
              , artistReleaseDescription   = aruDescription
              , artistReleaseCoverImageUrl = aruCoverImageUrl
              , artistReleaseSpotifyUrl    = aruSpotifyUrl
              , artistReleaseYoutubeUrl    = aruYoutubeUrl
              , artistReleaseCreatedAt     = now
              }
            entity <- getJustEntity releaseId
            pure (Just (artistReleaseEntityToDTOAdmin entity))
      maybe (throwError err404) pure dto

    updateArtistReleaseAdmin releaseId ArtistReleaseUpsert{..} = do
      ensureModule ModuleAdmin user
      let releaseKey = toSqlKey releaseId
          artistKey  = toSqlKey aruArtistId
      mRelease <- withPool $ get releaseKey
      case mRelease of
        Nothing -> throwError err404
        Just release -> do
          when (artistReleaseArtistPartyId release /= artistKey) $
            throwError err400 { errBody = "El release no pertenece a este artista" }
          withPool $ update releaseKey
            [ ArtistReleaseTitle        =. aruTitle
            , ArtistReleaseReleaseDate  =. aruReleaseDate
            , ArtistReleaseDescription  =. aruDescription
            , ArtistReleaseCoverImageUrl =. aruCoverImageUrl
            , ArtistReleaseSpotifyUrl   =. aruSpotifyUrl
            , ArtistReleaseYoutubeUrl   =. aruYoutubeUrl
            ]
          entity <- withPool $ getJustEntity releaseKey
          pure (artistReleaseEntityToDTOAdmin entity)

    listOptions rawCategory mIncludeInactive = do
      ensureModule ModuleAdmin user
      let categoryKey = normaliseCategory rawCategory
          includeInactive = fromMaybe False mIncludeInactive
          filters = [ME.DropdownOptionCategory ==. categoryKey]
                 ++ [ME.DropdownOptionActive ==. True | not includeInactive]
          ordering =
            [ Asc ME.DropdownOptionSortOrder
            , Asc ME.DropdownOptionLabel
            , Asc ME.DropdownOptionValue
            ]
      entities <- withPool $ selectList filters ordering
      pure (map toDTO entities)

    createOption rawCategory DropdownOptionCreate{..} = do
      ensureModule ModuleAdmin user
      let categoryKey = normaliseCategory rawCategory
          valueTxt    = T.strip docValue
      when (T.null valueTxt) $
        throwError err400 { errBody = "Value is required" }
      let labelValue    = normaliseText docLabel
          sortOrderValue = docSortOrder
          activeValue    = fromMaybe True docActive
      conflict <- withPool $ selectFirst
        [ ME.DropdownOptionCategory ==. categoryKey
        , ME.DropdownOptionValue ==. valueTxt
        ]
        []
      when (isJust conflict) $
        throwError err409 { errBody = "Option already exists for category" }
      now <- liftIO getCurrentTime
      entity <- withPool $ do
        optionId <- insert DropdownOption
          { dropdownOptionCategory  = categoryKey
          , dropdownOptionValue     = valueTxt
          , dropdownOptionLabel     = labelValue
          , dropdownOptionActive    = activeValue
          , dropdownOptionSortOrder = sortOrderValue
          , dropdownOptionCreatedAt = now
          , dropdownOptionUpdatedAt = now
          }
        getJustEntity optionId
      pure (toDTO entity)

    updateOption rawCategory rawId DropdownOptionUpdate{..} = do
      ensureModule ModuleAdmin user
      let categoryKey = normaliseCategory rawCategory
          valueUpdate = fmap T.strip douValue
      when (maybe False T.null valueUpdate) $
        throwError err400 { errBody = "Value must not be empty" }
      optionId <- parseKey rawId
      mOption <- withPool $ getEntity optionId
      case mOption of
        Nothing -> throwError err404
        Just (Entity key option)
          | dropdownOptionCategory option /= categoryKey -> throwError err404
          | otherwise -> do
              case valueUpdate of
                Nothing -> pure ()
                Just newValue -> do
                  conflict <- withPool $ selectFirst
                    [ ME.DropdownOptionCategory ==. categoryKey
                    , ME.DropdownOptionValue ==. newValue
                    , ME.DropdownOptionId !=. key
                    ]
                    []
                  when (isJust conflict) $
                    throwError err409 { errBody = "Option already exists for category" }
              let labelUpdate = fmap normaliseText douLabel
                  sortOrderUpdate = douSortOrder
                  activeUpdate = douActive
              now <- liftIO getCurrentTime
              let baseUpdates = concat
                    [ maybe [] (\v -> [ME.DropdownOptionValue =. v]) valueUpdate
                    , maybe [] (\lbl -> [ME.DropdownOptionLabel =. lbl]) labelUpdate
                    , maybe [] (\s -> [ME.DropdownOptionSortOrder =. s]) sortOrderUpdate
                    , maybe [] (\flag -> [ME.DropdownOptionActive =. flag]) activeUpdate
                    ]
                  updates = if null baseUpdates
                    then []
                    else baseUpdates ++ [ME.DropdownOptionUpdatedAt =. now]
              entity <- if null updates
                then pure (Entity key option)
                else withPool $ do
                  update key updates
                  getJustEntity key
              pure (toDTO entity)

    listUsers mIncludeInactive = do
      ensureModule ModuleAdmin user
      let includeInactive = fromMaybe False mIncludeInactive
          filters = [UserCredentialActive ==. True | not includeInactive]
      withPool $ do
        creds <- selectList filters [Asc UserCredentialId]
        mapM loadUserAccount creds

    createUser UserAccountCreate{..} = do
      ensureModule ModuleAdmin user
      config <- asks envConfig
      let partyKey = toSqlKey uacPartyId :: PartyId
          activeValue = fromMaybe True uacActive
          emailSvc = EmailSvc.mkEmailService config
      partyEnt <- do
        mParty <- withPool $ getEntity partyKey
        maybe (throwError err404 { errBody = "Party not found" }) pure mParty
      emailAddress <- case fmap T.strip (partyPrimaryEmail (entityVal partyEnt)) of
        Nothing -> throwError err400 { errBody = "Party must have a primary email before creating a user" }
        Just addr | T.null addr -> throwError err400 { errBody = "Party must have a primary email before creating a user" }
                  | otherwise -> pure addr
      baseUsername <-
        case normalizeUsername =<< uacUsername of
          Just provided -> pure provided
          Nothing       -> pure (deriveUsername partyEnt emailAddress)
      uniqueUsername <- generateUniqueUsername baseUsername
      let providedPassword =
            uacPassword >>= (\txt -> let trimmed = T.strip txt in if T.null trimmed then Nothing else Just trimmed)
      tempPassword <- maybe (liftIO Email.generateTempPassword) pure providedPassword
      hashed <- liftIO (hashPasswordText tempPassword)
      credId <- withPool $ insert UserCredential
        { userCredentialPartyId      = partyKey
        , userCredentialUsername     = uniqueUsername
        , userCredentialPasswordHash = hashed
        , userCredentialActive       = activeValue
        }
      for_ uacRoles $ \rolesList -> withPool $ setPartyRoles partyKey rolesList
      account <- withPool $ do
        credEnt <- getJustEntity credId
        loadUserAccount credEnt
      liftIO $
        EmailSvc.sendWelcome
          emailSvc
          (partyDisplayName (entityVal partyEnt))
          emailAddress
          uniqueUsername
          tempPassword
      pure account
      where
        normalizeUsername :: Text -> Maybe Text
        normalizeUsername txt =
          let lowered = T.toLower (T.strip txt)
              cleaned = T.filter (\c -> isAlphaNum c || c `elem` (".-_" :: String)) lowered
          in if T.null cleaned then Nothing else Just cleaned

        deriveUsername :: Entity Party -> Text -> Text
        deriveUsername (Entity pid party) emailVal
          | not (T.null emailVal) = T.toLower emailVal
          | otherwise =
              let slug = T.filter (\c -> isAlphaNum c || c == '.') . T.toLower $ partyDisplayName party
              in if T.null slug
                   then "tdf-user-" <> T.pack (show (fromSqlKey pid))
                   else slug

        generateUniqueUsername base = go (0 :: Int)
          where
            go attempt = do
              let suffix = if attempt == 0 then "" else "-" <> T.pack (show attempt)
                  candidate = T.take 60 (base <> suffix)
              conflict <- withPool $ getBy (UniqueCredentialUsername candidate)
              case conflict of
                Nothing -> pure candidate
                Just _  -> go (attempt + 1)

    getUser userId = do
      ensureModule ModuleAdmin user
      let credKey = toSqlKey userId :: UserCredentialId
      mCred <- withPool $ getEntity credKey
      case mCred of
        Nothing       -> throwError err404
        Just credEnt  -> withPool (loadUserAccount credEnt)

    updateUser userId UserAccountUpdate{..} = do
      ensureModule ModuleAdmin user
      let credKey         = toSqlKey userId :: UserCredentialId
          usernameUpdate  = T.strip <$> uauUsername
      when (maybe False T.null usernameUpdate) $
        throwError err400 { errBody = "Username must not be empty" }
      passwordHash <- case uauPassword of
        Nothing -> pure Nothing
        Just rawPwd ->
          let trimmed = T.strip rawPwd
          in if T.null trimmed
               then throwError err400 { errBody = "Password must not be empty" }
               else Just <$> liftIO (hashPasswordText trimmed)
      mCred <- withPool $ getEntity credKey
      case mCred of
        Nothing -> throwError err404
        Just (Entity _ cred) -> do
          for_ usernameUpdate $ \newUsername ->
            when (newUsername /= userCredentialUsername cred) $ do
              conflict <- withPool $ getBy (UniqueCredentialUsername newUsername)
              case conflict of
                Just (Entity otherId _) | otherId /= credKey ->
                  throwError err409 { errBody = "Username already exists" }
                _ -> pure ()
          let updates :: [Update UserCredential]
              updates = concat
                [ maybe [] (\newUsername -> [UserCredentialUsername =. newUsername]) usernameUpdate
                , maybe [] (\flag -> [UserCredentialActive =. flag]) uauActive
                , maybe [] (\hash -> [UserCredentialPasswordHash =. hash]) passwordHash
                ]
          when (not (null updates)) $
            withPool $ update credKey updates
          for_ uauRoles $ \rolesList -> withPool $ setPartyRoles (userCredentialPartyId cred) rolesList
          withPool $ do
            fresh <- getJustEntity credKey
            loadUserAccount fresh

withPool
  :: (MonadReader Env m, MonadIO m)
  => SqlPersistT IO a
  -> m a
withPool action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

parseKey
  :: forall record m.
     ( PathPiece (Key record)
     , MonadError ServerError m
     )
  => Text
  -> m (Key record)
parseKey raw =
  maybe (throwError err400 { errBody = "Invalid identifier" }) pure (fromPathPiece raw)

normaliseCategory :: Text -> Text
normaliseCategory = T.toLower . T.strip

normaliseText :: Maybe Text -> Maybe Text
normaliseText Nothing = Nothing
normaliseText (Just txt) =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just trimmed

ensureModule
  :: (MonadError ServerError m)
  => ModuleAccess
  -> AuthedUser
  -> m ()
ensureModule moduleTag user =
  unless (hasModuleAccess moduleTag user) $
    throwError err403 { errBody = "Missing required module access" }

loadUserAccount :: Entity UserCredential -> SqlPersistT IO UserAccountDTO
loadUserAccount (Entity credId cred) = do
  party <- getJustEntity (userCredentialPartyId cred)
  roles <- selectList
    [ PartyRolePartyId ==. userCredentialPartyId cred
    , PartyRoleActive ==. True
    ]
    [Asc PartyRoleRole]
  let roleList = map (partyRoleRole . entityVal) roles
  pure UserAccountDTO
    { userId    = fromSqlKey credId
    , partyId   = fromSqlKey (entityKey party)
    , partyName = partyDisplayName (entityVal party)
    , username  = userCredentialUsername cred
    , active    = userCredentialActive cred
    , roles     = roleList
    , modules   = map moduleName (Set.toList (modulesForRoles roleList))
    }

setPartyRoles :: PartyId -> [RoleEnum] -> SqlPersistT IO ()
setPartyRoles partyKey rolesList = do
  existing <- selectList [PartyRolePartyId ==. partyKey] []
  let desired = Set.fromList rolesList
  for_ (Set.toList desired) $ \role -> do
    _ <- upsert (PartyRole partyKey role True) [PartyRoleActive =. True]
    pure ()
  for_ existing $ \(Entity roleId partyRole) ->
    when (partyRoleActive partyRole && Set.notMember (partyRoleRole partyRole) desired) $
      update roleId [PartyRoleActive =. False]

artistReleaseEntityToDTOAdmin :: Entity ArtistRelease -> ArtistReleaseDTO
artistReleaseEntityToDTOAdmin (Entity releaseId release) =
  ArtistReleaseDTO
    { arArtistId      = fromSqlKey (artistReleaseArtistPartyId release)
    , arReleaseId     = fromSqlKey releaseId
    , arTitle         = artistReleaseTitle release
    , arReleaseDate   = artistReleaseReleaseDate release
    , arDescription   = artistReleaseDescription release
    , arCoverImageUrl = artistReleaseCoverImageUrl release
    , arSpotifyUrl    = artistReleaseSpotifyUrl release
    , arYoutubeUrl    = artistReleaseYoutubeUrl release
    }

hashPasswordText :: Text -> IO Text
hashPasswordText pwd = do
  let raw = TE.encodeUtf8 pwd
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy raw
  case mHash of
    Nothing   -> fail "Failed to hash password"
    Just hash -> pure (TE.decodeUtf8 hash)

toDTO :: Entity DropdownOption -> DropdownOptionDTO
toDTO (Entity key option) = DropdownOptionDTO
  { optionId  = toPathPiece key
  , category  = dropdownOptionCategory option
  , value     = dropdownOptionValue option
  , label     = dropdownOptionLabel option
  , active    = dropdownOptionActive option
  , sortOrder = dropdownOptionSortOrder option
  }

-- Log handlers
getLogs :: (MonadIO m) => Maybe Int -> m [LogEntryDTO]
getLogs mLimit = do
  let limit = fromMaybe 100 mLimit
  entries <- liftIO $ getRecentLogs limit
  pure $ map logEntryToDTO entries

clearLogsHandler :: (MonadIO m) => m NoContent
clearLogsHandler = do
  liftIO clearLogs
  pure NoContent

logEntryToDTO :: LogEntry -> LogEntryDTO
logEntryToDTO LogEntry{..} = LogEntryDTO
  { logTimestamp = logTimestamp
  , logLevel = levelToText logLevel
  , logMessage = logMessage
  }

levelToText :: LogLevel -> Text
levelToText LogInfo = "info"
levelToText LogWarning = "warning"
levelToText LogError = "error"
