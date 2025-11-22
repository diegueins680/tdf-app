{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.ServerAdmin
  ( adminServer
  ) where

import           Control.Monad          (unless, when)
import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Crypto.BCrypt          (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Data.Foldable          (for_)
import           Data.Char              (isAlphaNum)
import           Data.Maybe             (fromMaybe, isJust)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time              (getCurrentTime)
import           Database.Persist       ( (==.), (!=.)
                                        , (=.)
                                        , Entity(..)
                                        , Update
                                        , SelectOpt(..)
                                        , selectFirst
                                        , selectList
                                        , get
                                        , getBy
                                        , update
                                        , getEntity
                                        , getJustEntity
                                        , insert
                                        , upsert
                                        )
import           Database.Persist.Sql   (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant
import           Web.PathPieces         (PathPiece, fromPathPiece, toPathPiece)

import           TDF.API.Admin          (AdminAPI)
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
import           TDF.Config             (AppConfig(..))
import           TDF.DB                 (Env(..))
import           TDF.Models
import           TDF.ModelsExtra (DropdownOption(..), CourseRegistration(..))
import qualified TDF.ModelsExtra as ME
import           TDF.Seed               (seedAll)
import qualified TDF.Email              as Email
import qualified TDF.Email.Service      as EmailSvc
import qualified TDF.Services           as Services
import           TDF.Profiles.Artist    ( loadAllArtistProfilesDTO
                                        , upsertArtistProfileRecord
                                        )
import           TDF.Routes.Courses     ( CourseRegistrationStatusUpdate(..)
                                        , CourseRegistrationResponse(..)
                                        )
import           TDF.LogBuffer          ( LogEntry(..), LogLevel(..), getRecentLogs, clearLogs )
import           TDF.DTO                ( LogEntryDTO(..) )
import           Data.Time.Format       ( formatTime, defaultTimeLocale )

adminServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT AdminAPI m
adminServer user = seedHandler :<|> dropdownRouter :<|> usersRouter :<|> rolesHandler :<|> artistsRouter :<|> logsRouter :<|> courseRegistrationsRouter
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
      :<|> createArtistReleaseAdmin

    logsRouter mLimit =
      (getLogs mLimit :<|> clearLogsHandler)

    courseRegistrationsRouter slug regId =
      updateRegistrationStatus slug regId

    roleDetail role = RoleDetailDTO
      { role    = role
      , label   = T.pack (show role)
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

    updateRegistrationStatus slug regId CourseRegistrationStatusUpdate{..} = do
      ensureModule ModuleAdmin user
      let normalizedSlug = T.toLower (T.strip slug)
      normalizedStatus <- normaliseStatus status
      let regKey = toSqlKey regId :: Key CourseRegistration
      now <- liftIO getCurrentTime
      entity <- withPool $ get regKey
      reg <- maybe (throwError err404) pure entity
      when (ME.courseRegistrationCourseSlug reg /= normalizedSlug) $
        throwError err404
      withPool $ update regKey
        [ ME.CourseRegistrationStatus =. normalizedStatus
        , ME.CourseRegistrationUpdatedAt =. now
        ]
      pure CourseRegistrationResponse { id = regId, status = normalizedStatus }

    normaliseStatus raw =
      let trimmed = T.toLower (T.strip raw)
      in if trimmed `elem` ["pending_payment", "paid", "cancelled"]
           then pure trimmed
           else throwError err400 { errBody = "Estado inválido; usa pending_payment, paid o cancelled" }

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
          emailSvc = Services.emailService (Services.buildServices config)
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

        generateUniqueUsername base = go 0
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
