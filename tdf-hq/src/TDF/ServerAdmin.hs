{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerAdmin
  ( adminServer
  , dedupeAdminEmailRecipients
  , buildAdminUsernameCandidate
  , normalizeAdminEmailAddress
  , normalizeAdminUsername
  , normalizeAdminEmailBodyLines
  , parseSocialErrorsChannel
  , SocialUnholdLookup(..)
  , validateSocialUnholdLookup
  , validateSocialErrorsLimit
  , validateAdminWhatsAppSendMode
  , validateOptionalAdminUsername
  ) where

import           Control.Exception      (SomeException, try)
import           Control.Applicative    ((<|>))
import           Control.Monad          (forM, unless, when)
import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Crypto.BCrypt          (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Data.Foldable          (for_)
import           Data.Char              (isAlphaNum, isAsciiLower, isDigit, isSpace)
import           Data.List              (nub)
import           Data.Maybe             (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import           Data.Int               (Int64)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy   as BL
import           Data.Time              (diffUTCTime, getCurrentTime)
import           Database.Persist       ( (==.), (!=.), (<-.), (||.)
                                        , (=.)
                                        , count
                                        , updateWhere
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
                                        , AdminEmailBroadcastRecipientDTO(..)
                                        , AdminEmailBroadcastRequest(..)
                                        , AdminEmailBroadcastResponse(..)
                                        , AdminWhatsAppResendRequest(..)
                                        , AdminWhatsAppSendRequest(..)
                                        , AdminWhatsAppSendResponse(..)
                                        , BrainEntryCreate(..)
                                        , BrainEntryDTO(..)
                                        , BrainEntryUpdate(..)
                                        , EmailTestRequest(..)
                                        , EmailTestResponse(..)
                                        , RagIndexStatus(..)
                                        , RagRefreshResponse(..)
                                        , SocialUnholdRequest(..)
                                        , UserCommunicationHistoryDTO(..)
                                        , WhatsAppMessageAdminDTO(..)
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
                                        , auPartyId
                                        , hasStrictAdminAccess
                                        , hasModuleAccess
                                        , moduleName
                                        , modulesForRoles
                                        )
import           TDF.DB                 (Env(..))
import           TDF.Config             (ragRefreshHours)
import           TDF.Models
import           TDF.ModelsExtra (DropdownOption(..))
import qualified TDF.ModelsExtra as ME
import           TDF.WhatsApp.History   ( OutgoingWhatsAppRecord(..)
                                        , cleanMaybeText
                                        , messageBelongsToParty
                                        , normalizeWhatsAppPhone
                                        , phoneLookupAliases
                                        , recordOutgoingWhatsAppMessage
                                        , resolvePartyPhones
                                        )
import           TDF.WhatsApp.Transport (loadWhatsAppEnv, sendWhatsAppTextIO)
import           Data.Aeson (object, (.=))
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

data SocialUnholdLookup
  = SocialUnholdByExternalId Text
  | SocialUnholdBySenderId Text
  deriving (Show, Eq)

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
  :<|> userCommunicationsRouter
  :<|> rolesHandler
  :<|> artistsRouter
  :<|> logsRouter
  :<|> emailTestHandler
  :<|> brainRouter
  :<|> ragRouter
  :<|> socialRouter
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

    userCommunicationsRouter =
           userCommunicationHistoryHandler
      :<|> userCommunicationSendHandler
      :<|> userCommunicationResendHandler
      :<|> registeredUserEmailBroadcastHandler

    userById userId =
           getUser userId
      :<|> updateUser userId

    rolesHandler = do
      ensureStrictAdmin user
      pure (map roleDetail [minBound .. maxBound])

    artistsRouter =
      (listArtistProfilesAdmin :<|> upsertArtistProfileAdmin)
      :<|> (createArtistReleaseAdmin :<|> updateArtistReleaseAdmin)

    logsRouter =
      getLogs :<|> clearLogsHandler

    emailTestHandler EmailTestRequest{..} = do
      ensureModule ModuleAdmin user
      cfg <- asks envConfig
      targetEmail <- maybe
        (throwError err400 { errBody = "Invalid email address" })
        pure
        (normalizeAdminEmailAddress etrEmail)
      let emailSvc = EmailSvc.mkEmailService cfg
          subj = fromMaybe "Correo de prueba TDF" etrSubject
          body = maybe ["Correo de prueba desde TDF HQ."] (\txt -> [txt]) etrBody
          targetName = fromMaybe "" etrName
          preMsg = "[Admin][EmailTest] Sending to " <> targetEmail <> " | subject: " <> subj
      liftIO $ addLog LogInfo preMsg
      sendResult <- liftIO $ try $
        EmailSvc.sendTestEmail
          emailSvc
          targetName
          targetEmail
          subj
          body
          etrCtaUrl
      case sendResult of
        Left (err :: SomeException) -> do
          let msg = "[Admin][EmailTest] Failed for " <> targetEmail <> ": " <> T.pack (show err)
          liftIO $ addLog LogError msg
          liftIO $ hPutStrLn stderr (T.unpack msg)
          pure EmailTestResponse { status = "error", message = Just msg }
        Right () -> do
          let msg = "[Admin][EmailTest] Sent to " <> targetEmail
          liftIO $ addLog LogInfo msg
          liftIO $ hPutStrLn stderr (T.unpack msg)
          pure EmailTestResponse { status = "sent", message = Nothing }

    brainRouter =
      brainListHandler :<|> brainCreateHandler :<|> brainUpdateHandler

    ragRouter =
      ragStatusHandler :<|> ragRefreshHandler

    socialRouter =
      socialUnholdHandler :<|> socialStatusHandler :<|> socialErrorsHandler

    socialUnholdHandler SocialUnholdRequest{..} = do
      ensureModule ModuleAdmin user
      now <- liftIO getCurrentTime
      let channel = T.toLower (T.strip surChannel)
      lookupMode <- either throwError pure (validateSocialUnholdLookup surExternalId surSenderId)
      case lookupMode of
        SocialUnholdByExternalId extId -> do
          _ <- unholdByExternalId channel extId
          liftIO $ addLog LogInfo ("[Admin][Social] Unhold " <> channel <> " extId=" <> extId)
          pure (object ["status" .= ("ok" :: Text), "channel" .= channel, "externalId" .= extId, "ts" .= T.pack (show now)])
        SocialUnholdBySenderId sid -> do
          res <- unholdLatestBySender channel sid
          pure res

    socialStatusHandler = do
      ensureModule ModuleAdmin user
      ig <- withPool $ do
        pending <- count [InstagramMessageReplyStatus ==. "pending", InstagramMessageDirection ==. "incoming", InstagramMessageDeletedAt ==. Nothing]
        hold <- count [InstagramMessageReplyStatus ==. "hold", InstagramMessageDirection ==. "incoming", InstagramMessageDeletedAt ==. Nothing]
        err <- count [InstagramMessageReplyStatus ==. "error", InstagramMessageDirection ==. "incoming", InstagramMessageDeletedAt ==. Nothing]
        pure (pending, hold, err)
      fb <- withPool $ do
        pending <- count [ME.FacebookMessageReplyStatus ==. "pending", ME.FacebookMessageDirection ==. "incoming", ME.FacebookMessageDeletedAt ==. Nothing]
        hold <- count [ME.FacebookMessageReplyStatus ==. "hold", ME.FacebookMessageDirection ==. "incoming", ME.FacebookMessageDeletedAt ==. Nothing]
        err <- count [ME.FacebookMessageReplyStatus ==. "error", ME.FacebookMessageDirection ==. "incoming", ME.FacebookMessageDeletedAt ==. Nothing]
        pure (pending, hold, err)
      wa <- withPool $ do
        pending <- count [ME.WhatsAppMessageReplyStatus ==. "pending", ME.WhatsAppMessageDirection ==. "incoming"]
        hold <- count [ME.WhatsAppMessageReplyStatus ==. "hold", ME.WhatsAppMessageDirection ==. "incoming"]
        err <- count [ME.WhatsAppMessageReplyStatus ==. "error", ME.WhatsAppMessageDirection ==. "incoming"]
        pure (pending, hold, err)
      pure $ object
        [ "instagram" .= object ["pending" .= (let (p,_,_) = ig in p), "hold" .= (let (_,h,_) = ig in h), "error" .= (let (_,_,e) = ig in e)]
        , "facebook" .= object ["pending" .= (let (p,_,_) = fb in p), "hold" .= (let (_,h,_) = fb in h), "error" .= (let (_,_,e) = fb in e)]
        , "whatsapp" .= object ["pending" .= (let (p,_,_) = wa in p), "hold" .= (let (_,h,_) = wa in h), "error" .= (let (_,_,e) = wa in e)]
        ]

    socialErrorsHandler mChannel mLimit = do
      ensureModule ModuleAdmin user
      channel <- either throwError pure (parseSocialErrorsChannel mChannel)
      limit <- either throwError pure (validateSocialErrorsLimit mLimit)
      case channel of
        "instagram" -> do
          rows <- withPool $ selectList
            [ InstagramMessageDirection ==. "incoming"
            , InstagramMessageReplyStatus ==. "error"
            , InstagramMessageDeletedAt ==. Nothing
            ]
            [Desc InstagramMessageCreatedAt, LimitTo limit]
          pure $ object
            [ "channel" .= channel
            , "items" .= map (\(Entity _ m) -> object
                [ "externalId" .= instagramMessageExternalId m
                , "senderId" .= instagramMessageSenderId m
                , "text" .= instagramMessageText m
                , "replyError" .= instagramMessageReplyError m
                , "createdAt" .= instagramMessageCreatedAt m
                ]) rows
            ]
        "facebook" -> do
          rows <- withPool $ selectList
            [ ME.FacebookMessageDirection ==. "incoming"
            , ME.FacebookMessageReplyStatus ==. "error"
            , ME.FacebookMessageDeletedAt ==. Nothing
            ]
            [Desc ME.FacebookMessageCreatedAt, LimitTo limit]
          pure $ object
            [ "channel" .= channel
            , "items" .= map (\(Entity _ m) -> object
                [ "externalId" .= ME.facebookMessageExternalId m
                , "senderId" .= ME.facebookMessageSenderId m
                , "text" .= ME.facebookMessageText m
                , "replyError" .= ME.facebookMessageReplyError m
                , "createdAt" .= ME.facebookMessageCreatedAt m
                ]) rows
            ]
        "whatsapp" -> do
          rows <- withPool $ selectList
            [ ME.WhatsAppMessageDirection ==. "incoming"
            , ME.WhatsAppMessageReplyStatus ==. "error"
            ]
            [Desc ME.WhatsAppMessageCreatedAt, LimitTo limit]
          pure $ object
            [ "channel" .= channel
            , "items" .= map (\(Entity _ m) -> object
                [ "externalId" .= ME.whatsAppMessageExternalId m
                , "senderId" .= ME.whatsAppMessageSenderId m
                , "text" .= ME.whatsAppMessageText m
                , "replyError" .= ME.whatsAppMessageReplyError m
                , "createdAt" .= ME.whatsAppMessageCreatedAt m
                ]) rows
            ]
        _ -> throwError err400 { errBody = "channel inválido (instagram|facebook|whatsapp)" }

    unholdByExternalId channel extId =
      case channel of
        "instagram" -> withPool $ updateWhere
          [ InstagramMessageExternalId ==. extId ]
          [ InstagramMessageReplyStatus =. "pending"
          , InstagramMessageHoldReason =. Nothing
          , InstagramMessageHoldRequiredFields =. Nothing
          , InstagramMessageLastAttemptAt =. Nothing
          , InstagramMessageReplyError =. Nothing
          ]
        "facebook" -> withPool $ updateWhere
          [ ME.FacebookMessageExternalId ==. extId ]
          [ ME.FacebookMessageReplyStatus =. "pending"
          , ME.FacebookMessageHoldReason =. Nothing
          , ME.FacebookMessageHoldRequiredFields =. Nothing
          , ME.FacebookMessageLastAttemptAt =. Nothing
          , ME.FacebookMessageReplyError =. Nothing
          ]
        "whatsapp" -> withPool $ updateWhere
          [ ME.WhatsAppMessageExternalId ==. extId ]
          [ ME.WhatsAppMessageReplyStatus =. "pending"
          , ME.WhatsAppMessageHoldReason =. Nothing
          , ME.WhatsAppMessageHoldRequiredFields =. Nothing
          , ME.WhatsAppMessageLastAttemptAt =. Nothing
          , ME.WhatsAppMessageReplyError =. Nothing
          ]
        _ -> throwError err400 { errBody = "channel inválido (instagram|facebook|whatsapp)" }

    unholdLatestBySender channel senderId = do
      now <- liftIO getCurrentTime
      mExt <- case channel of
        "instagram" -> withPool $ do
          mRow <- selectFirst [InstagramMessageSenderId ==. senderId, InstagramMessageReplyStatus ==. "hold", InstagramMessageDirection ==. "incoming", InstagramMessageDeletedAt ==. Nothing] [Desc InstagramMessageCreatedAt]
          pure (fmap (instagramMessageExternalId . entityVal) mRow)
        "facebook" -> withPool $ do
          mRow <- selectFirst [ME.FacebookMessageSenderId ==. senderId, ME.FacebookMessageReplyStatus ==. "hold", ME.FacebookMessageDirection ==. "incoming", ME.FacebookMessageDeletedAt ==. Nothing] [Desc ME.FacebookMessageCreatedAt]
          pure (fmap (ME.facebookMessageExternalId . entityVal) mRow)
        "whatsapp" -> withPool $ do
          mRow <- selectFirst [ME.WhatsAppMessageSenderId ==. senderId, ME.WhatsAppMessageReplyStatus ==. "hold", ME.WhatsAppMessageDirection ==. "incoming"] [Desc ME.WhatsAppMessageCreatedAt]
          pure (fmap (ME.whatsAppMessageExternalId . entityVal) mRow)
        _ -> throwError err400 { errBody = "channel inválido (instagram|facebook|whatsapp)" }
      case mExt of
        Nothing -> pure (object ["status" .= ("noop" :: Text), "channel" .= channel, "senderId" .= senderId, "message" .= ("No hold found" :: Text), "ts" .= T.pack (show now)])
        Just extId -> do
          _ <- unholdByExternalId channel extId
          liftIO $ addLog LogInfo ("[Admin][Social] Unhold latest hold " <> channel <> " senderId=" <> senderId <> " extId=" <> extId)
          pure (object ["status" .= ("ok" :: Text), "channel" .= channel, "senderId" .= senderId, "externalId" .= extId, "ts" .= T.pack (show now)])

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
      ensureStrictAdmin user
      let includeInactive = fromMaybe False mIncludeInactive
          filters = [UserCredentialActive ==. True | not includeInactive]
      withPool $ do
        creds <- selectList filters [Asc UserCredentialId]
        mapM loadUserAccount creds

    createUser UserAccountCreate{..} = do
      ensureStrictAdmin user
      config <- asks envConfig
      let partyKey = toSqlKey uacPartyId :: PartyId
          activeValue = fromMaybe True uacActive
          emailSvc = EmailSvc.mkEmailService config
      partyEnt <- do
        mParty <- withPool $ getEntity partyKey
        maybe (throwError err404 { errBody = "Party not found" }) pure mParty
      emailAddress <- case partyPrimaryEmail (entityVal partyEnt) of
        Nothing -> throwError err400 { errBody = "Party must have a primary email before creating a user" }
        Just addr ->
          maybe
            (throwError err400 { errBody = "Party must have a valid primary email before creating a user" })
            pure
            (normalizeAdminEmailAddress addr)
      explicitUsername <- either throwError pure (validateOptionalAdminUsername uacUsername)
      baseUsername <-
        case explicitUsername of
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
      welcomeResult <- liftIO $ try $
        EmailSvc.sendWelcome
          emailSvc
          (partyDisplayName (entityVal partyEnt))
          emailAddress
          uniqueUsername
          tempPassword
      case welcomeResult of
        Left (err :: SomeException) -> do
          let msg = "[Admin][Users] Created account but failed to send welcome email to " <> emailAddress <> ": " <> T.pack (show err)
          liftIO $ do
            addLog LogWarning msg
            hPutStrLn stderr (T.unpack msg)
        Right () -> pure ()
      pure account
      where
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
            normalizedBase = sanitizeGeneratedAdminUsername base
            root = if T.null normalizedBase then "tdf-user" else normalizedBase
            go attempt = do
              let candidate = buildAdminUsernameCandidate root attempt
              conflict <- withPool $ getBy (UniqueCredentialUsername candidate)
              case conflict of
                Nothing -> pure candidate
                Just _  -> go (attempt + 1)

    getUser userId = do
      ensureStrictAdmin user
      let credKey = toSqlKey userId :: UserCredentialId
      mCred <- withPool $ getEntity credKey
      case mCred of
        Nothing       -> throwError err404
        Just credEnt  -> withPool (loadUserAccount credEnt)

    updateUser userId UserAccountUpdate{..} = do
      ensureStrictAdmin user
      let credKey = toSqlKey userId :: UserCredentialId
      usernameUpdate <- either throwError pure (validateOptionalAdminUsername uauUsername)
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

    userCommunicationHistoryHandler userId mLimit = do
      ensureStrictAdmin user
      let limit = max 1 (min 300 (fromMaybe 150 mLimit))
      mContext <- withPool (loadUserCommunicationContext userId)
      case mContext of
        Nothing -> throwError err404
        Just context -> withPool (buildUserCommunicationHistory context limit)

    userCommunicationSendHandler userId AdminWhatsAppSendRequest{..} = do
      ensureStrictAdmin user
      let body = T.strip awsrMessage
      mode <- either throwError pure (validateAdminWhatsAppSendMode awsrMode awsrReplyToMessageId)
      when (T.null body) $
        throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 "Mensaje vacío") }
      mContext <- withPool (loadUserCommunicationContext userId)
      (_, partyEnt) <- maybe (throwError err404) pure mContext
      let partyKey = entityKey partyEnt
          partyVal = entityVal partyEnt
          candidatePhones = resolvePartyPhones partyVal
      phone <- maybe (throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 "El usuario no tiene WhatsApp o teléfono configurado") }) pure (listToMaybe candidatePhones)
      replyTarget <- case (mode, awsrReplyToMessageId) of
        ("reply", Just replyId) -> do
          let msgKey = toSqlKey replyId :: ME.WhatsAppMessageId
          mMsg <- withPool (getEntity msgKey)
          case mMsg of
            Nothing -> throwError err404 { errBody = BL.fromStrict (TE.encodeUtf8 "Mensaje de referencia no encontrado") }
            Just msgEnt ->
              if messageBelongsToParty partyKey candidatePhones (entityVal msgEnt)
                then pure (Just msgEnt)
                else throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 "El mensaje no pertenece a este usuario") }
        _ -> pure Nothing
      now <- liftIO getCurrentTime
      waEnv <- liftIO loadWhatsAppEnv
      sendResult <- liftIO $ sendWhatsAppTextIO waEnv phone body
      sentEntity <- withPool $
        recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
          { owrRecipientPhone = phone
          , owrRecipientPartyId = Just partyKey
          , owrRecipientName = Just (partyDisplayName partyVal)
          , owrRecipientEmail = partyPrimaryEmail partyVal
          , owrActorPartyId = Just (auPartyId user)
          , owrBody = body
          , owrSource = Just (if mode == "reply" then "admin_reply" else "admin_notify")
          , owrReplyToMessageId = entityKey <$> replyTarget
          , owrReplyToExternalId = fmap (ME.whatsAppMessageExternalId . entityVal) replyTarget
          , owrResendOfMessageId = Nothing
          , owrMetadata = Nothing
          }
          sendResult
      pure (buildSendResponse sendResult sentEntity)

    userCommunicationResendHandler messageId AdminWhatsAppResendRequest{..} = do
      ensureStrictAdmin user
      let msgKey = toSqlKey messageId :: ME.WhatsAppMessageId
      msgEnt <- withPool (getEntity msgKey) >>= maybe (throwError err404) pure
      let msg = entityVal msgEnt
      when (ME.whatsAppMessageDirection msg /= "outgoing") $
        throwError err400 { errBody = "Solo se pueden reenviar mensajes salientes" }
      originalBody <- maybe
        (throwError err400 { errBody = "El mensaje original no tiene contenido de texto" })
        pure
        (cleanMaybeText (ME.whatsAppMessageText msg))
      let resendBody = fromMaybe originalBody (cleanMaybeText awrrMessage)
          phone = ME.whatsAppMessagePhoneE164 msg <|> normalizeWhatsAppPhone (ME.whatsAppMessageSenderId msg)
      targetPhone <- maybe (throwError err400 { errBody = "No se pudo determinar el número destino" }) pure phone
      now <- liftIO getCurrentTime
      waEnv <- liftIO loadWhatsAppEnv
      sendResult <- liftIO $ sendWhatsAppTextIO waEnv targetPhone resendBody
      sentEntity <- withPool $
        recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
          { owrRecipientPhone = targetPhone
          , owrRecipientPartyId = ME.whatsAppMessagePartyId msg
          , owrRecipientName = ME.whatsAppMessageSenderName msg
          , owrRecipientEmail = ME.whatsAppMessageContactEmail msg
          , owrActorPartyId = Just (auPartyId user)
          , owrBody = resendBody
          , owrSource = Just "admin_resend"
          , owrReplyToMessageId = Nothing
          , owrReplyToExternalId = Nothing
          , owrResendOfMessageId = Just (entityKey msgEnt)
          , owrMetadata = Nothing
          }
          sendResult
      pure (buildSendResponse sendResult sentEntity)

    registeredUserEmailBroadcastHandler AdminEmailBroadcastRequest{..} = do
      ensureStrictAdmin user
      let subject = T.strip aebrSubject
          bodyLines = normalizeAdminEmailBodyLines aebrBodyLines
          dryRun = fromMaybe False aebrDryRun
          includeInactive = fromMaybe False aebrIncludeInactive
      when (T.null subject) $
        throwError err400 { errBody = "Subject must not be empty" }
      when (null bodyLines) $
        throwError err400 { errBody = "At least one non-empty body line is required" }
      limitValue <- case aebrLimit of
        Nothing -> pure Nothing
        Just rawLimit
          | rawLimit <= 0 -> throwError err400 { errBody = "limit must be a positive integer" }
          | otherwise -> pure (Just (min 5000 rawLimit))
      cfg <- asks envConfig
      let emailSvc = EmailSvc.mkEmailService cfg
      when (not dryRun && isNothing (EmailSvc.esConfig emailSvc)) $
        throwError err409 { errBody = "SMTP not configured" }
      rawRecipients <- withPool (loadRegisteredUserEmailRecipients includeInactive)
      let matchedUsers = length rawRecipients
          uniqueRecipients = dedupeAdminEmailRecipients rawRecipients
          recipientsToProcess = maybe uniqueRecipients (`take` uniqueRecipients) limitValue
          processedRecipients = length recipientsToProcess
      liftIO $ addLog LogInfo $
        T.concat
          [ "[Admin][EmailBroadcast] Starting registered-user broadcast | subject="
          , subject
          , " | dryRun="
          , if dryRun then "true" else "false"
          , " | matchedUsers="
          , T.pack (show matchedUsers)
          , " | uniqueRecipients="
          , T.pack (show (length uniqueRecipients))
          , " | processedRecipients="
          , T.pack (show processedRecipients)
          ]
      recipientResults <-
        if dryRun
          then pure
            [ AdminEmailBroadcastRecipientDTO
                { aerdEmail = emailAddr
                , aerdName = name
                , aerdStatus = "dry_run"
                , aerdMessage = Nothing
                }
            | (name, emailAddr) <- recipientsToProcess
            ]
          else forM recipientsToProcess $ \(name, emailAddr) -> do
            sendResult <- liftIO $ try $
              EmailSvc.sendTestEmail emailSvc name emailAddr subject bodyLines Nothing
            case sendResult of
              Left (err :: SomeException) -> do
                let msg = T.pack (show err)
                liftIO $ addLog LogError $
                  "[Admin][EmailBroadcast] Failed for " <> emailAddr <> ": " <> msg
                pure AdminEmailBroadcastRecipientDTO
                  { aerdEmail = emailAddr
                  , aerdName = name
                  , aerdStatus = "failed"
                  , aerdMessage = Just msg
                  }
              Right () -> do
                liftIO $ addLog LogInfo $
                  "[Admin][EmailBroadcast] Sent to " <> emailAddr
                pure AdminEmailBroadcastRecipientDTO
                  { aerdEmail = emailAddr
                  , aerdName = name
                  , aerdStatus = "sent"
                  , aerdMessage = Nothing
                  }
      let sentCount = length [() | result <- recipientResults, aerdStatus result == "sent"]
          failedCount = length [() | result <- recipientResults, aerdStatus result == "failed"]
          finalStatus
            | failedCount == 0 = "ok"
            | sentCount == 0 = "error"
            | otherwise = "partial"
      liftIO $ addLog LogInfo $
        T.concat
          [ "[Admin][EmailBroadcast] Completed registered-user broadcast | status="
          , finalStatus
          , " | sent="
          , T.pack (show sentCount)
          , " | failed="
          , T.pack (show failedCount)
          ]
      pure AdminEmailBroadcastResponse
        { aersStatus = finalStatus
        , aersDryRun = dryRun
        , aersMatchedUsers = matchedUsers
        , aersUniqueRecipients = length uniqueRecipients
        , aersProcessedRecipients = processedRecipients
        , aersSentCount = sentCount
        , aersFailedCount = failedCount
        , aersRecipients = recipientResults
        }

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

ensureStrictAdmin
  :: (MonadError ServerError m)
  => AuthedUser
  -> m ()
ensureStrictAdmin user =
  unless (hasStrictAdminAccess user) $
    throwError err403 { errBody = "Admin role required" }

parseSocialErrorsChannel :: Maybe Text -> Either ServerError Text
parseSocialErrorsChannel mChannel =
  case fmap (T.toLower . T.strip) mChannel of
    Just "instagram" -> Right "instagram"
    Just "facebook" -> Right "facebook"
    Just "whatsapp" -> Right "whatsapp"
    Nothing -> missingChannel
    Just txt
      | T.null txt -> missingChannel
      | otherwise -> Left err400 { errBody = "channel inválido (instagram|facebook|whatsapp)" }
  where
    missingChannel =
      Left err400 { errBody = "channel requerido (instagram|facebook|whatsapp)" }

validateSocialUnholdLookup :: Maybe Text -> Maybe Text -> Either ServerError SocialUnholdLookup
validateSocialUnholdLookup mExternalId mSenderId =
  case (normaliseText mExternalId, normaliseText mSenderId) of
    (Just extId, Nothing) -> Right (SocialUnholdByExternalId extId)
    (Nothing, Just senderId) -> Right (SocialUnholdBySenderId senderId)
    (Nothing, Nothing) ->
      Left err400 { errBody = "Provide externalId or senderId" }
    (Just _, Just _) ->
      Left err400 { errBody = "Provide only one of externalId or senderId" }

validateSocialErrorsLimit :: Maybe Int -> Either ServerError Int
validateSocialErrorsLimit Nothing = Right 50
validateSocialErrorsLimit (Just rawLimit)
  | rawLimit < 1 || rawLimit > 200 =
      Left err400 { errBody = "limit debe estar entre 1 y 200" }
  | otherwise = Right rawLimit

validateAdminWhatsAppSendMode :: Text -> Maybe Int64 -> Either ServerError Text
validateAdminWhatsAppSendMode rawMode mReplyToMessageId =
  case T.toLower (T.strip rawMode) of
    "reply" ->
      case mReplyToMessageId of
        Nothing ->
          Left err400 { errBody = BL.fromStrict (TE.encodeUtf8 "replyToMessageId requerido para responder") }
        Just replyId
          | replyId <= 0 ->
              Left err400 { errBody = BL.fromStrict (TE.encodeUtf8 "replyToMessageId debe ser un entero positivo") }
          | otherwise -> Right "reply"
    "notify"
      | isJust mReplyToMessageId ->
          Left err400 { errBody = BL.fromStrict (TE.encodeUtf8 "replyToMessageId solo se permite en mode=reply") }
      | otherwise -> Right "notify"
    _ ->
      Left err400 { errBody = BL.fromStrict (TE.encodeUtf8 "mode inválido (reply|notify)") }

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
    , primaryEmail = partyPrimaryEmail (entityVal party)
    , primaryPhone = partyPrimaryPhone (entityVal party)
    , whatsapp = partyWhatsapp (entityVal party)
    , active    = userCredentialActive cred
    , roles     = roleList
    , modules   = map moduleName (Set.toList (modulesForRoles roleList))
    }

loadUserCommunicationContext
  :: Int64
  -> SqlPersistT IO (Maybe (Entity UserCredential, Entity Party))
loadUserCommunicationContext userId = do
  let credKey = toSqlKey userId :: UserCredentialId
  mCred <- getEntity credKey
  case mCred of
    Nothing -> pure Nothing
    Just credEnt -> do
      partyEnt <- getJustEntity (userCredentialPartyId (entityVal credEnt))
      pure (Just (credEnt, partyEnt))

buildUserCommunicationHistory
  :: (Entity UserCredential, Entity Party)
  -> Int
  -> SqlPersistT IO UserCommunicationHistoryDTO
buildUserCommunicationHistory (Entity credId cred, Entity partyKey party) limit = do
  let phones = resolvePartyPhones party
      senderAliases = nub (concatMap phoneLookupAliases phones)
      linkedFilters = [ME.WhatsAppMessagePartyId ==. Just partyKey]
      phoneFilters = if null phones then [] else [ME.WhatsAppMessagePhoneE164 <-. map Just phones]
      senderFilters = if null senderAliases then [] else [ME.WhatsAppMessageSenderId <-. senderAliases]
      filters =
        case (null phoneFilters, null senderFilters) of
          (True, True) -> linkedFilters
          (False, True) -> linkedFilters ||. phoneFilters
          (True, False) -> linkedFilters ||. senderFilters
          (False, False) -> (linkedFilters ||. phoneFilters) ||. senderFilters
  rows <- selectList filters [Desc ME.WhatsAppMessageCreatedAt, LimitTo limit]
  pure UserCommunicationHistoryDTO
    { uchUserId = fromSqlKey credId
    , uchPartyId = fromSqlKey partyKey
    , uchPartyName = partyDisplayName party
    , uchUsername = userCredentialUsername cred
    , uchPrimaryEmail = partyPrimaryEmail party
    , uchPrimaryPhone = partyPrimaryPhone party
    , uchWhatsapp = partyWhatsapp party
    , uchMessages = map toWhatsAppMessageAdminDTO rows
    }

toWhatsAppMessageAdminDTO :: Entity ME.WhatsAppMessage -> WhatsAppMessageAdminDTO
toWhatsAppMessageAdminDTO (Entity msgKey msg) =
  WhatsAppMessageAdminDTO
    { wmdId = fromSqlKey msgKey
    , wmdExternalId = ME.whatsAppMessageExternalId msg
    , wmdPartyId = fmap fromSqlKey (ME.whatsAppMessagePartyId msg)
    , wmdActorPartyId = fmap fromSqlKey (ME.whatsAppMessageActorPartyId msg)
    , wmdSenderId = ME.whatsAppMessageSenderId msg
    , wmdSenderName = ME.whatsAppMessageSenderName msg
    , wmdPhoneE164 = ME.whatsAppMessagePhoneE164 msg
    , wmdContactEmail = ME.whatsAppMessageContactEmail msg
    , wmdText = ME.whatsAppMessageText msg
    , wmdDirection = ME.whatsAppMessageDirection msg
    , wmdReplyStatus = ME.whatsAppMessageReplyStatus msg
    , wmdReplyError = ME.whatsAppMessageReplyError msg
    , wmdRepliedAt = ME.whatsAppMessageRepliedAt msg
    , wmdReplyText = ME.whatsAppMessageReplyText msg
    , wmdDeliveryStatus = ME.whatsAppMessageDeliveryStatus msg
    , wmdDeliveryUpdatedAt = ME.whatsAppMessageDeliveryUpdatedAt msg
    , wmdDeliveryError = ME.whatsAppMessageDeliveryError msg
    , wmdSource = ME.whatsAppMessageSource msg
    , wmdResendOfMessageId = fmap fromSqlKey (ME.whatsAppMessageResendOfMessageId msg)
    , wmdCreatedAt = ME.whatsAppMessageCreatedAt msg
    }

buildSendResponse
  :: Either Text sendResult
  -> Entity ME.WhatsAppMessage
  -> AdminWhatsAppSendResponse
buildSendResponse sendResult (Entity msgKey msg) =
  AdminWhatsAppSendResponse
    { awspStatus = either (const "error") (const "ok") sendResult
    , awspMessageId = Just (fromSqlKey msgKey)
    , awspDeliveryStatus = ME.whatsAppMessageDeliveryStatus msg
    , awspMessage = either Just (const (Just "sent")) sendResult
    }

normalizeAdminEmailBodyLines :: [Text] -> [Text]
normalizeAdminEmailBodyLines =
  filter (not . T.null) . map T.strip

normalizeAdminEmailAddress :: Text -> Maybe Text
normalizeAdminEmailAddress raw =
  let normalized = T.toLower (T.strip raw)
  in if isValidAdminEmailAddress normalized then Just normalized else Nothing

adminUsernameMaxLength :: Int
adminUsernameMaxLength = 60

isAdminUsernameChar :: Char -> Bool
isAdminUsernameChar c = isAlphaNum c || c `elem` (".-_" :: String)

sanitizeGeneratedAdminUsername :: Text -> Text
sanitizeGeneratedAdminUsername =
  T.take adminUsernameMaxLength . T.filter isAdminUsernameChar . T.toLower . T.strip

normalizeAdminUsername :: Text -> Maybe Text
normalizeAdminUsername raw =
  let normalized = T.toLower (T.strip raw)
  in
    if T.null normalized
         || T.length normalized > adminUsernameMaxLength
         || not (T.all isAdminUsernameChar normalized)
      then Nothing
      else Just normalized

buildAdminUsernameCandidate :: Text -> Int -> Text
buildAdminUsernameCandidate root attempt =
  let suffix = if attempt == 0 then "" else "-" <> T.pack (show attempt)
      rootBudget = max 0 (adminUsernameMaxLength - T.length suffix)
  in T.take rootBudget root <> suffix

validateOptionalAdminUsername :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalAdminUsername Nothing = Right Nothing
validateOptionalAdminUsername (Just raw) =
  let trimmed = T.toLower (T.strip raw)
  in case normalizeAdminUsername raw of
    Just normalized -> Right (Just normalized)
    Nothing
      | T.null trimmed ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 "Username must contain at least one letter, number, dot, dash, or underscore")
            }
      | T.length trimmed > adminUsernameMaxLength ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 "Username must be 60 characters or fewer")
            }
      | otherwise ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 "Username may only contain letters, numbers, dots, dashes, or underscores")
            }

isValidAdminEmailAddress :: Text -> Bool
isValidAdminEmailAddress candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      not (T.null localPart)
        && not (T.null domain)
        && not (T.any isSpace candidate)
        && T.isInfixOf "." domain
        && all isValidAdminEmailDomainLabel (T.splitOn "." domain)
    _ -> False

isValidAdminEmailDomainLabel :: Text -> Bool
isValidAdminEmailDomainLabel label =
  not (T.null label)
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidAdminEmailDomainChar label

isValidAdminEmailDomainChar :: Char -> Bool
isValidAdminEmailDomainChar c = isAsciiLower c || isDigit c || c == '-'

dedupeAdminEmailRecipients :: [(Text, Text)] -> [(Text, Text)]
dedupeAdminEmailRecipients = reverse . snd . foldl step (Set.empty, [])
  where
    step (seen, acc) (rawName, rawEmail) =
      case normalizeAdminEmailAddress rawEmail of
        Nothing -> (seen, acc)
        Just emailAddr ->
          let name = T.strip rawName
          in if Set.member emailAddr seen
               then (seen, acc)
               else (Set.insert emailAddr seen, (name, emailAddr) : acc)

loadRegisteredUserEmailRecipients :: Bool -> SqlPersistT IO [(Text, Text)]
loadRegisteredUserEmailRecipients includeInactive = do
  let filters = [UserCredentialActive ==. True | not includeInactive]
  creds <- selectList filters [Asc UserCredentialId]
  pairs <- forM creds $ \(Entity _ cred) -> do
    party <- getJust (userCredentialPartyId cred)
    pure $
      case partyPrimaryEmail party of
        Nothing -> Nothing
        Just emailAddr ->
          case normalizeAdminEmailAddress emailAddr of
            Nothing -> Nothing
            Just normalizedEmail -> Just (partyDisplayName party, normalizedEmail)
  pure (catMaybes pairs)

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
