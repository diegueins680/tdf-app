{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TDF.ServerProposals
  ( proposalsServer
  ) where

import           Control.Monad              (unless)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Data.Char                  (isAlphaNum, isAscii)
import           Data.List                  (foldl')
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe, maybeToList)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import           Data.Time                  (UTCTime, getCurrentTime)
import qualified Data.ByteString.Lazy       as BL
import           Database.Persist           (Entity(..), Key, SelectOpt(..), Update, getEntity,
                                             getJustEntity, insert, selectFirst, selectList, update)
import           Database.Persist           ((==.), (=.), (<-.))
import           Database.Persist.Sql       (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant
import           System.Directory           (doesFileExist)
import           System.FilePath            ((</>))
import           Web.PathPieces             (PathPiece, fromPathPiece, toPathPiece)

import           TDF.API.Proposals
import           TDF.Auth                   (AuthedUser(..), ModuleAccess(..), hasModuleAccess)
import           TDF.DB                     (Env(..))
import qualified TDF.ModelsExtra            as ME
import qualified TDF.Handlers.InputList     as InputList

proposalsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT ProposalsAPI m
proposalsServer user =
       listProposalsH
  :<|> createProposalH
  :<|> proposalRoutes
  where
    proposalRoutes rawId =
           getProposalH rawId
      :<|> updateProposalH rawId
      :<|> listVersionsH rawId
      :<|> createVersionH rawId
      :<|> getVersionH rawId
      :<|> proposalPdfH rawId

    ensureCRM :: MonadError ServerError m => m ()
    ensureCRM =
      unless (hasModuleAccess ModuleCRM user) $
        throwError err403 { errBody = "CRM access required" }

    listProposalsH = do
      ensureCRM
      entities <- withPool $ selectList [] [Desc ME.ProposalCreatedAt]
      versionMap <- latestVersionMap (map entityKey entities)
      pure (map (proposalToDTO versionMap) entities)

    getProposalH rawId = do
      ensureCRM
      proposalKey <- parseKey @ME.Proposal rawId
      mEntity <- withPool $ getEntity proposalKey
      case mEntity of
        Nothing -> throwError err404
        Just ent -> do
          mLatest <- withPool $ selectFirst
            [ ME.ProposalVersionProposalId ==. proposalKey ]
            [ Desc ME.ProposalVersionVersion, LimitTo 1 ]
          let versionMap = maybe Map.empty
                (\(Entity _ v) -> Map.singleton proposalKey (ME.proposalVersionVersion v))
                mLatest
          pure (proposalToDTO versionMap ent)

    createProposalH ProposalCreate{..} = do
      ensureCRM
      title <- requireText "title" pcTitle
      latex <- resolveLatex pcLatex pcTemplateKey
      now <- liftIO getCurrentTime
      pipelineCardKey <- parseOptionalKey @ME.PipelineCard pcPipelineCardId
      let statusVal = fromMaybe "draft" (normalizeStatus pcStatus)
          proposalRecord = ME.Proposal
            { ME.proposalTitle          = title
            , ME.proposalServiceKind    = pcServiceKind
            , ME.proposalClientPartyId  = toSqlKey <$> pcClientPartyId
            , ME.proposalContactName    = normalizeOptionalText pcContactName
            , ME.proposalContactEmail   = normalizeOptionalText pcContactEmail
            , ME.proposalContactPhone   = normalizeOptionalText pcContactPhone
            , ME.proposalPipelineCardId = pipelineCardKey
            , ME.proposalStatus         = statusVal
            , ME.proposalNotes          = normalizeOptionalText pcNotes
            , ME.proposalCreatedAt      = now
            , ME.proposalUpdatedAt      = now
            , ME.proposalLastGeneratedAt = Nothing
            , ME.proposalSentAt         = sentAtFromStatus Nothing statusVal now
            }
      proposalKey <- withPool $ insert proposalRecord
      let versionRecord = ME.ProposalVersion
            { ME.proposalVersionProposalId   = proposalKey
            , ME.proposalVersionVersion      = 1
            , ME.proposalVersionLatex        = latex
            , ME.proposalVersionCreatedAt    = now
            , ME.proposalVersionCreatedByRef = Just (toPathPiece (auPartyId user))
            , ME.proposalVersionNotes        = normalizeOptionalText pcVersionNotes
            }
      _ <- withPool $ insert versionRecord
      pure (proposalToDTO (Map.singleton proposalKey 1) (Entity proposalKey proposalRecord))

    updateProposalH rawId ProposalUpdate{..} = do
      ensureCRM
      proposalKey <- parseKey @ME.Proposal rawId
      mEntity <- withPool $ getEntity proposalKey
      case mEntity of
        Nothing -> throwError err404
        Just (Entity key proposal) -> do
          now <- liftIO getCurrentTime
          titleUpdate <- traverse (requireText "title") puTitle
          statusUpdate <- traverse requireStatus puStatus
          pipelineCardUpdate <- parseOptionalKeyUpdate @ME.PipelineCard puPipelineCardId
          let updates = catMaybes
                [ fmap (ME.ProposalTitle =.) titleUpdate
                , fmap (ME.ProposalStatus =.) statusUpdate
                , fmap (ME.ProposalServiceKind =.) puServiceKind
                , fmap (ME.ProposalClientPartyId =.)
                    (fmap (fmap toSqlKey) puClientPartyId)
                , fmap (ME.ProposalContactName =.) (normalizeOptionalUpdate puContactName)
                , fmap (ME.ProposalContactEmail =.) (normalizeOptionalUpdate puContactEmail)
                , fmap (ME.ProposalContactPhone =.) (normalizeOptionalUpdate puContactPhone)
                , fmap (ME.ProposalPipelineCardId =.) pipelineCardUpdate
                , fmap (ME.ProposalNotes =.) (normalizeOptionalUpdate puNotes)
                ]
              sentUpdate = sentAtUpdate (ME.proposalSentAt proposal) statusUpdate now
              updates' = updates ++ maybeToList sentUpdate
              finalUpdates =
                if null updates' then [] else updates' ++ [ME.ProposalUpdatedAt =. now]
          unless (null finalUpdates) (withPool $ update key finalUpdates)
          updated <- withPool $ getJustEntity key
          mLatest <- withPool $ selectFirst
            [ ME.ProposalVersionProposalId ==. key ]
            [ Desc ME.ProposalVersionVersion, LimitTo 1 ]
          let versionMap = maybe Map.empty
                (\(Entity _ v) -> Map.singleton key (ME.proposalVersionVersion v))
                mLatest
          pure (proposalToDTO versionMap updated)

    listVersionsH rawId = do
      ensureCRM
      proposalKey <- parseKey @ME.Proposal rawId
      versions <- withPool $ selectList
        [ ME.ProposalVersionProposalId ==. proposalKey ]
        [ Desc ME.ProposalVersionVersion ]
      pure (map proposalVersionSummaryToDTO versions)

    createVersionH rawId ProposalVersionCreate{..} = do
      ensureCRM
      proposalKey <- parseKey @ME.Proposal rawId
      _ <- ensureProposalExists proposalKey
      latex <- resolveLatex pvcLatex pvcTemplateKey
      now <- liftIO getCurrentTime
      mLatest <- withPool $ selectFirst
        [ ME.ProposalVersionProposalId ==. proposalKey ]
        [ Desc ME.ProposalVersionVersion, LimitTo 1 ]
      let nextVersion = maybe 1 (succ . ME.proposalVersionVersion . entityVal) mLatest
          versionRecord = ME.ProposalVersion
            { ME.proposalVersionProposalId   = proposalKey
            , ME.proposalVersionVersion      = nextVersion
            , ME.proposalVersionLatex        = latex
            , ME.proposalVersionCreatedAt    = now
            , ME.proposalVersionCreatedByRef = Just (toPathPiece (auPartyId user))
            , ME.proposalVersionNotes        = normalizeOptionalText pvcNotes
            }
      versionKey <- withPool $ insert versionRecord
      withPool $ update proposalKey [ME.ProposalUpdatedAt =. now]
      pure (proposalVersionToDTO proposalKey (Entity versionKey versionRecord))

    getVersionH rawId versionNumber = do
      ensureCRM
      proposalKey <- parseKey @ME.Proposal rawId
      mVersion <- withPool $ selectFirst
        [ ME.ProposalVersionProposalId ==. proposalKey
        , ME.ProposalVersionVersion ==. versionNumber
        ]
        []
      maybe (throwError err404) (pure . proposalVersionToDTO proposalKey) mVersion

    proposalPdfH rawId mVersion = do
      ensureCRM
      proposalKey <- parseKey @ME.Proposal rawId
      mProposal <- withPool $ getEntity proposalKey
      case mProposal of
        Nothing -> throwError err404
        Just (Entity _ proposal) -> do
          versionEnt <- fetchVersion proposalKey mVersion
          let latex = ME.proposalVersionLatex (entityVal versionEnt)
          pdfResult <- liftIO (InputList.generateInputListPdf latex)
          case pdfResult of
            Left errMsg -> throwError err500 { errBody = encodeUtf8Lazy errMsg }
            Right pdf -> do
              now <- liftIO getCurrentTime
              withPool $ update proposalKey
                [ ME.ProposalLastGeneratedAt =. Just now
                , ME.ProposalUpdatedAt =. now
                ]
              let fileName =
                    InputList.sanitizeFileName (ME.proposalTitle proposal) <> ".pdf"
                  disposition = T.concat ["attachment; filename=\"", fileName, "\""]
              pure (addHeader disposition pdf)

ensureProposalExists
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => ME.ProposalId
  -> m ()
ensureProposalExists proposalKey = do
  mProposal <- withPool $ getEntity proposalKey
  case mProposal of
    Nothing -> throwError err404
    Just _  -> pure ()

fetchVersion
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => ME.ProposalId
  -> Maybe Int
  -> m (Entity ME.ProposalVersion)
fetchVersion proposalKey mVersion =
  case mVersion of
    Just versionNum -> do
      mVersionEnt <- withPool $ selectFirst
        [ ME.ProposalVersionProposalId ==. proposalKey
        , ME.ProposalVersionVersion ==. versionNum
        ]
        []
      maybe (throwError err404) pure mVersionEnt
    Nothing -> do
      mVersionEnt <- withPool $ selectFirst
        [ ME.ProposalVersionProposalId ==. proposalKey ]
        [ Desc ME.ProposalVersionVersion, LimitTo 1 ]
      maybe (throwError err404) pure mVersionEnt

latestVersionMap
  :: (MonadReader Env m, MonadIO m)
  => [ME.ProposalId]
  -> m (Map.Map ME.ProposalId Int)
latestVersionMap [] = pure Map.empty
latestVersionMap proposalIds = do
  versions <- withPool $ selectList
    [ ME.ProposalVersionProposalId <-. proposalIds ]
    [ Desc ME.ProposalVersionVersion ]
  pure $ foldl'
    (\acc (Entity _ v) ->
      Map.insertWith max (ME.proposalVersionProposalId v) (ME.proposalVersionVersion v) acc)
    Map.empty
    versions

proposalToDTO :: Map.Map ME.ProposalId Int -> Entity ME.Proposal -> ProposalDTO
proposalToDTO versionMap (Entity key proposal) = ProposalDTO
  { proposalId      = toPathPiece key
  , title           = ME.proposalTitle proposal
  , status          = ME.proposalStatus proposal
  , serviceKind     = ME.proposalServiceKind proposal
  , clientPartyId   = fmap fromSqlKey (ME.proposalClientPartyId proposal)
  , contactName     = ME.proposalContactName proposal
  , contactEmail    = ME.proposalContactEmail proposal
  , contactPhone    = ME.proposalContactPhone proposal
  , pipelineCardId  = fmap toPathPiece (ME.proposalPipelineCardId proposal)
  , notes           = ME.proposalNotes proposal
  , createdAt       = ME.proposalCreatedAt proposal
  , updatedAt       = ME.proposalUpdatedAt proposal
  , lastGeneratedAt = ME.proposalLastGeneratedAt proposal
  , sentAt          = ME.proposalSentAt proposal
  , latestVersion   = Map.lookup key versionMap
  }

proposalVersionSummaryToDTO :: Entity ME.ProposalVersion -> ProposalVersionSummaryDTO
proposalVersionSummaryToDTO (Entity key v) = ProposalVersionSummaryDTO
  { versionId = toPathPiece key
  , version   = ME.proposalVersionVersion v
  , createdAt = ME.proposalVersionCreatedAt v
  , createdBy = ME.proposalVersionCreatedByRef v
  , notes     = ME.proposalVersionNotes v
  }

proposalVersionToDTO :: ME.ProposalId -> Entity ME.ProposalVersion -> ProposalVersionDTO
proposalVersionToDTO proposalKey (Entity key v) = ProposalVersionDTO
  { versionId  = toPathPiece key
  , proposalId = toPathPiece proposalKey
  , version    = ME.proposalVersionVersion v
  , latex      = ME.proposalVersionLatex v
  , createdAt  = ME.proposalVersionCreatedAt v
  , createdBy  = ME.proposalVersionCreatedByRef v
  , notes      = ME.proposalVersionNotes v
  }

normalizeOptionalText :: Maybe Text -> Maybe Text
normalizeOptionalText = (>>= normalizeText)

normalizeOptionalUpdate :: Maybe (Maybe Text) -> Maybe (Maybe Text)
normalizeOptionalUpdate = fmap normalizeOptionalText

normalizeLatex :: Maybe Text -> Maybe Text
normalizeLatex Nothing = Nothing
normalizeLatex (Just raw) =
  if T.null (T.strip raw) then Nothing else Just raw

normalizeText :: Text -> Maybe Text
normalizeText raw =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

requireText :: MonadError ServerError m => Text -> Text -> m Text
requireText label raw =
  case normalizeText raw of
    Nothing -> throwError err400 { errBody = encodeUtf8Lazy (label <> " required") }
    Just val -> pure val

requireStatus :: MonadError ServerError m => Text -> m Text
requireStatus raw =
  case normalizeText raw of
    Nothing -> throwError err400 { errBody = "status required" }
    Just val -> pure val

normalizeStatus :: Maybe Text -> Maybe Text
normalizeStatus = (>>= normalizeText)

sentAtFromStatus :: Maybe UTCTime -> Text -> UTCTime -> Maybe UTCTime
sentAtFromStatus current statusVal now =
  if isSentStatus statusVal && current == Nothing
    then Just now
    else current

sentAtUpdate :: Maybe UTCTime -> Maybe Text -> UTCTime -> Maybe (Update ME.Proposal)
sentAtUpdate current statusUpdate now =
  case statusUpdate of
    Nothing -> Nothing
    Just statusVal ->
      if isSentStatus statusVal && current == Nothing
        then Just (ME.ProposalSentAt =. Just now)
        else Nothing

isSentStatus :: Text -> Bool
isSentStatus =
  (== "sent") . T.toLower . T.filter (not . isSpace)
  where
    isSpace c = c == ' ' || c == '_' || c == '-'

parseOptionalKey
  :: forall record m.
     ( PathPiece (Key record)
     , MonadError ServerError m
     )
  => Maybe Text
  -> m (Maybe (Key record))
parseOptionalKey = traverse (parseKey @record)

parseOptionalKeyUpdate
  :: forall record m.
     ( PathPiece (Key record)
     , MonadError ServerError m
     )
  => Maybe (Maybe Text)
  -> m (Maybe (Maybe (Key record)))
parseOptionalKeyUpdate Nothing = pure Nothing
parseOptionalKeyUpdate (Just Nothing) = pure (Just Nothing)
parseOptionalKeyUpdate (Just (Just raw)) = Just . Just <$> parseKey @record raw

parseKey
  :: forall record m.
     ( PathPiece (Key record)
     , MonadError ServerError m
     )
  => Text
  -> m (Key record)
parseKey raw =
  maybe (throwError err400 { errBody = "Invalid identifier" }) pure (fromPathPiece raw)

withPool
  :: (MonadReader Env m, MonadIO m)
  => SqlPersistT IO a
  -> m a
withPool action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

resolveLatex
  :: (MonadIO m, MonadError ServerError m)
  => Maybe Text
  -> Maybe Text
  -> m Text
resolveLatex mLatex mTemplateKey =
  case normalizeLatex mLatex of
    Just latex -> pure latex
    Nothing -> do
      key <- maybe (throwError err400 { errBody = "latex or templateKey required" }) pure mTemplateKey
      mTemplate <- liftIO (loadTemplate key)
      case mTemplate of
        Nothing -> throwError err404 { errBody = "Template not found" }
        Just template -> pure template

loadTemplate :: Text -> IO (Maybe Text)
loadTemplate key =
  if isSafeTemplateKey key
    then do
      let path = templatesDir </> T.unpack key <> ".tex"
      exists <- doesFileExist path
      if exists then Just <$> TIO.readFile path else pure Nothing
    else pure Nothing

templatesDir :: FilePath
templatesDir = "templates" </> "proposals"

isSafeTemplateKey :: Text -> Bool
isSafeTemplateKey key =
  not (T.null key) && T.all isAllowed key
  where
    isAllowed c = isAscii c && (isAlphaNum c || c == '-' || c == '_')

encodeUtf8Lazy :: Text -> BL.ByteString
encodeUtf8Lazy = BL.fromStrict . TE.encodeUtf8
