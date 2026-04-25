{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TDF.ServerProposals
  ( proposalsServer
  , ProposalContentSource(..)
  , resolveOptionalProposalClientPartyReference
  , resolveOptionalProposalPipelineCardReference
  , resolveOptionalProposalPipelineCardReferenceUpdate
  , validateOptionalProposalStatus
  , validateOptionalProposalContactEmail
  , validateOptionalProposalContactPhone
  , validateOptionalProposalClientPartyId
  , validateProposalContentSource
  , validateProposalTitle
  , validateProposalStatus
  , validateProposalVersionNumber
  , validateTemplateKey
  ) where

import           Control.Monad              (unless, when)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Data.Char                  (isAlphaNum, isAscii, isAsciiLower, isControl, isDigit)
import           Data.Int                   (Int64)
import           Data.List                  (foldl')
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, maybeToList)
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
import           TDF.Models                 (Party)
import qualified TDF.ModelsExtra            as ME
import qualified TDF.Handlers.InputList     as InputList
import           TDF.WhatsApp.History       (normalizeWhatsAppPhone)

data ProposalContentSource
  = ProposalInlineLatex Text
  | ProposalTemplateKey Text
  deriving (Eq, Show)

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
        Nothing -> throwError proposalNotFound
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
      title <- either throwError pure (validateProposalTitle pcTitle)
      latex <- resolveLatex pcLatex pcTemplateKey
      statusVal <- either throwError pure (validateProposalStatus pcStatus)
      contactName <- either throwError pure (validateOptionalProposalContactName pcContactName)
      contactEmail <- either throwError pure (validateOptionalProposalContactEmail pcContactEmail)
      contactPhone <- either throwError pure (validateOptionalProposalContactPhone pcContactPhone)
      clientPartyKey <- withPool (resolveOptionalProposalClientPartyReference pcClientPartyId)
        >>= either throwError pure
      pipelineCardKey <- withPool (resolveOptionalProposalPipelineCardReference pcPipelineCardId)
        >>= either throwError pure
      now <- liftIO getCurrentTime
      let proposalRecord = ME.Proposal
            { ME.proposalTitle          = title
            , ME.proposalServiceKind    = pcServiceKind
            , ME.proposalClientPartyId  = clientPartyKey
            , ME.proposalContactName    = contactName
            , ME.proposalContactEmail   = contactEmail
            , ME.proposalContactPhone   = contactPhone
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
          titleUpdate <- either throwError pure (traverse validateProposalTitle puTitle)
          statusUpdate <- either throwError pure (validateOptionalProposalStatus puStatus)
          contactNameUpdate <- either throwError pure
            (traverse validateOptionalProposalContactName puContactName)
          contactEmailUpdate <- either throwError pure
            (traverse validateOptionalProposalContactEmail puContactEmail)
          contactPhoneUpdate <- either throwError pure
            (traverse validateOptionalProposalContactPhone puContactPhone)
          clientPartyIdUpdate <- case puClientPartyId of
            Nothing -> pure Nothing
            Just rawClientPartyId -> do
              resolvedClientParty <- withPool (resolveOptionalProposalClientPartyReference rawClientPartyId)
              Just <$> either throwError pure resolvedClientParty
          pipelineCardUpdate <- withPool (resolveOptionalProposalPipelineCardReferenceUpdate puPipelineCardId)
            >>= either throwError pure
          let updates = catMaybes
                [ fmap (ME.ProposalTitle =.) titleUpdate
                , fmap (ME.ProposalStatus =.) statusUpdate
                , fmap (ME.ProposalServiceKind =.) puServiceKind
                , fmap (ME.ProposalClientPartyId =.) clientPartyIdUpdate
                , fmap (ME.ProposalContactName =.) contactNameUpdate
                , fmap (ME.ProposalContactEmail =.) contactEmailUpdate
                , fmap (ME.ProposalContactPhone =.) contactPhoneUpdate
                , fmap (ME.ProposalPipelineCardId =.) pipelineCardUpdate
                , fmap (ME.ProposalNotes =.) (normalizeOptionalUpdate puNotes)
                ]
              sentUpdate = sentAtUpdate (ME.proposalSentAt proposal) statusUpdate now
              updates' = updates ++ maybeToList sentUpdate
          when (null updates') $
            throwError err400 { errBody = "Proposal update must include at least one field" }
          let finalUpdates = updates' ++ [ME.ProposalUpdatedAt =. now]
          withPool $ update key finalUpdates
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
      ensureProposalExists proposalKey
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
      validVersionNumber <- either throwError pure (validateProposalVersionNumber versionNumber)
      ensureProposalExists proposalKey
      mVersion <- withPool $ selectFirst
        [ ME.ProposalVersionProposalId ==. proposalKey
        , ME.ProposalVersionVersion ==. validVersionNumber
        ]
        []
      maybe (throwError proposalVersionNotFound) (pure . proposalVersionToDTO proposalKey) mVersion

    proposalPdfH rawId mVersion = do
      ensureCRM
      proposalKey <- parseKey @ME.Proposal rawId
      validVersion <- traverse (either throwError pure . validateProposalVersionNumber) mVersion
      mProposal <- withPool $ getEntity proposalKey
      case mProposal of
        Nothing -> throwError proposalNotFound
        Just (Entity _ proposal) -> do
          versionEnt <- fetchVersion proposalKey validVersion
          let latex = ME.proposalVersionLatex (entityVal versionEnt)
          pdfResult <- liftIO (InputList.generateInputListPdfWithAssets (Just proposalAssetsDir) latex)
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
    Nothing -> throwError proposalNotFound
    Just _  -> pure ()

proposalNotFound :: ServerError
proposalNotFound = err404 { errBody = "Proposal not found" }

proposalVersionNotFound :: ServerError
proposalVersionNotFound = err404 { errBody = "Proposal version not found" }

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
      maybe (throwError proposalVersionNotFound) pure mVersionEnt
    Nothing -> do
      mVersionEnt <- withPool $ selectFirst
        [ ME.ProposalVersionProposalId ==. proposalKey ]
        [ Desc ME.ProposalVersionVersion, LimitTo 1 ]
      maybe (throwError proposalVersionNotFound) pure mVersionEnt

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

maxProposalTitleChars :: Int
maxProposalTitleChars = 160

validateProposalTitle :: Text -> Either ServerError Text
validateProposalTitle rawTitle
  | T.null title =
      Left err400 { errBody = "title is required" }
  | T.length title > maxProposalTitleChars =
      Left err400 { errBody = "title must be 160 characters or fewer" }
  | T.any isControl title =
      Left err400 { errBody = "title must not contain control characters" }
  | otherwise =
      Right title
  where
    title = T.strip rawTitle

validateProposalStatus :: Maybe Text -> Either ServerError Text
validateProposalStatus Nothing = Right "draft"
validateProposalStatus (Just rawStatus) =
  case normalizeProposalStatus rawStatus of
    Just statusVal -> Right statusVal
    Nothing ->
      Left err400 { errBody = "status must be one of: draft, sent" }

validateOptionalProposalStatus :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalProposalStatus Nothing = Right Nothing
validateOptionalProposalStatus (Just rawStatus) =
  Just <$> validateProposalStatus (Just rawStatus)

validateProposalVersionNumber :: Int -> Either ServerError Int
validateProposalVersionNumber rawVersion
  | rawVersion < 1 =
      Left err400 { errBody = "version must be a positive integer" }
  | otherwise = Right rawVersion

validateOptionalProposalContactName :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalProposalContactName Nothing = Right Nothing
validateOptionalProposalContactName (Just rawName) =
  case normalizeOptionalText (Just rawName) of
    Nothing -> Right Nothing
    Just contactName
      | T.any isControl contactName ->
          Left err400 { errBody = "contactName must not contain control characters" }
      | otherwise ->
          Right (Just contactName)

validateOptionalProposalContactEmail :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalProposalContactEmail Nothing = Right Nothing
validateOptionalProposalContactEmail (Just rawEmail) =
  case normalizeOptionalText (Just rawEmail) of
    Nothing -> Right Nothing
    Just emailVal ->
      let normalized = T.toLower emailVal
      in if isValidProposalEmail normalized
           then Right (Just normalized)
           else Left err400 { errBody = "contactEmail must be a valid email address" }

validateOptionalProposalContactPhone :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalProposalContactPhone Nothing = Right Nothing
validateOptionalProposalContactPhone (Just rawPhone) =
  case normalizeOptionalText (Just rawPhone) of
    Nothing -> Right Nothing
    Just phoneVal
      | T.any isControl phoneVal ->
          Left err400 { errBody = "contactPhone must not contain control characters" }
      | otherwise ->
          case normalizeWhatsAppPhone phoneVal of
            Just normalizedPhone -> Right (Just normalizedPhone)
            Nothing ->
              Left err400 { errBody = "contactPhone must be a valid phone number" }

validateOptionalProposalClientPartyId :: Maybe Int64 -> Either ServerError (Maybe Int64)
validateOptionalProposalClientPartyId Nothing = Right Nothing
validateOptionalProposalClientPartyId (Just rawClientPartyId)
  | rawClientPartyId <= 0 =
      Left err400 { errBody = "clientPartyId must be a positive integer" }
  | otherwise =
      Right (Just rawClientPartyId)

resolveOptionalProposalClientPartyReference
  :: Maybe Int64
  -> SqlPersistT IO (Either ServerError (Maybe (Key Party)))
resolveOptionalProposalClientPartyReference rawClientPartyId =
  case validateOptionalProposalClientPartyId rawClientPartyId of
    Left err -> pure (Left err)
    Right Nothing -> pure (Right Nothing)
    Right (Just clientPartyId) -> do
      let partyKey = toSqlKey clientPartyId
      mParty <- getEntity partyKey
      pure $
        case mParty of
          Nothing ->
            Left err422
              { errBody = encodeUtf8Lazy "clientPartyId references an unknown party" }
          Just _ -> Right (Just partyKey)

resolveOptionalProposalPipelineCardReference
  :: Maybe Text
  -> SqlPersistT IO (Either ServerError (Maybe ME.PipelineCardId))
resolveOptionalProposalPipelineCardReference Nothing = pure (Right Nothing)
resolveOptionalProposalPipelineCardReference (Just rawPipelineCardId) = do
  case normalizeOptionalText (Just rawPipelineCardId) of
    Nothing ->
      pure (Right Nothing)
    Just pipelineCardId ->
      case fromPathPiece pipelineCardId of
        Nothing ->
          pure
            (Left err400 { errBody = encodeUtf8Lazy "pipelineCardId must be a valid identifier" })
        Just pipelineCardKey -> do
          mPipelineCard <- getEntity pipelineCardKey
          pure $
            case mPipelineCard of
              Nothing ->
                Left err422
                  { errBody = encodeUtf8Lazy "pipelineCardId references an unknown pipeline card" }
              Just _ ->
                Right (Just pipelineCardKey)

resolveOptionalProposalPipelineCardReferenceUpdate
  :: Maybe (Maybe Text)
  -> SqlPersistT IO (Either ServerError (Maybe (Maybe ME.PipelineCardId)))
resolveOptionalProposalPipelineCardReferenceUpdate Nothing =
  pure (Right Nothing)
resolveOptionalProposalPipelineCardReferenceUpdate (Just Nothing) =
  pure (Right (Just Nothing))
resolveOptionalProposalPipelineCardReferenceUpdate (Just (Just rawPipelineCardId)) = do
  resolved <- resolveOptionalProposalPipelineCardReference (Just rawPipelineCardId)
  pure (fmap Just resolved)

normalizeProposalStatus :: Text -> Maybe Text
normalizeProposalStatus rawStatus =
  case T.toLower (T.strip rawStatus) of
    "draft" -> Just "draft"
    "sent" -> Just "sent"
    _ -> Nothing

isValidProposalEmail :: Text -> Bool
isValidProposalEmail candidate =
  case T.split (== '@') candidate of
    [localPart, domain] ->
      T.length candidate <= maxProposalContactEmailLength
        && isValidProposalEmailLocalPart localPart
        && not (T.null domain)
        && not (T.any (`elem` [' ', '\t', '\n', '\r']) candidate)
        && T.isInfixOf "." domain
        && all isValidProposalDomainLabel (T.splitOn "." domain)
    _ -> False

maxProposalContactEmailLength :: Int
maxProposalContactEmailLength = 254

isValidProposalEmailLocalPart :: Text -> Bool
isValidProposalEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= maxProposalEmailLocalPartLength
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (".." `T.isInfixOf` localPart)
    && T.all isValidProposalEmailLocalChar localPart

maxProposalEmailLocalPartLength :: Int
maxProposalEmailLocalPartLength = 64

isValidProposalEmailLocalChar :: Char -> Bool
isValidProposalEmailLocalChar c =
  isAsciiLower c || isDigit c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidProposalDomainLabel :: Text -> Bool
isValidProposalDomainLabel label =
  not (T.null label)
    && T.length label <= maxProposalEmailDomainLabelLength
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidProposalDomainChar label

maxProposalEmailDomainLabelLength :: Int
maxProposalEmailDomainLabelLength = 63

isValidProposalDomainChar :: Char -> Bool
isValidProposalDomainChar c = isAsciiLower c || isDigit c || c == '-'

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
  case validateProposalContentSource mLatex mTemplateKey of
    Left err -> throwError err
    Right (ProposalInlineLatex latex) -> pure latex
    Right (ProposalTemplateKey key) -> do
      mTemplate <- liftIO (loadTemplate key)
      case mTemplate of
        Nothing -> throwError err404 { errBody = "Template not found" }
        Just template -> pure template

validateProposalContentSource :: Maybe Text -> Maybe Text -> Either ServerError ProposalContentSource
validateProposalContentSource mLatex mTemplateKey =
  case (normalizeLatex mLatex, normalizeOptionalText mTemplateKey) of
    (Just latex, Nothing) ->
      Right (ProposalInlineLatex latex)
    (Nothing, Just rawKey) ->
      ProposalTemplateKey <$> validateTemplateKey rawKey
    (Nothing, Nothing) ->
      Left err400 { errBody = "latex or templateKey required" }
    (Just _, Just _) ->
      Left err400 { errBody = "Provide either latex or templateKey, not both" }

validateTemplateKey :: Text -> Either ServerError Text
validateTemplateKey raw =
  let canonical = canonicalTemplateKey raw
  in if T.null canonical
      then Left err400 { errBody = "templateKey required" }
      else
        if T.length canonical > maxTemplateKeyLength
          then Left err400 { errBody = "templateKey must be 96 characters or fewer" }
          else if isSafeTemplateKey canonical
          then Right canonical
          else Left err400
            { errBody = "templateKey must contain only ASCII letters, numbers, hyphens, or underscores, and include at least one letter or number"
            }

loadTemplate :: Text -> IO (Maybe Text)
loadTemplate key =
  case validateTemplateKey key of
    Left _ -> pure Nothing
    Right canonical -> do
      let path = templatesDir </> T.unpack canonical <> ".tex"
      exists <- doesFileExist path
      if exists then Just <$> TIO.readFile path else pure Nothing

templatesDir :: FilePath
templatesDir = "templates" </> "proposals"

proposalAssetsDir :: FilePath
proposalAssetsDir = templatesDir </> "assets"

isSafeTemplateKey :: Text -> Bool
isSafeTemplateKey key =
  not (T.null key) && T.any isAsciiAlphaNum key && T.all isAllowed key
  where
    isAllowed c = isAscii c && (isAlphaNum c || c == '-' || c == '_')
    isAsciiAlphaNum c = isAscii c && isAlphaNum c

maxTemplateKeyLength :: Int
maxTemplateKeyLength = 96

canonicalTemplateKey :: Text -> Text
canonicalTemplateKey = T.toLower . T.strip

encodeUtf8Lazy :: Text -> BL.ByteString
encodeUtf8Lazy = BL.fromStrict . TE.encodeUtf8
