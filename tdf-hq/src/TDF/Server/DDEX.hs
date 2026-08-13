{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.Server.DDEX (ddexServer, validateDdexAccess) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import qualified Data.Set as Set
import Database.Persist (get, entityKey, entityVal, Entity)
import Database.Persist.Sql (SqlPersistT, runSqlPool, toSqlKey, fromSqlKey)
import Servant
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, takeFileName)
import TDF.API.DDEX
import TDF.Auth (AuthedUser(..), validateModuleAccess, moduleName, ModuleAccess(..))
import qualified TDF.Catalog.DB as CatalogDB
import TDF.DB (Env(..))
import qualified TDF.DDEX.DB as DB
import qualified TDF.DDEX.Detect as Detect
import qualified TDF.DDEX.ERN.V432.Normalize as Normalize
import qualified TDF.DDEX.ERN.V432.Parse as Parse
import qualified TDF.DDEX.ERN.V432.Types as ERN
import qualified TDF.DDEX.ERN.V432.BusinessRules as BusinessRules
import qualified TDF.DDEX.Models as M
import qualified TDF.DDEX.Security as Security
import qualified TDF.DDEX.Storage as Storage
import qualified TDF.DDEX.Types as DDEXTypes
import TDF.FeatureRegistry (findRegistryFeature, registryFeatureAllows)

type AppM = ReaderT Env Handler

-- | Main DDEX Server Implementation
ddexServer :: AuthedUser -> ServerT DDEXAPI AppM
ddexServer user =
       uploadDocumentHandler user
  :<|> listDocumentsHandler user
  :<|> getDocumentHandler user
  :<|> downloadRawHandler user
  :<|> validateDocumentHandler user
  :<|> getValidationReportHandler user
  :<|> getPreviewHandler user
  :<|> createImportPlanHandler user
  :<|> resolveImportPlanHandler user
  :<|> commitImportPlanHandler user
  :<|> createExportHandler user
  :<|> downloadExportHandler user
  :<|> listPartnersHandler user
  :<|> createPartnerHandler user
  :<|> getCatalogByDocumentHandler user

-- | Upload a DDEX document
uploadDocumentHandler :: AuthedUser -> DdexUploadRequest -> AppM DdexDocumentDTO
uploadDocumentHandler user DdexUploadRequest{..} = do
  requireDdexAccess "label.ddex.inbox" "import" user
  fileName <- either (throwError . invalidUpload) pure (validateUploadName uploadFileName)
  let uploadMimeType = T.toLower (T.strip uploadContentType)
  unless (uploadMimeType `elem` ["application/xml", "text/xml"]) $
    throwError err415 { errBody = "DDEX upload must use application/xml or text/xml" }
  when (T.length uploadContentBase64 > 69905068) $
    throwError err413 { errBody = "DDEX upload exceeds the 50 MiB decoded limit" }
  decoded <- case B64.decode (TE.encodeUtf8 (T.strip uploadContentBase64)) of
    Left _ -> throwError err400 { errBody = "DDEX upload content is not valid base64" }
    Right value -> pure (BL.fromStrict value)
  _ <- either (throwError . unsafeXml) pure $
    Security.safeParseXml Security.defaultXmlParseConfig decoded
  detection <- maybe
    (throwError err422 { errBody = "The XML root and namespace do not identify a supported DDEX family" })
    pure
    (Detect.detectDocument decoded)
  storage <- loadDdexStorage
  env <- ask
  let sha256 = Storage.computeSha256 decoded
  existing <- liftIO $ runSqlPool (DB.findDocumentBySha256 sha256) (envPool env)
  case existing of
    Just document -> pure (documentEntityToDTO document)
    Nothing -> do
      stored <- liftIO $ Storage.storeFile storage fileName decoded uploadMimeType
      let version = DDEXTypes.detectionVersion detection
          namespace = nonEmpty (DDEXTypes.detectionNamespace detection)
          uploadActor = fromIntegral (fromSqlKey (auPartyId user))
      inserted <- liftIO $ runSqlPool
        (DB.insertDocument
          fileName
          (Storage.storedFileUri stored)
          sha256
          (fromIntegral (Storage.storedFileSize stored))
          (DDEXTypes.detectionFamily detection)
          version
          namespace
          (Just (DDEXTypes.detectionRoot detection))
          DDEXTypes.StatusReceived
          uploadActor
          Nothing
          Nothing
          Nothing)
        (envPool env)
      case inserted of
        Nothing -> do
          _ <- liftIO $ Storage.deleteFile storage (Storage.storedFilePath stored)
          raced <- liftIO $ runSqlPool (DB.findDocumentBySha256 sha256) (envPool env)
          maybe (throwError err409 { errBody = "A concurrent upload created this document; retry retrieval" })
            (pure . documentEntityToDTO) raced
        Just documentId -> do
          created <- liftIO $ runSqlPool (DB.getDocumentById documentId) (envPool env)
          maybe (throwError err500 { errBody = "Stored DDEX document could not be reloaded" })
            (pure . documentEntityToDTO) created

-- | List DDEX documents
listDocumentsHandler :: AuthedUser -> Maybe Text -> Maybe Text -> AppM [DdexDocumentDTO]
listDocumentsHandler user mStatus mPartner = do
  requireDdexAccess "label.ddex.inbox" "view" user
  let validStatuses =
        [ "received", "quarantined", "queued", "validating", "invalid", "valid"
        , "mapping_required", "ready_to_import", "importing", "imported"
        , "import_failed", "superseded"
        ]
  when (maybe False (`notElem` validStatuses) mStatus) $
    throwError err400 { errBody = "Unsupported DDEX document status" }
  env <- ask
  docEntities <- liftIO $ runSqlPool (DB.listDocuments mStatus mPartner) (envPool env)
  return $ map documentEntityToDTO docEntities

-- | Get a single DDEX document
getDocumentHandler :: AuthedUser -> Int -> AppM DdexDocumentDTO
getDocumentHandler user docId = do
  requireDdexAccess "label.ddex.document" "view" user
  env <- ask
  mDocEntity <- liftIO $ runSqlPool (DB.getDocumentById (toSqlKey (fromIntegral docId))) (envPool env)
  case mDocEntity of
    Nothing -> throwError err404 { errBody = "Document not found" }
    Just docEntity -> return $ documentEntityToDTO docEntity

-- | Download raw XML file
downloadRawHandler :: AuthedUser -> Int -> AppM DdexDownloadResponse
downloadRawHandler user docId = do
  requireDdexAccess "label.ddex.document" "view" user
  (document, content) <- loadStoredDocument docId
  pure DdexDownloadResponse
    { downloadFileName = M.ddexDocumentFileName document
    , downloadContentType = "application/xml"
    , downloadContentBase64 = TE.decodeUtf8 (B64.encode (BL.toStrict content))
    }

-- | Validate a document
validateDocumentHandler :: AuthedUser -> Int -> AppM ValidationRunDTO
validateDocumentHandler user docId = do
  requireDdexAccess "label.ddex.document" "validate" user
  (document, content) <- loadStoredDocument docId
  env <- ask
  now <- liftIO getCurrentTime
  runId <- liftIO $ runSqlPool
    (DB.insertValidationRun (toSqlKey (fromIntegral docId)) (Just "tdf-structural-1") Nothing)
    (envPool env)
  let internalIssues = validateStoredErn document docId content
      profileIssue = InternalValidationIssue
        M.SeverityWarning
        M.LayerXSD
        "PROFILE_VALIDATION_REQUIRED"
        "Official DDEX XSD and recipient business-profile validation has not run"
        (Just "Configure licensed schemas and an approved recipient profile before import or delivery")
      issues = internalIssues ++ [profileIssue]
      errorCount = length [() | issue <- issues, iviSeverity issue == M.SeverityError]
      warningCount = length [() | issue <- issues, iviSeverity issue == M.SeverityWarning]
      result
        | errorCount > 0 = M.ResultFailure
        | otherwise = M.ResultWarning
      documentStatus
        | errorCount > 0 = DDEXTypes.StatusInvalid
        | otherwise = DDEXTypes.StatusMappingRequired
  liftIO $ runSqlPool (do
    mapM_ (persistInternalIssue runId) issues
    DB.completeValidationRun runId result errorCount warningCount
    DB.updateDocumentStatus (toSqlKey (fromIntegral docId)) documentStatus) (envPool env)
  finished <- liftIO getCurrentTime
  return ValidationRunDTO
    { validationRunId = fromIntegral $ fromSqlKey runId
    , validationRunDocumentId = docId
    , validationRunStatus = if errorCount > 0 then "validation_failed" else "profile_validation_required"
    , validationRunStartedAt = now
    , validationRunFinishedAt = Just finished
    }

-- | Get validation report
getValidationReportHandler :: AuthedUser -> Int -> AppM ValidationReportDTO
getValidationReportHandler user docId = do
  requireDdexAccess "label.ddex.document" "view" user
  env <- ask
  mReport <- liftIO $ runSqlPool (DB.getValidationReport (toSqlKey (fromIntegral docId))) (envPool env)
  case mReport of
    Nothing -> throwError err404 { errBody = "No validation report is available" }
    Just (runEntity, issues) -> return ValidationReportDTO
      { reportRunId = fromIntegral $ fromSqlKey (entityKey runEntity)
      , reportIssues = map issueToDTO issues
      , reportIsValid = M.ddexValidationRunResult (entityVal runEntity) == Just M.ResultSuccess
      }

-- | Get document preview
getPreviewHandler :: AuthedUser -> Int -> AppM DdexPreviewDTO
getPreviewHandler user docId = do
  requireDdexAccess "label.ddex.document" "view" user
  (document, content) <- loadStoredDocument docId
  unless (M.ddexDocumentFamily document == M.FamilyERN && M.ddexDocumentVersion document == "4.3.2") $
    throwError err503 { errBody = "Preview is enabled only for the validated ERN 4.3.2 profile" }
  parsed <- either (throwError . parseFailure) pure (Parse.parseErnMessage content)
  normalized <- either (throwError . normalizeFailure) pure (Normalize.normalizeErnMessage docId parsed)
  pure DdexPreviewDTO
    { previewMessageId = ERN.mhMessageId (ERN.ernMessageHeader parsed)
    , previewSender = T.pack (show (ERN.mhSenderPartyId (ERN.ernMessageHeader parsed)))
    , previewReleaseCount = length (Normalize.ciReleases normalized)
    , previewResourceCount = length (Normalize.ciResources normalized)
    , previewWarnings =
        [ "Preview only: no catalog records, deliveries, or statuses were changed."
        , "Official XSD and recipient-profile validation must pass before import."
        ]
    }

-- | Create import plan
createImportPlanHandler :: AuthedUser -> Int -> AppM ImportPlanDTO
createImportPlanHandler user _docId = do
  requireDdexAccess "label.ddex.import" "import" user
  capabilityDisabled "DDEX import planning" "official schema/profile validation and the transactional conflict engine are not enabled"

-- | Resolve import plan conflicts
resolveImportPlanHandler :: AuthedUser -> Int -> ImportPlanResolution -> AppM ImportPlanDTO
resolveImportPlanHandler user _ _ = do
  requireDdexAccess "label.ddex.import" "import" user
  capabilityDisabled "DDEX import resolution" "the transactional conflict engine is not enabled"

-- | Commit import plan
commitImportPlanHandler :: AuthedUser -> Int -> AppM ImportRunDTO
commitImportPlanHandler user _ = do
  requireDdexAccess "label.ddex.import" "approve" user
  capabilityDisabled "DDEX import commit" "rollback-tested catalog import is not enabled"

-- | Create export
createExportHandler :: AuthedUser -> DdexExportRequest -> AppM DdexExportDTO
createExportHandler user _req = do
  requireDdexAccess "label.ddex.inbox" "export" user
  capabilityDisabled "DDEX export" "a validated recipient profile, sender DPID, and private package store are required"

-- | Download export
downloadExportHandler :: AuthedUser -> Int -> AppM DdexDownloadResponse
downloadExportHandler user exportId = do
  requireDdexAccess "label.ddex.inbox" "export" user
  let _ = exportId
  capabilityDisabled "DDEX export download" "no immutable validated export package exists"

-- | List partners
listPartnersHandler :: AuthedUser -> AppM [DdexPartnerDTO]
listPartnersHandler user = do
  requireDdexAccess "label.ddex.partners" "view" user
  env <- ask
  partnerEntities <- liftIO $ runSqlPool DB.listPartners (envPool env)
  return $ map partnerEntityToDTO partnerEntities

-- | Create partner
createPartnerHandler :: AuthedUser -> DdexPartnerCreateRequest -> AppM DdexPartnerDTO
createPartnerHandler user req = do
  requireDdexAccess "label.ddex.partners" "create" user
  let cleanName = T.strip (partnerName req)
      cleanDpid = T.strip <$> partnerDpid req
      versions = partnerAllowedVersions req
      supportedVersions = ["3.8.2", "4.2", "4.3", "4.3.2"]
      invalidText value maxLength = T.null value || T.length value > maxLength || T.any isControl value
  if invalidText cleanName 160
      || maybe False (\value -> invalidText value 200) cleanDpid
      || null versions
      || length versions > 10
      || length versions /= Set.size (Set.fromList versions)
      || any (`notElem` supportedVersions) versions
    then throwError err400 { errBody = "Invalid DDEX partner configuration" }
    else pure ()
  env <- ask
  mPartnerId <- liftIO $ runSqlPool (DB.insertPartner cleanName cleanDpid versions) (envPool env)
  case mPartnerId of
    Nothing -> throwError err409 { errBody = "DDEX partner already exists" }
    Just partnerId -> do
      mPartner <- liftIO $ runSqlPool (get partnerId) (envPool env)
      case mPartner of
        Nothing -> throwError err500 { errBody = "Failed to create partner" }
        Just partner -> return $ partnerToDTO partnerId partner

-- | Get catalog releases by document
getCatalogByDocumentHandler :: AuthedUser -> Maybe Int -> AppM [CatalogReleaseDTO]
getCatalogByDocumentHandler user mDocumentId = do
  requireDdexAccess "label.ddex.document" "view" user
  env <- ask
  liftIO $ runSqlPool (CatalogDB.listReleaseDTOs mDocumentId) (envPool env)

loadDdexStorage :: AppM Storage.StorageBackend
loadDdexStorage = do
  backend <- liftIO $ lookupEnv "DDEX_STORAGE_BACKEND"
  root <- liftIO $ lookupEnv "DDEX_PRIVATE_STORAGE_ROOT"
  case (backend, root) of
    (Just "local-private", Just storageRoot)
      | isAbsolute storageRoot && not (null storageRoot) ->
          pure $ Storage.localStorageBackend Storage.StorageConfig
            { Storage.storageBasePath = storageRoot
            , Storage.storageBucket = "ddex-private"
            }
    _ -> capabilityDisabled
      "DDEX private storage"
      "set DDEX_STORAGE_BACKEND=local-private and an absolute DDEX_PRIVATE_STORAGE_ROOT for staging; production object storage is not configured"

loadStoredDocument :: Int -> AppM (M.DdexDocument, BL.ByteString)
loadStoredDocument docId = do
  env <- ask
  entity <- liftIO $ runSqlPool
    (DB.getDocumentById (toSqlKey (fromIntegral docId)))
    (envPool env)
  document <- maybe (throwError err404 { errBody = "Document not found" }) (pure . entityVal) entity
  relativePath <- case T.stripPrefix "local-private://" (M.ddexDocumentPrivateUri document) of
    Just path | not (T.null path) -> pure (T.unpack path)
    _ -> capabilityDisabled "DDEX document retrieval" "the document uses an unavailable private-storage adapter"
  storage <- loadDdexStorage
  content <- liftIO $ Storage.retrieveFile storage relativePath
  bytes <- maybe (throwError err404 { errBody = "Private DDEX object is unavailable" }) pure content
  unless (Storage.computeSha256 bytes == M.ddexDocumentSha256 document) $
    throwError err500 { errBody = "Private DDEX object checksum verification failed" }
  pure (document, bytes)

validateUploadName :: Text -> Either Text Text
validateUploadName rawName =
  let cleanName = T.strip rawName
      fileNameOnly = T.pack (takeFileName (T.unpack cleanName))
  in if T.null cleanName
        || T.length cleanName > 255
        || fileNameOnly /= cleanName
        || T.any isControl cleanName
        || not (".xml" `T.isSuffixOf` T.toLower cleanName)
      then Left "DDEX filename must be a safe .xml basename of at most 255 characters"
      else Right cleanName

nonEmpty :: Text -> Maybe Text
nonEmpty value
  | T.null (T.strip value) = Nothing
  | otherwise = Just (T.strip value)

invalidUpload :: Text -> ServerError
invalidUpload message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

unsafeXml :: [Security.XmlSecurityError] -> ServerError
unsafeXml errors = err422
  { errBody = BL.fromStrict (TE.encodeUtf8 ("Unsafe or malformed XML: " <> T.pack (show errors))) }

parseFailure :: [Parse.ParseError] -> ServerError
parseFailure errors = err422
  { errBody = BL.fromStrict (TE.encodeUtf8 ("ERN parsing failed: " <> T.pack (show errors))) }

normalizeFailure :: [Normalize.NormalizationError] -> ServerError
normalizeFailure errors = err422
  { errBody = BL.fromStrict (TE.encodeUtf8 ("ERN normalization failed: " <> T.pack (show errors))) }

capabilityDisabled :: Text -> Text -> AppM a
capabilityDisabled capability blocker =
  throwError err503
    { errBody = BL.fromStrict . TE.encodeUtf8 $
        capability <> " is feature-disabled: " <> blocker
    }

data InternalValidationIssue = InternalValidationIssue
  { iviSeverity   :: M.ValidationSeverityEnum
  , iviLayer      :: M.ValidationLayerEnum
  , iviCode       :: Text
  , iviMessage    :: Text
  , iviSuggestion :: Maybe Text
  }

validateStoredErn :: M.DdexDocument -> Int -> BL.ByteString -> [InternalValidationIssue]
validateStoredErn document docId content
  | M.ddexDocumentFamily document /= M.FamilyERN =
      [unsupported "FAMILY_NOT_IMPLEMENTED" "Only ERN intake validation is implemented"]
  | M.ddexDocumentVersion document /= "4.3.2" =
      [unsupported "ERN_PROFILE_NOT_IMPLEMENTED" "Only ERN 4.3.2 intake validation is implemented"]
  | otherwise = case Parse.parseErnMessage content of
      Left errors -> map parseIssue errors
      Right ern -> case Normalize.normalizeErnMessage docId ern of
        Left errors -> map normalizationIssue errors
        Right normalized -> map businessIssue (BusinessRules.validateBusinessRules normalized)
  where
    unsupported code message = InternalValidationIssue
      M.SeverityError M.LayerBusiness code message Nothing
    parseIssue err = InternalValidationIssue
      M.SeverityError
      M.LayerXML
      "ERN_PARSE_ERROR"
      (Parse.peMessage err)
      Nothing
    normalizationIssue err = InternalValidationIssue
      M.SeverityError
      M.LayerBusiness
      "ERN_NORMALIZATION_ERROR"
      (Normalize.neMessage err)
      Nothing
    businessIssue violation = InternalValidationIssue
      (case BusinessRules.brvSeverity violation of
        BusinessRules.RuleError -> M.SeverityError
        BusinessRules.RuleWarning -> M.SeverityWarning
        BusinessRules.RuleInfo -> M.SeverityInfo)
      M.LayerBusiness
      (BusinessRules.brvRule violation)
      (BusinessRules.brvMessage violation)
      (BusinessRules.brvSuggestion violation)

persistInternalIssue :: M.DdexValidationRunId -> InternalValidationIssue -> SqlPersistT IO ()
persistInternalIssue runId issue = do
  _ <- DB.insertValidationIssue
    runId
    (iviSeverity issue)
    (iviLayer issue)
    (Just (iviCode issue))
    Nothing
    Nothing
    Nothing
    (iviMessage issue)
    (iviSuggestion issue)
  pure ()

requireDdexAccess :: Text -> Text -> AuthedUser -> AppM ()
requireDdexAccess featureId action user =
  either throwError pure (validateDdexAccess featureId action user)

validateDdexAccess :: Text -> Text -> AuthedUser -> Either ServerError ()
validateDdexAccess featureId action user@AuthedUser{..} = do
  validateModuleAccess ModuleCatalog user
  case findRegistryFeature featureId of
    Nothing -> Left err500 { errBody = "DDEX feature authorization is not configured" }
    Just feature ->
      let modules = map moduleName (Set.toList auModules)
      in if registryFeatureAllows auRoles modules feature action
          then Right ()
          else Left err403 { errBody = "Feature action is not permitted" }

-- ============================================================
-- Conversion helpers
-- ============================================================

-- | Convert Entity DdexDocument to DdexDocumentDTO
documentEntityToDTO :: Entity M.DdexDocument -> DdexDocumentDTO
documentEntityToDTO docEntity =
  let doc = entityVal docEntity
      docId = entityKey docEntity
  in DdexDocumentDTO
    { ddexDocumentId = fromIntegral $ fromSqlKey docId
    , ddexDocumentFileName = M.ddexDocumentFileName doc
    , ddexDocumentSha256 = M.ddexDocumentSha256 doc
    , ddexDocumentFamily = DDEXTypes.familyToText . DB.fromFamilyEnum $ M.ddexDocumentFamily doc
    , ddexDocumentVersion = M.ddexDocumentVersion doc
    , ddexDocumentStatus = DDEXTypes.documentStatusToText . DB.fromStatusEnum $ M.ddexDocumentStatus doc
    , ddexDocumentMessageId = M.ddexDocumentMessageId doc
    , ddexDocumentSenderId = M.ddexDocumentSenderId doc
    , ddexDocumentRecipientId = M.ddexDocumentRecipientId doc
    , ddexDocumentCreatedAt = M.ddexDocumentCreatedAt doc
    }

-- | Convert DdexValidationIssue to ValidationIssueDTO
issueToDTO :: M.DdexValidationIssue -> ValidationIssueDTO
issueToDTO issue = ValidationIssueDTO
  { issueSeverity = case M.ddexValidationIssueSeverity issue of
      M.SeverityError -> "error"
      M.SeverityWarning -> "warning"
      M.SeverityInfo -> "info"
  , issueLayer = case M.ddexValidationIssueLayer issue of
      M.LayerXML -> "xml"
      M.LayerXSD -> "xsd"
      M.LayerAVS -> "allowed-value-set"
      M.LayerBusiness -> "business"
  , issueCode = maybe "" id (M.ddexValidationIssueCode issue)
  , issueMessage = M.ddexValidationIssueMessage issue
  , issueLine = M.ddexValidationIssueLineNumber issue
  , issueColumn = M.ddexValidationIssueColumnNumber issue
  }

-- | Convert Entity DdexPartner to DdexPartnerDTO
partnerEntityToDTO :: Entity M.DdexPartner -> DdexPartnerDTO
partnerEntityToDTO partnerEntity =
  let partner = entityVal partnerEntity
      partnerId = entityKey partnerEntity
  in DdexPartnerDTO
    { ddexPartnerId = fromIntegral $ fromSqlKey partnerId
    , ddexPartnerName = M.ddexPartnerName partner
    , ddexPartnerDpid = M.ddexPartnerDpid partner
    , ddexPartnerAllowedVersions = M.ddexPartnerAllowedVersions partner
    }

-- | Convert DdexPartner with key to DdexPartnerDTO
partnerToDTO :: M.DdexPartnerId -> M.DdexPartner -> DdexPartnerDTO
partnerToDTO partnerId partner = DdexPartnerDTO
  { ddexPartnerId = fromIntegral $ fromSqlKey partnerId
  , ddexPartnerName = M.ddexPartnerName partner
  , ddexPartnerDpid = M.ddexPartnerDpid partner
  , ddexPartnerAllowedVersions = M.ddexPartnerAllowedVersions partner
  }
