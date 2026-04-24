{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TDF.ServerExtra where

import           Control.Monad              (filterM, unless, when, join, guard)
import           Control.Applicative        ((<|>))
import           Control.Exception          (SomeException, try)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask, asks)
import           Data.Foldable              (for_)
import           Data.List                  (sortOn)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Set                   as Set
import           Data.Bits                  (xor)
import           Data.Char                  (isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isControl, isDigit, isSpace, ord)
import           Data.Word                  (Word64)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Time                  (Day, UTCTime(..), defaultTimeLocale, getCurrentTime, parseTimeM)
import           Data.UUID                  (toText)
import qualified Data.UUID                  as UUID
import           Data.UUID.V4               (nextRandom)
import           Data.Aeson                 (object, (.:), (.:?), (.=))
import           Data.Aeson.Types           (Parser, parseMaybe, withObject, (.!=))
import qualified Data.Aeson                as A
import           Data.Int                   (Int64)
import qualified Data.Scientific            as Sci
import           Numeric                    (showHex)
import           System.Directory           (copyFile, createDirectoryIfMissing)
import           System.FilePath            ((</>), takeExtension, takeFileName)
import           System.IO                  (hPutStrLn, stderr)
import           Database.Persist        hiding (Active)
import           Database.Persist.Sql       (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Network.HTTP.Client        (Manager, Request(..), Response, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Header  (hAuthorization)
import           Network.HTTP.Types.Status  (statusCode)
import           Network.HTTP.Types.URI     (urlEncode)
import           Servant
import           Servant.Multipart          (FileData(..))
import           Web.PathPieces             (PathPiece, fromPathPiece, toPathPiece)

import           TDF.API.Inventory          (InventoryAPI, InventoryPublicAPI, AssetUploadForm(..))
import           TDF.API.Bands              (BandsAPI)
import           TDF.API.Pipelines          (PipelinesAPI)
import           TDF.API.Rooms              (RoomsAPI, RoomsPublicAPI)
import           TDF.API.Sessions           (SessionsAPI)
import           TDF.API.Services           (ServiceCatalogAPI, ServiceCatalogPublicAPI)
import           TDF.API.Types
import           TDF.Auth                   (AuthedUser(..), ModuleAccess(..), hasModuleAccess, hasOperationsAccess, hasSocialInboxAccess)
import           TDF.API.Payments          (PaymentDTO(..), PaymentCreate(..), PaymentsAPI)
import qualified TDF.API.Facebook          as FB
import qualified TDF.API.Instagram         as IG
import           TDF.DB                     (Env(..))
import           TDF.Config                 (AppConfig, assetsRootDir, facebookMessagingApiBase, facebookMessagingToken, instagramAppToken, instagramMessagingApiBase, instagramMessagingToken, instagramVerifyToken, resolveConfiguredAppBase, resolveConfiguredAssetsBase)
import           TDF.Services.InstagramMessaging (sendInstagramTextWithContext)
import           TDF.Services.FacebookMessaging (sendFacebookText)
import           TDF.Models                 (Party(..), Payment(..), PaymentMethod(..))
import qualified TDF.Models                 as M
import           TDF.ModelsExtra
import qualified TDF.ModelsExtra as ME
import           TDF.Pipelines              (canonicalStage, defaultStage, pipelineStages, pipelineTypeSlug, parsePipelineType)
import qualified TDF.Trials.Server          as TrialsServer (isValidHttpUrl)
import qualified TDF.Handlers.InputList     as InputList
import           TDF.WhatsApp.History       (normalizeWhatsAppPhone)

-- Helpers for simple date parsing (YYYY-MM-DD)
parseDayText :: MonadError ServerError m => Text -> m Day
parseDayText rawText =
  let t = T.strip rawText
  in unlessIsoDateShape t $
  case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
    Just d  -> pure d
    Nothing -> throwError err400 { errBody = "Invalid date format, expected YYYY-MM-DD" }

parseUTCTimeText :: MonadError ServerError m => Text -> m UTCTime
parseUTCTimeText rawText =
  let t = T.strip rawText
  in unlessIsoDateShape t $
  case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
    Just d  -> pure (UTCTime d 0)
    Nothing -> throwError err400 { errBody = "Invalid date format, expected YYYY-MM-DD" }

unlessIsoDateShape :: MonadError ServerError m => Text -> m a -> m a
unlessIsoDateShape t action
  | hasIsoDateShape t = action
  | otherwise = throwError err400 { errBody = "Invalid date format, expected YYYY-MM-DD" }

hasIsoDateShape :: Text -> Bool
hasIsoDateShape t =
  case T.splitOn "-" t of
    [yearPart, monthPart, dayPart] ->
      T.length yearPart == 4
        && T.length monthPart == 2
        && T.length dayPart == 2
        && T.all isDigit (yearPart <> monthPart <> dayPart)
    _ -> False

parseCheckoutTargetKind :: Maybe Text -> Either ServerError CheckoutTarget
parseCheckoutTargetKind Nothing =
  Left err400 { errBody = "targetKind is required and must be one of: party, room, session" }
parseCheckoutTargetKind (Just raw) =
  case T.toLower (T.strip raw) of
    "session" -> Right TargetSession
    "party" -> Right TargetParty
    "room" -> Right TargetRoom
    _ -> Left err400 { errBody = "targetKind must be one of: party, room, session" }

parseCheckoutDisposition :: Maybe Text -> Either ServerError CheckoutDisposition
parseCheckoutDisposition Nothing = Right Loan
parseCheckoutDisposition (Just raw) =
  case T.toLower (T.strip raw) of
    "" -> Left err400 { errBody = "disposition must be one of: loan, rental, sale, repair, transfer, other" }
    "loan" -> Right Loan
    "lend" -> Right Loan
    "rental" -> Right Rental
    "rent" -> Right Rental
    "sale" -> Right Sale
    "sell" -> Right Sale
    "repair" -> Right Repair
    "maintenance" -> Right Repair
    "transfer" -> Right Transfer
    "other" -> Right OtherDisposition
    _ -> Left err400 { errBody = "disposition must be one of: loan, rental, sale, repair, transfer, other" }

validateCheckoutTargets
  :: CheckoutTarget
  -> Maybe Text
  -> Maybe (Key Room)
  -> Maybe (Key ME.Session)
  -> Either ServerError (Maybe Text, Maybe (Key Room), Maybe (Key ME.Session))
validateCheckoutTargets targetKind mTargetParty mRoom mSession =
  case targetKind of
    TargetRoom ->
      case (targetPartyPresent, mRoom, mSession) of
        (Just _, _, _) -> Left err400 { errBody = "targetParty is only allowed for party checkout" }
        (_, Nothing, _) -> Left err400 { errBody = "targetRoom required for room checkout" }
        (_, Just _, Just _) -> Left err400 { errBody = "targetSession is only allowed for session checkout" }
        (Nothing, Just roomKey, Nothing) -> Right (Nothing, Just roomKey, Nothing)
    TargetSession ->
      case (targetPartyPresent, mRoom, mSession) of
        (Just _, _, _) -> Left err400 { errBody = "targetParty is only allowed for party checkout" }
        (_, _, Nothing) -> Left err400 { errBody = "targetSession required for session checkout" }
        (_, Just _, Just _) -> Left err400 { errBody = "targetRoom is only allowed for room checkout" }
        (Nothing, Nothing, Just sessionKey) -> Right (Nothing, Nothing, Just sessionKey)
    TargetParty ->
      case (targetPartyPresent, mRoom, mSession) of
        (Nothing, _, _) -> Left err400 { errBody = "targetParty required for party checkout" }
        (_, Just _, _) -> Left err400 { errBody = "targetRoom is only allowed for room checkout" }
        (_, _, Just _) -> Left err400 { errBody = "targetSession is only allowed for session checkout" }
        (Just _, Nothing, Nothing) -> do
          targetParty <- validateCheckoutTargetParty mTargetParty
          Right (targetParty, Nothing, Nothing)
  where
    targetPartyPresent = normalizeOptionalTextField mTargetParty

validateCheckoutTargetParty :: Maybe Text -> Either ServerError (Maybe Text)
validateCheckoutTargetParty mTargetParty =
  case normalizeOptionalTextField mTargetParty of
    Nothing -> Right Nothing
    Just targetParty
      | T.length targetParty > 160 ->
          Left err400 { errBody = "targetParty must be 160 characters or fewer" }
      | T.any isControl targetParty ->
          Left err400 { errBody = "targetParty must not contain control characters" }
      | otherwise ->
          Right (Just targetParty)

inventoryServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT InventoryAPI m
inventoryServer user =
       listAssets
  :<|> createAssetH
  :<|> uploadAssetPhotoH
  :<|> getAssetH
  :<|> patchAssetH
  :<|> deleteAssetH
  :<|> checkoutAssetH
  :<|> checkinAssetH
  :<|> checkoutHistoryH
  :<|> refreshQrH
  :<|> resolveByQrH
  where
    ensureInventoryAccess =
      unless (hasOperationsAccess user) $
        throwError err403 { errBody = "Missing required module access" }

    listAssets mq mp mps = do
      ensureInventoryAccess
      (pageNum, pageSize') <- either throwError pure (validatePageParams mp mps)
      let
          pageOffset = (pageNum - 1) * pageSize'
      entities <- withPool $ selectList ([] :: [Filter Asset]) [Asc AssetName]
      let filteredEntities = filterAssetsByQuery mq entities
          totalCount = length filteredEntities
          pagedEntities = take pageSize' (drop pageOffset filteredEntities)
      activeMap <- withPool $ loadActiveCheckoutMap (map entityKey pagedEntities)
      pure (mkPage pageNum pageSize' totalCount (map (\entity -> toAssetDTO entity (Map.lookup (entityKey entity) activeMap)) pagedEntities))

    createAssetH req = do
      ensureInventoryAccess
      nameClean <- either throwError pure (normalizeAssetName (cName req))
      categoryClean <- either throwError pure (normalizeAssetCategory (cCategory req))
      photoUrlValue <- either throwError pure (validateAssetPhotoUrl (cPhotoUrl req))
      entity <- withPool $ do
        newAssetId <- insert Asset
          { assetName                  = nameClean
          , assetCategory              = categoryClean
          , assetBrand                 = Nothing
          , assetModel                 = Nothing
          , assetSerialNumber          = Nothing
          , assetPurchaseDate          = Nothing
          , assetPurchasePriceUsdCents = Nothing
          , assetCondition             = Good
          , assetStatus                = Active
          , assetLocationId            = Nothing
          , assetOwner                 = "TDF"
          , assetQrCode                = Nothing
          , assetPhotoUrl              = photoUrlValue
          , assetNotes                 = Nothing
          , assetWarrantyExpires       = Nothing
          , assetMaintenancePolicy     = None
          , assetNextMaintenanceDue    = Nothing
          }
        getJustEntity newAssetId
      pure (toAssetDTO entity Nothing)

    uploadAssetPhotoH uploadForm = do
      ensureInventoryAccess
      storeAssetUpload uploadForm

    getAssetH rawId = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      loadAssetDTOByKey assetKey

    patchAssetH rawId req = do
      ensureInventoryAccess
      assetKey    <- parseKey @Asset rawId
      locationKey <- traverse (parseKey @Room) (uLocationId req)
      nameUpdate <- either throwError pure (normalizeAssetNameUpdate (uName req))
      categoryUpdate <- either throwError pure (normalizeAssetCategoryUpdate (uCategory req))
      statusValue <- either throwError pure (validateAssetStatusUpdate (uStatus req))
      notesUpdate <- either throwError pure (normalizeAssetNotesUpdate (uNotes req))
      photoUrlUpdate <- either throwError pure (validateAssetPhotoUrlUpdate (uPhotoUrl req))
      let updates = catMaybes
            [ (AssetName =.) <$> nameUpdate
            , (AssetCategory =.) <$> categoryUpdate
            , (AssetStatus =.) <$> statusValue
            , fmap (\rid -> AssetLocationId =. Just rid) locationKey
            , (AssetNotes =.) <$> notesUpdate
            , (AssetPhotoUrl =.) <$> photoUrlUpdate
            ]
      result <- withPool $ do
        mEntity <- getEntity assetKey
        case mEntity of
          Nothing -> pure (Right Nothing)
          Just entity -> do
            mActiveCheckout <- selectFirst
              [ AssetCheckoutAssetId ==. assetKey
              , AssetCheckoutReturnedAt ==. Nothing
              ]
              [Desc AssetCheckoutCheckedOutAt]
            let activeCheckoutStatus =
                  assetStatusForCheckoutDisposition . assetCheckoutDisposition . entityVal <$> mActiveCheckout
            case validateAssetPatchStatusInvariant
              (assetStatus (entityVal entity))
              statusValue
              activeCheckoutStatus of
              Left err ->
                pure (Left err)
              Right () -> do
                unless (null updates) (update assetKey updates)
                Right <$> getEntity assetKey
      case result of
        Left err -> throwError err
        Right Nothing -> throwError err404
        Right (Just entity) -> do
          activeMap <- withPool $ loadActiveCheckoutMap [entityKey entity]
          pure (toAssetDTO entity (Map.lookup (entityKey entity) activeMap))

    deleteAssetH rawId = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      deleteResult <- withPool $ do
        mAsset <- get assetKey
        case mAsset of
          Nothing ->
            pure (Left err404 { errBody = "Asset not found" })
          Just _ -> do
            mCheckout <- selectFirst [AssetCheckoutAssetId ==. assetKey] [LimitTo 1]
            case mCheckout of
              Just _ ->
                pure
                  (Left err409 { errBody = "Asset has checkout history and cannot be deleted" })
              Nothing -> do
                delete assetKey
                pure (Right ())
      either throwError pure deleteResult
      pure NoContent

    checkoutAssetH rawId req = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      assetEntity <- loadAssetEntityByKey assetKey
      let checkedOutBy = T.pack (show (fromSqlKey (auPartyId user)))
      performCheckout checkedOutBy assetEntity req

    checkinAssetH rawId req = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      assetEntity <- loadAssetEntityByKey assetKey
      performCheckin assetEntity req

    checkoutHistoryH rawId = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      recs <- withPool $ selectList [AssetCheckoutAssetId ==. assetKey] [Desc AssetCheckoutCheckedOutAt, LimitTo 50]
      pure (map toCheckoutDTO recs)

    refreshQrH rawId = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      _ <- loadAssetEntityByKey assetKey
      token <- liftIO (fmap (T.pack . show) nextRandom)
      Env{envConfig} <- ask
      let qrBase = resolveConfiguredAppBase envConfig <> "/inventario/scan/"
          qrUrl tokenVal = qrBase <> tokenVal
      withPool $ update assetKey [AssetQrCode =. Just token]
      pure AssetQrDTO { qrToken = token, qrUrl = qrUrl token }

    resolveByQrH token = do
      ensureInventoryAccess
      loadAssetDTOByQrToken token

inventoryPublicServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => ServerT InventoryPublicAPI m
inventoryPublicServer =
       loadAssetDTOByQrToken
  :<|> checkoutByQrToken
  :<|> checkinByQrToken
  :<|> uploadByQrToken
  where
    checkoutByQrToken token req = do
      assetEntity <- loadAssetEntityByQrToken token
      normalized <- either throwError pure (normalizeCheckoutRequest req)
      either throwError pure (validatePublicQrCheckoutRequest normalized)
      performCheckout "public-link" assetEntity req

    checkinByQrToken token req = do
      assetEntity <- loadAssetEntityByQrToken token
      normalized <- either throwError pure (normalizeAssetCheckinFields req)
      either throwError pure (validatePublicQrCheckinFields normalized)
      performCheckinWith ensurePublicQrCheckinAllowed assetEntity req

    uploadByQrToken token uploadForm = do
      _ <- loadAssetEntityByQrToken token
      storeAssetUpload uploadForm

loadAssetEntityByKey
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => Key Asset
  -> m (Entity Asset)
loadAssetEntityByKey assetKey = do
  mEntity <- withPool $ getEntity assetKey
  maybe (throwError err404 { errBody = "Asset not found" }) pure mEntity

loadAssetEntityByQrToken
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => Text
  -> m (Entity Asset)
loadAssetEntityByQrToken token = do
  tokenCanonical <- either throwError pure (validateAssetQrToken token)
  mEntity <- withPool $ selectFirst [AssetQrCode ==. Just tokenCanonical] []
  maybe (throwError err404 { errBody = "Asset not found" }) pure mEntity

loadAssetDTOByKey
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => Key Asset
  -> m AssetDTO
loadAssetDTOByKey assetKey = do
  assetEntity <- loadAssetEntityByKey assetKey
  activeMap <- withPool $ loadActiveCheckoutMap [assetKey]
  pure (toAssetDTO assetEntity (Map.lookup assetKey activeMap))

loadAssetDTOByQrToken
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => Text
  -> m AssetDTO
loadAssetDTOByQrToken token = do
  assetEntity@(Entity assetKey _) <- loadAssetEntityByQrToken token
  activeMap <- withPool $ loadActiveCheckoutMap [assetKey]
  pure (toAssetDTO assetEntity (Map.lookup assetKey activeMap))

loadActiveCheckoutMap
  :: MonadIO m
  => [Key Asset]
  -> SqlPersistT m (Map.Map (Key Asset) (Entity AssetCheckout))
loadActiveCheckoutMap [] = pure Map.empty
loadActiveCheckoutMap assetKeys = do
  recs <- selectList
    [ AssetCheckoutAssetId <-. assetKeys
    , AssetCheckoutReturnedAt ==. Nothing
    ]
    [Desc AssetCheckoutCheckedOutAt]
  pure (Map.fromListWith (\left _ -> left) [(assetCheckoutAssetId rec, ent) | ent@(Entity _ rec) <- recs])

performCheckout
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => Text
  -> Entity Asset
  -> AssetCheckoutRequest
  -> m AssetCheckoutDTO
performCheckout checkedOutBy (Entity assetKey assetRecord) req = do
  now <- liftIO getCurrentTime
  normalized <- either throwError pure (normalizeCheckoutRequest req)
  dueAtValue <- either throwError pure (validateCheckoutDueAt now (ncrDueAt normalized))
  active <- withPool $ selectFirst [AssetCheckoutAssetId ==. assetKey, AssetCheckoutReturnedAt ==. Nothing] [Desc AssetCheckoutCheckedOutAt]
  when (isJust active) $
    throwError err409 { errBody = "Asset already checked out" }
  either throwError pure (validateAssetCheckoutStatus (assetStatus assetRecord))
  either throwError pure =<< withPool (validateCheckoutTargetReferences (ncrTargetRoom normalized) (ncrTargetSession normalized))
  recEnt <- withPool $ do
    checkoutId <- insert AssetCheckout
      { assetCheckoutAssetId          = assetKey
      , assetCheckoutTargetKind       = ncrTargetKind normalized
      , assetCheckoutTargetSessionId  = ncrTargetSession normalized
      , assetCheckoutTargetPartyRef   = ncrTargetParty normalized
      , assetCheckoutTargetRoomId     = ncrTargetRoom normalized
      , assetCheckoutDisposition      = ncrDisposition normalized
      , assetCheckoutTermsAndConditions = ncrTermsAndConditions normalized
      , assetCheckoutHolderEmail      = ncrHolderEmail normalized
      , assetCheckoutHolderPhone      = ncrHolderPhone normalized
      , assetCheckoutPaymentType      = ncrPaymentType normalized
      , assetCheckoutPaymentInstallments = ncrPaymentInstallments normalized
      , assetCheckoutPaymentReference = ncrPaymentReference normalized
      , assetCheckoutPaymentAmountCents = ncrPaymentAmountCents normalized
      , assetCheckoutPaymentCurrency  = ncrPaymentCurrency normalized
      , assetCheckoutPaymentOutstandingCents = ncrPaymentOutstandingCents normalized
      , assetCheckoutCheckedOutByRef  = checkedOutBy
      , assetCheckoutCheckedOutAt     = now
      , assetCheckoutDueAt            = dueAtValue
      , assetCheckoutConditionOut     = ncrConditionOut normalized
      , assetCheckoutPhotoOutUrl      = ncrPhotoOutUrl normalized
      , assetCheckoutPhotoDriveFileId = Nothing
      , assetCheckoutReturnedAt       = Nothing
      , assetCheckoutConditionIn      = Nothing
      , assetCheckoutPhotoInUrl       = Nothing
      , assetCheckoutNotes            = ncrNotes normalized
      }
    update assetKey [AssetStatus =. assetStatusForCheckoutDisposition (ncrDisposition normalized)]
    getJustEntity checkoutId
  pure (toCheckoutDTO recEnt)

performCheckin
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => Entity Asset
  -> AssetCheckinRequest
  -> m AssetCheckoutDTO
performCheckin =
  performCheckinWith (\_ -> pure ())

performCheckinWith
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => (Entity AssetCheckout -> m ())
  -> Entity Asset
  -> AssetCheckinRequest
  -> m AssetCheckoutDTO
performCheckinWith validateOpenCheckout (Entity assetKey _) req = do
  now <- liftIO getCurrentTime
  (conditionInUpdate, checkinNotesUpdate, photoInUpdate) <- either throwError pure (normalizeAssetCheckinFields req)
  mOpen <- withPool $ selectFirst [AssetCheckoutAssetId ==. assetKey, AssetCheckoutReturnedAt ==. Nothing] [Desc AssetCheckoutCheckedOutAt]
  case mOpen of
    Nothing -> throwError err404 { errBody = "No active checkout" }
    Just checkoutEnt@(Entity checkoutId checkoutRecord) -> do
      validateOpenCheckout checkoutEnt
      recEnt <- withPool $ do
        let updates = catMaybes
              [ Just (AssetCheckoutReturnedAt =. Just now)
              , fmap (\conditionText -> AssetCheckoutConditionIn =. Just conditionText) conditionInUpdate
              , fmap (\notesText -> AssetCheckoutNotes =. Just notesText) checkinNotesUpdate
              , fmap (\photoUrl -> AssetCheckoutPhotoInUrl =. Just photoUrl) photoInUpdate
              ]
        update checkoutId updates
        update assetKey [AssetStatus =. assetStatusForCheckinDisposition (assetCheckoutDisposition checkoutRecord)]
        getJustEntity checkoutId
      pure (toCheckoutDTO recEnt)

ensurePublicQrCheckinAllowed
  :: MonadError ServerError m
  => Entity AssetCheckout
  -> m ()
ensurePublicQrCheckinAllowed (Entity _ checkoutRecord)
  | assetCheckoutTargetKind checkoutRecord == TargetParty =
      pure ()
  | otherwise =
      throwError err409 { errBody = "Public QR check-in only supports party checkouts" }

data NormalizedCheckoutRequest = NormalizedCheckoutRequest
  { ncrTargetKind :: CheckoutTarget
  , ncrTargetParty :: Maybe Text
  , ncrTargetRoom :: Maybe (Key Room)
  , ncrTargetSession :: Maybe (Key ME.Session)
  , ncrDisposition :: CheckoutDisposition
  , ncrTermsAndConditions :: Maybe Text
  , ncrHolderEmail :: Maybe Text
  , ncrHolderPhone :: Maybe Text
  , ncrPaymentType :: Maybe Text
  , ncrPaymentInstallments :: Maybe Int
  , ncrPaymentReference :: Maybe Text
  , ncrPaymentAmountCents :: Maybe Int
  , ncrPaymentCurrency :: Maybe Text
  , ncrPaymentOutstandingCents :: Maybe Int
  , ncrDueAt :: Maybe UTCTime
  , ncrConditionOut :: Maybe Text
  , ncrPhotoOutUrl :: Maybe Text
  , ncrNotes :: Maybe Text
  }

normalizeCheckoutRequest :: AssetCheckoutRequest -> Either ServerError NormalizedCheckoutRequest
normalizeCheckoutRequest req = do
  targetKind <- parseCheckoutTargetKind (coTargetKind req)
  targetRoomKey <- parseOptionalKeyField @Room "targetRoom" (coTargetRoom req)
  targetSessionKey <- parseOptionalKeyField @ME.Session "targetSession" (coTargetSession req)
  (mTargetParty, mRoom, mSession) <-
    validateCheckoutTargets targetKind (coTargetParty req) targetRoomKey targetSessionKey
  disposition <- parseCheckoutDisposition (coDisposition req)
  termsAndConditions <- validateCheckoutTermsField "termsAndConditions" (coTermsAndConditions req)
  holderEmail <- validateCheckoutHolderEmail (coHolderEmail req)
  holderPhone <- validateCheckoutHolderPhone (coHolderPhone req)
  paymentType <- normalizeCheckoutPaymentType (coPaymentType req)
  paymentInstallments <- validateCheckoutInstallments (coPaymentInstallments req)
  paymentReference <- validateOptionalPaymentTextField "paymentReference" 160 (coPaymentReference req)
  paymentAmountCents <- validateCheckoutAmountField "paymentAmount" (coPaymentAmount req)
  paymentCurrency <- validateCheckoutCurrency (coPaymentCurrency req)
  paymentOutstandingCents <- validateCheckoutMoneyField "paymentOutstanding" True (coPaymentOutstanding req)
  validateCheckoutFinancials
    disposition
    paymentType
    paymentInstallments
    paymentReference
    paymentAmountCents
    paymentCurrency
    paymentOutstandingCents
  validateCheckoutDispositionFields disposition (coDueAt req)
  conditionOut <- validateInventoryConditionField "conditionOut" (coConditionOut req)
  checkoutNotes <- validateInventoryNotesField "notes" (coNotes req)
  photoOutUrl <- validateAssetPhotoUrl (coPhotoUrl req)
  pure NormalizedCheckoutRequest
    { ncrTargetKind = targetKind
    , ncrTargetParty = mTargetParty
    , ncrTargetRoom = mRoom
    , ncrTargetSession = mSession
    , ncrDisposition = disposition
    , ncrTermsAndConditions = termsAndConditions
    , ncrHolderEmail = holderEmail
    , ncrHolderPhone = holderPhone
    , ncrPaymentType = paymentType
    , ncrPaymentInstallments = paymentInstallments
    , ncrPaymentReference = paymentReference
    , ncrPaymentAmountCents = paymentAmountCents
    , ncrPaymentCurrency = paymentCurrency
    , ncrPaymentOutstandingCents = paymentOutstandingCents
    , ncrDueAt = coDueAt req
    , ncrConditionOut = conditionOut
    , ncrPhotoOutUrl = photoOutUrl
    , ncrNotes = checkoutNotes
    }

validatePublicQrCheckoutRequest :: NormalizedCheckoutRequest -> Either ServerError ()
validatePublicQrCheckoutRequest normalized
  | ncrTargetKind normalized /= TargetParty =
      Left err400 { errBody = "Public QR checkout only supports party targets" }
  | ncrDisposition normalized == Sale =
      Left err400 { errBody = "Public QR checkout does not allow sale disposition" }
  | isNothing (ncrHolderEmail normalized) && isNothing (ncrHolderPhone normalized) =
      Left err400 { errBody = "Public QR checkout requires holderEmail or holderPhone" }
  | isNothing (ncrPhotoOutUrl normalized) =
      Left err400 { errBody = "Public QR checkout requires coPhotoUrl" }
  | otherwise =
      Right ()

validatePublicQrCheckinFields :: (Maybe Text, Maybe Text, Maybe Text) -> Either ServerError ()
validatePublicQrCheckinFields (mConditionIn, _, _)
  | isNothing mConditionIn =
      Left err400 { errBody = "Public QR check-in requires ciConditionIn" }
  | otherwise =
      Right ()

validateCheckoutHolderEmail :: Maybe Text -> Either ServerError (Maybe Text)
validateCheckoutHolderEmail rawValue =
  case normalizeOptionalTextField rawValue of
    Nothing -> Right Nothing
    Just cleanValue
      | T.length cleanValue > maxCheckoutHolderEmailChars ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 ("holderEmail must be " <> T.pack (show maxCheckoutHolderEmailChars) <> " characters or fewer"))
            }
      | T.any isControl cleanValue ->
          Left err400 { errBody = "holderEmail must not contain control characters" }
      | isValidCheckoutHolderEmail normalized ->
          Right (Just normalized)
      | otherwise ->
          Left err400 { errBody = "holderEmail must be a valid email address" }
      where
        normalized = T.toLower cleanValue

validateCheckoutHolderPhone :: Maybe Text -> Either ServerError (Maybe Text)
validateCheckoutHolderPhone rawValue =
  case normalizeOptionalTextField rawValue of
    Nothing -> Right Nothing
    Just cleanValue
      | T.length cleanValue > maxCheckoutHolderPhoneChars ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 ("holderPhone must be " <> T.pack (show maxCheckoutHolderPhoneChars) <> " characters or fewer"))
            }
      | T.any isControl cleanValue ->
          Left err400 { errBody = "holderPhone must not contain control characters" }
      | otherwise ->
          maybe
            (Left err400 { errBody = "holderPhone must be a valid phone number" })
            (Right . Just)
            (normalizeWhatsAppPhone cleanValue)

isValidCheckoutHolderEmail :: Text -> Bool
isValidCheckoutHolderEmail candidate =
  case T.split (== '@') candidate of
    [localPart, domain] ->
      T.length candidate <= maxCheckoutHolderEmailChars
        && isValidCheckoutHolderEmailLocalPart localPart
        && not (T.null domain)
        && not (T.any (`elem` [' ', '\t', '\n', '\r']) candidate)
        && T.isInfixOf "." domain
        && all isValidCheckoutHolderDomainLabel (T.splitOn "." domain)
    _ -> False

isValidCheckoutHolderEmailLocalPart :: Text -> Bool
isValidCheckoutHolderEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= maxCheckoutHolderEmailLocalPartChars
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (".." `T.isInfixOf` localPart)
    && T.all isValidCheckoutHolderEmailLocalChar localPart

isValidCheckoutHolderEmailLocalChar :: Char -> Bool
isValidCheckoutHolderEmailLocalChar c =
  isAsciiLower c || isDigit c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidCheckoutHolderDomainLabel :: Text -> Bool
isValidCheckoutHolderDomainLabel label =
  not (T.null label)
    && T.length label <= maxCheckoutHolderEmailDomainLabelChars
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidCheckoutHolderDomainChar label

isValidCheckoutHolderDomainChar :: Char -> Bool
isValidCheckoutHolderDomainChar c = isAsciiLower c || isDigit c || c == '-'

maxCheckoutHolderEmailChars :: Int
maxCheckoutHolderEmailChars = 160

maxCheckoutHolderEmailLocalPartChars :: Int
maxCheckoutHolderEmailLocalPartChars = 64

maxCheckoutHolderEmailDomainLabelChars :: Int
maxCheckoutHolderEmailDomainLabelChars = 63

maxCheckoutHolderPhoneChars :: Int
maxCheckoutHolderPhoneChars = 60

validateCheckoutTermsField :: Text -> Maybe Text -> Either ServerError (Maybe Text)
validateCheckoutTermsField fieldName =
  validateInventoryOptionalTextField fieldName inventoryTermsMaxLength

normalizeCheckoutPaymentType :: Maybe Text -> Either ServerError (Maybe Text)
normalizeCheckoutPaymentType Nothing = Right Nothing
normalizeCheckoutPaymentType (Just rawValue) =
  case normalizeOptionalTextField (Just rawValue) of
    Nothing -> Right Nothing
    Just cleanValue ->
      fmap (Just . paymentMethodToInventoryPaymentType) (validatePaymentMethod cleanValue)

validateCheckoutInstallments :: Maybe Int -> Either ServerError (Maybe Int)
validateCheckoutInstallments Nothing = Right Nothing
validateCheckoutInstallments (Just installments)
  | installments < 1 =
      Left err400 { errBody = "paymentInstallments must be at least 1" }
  | installments > 60 =
      Left err400 { errBody = "paymentInstallments must be 60 or fewer" }
  | otherwise =
      Right (Just installments)

validateCheckoutAmountField :: Text -> Maybe Text -> Either ServerError (Maybe Int)
validateCheckoutAmountField fieldName rawValue = do
  amountCents <- validateCheckoutMoneyField fieldName False rawValue
  case amountCents of
    Just 0 ->
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 (fieldName <> " must be greater than 0"))
        }
    _ ->
      Right amountCents

validateCheckoutCurrency :: Maybe Text -> Either ServerError (Maybe Text)
validateCheckoutCurrency Nothing = Right Nothing
validateCheckoutCurrency (Just rawCurrency) =
  case normalizeOptionalTextField (Just rawCurrency) of
    Nothing -> Right Nothing
    Just currency ->
      let normalized = T.toUpper currency
      in if T.length normalized == 3 && T.all isAsciiUpper normalized
           then Right (Just normalized)
           else Left err400 { errBody = "paymentCurrency must be a 3-letter ISO code" }

validateCheckoutMoneyField :: Text -> Bool -> Maybe Text -> Either ServerError (Maybe Int)
validateCheckoutMoneyField fieldName allowZero rawValue =
  case normalizeOptionalTextField rawValue of
    Nothing -> Right Nothing
    Just cleanValue ->
      case parseMoneyToCents cleanValue of
        Nothing ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 (fieldName <> " must be a non-negative amount with up to 2 decimals"))
            }
        Just cents
          | not allowZero && cents <= 0 ->
              Left err400
                { errBody =
                    BL.fromStrict
                      (TE.encodeUtf8 (fieldName <> " must be greater than 0"))
                }
          | otherwise ->
              Right (Just cents)

parseMoneyToCents :: Text -> Maybe Int
parseMoneyToCents rawValue
  | T.any isSpace rawValue = Nothing
  | T.any isControl rawValue = Nothing
  | T.isPrefixOf "-" rawValue = Nothing
  | otherwise =
      case T.splitOn "." rawValue of
        [wholePart]
          | validWholePart wholePart ->
              buildCents wholePart ""
        [wholePart, fractionalPart]
          | validWholePart wholePart
              && not (T.null fractionalPart)
              && T.length fractionalPart <= 2
              && T.all isDigit fractionalPart ->
                  buildCents wholePart fractionalPart
        _ ->
          Nothing
  where
    validWholePart part = not (T.null part) && T.all isDigit part
    buildCents wholePart fractionalPart = do
      wholeUnits <- parseInteger wholePart
      fractionalUnits <- parseInteger (fractionalPart <> T.replicate (2 - T.length fractionalPart) "0")
      let totalCents = wholeUnits * 100 + fractionalUnits
      if totalCents <= fromIntegral (maxBound :: Int)
        then Just (fromIntegral totalCents)
        else Nothing
    parseInteger txt =
      case reads (T.unpack txt) :: [(Integer, String)] of
        [(value, "")] -> Just value
        _             -> Nothing

validateCheckoutFinancials
  :: CheckoutDisposition
  -> Maybe Text
  -> Maybe Int
  -> Maybe Text
  -> Maybe Int
  -> Maybe Text
  -> Maybe Int
  -> Either ServerError ()
validateCheckoutFinancials disposition paymentType paymentInstallments paymentReference paymentAmount paymentCurrency paymentOutstanding
  | not (checkoutDispositionSupportsPaymentDetails disposition)
      && any isJust
        [ fmap (const ()) paymentType
        , fmap (const ()) paymentInstallments
        , fmap (const ()) paymentReference
        , fmap (const ()) paymentAmount
        , fmap (const ()) paymentCurrency
        , fmap (const ()) paymentOutstanding
        ] =
      Left err400 { errBody = "payment fields are only allowed for sale or rental checkout" }
validateCheckoutFinancials _ Nothing (Just _) _ _ _ _ =
  Left err400 { errBody = "paymentInstallments requires paymentType" }
validateCheckoutFinancials _ Nothing _ (Just _) _ _ _ =
  Left err400 { errBody = "paymentReference requires paymentType" }
validateCheckoutFinancials _ Nothing _ _ (Just _) _ _ =
  Left err400 { errBody = "paymentAmount requires paymentType" }
validateCheckoutFinancials _ Nothing _ _ _ (Just _) _ =
  Left err400 { errBody = "paymentCurrency requires paymentType" }
validateCheckoutFinancials _ Nothing _ _ _ _ (Just _) =
  Left err400 { errBody = "paymentOutstanding requires paymentType" }
validateCheckoutFinancials _ _ _ _ Nothing (Just _) _ =
  Left err400 { errBody = "paymentCurrency requires paymentAmount" }
validateCheckoutFinancials _ _ _ _ (Just _) Nothing _ =
  Left err400 { errBody = "paymentAmount requires paymentCurrency" }
validateCheckoutFinancials _ _ _ _ Nothing _ (Just _) =
  Left err400 { errBody = "paymentOutstanding requires paymentAmount" }
validateCheckoutFinancials _ _ _ _ (Just amountCents) _ (Just outstandingCents)
  | outstandingCents > amountCents =
      Left err400 { errBody = "paymentOutstanding must be less than or equal to paymentAmount" }
validateCheckoutFinancials _ _ _ _ _ _ _ =
  Right ()

validateCheckoutDispositionFields
  :: CheckoutDisposition
  -> Maybe UTCTime
  -> Either ServerError ()
validateCheckoutDispositionFields Sale (Just _) =
  Left err400 { errBody = "dueAt is not allowed for sale checkout" }
validateCheckoutDispositionFields _ _ =
  Right ()

checkoutDispositionSupportsPaymentDetails :: CheckoutDisposition -> Bool
checkoutDispositionSupportsPaymentDetails Rental = True
checkoutDispositionSupportsPaymentDetails Sale = True
checkoutDispositionSupportsPaymentDetails _ = False

validateInventoryConditionField :: Text -> Maybe Text -> Either ServerError (Maybe Text)
validateInventoryConditionField fieldName =
  validateInventoryOptionalTextField fieldName inventoryConditionMaxLength

validateInventoryNotesField :: Text -> Maybe Text -> Either ServerError (Maybe Text)
validateInventoryNotesField fieldName =
  validateInventoryOptionalTextField fieldName inventoryNotesMaxLength

validateInventoryOptionalTextField :: Text -> Int -> Maybe Text -> Either ServerError (Maybe Text)
validateInventoryOptionalTextField fieldName maxLength rawValue =
  case normalizeOptionalTextField rawValue of
    Nothing -> Right Nothing
    Just cleanValue
      | T.length cleanValue > maxLength ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 (fieldName <> " must be " <> T.pack (show maxLength) <> " characters or fewer"))
            }
      | T.any isUnsafeInventoryTextControl cleanValue ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 (fieldName <> " must not contain control characters other than tabs or line breaks"))
            }
      | otherwise ->
          Right (Just cleanValue)

isUnsafeInventoryTextControl :: Char -> Bool
isUnsafeInventoryTextControl ch =
  isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t'

inventoryConditionMaxLength :: Int
inventoryConditionMaxLength = 240

inventoryNotesMaxLength :: Int
inventoryNotesMaxLength = 1000

inventoryTermsMaxLength :: Int
inventoryTermsMaxLength = 4000

validateCheckoutDueAt :: UTCTime -> Maybe UTCTime -> Either ServerError (Maybe UTCTime)
validateCheckoutDueAt _ Nothing = Right Nothing
validateCheckoutDueAt now (Just dueAt)
  | dueAt < now =
      Left err400 { errBody = "dueAt must not be in the past" }
  | otherwise =
      Right (Just dueAt)

assetStatusForCheckoutDisposition :: CheckoutDisposition -> AssetStatus
assetStatusForCheckoutDisposition Sale = Retired
assetStatusForCheckoutDisposition _ = Booked

assetStatusForCheckinDisposition :: CheckoutDisposition -> AssetStatus
assetStatusForCheckinDisposition Sale = Retired
assetStatusForCheckinDisposition _ = Active

checkoutTargetToText :: CheckoutTarget -> Text
checkoutTargetToText TargetParty = "party"
checkoutTargetToText TargetRoom = "room"
checkoutTargetToText TargetSession = "session"

checkoutDispositionToText :: CheckoutDisposition -> Text
checkoutDispositionToText Loan = "loan"
checkoutDispositionToText Rental = "rental"
checkoutDispositionToText Sale = "sale"
checkoutDispositionToText Repair = "repair"
checkoutDispositionToText Transfer = "transfer"
checkoutDispositionToText OtherDisposition = "other"

paymentMethodToInventoryPaymentType :: PaymentMethod -> Text
paymentMethodToInventoryPaymentType CashM = "cash"
paymentMethodToInventoryPaymentType BankTransferM = "bank_transfer"
paymentMethodToInventoryPaymentType CardPOSM = "card"
paymentMethodToInventoryPaymentType PayPalM = "paypal"
paymentMethodToInventoryPaymentType StripeM = "stripe"
paymentMethodToInventoryPaymentType WompiM = "wompi"
paymentMethodToInventoryPaymentType PayPhoneM = "payphone"
paymentMethodToInventoryPaymentType CryptoM = "crypto"
paymentMethodToInventoryPaymentType OtherM = "other"

checkoutTargetRef :: AssetCheckout -> Maybe Text
checkoutTargetRef rec =
  assetCheckoutTargetPartyRef rec
    <|> fmap toPathPiece (assetCheckoutTargetRoomId rec)
    <|> fmap toPathPiece (assetCheckoutTargetSessionId rec)

checkoutPhotoOutUrl :: AssetCheckout -> Maybe Text
checkoutPhotoOutUrl rec = assetCheckoutPhotoOutUrl rec <|> assetCheckoutPhotoDriveFileId rec

toAssetDTO :: Entity Asset -> Maybe (Entity AssetCheckout) -> AssetDTO
toAssetDTO (Entity key asset) mCurrentCheckout = AssetDTO
  { assetId  = toPathPiece key
  , name     = assetName asset
  , category = assetCategory asset
  , status   = T.pack (show (assetStatus asset))
  , condition = Just (T.pack (show (assetCondition asset)))
  , brand    = assetBrand asset
  , model    = assetModel asset
  , location = fmap toPathPiece (assetLocationId asset)
  , qrToken  = assetQrCode asset
  , photoUrl = assetPhotoUrl asset
  , currentCheckoutKind = fmap (checkoutTargetToText . assetCheckoutTargetKind . entityVal) mCurrentCheckout
  , currentCheckoutTarget = mCurrentCheckout >>= (checkoutTargetRef . entityVal)
  , currentCheckoutDisposition = fmap (checkoutDispositionToText . assetCheckoutDisposition . entityVal) mCurrentCheckout
  , currentCheckoutHolderEmail = mCurrentCheckout >>= (assetCheckoutHolderEmail . entityVal)
  , currentCheckoutHolderPhone = mCurrentCheckout >>= (assetCheckoutHolderPhone . entityVal)
  , currentCheckoutAt = fmap (assetCheckoutCheckedOutAt . entityVal) mCurrentCheckout
  , currentCheckoutDueAt = mCurrentCheckout >>= (assetCheckoutDueAt . entityVal)
  , currentCheckoutPaymentType = mCurrentCheckout >>= (assetCheckoutPaymentType . entityVal)
  , currentCheckoutPaymentInstallments = mCurrentCheckout >>= (assetCheckoutPaymentInstallments . entityVal)
  , currentCheckoutPaymentAmountCents = mCurrentCheckout >>= (assetCheckoutPaymentAmountCents . entityVal)
  , currentCheckoutPaymentCurrency = mCurrentCheckout >>= (assetCheckoutPaymentCurrency . entityVal)
  , currentCheckoutPaymentOutstandingCents = mCurrentCheckout >>= (assetCheckoutPaymentOutstandingCents . entityVal)
  , currentCheckoutPhotoUrl = mCurrentCheckout >>= (checkoutPhotoOutUrl . entityVal)
  }

toCheckoutDTO :: Entity AssetCheckout -> AssetCheckoutDTO
toCheckoutDTO (Entity key rec) = AssetCheckoutDTO
  { checkoutId      = toPathPiece key
  , assetId         = toPathPiece (assetCheckoutAssetId rec)
  , targetKind      = checkoutTargetToText (assetCheckoutTargetKind rec)
  , targetSessionId = fmap toPathPiece (assetCheckoutTargetSessionId rec)
  , targetPartyRef  = assetCheckoutTargetPartyRef rec
  , targetRoomId    = fmap toPathPiece (assetCheckoutTargetRoomId rec)
  , disposition     = checkoutDispositionToText (assetCheckoutDisposition rec)
  , termsAndConditions = assetCheckoutTermsAndConditions rec
  , holderEmail     = assetCheckoutHolderEmail rec
  , holderPhone     = assetCheckoutHolderPhone rec
  , paymentType     = assetCheckoutPaymentType rec
  , paymentInstallments = assetCheckoutPaymentInstallments rec
  , paymentReference = assetCheckoutPaymentReference rec
  , paymentAmountCents = assetCheckoutPaymentAmountCents rec
  , paymentCurrency = assetCheckoutPaymentCurrency rec
  , paymentOutstandingCents = assetCheckoutPaymentOutstandingCents rec
  , checkedOutBy    = assetCheckoutCheckedOutByRef rec
  , checkedOutAt    = assetCheckoutCheckedOutAt rec
  , dueAt           = assetCheckoutDueAt rec
  , conditionOut    = assetCheckoutConditionOut rec
  , photoOutUrl     = checkoutPhotoOutUrl rec
  , conditionIn     = assetCheckoutConditionIn rec
  , photoInUrl      = assetCheckoutPhotoInUrl rec
  , returnedAt      = assetCheckoutReturnedAt rec
  , notes           = assetCheckoutNotes rec
  }

storeAssetUpload
  :: ( MonadReader Env m
     , MonadIO m
     )
  => AssetUploadForm
  -> m AssetUploadDTO
storeAssetUpload AssetUploadForm{..} = do
  Env{envConfig} <- ask
  let assetsBase = resolveConfiguredAssetsBase envConfig
      assetsRoot = assetsRootDir envConfig
      fallbackName = nonEmptyText (fdFileName aufFile)
      requestedName = aufName >>= nonEmptyText
      nameWithExt = applyExtension (requestedName <|> fallbackName) fallbackName
      safeName = sanitizeAssetName nameWithExt
  uuid <- liftIO nextRandom
  let storedName = toText uuid <> "-" <> safeName
      relPath = "inventory/" <> storedName
      targetDir = assetsRoot </> "inventory"
      targetPath = targetDir </> T.unpack storedName
  liftIO $ createDirectoryIfMissing True targetDir
  liftIO $ copyFile (fdPayload aufFile) targetPath
  let publicUrl = buildAssetUrl assetsBase relPath
  pure AssetUploadDTO
    { auFileName = storedName
    , auPath = relPath
    , auPublicUrl = publicUrl
    }

nonEmptyText :: Text -> Maybe Text
nonEmptyText txt =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just trimmed

applyExtension :: Maybe Text -> Maybe Text -> Text
applyExtension name fallback =
  let resolved = fromMaybe "upload" name
      extFromFallback =
        case fallback of
          Nothing -> ""
          Just raw -> T.pack (takeExtension (T.unpack raw))
      extFromName = T.pack (takeExtension (T.unpack resolved))
  in if T.null extFromName && not (T.null extFromFallback)
      then resolved <> extFromFallback
      else resolved

sanitizeAssetName :: Text -> Text
sanitizeAssetName raw =
  let trimmed = T.strip raw
      baseName = T.pack (takeFileName (T.unpack trimmed))
      cleaned = T.map normalizeAssetUploadChar baseName
      stripped = T.dropWhile (== '-') (T.dropWhileEnd (== '-') cleaned)
  in if T.null stripped || stripped == "." || stripped == ".."
      then "upload"
      else stripped

normalizeAssetUploadChar :: Char -> Char
normalizeAssetUploadChar ch
  | isAscii ch && isAlphaNum ch = ch
  | ch == '.' || ch == '-' || ch == '_' = ch
  | ch == ' ' = '-'
  | otherwise = '-'

buildAssetUrl :: Text -> Text -> Text
buildAssetUrl assetsBase relPath =
  let base = T.dropWhileEnd (== '/') assetsBase
      path = T.dropWhile (== '/') relPath
  in base <> "/" <> path

bandsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT BandsAPI m
bandsServer user =
       listBands
  :<|> createBandH
  :<|> bandOptionsH
  :<|> getBandH
  where
    listBands mp mps = do
      ensureModule ModuleCRM user
      (pageNum, pageSize') <- either throwError pure (validatePageParams mp mps)
      let
          pageOffset = (pageNum - 1) * pageSize'
          opts       = [Asc BandName, LimitTo pageSize', OffsetBy pageOffset]
      (bandEntities, totalCount, memberEntities, partyEntities) <- withPool $ do
        bs <- selectList ([] :: [Filter Band]) opts
        totalCount <- count ([] :: [Filter Band])
        members <- if null bs
          then pure []
          else selectList [BandMemberBandId <-. map entityKey bs] [Asc BandMemberId]
        let bandPartyIds = map (bandPartyId . entityVal) bs
            memberPartyIds = map (bandMemberPartyId . entityVal) members
            neededPartyIds = Set.toList (Set.fromList (bandPartyIds ++ memberPartyIds))
        parties <- if null neededPartyIds
          then pure []
          else selectList [M.PartyId <-. neededPartyIds] []
        pure (bs, totalCount, members, parties)
      let memberMap = Map.fromListWith (++)
            [ (bandMemberBandId (entityVal entity), [entity])
            | entity <- memberEntities
            ]
          partyMap = Map.fromList
            [ (entityKey partyEnt, partyEnt)
            | partyEnt <- partyEntities
            ]
          toMembers bandKey = Map.findWithDefault [] bandKey memberMap
          dtos = map
            (\bandEnt -> toBandDTO partyMap bandEnt (toMembers (entityKey bandEnt)))
            bandEntities
      pure (mkPage pageNum pageSize' totalCount dtos)

    createBandH req = do
      ensureModule ModuleCRM user
      let trimmedName = T.strip (bcName req)
          toPartyKey pid = toSqlKey pid :: Key Party
      memberPartyKeys <- either throwError pure
        (validateDistinctBandMemberIds (map (toPartyKey . bmiPartyId) (bcMembers req)))
      when (trimmedName == "") $
        throwError err400 { errBody = "Band name is required" }
      nameExists <- withPool $ do
        existing <- selectFirst [BandName ==. trimmedName] []
        pure (isJust existing)
      when nameExists $
        throwError err409 { errBody = "A band with this name already exists" }
      missingMemberKeys <- if null memberPartyKeys
        then pure []
        else withPool $ filterM (fmap isNothing . get) memberPartyKeys
      unless (null missingMemberKeys) $
        throwError err400 { errBody = "One or more band members reference an unknown party" }
      now <- liftIO getCurrentTime
      (bandEntity, memberEntities, partyEntities) <- withPool $ do
        partyId <- insert Party
          { partyLegalName        = Nothing
          , partyDisplayName      = trimmedName
          , partyIsOrg            = True
          , partyTaxId            = Nothing
          , partyPrimaryEmail     = Nothing
          , partyPrimaryPhone     = Nothing
          , partyWhatsapp         = Nothing
          , partyInstagram        = Nothing
          , partyEmergencyContact = Nothing
          , partyNotes            = Nothing
          , partyCreatedAt        = now
          }
        let band = Band
              { bandPartyId      = partyId
              , bandName         = trimmedName
              , bandLabelArtist  = fromMaybe False (bcLabelArtist req)
              , bandPrimaryGenre = bcPrimaryGenre req
              , bandHomeCity     = bcHomeCity req
              , bandPhotoUrl     = bcPhotoUrl req
              , bandContractFlags= bcContractFlags req
              }
        newBandId <- insert band
        for_ (bcMembers req) $ \BandMemberInput{bmiPartyId, bmiRole} -> do
          let memberPartyKey = toSqlKey bmiPartyId :: Key Party
          insert_ BandMember
            { bandMemberBandId     = newBandId
            , bandMemberPartyId    = memberPartyKey
            , bandMemberRoleInBand = bmiRole
            }
        bandEnt <- getJustEntity newBandId
        members <- selectList [BandMemberBandId ==. newBandId] [Asc BandMemberId]
        let requiredPartyIds = Set.toList . Set.fromList $ partyId : map (bandMemberPartyId . entityVal) members
        partyList <- if null requiredPartyIds
          then pure []
          else selectList [M.PartyId <-. requiredPartyIds] []
        pure (bandEnt, members, partyList)
      let partyMap = Map.fromList [ (entityKey p, p) | p <- partyEntities ]
      pure (toBandDTO partyMap bandEntity memberEntities)

    getBandH rawId = do
      ensureModule ModuleCRM user
      bandKey <- parseKey @Band rawId
      result <- withPool $ do
        mBand <- getEntity bandKey
        case mBand of
          Nothing -> pure Nothing
          Just bandEnt -> do
            members <- selectList [BandMemberBandId ==. bandKey] [Asc BandMemberId]
            let requiredPartyIds = Set.toList . Set.fromList $ bandPartyId (entityVal bandEnt) : map (bandMemberPartyId . entityVal) members
            partyList <- if null requiredPartyIds
              then pure []
              else selectList [M.PartyId <-. requiredPartyIds] []
            pure (Just (bandEnt, members, partyList))
      maybe (throwError err404)
        (\(bandEnt, members, parties) ->
          let partyMap = Map.fromList [ (entityKey p, p) | p <- parties ]
          in pure (toBandDTO partyMap bandEnt members))
        result

    bandOptionsH = do
      ensureModule ModuleCRM user
      roleOptions <- withPool $ selectList
        [ ME.DropdownOptionCategory ==. "band-role"
        , ME.DropdownOptionActive ==. True
        ]
        [ Asc ME.DropdownOptionSortOrder
        , Asc ME.DropdownOptionLabel
        , Asc ME.DropdownOptionValue
        ]
      genreOptions <- withPool $ selectList
        [ ME.DropdownOptionCategory ==. "band-genre"
        , ME.DropdownOptionActive ==. True
        ]
        [ Asc ME.DropdownOptionSortOrder
        , Asc ME.DropdownOptionLabel
        , Asc ME.DropdownOptionValue
        ]
      pure BandOptionsDTO
        { roles  = map toOptionDTO roleOptions
        , genres = map toOptionDTO genreOptions
        }

    toOptionDTO (Entity optKey option) = DropdownOptionDTO
      { optionId  = toPathPiece optKey
      , category  = dropdownOptionCategory option
      , value     = dropdownOptionValue option
      , label     = dropdownOptionLabel option
      , active    = dropdownOptionActive option
      , sortOrder = dropdownOptionSortOrder option
      }

sessionsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT SessionsAPI m
sessionsServer user =
       listSessions
  :<|> createSessionH
  :<|> sessionOptionsH
  :<|> getSessionH
  :<|> patchSessionH
  :<|> sessionInputListH
  :<|> sessionInputListPdfH
  where
    listSessions mp mps = do
      ensureModule ModuleScheduling user
      (pageNum, pageSize') <- either throwError pure (validatePageParams mp mps)
      let
          pageOffset = (pageNum - 1) * pageSize'
          opts       = [Desc SessionStartAt, LimitTo pageSize', OffsetBy pageOffset]
      (sessions, totalCount, roomEntities) <- withPool $ do
        ss <- selectList ([] :: [Filter Session]) opts
        totalCount <- count ([] :: [Filter Session])
        rooms <- if null ss
          then pure []
          else selectList [SessionRoomSessionId <-. map entityKey ss] [Asc SessionRoomId]
        pure (ss, totalCount, rooms)
      let roomMap = Map.fromListWith (++)
            [ (sessionRoomSessionId (entityVal entity), [entity])
            | entity <- roomEntities
            ]
          toRooms sessionKey = Map.findWithDefault [] sessionKey roomMap
          dtos = map (\sessionEnt -> toSessionDTO sessionEnt (toRooms (entityKey sessionEnt))) sessions
      pure (mkPage pageNum pageSize' totalCount dtos)

    createSessionH req = do
      ensureModule ModuleScheduling user
      either throwError pure (validateSessionInputRowsWrite (scInputListRows req))
      parsedRoomKeys <- traverse (parseKey @Room) (scRoomIds req)
      roomKeys <- either throwError pure (validateDistinctSessionRooms parsedRoomKeys)
      bandKey  <- traverse (parseKey @Band) (scBandId req)
      statusVal <- either throwError pure (validateSessionStatusInput (scStatus req))
      either throwError pure (validateSessionTimeRange (scStartAt req) (scEndAt req))
      either throwError pure =<< withPool (validateSessionReferences bandKey roomKeys)
      let
          status'   = fromMaybe InPrep statusVal
      (sessionEnt, rooms) <- withPool $ do
        newSessionId <- insert Session
          { sessionBookingRef           = scBookingRef req
          , sessionBandId               = bandKey
          , sessionClientPartyRef       = scClientPartyRef req
          , sessionService              = scService req
          , sessionStartAt              = scStartAt req
          , sessionEndAt                = scEndAt req
          , sessionEngineerRef          = scEngineerRef req
          , sessionAssistantRef         = scAssistantRef req
          , sessionStatus               = status'
          , sessionSampleRate           = scSampleRate req
          , sessionBitDepth             = scBitDepth req
          , sessionDaw                  = scDaw req
          , sessionSessionFolderDriveId = scSessionFolderDriveId req
          , sessionNotes                = scNotes req
          }
        for_ roomKeys $ \roomKey ->
          insert_ SessionRoom
            { sessionRoomSessionId = newSessionId
            , sessionRoomRoomId    = roomKey
            }
        ent <- getJustEntity newSessionId
        roomEnts <- selectList [SessionRoomSessionId ==. newSessionId] [Asc SessionRoomId]
        pure (ent, roomEnts)
      pure (toSessionDTO sessionEnt rooms)

    getSessionH rawId = do
      ensureModule ModuleScheduling user
      sessionKey <- parseKey @Session rawId
      result <- withPool $ do
        mSession <- getEntity sessionKey
        case mSession of
          Nothing -> pure Nothing
          Just ent -> do
            rooms <- selectList [SessionRoomSessionId ==. sessionKey] [Asc SessionRoomId]
            pure (Just (ent, rooms))
      maybe (throwError err404) (\(ent, rooms) -> pure (toSessionDTO ent rooms)) result

    patchSessionH rawId req = do
      ensureModule ModuleScheduling user
      either throwError pure (validateSessionInputRowsWrite (suInputListRows req))
      sessionKey <- parseKey @Session rawId
      existingSession <- withPool $ getEntity sessionKey
      existing <- maybe (throwError err404) pure existingSession
      bandUpdate <- case suBandId req of
        Nothing          -> pure Nothing
        Just Nothing     -> pure (Just Nothing)
        Just (Just raw)  -> do
          parsed <- parseKey @Band raw
          pure (Just (Just parsed))
      roomKeysUpdate <- case suRoomIds req of
        Nothing     -> pure Nothing
        Just rooms  -> do
          parsedRoomKeys <- traverse (parseKey @Room) rooms
          Just <$> either throwError pure (validateDistinctSessionRooms parsedRoomKeys)
      statusVal <- either throwError pure (validateSessionStatusInput (suStatus req))
      let currentSession = entityVal existing
          effectiveStartAt = fromMaybe (sessionStartAt currentSession) (suStartAt req)
          effectiveEndAt = fromMaybe (sessionEndAt currentSession) (suEndAt req)
      either throwError pure (validateSessionTimeRange effectiveStartAt effectiveEndAt)
      either throwError pure =<< withPool
        (validateSessionReferences (join bandUpdate) (fromMaybe [] roomKeysUpdate))
      let
          updates     = catMaybes
            [ fmap (SessionBookingRef =.)           (suBookingRef req)
            , fmap (SessionBandId =.)               bandUpdate
            , fmap (SessionClientPartyRef =.)       (suClientPartyRef req)
            , fmap (SessionService =.)              (suService req)
            , fmap (SessionStartAt =.)              (suStartAt req)
            , fmap (SessionEndAt =.)                (suEndAt req)
            , fmap (SessionEngineerRef =.)          (suEngineerRef req)
            , fmap (SessionAssistantRef =.)         (suAssistantRef req)
            , fmap (SessionStatus =.)               statusVal
            , fmap (SessionSampleRate =.)           (suSampleRate req)
            , fmap (SessionBitDepth =.)             (suBitDepth req)
            , fmap (SessionDaw =.)                  (suDaw req)
            , fmap (SessionSessionFolderDriveId =.) (suSessionFolderDriveId req)
            , fmap (SessionNotes =.)                (suNotes req)
            ]
      result <- withPool $ do
        unless (null updates) (update sessionKey updates)
        case roomKeysUpdate of
          Nothing      -> pure ()
          Just roomIds -> do
            deleteWhere [SessionRoomSessionId ==. sessionKey]
            for_ roomIds $ \roomKey ->
              insert_ SessionRoom
                { sessionRoomSessionId = sessionKey
                , sessionRoomRoomId    = roomKey
                }
        ent <- getJustEntity sessionKey
        rooms <- selectList [SessionRoomSessionId ==. sessionKey] [Asc SessionRoomId]
        pure (ent, rooms)
      pure (toSessionDTO (fst result) (snd result))

    sessionInputListH rawId = do
      ensureModule ModuleScheduling user
      sessionKey <- parseKey @Session rawId
      result <- withPool (InputList.fetchSessionInputRowsByKey sessionKey)
      case result of
        Nothing                -> throwError err404
        Just (_session, rows)  -> pure (map toSessionInputRowDTO rows)

    sessionInputListPdfH rawId = do
      ensureModule ModuleScheduling user
      sessionKey <- parseKey @Session rawId
      result <- withPool (InputList.fetchSessionInputRowsByKey sessionKey)
      case result of
        Nothing -> throwError err404
        Just (Entity _ session, rows) -> do
          let title = fromMaybe (sessionService session <> " session") (sessionClientPartyRef session)
              latex = InputList.renderInputListLatex title rows
          pdfResult <- liftIO (InputList.generateInputListPdf latex)
          case pdfResult of
            Left errMsg ->
              throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 errMsg) }
            Right pdf -> do
              let fileName   = InputList.sanitizeFileName title <> ".pdf"
                  disposition = T.concat ["attachment; filename=\"", fileName, "\""]
              pure (addHeader disposition pdf)

    toSessionInputRowDTO (Entity _ row) = SessionInputRow
      { channelNumber    = ME.inputRowChannelNumber row
      , trackName        = ME.inputRowTrackName row
      , instrument       = ME.inputRowInstrument row
      , micId            = fmap toPathPiece (ME.inputRowMicId row)
      , standId          = fmap toPathPiece (ME.inputRowStandId row)
      , cableId          = fmap toPathPiece (ME.inputRowCableId row)
      , preampId         = fmap toPathPiece (ME.inputRowPreampId row)
      , insertOutboardId = fmap toPathPiece (ME.inputRowInsertOutboardId row)
      , converterChannel = ME.inputRowConverterChannel row
      , phantom          = ME.inputRowPhantom row
      , polarity         = ME.inputRowPolarity row
      , hpf              = ME.inputRowHpf row
      , pad              = ME.inputRowPad row
      , notes            = ME.inputRowNotes row
      }

    sessionOptionsH = do
      ensureModule ModuleScheduling user
      (bandEntities, partyEntities) <- withPool $ do
        bs <- selectList ([] :: [Filter Band]) [Asc BandName]
        let partyIds = map (bandPartyId . entityVal) bs
        parties <- if null partyIds
          then pure []
          else selectList [M.PartyId <-. partyIds] []
        pure (bs, parties)
      let partyMap = Map.fromList [ (entityKey p, p) | p <- partyEntities ]
      pure SessionOptionsDTO
        { bands = map (toBandChoice partyMap) bandEntities
        }

    toSessionDTO (Entity key session) rooms = SessionDTO
      { sessionId             = toPathPiece key
      , sStartAt              = sessionStartAt session
      , sEndAt                = sessionEndAt session
      , sStatus               = T.pack (show (sessionStatus session))
      , sBookingRef           = sessionBookingRef session
      , sBandId               = fmap toPathPiece (sessionBandId session)
      , sClientPartyRef       = sessionClientPartyRef session
      , sService              = sessionService session
      , sEngineerRef          = sessionEngineerRef session
      , sAssistantRef         = sessionAssistantRef session
      , sRoomIds              = map (toPathPiece . sessionRoomRoomId . entityVal) rooms
      , sSampleRate           = sessionSampleRate session
      , sBitDepth             = sessionBitDepth session
      , sDaw                  = sessionDaw session
      , sSessionFolderDriveId = sessionSessionFolderDriveId session
      , sNotes                = sessionNotes session
      , sInputListRows        = []
      }

    toBandChoice partyMap (Entity bandKey band) = BandChoiceDTO
      { bandId = toPathPiece bandKey
      , name   = maybe (bandName band) (partyDisplayName . entityVal) (Map.lookup (bandPartyId band) partyMap)
      }

pipelinesServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT PipelinesAPI m
pipelinesServer user rawType =
  case parsePipelineType rawType of
    Nothing   -> notFoundHandlers
    Just kind ->
      (     listCards kind
       :<|> listStages kind
       :<|> createCard kind
       :<|> cardServer kind
      )
  where
    throw404 = throwError err404 { errBody = "Unknown pipeline type" }

    notFoundHandlers =
          throw404
      :<|> throw404
      :<|> (\_ -> throw404)
      :<|> (\_ ->
            throw404
        :<|> (\_ -> throw404)
        :<|> throw404
        )

    listCards kind = do
      ensureModule ModuleScheduling user
      entities <- withPool $ selectList
        [ ME.PipelineCardServiceKind ==. kind ]
        [ Asc ME.PipelineCardSortOrder
        , Asc ME.PipelineCardCreatedAt
        ]
      pure (map toPipelineDTO entities)

    listStages kind = do
      ensureModule ModuleScheduling user
      pure (pipelineStages kind)

    createCard kind req = do
      ensureModule ModuleScheduling user
      titleClean <- either throwError pure (normalizePipelineCardTitle (pccTitle req))
      stageValue <- resolveStage kind (pccStage req)
      let artistValue = normalizeOptionalTextField (pccArtist req)
          notesValue = normalizeOptionalTextField (pccNotes req)
      now <- liftIO getCurrentTime
      entity <- withPool $ do
        newId <- insert ME.PipelineCard
          { ME.pipelineCardServiceKind = kind
          , ME.pipelineCardTitle       = titleClean
          , ME.pipelineCardArtist      = artistValue
          , ME.pipelineCardStage       = stageValue
          , ME.pipelineCardSortOrder   = fromMaybe 0 (pccSortOrder req)
          , ME.pipelineCardNotes       = notesValue
          , ME.pipelineCardCreatedAt   = now
          , ME.pipelineCardUpdatedAt   = now
          }
        getJustEntity newId
      pure (toPipelineDTO entity)

    cardServer kind rawId =
          getCard kind rawId
     :<|> updateCard kind rawId
     :<|> deleteCard kind rawId

    getCard kind rawId = do
      ensureModule ModuleScheduling user
      cardKey <- parseKey @ME.PipelineCard rawId
      mEntity <- withPool $ getEntity cardKey
      case mEntity of
        Nothing -> throwError err404
        Just ent ->
          if ME.pipelineCardServiceKind (entityVal ent) /= kind
            then throwError err404
            else pure (toPipelineDTO ent)

    updateCard kind rawId req = do
      ensureModule ModuleScheduling user
      cardKey <- parseKey @ME.PipelineCard rawId
      titleUpdate <- either throwError pure (normalizePipelineCardTitleUpdate (pcuTitle req))
      stageUpdate <- case pcuStage req of
        Nothing   -> pure Nothing
        Just raw  -> Just <$> resolveStage kind (Just raw)
      let artistUpdate = normalizeOptionalTextFieldUpdate (pcuArtist req)
          notesUpdate = normalizeOptionalTextFieldUpdate (pcuNotes req)
      now <- liftIO getCurrentTime
      result <- withPool $ do
        mEntity <- getEntity cardKey
        case mEntity of
          Nothing -> pure Nothing
          Just (Entity key card)
            | ME.pipelineCardServiceKind card /= kind -> pure Nothing
            | otherwise -> do
                let updates = catMaybes
                      [ fmap (ME.PipelineCardTitle =.) titleUpdate
                      , fmap (ME.PipelineCardArtist =.) artistUpdate
                      , fmap (ME.PipelineCardStage =.) stageUpdate
                      , fmap (ME.PipelineCardSortOrder =.) (pcuSortOrder req)
                      , fmap (ME.PipelineCardNotes =.) notesUpdate
                      ]
                    updates' = if null updates
                      then []
                      else updates ++ [ME.PipelineCardUpdatedAt =. now]
                unless (null updates') (update key updates')
                getEntity key
      maybe (throwError err404) (pure . toPipelineDTO) result

    deleteCard kind rawId = do
      ensureModule ModuleScheduling user
      cardKey <- parseKey @ME.PipelineCard rawId
      deleted <- withPool $ do
        mCard <- get cardKey
        case mCard of
          Nothing -> pure False
          Just card ->
            if ME.pipelineCardServiceKind card /= kind
              then pure False
              else delete cardKey >> pure True
      unless deleted (throwError err404)
      pure NoContent

    resolveStage kind Nothing  = pure (defaultStage kind)
    resolveStage kind (Just raw) =
      case canonicalStage kind raw of
        Nothing   -> throwError err400 { errBody = "Invalid stage for pipeline" }
        Just val  -> pure val

    toPipelineDTO (Entity key card) = PipelineCardDTO
      { pcId        = toPathPiece key
      , pcTitle     = ME.pipelineCardTitle card
      , pcArtist    = ME.pipelineCardArtist card
      , pcType      = pipelineTypeSlug (ME.pipelineCardServiceKind card)
      , pcStage     = ME.pipelineCardStage card
      , pcSortOrder = ME.pipelineCardSortOrder card
      , pcNotes     = ME.pipelineCardNotes card
      }

roomsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT RoomsAPI m
roomsServer user = listRooms :<|> createRoomH :<|> patchRoomH
  where
    listRooms = do
      ensureModule ModuleScheduling user
      entities <- withPool $ selectList ([] :: [Filter Room]) [Asc RoomName]
      pure (map toRoomDTO entities)

    createRoomH req = do
      ensureModule ModuleScheduling user
      nameClean <- either throwError pure (normalizeRoomName (rcName req))
      ensureRoomNameAvailable Nothing nameClean
      entity <- withPool $ do
        newRoomId <- insert Room
          { roomName              = nameClean
          , roomIsBookable        = True
          , roomCapacity          = Nothing
          , roomChannelCount      = Nothing
          , roomDefaultSampleRate = Nothing
          , roomPatchbayNotes     = Nothing
          }
        getJustEntity newRoomId
      pure (toRoomDTO entity)

    patchRoomH rawId req = do
      ensureModule ModuleScheduling user
      roomKey <- parseKey @Room rawId
      nameUpdate <- either throwError pure (normalizeRoomNameUpdate (ruName req))
      for_ nameUpdate (ensureRoomNameAvailable (Just roomKey))
      let updates = catMaybes
            [ (RoomName =.)       <$> nameUpdate
            , (RoomIsBookable =.) <$> ruIsBookable req
            ]
      result <- withPool $ do
        mEntity <- getEntity roomKey
        case mEntity of
          Nothing -> pure Nothing
          Just _  -> do
            unless (null updates) (update roomKey updates)
            getEntity roomKey
      maybe (throwError err404) (pure . toRoomDTO) result

    ensureRoomNameAvailable currentRoomKey nameClean = do
      let canonicalName = canonicalRoomName nameClean
      duplicate <- withPool $ do
        existing <- selectList ([] :: [Filter Room]) []
        pure $
          any
            (\(Entity existingKey room) ->
                maybe True (/= existingKey) currentRoomKey
                  && canonicalRoomName (roomName room) == canonicalName
            )
            existing
      when duplicate $
        throwError err409 { errBody = "A room with this name already exists" }

toRoomDTO :: Entity Room -> RoomDTO
toRoomDTO (Entity key room) = RoomDTO
  { roomId    = toPathPiece key
  , rName     = roomName room
  , rBookable = roomIsBookable room
  }

normalizeRoomName :: Text -> Either ServerError Text
normalizeRoomName rawName =
  let normalized = normalizeRoomNameValue rawName
  in if T.null normalized
       then Left err400 { errBody = "Room name is required" }
       else Right normalized

normalizeRoomNameUpdate :: Maybe Text -> Either ServerError (Maybe Text)
normalizeRoomNameUpdate Nothing = Right Nothing
normalizeRoomNameUpdate (Just rawName) =
  Just <$> normalizeRoomName rawName

normalizeRoomNameValue :: Text -> Text
normalizeRoomNameValue = T.unwords . T.words

canonicalRoomName :: Text -> Text
canonicalRoomName = T.toCaseFold . normalizeRoomNameValue

normalizePipelineCardTitle :: Text -> Either ServerError Text
normalizePipelineCardTitle rawTitle =
  let trimmed = T.strip rawTitle
  in if T.null trimmed
       then Left err400 { errBody = "Pipeline card title is required" }
       else Right trimmed

normalizePipelineCardTitleUpdate :: Maybe Text -> Either ServerError (Maybe Text)
normalizePipelineCardTitleUpdate Nothing = Right Nothing
normalizePipelineCardTitleUpdate (Just rawTitle) =
  Just <$> normalizePipelineCardTitle rawTitle

normalizeOptionalTextFieldUpdate :: Maybe (Maybe Text) -> Maybe (Maybe Text)
normalizeOptionalTextFieldUpdate = fmap normalizeOptionalTextField

normalizeAssetName :: Text -> Either ServerError Text
normalizeAssetName rawName =
  validateRequiredAssetTextField "Asset name" assetNameMaxLength rawName

normalizeAssetNameUpdate :: Maybe Text -> Either ServerError (Maybe Text)
normalizeAssetNameUpdate Nothing = Right Nothing
normalizeAssetNameUpdate (Just rawName) =
  Just <$> normalizeAssetName rawName

normalizeAssetCategory :: Text -> Either ServerError Text
normalizeAssetCategory rawCategory =
  validateRequiredAssetTextField "Asset category" assetCategoryMaxLength rawCategory

normalizeAssetCategoryUpdate :: Maybe Text -> Either ServerError (Maybe Text)
normalizeAssetCategoryUpdate Nothing = Right Nothing
normalizeAssetCategoryUpdate (Just rawCategory) =
  Just <$> normalizeAssetCategory rawCategory

validateRequiredAssetTextField :: Text -> Int -> Text -> Either ServerError Text
validateRequiredAssetTextField fieldName maxLength rawValue =
  let trimmed = T.strip rawValue
  in if T.null trimmed
      then Left err400 { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " is required")) }
      else if T.length trimmed > maxLength
        then Left err400
          { errBody =
              BL.fromStrict
                (TE.encodeUtf8 (fieldName <> " must be " <> T.pack (show maxLength) <> " characters or fewer"))
          }
        else if T.any isControl trimmed
          then Left err400
            { errBody =
                BL.fromStrict (TE.encodeUtf8 (fieldName <> " must not contain control characters"))
            }
          else Right trimmed

assetNameMaxLength :: Int
assetNameMaxLength = 160

assetCategoryMaxLength :: Int
assetCategoryMaxLength = 120

validateAssetPhotoUrl :: Maybe Text -> Either ServerError (Maybe Text)
validateAssetPhotoUrl Nothing = Right Nothing
validateAssetPhotoUrl (Just rawUrl) =
  case normalizeOptionalTextField (Just rawUrl) of
    Nothing -> Right Nothing
    Just trimmedUrl
      | "https://" `T.isPrefixOf` T.toLower trimmedUrl
          && TrialsServer.isValidHttpUrl trimmedUrl ->
          Right (Just trimmedUrl)
      | Just normalizedPath <- normalizeAssetPhotoPath trimmedUrl -> Right (Just normalizedPath)
      | otherwise ->
          Left err400
            { errBody = "photoUrl must be an absolute https URL or an inventory asset path"
            }

validateAssetPhotoUrlUpdate :: Maybe Text -> Either ServerError (Maybe (Maybe Text))
validateAssetPhotoUrlUpdate Nothing = Right Nothing
validateAssetPhotoUrlUpdate (Just rawUrl) =
  case normalizeOptionalTextField (Just rawUrl) of
    Nothing ->
      Right (Just Nothing)
    Just _ ->
      Just <$> validateAssetPhotoUrl (Just rawUrl)

normalizeAssetPhotoPath :: Text -> Maybe Text
normalizeAssetPhotoPath rawPath =
  let trimmed = T.strip rawPath
      path0 = T.dropWhile (== '/') trimmed
      path1
        | "assets/serve/" `T.isPrefixOf` path0 = T.drop (T.length ("assets/serve/" :: Text)) path0
        | "assets/" `T.isPrefixOf` path0 = T.drop (T.length ("assets/" :: Text)) path0
        | otherwise = path0
      pathSegments = T.splitOn "/" path1
  in if "inventory/" `T.isPrefixOf` path1 && all isValidAssetPhotoPathSegment pathSegments
       then Just path1
       else Nothing

isValidAssetPhotoPathSegment :: Text -> Bool
isValidAssetPhotoPathSegment segment =
  not (T.null segment)
    && segment /= "."
    && segment /= ".."
    && T.all isValidAssetPhotoPathChar segment

isValidAssetPhotoPathChar :: Char -> Bool
isValidAssetPhotoPathChar ch =
  isAscii ch && (isAlphaNum ch || ch `elem` ("._-" :: String))

roomsPublicServer
  :: ( MonadReader Env m
     , MonadIO m
     )
  => ServerT RoomsPublicAPI m
roomsPublicServer = do
  entities <- withPool $ selectList
    [RoomIsBookable ==. True]
    [Asc RoomName]
  pure (map toRoomDTO entities)

serviceCatalogPublicServer
  :: ( MonadReader Env m
     , MonadIO m
     )
  => ServerT ServiceCatalogPublicAPI m
serviceCatalogPublicServer = do
  entities <- withPool $ selectList [M.ServiceCatalogActive ==. True] []
  let sorted = sortOn serviceCatalogSortKey entities
  pure (map serviceCatalogToDTO sorted)
  where
    serviceCatalogSortKey (Entity _ svc) =
      let nameNorm = normalizeServiceName (M.serviceCatalogName svc)
      in ( groupRank (M.serviceCatalogKind svc) nameNorm
         , nameRank nameNorm
         , nameNorm
         )
    groupRank kind nameNorm
      | nameNorm == "podcast" = 0 :: Int
      | otherwise =
          case kind of
            M.Recording       -> 0
            M.Rehearsal       -> 1
            M.Mixing          -> 2
            M.Mastering       -> 3
            M.Classes         -> 4
            M.EventProduction -> 5
    nameRank nameNorm =
      case nameNorm of
        "grabacion de banda"     -> 0 :: Int
        "grabacion de voz"       -> 1
        "podcast"                -> 2
        "ensayo"                 -> 0
        "practica en dj booth"   -> 1
        _                        -> 999
    normalizeServiceName =
      stripDiacritics . T.unwords . T.words . T.toLower . T.strip
    stripDiacritics = T.map replaceChar
      where
        replaceChar c =
          case c of
            'á' -> 'a'
            'é' -> 'e'
            'í' -> 'i'
            'ó' -> 'o'
            'ú' -> 'u'
            'ü' -> 'u'
            'ñ' -> 'n'
            _   -> c

serviceCatalogServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT ServiceCatalogAPI m
serviceCatalogServer user = listH :<|> createH :<|> updateH :<|> deleteH
  where
    listH mIncludeInactive = do
      ensureModule ModuleScheduling user
      let filters = [M.ServiceCatalogActive ==. True | not (fromMaybe False mIncludeInactive)]
      entities <- withPool $ selectList filters [Asc M.ServiceCatalogName]
      pure (map serviceCatalogToDTO entities)

    createH ServiceCatalogCreate{..} = do
      ensureModule ModuleScheduling user
      nameClean <- normalizeName sccName
      currencyClean <- either throwError pure (validateServiceCatalogCurrency sccCurrency)
      taxClean <- either throwError pure (validateServiceCatalogTaxBps sccTaxBps)
      billingUnitClean <-
        either throwError pure (validateServiceCatalogBillingUnit sccBillingUnit)
      when (maybe False (< 0) sccRateCents) $
        throwError err400 { errBody = "La tarifa debe ser mayor o igual a cero" }
      duplicate <- withPool $ selectFirst [M.ServiceCatalogName ==. nameClean] []
      when (isJust duplicate) $
        throwError err409 { errBody = "Ya existe un servicio con ese nombre" }
      entity <- withPool $ do
        let record = M.ServiceCatalog
              { M.serviceCatalogName = nameClean
              , M.serviceCatalogKind = fromMaybe M.Recording sccKind
              , M.serviceCatalogPricingModel = fromMaybe M.Hourly sccPricingModel
              , M.serviceCatalogDefaultRateCents = sccRateCents
              , M.serviceCatalogTaxBps = taxClean
              , M.serviceCatalogCurrency = currencyClean
              , M.serviceCatalogBillingUnit = billingUnitClean
              , M.serviceCatalogActive = fromMaybe True sccActive
              }
        newId <- insert record
        getJustEntity newId
      pure (serviceCatalogToDTO entity)

    updateH rawId ServiceCatalogUpdate{..} = do
      ensureModule ModuleScheduling user
      let svcKey = toSqlKey rawId :: Key M.ServiceCatalog
      let rateCandidate = join scuRateCents
      currencyUpdate <- either throwError pure (validateServiceCatalogCurrencyUpdate scuCurrency)
      taxUpdate <- either throwError pure (validateServiceCatalogTaxBpsUpdate scuTaxBps)
      billingUnitUpdate <-
        either throwError pure (validateServiceCatalogBillingUnitUpdate scuBillingUnit)
      when (maybe False (< 0) rateCandidate) $
        throwError err400 { errBody = "La tarifa debe ser mayor o igual a cero" }
      nameClean <- either throwError pure (normalizeServiceCatalogNameUpdate scuName)
      case nameClean of
        Just nm -> do
          conflict <- withPool $ selectFirst
            [ M.ServiceCatalogName ==. nm
            , M.ServiceCatalogId !=. svcKey
            ]
            []
          when (isJust conflict) $
            throwError err409 { errBody = "Ya existe un servicio con ese nombre" }
        Nothing -> pure ()
      updated <- withPool $ do
        mExisting <- getEntity svcKey
        case mExisting of
          Nothing -> pure Nothing
          Just _ -> do
            let updates = catMaybes
                  [ (M.ServiceCatalogName =.) <$> nameClean
                  , (M.ServiceCatalogKind =.) <$> scuKind
                  , (M.ServiceCatalogPricingModel =.) <$> scuPricingModel
                  , (M.ServiceCatalogDefaultRateCents =.) <$> scuRateCents
                  , (M.ServiceCatalogTaxBps =.) <$> taxUpdate
                  , (M.ServiceCatalogCurrency =.) <$> currencyUpdate
                  , (M.ServiceCatalogBillingUnit =.) <$> billingUnitUpdate
                  , (M.ServiceCatalogActive =.) <$> scuActive
                  ]
            unless (null updates) (update svcKey updates)
            getEntity svcKey
      maybe (throwError err404) (pure . serviceCatalogToDTO) updated

    deleteH rawId = do
      ensureModule ModuleScheduling user
      let svcKey = toSqlKey rawId :: Key M.ServiceCatalog
      found <- withPool $ do
        mSvc <- getEntity svcKey
        case mSvc of
          Nothing -> pure False
          Just _ -> update svcKey [M.ServiceCatalogActive =. False] >> pure True
      if found then pure NoContent else throwError err404

    normalizeName txt =
      either throwError pure (normalizeServiceCatalogName txt)

normalizeServiceCatalogNameUpdate :: Maybe Text -> Either ServerError (Maybe Text)
normalizeServiceCatalogNameUpdate Nothing = Right Nothing
normalizeServiceCatalogNameUpdate (Just rawName) =
  Just <$> normalizeServiceCatalogName rawName

normalizeServiceCatalogName :: Text -> Either ServerError Text
normalizeServiceCatalogName rawName =
  let trimmed = T.strip rawName
  in if T.null trimmed
       then Left err400 { errBody = "Nombre requerido" }
       else if T.length trimmed > 160
         then Left err400 { errBody = "Nombre debe tener 160 caracteres o menos" }
         else if T.any isControl trimmed
         then Left err400 { errBody = "Nombre no debe contener caracteres de control" }
         else Right trimmed

validateServiceCatalogBillingUnit :: Maybe Text -> Either ServerError (Maybe Text)
validateServiceCatalogBillingUnit Nothing = Right Nothing
validateServiceCatalogBillingUnit (Just rawBillingUnit) =
  case normalizeOptionalTextField (Just rawBillingUnit) of
    Nothing -> Right Nothing
    Just billingUnit -> Just <$> validateServiceCatalogBillingUnitValue billingUnit

validateServiceCatalogBillingUnitUpdate
  :: Maybe (Maybe Text)
  -> Either ServerError (Maybe (Maybe Text))
validateServiceCatalogBillingUnitUpdate Nothing = Right Nothing
validateServiceCatalogBillingUnitUpdate (Just Nothing) = Right (Just Nothing)
validateServiceCatalogBillingUnitUpdate (Just (Just rawBillingUnit)) =
  case normalizeOptionalTextField (Just rawBillingUnit) of
    Nothing ->
      Left err400 { errBody = "Unidad debe omitirse, ser null, o contener texto" }
    Just billingUnit ->
      Just . Just <$> validateServiceCatalogBillingUnitValue billingUnit

validateServiceCatalogBillingUnitValue :: Text -> Either ServerError Text
validateServiceCatalogBillingUnitValue billingUnit
  | T.length billingUnit > 80 =
      Left err400 { errBody = "Unidad debe tener 80 caracteres o menos" }
  | T.any isControl billingUnit =
      Left err400 { errBody = "Unidad no debe contener caracteres de control" }
  | otherwise =
      Right billingUnit

validateServiceCatalogCurrency :: Maybe Text -> Either ServerError Text
validateServiceCatalogCurrency Nothing = Right "USD"
validateServiceCatalogCurrency (Just rawCurrency) =
  let trimmed = T.toUpper (T.strip rawCurrency)
  in if T.null trimmed
       then invalidCurrency
       else
         if T.length trimmed == 3 && T.all isAsciiUpper trimmed
           then Right trimmed
           else invalidCurrency
  where
    invalidCurrency =
      Left err400 { errBody = "Moneda inválida. Usa un código ISO de 3 letras, por ejemplo USD" }

validateServiceCatalogCurrencyUpdate :: Maybe Text -> Either ServerError (Maybe Text)
validateServiceCatalogCurrencyUpdate Nothing = Right Nothing
validateServiceCatalogCurrencyUpdate (Just rawCurrency) =
  Just <$> validateServiceCatalogCurrency (Just rawCurrency)

validateServiceCatalogTaxBps :: Maybe Int -> Either ServerError (Maybe Int)
validateServiceCatalogTaxBps Nothing = Right Nothing
validateServiceCatalogTaxBps (Just rawTaxBps)
  | rawTaxBps < 0 = invalidTaxBps
  | rawTaxBps > 10000 = invalidTaxBps
  | otherwise = Right (Just rawTaxBps)
  where
    invalidTaxBps =
      Left err400 { errBody = "Impuesto inválido. Usa basis points entre 0 y 10000" }

validateServiceCatalogTaxBpsUpdate :: Maybe (Maybe Int) -> Either ServerError (Maybe (Maybe Int))
validateServiceCatalogTaxBpsUpdate Nothing = Right Nothing
validateServiceCatalogTaxBpsUpdate (Just Nothing) = Right (Just Nothing)
validateServiceCatalogTaxBpsUpdate (Just (Just rawTaxBps)) =
  Just <$> validateServiceCatalogTaxBps (Just rawTaxBps)

serviceCatalogToDTO :: Entity M.ServiceCatalog -> ServiceCatalogDTO
serviceCatalogToDTO (Entity key svc) = ServiceCatalogDTO
  { scId           = fromSqlKey key
  , scName         = M.serviceCatalogName svc
  , scKind         = M.serviceCatalogKind svc
  , scPricingModel = M.serviceCatalogPricingModel svc
  , scRateCents    = M.serviceCatalogDefaultRateCents svc
  , scCurrency     = M.serviceCatalogCurrency svc
  , scBillingUnit  = M.serviceCatalogBillingUnit svc
  , scTaxBps       = M.serviceCatalogTaxBps svc
  , scActive       = M.serviceCatalogActive svc
  }

mkPage :: Int -> Int -> Int -> [a] -> Page a
mkPage current size totalCount values =
  Page { items = values, page = current, pageSize = size, total = totalCount }

validatePageParams :: Maybe Int -> Maybe Int -> Either ServerError (Int, Int)
validatePageParams mPage mPageSize = do
  pageNum <- case mPage of
    Nothing -> Right 1
    Just n
      | n < 1 -> Left err400 { errBody = "page must be greater than or equal to 1" }
      | otherwise -> Right n
  pageSize <- case mPageSize of
    Nothing -> Right 50
    Just n
      | n < 1 || n > 100 -> Left err400 { errBody = "pageSize must be between 1 and 100" }
      | otherwise -> Right n
  pure (pageNum, pageSize)

validateInventoryPageParams :: Maybe Int -> Maybe Int -> Either ServerError (Int, Int)
validateInventoryPageParams = validatePageParams

normalizeAssetSearchQuery :: Maybe Text -> Maybe Text
normalizeAssetSearchQuery Nothing = Nothing
normalizeAssetSearchQuery (Just rawQuery) =
  let normalized = T.toCaseFold (T.strip rawQuery)
  in if T.null normalized then Nothing else Just normalized

assetMatchesSearchQuery :: Text -> Asset -> Bool
assetMatchesSearchQuery normalizedQuery asset =
  any (T.isInfixOf normalizedQuery . T.toCaseFold)
    ( assetName asset
    : assetCategory asset
    : catMaybes
        [ assetBrand asset
        , assetModel asset
        , assetSerialNumber asset
        , Just (assetOwner asset)
        , assetNotes asset
        ]
    )

filterAssetsByQuery :: Maybe Text -> [Entity Asset] -> [Entity Asset]
filterAssetsByQuery maybeQuery assets =
  case normalizeAssetSearchQuery maybeQuery of
    Nothing -> assets
    Just normalizedQuery ->
      filter (assetMatchesSearchQuery normalizedQuery . entityVal) assets

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

parseOptionalKeyField
  :: forall record.
     PathPiece (Key record)
  => Text
  -> Maybe Text
  -> Either ServerError (Maybe (Key record))
parseOptionalKeyField _ Nothing = Right Nothing
parseOptionalKeyField fieldName (Just raw) =
  let trimmed = T.strip raw
  in if T.null trimmed
      then Right Nothing
      else maybe
        (Left err400 { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be a valid identifier")) })
        (Right . Just)
        (fromPathPiece trimmed)

validateAssetQrToken :: Text -> Either ServerError Text
validateAssetQrToken rawToken =
  case UUID.fromText (T.strip rawToken) of
    Just uuid -> Right (UUID.toText uuid)
    Nothing -> Left err400 { errBody = "Invalid asset QR token" }

normalizeOptionalTextField :: Maybe Text -> Maybe Text
normalizeOptionalTextField Nothing = Nothing
normalizeOptionalTextField (Just raw) =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

normalizeAssetNotesUpdate :: Maybe Text -> Either ServerError (Maybe (Maybe Text))
normalizeAssetNotesUpdate Nothing = Right Nothing
normalizeAssetNotesUpdate (Just rawNotes) =
  case normalizeOptionalTextField (Just rawNotes) of
    Nothing -> Right (Just Nothing)
    Just _ ->
      Just <$> validateInventoryNotesField "Asset notes" (Just rawNotes)

normalizeAssetCheckinFields :: AssetCheckinRequest -> Either ServerError (Maybe Text, Maybe Text, Maybe Text)
normalizeAssetCheckinFields AssetCheckinRequest{..} =
  (\conditionIn notesVal photoUrl -> (conditionIn, notesVal, photoUrl))
    <$> validateInventoryConditionField "conditionIn" ciConditionIn
    <*> validateInventoryNotesField "notes" ciNotes
    <*> validateAssetPhotoUrl ciPhotoUrl

parseAssetStatus :: Text -> Maybe AssetStatus
parseAssetStatus = lookupStatus . normalise
  where
    lookupStatus t = case t of
      "active"             -> Just Active
      "booked"             -> Just Booked
      "outformaintenance"  -> Just OutForMaintenance
      "retired"            -> Just Retired
      _                     -> Nothing
    normalise = T.toLower . T.filter (`notElem` [' ', '_'])

validateAssetStatusUpdate :: Maybe Text -> Either ServerError (Maybe AssetStatus)
validateAssetStatusUpdate Nothing = Right Nothing
validateAssetStatusUpdate (Just rawStatus) =
  case parseAssetStatus rawStatus of
    Just statusValue -> Right (Just statusValue)
    Nothing -> Left err400
      { errBody = "Invalid asset status. Allowed values: active, booked, out_for_maintenance, retired"
      }

validateAssetPatchStatusInvariant
  :: AssetStatus
  -> Maybe AssetStatus
  -> Maybe AssetStatus
  -> Either ServerError ()
validateAssetPatchStatusInvariant currentStatus requestedStatus mActiveCheckoutStatus =
  case requestedStatus of
    Just requestedStatusValue
      | Just activeCheckoutStatus <- mActiveCheckoutStatus
      , requestedStatusValue /= activeCheckoutStatus ->
          Left err409
            { errBody = "Asset status cannot change while an active checkout exists"
            }
    Just Booked
      | isJust mActiveCheckoutStatus || currentStatus == Booked ->
          Right ()
      | otherwise ->
          Left err409
            { errBody = "Asset status can only become booked through the checkout endpoint"
            }
    _ ->
      Right ()

validateAssetCheckoutStatus :: AssetStatus -> Either ServerError ()
validateAssetCheckoutStatus Active = Right ()
validateAssetCheckoutStatus Booked =
  Left err409
    { errBody = "Asset status is booked; resolve the existing checkout state before creating a new checkout"
    }
validateAssetCheckoutStatus OutForMaintenance =
  Left err409
    { errBody = "Asset is out for maintenance and cannot be checked out"
    }
validateAssetCheckoutStatus Retired =
  Left err409
    { errBody = "Asset is retired and cannot be checked out"
    }

parseSessionStatus :: Text -> Maybe SessionStatus
parseSessionStatus = lookupStatus . normalise
  where
    lookupStatus t = case t of
      "inprep"     -> Just InPrep
      "insession"  -> Just InSession
      "break"      -> Just Break
      "editing"    -> Just Editing
      "approved"   -> Just Approved
      "delivered"  -> Just Delivered
      "closed"     -> Just Closed
      _             -> Nothing
    normalise = T.toLower . T.filter (`notElem` [' ', '_'])

validateSessionStatusInput :: Maybe Text -> Either ServerError (Maybe SessionStatus)
validateSessionStatusInput Nothing = Right Nothing
validateSessionStatusInput (Just rawStatus) =
  case parseSessionStatus rawStatus of
    Just statusValue -> Right (Just statusValue)
    Nothing -> Left err400
      { errBody = "Invalid session status. Allowed values: in_prep, in_session, break, editing, approved, delivered, closed"
      }

validateSessionTimeRange :: UTCTime -> UTCTime -> Either ServerError ()
validateSessionTimeRange startAt endAt
  | endAt > startAt = Right ()
  | otherwise = Left err400
      { errBody = "sessionEndAt must be after sessionStartAt"
      }

validateSessionInputRowsWrite :: Maybe [SessionInputRow] -> Either ServerError ()
validateSessionInputRowsWrite Nothing = Right ()
validateSessionInputRowsWrite (Just _) = Left err400
  { errBody =
      "Session input list rows are read-only on session writes; "
        <> "omit scInputListRows/suInputListRows"
  }

validateDistinctSessionRooms :: [Key Room] -> Either ServerError [Key Room]
validateDistinctSessionRooms roomKeys
  | length roomKeys == Set.size (Set.fromList roomKeys) = Right roomKeys
  | otherwise = Left err400
      { errBody = "roomIds must not contain duplicates"
      }

validateSessionReferences
  :: MonadIO m
  => Maybe (Key Band)
  -> [Key Room]
  -> SqlPersistT m (Either ServerError ())
validateSessionReferences mBandKey roomKeys = do
  mBand <- join <$> traverse getEntity mBandKey
  existingRoomKeys <-
    if null roomKeys
      then pure Set.empty
      else Set.fromList . map entityKey <$> selectList [RoomId <-. roomKeys] []
  pure $
    if isJust mBandKey && isNothing mBand
      then Left err400 { errBody = "bandId references an unknown band" }
      else if any (`Set.notMember` existingRoomKeys) roomKeys
        then Left err400 { errBody = "roomIds reference one or more unknown rooms" }
        else Right ()

validateCheckoutTargetReferences
  :: MonadIO m
  => Maybe (Key Room)
  -> Maybe (Key ME.Session)
  -> SqlPersistT m (Either ServerError ())
validateCheckoutTargetReferences mRoomKey mSessionKey = do
  mRoom <- join <$> traverse getEntity mRoomKey
  mSession <- join <$> traverse getEntity mSessionKey
  pure $
    if isJust mRoomKey && isNothing mRoom
      then Left err400 { errBody = "targetRoom references an unknown room" }
      else if isJust mSessionKey && isNothing mSession
        then Left err400 { errBody = "targetSession references an unknown session" }
        else Right ()

validateDistinctBandMemberIds :: [Key Party] -> Either ServerError [Key Party]
validateDistinctBandMemberIds partyKeys
  | any ((<= 0) . fromSqlKey) partyKeys = Left err400
      { errBody = "band member party ids must be positive"
      }
  | length partyKeys == Set.size (Set.fromList partyKeys) = Right partyKeys
  | otherwise = Left err400
      { errBody = "band members must not contain duplicates"
      }

ensureModule
  :: (MonadError ServerError m)
  => ModuleAccess
  -> AuthedUser
  -> m ()
ensureModule moduleTag user =
  unless (hasModuleAccess moduleTag user) $
    throwError err403 { errBody = "Missing required module access" }

-- Basic payments server (manual payouts / honorarios)
paymentsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT PaymentsAPI m
paymentsServer user =
       listPaymentsH
  :<|> createPaymentH
  :<|> getPaymentH
  where
    listPaymentsH mPartyId = do
      ensureModule ModuleAdmin user
      partyIdFilter <- either throwError pure (validateOptionalPositivePaymentReferenceId "partyId" mPartyId)
      let filt = maybe [] (\pid -> [M.PaymentPartyId ==. toSqlKey pid]) partyIdFilter
      recs <- withPool $ selectList filt [Desc M.PaymentReceivedAt, LimitTo 200]
      pure (map toPaymentDTO recs)

    createPaymentH PaymentCreate{..} = do
      ensureModule ModuleAdmin user
      partyId <- either throwError pure (validatePositivePaymentReferenceId "partyId" pcPartyId)
      orderId <- either throwError pure (validateOptionalPositivePaymentReferenceId "orderId" pcOrderId)
      invoiceId <- either throwError pure (validateOptionalPositivePaymentReferenceId "invoiceId" pcInvoiceId)
      parsedPaidAt <- parseUTCTimeText pcPaidAt
      now <- liftIO getCurrentTime
      paidAt <- either throwError pure (validatePaymentPaidAt now parsedPaidAt)
      amountCents <- either throwError pure (validatePaymentAmountCents pcAmountCents)
      _ <- either throwError pure (validatePaymentCurrency pcCurrency)
      conceptVal <- either throwError pure (validatePaymentConcept pcConcept)
      paymentMethodVal <- either throwError pure (validatePaymentMethod pcMethod)
      referenceVal <- either throwError pure (validatePaymentReference pcReference)
      periodVal <- either throwError pure (validatePaymentPeriod pcPeriod)
      attachmentUrl <- either throwError pure (validatePaymentAttachmentUrl pcAttachmentUrl)
      let partyKey   = toSqlKey partyId
          mOrderKey  = toSqlKey <$> orderId
          mInvoiceKey= toSqlKey <$> invoiceId
      either throwError pure =<< withPool (validatePaymentReferences partyKey mOrderKey mInvoiceKey)
      ent <- withPool $ do
        payId <- insert Payment
          { paymentInvoiceId   = mInvoiceKey
          , paymentOrderId     = mOrderKey
          , paymentPartyId     = partyKey
          , paymentMethod      = paymentMethodVal
          , paymentAmountCents = amountCents
          , paymentReceivedAt  = paidAt
          , paymentReference   = referenceVal
          , paymentConcept     = Just conceptVal
          , paymentPeriod      = periodVal
          , paymentAttachment  = attachmentUrl
          , paymentCreatedBy   = Just (auPartyId user)
          , paymentCreatedAt   = Just now
          }
        getJustEntity payId
      pure (toPaymentDTO ent)

    getPaymentH pid = do
      ensureModule ModuleAdmin user
      paymentId <- either throwError pure (validatePositivePaymentReferenceId "paymentId" pid)
      mEnt <- withPool $ getEntity (toSqlKey paymentId :: Key Payment)
      maybe (throwError err404) (pure . toPaymentDTO) mEnt

    toPaymentDTO (Entity key p) = PaymentDTO
      { payId          = fromSqlKey key
      , payPartyId     = fromSqlKey (paymentPartyId p)
      , payOrderId     = fmap fromSqlKey (paymentOrderId p)
      , payInvoiceId   = fmap fromSqlKey (paymentInvoiceId p)
      , payAmountCents = M.paymentAmountCents p
      , payCurrency    = "USD"
      , payMethod      = T.pack (show (paymentMethod p))
      , payReference   = M.paymentReference p
      , payPaidAt      = T.pack (show (paymentReceivedAt p))
      , payConcept     = fromMaybe "" (paymentConcept p)
      , payPeriod      = paymentPeriod p
      , payAttachment  = paymentAttachment p
      }

validatePaymentAmountCents :: Int -> Either ServerError Int
validatePaymentAmountCents amountCents
  | amountCents > 0 = Right amountCents
  | otherwise = Left err400 { errBody = "amountCents must be greater than 0" }

validatePaymentPaidAt :: UTCTime -> UTCTime -> Either ServerError UTCTime
validatePaymentPaidAt now paidAt
  | paidAt <= now = Right paidAt
  | otherwise = Left err400 { errBody = "paidAt must not be in the future" }

validatePositivePaymentReferenceId :: Text -> Int64 -> Either ServerError Int64
validatePositivePaymentReferenceId fieldName rawId
  | rawId > 0 = Right rawId
  | otherwise =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be a positive integer"))
        }

validateOptionalPositivePaymentReferenceId :: Text -> Maybe Int64 -> Either ServerError (Maybe Int64)
validateOptionalPositivePaymentReferenceId _ Nothing = Right Nothing
validateOptionalPositivePaymentReferenceId fieldName (Just rawId) =
  Just <$> validatePositivePaymentReferenceId fieldName rawId

validatePaymentReferences
  :: MonadIO m
  => Key Party
  -> Maybe (Key M.ServiceOrder)
  -> Maybe (Key M.Invoice)
  -> SqlPersistT m (Either ServerError ())
validatePaymentReferences partyKey mOrderKey mInvoiceKey = do
  mParty <- getEntity partyKey
  mOrder <- join <$> traverse getEntity mOrderKey
  mInvoice <- join <$> traverse getEntity mInvoiceKey
  mInvoiceOrderLine <- case (mOrderKey, mInvoiceKey) of
    (Just orderKey, Just invoiceKey) ->
      selectFirst
        [ M.InvoiceLineInvoiceId ==. invoiceKey
        , M.InvoiceLineServiceOrderId ==. Just orderKey
        ]
        []
    _ -> pure Nothing
  pure $
    if isNothing mParty
      then Left err400 { errBody = "partyId references an unknown party" }
      else if isJust mOrderKey && isNothing mOrder
        then Left err400 { errBody = "orderId references an unknown service order" }
        else if isJust mInvoiceKey && isNothing mInvoice
          then Left err400 { errBody = "invoiceId references an unknown invoice" }
          else if maybe False ((/= partyKey) . M.serviceOrderCustomerId . entityVal) mOrder
            then Left err400 { errBody = "orderId does not belong to partyId" }
            else if maybe False ((/= partyKey) . M.invoiceCustomerId . entityVal) mInvoice
              then Left err400 { errBody = "invoiceId does not belong to partyId" }
              else if isJust mOrderKey && isJust mInvoiceKey && isNothing mInvoiceOrderLine
                then Left err400 { errBody = "invoiceId does not include orderId" }
              else Right ()

validatePaymentConcept :: Text -> Either ServerError Text
validatePaymentConcept rawConcept =
  let trimmed = T.strip rawConcept
  in if T.null trimmed
       then Left err400 { errBody = "concept is required" }
       else if T.length trimmed > 240
         then Left err400 { errBody = "concept must be 240 characters or fewer" }
         else if T.any isControl trimmed
           then Left err400 { errBody = "concept must not contain control characters" }
           else Right trimmed

validatePaymentReference :: Maybe Text -> Either ServerError (Maybe Text)
validatePaymentReference =
  validateOptionalPaymentTextField "reference" 160

validatePaymentPeriod :: Maybe Text -> Either ServerError (Maybe Text)
validatePaymentPeriod =
  validateOptionalPaymentTextField "period" 80

validateOptionalPaymentTextField :: Text -> Int -> Maybe Text -> Either ServerError (Maybe Text)
validateOptionalPaymentTextField fieldName maxLength rawValue =
  case normalizeOptionalTextField rawValue of
    Nothing -> Right Nothing
    Just value
      | T.length value > maxLength ->
          Left err400
            { errBody =
                BL.fromStrict $
                  TE.encodeUtf8 $
                    fieldName <> " must be " <> T.pack (show maxLength) <> " characters or fewer"
            }
      | T.any isControl value ->
          Left err400
            { errBody =
                BL.fromStrict $
                  TE.encodeUtf8 $
                    fieldName <> " must not contain control characters"
            }
      | otherwise ->
          Right (Just value)

validatePaymentMethod :: Text -> Either ServerError PaymentMethod
validatePaymentMethod rawMethod
  | T.any isControl rawMethod =
      Left err400 { errBody = "paymentMethod must not contain control characters" }
  | otherwise =
      case normalized of
        "cash" -> Right CashM
        "cashm" -> Right CashM
        "efectivo" -> Right CashM
        "bank" -> Right BankTransferM
        "banktransfer" -> Right BankTransferM
        "banktransferm" -> Right BankTransferM
        "transferencia" -> Right BankTransferM
        "produbanco" -> Right BankTransferM
        "card" -> Right CardPOSM
        "cardpos" -> Right CardPOSM
        "cardposm" -> Right CardPOSM
        "paypal" -> Right PayPalM
        "paypalm" -> Right PayPalM
        "stripe" -> Right StripeM
        "stripem" -> Right StripeM
        "wompi" -> Right WompiM
        "wompim" -> Right WompiM
        "payphone" -> Right PayPhoneM
        "payphonem" -> Right PayPhoneM
        "crypto" -> Right CryptoM
        "cryptom" -> Right CryptoM
        "other" -> Right OtherM
        "otherm" -> Right OtherM
        _ -> invalidPaymentMethod
  where
    normalized =
      T.toLower (T.filter (`notElem` [' ', '_', '-']) (T.strip rawMethod))
    invalidPaymentMethod =
      Left err400
        { errBody =
            "paymentMethod must be one of: cash, bank_transfer, bank, transferencia, produbanco, card, paypal, stripe, wompi, payphone, crypto, other"
        }

validatePaymentCurrency :: Text -> Either ServerError Text
validatePaymentCurrency rawCurrency =
  let normalized = T.toUpper (T.strip rawCurrency)
  in if T.any isControl rawCurrency
       then Left err400 { errBody = "currency must not contain control characters" }
     else if normalized == "USD"
       then Right normalized
       else Left err400 { errBody = "Only USD manual payments are currently supported" }

validatePaymentAttachmentUrl :: Maybe Text -> Either ServerError (Maybe Text)
validatePaymentAttachmentUrl Nothing = Right Nothing
validatePaymentAttachmentUrl (Just rawUrl) =
  case normalizeOptionalTextField (Just rawUrl) of
    Nothing -> Right Nothing
    Just attachmentUrl
      | "https://" `T.isPrefixOf` T.toLower attachmentUrl
          && TrialsServer.isValidHttpUrl attachmentUrl ->
          Right (Just attachmentUrl)
      | otherwise ->
          Left err400 { errBody = "attachmentUrl must be an absolute https URL" }

data MetaChannel = MetaInstagram | MetaFacebook
  deriving (Eq, Show)

metaChannelLabel :: MetaChannel -> Text
metaChannelLabel MetaInstagram = "instagram"
metaChannelLabel MetaFacebook = "facebook"

-- Meta webhook parser: accepts Messenger-style `entry.messaging` and Graph-style
-- `entry.changes[].value` message envelopes used by Instagram/Facebook webhooks.
data IGWebhook = IGWebhook
  { igEntries :: [IGEntry]
  } deriving (Show)

data IGEntry = IGEntry
  { igEntryId :: Maybe Text
  , igMessaging :: [IGMessaging]
  , igChanges :: [IGChange]
  } deriving (Show)

data IGActor = IGActor
  { igId :: Text
  } deriving (Show)

data IGMessage = IGMessage
  { igMid    :: Maybe Text
  , igText   :: Maybe Text
  , igIsEcho :: Maybe Bool
  , igIsDeleted :: Maybe Bool
  , igReferral :: Maybe IGReferral
  , igAttachments :: Maybe [A.Value]
  } deriving (Show)

data IGReferral = IGReferral
  { igRefAdId        :: Maybe Text
  , igRefAdTitle     :: Maybe Text
  , igRefCampaignId  :: Maybe Text
  , igRefCampaignName :: Maybe Text
  , igRefSourceType  :: Maybe Text
  , igRefSourceId    :: Maybe Text
  } deriving (Show)

data IGMessaging = IGMessaging
  { igSender    :: IGActor
  , igRecipient :: Maybe IGActor
  , igMessage   :: Maybe IGMessage
  , igReferral  :: Maybe IGReferral
  , igTimestamp :: Maybe Int
  } deriving (Show)

data IGChange = IGChange
  { igChangeField :: Maybe Text
  , igChangeValue :: Maybe IGChangeValue
  } deriving (Show)

data IGChangeValue = IGChangeValue
  { igChangeMessage :: Maybe IGMessage
  , igChangeFrom :: Maybe IGChangeActor
  , igChangeTimestamp :: Maybe Int
  , igChangeReferral :: Maybe IGReferral
  , igChangeDeleted :: Maybe Bool
  , igChangeMid :: Maybe Text
  } deriving (Show)

data IGChangeActor = IGChangeActor
  { igActorId :: Text
  , igActorName :: Maybe Text
  } deriving (Show)

instance A.FromJSON IGWebhook where
  parseJSON = withObject "IGWebhook" $ \o -> do
    igEntries <- o .:? "entry" .!= []
    pure IGWebhook{..}

instance A.FromJSON IGEntry where
  parseJSON = withObject "IGEntry" $ \o -> do
    igEntryId <- o .:? "id"
    igMessaging <- o .:? "messaging" .!= []
    igChanges <- o .:? "changes" .!= []
    pure IGEntry{..}

instance A.FromJSON IGActor where
  parseJSON = withObject "IGActor" $ \o -> do
    igId <- o .: "id"
    pure IGActor{..}

instance A.FromJSON IGMessage where
  parseJSON = withObject "IGMessage" $ \o -> do
    igMid <- o .:? "mid"
    igText <- o .:? "text"
    igIsEcho <- o .:? "is_echo"
    igIsDeleted <- o .:? "is_deleted" <|> o .:? "deleted"
    igReferral <- o .:? "referral"
    igAttachments <- o .:? "attachments"
    pure IGMessage{..}

instance A.FromJSON IGReferral where
  parseJSON = withObject "IGReferral" $ \o -> do
    igRefAdId <- o .:? "ad_id"
    igRefAdTitle <- o .:? "ad_title"
    igRefCampaignId <- o .:? "campaign_id"
    igRefCampaignName <- o .:? "campaign_name"
    igRefSourceType <- o .:? "source_type"
    igRefSourceId <- o .:? "source_id"
    pure IGReferral{..}

instance A.FromJSON IGMessaging where
  parseJSON = withObject "IGMessaging" $ \o -> do
    igSender <- o .: "sender"
    igRecipient <- o .:? "recipient"
    igMessage <- o .:? "message"
    igReferral <- o .:? "referral"
    rawTs <- o .:? "timestamp"
    igTimestamp <- parseTimestampMaybe rawTs
    pure IGMessaging{..}

instance A.FromJSON IGChange where
  parseJSON = withObject "IGChange" $ \o -> do
    igChangeField <- o .:? "field"
    igChangeValue <- o .:? "value"
    pure IGChange{..}

instance A.FromJSON IGChangeValue where
  parseJSON = withObject "IGChangeValue" $ \o -> do
    igChangeMessage <- o .:? "message"
    igChangeFrom <- o .:? "from"
    rawTs <- o .:? "timestamp"
    igChangeTimestamp <- parseTimestampMaybe rawTs
    igChangeReferral <- o .:? "referral"
    igChangeDeleted <- o .:? "is_deleted" <|> o .:? "deleted"
    igChangeMid <- o .:? "mid"
    pure IGChangeValue{..}

instance A.FromJSON IGChangeActor where
  parseJSON = withObject "IGChangeActor" $ \o -> do
    igActorId <- o .: "id"
    igActorName <- o .:? "username"
      <|> o .:? "name"
    pure IGChangeActor{..}

parseTimestampMaybe :: Maybe A.Value -> Parser (Maybe Int)
parseTimestampMaybe Nothing = pure Nothing
parseTimestampMaybe (Just raw) =
  case raw of
    A.Number n ->
      case Sci.toBoundedInteger n of
        Just v -> pure (Just v)
        Nothing -> fail "Invalid timestamp number"
    A.String txt ->
      case reads (T.unpack (T.strip txt)) of
        [(v, "")] -> pure (Just v)
        _ -> fail "Invalid timestamp text"
    _ -> fail "Invalid timestamp type"

data IGInbound = IGInbound
  { igInboundExternalId :: Text
  , igInboundSenderId   :: Text
  , igInboundSenderName :: Maybe Text
  , igInboundText       :: Text
  , igInboundAdExternalId :: Maybe Text
  , igInboundAdName     :: Maybe Text
  , igInboundCampaignExternalId :: Maybe Text
  , igInboundCampaignName :: Maybe Text
  , igInboundMetadata   :: Maybe Text
  } deriving (Eq, Show)

data IGInboundDeleted = IGInboundDeleted
  { igInboundDeletedExternalId :: Text
  , igInboundDeletedSenderId :: Text
  , igInboundDeletedSenderName :: Maybe Text
  , igInboundDeletedMetadata :: Maybe Text
  } deriving (Eq, Show)

data MetaInboundEvent
  = MetaInboundMessage IGInbound
  | MetaInboundDeleted IGInboundDeleted
  deriving (Eq, Show)

extractMetaInbound :: A.Value -> [MetaInboundEvent]
extractMetaInbound payload =
  case parseMaybe A.parseJSON payload of
    Nothing -> []
    Just IGWebhook{igEntries} -> concatMap extractEntry igEntries
  where
    extractEntry IGEntry{igEntryId, igMessaging, igChanges} =
      mapMaybe (extractMessagingEvent igEntryId) igMessaging <> mapMaybe (extractChangeEvent igEntryId) igChanges

    extractMessagingEvent mEntryId IGMessaging{igSender, igRecipient, igMessage, igReferral = eventReferral, igTimestamp} = do
      msg@IGMessage{igMid, igText, igIsEcho, igReferral = msgReferral, igAttachments, igIsDeleted} <- igMessage
      if fromMaybe False igIsDeleted
        then buildDeleted
          (igId igSender)
          Nothing
          (igId <$> igRecipient)
          mEntryId
          (igMid <|> (stripDeletedMessageId msg))
          (eventReferral <|> msgReferral)
          igTimestamp
        else buildInbound
          (igId igSender)
          Nothing
          (igId <$> igRecipient)
          mEntryId
          igMid
          igText
          igIsEcho
          (eventReferral <|> msgReferral)
          igAttachments
          igTimestamp

    extractChangeEvent mEntryId IGChange{igChangeField, igChangeValue} = do
      guard (maybe True (\raw -> T.toCaseFold (T.strip raw) == "messages") igChangeField)
      IGChangeValue{igChangeMessage, igChangeFrom, igChangeTimestamp, igChangeReferral, igChangeDeleted, igChangeMid} <- igChangeValue
      IGChangeActor{igActorId, igActorName} <- igChangeFrom
      case igChangeMessage of
        Just msg@IGMessage{igMid, igText, igIsEcho, igReferral = msgReferral, igAttachments, igIsDeleted} ->
          if fromMaybe False (igIsDeleted <|> igChangeDeleted)
            then buildDeleted
              igActorId
              igActorName
              Nothing
              mEntryId
              (igMid <|> igChangeMid <|> stripDeletedMessageId msg)
              (igChangeReferral <|> msgReferral)
              igChangeTimestamp
            else buildInbound
              igActorId
              igActorName
              Nothing
              mEntryId
              igMid
              igText
              igIsEcho
              (igChangeReferral <|> msgReferral)
              igAttachments
              igChangeTimestamp
        Nothing ->
          guard (fromMaybe False igChangeDeleted) >>
          buildDeleted
            igActorId
            igActorName
            Nothing
            mEntryId
            igChangeMid
            igChangeReferral
            igChangeTimestamp

    buildInbound rawSenderId senderName mRecipientId mEntryId mMid mText mIsEcho mReferral mAttachments mTs = do
      guard (not (fromMaybe False mIsEcho))
      senderId <- normalizeMetaWebhookActorId rawSenderId
      let (adExt, adName, campExt, campName, refMeta) = toReferralMeta mReferral
          cleanRecipientId = mRecipientId >>= normalizeMetaWebhookActorId
          attachmentPairs = case mAttachments of
            Just xs | not (null xs) -> ["attachments" .= xs]
            _ -> []
          meta = encodeMeta refMeta senderName cleanRecipientId mEntryId attachmentPairs
          rawText = fromMaybe "" mText
          body = if not (T.null (T.strip rawText))
            then rawText
            else if null attachmentPairs
              then ""
              else "[attachment]"
      guard (not (T.null (T.strip body)))
      let fallbackBase = T.intercalate "|"
            [ senderId
            , fromMaybe "" senderName
            , fromMaybe "" cleanRecipientId
            , fromMaybe "" mEntryId
            , maybe "" (T.pack . show) mTs
            , body
            , fromMaybe "" meta
            ]
          fallbackId = senderId <> "-" <> toHashText fallbackBase
          externalId = fromMaybe fallbackId (normalizeMetaWebhookExternalId mMid)
      pure (MetaInboundMessage IGInbound
        { igInboundExternalId = externalId
        , igInboundSenderId = senderId
        , igInboundSenderName = senderName
        , igInboundText = body
        , igInboundAdExternalId = adExt
        , igInboundAdName = adName
        , igInboundCampaignExternalId = campExt
        , igInboundCampaignName = campName
        , igInboundMetadata = meta
        })

    buildDeleted rawSenderId senderName mRecipientId mEntryId mMid mReferral _mTs = do
      senderId <- normalizeMetaWebhookActorId rawSenderId
      externalId <- normalizeMetaWebhookExternalId mMid
      let (_, _, _, _, refMeta) = toReferralMeta mReferral
          cleanRecipientId = mRecipientId >>= normalizeMetaWebhookActorId
          meta = encodeMeta refMeta senderName cleanRecipientId mEntryId ["event" .= ("message_deleted" :: Text)]
      pure (MetaInboundDeleted IGInboundDeleted
        { igInboundDeletedExternalId = externalId
        , igInboundDeletedSenderId = senderId
        , igInboundDeletedSenderName = senderName
        , igInboundDeletedMetadata = meta
        })

    encodeMeta refMeta senderName mRecipientId mEntryId extraPairs =
      let senderNamePairs = case senderName of
            Just nm | not (T.null (T.strip nm)) -> ["sender_name" .= nm]
            _ -> []
          recipientPairs = case mRecipientId of
            Just rid | not (T.null (T.strip rid)) -> ["recipient_id" .= rid]
            _ -> []
          entryPairs = case mEntryId of
            Just eid | not (T.null (T.strip eid)) -> ["entry_id" .= eid]
            _ -> []
          metaPairs = refMeta ++ extraPairs ++ senderNamePairs ++ recipientPairs ++ entryPairs
      in if null metaPairs
          then Nothing
          else Just (TE.decodeUtf8 (BL.toStrict (A.encode (object metaPairs))))

    stripDeletedMessageId :: IGMessage -> Maybe Text
    stripDeletedMessageId IGMessage{igAttachments} =
      igAttachments >>= extractDeletedMidFromAttachments

    extractDeletedMidFromAttachments :: [A.Value] -> Maybe Text
    extractDeletedMidFromAttachments [] = Nothing
    extractDeletedMidFromAttachments (raw:rest) =
      extractDeletedMid raw <|> extractDeletedMidFromAttachments rest

    extractDeletedMid :: A.Value -> Maybe Text
    extractDeletedMid raw =
      join (parseMaybe (withObject "IGAttachment" (\o -> do
        attachmentPayload <- o .:? "payload"
        case attachmentPayload of
          Just rawPayload ->
            withObject "IGAttachmentPayload" (\payloadObj ->
              payloadObj .:? "mid" <|> payloadObj .:? "message_id" <|> payloadObj .:? "id"
            ) rawPayload
          Nothing ->
            o .:? "mid" <|> o .:? "message_id" <|> o .:? "id"
      )) raw)
    simpleHash64 = T.foldl' step (14695981039346656037 :: Word64)
      where
        step h c = (h `xor` fromIntegral (ord c)) * 1099511628211

    toHashText txt =
      let hexTxt = T.pack (showHex (simpleHash64 txt) "")
      in T.justifyRight 16 '0' hexTxt

    toReferralMeta Nothing = (Nothing, Nothing, Nothing, Nothing, [])
    toReferralMeta (Just IGReferral{..}) =
      let adExt = igRefAdId <|> igRefSourceId
          adName = igRefAdTitle
          campExt = igRefCampaignId
          campName = igRefCampaignName
          metaPairs =
            [ "ad_id" .= igRefAdId
            , "ad_title" .= igRefAdTitle
            , "campaign_id" .= igRefCampaignId
            , "campaign_name" .= igRefCampaignName
            , "source_type" .= igRefSourceType
            , "source_id" .= igRefSourceId
            ]
      in (adExt, adName, campExt, campName, metaPairs)

extractMetaChannel :: A.Value -> Maybe MetaChannel
extractMetaChannel payload =
  join (parseMaybe parseObject payload)
  where
    parseObject = withObject "MetaWebhookObject" $ \o -> do
      mObj <- o .:? "object"
      pure $ case fmap (T.toCaseFold . T.strip) mObj of
        Just "instagram" -> Just MetaInstagram
        Just "page" -> Just MetaFacebook
        Just "facebook" -> Just MetaFacebook
        _ -> Nothing

validateMetaWebhookChannel :: MetaChannel -> A.Value -> Either ServerError MetaChannel
validateMetaWebhookChannel expected payload =
  case extractMetaChannel payload of
    Nothing ->
      Left err400 { errBody = "Meta webhook object must be one of: instagram, page, facebook" }
    Just actual
      | actual == expected -> Right actual
      | otherwise ->
          Left err400
            { errBody =
                "Meta webhook object does not match the webhook endpoint"
            }

persistMetaInbound
  :: MonadIO m
  => MetaChannel
  -> UTCTime
  -> [MetaInboundEvent]
  -> SqlPersistT m ()
persistMetaInbound channel now incoming =
  for_ incoming $ \event ->
    case event of
      MetaInboundMessage IGInbound{..} ->
        case channel of
          MetaInstagram ->
            upsertInstagram igInboundExternalId igInboundSenderId igInboundSenderName igInboundText
              igInboundAdExternalId igInboundAdName igInboundCampaignExternalId igInboundCampaignName igInboundMetadata
          MetaFacebook ->
            upsertFacebook igInboundExternalId igInboundSenderId igInboundSenderName igInboundText
              igInboundAdExternalId igInboundAdName igInboundCampaignExternalId igInboundCampaignName igInboundMetadata
      MetaInboundDeleted IGInboundDeleted{..} ->
        case channel of
          MetaInstagram ->
            tombstoneInstagram igInboundDeletedExternalId igInboundDeletedSenderId igInboundDeletedSenderName igInboundDeletedMetadata
          MetaFacebook ->
            tombstoneFacebook igInboundDeletedExternalId igInboundDeletedSenderId igInboundDeletedSenderName igInboundDeletedMetadata
  where
    upsertInstagram externalId senderId senderName body adExt adName campExt campName meta = do
      _ <- upsert (M.InstagramMessage externalId
                    senderId
                    senderName
                    (Just body)
                    "incoming"
                    adExt
                    adName
                    campExt
                    campName
                    meta
                    "pending"
                    Nothing
                    Nothing
                    Nothing
                    0
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    now)
           ( [ M.InstagramMessageDirection =. "incoming"
             , M.InstagramMessageReplyStatus =. "pending"
             , M.InstagramMessageText =. Just body
             ]
             ++ maybe [] (\value -> [M.InstagramMessageSenderName =. Just value]) senderName
             ++ maybe [] (\value -> [M.InstagramMessageAdExternalId =. Just value]) adExt
             ++ maybe [] (\value -> [M.InstagramMessageAdName =. Just value]) adName
             ++ maybe [] (\value -> [M.InstagramMessageCampaignExternalId =. Just value]) campExt
             ++ maybe [] (\value -> [M.InstagramMessageCampaignName =. Just value]) campName
             ++ maybe [] (\value -> [M.InstagramMessageMetadata =. Just value]) meta
           )
      pure ()

    upsertFacebook externalId senderId senderName body adExt adName campExt campName meta = do
      _ <- upsert (ME.FacebookMessage externalId
                    senderId
                    senderName
                    (Just body)
                    "incoming"
                    adExt
                    adName
                    campExt
                    campName
                    meta
                    "pending"
                    Nothing
                    Nothing
                    Nothing
                    0
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    now)
           ( [ ME.FacebookMessageDirection =. "incoming"
             , ME.FacebookMessageReplyStatus =. "pending"
             , ME.FacebookMessageText =. Just body
             ]
             ++ maybe [] (\value -> [ME.FacebookMessageSenderName =. Just value]) senderName
             ++ maybe [] (\value -> [ME.FacebookMessageAdExternalId =. Just value]) adExt
             ++ maybe [] (\value -> [ME.FacebookMessageAdName =. Just value]) adName
             ++ maybe [] (\value -> [ME.FacebookMessageCampaignExternalId =. Just value]) campExt
             ++ maybe [] (\value -> [ME.FacebookMessageCampaignName =. Just value]) campName
             ++ maybe [] (\value -> [ME.FacebookMessageMetadata =. Just value]) meta
           )
      pure ()

    tombstoneInstagram externalId senderId senderName meta = do
      _ <- upsert (M.InstagramMessage externalId
                    senderId
                    senderName
                    Nothing
                    "incoming"
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    meta
                    "pending"
                    Nothing
                    Nothing
                    Nothing
                    0
                    Nothing
                    Nothing
                    Nothing
                    (Just now)
                    now)
           ( [ M.InstagramMessageDeletedAt =. Just now ]
             ++ maybe [] (\value -> [M.InstagramMessageSenderName =. Just value]) senderName
             ++ maybe [] (\value -> [M.InstagramMessageMetadata =. Just value]) meta
           )
      pure ()

    tombstoneFacebook externalId senderId senderName meta = do
      _ <- upsert (ME.FacebookMessage externalId
                    senderId
                    senderName
                    Nothing
                    "incoming"
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    meta
                    "pending"
                    Nothing
                    Nothing
                    Nothing
                    0
                    Nothing
                    Nothing
                    Nothing
                    (Just now)
                    now)
           ( [ ME.FacebookMessageDeletedAt =. Just now ]
             ++ maybe [] (\value -> [ME.FacebookMessageSenderName =. Just value]) senderName
             ++ maybe [] (\value -> [ME.FacebookMessageMetadata =. Just value]) meta
           )
      pure ()

verifyMetaWebhook
  :: ( MonadReader Env m
     , MonadError ServerError m
     )
  => MetaChannel
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> m Text
verifyMetaWebhook channel mMode mToken mChallenge = do
  Env{envConfig} <- ask
  either throwError pure $
    validateMetaWebhookVerifyRequest
      (metaChannelLabel channel)
      mMode
      mChallenge
      mToken
      (metaWebhookVerifyTokenCandidates channel envConfig)

metaWebhookVerifyTokenCandidates :: MetaChannel -> AppConfig -> [Maybe Text]
metaWebhookVerifyTokenCandidates channel cfg =
  case channel of
    MetaInstagram ->
      [ instagramVerifyToken cfg
      , instagramMessagingToken cfg
      , instagramAppToken cfg
      ]
    MetaFacebook ->
      [ facebookMessagingToken cfg
      ]

validateMetaWebhookVerifyRequest
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> [Maybe Text]
  -> Either ServerError Text
validateMetaWebhookVerifyRequest platformLabel mMode mChallenge mToken expectedCandidates =
  case validateConfiguredMetaVerifyToken expectedCandidates of
    Left err ->
      Left err
    Right expected ->
      case fmap T.toLower (nonBlankText mMode) of
        Nothing ->
          Left err400 { errBody = "hub.mode is required" }
        Just "subscribe" ->
          case nonBlankText mChallenge of
            Nothing ->
              Left err400 { errBody = "hub.challenge is required" }
            Just challenge ->
              case validateMetaHookChallenge challenge of
                Left err ->
                  Left err
                Right challengeVal ->
                  case nonBlankText mToken of
                    Nothing ->
                      Left err400 { errBody = "hub.verify_token is required" }
                    Just provided
                      | T.any isControl provided ->
                          Left err400
                            { errBody = "hub.verify_token must not contain control characters" }
                      | provided == expected -> Right challengeVal
                      | otherwise ->
                          Left err403
                            { errBody =
                                BL8.pack
                                  ("Meta verify token mismatch for " <> T.unpack platformLabel)
                            }
        Just _ ->
          Left err400 { errBody = "hub.mode must be subscribe" }
  where
    validateConfiguredMetaVerifyToken :: [Maybe Text] -> Either ServerError Text
    validateConfiguredMetaVerifyToken candidates =
      case listToMaybe (mapMaybe nonBlankText candidates) of
        Nothing ->
          Left err403 { errBody = "Meta verify token not configured" }
        Just expected
          | T.any isControl expected ->
              Left err403 { errBody = "Meta verify token is misconfigured" }
          | otherwise ->
              Right expected

    nonBlankText :: Maybe Text -> Maybe Text
    nonBlankText mTxt =
      case fmap T.strip mTxt of
        Just txt | not (T.null txt) -> Just txt
        _ -> Nothing

    validateMetaHookChallenge :: Text -> Either ServerError Text
    validateMetaHookChallenge challenge
      | T.length challenge > 512 =
          Left err400 { errBody = "hub.challenge must be 512 characters or fewer" }
      | T.any isControl challenge =
          Left err400 { errBody = "hub.challenge must not contain control characters" }
      | otherwise =
          Right challenge

instagramWebhookServer
  :: ( MonadIO m
     , MonadReader Env m
     , MonadError ServerError m
     )
  => ServerT IG.InstagramWebhookAPI m
instagramWebhookServer =
       verifyWebhook
  :<|> handleWebhook
  where
    verifyWebhook mMode mToken mChallenge =
      verifyMetaWebhook MetaInstagram mMode mToken mChallenge

    handleWebhook payload = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      channel <- either throwError pure (validateMetaWebhookChannel MetaInstagram payload)
      let incoming = extractMetaInbound payload
      liftIO $ do
        hPutStrLn stderr ("[" <> T.unpack (metaChannelLabel channel) <> "] received webhook payload")
        BL8.hPutStrLn stderr (A.encode payload)
        flip runSqlPool envPool (persistMetaInbound channel now incoming)
      pure NoContent

facebookWebhookServer
  :: ( MonadIO m
     , MonadReader Env m
     , MonadError ServerError m
     )
  => ServerT FB.FacebookWebhookAPI m
facebookWebhookServer =
       verifyWebhook
  :<|> handleWebhook
  where
    verifyWebhook mMode mToken mChallenge =
      verifyMetaWebhook MetaFacebook mMode mToken mChallenge

    handleWebhook payload = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      channel <- either throwError pure (validateMetaWebhookChannel MetaFacebook payload)
      let incoming = extractMetaInbound payload
      liftIO $ do
        hPutStrLn stderr ("[" <> T.unpack (metaChannelLabel channel) <> "] received webhook payload")
        BL8.hPutStrLn stderr (A.encode payload)
        flip runSqlPool envPool (persistMetaInbound channel now incoming)
      pure NoContent

instagramServer
  :: ( MonadIO m
     , MonadReader Env m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT IG.InstagramAPI m
instagramServer user =
       handleReply
  :<|> listMessages
  where
    ensureInboxAccess =
      unless (hasSocialInboxAccess user) $
        throwError err403 { errBody = "Missing required module access" }

    handleReply req = do
      ensureInboxAccess
      now <- liftIO getCurrentTime
      Env{..} <- ask
      recipient <- either throwError pure (validateSocialReplySenderId (IG.irSenderId req))
      mExternalId <- either throwError pure (validateSocialReplyExternalId (IG.irExternalId req))
      body <- either throwError pure (validateSocialReplyBody (IG.irMessage req))
      (mTargetAccountId, mTargetAccessToken) <-
        liftIO $
          flip runSqlPool envPool $
            resolveInstagramReplyContext mExternalId
      sendResult <- liftIO $ sendInstagramTextWithContext envConfig mTargetAccessToken mTargetAccountId recipient body
      let (replyStatusValue, replyErrorValue) = socialReplyOutcomeFields sendResult
      liftIO $ flip runSqlPool envPool $ do
        insert_ (M.InstagramMessage (recipient <> "-out-" <> T.pack (show now))
                  recipient
                  Nothing
                  (Just body)
                  "outgoing"
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  replyStatusValue
                  Nothing
                  Nothing
                  (Just now)
                  1
                  Nothing
                  Nothing
                  replyErrorValue
                  Nothing
                  now)
        for_ mExternalId $ \extId -> do
          let baseFilters =
                [ M.InstagramMessageExternalId ==. extId
                , M.InstagramMessageDirection ==. "incoming"
                , M.InstagramMessageRepliedAt ==. Nothing
                , M.InstagramMessageDeletedAt ==. Nothing
                ]
          case sendResult of
            Left err ->
              updateWhere baseFilters
                [ M.InstagramMessageReplyStatus =. replyStatusValue
                , M.InstagramMessageReplyError =. Just err
                ]
            Right _ ->
              updateWhere baseFilters
                [ M.InstagramMessageReplyStatus =. replyStatusValue
                , M.InstagramMessageRepliedAt =. Just now
                , M.InstagramMessageReplyText =. Just body
                , M.InstagramMessageReplyError =. Nothing
                ]
      case sendResult of
        Left err ->
          pure (object ["status" .= ("error" :: Text), "message" .= err])
        Right responseBody ->
          pure (object ["status" .= ("ok" :: Text), "message" .= ("sent" :: Text), "response" .= responseBody])


    listMessages mLimit mDirection mRepliedOnly = do
      ensureInboxAccess
      Env{..} <- ask
      limit <- either throwError pure (validateSocialLimit mLimit)
      direction <- parseSocialDirectionParam mDirection
      repliedOnly <- parseSocialBoolParam mRepliedOnly
      let filters =
            concat
              [ [M.InstagramMessageDeletedAt ==. Nothing]
              , maybe [] (\dir -> [M.InstagramMessageDirection ==. dir]) direction
              , if repliedOnly then [M.InstagramMessageRepliedAt !=. Nothing] else []
              ]
      rows <- liftIO $
        flip runSqlPool envPool $
          selectList filters [Desc M.InstagramMessageCreatedAt, LimitTo limit]
      let missing =
            [ (key, M.instagramMessageSenderId m)
            | Entity key m <- rows
            , isNothing (normalizeSenderLabelText (M.instagramMessageSenderName m) <|> extractSenderNameFromMetadata (M.instagramMessageMetadata m))
            ]
      resolved <- liftIO $ resolveMetaSenderLabels envConfig MetaInstagram (map snd missing)
      liftIO $ flip runSqlPool envPool $
        for_ missing $ \(key, sid) ->
          for_ (Map.lookup (T.strip sid) resolved) $ \label ->
            update key [M.InstagramMessageSenderName =. Just label]
      let toObj (Entity _ m) =
            let sid = M.instagramMessageSenderId m
                senderName =
                  normalizeSenderLabelText (M.instagramMessageSenderName m)
                    <|> extractSenderNameFromMetadata (M.instagramMessageMetadata m)
                    <|> Map.lookup (T.strip sid) resolved
            in object
            [ "externalId" .= M.instagramMessageExternalId m
            , "senderId"   .= M.instagramMessageSenderId m
            , "senderName" .= senderName
            , "text"       .= M.instagramMessageText m
            , "metadata"   .= M.instagramMessageMetadata m
            , "direction"  .= M.instagramMessageDirection m
            , "repliedAt"  .= M.instagramMessageRepliedAt m
            , "replyText"  .= M.instagramMessageReplyText m
            , "replyError" .= M.instagramMessageReplyError m
            , "createdAt"  .= M.instagramMessageCreatedAt m
            ]
      pure (A.toJSON (map toObj rows))

facebookServer
  :: ( MonadIO m
     , MonadReader Env m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT FB.FacebookAPI m
facebookServer user =
       handleReply
  :<|> listMessages
  where
    ensureInboxAccess =
      unless (hasSocialInboxAccess user) $
        throwError err403 { errBody = "Missing required module access" }

    handleReply req = do
      ensureInboxAccess
      now <- liftIO getCurrentTime
      Env{..} <- ask
      recipient <- either throwError pure (validateSocialReplySenderId (FB.frSenderId req))
      mExternalId <- either throwError pure (validateSocialReplyExternalId (FB.frExternalId req))
      body <- either throwError pure (validateSocialReplyBody (FB.frMessage req))
      sendResult <- liftIO $ sendFacebookText envConfig recipient body
      let (replyStatusValue, replyErrorValue) = socialReplyOutcomeFields sendResult
      liftIO $ flip runSqlPool envPool $ do
        insert_ (ME.FacebookMessage (recipient <> "-out-" <> T.pack (show now))
                  recipient
                  Nothing
                  (Just body)
                  "outgoing"
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  replyStatusValue
                  Nothing
                  Nothing
                  (Just now)
                  1
                  Nothing
                  Nothing
                  replyErrorValue
                  Nothing
                  now)
        for_ mExternalId $ \extId -> do
          let baseFilters =
                [ ME.FacebookMessageExternalId ==. extId
                , ME.FacebookMessageDirection ==. "incoming"
                , ME.FacebookMessageRepliedAt ==. Nothing
                , ME.FacebookMessageDeletedAt ==. Nothing
                ]
          case sendResult of
            Left err ->
              updateWhere baseFilters
                [ ME.FacebookMessageReplyStatus =. replyStatusValue
                , ME.FacebookMessageReplyError =. Just err
                ]
            Right _ ->
              updateWhere baseFilters
                [ ME.FacebookMessageReplyStatus =. replyStatusValue
                , ME.FacebookMessageRepliedAt =. Just now
                , ME.FacebookMessageReplyText =. Just body
                , ME.FacebookMessageReplyError =. Nothing
                ]
      case sendResult of
        Left err ->
          pure (object ["status" .= ("error" :: Text), "message" .= err])
        Right responseBody ->
          pure (object ["status" .= ("ok" :: Text), "message" .= ("sent" :: Text), "response" .= responseBody])

    listMessages mLimit mDirection mRepliedOnly = do
      ensureInboxAccess
      Env{..} <- ask
      limit <- either throwError pure (validateSocialLimit mLimit)
      direction <- parseSocialDirectionParam mDirection
      repliedOnly <- parseSocialBoolParam mRepliedOnly
      let filters =
            concat
              [ [ME.FacebookMessageDeletedAt ==. Nothing]
              , maybe [] (\dir -> [ME.FacebookMessageDirection ==. dir]) direction
              , if repliedOnly then [ME.FacebookMessageRepliedAt !=. Nothing] else []
              ]
      rows <- liftIO $
        flip runSqlPool envPool $
          selectList filters [Desc ME.FacebookMessageCreatedAt, LimitTo limit]
      let missing =
            [ (key, ME.facebookMessageSenderId m)
            | Entity key m <- rows
            , isNothing (normalizeSenderLabelText (ME.facebookMessageSenderName m) <|> extractSenderNameFromMetadata (ME.facebookMessageMetadata m))
            ]
      resolved <- liftIO $ resolveMetaSenderLabels envConfig MetaFacebook (map snd missing)
      liftIO $ flip runSqlPool envPool $
        for_ missing $ \(key, sid) ->
          for_ (Map.lookup (T.strip sid) resolved) $ \label ->
            update key [ME.FacebookMessageSenderName =. Just label]
      let toObj (Entity _ m) =
            let sid = ME.facebookMessageSenderId m
                senderName =
                  normalizeSenderLabelText (ME.facebookMessageSenderName m)
                    <|> extractSenderNameFromMetadata (ME.facebookMessageMetadata m)
                    <|> Map.lookup (T.strip sid) resolved
            in object
            [ "externalId" .= ME.facebookMessageExternalId m
            , "senderId"   .= ME.facebookMessageSenderId m
            , "senderName" .= senderName
            , "text"       .= ME.facebookMessageText m
            , "metadata"   .= ME.facebookMessageMetadata m
            , "direction"  .= ME.facebookMessageDirection m
            , "repliedAt"  .= ME.facebookMessageRepliedAt m
            , "replyText"  .= ME.facebookMessageReplyText m
            , "replyError" .= ME.facebookMessageReplyError m
            , "createdAt"  .= ME.facebookMessageCreatedAt m
            ]
      pure (A.toJSON (map toObj rows))

data MetaProfile = MetaProfile
  { mpUsername :: Maybe Text
  , mpName :: Maybe Text
  , mpFirstName :: Maybe Text
  , mpLastName :: Maybe Text
  } deriving (Show)

instance A.FromJSON MetaProfile where
  parseJSON = withObject "MetaProfile" $ \o -> do
    mpUsername <- o .:? "username"
    mpName <- o .:? "name"
    mpFirstName <- o .:? "first_name"
    mpLastName <- o .:? "last_name"
    pure MetaProfile{..}

stripNonEmptyText :: Maybe Text -> Maybe Text
stripNonEmptyText Nothing = Nothing
stripNonEmptyText (Just raw) =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

normalizeMetaWebhookActorId :: Text -> Maybe Text
normalizeMetaWebhookActorId rawActorId =
  stripNonEmptyText (Just rawActorId) >>= \actorId ->
    either (const Nothing) Just (validateSocialReplyIdentifier "senderId" actorId)

normalizeMetaWebhookExternalId :: Maybe Text -> Maybe Text
normalizeMetaWebhookExternalId rawExternalId =
  stripNonEmptyText rawExternalId >>= \externalId ->
    either (const Nothing) Just (validateSocialReplyIdentifier "externalId" externalId)

looksLikeOpaqueSenderLabel :: Text -> Bool
looksLikeOpaqueSenderLabel raw =
  let trimmed = T.strip raw
      hasOpaqueShape =
        T.length trimmed >= 48
          && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("#+/:=_-." :: String))) trimmed
      isVeryLongToken = T.length trimmed > 70 && not (T.any isSpace trimmed)
  in hasOpaqueShape || isVeryLongToken

normalizeSenderLabelText :: Maybe Text -> Maybe Text
normalizeSenderLabelText mRaw =
  stripNonEmptyText mRaw >>= \trimmed ->
    if looksLikeOpaqueSenderLabel trimmed
      then Nothing
      else Just trimmed

extractSenderNameFromMetadata :: Maybe Text -> Maybe Text
extractSenderNameFromMetadata Nothing = Nothing
extractSenderNameFromMetadata (Just raw) =
  case A.decodeStrict' (TE.encodeUtf8 raw) of
    Nothing -> Nothing
    Just payload -> parseMaybe parseSender payload
  where
    parseSender = withObject "MetaMessageMetadata" $ \o -> do
      mDirect <- o .:? "sender_name"
      case normalizeSenderLabelText mDirect of
        Just label -> pure label
        Nothing -> do
          mFrom <- o .:? "from"
          fromLabel <- case mFrom of
            Just (A.Object fromObj) -> do
              fromName <- fromObj .:? "name"
              fromUsername <- fromObj .:? "username"
              pure (normalizeSenderLabelText fromName <|> normalizeSenderLabelText fromUsername)
            _ -> pure Nothing
          case fromLabel of
            Just label -> pure label
            Nothing -> do
              mContacts <- (o .:? "contacts" :: Parser (Maybe [A.Value]))
              case mContacts of
                Just (firstContact : _) ->
                  case firstContact of
                    A.Object contactObj -> do
                      mProfile <- contactObj .:? "profile"
                      case mProfile of
                        Just (A.Object profileObj) -> do
                          profileName <- profileObj .:? "name"
                          case normalizeSenderLabelText profileName of
                            Just label -> pure label
                            Nothing -> fail "contact profile name missing"
                        _ -> fail "contact profile missing"
                    _ -> fail "contact invalid"
                _ -> fail "contacts missing"

extractRecipientIdFromMetadata :: Maybe Text -> Maybe Text
extractRecipientIdFromMetadata Nothing = Nothing
extractRecipientIdFromMetadata (Just raw) =
  case A.decodeStrict' (TE.encodeUtf8 raw) of
    Nothing -> Nothing
    Just payload -> parseMaybe parseRecipient payload >>= stripNonEmptyText . Just
  where
    parseRecipient = withObject "MetaMessageMetadata" $ \o -> do
      mRecipient <- o .:? "recipient_id"
      mSource <- o .:? "source_id"
      case stripNonEmptyText mRecipient <|> stripNonEmptyText mSource of
        Just recipientId -> pure recipientId
        Nothing -> fail "recipient id missing"

resolveInstagramReplyContext
  :: MonadIO m
  => Maybe Text
  -> SqlPersistT m (Maybe Text, Maybe Text)
resolveInstagramReplyContext mExternalId = do
  let mCleanExternalId = stripNonEmptyText mExternalId
  mPreferredAccountId <- case mCleanExternalId of
    Nothing -> pure Nothing
    Just extId -> do
      mIncoming <- selectFirst
        [ M.InstagramMessageExternalId ==. extId
        , M.InstagramMessageDirection ==. "incoming"
        ]
        [Desc M.InstagramMessageCreatedAt]
      pure (mIncoming >>= extractRecipientIdFromMetadata . M.instagramMessageMetadata . entityVal)
  resolveInstagramDeliveryAccount mPreferredAccountId

resolveInstagramDeliveryAccount
  :: MonadIO m
  => Maybe Text
  -> SqlPersistT m (Maybe Text, Maybe Text)
resolveInstagramDeliveryAccount mPreferredAccountId = do
  let baseFilters =
        [ M.SocialSyncAccountPlatform ==. "instagram"
        , M.SocialSyncAccountStatus ==. "connected"
        , M.SocialSyncAccountAccessToken !=. Nothing
        ]
      ordering = [Desc M.SocialSyncAccountUpdatedAt, Desc M.SocialSyncAccountCreatedAt]
      mCleanPreferred = stripNonEmptyText mPreferredAccountId
  mSelected <- case mCleanPreferred of
    Just accountId ->
      selectFirst ([M.SocialSyncAccountExternalUserId ==. accountId] ++ baseFilters) ordering
    Nothing ->
      selectFirst baseFilters ordering
  let selectedAccountId = mSelected >>= stripNonEmptyText . Just . M.socialSyncAccountExternalUserId . entityVal
      selectedToken = mSelected >>= stripNonEmptyText . M.socialSyncAccountAccessToken . entityVal
      accountId = selectedAccountId <|> mCleanPreferred
  pure (accountId, selectedToken)

metaProfileLabel :: MetaChannel -> MetaProfile -> Maybe Text
metaProfileLabel MetaInstagram MetaProfile{..} =
  normalizeSenderLabelText (mpUsername <|> mpName)
metaProfileLabel MetaFacebook MetaProfile{..} =
  let first = stripNonEmptyText mpFirstName
      lastN = stripNonEmptyText mpLastName
      fullName =
        case (first, lastN) of
          (Just f, Just l) -> Just (f <> " " <> l)
          (Just f, Nothing) -> Just f
          (Nothing, Just l) -> Just l
          _ -> Nothing
  in normalizeSenderLabelText fullName <|> normalizeSenderLabelText mpName

fetchMetaProfileLabel :: Manager -> Text -> Text -> MetaChannel -> Text -> IO (Maybe Text)
fetchMetaProfileLabel manager base token channel senderId = do
  let sid = T.strip senderId
  if T.null sid
    then pure Nothing
    else do
      let baseClean = T.dropWhileEnd (== '/') (T.strip base)
          encodedSid = TE.decodeUtf8 (urlEncode True (TE.encodeUtf8 sid))
          fields = case channel of
            MetaInstagram -> "username,name"
            MetaFacebook -> "name,first_name,last_name"
          urlTxt = baseClean <> "/" <> encodedSid <> "?fields=" <> fields
      reqE <- try (parseRequest (T.unpack urlTxt)) :: IO (Either SomeException Request)
      case reqE of
        Left _ -> pure Nothing
        Right req0 -> do
          let req = req0
                { method = "GET"
                , requestHeaders =
                    (hAuthorization, BS.pack ("Bearer " <> T.unpack token)) : requestHeaders req0
                }
          respE <- try (httpLbs req manager) :: IO (Either SomeException (Response BL.ByteString))
          case respE of
            Left _ -> pure Nothing
            Right resp -> do
              let st = statusCode (responseStatus resp)
              if st < 200 || st >= 300
                then pure Nothing
                else
                  case A.eitherDecode (responseBody resp) of
                    Left _ -> pure Nothing
                    Right profile -> pure (metaProfileLabel channel profile)

resolveMetaSenderLabels :: AppConfig -> MetaChannel -> [Text] -> IO (Map.Map Text Text)
resolveMetaSenderLabels cfg channel senderIds = do
  let mToken =
        case channel of
          MetaInstagram -> stripNonEmptyText (instagramMessagingToken cfg <|> instagramAppToken cfg)
          MetaFacebook -> stripNonEmptyText (facebookMessagingToken cfg)
      base = case channel of
        MetaInstagram -> instagramMessagingApiBase cfg
        MetaFacebook -> facebookMessagingApiBase cfg
  case mToken of
    Nothing -> pure Map.empty
    Just tok -> do
      manager <- newManager tlsManagerSettings
      let uniqueIds = take 25 (Set.toList (Set.fromList (map T.strip senderIds)))
      pairs <- mapM (\sid -> do
        mLabel <- fetchMetaProfileLabel manager base tok channel sid
        pure (sid, mLabel)
        ) uniqueIds
      pure $ Map.fromList [ (sid, label) | (sid, Just label) <- pairs, not (T.null (T.strip label)) ]

validateSocialLimit :: Maybe Int -> Either ServerError Int
validateSocialLimit Nothing = Right 100
validateSocialLimit (Just rawLimit)
  | rawLimit < 1 || rawLimit > 200 =
      Left err400 { errBody = "limit must be between 1 and 200" }
  | otherwise = Right rawLimit

validateSocialReplySenderId :: Text -> Either ServerError Text
validateSocialReplySenderId rawSenderId =
  case normalizeOptionalTextField (Just rawSenderId) of
    Nothing -> Left err400 { errBody = "senderId is required" }
    Just senderId -> validateSocialReplyIdentifier "senderId" senderId

validateSocialReplyExternalId :: Maybe Text -> Either ServerError (Maybe Text)
validateSocialReplyExternalId Nothing = Right Nothing
validateSocialReplyExternalId (Just rawExternalId) =
  case normalizeOptionalTextField (Just rawExternalId) of
    Nothing -> Left err400 { errBody = "externalId must be omitted or a non-empty string" }
    Just externalId -> Just <$> validateSocialReplyIdentifier "externalId" externalId

validateSocialReplyBody :: Text -> Either ServerError Text
validateSocialReplyBody rawBody
  | T.null body =
      Left err400 { errBody = "message is required" }
  | T.length body > maxSocialReplyBodyChars =
      Left err400 { errBody = "message must be 4096 characters or fewer" }
  | T.any isUnsupportedMessageControl body =
      Left err400 { errBody = "message must not contain unsupported control characters" }
  | otherwise =
      Right body
  where
    body = T.strip rawBody

maxSocialReplyBodyChars :: Int
maxSocialReplyBodyChars = 4096

isUnsupportedMessageControl :: Char -> Bool
isUnsupportedMessageControl ch =
  isControl ch && ch `notElem` ("\n\r\t" :: String)

validateSocialReplyIdentifier :: Text -> Text -> Either ServerError Text
validateSocialReplyIdentifier fieldName value
  | T.any isSpace value =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must not contain whitespace")) }
  | T.any isControl value =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must not contain control characters")) }
  | T.length value > 256 =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 (fieldName <> " must be 256 characters or fewer"))
        }
  | otherwise =
      Right value

socialReplyOutcomeFields :: Either Text a -> (Text, Maybe Text)
socialReplyOutcomeFields sendResult =
  case sendResult of
    Left err -> ("error", Just err)
    Right _ -> ("sent", Nothing)

parseSocialBoolParam :: MonadError ServerError m => Maybe Text -> m Bool
parseSocialBoolParam Nothing = pure False
parseSocialBoolParam (Just raw) =
  case T.toCaseFold (T.strip raw) of
    "true" -> pure True
    "1" -> pure True
    "yes" -> pure True
    "false" -> pure False
    "0" -> pure False
    "no" -> pure False
    "" -> invalidRepliedOnly
    _ -> invalidRepliedOnly
  where
    invalidRepliedOnly =
      throwError err400 { errBody = "repliedOnly must be omitted or one of: true, false, 1, 0, yes, no" }

parseSocialDirectionParam :: MonadError ServerError m => Maybe Text -> m (Maybe Text)
parseSocialDirectionParam Nothing = pure Nothing
parseSocialDirectionParam (Just raw) =
  case T.toCaseFold (T.strip raw) of
    "" -> invalidDirection
    "all" -> pure Nothing
    "incoming" -> pure (Just "incoming")
    "outgoing" -> pure (Just "outgoing")
    _ -> invalidDirection
  where
    invalidDirection =
      throwError err400 { errBody = "direction must be omitted or one of: all, incoming, outgoing" }

-- Shared helpers ----------------------------------------------------------

toBandDTO
  :: Map.Map (Key Party) (Entity Party)
  -> Entity Band
  -> [Entity BandMember]
  -> BandDTO
toBandDTO partyMap (Entity key band) members = BandDTO
  { bandId         = toPathPiece key
  , partyId        = fromSqlKey (bandPartyId band)
  , bName          = maybe (bandName band) (partyDisplayName . entityVal) (Map.lookup (bandPartyId band) partyMap)
  , bLabelArtist   = bandLabelArtist band
  , bPrimaryGenre  = bandPrimaryGenre band
  , bHomeCity      = bandHomeCity band
  , bPhotoUrl      = bandPhotoUrl band
  , bContractFlags = bandContractFlags band
  , bMembers       = map (toMemberDTO partyMap) members
  }
  where
    toMemberDTO pMap (Entity memberKey member) = BandMemberDTO
      { bmId         = toPathPiece memberKey
      , bmPartyId    = fromSqlKey (bandMemberPartyId member)
      , bmPartyName  = maybe
          (T.pack . show $ fromSqlKey (bandMemberPartyId member))
          (partyDisplayName . entityVal)
          (Map.lookup (bandMemberPartyId member) pMap)
      , bmRole       = bandMemberRoleInBand member
      }

loadBandForParty
  :: Key Party
  -> SqlPersistT IO (Maybe BandDTO)
loadBandForParty partyKey = do
  mBand <- selectFirst [BandPartyId ==. partyKey] []
  case mBand of
    Nothing -> pure Nothing
    Just bandEnt -> do
      let bandKey = entityKey bandEnt
      members <- selectList [BandMemberBandId ==. bandKey] [Asc BandMemberId]
      let requiredPartyIds = Set.toList . Set.fromList
            $ bandPartyId (entityVal bandEnt)
            : map (bandMemberPartyId . entityVal) members
      partyList <- if null requiredPartyIds
        then pure []
        else selectList [M.PartyId <-. requiredPartyIds] []
      let partyMap = Map.fromList [ (entityKey p, p) | p <- partyList ]
      pure (Just (toBandDTO partyMap bandEnt members))
