{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.Server.ServiceStorefront
  ( serviceStorefrontPublicServer
  , serviceStorefrontAdminServer
  ) where

import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Data.Aeson (eitherDecode, FromJSON(..), Value(..), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe, catMaybes)
import           Control.Applicative ((<|>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (getCurrentTime, UTCTime)
import           Data.UUID (UUID, toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (selectList, get, insert, getBy, replace, update, Entity(..), (==.), (=.), SelectOpt(..))
import           Database.Persist.Sql (runSqlPool, SqlBackend)
import           Network.HTTP.Client (httpLbs, parseRequest, responseBody, responseStatus, method, requestBody, requestHeaders, Request(..), RequestBody(..), Manager)
import           Network.HTTP.Client.TLS (tlsManagerSettings, newTlsManager)
import           Network.HTTP.Types (statusCode)
import           Servant
import           System.Environment (lookupEnv)
import           Web.PathPieces (fromPathPiece, toPathPiece)

import           TDF.API.ServiceStorefront (ServiceStorefrontPublicAPI, ServiceStorefrontAdminAPI)
import           TDF.API.ServiceStorefrontTypes
import           TDF.API.Types (DatafastCheckoutDTO(..), PaypalCreateDTO(..), PaypalCaptureReq(..))
import           TDF.Config (defaultCurrency, defaultLocale, supportedCurrencies)
import           TDF.DB (Env(..))
import           TDF.Internationalization (formatMinorUnitsDecimal, formatMoney, normalizeCurrencyCode)
import qualified TDF.ModelsExtra as ME
import           TDF.DTO.SocialEventsDTO (StripePaymentIntentDTO(..))

type AppM = ReaderT Env Handler

-- | Public server for the service storefront.
serviceStorefrontPublicServer :: ServerT ServiceStorefrontPublicAPI AppM
serviceStorefrontPublicServer =
       listPackagesHandler
  :<|> getPackageHandler
  :<|> createOrderHandler
  :<|> getOrderHandler
  :<|> createStripePaymentIntentHandler
  :<|> createDatafastCheckoutHandler
  :<|> confirmDatafastStatusHandler
  :<|> createPaypalOrderHandler
  :<|> capturePaypalHandler
  :<|> createRevisionHandler

-- | Admin server for the service storefront.
serviceStorefrontAdminServer :: ServerT ServiceStorefrontAdminAPI AppM
serviceStorefrontAdminServer =
       listOrdersAdminHandler
  :<|> updateOrderAdminHandler
  :<|> listPackagesAdminHandler
  :<|> createPackageAdminHandler
  :<|> updatePackageAdminHandler

-- ============================================================================
-- Public Handlers
-- ============================================================================

listPackagesHandler :: AppM [ServiceStorefrontPackageDTO]
listPackagesHandler = do
  Env{..} <- ask
  packages <- liftIO $ flip runSqlPool envPool $
    selectList [ME.ServiceStorefrontPackageActive ==. True]
               [Asc ME.ServiceStorefrontPackageSortOrder]
  pure (map packageEntityToDTO packages)

getPackageHandler :: Text -> AppM ServiceStorefrontPackageDTO
getPackageHandler packageIdText = do
  Env{..} <- ask
  packageId <- parsePackageId packageIdText
  mPackage <- liftIO $ flip runSqlPool envPool $ get packageId
  case mPackage of
    Nothing -> throwError err404 { errBody = "Package not found" }
    Just pkg -> pure (packageEntityToDTO (Entity packageId pkg))

createOrderHandler :: ServiceStorefrontOrderCreate -> AppM ServiceStorefrontOrderDTO
createOrderHandler ServiceStorefrontOrderCreate{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  
  -- Validate required fields
  let buyerName = T.strip ssocBuyerName
  let buyerEmail = T.strip ssocBuyerEmail
  when (T.null buyerName) $
    throwError err400 { errBody = "Buyer name is required" }
  when (T.null buyerEmail) $
    throwError err400 { errBody = "Buyer email is required" }
  when (T.length buyerName > 200) $
    throwError err400 { errBody = "Buyer name too long (max 200 characters)" }
  when (T.length buyerEmail > 200) $
    throwError err400 { errBody = "Buyer email too long (max 200 characters)" }
  
  -- Parse package ID
  packageId <- parsePackageId ssocPackageId
  
  -- Load package
  mPackage <- liftIO $ flip runSqlPool envPool $ get packageId
  case mPackage of
    Nothing -> throwError err404 { errBody = "Package not found" }
    Just pkg -> do
      -- Generate order number
      orderId <- liftIO nextRandom
      let orderNumber = generateOrderNumber orderId
      
      -- Insert order
      let order = ME.ServiceStorefrontOrder
            { ME.serviceStorefrontOrderOrderNumber = orderNumber
            , ME.serviceStorefrontOrderBuyerName = buyerName
            , ME.serviceStorefrontOrderBuyerEmail = buyerEmail
            , ME.serviceStorefrontOrderBuyerPhone = fmap T.strip ssocBuyerPhone
            , ME.serviceStorefrontOrderArtistName = fmap T.strip ssocArtistName
            , ME.serviceStorefrontOrderPackageId = packageId
            , ME.serviceStorefrontOrderServiceKind = ME.serviceStorefrontPackageServiceKind pkg
            , ME.serviceStorefrontOrderTier = ME.serviceStorefrontPackageTier pkg
            , ME.serviceStorefrontOrderPriceUsdCents = ME.serviceStorefrontPackagePriceUsdCents pkg
            , ME.serviceStorefrontOrderCurrency = ME.serviceStorefrontPackageCurrency pkg
            , ME.serviceStorefrontOrderStatus = "pending_payment"
            , ME.serviceStorefrontOrderPaymentProvider = Nothing
            , ME.serviceStorefrontOrderStripePaymentIntentId = Nothing
            , ME.serviceStorefrontOrderStripeIdempotencyKey = Nothing
            , ME.serviceStorefrontOrderDatafastCheckoutId = Nothing
            , ME.serviceStorefrontOrderDatafastResourcePath = Nothing
            , ME.serviceStorefrontOrderDatafastPaymentId = Nothing
            , ME.serviceStorefrontOrderPaypalOrderId = Nothing
            , ME.serviceStorefrontOrderPaypalPayerEmail = Nothing
            , ME.serviceStorefrontOrderPaidAt = Nothing
            , ME.serviceStorefrontOrderGenre = fmap T.strip ssocGenre
            , ME.serviceStorefrontOrderSongCount = fromMaybe 1 ssocSongCount
            , ME.serviceStorefrontOrderNotes = fmap T.strip ssocNotes
            , ME.serviceStorefrontOrderReferenceTrackUrl = fmap T.strip ssocReferenceTrackUrl
            , ME.serviceStorefrontOrderDeadline = Nothing
            , ME.serviceStorefrontOrderSourceFilesUrl = Nothing
            , ME.serviceStorefrontOrderDeliverablesUrl = Nothing
            , ME.serviceStorefrontOrderPipelineCardId = Nothing
            , ME.serviceStorefrontOrderCreatedAt = now
            , ME.serviceStorefrontOrderUpdatedAt = now
            }
      
      orderIdKey <- liftIO $ flip runSqlPool envPool $ insert order
      
      -- Insert status change
      let statusChange = ME.ServiceStorefrontOrderStatusChange
            { ME.serviceStorefrontOrderStatusChangeOrderId = orderIdKey
            , ME.serviceStorefrontOrderStatusChangeStatus = "pending_payment"
            , ME.serviceStorefrontOrderStatusChangeNotes = Just "Order created"
            , ME.serviceStorefrontOrderStatusChangeChangedBy = Just "system"
            , ME.serviceStorefrontOrderStatusChangeCreatedAt = now
            }
      _ <- liftIO $ flip runSqlPool envPool $ insert statusChange
      
      -- Return order DTO
      pure (orderToDTO orderIdKey order)

getOrderHandler :: Text -> AppM ServiceStorefrontOrderDTO
getOrderHandler orderIdText = do
  Env{..} <- ask
  -- Try by order number first (more user-friendly)
  mOrder <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mOrder of
    Just (Entity oid order) -> pure (orderToDTO oid order)
    Nothing -> throwError err404 { errBody = "Order not found" }

-- Payment handlers (stubs - need provider credentials)

createStripePaymentIntentHandler :: Text -> AppM StripePaymentIntentDTO
createStripePaymentIntentHandler _ =
  throwError err501
    { errBody = "Stripe checkout is not configured for the service storefront. Use an enabled payment provider." }

createDatafastCheckoutHandler :: Text -> AppM DatafastCheckoutDTO
createDatafastCheckoutHandler orderIdText = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  
  -- Load order by order number
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      -- Verify order is in a payable state
      let status = ME.serviceStorefrontOrderStatus order
      when (status `notElem` ["pending_payment", "payment_failed"]) $
        throwError err400 { errBody = "Order is not in a payable state" }
      
      let totalCents = ME.serviceStorefrontOrderPriceUsdCents order
          currency = ME.serviceStorefrontOrderCurrency order
          buyerName = ME.serviceStorefrontOrderBuyerName order
          buyerEmail = ME.serviceStorefrontOrderBuyerEmail order
          buyerPhone = ME.serviceStorefrontOrderBuyerPhone order
      
      -- Request Datafast checkout
      (checkoutId, widgetUrl) <- requestDatafastCheckoutForService
        (toPathPiece oid) totalCents currency buyerName buyerEmail buyerPhone
      
      -- Update order with Datafast info
      liftIO $ flip runSqlPool envPool $ update oid
        [ ME.ServiceStorefrontOrderStatus =. "datafast_pending"
        , ME.ServiceStorefrontOrderPaymentProvider =. Just "datafast"
        , ME.ServiceStorefrontOrderDatafastCheckoutId =. Just checkoutId
        , ME.ServiceStorefrontOrderUpdatedAt =. now
        ]
      
      pure DatafastCheckoutDTO
        { dcOrderId    = toPathPiece oid
        , dcCheckoutId = checkoutId
        , dcWidgetUrl  = T.pack widgetUrl
        , dcAmount     = formatMoney (defaultLocale envConfig) currency (fromIntegral totalCents)
        , dcCurrency   = currency
        }

confirmDatafastStatusHandler :: Maybe Text -> Maybe Text -> AppM ServiceStorefrontOrderDTO
confirmDatafastStatusHandler mOrderId mResourcePath = do
  Env{..} <- ask
  orderIdText <- maybe (throwError err400 { errBody = "orderId requerido" }) pure mOrderId
  resourcePathTxt <- maybe (throwError err400 { errBody = "resourcePath requerido" }) pure mResourcePath
  
  -- Load order
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      -- Check payment status with Datafast
      paymentStatus <- checkDatafastPaymentStatus resourcePathTxt
      
      now <- liftIO getCurrentTime
      let (newStatus, paidAt) = case paymentStatus of
            "completed" -> ("paid", Just now)
            "pending"   -> ("datafast_pending", Nothing)
            _           -> ("payment_failed", Nothing)
      
      -- Update order
      liftIO $ flip runSqlPool envPool $ update oid
        [ ME.ServiceStorefrontOrderStatus =. newStatus
        , ME.ServiceStorefrontOrderDatafastResourcePath =. Just resourcePathTxt
        , ME.ServiceStorefrontOrderDatafastPaymentId =. Just resourcePathTxt
        , ME.ServiceStorefrontOrderPaidAt =. paidAt
        , ME.ServiceStorefrontOrderUpdatedAt =. now
        ]
      
      -- Insert status change
      let statusChange = ME.ServiceStorefrontOrderStatusChange
            { ME.serviceStorefrontOrderStatusChangeOrderId = oid
            , ME.serviceStorefrontOrderStatusChangeStatus = newStatus
            , ME.serviceStorefrontOrderStatusChangeNotes = Just $ "Datafast payment status: " <> paymentStatus
            , ME.serviceStorefrontOrderStatusChangeChangedBy = Just "datafast_webhook"
            , ME.serviceStorefrontOrderStatusChangeCreatedAt = now
            }
      _ <- liftIO $ flip runSqlPool envPool $ insert statusChange
      
      -- Return updated order
      mUpdated <- liftIO $ flip runSqlPool envPool $ get oid
      case mUpdated of
        Nothing -> throwError err500 { errBody = "Failed to load updated order" }
        Just updated -> pure (orderToDTO oid updated)

createPaypalOrderHandler :: Text -> AppM PaypalCreateDTO
createPaypalOrderHandler orderIdText = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  
  -- Load order by order number
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      -- Verify order is in a payable state
      let status = ME.serviceStorefrontOrderStatus order
      when (status `notElem` ["pending_payment", "payment_failed"]) $
        throwError err400 { errBody = "Order is not in a payable state" }
      
      let totalCents = ME.serviceStorefrontOrderPriceUsdCents order
          currency = ME.serviceStorefrontOrderCurrency order
          buyerName = ME.serviceStorefrontOrderBuyerName order
          buyerEmail = ME.serviceStorefrontOrderBuyerEmail order
      
      -- Load PayPal env
      (cid, sec, baseUrl) <- loadPaypalEnvForService
      manager <- liftIO newTlsManager
      
      -- Create PayPal order remotely
      (ppOrderId, approvalUrl) <- createPaypalOrderRemoteForService
        manager cid sec baseUrl totalCents currency buyerName buyerEmail
      
      -- Update order with PayPal info
      liftIO $ flip runSqlPool envPool $ update oid
        [ ME.ServiceStorefrontOrderStatus =. "paypal_pending"
        , ME.ServiceStorefrontOrderPaymentProvider =. Just "paypal"
        , ME.ServiceStorefrontOrderPaypalOrderId =. Just ppOrderId
        , ME.ServiceStorefrontOrderUpdatedAt =. now
        ]
      
      pure PaypalCreateDTO
        { pcOrderId = toPathPiece oid
        , pcPaypalOrderId = ppOrderId
        , pcApprovalUrl = approvalUrl
        }

capturePaypalHandler :: PaypalCaptureReq -> AppM ServiceStorefrontOrderDTO
capturePaypalHandler PaypalCaptureReq{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  
  -- Load order by order number
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber pcCaptureOrderId)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      -- Verify PayPal order ID matches
      case ME.serviceStorefrontOrderPaypalOrderId order of
        Nothing -> throwError err400 { errBody = "Order has no PayPal order" }
        Just storedPpOrderId | storedPpOrderId /= pcCapturePaypalId ->
          throwError err400 { errBody = "PayPal order ID mismatch" }
        _ -> pure ()
      
      -- Load PayPal env
      (cid, sec, baseUrl) <- loadPaypalEnvForService
      manager <- liftIO newTlsManager
      
      -- Capture PayPal order
      (captureStatus, payerEmail) <- capturePaypalOrderRemoteForService
        manager cid sec baseUrl pcCapturePaypalId
      
      -- Determine next status
      let nextStatus = case captureStatus of
            "COMPLETED" -> "paid"
            "APPROVED"  -> "paypal_pending"
            _           -> "payment_failed"
          paidAt = if nextStatus == "paid" then Just now else Nothing
      
      -- Update order
      liftIO $ flip runSqlPool envPool $ update oid
        [ ME.ServiceStorefrontOrderStatus =. nextStatus
        , ME.ServiceStorefrontOrderPaypalPayerEmail =. (payerEmail <|> ME.serviceStorefrontOrderPaypalPayerEmail order)
        , ME.ServiceStorefrontOrderPaidAt =. paidAt
        , ME.ServiceStorefrontOrderUpdatedAt =. now
        ]
      
      -- Insert status change
      let statusChange = ME.ServiceStorefrontOrderStatusChange
            { ME.serviceStorefrontOrderStatusChangeOrderId = oid
            , ME.serviceStorefrontOrderStatusChangeStatus = nextStatus
            , ME.serviceStorefrontOrderStatusChangeNotes = Just $ "PayPal capture: " <> captureStatus
            , ME.serviceStorefrontOrderStatusChangeChangedBy = Just "paypal_capture"
            , ME.serviceStorefrontOrderStatusChangeCreatedAt = now
            }
      _ <- liftIO $ flip runSqlPool envPool $ insert statusChange
      
      -- Return updated order
      mUpdated <- liftIO $ flip runSqlPool envPool $ get oid
      case mUpdated of
        Nothing -> throwError err500 { errBody = "Failed to load updated order" }
        Just updated -> pure (orderToDTO oid updated)

createRevisionHandler :: Text -> ServiceStorefrontRevisionCreate -> AppM ServiceStorefrontRevisionDTO
createRevisionHandler _ _ =
  throwError err501 { errBody = "Revision system not yet implemented." }

-- ============================================================================
-- Admin Handlers
-- ============================================================================

listOrdersAdminHandler :: Maybe Text -> Maybe Int -> Maybe Int -> AppM [ServiceStorefrontOrderDTO]
listOrdersAdminHandler mStatus mLimit mOffset = do
  Env{..} <- ask
  let lim = fromMaybe 50 mLimit
  let off = fromMaybe 0 mOffset
  let filters = case mStatus of
        Nothing -> []
        Just status -> [ME.ServiceStorefrontOrderStatus ==. status]
  orders <- liftIO $ flip runSqlPool envPool $
    selectList filters [Desc ME.ServiceStorefrontOrderCreatedAt, LimitTo lim, OffsetBy off]
  pure (map (\(Entity oid o) -> orderToDTO oid o) orders)

updateOrderAdminHandler :: Text -> ServiceStorefrontOrderUpdate -> AppM ServiceStorefrontOrderDTO
updateOrderAdminHandler orderIdText ServiceStorefrontOrderUpdate{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  -- Lookup by order number
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      let updatedOrder = order
            { ME.serviceStorefrontOrderStatus = fromMaybe (ME.serviceStorefrontOrderStatus order) ssouStatus
            , ME.serviceStorefrontOrderDeliverablesUrl = maybe (ME.serviceStorefrontOrderDeliverablesUrl order) Just ssouDeliverablesUrl
            , ME.serviceStorefrontOrderNotes = maybe (ME.serviceStorefrontOrderNotes order) (Just . T.strip) ssouNotes
            , ME.serviceStorefrontOrderUpdatedAt = now
            }
      liftIO $ flip runSqlPool envPool $ do
        -- Insert status change if status changed
        case ssouStatus of
          Just newStatus -> do
            let statusChange = ME.ServiceStorefrontOrderStatusChange
                  { ME.serviceStorefrontOrderStatusChangeOrderId = oid
                  , ME.serviceStorefrontOrderStatusChangeStatus = newStatus
                  , ME.serviceStorefrontOrderStatusChangeNotes = Just "Status updated by admin"
                  , ME.serviceStorefrontOrderStatusChangeChangedBy = Just "admin"
                  , ME.serviceStorefrontOrderStatusChangeCreatedAt = now
                  }
            _ <- insert statusChange
            pure ()
          Nothing -> pure ()
        replace oid updatedOrder
      pure (orderToDTO oid updatedOrder)

listPackagesAdminHandler :: AppM [ServiceStorefrontPackageDTO]
listPackagesAdminHandler = do
  Env{..} <- ask
  packages <- liftIO $ flip runSqlPool envPool $
    selectList [] [Asc ME.ServiceStorefrontPackageSortOrder]
  pure (map packageEntityToDTO packages)

createPackageAdminHandler :: ServiceStorefrontPackageCreate -> AppM ServiceStorefrontPackageDTO
createPackageAdminHandler ServiceStorefrontPackageCreate{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  currency <-
    case normalizeCurrencyCode (fromMaybe (defaultCurrency envConfig) sspcCurrency) of
      Just value | value `elem` supportedCurrencies envConfig -> pure value
      _ -> throwError err400 { errBody = "Currency is not enabled by SUPPORTED_CURRENCIES" }
  let pkg = ME.ServiceStorefrontPackage
        { ME.serviceStorefrontPackageServiceKind = sspcServiceKind
        , ME.serviceStorefrontPackageTier = sspcTier
        , ME.serviceStorefrontPackageName = sspcName
        , ME.serviceStorefrontPackageDescription = sspcDescription
        , ME.serviceStorefrontPackagePriceUsdCents = sspcPriceUsdCents
        , ME.serviceStorefrontPackageCurrency = currency
        , ME.serviceStorefrontPackageTurnaroundDays = fromMaybe 7 sspcTurnaroundDays
        , ME.serviceStorefrontPackageRevisionCount = fromMaybe 2 sspcRevisionCount
        , ME.serviceStorefrontPackageDeliverables = Nothing -- TODO: JSON encode
        , ME.serviceStorefrontPackageFeatures = Nothing -- TODO: JSON encode
        , ME.serviceStorefrontPackageActive = True
        , ME.serviceStorefrontPackageSortOrder = fromMaybe 0 sspcSortOrder
        , ME.serviceStorefrontPackageCreatedAt = now
        , ME.serviceStorefrontPackageUpdatedAt = now
        }
  pkgId <- liftIO $ flip runSqlPool envPool $ insert pkg
  mPkg <- liftIO $ flip runSqlPool envPool $ get pkgId
  case mPkg of
    Nothing -> throwError err500 { errBody = "Failed to create package" }
    Just p -> pure (packageEntityToDTO (Entity pkgId p))

updatePackageAdminHandler :: Text -> ServiceStorefrontPackageUpdate -> AppM ServiceStorefrontPackageDTO
updatePackageAdminHandler packageIdText ServiceStorefrontPackageUpdate{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  packageId <- parsePackageId packageIdText
  mPkg <- liftIO $ flip runSqlPool envPool $ get packageId
  case mPkg of
    Nothing -> throwError err404 { errBody = "Package not found" }
    Just pkg -> do
      let updatedPkg = pkg
            { ME.serviceStorefrontPackageName = fromMaybe (ME.serviceStorefrontPackageName pkg) sspuName
            , ME.serviceStorefrontPackageDescription = maybe (ME.serviceStorefrontPackageDescription pkg) Just sspuDescription
            , ME.serviceStorefrontPackagePriceUsdCents = fromMaybe (ME.serviceStorefrontPackagePriceUsdCents pkg) sspuPriceUsdCents
            , ME.serviceStorefrontPackageTurnaroundDays = fromMaybe (ME.serviceStorefrontPackageTurnaroundDays pkg) sspuTurnaroundDays
            , ME.serviceStorefrontPackageRevisionCount = fromMaybe (ME.serviceStorefrontPackageRevisionCount pkg) sspuRevisionCount
            , ME.serviceStorefrontPackageActive = fromMaybe (ME.serviceStorefrontPackageActive pkg) sspuActive
            , ME.serviceStorefrontPackageSortOrder = fromMaybe (ME.serviceStorefrontPackageSortOrder pkg) sspuSortOrder
            , ME.serviceStorefrontPackageUpdatedAt = now
            }
      liftIO $ flip runSqlPool envPool $ replace packageId updatedPkg
      pure (packageEntityToDTO (Entity packageId updatedPkg))

-- ============================================================================
-- Helpers
-- ============================================================================

packageEntityToDTO :: Entity ME.ServiceStorefrontPackage -> ServiceStorefrontPackageDTO
packageEntityToDTO (Entity pid pkg) = ServiceStorefrontPackageDTO
  { sspId = toPathPiece pid
  , sspServiceKind = ME.serviceStorefrontPackageServiceKind pkg
  , sspTier = ME.serviceStorefrontPackageTier pkg
  , sspName = ME.serviceStorefrontPackageName pkg
  , sspDescription = ME.serviceStorefrontPackageDescription pkg
  , sspPriceUsdCents = ME.serviceStorefrontPackagePriceUsdCents pkg
  , sspCurrency = ME.serviceStorefrontPackageCurrency pkg
  , sspTurnaroundDays = ME.serviceStorefrontPackageTurnaroundDays pkg
  , sspRevisionCount = ME.serviceStorefrontPackageRevisionCount pkg
  , sspDeliverables = Nothing -- TODO: JSON decode
  , sspFeatures = Nothing -- TODO: JSON decode
  , sspActive = ME.serviceStorefrontPackageActive pkg
  , sspSortOrder = ME.serviceStorefrontPackageSortOrder pkg
  }

orderToDTO :: ME.ServiceStorefrontOrderId -> ME.ServiceStorefrontOrder -> ServiceStorefrontOrderDTO
orderToDTO oid order = ServiceStorefrontOrderDTO
  { ssoId = toPathPiece oid
  , ssoOrderNumber = ME.serviceStorefrontOrderOrderNumber order
  , ssoBuyerName = ME.serviceStorefrontOrderBuyerName order
  , ssoBuyerEmail = ME.serviceStorefrontOrderBuyerEmail order
  , ssoBuyerPhone = ME.serviceStorefrontOrderBuyerPhone order
  , ssoArtistName = ME.serviceStorefrontOrderArtistName order
  , ssoPackageId = toPathPiece (ME.serviceStorefrontOrderPackageId order)
  , ssoServiceKind = ME.serviceStorefrontOrderServiceKind order
  , ssoTier = ME.serviceStorefrontOrderTier order
  , ssoPriceUsdCents = ME.serviceStorefrontOrderPriceUsdCents order
  , ssoCurrency = ME.serviceStorefrontOrderCurrency order
  , ssoStatus = ME.serviceStorefrontOrderStatus order
  , ssoPaymentProvider = ME.serviceStorefrontOrderPaymentProvider order
  , ssoPaidAt = ME.serviceStorefrontOrderPaidAt order
  , ssoGenre = ME.serviceStorefrontOrderGenre order
  , ssoSongCount = ME.serviceStorefrontOrderSongCount order
  , ssoNotes = ME.serviceStorefrontOrderNotes order
  , ssoReferenceTrackUrl = ME.serviceStorefrontOrderReferenceTrackUrl order
  , ssoDeadline = ME.serviceStorefrontOrderDeadline order
  , ssoDeliverablesUrl = ME.serviceStorefrontOrderDeliverablesUrl order
  , ssoCreatedAt = ME.serviceStorefrontOrderCreatedAt order
  , ssoUpdatedAt = ME.serviceStorefrontOrderUpdatedAt order
  }

-- | Generate a human-readable order number from a UUID.
generateOrderNumber :: UUID -> Text
generateOrderNumber uuid =
  let uuidText = T.replace "-" "" (toText uuid)
  in "TDF-" <> T.take 8 uuidText

-- | Parse a package ID from text.
parsePackageId :: Text -> AppM ME.ServiceStorefrontPackageId
parsePackageId txt = case fromPathPiece (T.strip txt) of
  Nothing -> throwError err400 { errBody = "Invalid package ID format" }
  Just key -> pure key

-- ============================================================================
-- Datafast Integration
-- ============================================================================

-- | Datafast environment configuration.
data ServiceDatafastEnv = ServiceDatafastEnv
  { sdfEntityId    :: Text
  , sdfBearerToken :: Text
  , sdfBaseUrl     :: String
  , sdfTestMode    :: Maybe Text
  } deriving (Show)

-- | Load Datafast environment from env vars.
loadServiceDatafastEnv :: AppM ServiceDatafastEnv
loadServiceDatafastEnv = do
  mEntity <- liftIO $ lookupEnv "DATAFAST_ENTITY_ID"
  mBearer <- liftIO $ lookupEnv "DATAFAST_BEARER_TOKEN"
  mBase   <- liftIO $ lookupEnv "DATAFAST_BASE_URL"
  mTest   <- liftIO $ lookupEnv "DATAFAST_TEST_MODE"
  entityId <- maybe (throwError err500 { errBody = "DATAFAST_ENTITY_ID not set" }) (pure . T.pack) mEntity
  bearer   <- maybe (throwError err500 { errBody = "DATAFAST_BEARER_TOKEN not set" }) (pure . T.pack) mBearer
  baseUrl  <- maybe (throwError err500 { errBody = "DATAFAST_BASE_URL not set" }) (pure) mBase
  let testMode = T.pack <$> mTest
  pure ServiceDatafastEnv
    { sdfEntityId = entityId
    , sdfBearerToken = bearer
    , sdfBaseUrl = baseUrl
    , sdfTestMode = testMode
    }

-- | Request a Datafast checkout session for a service order.
requestDatafastCheckoutForService
  :: Text  -- ^ Transaction ID (order UUID)
  -> Int   -- ^ Amount in cents
  -> Text  -- ^ Currency
  -> Text  -- ^ Buyer name
  -> Text  -- ^ Buyer email
  -> Maybe Text  -- ^ Buyer phone
  -> AppM (Text, String)  -- ^ (checkoutId, widgetUrl)
requestDatafastCheckoutForService txnId totalCents currency name email mPhone = do
  dfEnv <- loadServiceDatafastEnv
  manager <- liftIO $ newTlsManager
  let amountTxt = T.pack $ show (totalCents `div` 100) <> "." <> pad2 (totalCents `mod` 100)
      currencyTxt = T.toUpper (T.strip currency)
      (givenName, surname) = splitBuyerName name
      -- Build form body
      baseParams =
        [ ("entityId", TE.encodeUtf8 (sdfEntityId dfEnv))
        , ("amount", TE.encodeUtf8 amountTxt)
        , ("currency", TE.encodeUtf8 currencyTxt)
        , ("paymentType", "DB")
        , ("merchantTransactionId", TE.encodeUtf8 txnId)
        , ("customer.givenName", TE.encodeUtf8 givenName)
        , ("customer.surname", TE.encodeUtf8 surname)
        , ("customer.email", TE.encodeUtf8 email)
        ]
      phoneParam = maybe [] (\p -> [("customer.phone", TE.encodeUtf8 p)]) mPhone
      testModeParam = maybe [] (\tm -> [("testMode", TE.encodeUtf8 tm)]) (sdfTestMode dfEnv)
      allParams = baseParams <> phoneParam <> testModeParam
      body = renderFormBody allParams
      baseUrlClean = stripTrailingSlash (sdfBaseUrl dfEnv)
  req0 <- liftIO $ parseRequest (baseUrlClean ++ "/v1/checkouts")
  let req = req0
        { method = "POST"
        , requestBody = RequestBodyBS body
        , requestHeaders =
            [ ("Authorization", "Bearer " <> TE.encodeUtf8 (sdfBearerToken dfEnv))
            , ("Content-Type", "application/x-www-form-urlencoded")
            ]
        }
  resp <- liftIO $ httpLbs req manager
  when (statusCode (responseStatus resp) >= 400) $
    throwError err502 { errBody = "Datafast checkout request failed." }
  case eitherDecode (responseBody resp) of
    Left err -> throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid Datafast response: " <> T.pack err)) }
    Right dfResp -> do
      let mCheckoutId = extractCheckoutId dfResp
          mResultCode = extractResultCode dfResp
      case mResultCode of
        Just code | not (isSuccessCode code) ->
          throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 ("Datafast rejected checkout: " <> code)) }
        _ -> pure ()
      checkoutId <- maybe (throwError err502 { errBody = "No checkout ID in response" }) pure mCheckoutId
      let widgetUrl = baseUrlClean ++ "/v1/paymentWidgets.js?checkoutId=" ++ T.unpack checkoutId
      pure (checkoutId, widgetUrl)
  where
    pad2 n = if n < 10 then "0" <> show n else show n

-- | Check Datafast payment status.
checkDatafastPaymentStatus :: Text -> AppM Text
checkDatafastPaymentStatus resourcePath = do
  dfEnv <- loadServiceDatafastEnv
  manager <- liftIO $ newTlsManager
  let baseUrlClean = stripTrailingSlash (sdfBaseUrl dfEnv)
      rp = T.unpack resourcePath
      basePath = baseUrlClean ++ rp
      sep = if '?' `elem` basePath then "&" else "?"
      fullUrl = basePath ++ sep ++ "entityId=" ++ T.unpack (sdfEntityId dfEnv)
  req0 <- liftIO $ parseRequest fullUrl
  let req = req0
        { method = "GET"
        , requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 (sdfBearerToken dfEnv))]
        }
  resp <- liftIO $ httpLbs req manager
  when (statusCode (responseStatus resp) >= 400) $
    throwError err502 { errBody = "Datafast status check failed." }
  case eitherDecode (responseBody resp) of
    Left err -> throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid Datafast status response: " <> T.pack err)) }
    Right dfResp -> do
      let mStatus = extractPaymentStatus dfResp
      pure (fromMaybe "unknown" mStatus)

-- | Extract checkout ID from Datafast response.
extractCheckoutId :: Value -> Maybe Text
extractCheckoutId (Object obj) = case KM.lookup "id" obj of
  Just (String s) -> Just s
  _ -> Nothing
extractCheckoutId _ = Nothing

-- | Extract result code from Datafast response.
extractResultCode :: Value -> Maybe Text
extractResultCode (Object obj) = case KM.lookup "result" obj of
  Just (Object resultObj) -> case KM.lookup "code" resultObj of
    Just (String s) -> Just s
    _ -> Nothing
  _ -> Nothing
extractResultCode _ = Nothing

-- | Extract payment status from Datafast status response.
extractPaymentStatus :: Value -> Maybe Text
extractPaymentStatus (Object obj) = case KM.lookup "payments" obj of
  Just (Array payments) ->
    case take 1 (foldr (:) [] payments) of
      [Object payment] -> case KM.lookup "status" payment of
        Just (String s) -> Just s
        _ -> Nothing
      _ -> Nothing
  _ -> Nothing
extractPaymentStatus _ = Nothing

-- | Check if a Datafast result code indicates success.
isSuccessCode :: Text -> Bool
isSuccessCode code = code `elem` ["000.000.000", "000.100.110", "000.200.000"]

-- | Split a buyer name into (givenName, surname).
splitBuyerName :: Text -> (Text, Text)
splitBuyerName name =
  let parts = T.words name
  in case parts of
    [] -> ("", "")
    [x] -> (x, "")
    (x:xs) -> (x, T.unwords xs)

-- | Render form parameters as URL-encoded body (strict ByteString).
renderFormBody :: [(ByteString, ByteString)] -> ByteString
renderFormBody params =
  let encoded = map (\(k, v) -> urlEncodeBS k <> "=" <> urlEncodeBS v) params
  in BS.intercalate "&" encoded
  where
    urlEncodeBS bs =
      let txt = TE.decodeUtf8 bs
          encodedTxt = T.concatMap (\c ->
            if c `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~" :: String)
              then T.singleton c
              else "%" <> hexByte c) txt
      in TE.encodeUtf8 encodedTxt
    hexByte c = let n = fromEnum c in T.pack [intToHex (n `div` 16), intToHex (n `mod` 16)]
    intToHex n | n < 10 = toEnum (fromEnum '0' + n)
               | otherwise = toEnum (fromEnum 'A' + n - 10)

-- | Strip trailing slash from a URL string.
stripTrailingSlash :: String -> String
stripTrailingSlash s = if not (null s) && last s == '/' then init s else s

-- ============================================================================
-- PayPal Integration
-- ============================================================================

-- | Load PayPal environment configuration.
loadPaypalEnvForService :: AppM (Text, Text, String)
loadPaypalEnvForService = do
  mCid <- liftIO $ lookupEnv "PAYPAL_CLIENT_ID"
  mSecret <- liftIO $ lookupEnv "PAYPAL_CLIENT_SECRET"
  mEnv <- liftIO $ lookupEnv "PAYPAL_ENV"
  cid <- maybe (throwError err500 { errBody = "PAYPAL_CLIENT_ID not set" }) (pure . T.pack) mCid
  secret <- maybe (throwError err500 { errBody = "PAYPAL_CLIENT_SECRET not set" }) (pure . T.pack) mSecret
  let baseUrl = case mEnv of
        Just "sandbox" -> "https://api-m.sandbox.paypal.com"
        Just "live"    -> "https://api-m.paypal.com"
        _              -> "https://api-m.sandbox.paypal.com"
  pure (cid, secret, baseUrl)

-- | Get PayPal access token.
paypalAccessTokenForService :: Manager -> Text -> Text -> String -> AppM Text
paypalAccessTokenForService manager cid sec baseUrl = do
  req0 <- liftIO $ parseRequest (baseUrl ++ "/v1/oauth2/token")
  let req = req0
        { method = "POST"
        , requestBody = RequestBodyBS "grant_type=client_credentials"
        , requestHeaders =
            [ ("Authorization", "Basic " <> encodeBasicAuth cid sec)
            , ("Content-Type", "application/x-www-form-urlencoded")
            ]
        }
  resp <- liftIO $ httpLbs req manager
  when (statusCode (responseStatus resp) >= 400) $
    throwError err502 { errBody = "PayPal token request failed." }
  case eitherDecode (responseBody resp) of
    Left err -> throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid PayPal token response: " <> T.pack err)) }
    Right (Object obj) -> case KM.lookup "access_token" obj of
      Just (String token) -> pure token
      _ -> throwError err502 { errBody = "No access_token in PayPal response" }
    _ -> throwError err502 { errBody = "Invalid PayPal token response format" }

-- | Create a PayPal order remotely.
createPaypalOrderRemoteForService
  :: Manager -> Text -> Text -> String -> Int -> Text -> Text -> Text
  -> AppM (Text, Maybe Text)
createPaypalOrderRemoteForService manager cid sec baseUrl totalCents currency buyerName buyerEmail = do
  token <- paypalAccessTokenForService manager cid sec baseUrl
  let amountStr = formatMinorUnitsDecimal currency (fromIntegral totalCents)
      body = object
        [ "intent" .= ("CAPTURE" :: Text)
        , "purchase_units" .=
            [ object
                [ "amount" .= object
                    [ "currency_code" .= T.toUpper currency
                    , "value" .= amountStr
                    ]
                ]
            ]
        , "payer" .= object
            [ "name" .= object ["given_name" .= buyerName]
            , "email_address" .= buyerEmail
            ]
        , "application_context" .= object
            [ "shipping_preference" .= ("NO_SHIPPING" :: Text) ]
        ]
  req0 <- liftIO $ parseRequest (baseUrl ++ "/v2/checkout/orders")
  let req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS (Aeson.encode body)
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 token)
            ]
        }
  resp <- liftIO $ httpLbs req manager
  when (statusCode (responseStatus resp) >= 400) $
    throwError err502 { errBody = "PayPal create order failed." }
  case eitherDecode (responseBody resp) of
    Left err -> throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid PayPal response: " <> T.pack err)) }
    Right (Object obj) -> do
      ppOrderId <- case KM.lookup "id" obj of
        Just (String s) -> pure s
        _ -> throwError err502 { errBody = "No order ID in PayPal response" }
      approvalUrl <- extractPaypalApprovalUrl obj
      pure (ppOrderId, approvalUrl)
    _ -> throwError err502 { errBody = "Invalid PayPal response format" }

-- | Capture a PayPal order remotely.
capturePaypalOrderRemoteForService
  :: Manager -> Text -> Text -> String -> Text
  -> AppM (Text, Maybe Text)  -- (status, payerEmail)
capturePaypalOrderRemoteForService manager cid sec baseUrl ppOrderId = do
  token <- paypalAccessTokenForService manager cid sec baseUrl
  req0 <- liftIO $ parseRequest (baseUrl ++ "/v2/checkout/orders/" ++ T.unpack ppOrderId ++ "/capture")
  let req = req0
        { method = "POST"
        , requestBody = RequestBodyBS "{}"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 token)
            ]
        }
  resp <- liftIO $ httpLbs req manager
  when (statusCode (responseStatus resp) >= 400) $
    throwError err502 { errBody = "PayPal capture failed." }
  case eitherDecode (responseBody resp) of
    Left err -> throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid PayPal capture response: " <> T.pack err)) }
    Right (Object obj) -> do
      let captureStatus = case KM.lookup "status" obj of
            Just (String s) -> s
            _ -> "UNKNOWN"
      let payerEmail = case KM.lookup "payer" obj of
            Just (Object payerObj) -> case KM.lookup "email_address" payerObj of
              Just (String e) -> Just e
              _ -> Nothing
            _ -> Nothing
      pure (captureStatus, payerEmail)
    _ -> throwError err502 { errBody = "Invalid PayPal capture response format" }

-- | Extract approval URL from PayPal order response.
extractPaypalApprovalUrl :: Aeson.Object -> AppM (Maybe Text)
extractPaypalApprovalUrl obj = case KM.lookup "links" obj of
  Just (Array links) -> pure $ findApprovalUrl (foldr (:) [] links)
  _ -> pure Nothing

findApprovalUrl :: [Value] -> Maybe Text
findApprovalUrl [] = Nothing
findApprovalUrl (Object lnk : rest) =
  case (KM.lookup "rel" lnk, KM.lookup "href" lnk) of
    (Just (String rel), Just (String href))
      | T.toLower (T.strip rel) == "approve" -> Just href
    _ -> findApprovalUrl rest
findApprovalUrl (_ : rest) = findApprovalUrl rest

-- | Encode Basic auth header.
encodeBasicAuth :: Text -> Text -> ByteString
encodeBasicAuth cid sec =
  let credentials = TE.encodeUtf8 (cid <> ":" <> sec)
  in TE.encodeUtf8 (T.pack (encodeBase64 credentials))

-- | Simple Base64 encoding.
encodeBase64 :: ByteString -> String
encodeBase64 bs =
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
      toChar n = chars !! (n `mod` 64)
      bytes = map fromIntegral (BS.unpack bs) :: [Int]
      triples = splitInto 3 bytes
      encodeTriple [a] = [toChar (a `div` 4), toChar ((a `mod` 4) * 16)]
      encodeTriple [a, b] = [toChar (a `div` 4), toChar ((a `mod` 4) * 16 + b `div` 16), toChar ((b `mod` 16) * 4)]
      encodeTriple [a, b, c] = [toChar (a `div` 4), toChar ((a `mod` 4) * 16 + b `div` 16), toChar ((b `mod` 16) * 4 + c `div` 64), toChar (c `mod` 64)]
      encodeTriple _ = ""
      splitInto _ [] = []
      splitInto n xs = take n xs : splitInto n (drop n xs)
      pad = let r = BS.length bs `mod` 3 in if r == 0 then "" else replicate (3 - r) '='
  in concatMap encodeTriple triples ++ pad
