{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.Server.ServiceStorefront
  ( serviceStorefrontPublicServer
  , serviceStorefrontAdminServer
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime, UTCTime)
import           Data.UUID (UUID, toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (selectList, get, insert, getBy, replace, Entity(..), (==.), SelectOpt(..))
import           Database.Persist.Sql (runSqlPool, SqlBackend)
import           Servant
import           Web.PathPieces (fromPathPiece, toPathPiece)

import           TDF.API.ServiceStorefront (ServiceStorefrontPublicAPI, ServiceStorefrontAdminAPI)
import           TDF.API.ServiceStorefrontTypes
import           TDF.API.Types (DatafastCheckoutDTO(..), PaypalCreateDTO(..), PaypalCaptureReq(..))
import           TDF.DB (Env(..))
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
  throwError err501 { errBody = "Stripe is not available in Ecuador. Use Datafast or PayPal." }

createDatafastCheckoutHandler :: Text -> AppM DatafastCheckoutDTO
createDatafastCheckoutHandler _ =
  throwError err501 { errBody = "Datafast checkout not yet configured. Contact admin." }

confirmDatafastStatusHandler :: Maybe Text -> Maybe Text -> AppM ServiceStorefrontOrderDTO
confirmDatafastStatusHandler _ _ =
  throwError err501 { errBody = "Datafast status confirmation not yet configured." }

createPaypalOrderHandler :: Text -> AppM PaypalCreateDTO
createPaypalOrderHandler _ =
  throwError err501 { errBody = "PayPal not yet configured. Contact admin." }

capturePaypalHandler :: PaypalCaptureReq -> AppM ServiceStorefrontOrderDTO
capturePaypalHandler _ =
  throwError err501 { errBody = "PayPal capture not yet configured." }

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
  let pkg = ME.ServiceStorefrontPackage
        { ME.serviceStorefrontPackageServiceKind = sspcServiceKind
        , ME.serviceStorefrontPackageTier = sspcTier
        , ME.serviceStorefrontPackageName = sspcName
        , ME.serviceStorefrontPackageDescription = sspcDescription
        , ME.serviceStorefrontPackagePriceUsdCents = sspcPriceUsdCents
        , ME.serviceStorefrontPackageCurrency = fromMaybe "USD" sspcCurrency
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
