{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.Server.ServiceStorefront
  ( serviceStorefrontPublicServer
  , serviceStorefrontAdminServer
  , validatePackageOrder
  , validateDatafastOrderResourcePath
  , validateIdempotencyKey
  ) where

import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Crypto.Hash (Digest, SHA256, hash)
import           Data.Aeson (eitherDecode, FromJSON(..), Value(..), (.=), (.:), (.:?), object, withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import           Data.ByteArray (constEq)
import qualified Data.ByteArray.Encoding as BAE
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import           Data.Maybe (fromMaybe)
import           Control.Applicative ((<|>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (getCurrentTime)
import           Data.UUID (UUID, toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (selectList, get, insert, insertUnique, getBy, replace, update, Entity(..), (==.), (=.), SelectOpt(..))
import           Database.Persist.Sql (runSqlPool)
import           Network.HTTP.Client (httpLbs, parseRequest, responseBody, responseStatus, method, requestBody, requestHeaders, Request(..), RequestBody(..), Manager)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Network.HTTP.Types (statusCode)
import           Servant
import           System.Environment (lookupEnv)
import           Web.PathPieces (fromPathPiece, toPathPiece)

import           TDF.API.ServiceStorefront (ServiceStorefrontPublicAPI, ServiceStorefrontAdminAPI)
import           TDF.API.ServiceStorefrontTypes
import           TDF.API.Types (DatafastCheckoutDTO(..), PaypalCreateDTO(..))
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
  :<|> selectManualPaymentHandler
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
    Just pkg
      | ME.serviceStorefrontPackageActive pkg -> pure (packageEntityToDTO (Entity packageId pkg))
      | otherwise -> throwError err404 { errBody = "Package not found" }

createOrderHandler :: Maybe Text -> ServiceStorefrontOrderCreate -> AppM ServiceStorefrontOrderDTO
createOrderHandler mIdempotencyKey request@ServiceStorefrontOrderCreate{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  idempotencyKey <- either (throwError . badRequestText) pure $
    validateIdempotencyKey mIdempotencyKey
  let requestHash = hashBytes (BL.toStrict (Aeson.encode request))
  mExisting <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderCreateIdempotency (Just idempotencyKey))
  case mExisting of
    Just existing -> replayExistingOrder requestHash existing
    Nothing -> createNewOrder now idempotencyKey requestHash
  where
    createNewOrder now idempotencyKey requestHash = do
      Env{..} <- ask
      let buyerName = T.strip ssocBuyerName
          buyerEmail = T.toLower (T.strip ssocBuyerEmail)
      when (T.null buyerName) $
        throwError err400 { errBody = "Buyer name is required" }
      when (T.length buyerName > 200) $
        throwError err400 { errBody = "Buyer name too long (max 200 characters)" }
      unless (isPlausibleEmail buyerEmail) $
        throwError err400 { errBody = "Buyer email is invalid" }

      packageId <- parsePackageId ssocPackageId
      mPackage <- liftIO $ flip runSqlPool envPool $ get packageId
      case mPackage of
        Nothing -> throwError err404 { errBody = "Package not found" }
        Just pkg -> do
          unless (ME.serviceStorefrontPackageActive pkg) $
            throwError err404 { errBody = "Package not found" }
          songCount <- either (throwError . badRequestText) pure $
            validatePackageOrder
              (ME.serviceStorefrontPackagePriceUsdCents pkg)
              (ME.serviceStorefrontPackageCurrency pkg)
              (ME.serviceStorefrontPackageMinSongCount pkg)
              (ME.serviceStorefrontPackageMaxSongCount pkg)
              (fromMaybe 1 ssocSongCount)
          orderId <- liftIO nextRandom
          tokenPartA <- liftIO nextRandom
          tokenPartB <- liftIO nextRandom
          let orderNumber = generateOrderNumber orderId
              lookupToken = T.replace "-" "" (toText tokenPartA <> toText tokenPartB)
              order = ME.ServiceStorefrontOrder
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
                , ME.serviceStorefrontOrderStatus = "awaiting_payment"
                , ME.serviceStorefrontOrderPaymentProvider = Nothing
                , ME.serviceStorefrontOrderStripePaymentIntentId = Nothing
                , ME.serviceStorefrontOrderStripeIdempotencyKey = Nothing
                , ME.serviceStorefrontOrderDatafastCheckoutId = Nothing
                , ME.serviceStorefrontOrderDatafastResourcePath = Nothing
                , ME.serviceStorefrontOrderDatafastPaymentId = Nothing
                , ME.serviceStorefrontOrderPaypalOrderId = Nothing
                , ME.serviceStorefrontOrderPaypalCaptureId = Nothing
                , ME.serviceStorefrontOrderPaypalPayerEmail = Nothing
                , ME.serviceStorefrontOrderLookupTokenHash = Just (hashLookupToken lookupToken)
                , ME.serviceStorefrontOrderCreateIdempotencyKey = Just idempotencyKey
                , ME.serviceStorefrontOrderCreateRequestSha256 = Just requestHash
                , ME.serviceStorefrontOrderPaidAt = Nothing
                , ME.serviceStorefrontOrderGenre = fmap T.strip ssocGenre
                , ME.serviceStorefrontOrderSongCount = songCount
                , ME.serviceStorefrontOrderNotes = fmap T.strip ssocNotes
                , ME.serviceStorefrontOrderReferenceTrackUrl = fmap T.strip ssocReferenceTrackUrl
                , ME.serviceStorefrontOrderDeadline = Nothing
                , ME.serviceStorefrontOrderSourceFilesUrl = Nothing
                , ME.serviceStorefrontOrderDeliverablesUrl = Nothing
                , ME.serviceStorefrontOrderPipelineCardId = Nothing
                , ME.serviceStorefrontOrderCreatedAt = now
                , ME.serviceStorefrontOrderUpdatedAt = now
                }

          mOrderIdKey <- liftIO $ flip runSqlPool envPool $ insertUnique order
          case mOrderIdKey of
            Nothing -> do
              raced <- liftIO $ flip runSqlPool envPool $
                getBy (ME.UniqueServiceStorefrontOrderCreateIdempotency (Just idempotencyKey))
              maybe (throwError err409 { errBody = "Order creation conflicted; retry with the same idempotency key" })
                (replayExistingOrder requestHash) raced
            Just orderIdKey -> do
              _ <- liftIO $ flip runSqlPool envPool $ insert ME.ServiceStorefrontOrderStatusChange
                { ME.serviceStorefrontOrderStatusChangeOrderId = orderIdKey
                , ME.serviceStorefrontOrderStatusChangeStatus = "awaiting_payment"
                , ME.serviceStorefrontOrderStatusChangeNotes = Just "Order created"
                , ME.serviceStorefrontOrderStatusChangeChangedBy = Just "system"
                , ME.serviceStorefrontOrderStatusChangeCreatedAt = now
                }
              pure (orderToDTOWithLookupToken (Just lookupToken) orderIdKey order)

    replayExistingOrder requestHash (Entity oid existing)
      | ME.serviceStorefrontOrderCreateRequestSha256 existing == Just requestHash =
          pure (orderToDTO oid existing)
      | otherwise =
          throwError err409 { errBody = "Idempotency key was already used for a different order request" }

getOrderHandler :: Text -> Maybe Text -> AppM ServiceStorefrontOrderDTO
getOrderHandler orderIdText mLookupToken = do
  Env{..} <- ask
  -- Try by order number first (more user-friendly)
  mOrder <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mOrder of
    Just (Entity oid order) -> do
      requireOrderLookupToken mLookupToken order
      pure (orderToDTO oid order)
    Nothing -> throwError err404 { errBody = "Order not found" }

-- Payment handlers (stubs - need provider credentials)

createStripePaymentIntentHandler :: Text -> Maybe Text -> AppM StripePaymentIntentDTO
createStripePaymentIntentHandler _ _ =
  throwError err503
    { errBody = "Stripe checkout is not configured for the service storefront. Use an enabled payment provider." }

createDatafastCheckoutHandler :: Text -> Maybe Text -> AppM DatafastCheckoutDTO
createDatafastCheckoutHandler orderIdText mLookupToken = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  
  -- Load order by order number
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      requireOrderLookupToken mLookupToken order
      -- Verify order is in a payable state
      let status = ME.serviceStorefrontOrderStatus order
      when (status `notElem` ["awaiting_payment", "pending_payment", "payment_failed"]) $
        throwError err400 { errBody = "Order is not in a payable state" }
      
      let totalCents = ME.serviceStorefrontOrderPriceUsdCents order
          currency = ME.serviceStorefrontOrderCurrency order
          buyerName = ME.serviceStorefrontOrderBuyerName order
          buyerEmail = ME.serviceStorefrontOrderBuyerEmail order
          buyerPhone = ME.serviceStorefrontOrderBuyerPhone order
      
      (checkoutId, widgetUrl) <- case (status, ME.serviceStorefrontOrderDatafastCheckoutId order) of
        ("datafast_pending", Just existingCheckoutId) -> do
          dfEnv <- loadServiceDatafastEnv
          let baseUrlClean = stripTrailingSlash (sdfBaseUrl dfEnv)
          pure (existingCheckoutId, baseUrlClean ++ "/v1/paymentWidgets.js?checkoutId=" ++ T.unpack existingCheckoutId)
        _ -> requestDatafastCheckoutForService
          (toPathPiece oid) totalCents currency buyerName buyerEmail buyerPhone
      
      -- Update order with Datafast info
      liftIO $ flip runSqlPool envPool $ update oid
        [ ME.ServiceStorefrontOrderStatus =. "datafast_pending"
        , ME.ServiceStorefrontOrderPaymentProvider =. Just "datafast"
        , ME.ServiceStorefrontOrderDatafastCheckoutId =. Just checkoutId
        , ME.ServiceStorefrontOrderUpdatedAt =. now
        ]
      
      pure DatafastCheckoutDTO
        { dcOrderId    = ME.serviceStorefrontOrderOrderNumber order
        , dcCheckoutId = checkoutId
        , dcWidgetUrl  = T.pack widgetUrl
        , dcAmount     = formatMoney (defaultLocale envConfig) currency (fromIntegral totalCents)
        , dcCurrency   = currency
        }

confirmDatafastStatusHandler :: Maybe Text -> Maybe Text -> Maybe Text -> AppM ServiceStorefrontOrderDTO
confirmDatafastStatusHandler mOrderId mResourcePath mLookupToken = do
  Env{..} <- ask
  orderIdText <- maybe (throwError err400 { errBody = "orderId requerido" }) pure mOrderId
  resourcePathTxt <- maybe (throwError err400 { errBody = "resourcePath requerido" }) pure mResourcePath
  
  -- Load order
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      requireOrderLookupToken mLookupToken order
      resourcePath <- either (throwError . badRequestText) pure $
        validateDatafastOrderResourcePath
          (ME.serviceStorefrontOrderDatafastCheckoutId order)
          resourcePathTxt
      paymentStatus <- checkDatafastPaymentStatus resourcePath
      now <- liftIO getCurrentTime
      let resultCode = sdfpsResultCode paymentStatus
          providerSuccess = isDatafastPaymentSuccess resultCode
          providerPending = resultCode == "000.200.000"
      when providerSuccess $
        either (throwError . providerValidationError) pure $
          validateDatafastSuccessfulPayment
            (toPathPiece oid)
            (ME.serviceStorefrontOrderPriceUsdCents order)
            (ME.serviceStorefrontOrderCurrency order)
            paymentStatus
      let alreadyPaid = ME.serviceStorefrontOrderStatus order == "paid"
          newStatus
            | alreadyPaid = "paid"
            | providerSuccess = "paid"
            | providerPending = "datafast_pending"
            | otherwise = "payment_failed"
          paidAt
            | alreadyPaid = ME.serviceStorefrontOrderPaidAt order
            | providerSuccess = Just now
            | otherwise = Nothing
          providerPaymentId = sdfpsPaymentId paymentStatus
      liftIO $ flip runSqlPool envPool $ update oid
        [ ME.ServiceStorefrontOrderStatus =. newStatus
        , ME.ServiceStorefrontOrderDatafastResourcePath =. Just resourcePath
        , ME.ServiceStorefrontOrderDatafastPaymentId =. (providerPaymentId <|> ME.serviceStorefrontOrderDatafastPaymentId order)
        , ME.ServiceStorefrontOrderPaidAt =. paidAt
        , ME.ServiceStorefrontOrderUpdatedAt =. now
        ]
      when (newStatus /= ME.serviceStorefrontOrderStatus order) $ do
        let statusChange = ME.ServiceStorefrontOrderStatusChange
              { ME.serviceStorefrontOrderStatusChangeOrderId = oid
              , ME.serviceStorefrontOrderStatusChangeStatus = newStatus
              , ME.serviceStorefrontOrderStatusChangeNotes = Just $ "Datafast server verification result: " <> resultCode
              , ME.serviceStorefrontOrderStatusChangeChangedBy = Just "datafast_server_verification"
              , ME.serviceStorefrontOrderStatusChangeCreatedAt = now
              }
        _ <- liftIO $ flip runSqlPool envPool $ insert statusChange
        pure ()
      mUpdated <- liftIO $ flip runSqlPool envPool $ get oid
      case mUpdated of
        Nothing -> throwError err500 { errBody = "Failed to load updated order" }
        Just updated -> pure (orderToDTO oid updated)

createPaypalOrderHandler :: Text -> Maybe Text -> AppM PaypalCreateDTO
createPaypalOrderHandler orderIdText mLookupToken = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  
  -- Load order by order number
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      requireOrderLookupToken mLookupToken order
      -- Verify order is in a payable state
      let status = ME.serviceStorefrontOrderStatus order
      when (status `notElem` ["awaiting_payment", "pending_payment", "payment_failed"]) $
        throwError err400 { errBody = "Order is not in a payable state" }
      
      let totalCents = ME.serviceStorefrontOrderPriceUsdCents order
          currency = ME.serviceStorefrontOrderCurrency order
          buyerName = ME.serviceStorefrontOrderBuyerName order
          buyerEmail = ME.serviceStorefrontOrderBuyerEmail order
      
      (ppOrderId, approvalUrl) <- case (status, ME.serviceStorefrontOrderPaypalOrderId order) of
        ("paypal_pending", Just existingOrderId) -> pure (existingOrderId, Nothing)
        _ -> do
          (cid, sec, baseUrl) <- loadPaypalEnvForService
          manager <- liftIO newTlsManager
          createPaypalOrderRemoteForService
            manager cid sec baseUrl (toPathPiece oid) totalCents currency buyerName buyerEmail
      
      -- Update order with PayPal info
      liftIO $ flip runSqlPool envPool $ update oid
        [ ME.ServiceStorefrontOrderStatus =. "paypal_pending"
        , ME.ServiceStorefrontOrderPaymentProvider =. Just "paypal"
        , ME.ServiceStorefrontOrderPaypalOrderId =. Just ppOrderId
        , ME.ServiceStorefrontOrderUpdatedAt =. now
        ]
      
      pure PaypalCreateDTO
        { pcOrderId = ME.serviceStorefrontOrderOrderNumber order
        , pcPaypalOrderId = ppOrderId
        , pcApprovalUrl = approvalUrl
        }

capturePaypalHandler :: Maybe Text -> ServiceStorefrontPaypalCaptureReq -> AppM ServiceStorefrontOrderDTO
capturePaypalHandler mLookupToken ServiceStorefrontPaypalCaptureReq{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  
  -- Load order by order number
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber pcCaptureOrderId)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      requireOrderLookupToken mLookupToken order
      -- Verify PayPal order ID matches
      case ME.serviceStorefrontOrderPaypalOrderId order of
        Nothing -> throwError err400 { errBody = "Order has no PayPal order" }
        Just storedPpOrderId | storedPpOrderId /= pcCapturePaypalId ->
          throwError err400 { errBody = "PayPal order ID mismatch" }
        _ -> pure ()
      if ME.serviceStorefrontOrderStatus order == "paid"
        then pure (orderToDTO oid order)
        else do
      
          (cid, sec, baseUrl) <- loadPaypalEnvForService
          manager <- liftIO newTlsManager
      
      -- Capture PayPal order
          captureOutcome <- capturePaypalOrderRemoteForService
            manager cid sec baseUrl pcCapturePaypalId
          when (spcoStatus captureOutcome == "COMPLETED") $
            either (throwError . providerValidationError) pure $
              validatePaypalSuccessfulCapture
                (toPathPiece oid)
                (ME.serviceStorefrontOrderPriceUsdCents order)
                (ME.serviceStorefrontOrderCurrency order)
                captureOutcome
          let nextStatus
                | spcoStatus captureOutcome == "COMPLETED" = "paid"
                | spcoStatus captureOutcome `elem` ["APPROVED", "PENDING"] = "paypal_pending"
                | otherwise = "payment_failed"
              paidAt
                | nextStatus == "paid" = Just now
                | otherwise = Nothing
      
          liftIO $ flip runSqlPool envPool $ update oid
            [ ME.ServiceStorefrontOrderStatus =. nextStatus
            , ME.ServiceStorefrontOrderPaypalCaptureId =. (spcoCaptureId captureOutcome <|> ME.serviceStorefrontOrderPaypalCaptureId order)
            , ME.ServiceStorefrontOrderPaypalPayerEmail =. (spcoPayerEmail captureOutcome <|> ME.serviceStorefrontOrderPaypalPayerEmail order)
            , ME.ServiceStorefrontOrderPaidAt =. paidAt
            , ME.ServiceStorefrontOrderUpdatedAt =. now
            ]
      
          when (nextStatus /= ME.serviceStorefrontOrderStatus order) $ do
            let statusChange = ME.ServiceStorefrontOrderStatusChange
                  { ME.serviceStorefrontOrderStatusChangeOrderId = oid
                  , ME.serviceStorefrontOrderStatusChangeStatus = nextStatus
                  , ME.serviceStorefrontOrderStatusChangeNotes = Just $ "PayPal server capture: " <> spcoStatus captureOutcome
                  , ME.serviceStorefrontOrderStatusChangeChangedBy = Just "paypal_server_capture"
                  , ME.serviceStorefrontOrderStatusChangeCreatedAt = now
                  }
            _ <- liftIO $ flip runSqlPool envPool $ insert statusChange
            pure ()
          mUpdated <- liftIO $ flip runSqlPool envPool $ get oid
          case mUpdated of
            Nothing -> throwError err500 { errBody = "Failed to load updated order" }
            Just updated -> pure (orderToDTO oid updated)

selectManualPaymentHandler
  :: Text
  -> Maybe Text
  -> ServiceStorefrontManualPaymentCreate
  -> AppM ServiceStorefrontOrderDTO
selectManualPaymentHandler orderIdText mLookupToken ServiceStorefrontManualPaymentCreate{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  let methodName = T.toLower (T.strip ssmPaymentMethod)
  unless (methodName `elem` ["bank_transfer", "cash", "pos"]) $
    throwError err400 { errBody = "Unsupported manual payment method" }
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      requireOrderLookupToken mLookupToken order
      when (ME.serviceStorefrontOrderStatus order `notElem`
              ["awaiting_payment", "pending_payment", "payment_failed", "awaiting_manual_confirmation"]) $
        throwError err409 { errBody = "Order is not awaiting payment" }
      liftIO $ flip runSqlPool envPool $ do
        update oid
          [ ME.ServiceStorefrontOrderStatus =. "awaiting_manual_confirmation"
          , ME.ServiceStorefrontOrderPaymentProvider =. Just methodName
          , ME.ServiceStorefrontOrderUpdatedAt =. now
          ]
        _ <- insert ME.ServiceStorefrontOrderStatusChange
          { ME.serviceStorefrontOrderStatusChangeOrderId = oid
          , ME.serviceStorefrontOrderStatusChangeStatus = "awaiting_manual_confirmation"
          , ME.serviceStorefrontOrderStatusChangeNotes = Just ("Customer selected " <> methodName <> "; staff verification required")
          , ME.serviceStorefrontOrderStatusChangeChangedBy = Just "customer"
          , ME.serviceStorefrontOrderStatusChangeCreatedAt = now
          }
        pure ()
      mUpdated <- liftIO $ flip runSqlPool envPool $ get oid
      maybe (throwError err500 { errBody = "Failed to load updated order" })
        (pure . orderToDTO oid) mUpdated

createRevisionHandler :: Text -> Maybe Text -> ServiceStorefrontRevisionCreate -> AppM ServiceStorefrontRevisionDTO
createRevisionHandler orderIdText mLookupToken ServiceStorefrontRevisionCreate{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  let feedback = T.strip ssrcFeedback
  when (T.null feedback || T.length feedback > 5000) $
    throwError err400 { errBody = "Revision feedback must contain 1 to 5000 characters" }
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderIdText)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      requireOrderLookupToken mLookupToken order
      unless (ME.serviceStorefrontOrderStatus order `elem` ["v1_delivered", "revisions"]) $
        throwError err409 { errBody = "A revision cannot be requested in the current order state" }
      revisions <- liftIO $ flip runSqlPool envPool $
        selectList [ME.ServiceStorefrontRevisionOrderId ==. oid] [Desc ME.ServiceStorefrontRevisionRevisionNumber]
      let revisionNumber = case revisions of
            Entity _ revision : _ -> ME.serviceStorefrontRevisionRevisionNumber revision + 1
            [] -> 1
      when (revisionNumber > 50) $
        throwError err409 { errBody = "Revision limit reached; contact support" }
      revisionId <- liftIO $ flip runSqlPool envPool $ insert ME.ServiceStorefrontRevision
        { ME.serviceStorefrontRevisionOrderId = oid
        , ME.serviceStorefrontRevisionRevisionNumber = revisionNumber
        , ME.serviceStorefrontRevisionFeedback = feedback
        , ME.serviceStorefrontRevisionStatus = "pending"
        , ME.serviceStorefrontRevisionCreatedAt = now
        , ME.serviceStorefrontRevisionCompletedAt = Nothing
        }
      pure ServiceStorefrontRevisionDTO
        { ssrId = toPathPiece revisionId
        , ssrOrderId = toPathPiece oid
        , ssrRevisionNumber = revisionNumber
        , ssrFeedback = feedback
        , ssrStatus = "pending"
        , ssrCreatedAt = now
        , ssrCompletedAt = Nothing
        }

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
  let minSongCount = fromMaybe 1 sspcMinSongCount
      maxSongCount = fromMaybe 1 sspcMaxSongCount
  _ <- either (throwError . badRequestText) pure $
    validatePackageOrder sspcPriceUsdCents currency minSongCount maxSongCount minSongCount
  let pkg = ME.ServiceStorefrontPackage
        { ME.serviceStorefrontPackageServiceKind = sspcServiceKind
        , ME.serviceStorefrontPackageTier = sspcTier
        , ME.serviceStorefrontPackageName = sspcName
        , ME.serviceStorefrontPackageDescription = sspcDescription
        , ME.serviceStorefrontPackagePriceUsdCents = sspcPriceUsdCents
        , ME.serviceStorefrontPackageCurrency = currency
        , ME.serviceStorefrontPackageMinSongCount = minSongCount
        , ME.serviceStorefrontPackageMaxSongCount = maxSongCount
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
      let nextPrice = fromMaybe (ME.serviceStorefrontPackagePriceUsdCents pkg) sspuPriceUsdCents
          nextMinSongs = fromMaybe (ME.serviceStorefrontPackageMinSongCount pkg) sspuMinSongCount
          nextMaxSongs = fromMaybe (ME.serviceStorefrontPackageMaxSongCount pkg) sspuMaxSongCount
      _ <- either (throwError . badRequestText) pure $
        validatePackageOrder nextPrice (ME.serviceStorefrontPackageCurrency pkg) nextMinSongs nextMaxSongs nextMinSongs
      let updatedPkg = pkg
            { ME.serviceStorefrontPackageName = fromMaybe (ME.serviceStorefrontPackageName pkg) sspuName
            , ME.serviceStorefrontPackageDescription = maybe (ME.serviceStorefrontPackageDescription pkg) Just sspuDescription
            , ME.serviceStorefrontPackagePriceUsdCents = nextPrice
            , ME.serviceStorefrontPackageMinSongCount = nextMinSongs
            , ME.serviceStorefrontPackageMaxSongCount = nextMaxSongs
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
  , sspMinSongCount = ME.serviceStorefrontPackageMinSongCount pkg
  , sspMaxSongCount = ME.serviceStorefrontPackageMaxSongCount pkg
  , sspTurnaroundDays = ME.serviceStorefrontPackageTurnaroundDays pkg
  , sspRevisionCount = ME.serviceStorefrontPackageRevisionCount pkg
  , sspDeliverables = Nothing -- TODO: JSON decode
  , sspFeatures = Nothing -- TODO: JSON decode
  , sspActive = ME.serviceStorefrontPackageActive pkg
  , sspSortOrder = ME.serviceStorefrontPackageSortOrder pkg
  }

orderToDTO :: ME.ServiceStorefrontOrderId -> ME.ServiceStorefrontOrder -> ServiceStorefrontOrderDTO
orderToDTO = orderToDTOWithLookupToken Nothing

orderToDTOWithLookupToken :: Maybe Text -> ME.ServiceStorefrontOrderId -> ME.ServiceStorefrontOrder -> ServiceStorefrontOrderDTO
orderToDTOWithLookupToken lookupToken oid order = ServiceStorefrontOrderDTO
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
  , ssoLookupToken = lookupToken
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

badRequestText :: Text -> ServerError
badRequestText message =
  err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

-- | Server-side package snapshot invariant. The package price is fixed for the
-- configured quantity range; clients never multiply or override it.
validatePackageOrder :: Int -> Text -> Int -> Int -> Int -> Either Text Int
validatePackageOrder priceCents currency minSongs maxSongs requestedSongs
  | priceCents <= 0 = Left "Package price must be positive"
  | T.toUpper (T.strip currency) /= "USD" = Left "Package currency is not supported"
  | minSongs < 1 || maxSongs < minSongs = Left "Package quantity configuration is invalid"
  | requestedSongs < minSongs || requestedSongs > maxSongs =
      Left ("Song count must be between " <> T.pack (show minSongs) <> " and " <> T.pack (show maxSongs))
  | otherwise = Right requestedSongs

isPlausibleEmail :: Text -> Bool
isPlausibleEmail email =
  not (T.null email)
    && T.length email <= 254
    && T.count "@" email == 1
    && case T.splitOn "@" email of
      [localPart, domainPart] ->
        not (T.null localPart)
          && T.any (== '.') domainPart
          && not (T.isPrefixOf "." domainPart)
          && not (T.isSuffixOf "." domainPart)
          && T.all (\c -> c > ' ' && c /= '\DEL') email
      _ -> False

hashLookupToken :: Text -> Text
hashLookupToken = hashBytes . TE.encodeUtf8

hashBytes :: ByteString -> Text
hashBytes bytes =
  TE.decodeUtf8 $
    BAE.convertToBase BAE.Base16 (hash bytes :: Digest SHA256)

validateIdempotencyKey :: Maybe Text -> Either Text Text
validateIdempotencyKey mRawKey = do
  key <- maybe (Left "Idempotency-Key header is required") (Right . T.strip) mRawKey
  unless (T.length key >= 16 && T.length key <= 128) $
    Left "Idempotency-Key must contain 16 to 128 characters"
  unless (T.all (\c -> c >= '!' && c <= '~') key) $
    Left "Idempotency-Key must contain visible ASCII characters only"
  pure key

requireOrderLookupToken :: Maybe Text -> ME.ServiceStorefrontOrder -> AppM ()
requireOrderLookupToken mRawToken order =
  case (ME.serviceStorefrontOrderLookupTokenHash order, T.strip <$> mRawToken) of
    (Just expectedHash, Just rawToken)
      | not (T.null rawToken)
      , TE.encodeUtf8 expectedHash `constEq` TE.encodeUtf8 (hashLookupToken rawToken) -> pure ()
    _ -> throwError err404 { errBody = "Order not found" }

validateDatafastOrderResourcePath :: Maybe Text -> Text -> Either Text Text
validateDatafastOrderResourcePath mCheckoutId rawResourcePath = do
  checkoutId <- maybe (Left "Order does not have a Datafast checkout to confirm") Right mCheckoutId
  let resourcePath = T.strip rawResourcePath
      expected = "/v1/checkouts/" <> checkoutId <> "/payment"
      validCheckoutId =
        not (T.null checkoutId)
          && T.length checkoutId <= 256
          && T.all isDatafastIdChar checkoutId
  unless validCheckoutId (Left "Stored Datafast checkout ID is invalid")
  unless (resourcePath == expected) (Left "resourcePath does not match this order's Datafast checkout")
  pure resourcePath
  where
    isDatafastIdChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-_." :: String)

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
      unless (isProviderReference checkoutId) $
        throwError err502 { errBody = "Datafast returned an invalid checkout ID" }
      let widgetUrl = baseUrlClean ++ "/v1/paymentWidgets.js?checkoutId=" ++ T.unpack checkoutId
      pure (checkoutId, widgetUrl)
  where
    pad2 n = if n < 10 then "0" <> show n else show n

data ServiceDatafastPaymentStatus = ServiceDatafastPaymentStatus
  { sdfpsPaymentId             :: Maybe Text
  , sdfpsAmount                :: Maybe Text
  , sdfpsCurrency              :: Maybe Text
  , sdfpsMerchantTransactionId :: Maybe Text
  , sdfpsResultCode            :: Text
  } deriving (Show)

instance FromJSON ServiceDatafastPaymentStatus where
  parseJSON = withObject "ServiceDatafastPaymentStatus" $ \o -> do
    result <- o .: "result"
    ServiceDatafastPaymentStatus
      <$> o .:? "id"
      <*> o .:? "amount"
      <*> o .:? "currency"
      <*> o .:? "merchantTransactionId"
      <*> result .: "code"

-- | Check Datafast payment status using the server-held merchant credentials.
checkDatafastPaymentStatus :: Text -> AppM ServiceDatafastPaymentStatus
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
    Right dfResp -> pure dfResp

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

-- | Check if a Datafast result code indicates success.
isSuccessCode :: Text -> Bool
isSuccessCode code = code `elem` ["000.000.000", "000.100.110", "000.200.000"]

isDatafastPaymentSuccess :: Text -> Bool
isDatafastPaymentSuccess code =
  "000.000" `T.isPrefixOf` code || "000.100" `T.isPrefixOf` code

providerValidationError :: Text -> ServerError
providerValidationError message =
  err502 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

validateDatafastSuccessfulPayment
  :: Text
  -> Int
  -> Text
  -> ServiceDatafastPaymentStatus
  -> Either Text ()
validateDatafastSuccessfulPayment expectedOrderId expectedCents expectedCurrency status = do
  paidCents <- maybe (Left "Datafast response did not include an amount") parseDatafastCents (sdfpsAmount status)
  paidCurrency <- maybe (Left "Datafast response did not include a currency") (Right . T.toUpper . T.strip) (sdfpsCurrency status)
  merchantReference <- maybe (Left "Datafast response did not include the merchant transaction ID") (Right . T.strip) (sdfpsMerchantTransactionId status)
  paymentId <- maybe (Left "Datafast response did not include a payment ID") (Right . T.strip) (sdfpsPaymentId status)
  unless (paidCents == expectedCents) (Left "Datafast amount does not match the immutable order total")
  unless (paidCurrency == T.toUpper (T.strip expectedCurrency)) (Left "Datafast currency does not match the immutable order currency")
  unless (merchantReference == expectedOrderId) (Left "Datafast merchant reference does not match the internal order")
  unless (not (T.null paymentId) && T.length paymentId <= 256) (Left "Datafast payment ID is invalid")

parseDatafastCents :: Text -> Either Text Int
parseDatafastCents raw =
  case T.splitOn "." (T.strip raw) of
    [whole] -> parseParts whole "0"
    [whole, fraction] | T.length fraction <= 2 -> parseParts whole fraction
    _ -> Left "Datafast amount is invalid"
  where
    parseParts whole fraction
      | T.null whole || T.null fraction = Left "Datafast amount is invalid"
      | not (T.all isDigit whole && T.all isDigit fraction) = Left "Datafast amount is invalid"
      | otherwise =
          let wholeValue = read (T.unpack whole) :: Integer
              fractionValue = read (T.unpack (T.take 2 (fraction <> "00"))) :: Integer
              cents = wholeValue * 100 + fractionValue
          in if cents > 0 && cents <= fromIntegral (maxBound :: Int)
               then Right (fromIntegral cents)
               else Left "Datafast amount is invalid"

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
  :: Manager -> Text -> Text -> String -> Text -> Int -> Text -> Text -> Text
  -> AppM (Text, Maybe Text)
createPaypalOrderRemoteForService manager cid sec baseUrl internalOrderId totalCents currency buyerName buyerEmail = do
  token <- paypalAccessTokenForService manager cid sec baseUrl
  let amountStr = formatMinorUnitsDecimal currency (fromIntegral totalCents)
      body = object
        [ "intent" .= ("CAPTURE" :: Text)
        , "purchase_units" .=
            [ object
                [ "custom_id" .= internalOrderId
                , "invoice_id" .= internalOrderId
                , "amount" .= object
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
            , ("PayPal-Request-Id", TE.encodeUtf8 ("svc-create-" <> T.take 27 internalOrderId))
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
      unless (isProviderReference ppOrderId) $
        throwError err502 { errBody = "PayPal returned an invalid order ID" }
      approvalUrl <- extractPaypalApprovalUrl obj
      pure (ppOrderId, approvalUrl)
    _ -> throwError err502 { errBody = "Invalid PayPal response format" }

data ServicePaypalCaptureOutcome = ServicePaypalCaptureOutcome
  { spcoStatus          :: Text
  , spcoPayerEmail      :: Maybe Text
  , spcoCaptureId       :: Maybe Text
  , spcoAmount          :: Maybe Text
  , spcoCurrency        :: Maybe Text
  , spcoInternalOrderId :: Maybe Text
  } deriving (Show)

-- | Capture a PayPal order remotely and retain the fields required for binding.
capturePaypalOrderRemoteForService
  :: Manager -> Text -> Text -> String -> Text
  -> AppM ServicePaypalCaptureOutcome
capturePaypalOrderRemoteForService manager cid sec baseUrl ppOrderId = do
  token <- paypalAccessTokenForService manager cid sec baseUrl
  req0 <- liftIO $ parseRequest (baseUrl ++ "/v2/checkout/orders/" ++ T.unpack ppOrderId ++ "/capture")
  let req = req0
        { method = "POST"
        , requestBody = RequestBodyBS "{}"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 token)
            , ("PayPal-Request-Id", TE.encodeUtf8 ("service-capture-" <> ppOrderId))
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
          purchaseUnit = firstObject "purchase_units" obj
          captureObject = purchaseUnit >>= firstObject "payments" >>= firstObject "captures"
          captureId = captureObject >>= lookupObjectText "id"
          amountObject = captureObject >>= lookupObject "amount"
          capturedValue = amountObject >>= lookupObjectText "value"
          capturedCurrency = amountObject >>= lookupObjectText "currency_code"
          internalOrderId = purchaseUnit >>= lookupObjectText "custom_id"
      pure ServicePaypalCaptureOutcome
        { spcoStatus = captureStatus
        , spcoPayerEmail = payerEmail
        , spcoCaptureId = captureId
        , spcoAmount = capturedValue
        , spcoCurrency = capturedCurrency
        , spcoInternalOrderId = internalOrderId
        }
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

lookupObject :: Text -> Aeson.Object -> Maybe Aeson.Object
lookupObject key obj = case KM.lookup (AesonKey.fromText key) obj of
  Just (Object value) -> Just value
  _ -> Nothing

lookupObjectText :: Text -> Aeson.Object -> Maybe Text
lookupObjectText key obj = case KM.lookup (AesonKey.fromText key) obj of
  Just (String value) -> Just value
  _ -> Nothing

firstObject :: Text -> Aeson.Object -> Maybe Aeson.Object
firstObject key obj = case KM.lookup (AesonKey.fromText key) obj of
  Just (Array values) -> case foldr (:) [] values of
    Object value : _ -> Just value
    _ -> Nothing
  Just (Object value) -> Just value
  _ -> Nothing

validatePaypalSuccessfulCapture
  :: Text
  -> Int
  -> Text
  -> ServicePaypalCaptureOutcome
  -> Either Text ()
validatePaypalSuccessfulCapture expectedOrderId expectedCents expectedCurrency outcome = do
  captureId <- maybe (Left "PayPal response did not include a capture ID") (Right . T.strip) (spcoCaptureId outcome)
  capturedCents <- maybe (Left "PayPal response did not include a captured amount") parseDatafastCents (spcoAmount outcome)
  capturedCurrency <- maybe (Left "PayPal response did not include a captured currency") (Right . T.toUpper . T.strip) (spcoCurrency outcome)
  customId <- maybe (Left "PayPal response did not include the internal order binding") (Right . T.strip) (spcoInternalOrderId outcome)
  unless (not (T.null captureId) && T.length captureId <= 128) (Left "PayPal capture ID is invalid")
  unless (capturedCents == expectedCents) (Left "PayPal captured amount does not match the immutable order total")
  unless (capturedCurrency == T.toUpper (T.strip expectedCurrency)) (Left "PayPal captured currency does not match the immutable order currency")
  unless (customId == expectedOrderId) (Left "PayPal custom_id does not match the internal order")

isProviderReference :: Text -> Bool
isProviderReference value =
  not (T.null value)
    && T.length value <= 256
    && T.any (\c -> isAsciiLower c || isAsciiUpper c || isDigit c) value
    && T.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-_." :: String)) value

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
