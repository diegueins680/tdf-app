{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.Server.ServiceStorefront
  ( serviceStorefrontPublicServer
  , serviceStorefrontAdminServer
  , validatePackageOrder
  , validateDatafastOrderResourcePath
  , validateDatafastEnvironmentBase
  , isDatafastCheckoutCreationSuccess
  , isDatafastPaymentSuccess
  , validateIdempotencyKey
  , validateServiceFulfillmentTransition
  , ServicePaypalCaptureOutcome(..)
  , parsePaypalCaptureOutcome
  , PaypalWebhookEnvelope(..)
  , PaypalWebhookHeaders(..)
  , PaypalWebhookCapture(..)
  , PaypalRefundOutcome(..)
  , BoundPaypalCapture(..)
  , parsePaypalWebhookEnvelope
  , parsePaypalWebhookCapture
  , validatePaypalWebhookHeaders
  , validatePaypalWebhookCaptureBinding
  , buildPaypalWebhookVerificationBody
  , parsePaypalRefundOutcome
  ) where

import           Control.Monad (when, unless)
import           Control.Monad.Except (catchError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Control.Exception.Safe (tryAny)
import           Crypto.Hash (Digest, SHA256, hash)
import           Data.Aeson (Result(..), eitherDecode, FromJSON(..), Value(..), (.=), (.:), (.:?), object, withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import           Data.ByteArray (constEq)
import qualified Data.ByteArray.Encoding as BAE
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Control.Applicative ((<|>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import           Data.UUID (UUID, fromText, toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..), selectList, get, insert, insertUnique, getBy, replace, update, Entity(..), (==.), (=.), SelectOpt(..))
import           Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import           Network.HTTP.Client (httpLbs, parseRequest, responseBody, responseStatus, method, requestBody, requestHeaders, Request(..), RequestBody(..), Manager)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Network.HTTP.Types (statusCode)
import           Servant
import           System.Environment (lookupEnv)
import           Web.PathPieces (fromPathPiece, toPathPiece)

import           TDF.API.ServiceStorefront (ServiceStorefrontPublicAPI, ServiceStorefrontAdminAPI)
import           TDF.API.ServiceStorefrontTypes
import           TDF.API.Types (DatafastCheckoutDTO(..), PaypalCreateDTO(..))
import           TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.ProviderEventStore as ProviderEvent
import qualified TDF.Commerce.RefundStore as Refund
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
  :<|> paypalWebhookHandler
  :<|> selectManualPaymentHandler
  :<|> createRevisionHandler

-- | Admin server for the service storefront.
serviceStorefrontAdminServer :: AuthedUser -> ServerT ServiceStorefrontAdminAPI AppM
serviceStorefrontAdminServer user =
       (\status limit offset -> requireAccess *> listOrdersAdminHandler status limit offset)
  :<|> (\orderId request -> requireAccess *> updateOrderAdminHandler orderId request)
  :<|> (requireAccess *> listPackagesAdminHandler)
  :<|> (\request -> requireAccess *> createPackageAdminHandler request)
  :<|> (\packageId request -> requireAccess *> updatePackageAdminHandler packageId request)
  :<|> (\orderId -> requireAccess *> listServiceRefundsHandler orderId)
  :<|> (\orderId idempotency request ->
          requireAccess *> requestServiceRefundHandler user orderId idempotency request)
  :<|> (\refundId -> requireAccess *> approveServiceRefundHandler user refundId)
  :<|> (\orderId -> requireAccess *> reconcileServiceOrderHandler orderId)
  where
    requireAccess = unless (hasStrictAdminAccess user) $
      throwError err403 { errBody = "Strict Admin access required" }

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
      checkoutEnvironment <- loadConfiguredCheckoutEnvironment
      domainEnabled <- liftIO $ flip runSqlPool envPool $
        Checkout.domainEnabledForEnvironment checkoutEnvironment "mixing_mastering"
      unless domainEnabled $
        throwError err503
          { errBody = "Mixing/mastering checkout is disabled for this environment" }
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
                , ME.serviceStorefrontOrderCheckoutId = Nothing
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

              checkoutSnapshot = object
                [ "domain" .= ("mixing_mastering" :: Text)
                , "package_id" .= toPathPiece packageId
                , "package_name" .= ME.serviceStorefrontPackageName pkg
                , "service_kind" .= ME.serviceStorefrontPackageServiceKind pkg
                , "tier" .= ME.serviceStorefrontPackageTier pkg
                , "song_count" .= songCount
                , "price_minor" .= ME.serviceStorefrontPackagePriceUsdCents pkg
                , "currency" .= ME.serviceStorefrontPackageCurrency pkg
                , "turnaround_days" .= ME.serviceStorefrontPackageTurnaroundDays pkg
                , "included_revisions" .= ME.serviceStorefrontPackageRevisionCount pkg
                ]
              productVersion =
                T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
                  (ME.serviceStorefrontPackageUpdatedAt pkg))
              checkoutExpiry = addUTCTime (24 * 60 * 60) now

          creationResult <- liftIO $ flip runSqlPool envPool $ do
            mOrderIdKey <- insertUnique order
            case mOrderIdKey of
              Nothing -> pure Nothing
              Just orderIdKey -> do
                checkout <- Checkout.createCheckout Checkout.CheckoutCreation
                  { Checkout.ccDomainType = "mixing_mastering"
                  , Checkout.ccDomainOrderId = toPathPiece orderIdKey
                  , Checkout.ccEnvironment = checkoutEnvironment
                  , Checkout.ccCurrency = ME.serviceStorefrontPackageCurrency pkg
                  , Checkout.ccAmountMinor = fromIntegral (ME.serviceStorefrontPackagePriceUsdCents pkg)
                  , Checkout.ccCustomerEmail = buyerEmail
                  , Checkout.ccLookupTokenHash = hashLookupToken lookupToken
                  , Checkout.ccIdempotencyKey = idempotencyKey
                  , Checkout.ccExpiresAt = checkoutExpiry
                  , Checkout.ccProductType = "service_storefront_package"
                  , Checkout.ccProductId = toPathPiece packageId
                  , Checkout.ccProductVersion = productVersion
                  , Checkout.ccDescription = ME.serviceStorefrontPackageName pkg
                  , Checkout.ccSnapshot = checkoutSnapshot
                  , Checkout.ccCorrelationId = "service-order-create:" <> orderNumber
                  }
                checkoutUuid <- maybe
                  (fail "Checkout store generated an invalid UUID")
                  pure
                  (fromText (Checkout.checkoutReferenceId checkout))
                update orderIdKey
                  [ME.ServiceStorefrontOrderCheckoutId =. Just checkoutUuid]
                _ <- insert ME.ServiceStorefrontOrderStatusChange
                  { ME.serviceStorefrontOrderStatusChangeOrderId = orderIdKey
                  , ME.serviceStorefrontOrderStatusChangeStatus = "awaiting_payment"
                  , ME.serviceStorefrontOrderStatusChangeNotes = Just "Order and canonical checkout created"
                  , ME.serviceStorefrontOrderStatusChangeChangedBy = Just "system"
                  , ME.serviceStorefrontOrderStatusChangeCreatedAt = now
                  }
                pure (Just (orderIdKey, checkoutUuid))
          case creationResult of
            Nothing -> do
              raced <- liftIO $ flip runSqlPool envPool $
                getBy (ME.UniqueServiceStorefrontOrderCreateIdempotency (Just idempotencyKey))
              maybe (throwError err409 { errBody = "Order creation conflicted; retry with the same idempotency key" })
                (replayExistingOrder requestHash) raced
            Just (orderIdKey, checkoutUuid) ->
              pure (orderToDTOWithLookupToken (Just lookupToken) orderIdKey
                order { ME.serviceStorefrontOrderCheckoutId = Just checkoutUuid })

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
      when (status `notElem` ["awaiting_payment", "pending_payment", "payment_failed", "datafast_pending"]) $
        throwError err400 { errBody = "Order is not in a payable state" }
      
      let totalCents = ME.serviceStorefrontOrderPriceUsdCents order
          currency = ME.serviceStorefrontOrderCurrency order
          buyerName = ME.serviceStorefrontOrderBuyerName order
          buyerEmail = ME.serviceStorefrontOrderBuyerEmail order
          buyerPhone = ME.serviceStorefrontOrderBuyerPhone order

      dfEnv <- loadServiceDatafastEnv
      (checkout, attempt) <- beginCanonicalPaymentAttempt
        oid order (sdfEnvironment dfEnv) Checkout.ProviderDatafast
        Checkout.OperationCreate (sdfEntityId dfEnv) "create"
      
      (checkoutId, widgetUrl) <- case (status, ME.serviceStorefrontOrderDatafastCheckoutId order) of
        ("datafast_pending", Just existingCheckoutId) -> do
          let baseUrlClean = stripTrailingSlash (sdfBaseUrl dfEnv)
          pure (existingCheckoutId, baseUrlClean ++ "/v1/paymentWidgets.js?checkoutId=" ++ T.unpack existingCheckoutId)
        _ -> requestDatafastCheckoutForService
          (toPathPiece oid) totalCents currency buyerName buyerEmail buyerPhone
          `catchError` failCanonicalPaymentAttempt
            checkout attempt Checkout.ProviderDatafast "datafast_checkout_create"
      
      bindingResult <- liftIO $ flip runSqlPool envPool $ do
        result <- Checkout.bindProviderResource Checkout.ProviderBindingCreation
          { Checkout.pbcAttempt = attempt
          , Checkout.pbcCheckout = checkout
          , Checkout.pbcProvider = Checkout.ProviderDatafast
          , Checkout.pbcEnvironment = sdfEnvironment dfEnv
          , Checkout.pbcMerchantRef = sdfEntityId dfEnv
          , Checkout.pbcResourceType = "checkout"
          , Checkout.pbcProviderResource = checkoutId
          , Checkout.pbcResourcePath = Just ("/v1/checkouts/" <> checkoutId)
          , Checkout.pbcOrderReference = toPathPiece oid
          , Checkout.pbcAmountMinor = fromIntegral totalCents
          , Checkout.pbcCurrency = currency
          , Checkout.pbcStage = Checkout.AttemptRequiresCustomerAction
          , Checkout.pbcOccurredAt = now
          , Checkout.pbcCorrelationId = paymentCorrelationId oid "datafast" "create"
          }
        case result of
          Left message -> pure (Left message)
          Right () -> do
            update oid
              [ ME.ServiceStorefrontOrderStatus =. "datafast_pending"
              , ME.ServiceStorefrontOrderPaymentProvider =. Just "datafast"
              , ME.ServiceStorefrontOrderDatafastCheckoutId =. Just checkoutId
              , ME.ServiceStorefrontOrderUpdatedAt =. now
              ]
            pure (Right ())
      either (throwError . providerValidationError) pure bindingResult
      
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
      if isServicePaymentConfirmed order
        then pure (orderToDTO oid order)
        else do
          dfEnv <- loadServiceDatafastEnv
          paymentStatus <- checkDatafastPaymentStatus resourcePath
          now <- liftIO getCurrentTime
          let resultCode = sdfpsResultCode paymentStatus
              providerSuccess = isDatafastPaymentSuccess (sdfEnvironment dfEnv) resultCode
              providerPending = resultCode == "000.200.000"
              totalCents = ME.serviceStorefrontOrderPriceUsdCents order
              currency = ME.serviceStorefrontOrderCurrency order
          result <- if providerSuccess
            then do
              let validation = validateDatafastSuccessfulPayment
                    (toPathPiece oid) totalCents currency paymentStatus
                  actualAmount = sdfpsAmount paymentStatus >>= either (const Nothing) Just . parseDatafastCents
              paymentId <- maybe
                (pure resourcePath)
                pure
                (sdfpsPaymentId paymentStatus)
              (checkout, attempt) <- beginCanonicalPaymentAttempt
                oid order (sdfEnvironment dfEnv) Checkout.ProviderDatafast
                Checkout.OperationCapture (sdfEntityId dfEnv) "capture"
              case validation of
                Left message -> providerVerificationMismatch
                  checkout attempt Checkout.ProviderDatafast (sdfEnvironment dfEnv)
                  (sdfEntityId dfEnv) (toPathPiece oid) paymentId
                  (fromIntegral totalCents) (fromIntegral <$> actualAmount)
                  currency message
                Right () -> pure ()
              liftIO $ flip runSqlPool envPool $ do
                binding <- Checkout.bindProviderResource Checkout.ProviderBindingCreation
                  { Checkout.pbcAttempt = attempt
                  , Checkout.pbcCheckout = checkout
                  , Checkout.pbcProvider = Checkout.ProviderDatafast
                  , Checkout.pbcEnvironment = sdfEnvironment dfEnv
                  , Checkout.pbcMerchantRef = sdfEntityId dfEnv
                  , Checkout.pbcResourceType = "payment"
                  , Checkout.pbcProviderResource = paymentId
                  , Checkout.pbcResourcePath = Just resourcePath
                  , Checkout.pbcOrderReference = toPathPiece oid
                  , Checkout.pbcAmountMinor = fromIntegral totalCents
                  , Checkout.pbcCurrency = currency
                  , Checkout.pbcStage = Checkout.AttemptProcessing
                  , Checkout.pbcOccurredAt = now
                  , Checkout.pbcCorrelationId = paymentCorrelationId oid "datafast" "capture"
                  }
                case binding of
                  Left message -> pure (Left message)
                  Right () -> do
                    verified <- Checkout.recordVerifiedPayment Checkout.VerifiedPayment
                      { Checkout.vpAttempt = attempt
                      , Checkout.vpCheckout = checkout
                      , Checkout.vpProvider = Checkout.ProviderDatafast
                      , Checkout.vpEnvironment = sdfEnvironment dfEnv
                      , Checkout.vpMerchantRef = sdfEntityId dfEnv
                      , Checkout.vpResourceType = "payment"
                      , Checkout.vpProviderResource = paymentId
                      , Checkout.vpOrderReference = toPathPiece oid
                      , Checkout.vpAmountMinor = fromIntegral totalCents
                      , Checkout.vpCurrency = currency
                      , Checkout.vpEvidence = "server_to_server"
                      , Checkout.vpOccurredAt = now
                      , Checkout.vpCorrelationId = paymentCorrelationId oid "datafast" "capture"
                      }
                    case verified of
                      Left message -> pure (Left message)
                      Right newlyPaid -> do
                        when newlyPaid $ do
                          update oid
                            [ ME.ServiceStorefrontOrderStatus =. "paid"
                            , ME.ServiceStorefrontOrderDatafastResourcePath =. Just resourcePath
                            , ME.ServiceStorefrontOrderDatafastPaymentId =. Just paymentId
                            , ME.ServiceStorefrontOrderPaidAt =. Just now
                            , ME.ServiceStorefrontOrderUpdatedAt =. now
                            ]
                          insertServiceStatusChange oid "paid"
                            ("Datafast server verification result: " <> resultCode)
                            "datafast_server_verification" now
                        pure (Right ())
            else if providerPending
              then do
                (checkout, attempt) <- beginCanonicalPaymentAttempt
                  oid order (sdfEnvironment dfEnv) Checkout.ProviderDatafast
                  Checkout.OperationCreate (sdfEntityId dfEnv) "create"
                liftIO $ flip runSqlPool envPool $ do
                  Checkout.recordPaymentProcessing checkout attempt Checkout.ProviderDatafast
                    (paymentCorrelationId oid "datafast" "status") now
                  update oid
                    [ ME.ServiceStorefrontOrderStatus =. "datafast_pending"
                    , ME.ServiceStorefrontOrderDatafastResourcePath =. Just resourcePath
                    , ME.ServiceStorefrontOrderUpdatedAt =. now
                    ]
                  pure (Right ())
              else do
                (checkout, attempt) <- beginCanonicalPaymentAttempt
                  oid order (sdfEnvironment dfEnv) Checkout.ProviderDatafast
                  Checkout.OperationCapture (sdfEntityId dfEnv) "capture"
                liftIO $ flip runSqlPool envPool $ do
                  Checkout.recordPaymentFailure checkout attempt Checkout.ProviderDatafast
                    ("datafast_" <> resultCode)
                    (paymentCorrelationId oid "datafast" "status") now
                  update oid
                    [ ME.ServiceStorefrontOrderStatus =. "payment_failed"
                    , ME.ServiceStorefrontOrderDatafastResourcePath =. Just resourcePath
                    , ME.ServiceStorefrontOrderDatafastPaymentId =.
                        (sdfpsPaymentId paymentStatus <|>
                          ME.serviceStorefrontOrderDatafastPaymentId order)
                    , ME.ServiceStorefrontOrderUpdatedAt =. now
                    ]
                  when (ME.serviceStorefrontOrderStatus order /= "payment_failed") $
                    insertServiceStatusChange oid "payment_failed"
                      ("Datafast server verification result: " <> resultCode)
                      "datafast_server_verification" now
                  pure (Right ())
          either (throwError . providerValidationError) pure result
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
      when (status `notElem` ["awaiting_payment", "pending_payment", "payment_failed", "paypal_pending"]) $
        throwError err400 { errBody = "Order is not in a payable state" }
      
      let totalCents = ME.serviceStorefrontOrderPriceUsdCents order
          currency = ME.serviceStorefrontOrderCurrency order
          buyerName = ME.serviceStorefrontOrderBuyerName order
          buyerEmail = ME.serviceStorefrontOrderBuyerEmail order

      (cid, sec, baseUrl, paypalEnvironment, merchantRef) <- loadPaypalEnvForService
      (checkout, attempt) <- beginCanonicalPaymentAttempt
        oid order paypalEnvironment Checkout.ProviderPayPal
        Checkout.OperationCreate merchantRef "create"
      
      (ppOrderId, approvalUrl) <- case (status, ME.serviceStorefrontOrderPaypalOrderId order) of
        ("paypal_pending", Just existingOrderId) -> pure (existingOrderId, Nothing)
        _ -> do
          manager <- liftIO newTlsManager
          createPaypalOrderRemoteForService
            manager cid sec baseUrl (toPathPiece oid) totalCents currency buyerName buyerEmail
            `catchError` failCanonicalPaymentAttempt
              checkout attempt Checkout.ProviderPayPal "paypal_order_create"
      
      bindingResult <- liftIO $ flip runSqlPool envPool $ do
        result <- Checkout.bindProviderResource Checkout.ProviderBindingCreation
          { Checkout.pbcAttempt = attempt
          , Checkout.pbcCheckout = checkout
          , Checkout.pbcProvider = Checkout.ProviderPayPal
          , Checkout.pbcEnvironment = paypalEnvironment
          , Checkout.pbcMerchantRef = merchantRef
          , Checkout.pbcResourceType = "order"
          , Checkout.pbcProviderResource = ppOrderId
          , Checkout.pbcResourcePath = Just ("/v2/checkout/orders/" <> ppOrderId)
          , Checkout.pbcOrderReference = toPathPiece oid
          , Checkout.pbcAmountMinor = fromIntegral totalCents
          , Checkout.pbcCurrency = currency
          , Checkout.pbcStage = Checkout.AttemptRequiresCustomerAction
          , Checkout.pbcOccurredAt = now
          , Checkout.pbcCorrelationId = paymentCorrelationId oid "paypal" "create"
          }
        case result of
          Left message -> pure (Left message)
          Right () -> do
            update oid
              [ ME.ServiceStorefrontOrderStatus =. "paypal_pending"
              , ME.ServiceStorefrontOrderPaymentProvider =. Just "paypal"
              , ME.ServiceStorefrontOrderPaypalOrderId =. Just ppOrderId
              , ME.ServiceStorefrontOrderUpdatedAt =. now
              ]
            pure (Right ())
      either (throwError . providerValidationError) pure bindingResult
      
      pure PaypalCreateDTO
        { pcOrderId = ME.serviceStorefrontOrderOrderNumber order
        , pcPaypalOrderId = ppOrderId
        , pcApprovalUrl = approvalUrl
        }

capturePaypalHandler :: Maybe Text -> ServiceStorefrontPaypalCaptureReq -> AppM ServiceStorefrontOrderDTO
capturePaypalHandler mLookupToken ServiceStorefrontPaypalCaptureReq{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  mEntity <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber pcCaptureOrderId)
  case mEntity of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity oid order) -> do
      requireOrderLookupToken mLookupToken order
      case ME.serviceStorefrontOrderPaypalOrderId order of
        Nothing -> throwError err400 { errBody = "Order has no PayPal order" }
        Just storedPpOrderId | storedPpOrderId /= pcCapturePaypalId ->
          throwError err400 { errBody = "PayPal order ID mismatch" }
        _ -> pure ()
      if isServicePaymentConfirmed order
        then pure (orderToDTO oid order)
        else do
          (cid, sec, baseUrl, paypalEnvironment, merchantRef) <- loadPaypalEnvForService
          (checkout, attempt) <- beginCanonicalPaymentAttempt
            oid order paypalEnvironment Checkout.ProviderPayPal
            Checkout.OperationCapture merchantRef "capture"
          manager <- liftIO newTlsManager
          captureOutcome <- capturePaypalOrderRemoteForService
            manager cid sec baseUrl pcCapturePaypalId
            `catchError` failCanonicalPaymentAttempt
              checkout attempt Checkout.ProviderPayPal "paypal_capture"
          when (spcoStatus captureOutcome == "COMPLETED") $ do
            let validation = validatePaypalSuccessfulCapture
                  (toPathPiece oid)
                  (ME.serviceStorefrontOrderPriceUsdCents order)
                  (ME.serviceStorefrontOrderCurrency order)
                  merchantRef
                  captureOutcome
                actualAmount = spcoAmount captureOutcome >>= either (const Nothing) Just . parseDatafastCents
                providerReference = fromMaybe pcCapturePaypalId (spcoCaptureId captureOutcome)
            case validation of
              Left message -> providerVerificationMismatch
                checkout attempt Checkout.ProviderPayPal paypalEnvironment merchantRef
                (toPathPiece oid) providerReference
                (fromIntegral (ME.serviceStorefrontOrderPriceUsdCents order))
                (fromIntegral <$> actualAmount)
                (ME.serviceStorefrontOrderCurrency order) message
              Right () -> pure ()
          let nextStatus :: Text
              nextStatus
                | spcoStatus captureOutcome == "COMPLETED" = "paid"
                | spcoStatus captureOutcome `elem` ["APPROVED", "PENDING"] = "paypal_pending"
                | otherwise = "payment_failed"

          result <- case nextStatus of
            "paid" -> do
              captureId <- maybe
                (throwError (providerValidationError "PayPal capture ID is required"))
                pure
                (spcoCaptureId captureOutcome)
              liftIO $ flip runSqlPool envPool $ do
                binding <- Checkout.bindProviderResource Checkout.ProviderBindingCreation
                  { Checkout.pbcAttempt = attempt
                  , Checkout.pbcCheckout = checkout
                  , Checkout.pbcProvider = Checkout.ProviderPayPal
                  , Checkout.pbcEnvironment = paypalEnvironment
                  , Checkout.pbcMerchantRef = merchantRef
                  , Checkout.pbcResourceType = "capture"
                  , Checkout.pbcProviderResource = captureId
                  , Checkout.pbcResourcePath = Just
                      ("/v2/checkout/orders/" <> pcCapturePaypalId <> "/capture")
                  , Checkout.pbcOrderReference = toPathPiece oid
                  , Checkout.pbcAmountMinor = fromIntegral
                      (ME.serviceStorefrontOrderPriceUsdCents order)
                  , Checkout.pbcCurrency = ME.serviceStorefrontOrderCurrency order
                  , Checkout.pbcStage = Checkout.AttemptProcessing
                  , Checkout.pbcOccurredAt = now
                  , Checkout.pbcCorrelationId = paymentCorrelationId oid "paypal" "capture"
                  }
                case binding of
                  Left message -> pure (Left message)
                  Right () -> do
                    verified <- Checkout.recordVerifiedPayment Checkout.VerifiedPayment
                      { Checkout.vpAttempt = attempt
                      , Checkout.vpCheckout = checkout
                      , Checkout.vpProvider = Checkout.ProviderPayPal
                      , Checkout.vpEnvironment = paypalEnvironment
                      , Checkout.vpMerchantRef = merchantRef
                      , Checkout.vpResourceType = "capture"
                      , Checkout.vpProviderResource = captureId
                      , Checkout.vpOrderReference = toPathPiece oid
                      , Checkout.vpAmountMinor = fromIntegral
                          (ME.serviceStorefrontOrderPriceUsdCents order)
                      , Checkout.vpCurrency = ME.serviceStorefrontOrderCurrency order
                      , Checkout.vpEvidence = "server_to_server"
                      , Checkout.vpOccurredAt = now
                      , Checkout.vpCorrelationId = paymentCorrelationId oid "paypal" "capture"
                      }
                    case verified of
                      Left message -> pure (Left message)
                      Right newlyPaid -> do
                        when newlyPaid $ do
                          update oid
                            [ ME.ServiceStorefrontOrderStatus =. "paid"
                            , ME.ServiceStorefrontOrderPaypalCaptureId =. Just captureId
                            , ME.ServiceStorefrontOrderPaypalPayerEmail =.
                                (spcoPayerEmail captureOutcome <|>
                                  ME.serviceStorefrontOrderPaypalPayerEmail order)
                            , ME.ServiceStorefrontOrderPaidAt =. Just now
                            , ME.ServiceStorefrontOrderUpdatedAt =. now
                            ]
                          insertServiceStatusChange oid "paid"
                            ("PayPal server capture: " <> spcoStatus captureOutcome)
                            "paypal_server_capture" now
                        pure (Right ())
            "paypal_pending" -> do
              liftIO $ flip runSqlPool envPool $ do
                Checkout.recordPaymentProcessing checkout attempt Checkout.ProviderPayPal
                  (paymentCorrelationId oid "paypal" "capture") now
                update oid
                  [ ME.ServiceStorefrontOrderStatus =. "paypal_pending"
                  , ME.ServiceStorefrontOrderPaypalPayerEmail =.
                      (spcoPayerEmail captureOutcome <|>
                        ME.serviceStorefrontOrderPaypalPayerEmail order)
                  , ME.ServiceStorefrontOrderUpdatedAt =. now
                  ]
                when (ME.serviceStorefrontOrderStatus order /= "paypal_pending") $
                  insertServiceStatusChange oid "paypal_pending"
                    ("PayPal server capture: " <> spcoStatus captureOutcome)
                    "paypal_server_capture" now
                pure (Right ())
            _ -> do
              liftIO $ flip runSqlPool envPool $ do
                Checkout.recordPaymentFailure checkout attempt Checkout.ProviderPayPal
                  ("paypal_" <> T.toLower (spcoStatus captureOutcome))
                  (paymentCorrelationId oid "paypal" "capture") now
                update oid
                  [ ME.ServiceStorefrontOrderStatus =. "payment_failed"
                  , ME.ServiceStorefrontOrderUpdatedAt =. now
                  ]
                when (ME.serviceStorefrontOrderStatus order /= "payment_failed") $
                  insertServiceStatusChange oid "payment_failed"
                    ("PayPal server capture: " <> spcoStatus captureOutcome)
                    "paypal_server_capture" now
                pure (Right ())
          either (throwError . providerValidationError) pure result
          mUpdated <- liftIO $ flip runSqlPool envPool $ get oid
          case mUpdated of
            Nothing -> throwError err500 { errBody = "Failed to load updated order" }
            Just updated -> pure (orderToDTO oid updated)

paypalWebhookHandler
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> BL.ByteString
  -> AppM NoContent
paypalWebhookHandler transmissionId transmissionTime certUrl authAlgo transmissionSig rawBody = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  headers <- either (throwError . badRequestText) pure $
    validatePaypalWebhookHeaders
      now transmissionId transmissionTime certUrl authAlgo transmissionSig
  envelope <- either (throwError . badRequestText) pure $
    parsePaypalWebhookEnvelope rawBody
  (cid, sec, baseUrl, paypalEnvironment, merchantRef) <- loadPaypalEnvForService
  webhookId <- loadRequiredSafeEnv "PAYPAL_WEBHOOK_ID" 50
  encryptionKey <- loadProviderEventEncryptionKey
  unless (paypalCertUrlMatchesEnvironment paypalEnvironment (pwhCertUrl headers)) $
    throwError err400 { errBody = "PayPal certificate URL does not match the configured environment" }
  enabled <- liftIO $ flip runSqlPool envPool $
    Checkout.capabilityEnabledForEnvironment paypalEnvironment "checkout.paypal.webhooks"
  unless enabled $
    throwError err503 { errBody = "PayPal webhook processing is disabled for this environment" }
  manager <- liftIO newTlsManager
  signatureVerified <- verifyPaypalWebhookRemote
    manager cid sec baseUrl webhookId headers rawBody
  unless signatureVerified $
    throwError err401 { errBody = "PayPal webhook signature verification failed" }
  storedResult <- liftIO $ flip runSqlPool envPool $
    ProviderEvent.storeVerifiedProviderEvent ProviderEvent.ProviderEventCreation
      { ProviderEvent.pecProvider = Checkout.ProviderPayPal
      , ProviderEvent.pecEnvironment = paypalEnvironment
      , ProviderEvent.pecMerchantRef = merchantRef
      , ProviderEvent.pecProviderEventId = pweEventId envelope
      , ProviderEvent.pecEventType = pweEventType envelope
      , ProviderEvent.pecProviderCreatedAt = Just (pweCreatedAt envelope)
      , ProviderEvent.pecProviderResource = paypalWebhookResourceId envelope
      , ProviderEvent.pecRawPayload = BL.toStrict rawBody
      , ProviderEvent.pecEncryptionKey = encryptionKey
      , ProviderEvent.pecReceivedAt = now
      }
  stored <- either (throwError . providerValidationError) pure storedResult
  claim <- liftIO $ flip runSqlPool envPool $
    ProviderEvent.claimProviderEvent (ProviderEvent.pesReference stored) now
  case claim of
    ProviderEvent.ProviderEventAlreadyHandled _ -> pure NoContent
    ProviderEvent.ProviderEventBusy ->
      throwError err503 { errBody = "Verified PayPal event is already processing or awaiting retry" }
    ProviderEvent.ProviderEventClaimed attemptCount -> do
      outcome <- processPaypalWebhookEvent
        paypalEnvironment merchantRef envelope now
      case outcome of
        PaypalEventProcessed checkoutId attemptId refundId -> do
          liftIO $ flip runSqlPool envPool $
            ProviderEvent.markProviderEventProcessed
              (ProviderEvent.pesReference stored) checkoutId attemptId refundId now
          pure NoContent
        PaypalEventIgnored -> do
          liftIO $ flip runSqlPool envPool $
            ProviderEvent.markProviderEventIgnored
              (ProviderEvent.pesReference stored) Nothing Nothing Nothing now
          pure NoContent
        PaypalEventPermanentFailure summary checkoutId attemptId refundId -> do
          liftIO $ flip runSqlPool envPool $
            ProviderEvent.markProviderEventDeadLetter
              (ProviderEvent.pesReference stored)
              checkoutId attemptId refundId summary now
          pure NoContent
        PaypalEventRetry summary -> do
          exhausted <- liftIO $ flip runSqlPool envPool $
            ProviderEvent.markProviderEventRetry
              (ProviderEvent.pesReference stored) attemptCount summary now
          if exhausted
            then pure NoContent
            else throwError err503 { errBody = "Verified PayPal event is queued for retry" }

processPaypalWebhookEvent
  :: Checkout.CheckoutEnvironment
  -> Text
  -> PaypalWebhookEnvelope
  -> UTCTime
  -> AppM PaypalEventProcessResult
processPaypalWebhookEvent environment merchantRef envelope now =
  case pweEventType envelope of
    "PAYMENT.CAPTURE.COMPLETED" -> processCompletedCapture
    "PAYMENT.CAPTURE.REFUNDED" -> processExternalCaptureChange
      "external_refund_detected"
    "PAYMENT.CAPTURE.REVERSED" -> processExternalCaptureChange
      "external_reversal_detected"
    _ -> pure PaypalEventIgnored
  where
    processCompletedCapture = case parsePaypalWebhookCapture envelope of
      Left message -> pure (PaypalEventPermanentFailure message Nothing Nothing Nothing)
      Right capture -> do
        Env{..} <- ask
        result <- liftIO $ tryAny $ flip runSqlPool envPool $ do
          mBound <- loadBoundPaypalOrder environment merchantRef (pwcPaypalOrderId capture)
          case mBound of
            Nothing -> pure PaypalEventIgnored
            Just bound -> case validatePaypalWebhookCaptureBinding merchantRef bound capture of
              Left message -> do
                recordPaypalWebhookMismatch environment (pweCreatedAt envelope) bound capture message
                pure (PaypalEventPermanentFailure message
                  (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing)
              Right () -> do
                let checkout = Checkout.CheckoutReference (bpcCheckoutId bound)
                    attempt = Checkout.PaymentAttemptReference (bpcAttemptId bound)
                    correlationId = "paypal-webhook:" <> pweEventId envelope
                binding <- Checkout.bindProviderResource Checkout.ProviderBindingCreation
                  { Checkout.pbcAttempt = attempt
                  , Checkout.pbcCheckout = checkout
                  , Checkout.pbcProvider = Checkout.ProviderPayPal
                  , Checkout.pbcEnvironment = environment
                  , Checkout.pbcMerchantRef = merchantRef
                  , Checkout.pbcResourceType = "capture"
                  , Checkout.pbcProviderResource = pwcCaptureId capture
                  , Checkout.pbcResourcePath = Just
                      ("/v2/checkout/orders/" <> pwcPaypalOrderId capture <> "/capture")
                  , Checkout.pbcOrderReference = bpcDomainOrderId bound
                  , Checkout.pbcAmountMinor = bpcExpectedAmount bound
                  , Checkout.pbcCurrency = bpcCurrency bound
                  , Checkout.pbcStage = Checkout.AttemptProcessing
                  , Checkout.pbcOccurredAt = pweCreatedAt envelope
                  , Checkout.pbcCorrelationId = correlationId
                  }
                case binding of
                  Left message -> do
                    recordPaypalWebhookMismatch environment (pweCreatedAt envelope) bound capture message
                    pure (PaypalEventPermanentFailure message
                      (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing)
                  Right () -> do
                    verified <- Checkout.recordVerifiedPayment Checkout.VerifiedPayment
                      { Checkout.vpAttempt = attempt
                      , Checkout.vpCheckout = checkout
                      , Checkout.vpProvider = Checkout.ProviderPayPal
                      , Checkout.vpEnvironment = environment
                      , Checkout.vpMerchantRef = merchantRef
                      , Checkout.vpResourceType = "capture"
                      , Checkout.vpProviderResource = pwcCaptureId capture
                      , Checkout.vpOrderReference = bpcDomainOrderId bound
                      , Checkout.vpAmountMinor = bpcExpectedAmount bound
                      , Checkout.vpCurrency = bpcCurrency bound
                      , Checkout.vpEvidence = "signature_verified_webhook"
                      , Checkout.vpOccurredAt = pweCreatedAt envelope
                      , Checkout.vpCorrelationId = correlationId
                      }
                    case verified of
                      Left message -> do
                        recordPaypalWebhookMismatch environment (pweCreatedAt envelope) bound capture message
                        pure (PaypalEventPermanentFailure message
                          (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing)
                      Right newlyPaid -> do
                        when newlyPaid $ do
                          rawExecute
                            "UPDATE service_storefront_order SET status = 'paid',\
                            \ paypal_capture_id = ?, paid_at = COALESCE(paid_at, ?), updated_at = ?\
                            \ WHERE id = ?::uuid AND checkout_id = ?::uuid\
                            \ AND paypal_order_id = ?"
                            [ PersistText (pwcCaptureId capture)
                            , PersistUTCTime (pweCreatedAt envelope)
                            , PersistUTCTime now
                            , PersistText (bpcDomainOrderId bound)
                            , PersistText (bpcCheckoutId bound)
                            , PersistText (pwcPaypalOrderId capture)
                            ]
                          rawExecute
                            "INSERT INTO service_storefront_order_status_change\
                            \ (order_id, status, notes, changed_by, created_at)\
                            \ VALUES (?::uuid, 'paid',\
                            \ 'PayPal capture completed through a signature-verified webhook',\
                            \ 'paypal_signature_verified_webhook', ?)"
                            [ PersistText (bpcDomainOrderId bound)
                            , PersistUTCTime now
                            ]
                        pure (PaypalEventProcessed
                          (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing)
        pure $ case result of
          Left _ -> PaypalEventRetry "PayPal capture event database processing failed"
          Right outcome -> outcome

    processExternalCaptureChange exceptionType =
      case parsePaypalWebhookCapture envelope of
        Left _ -> do
          Env{..} <- ask
          result <- liftIO $ tryAny $ flip runSqlPool envPool $
            Checkout.recordReconciliationException
              Checkout.ProviderPayPal environment merchantRef exceptionType
              ("provider-event:" <> pweEventId envelope)
              (fromMaybe (pweEventId envelope) (paypalWebhookResourceId envelope))
              0 Nothing "USD" now
          pure $ case result of
            Left _ -> PaypalEventRetry "Malformed PayPal refund or reversal event could not be recorded"
            Right () -> PaypalEventProcessed Nothing Nothing Nothing
        Right capture -> do
          Env{..} <- ask
          result <- liftIO $ tryAny $ flip runSqlPool envPool $ do
            mBound <- loadBoundPaypalCapture environment merchantRef (pwcCaptureId capture)
            case mBound of
              Nothing -> pure PaypalEventIgnored
              Just bound -> do
                let actualAmount = fromIntegral <$> either (const Nothing) Just
                      (parseDatafastCents (pwcAmount capture))
                Checkout.recordReconciliationException
                  Checkout.ProviderPayPal environment merchantRef exceptionType
                  (bpcDomainOrderId bound) (pwcCaptureId capture)
                  (bpcExpectedAmount bound) actualAmount (bpcCurrency bound) now
                pure (PaypalEventProcessed
                  (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing)
          pure $ case result of
            Left _ -> PaypalEventRetry "PayPal refund or reversal event database processing failed"
            Right outcome -> outcome

loadBoundPaypalOrder
  :: Checkout.CheckoutEnvironment
  -> Text
  -> Text
  -> SqlPersistT IO (Maybe BoundPaypalCapture)
loadBoundPaypalOrder environment merchantRef paypalOrderId =
  loadBoundPaypalCaptureBy
    "binding.resource_type = 'order' AND binding.provider_resource_id = ?"
    environment merchantRef paypalOrderId

loadBoundPaypalCapture
  :: Checkout.CheckoutEnvironment
  -> Text
  -> Text
  -> SqlPersistT IO (Maybe BoundPaypalCapture)
loadBoundPaypalCapture environment merchantRef captureId =
  loadBoundPaypalCaptureBy
    "binding.resource_type = 'capture' AND binding.provider_resource_id = ?"
    environment merchantRef captureId

loadBoundPaypalCaptureBy
  :: Text
  -> Checkout.CheckoutEnvironment
  -> Text
  -> Text
  -> SqlPersistT IO (Maybe BoundPaypalCapture)
loadBoundPaypalCaptureBy bindingPredicate environment merchantRef providerResource = do
  rows <- (rawSql
    ("SELECT checkout.id::text, attempt.id::text, checkout.domain_order_id,\
     \ checkout.total_minor, checkout.currency, attempt.merchant_account_ref,\
     \ order_binding.provider_resource_id\
     \ FROM commerce_checkout_session checkout\
     \ JOIN commerce_payment_attempt attempt ON attempt.checkout_id = checkout.id\
     \ JOIN commerce_provider_binding binding ON binding.payment_attempt_id = attempt.id\
     \ JOIN commerce_provider_binding order_binding\
     \   ON order_binding.payment_attempt_id = attempt.id\
     \  AND order_binding.resource_type = 'order'\
     \ WHERE checkout.domain_type = 'mixing_mastering'\
     \ AND attempt.provider = 'paypal' AND attempt.environment = ?\
     \ AND attempt.merchant_account_ref = ? AND " <> bindingPredicate)
    [ PersistText (Checkout.checkoutEnvironmentText environment)
    , PersistText merchantRef
    , PersistText providerResource
    ] :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Int64
       , Single Text, Single Text, Single Text
       )])
  pure $ case rows of
    [( Single checkoutId, Single attemptId, Single domainOrderId, Single amount
     , Single currency, Single storedMerchant, Single paypalOrderId
     )] -> Just BoundPaypalCapture
        { bpcCheckoutId = checkoutId
        , bpcAttemptId = attemptId
        , bpcDomainOrderId = domainOrderId
        , bpcExpectedAmount = amount
        , bpcCurrency = currency
        , bpcMerchantRef = storedMerchant
        , bpcPaypalOrderId = paypalOrderId
        }
    _ -> Nothing

validatePaypalWebhookCaptureBinding
  :: Text
  -> BoundPaypalCapture
  -> PaypalWebhookCapture
  -> Either Text ()
validatePaypalWebhookCaptureBinding configuredMerchant bound capture = do
  capturedMinor <- fromIntegral <$> parseDatafastCents (pwcAmount capture)
  unless (pwcStatus capture == "COMPLETED") $
    Left "PayPal webhook capture is not completed"
  unless (pwcPaypalOrderId capture == bpcPaypalOrderId bound) $
    Left "PayPal webhook order ID does not match the immutable provider binding"
  unless (pwcMerchantId capture == configuredMerchant
      && pwcMerchantId capture == bpcMerchantRef bound) $
    Left "PayPal webhook merchant does not match the configured merchant binding"
  unless (capturedMinor == bpcExpectedAmount bound) $
    Left "PayPal webhook amount does not match the immutable checkout"
  unless (pwcCurrency capture == T.toUpper (T.strip (bpcCurrency bound))) $
    Left "PayPal webhook currency does not match the immutable checkout"

recordPaypalWebhookMismatch
  :: Checkout.CheckoutEnvironment
  -> UTCTime
  -> BoundPaypalCapture
  -> PaypalWebhookCapture
  -> Text
  -> SqlPersistT IO ()
recordPaypalWebhookMismatch environment occurredAt bound capture _ =
  Checkout.recordReconciliationException
    Checkout.ProviderPayPal
    environment
    (bpcMerchantRef bound)
    "provider_verification_mismatch"
    (bpcDomainOrderId bound)
    (pwcCaptureId capture)
    (bpcExpectedAmount bound)
    (fromIntegral <$> either (const Nothing) Just (parseDatafastCents (pwcAmount capture)))
    (bpcCurrency bound)
    occurredAt

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
      let provider = case methodName of
            "bank_transfer" -> Checkout.ProviderBankTransfer
            "cash" -> Checkout.ProviderCash
            _ -> Checkout.ProviderPos
      checkoutEnvironment <- loadConfiguredCheckoutEnvironment
      (checkout, attempt) <- beginCanonicalPaymentAttempt
        oid order checkoutEnvironment provider Checkout.OperationManualVerify
        "tdf-manual-settlement" ("manual-" <> methodName)
      liftIO $ flip runSqlPool envPool $ do
        Checkout.recordManualPaymentSelection checkout attempt provider
          (paymentCorrelationId oid methodName "manual-selection") now
        update oid
          [ ME.ServiceStorefrontOrderStatus =. "awaiting_manual_confirmation"
          , ME.ServiceStorefrontOrderPaymentProvider =. Just methodName
          , ME.ServiceStorefrontOrderUpdatedAt =. now
          ]
        when (ME.serviceStorefrontOrderStatus order /= "awaiting_manual_confirmation") $
          insertServiceStatusChange oid "awaiting_manual_confirmation"
            ("Customer selected " <> methodName <> "; staff verification required")
            "customer" now
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
      case ssouStatus of
        Just nextStatus ->
          either (throwError . badRequestText) pure $
            validateServiceFulfillmentTransition
              (ME.serviceStorefrontOrderStatus order)
              nextStatus
        Nothing -> pure ()
      let updatedOrder = order
            { ME.serviceStorefrontOrderStatus = fromMaybe (ME.serviceStorefrontOrderStatus order) ssouStatus
            , ME.serviceStorefrontOrderDeliverablesUrl = maybe (ME.serviceStorefrontOrderDeliverablesUrl order) Just ssouDeliverablesUrl
            , ME.serviceStorefrontOrderNotes = maybe (ME.serviceStorefrontOrderNotes order) (Just . T.strip) ssouNotes
            , ME.serviceStorefrontOrderUpdatedAt = now
            }
      liftIO $ flip runSqlPool envPool $ do
        -- Insert status change if status changed
        case ssouStatus of
          Just newStatus | newStatus /= ME.serviceStorefrontOrderStatus order -> do
            let statusChange = ME.ServiceStorefrontOrderStatusChange
                  { ME.serviceStorefrontOrderStatusChangeOrderId = oid
                  , ME.serviceStorefrontOrderStatusChangeStatus = newStatus
                  , ME.serviceStorefrontOrderStatusChangeNotes = Just "Status updated by admin"
                  , ME.serviceStorefrontOrderStatusChangeChangedBy = Just "admin"
                  , ME.serviceStorefrontOrderStatusChangeCreatedAt = now
                  }
            _ <- insert statusChange
            pure ()
          _ -> pure ()
        replace oid updatedOrder
      pure (orderToDTO oid updatedOrder)

listServiceRefundsHandler :: Text -> AppM [ServiceStorefrontRefundDTO]
listServiceRefundsHandler orderNumber = do
  Env{..} <- ask
  paidCheckout <- liftIO (flip runSqlPool envPool
      (loadServicePaidCheckoutByOrder orderNumber))
    >>= either (throwError . refundLookupError) pure
  refunds <- liftIO $ flip runSqlPool envPool $
    Refund.listCheckoutRefunds
      (Checkout.CheckoutReference (spcCheckoutId paidCheckout))
  pure (map (serviceRefundToDTO orderNumber) refunds)

requestServiceRefundHandler
  :: AuthedUser
  -> Text
  -> Maybe Text
  -> ServiceStorefrontRefundCreate
  -> AppM ServiceStorefrontRefundDTO
requestServiceRefundHandler user orderNumber mIdempotencyKey ServiceStorefrontRefundCreate{..} = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  idempotencyKey <- either (throwError . badRequestText) pure $
    validateIdempotencyKey mIdempotencyKey
  paidCheckout <- liftIO (flip runSqlPool envPool
      (loadServicePaidCheckoutByOrder orderNumber))
    >>= either (throwError . refundLookupError) pure
  provider <- either (throwError . configurationError) pure $
    paymentProviderFromText (spcProvider paidCheckout)
  environment <- either (throwError . configurationError) pure $
    checkoutEnvironmentFromText (spcEnvironment paidCheckout)
  unless (provider == Checkout.ProviderPayPal) $
    throwError err503
      { errBody = "Refund adapter is not enabled for this payment provider" }
  capabilityEnabled <- liftIO $ flip runSqlPool envPool $
    Checkout.capabilityEnabledForEnvironment environment "checkout.paypal.refunds"
  unless capabilityEnabled $
    throwError err503 { errBody = "PayPal refunds are disabled for this environment" }
  let remainingMinor = spcPaidMinor paidCheckout
        - spcRefundedMinor paidCheckout - spcReservedMinor paidCheckout
      requestedMinor = maybe remainingMinor fromIntegral ssrfcAmountUsdCents
  when (requestedMinor > fromIntegral (maxBound :: Int)) $
    throwError err409 { errBody = "Refund amount exceeds the supported API range" }
  result <- liftIO $ flip runSqlPool envPool $
    Refund.requestSingleLineRefund Refund.RefundCreation
      { Refund.rcCheckout = Checkout.CheckoutReference (spcCheckoutId paidCheckout)
      , Refund.rcPaymentAttempt = Checkout.PaymentAttemptReference (spcAttemptId paidCheckout)
      , Refund.rcProvider = provider
      , Refund.rcEnvironment = environment
      , Refund.rcMerchantRef = spcMerchantRef paidCheckout
      , Refund.rcAmountMinor = requestedMinor
      , Refund.rcCurrency = spcCurrency paidCheckout
      , Refund.rcReasonCode = ssrfcReasonCode
      , Refund.rcIdempotencyKey = idempotencyKey
      , Refund.rcRequestedBy = fromSqlKey (auPartyId user)
      , Refund.rcCreatedAt = now
      }
  record <- either (throwError . badRequestText) pure result
  pure (serviceRefundToDTO orderNumber record)

approveServiceRefundHandler
  :: AuthedUser
  -> Text
  -> AppM ServiceStorefrontRefundDTO
approveServiceRefundHandler user rawRefundId = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  refundRef <- parseRefundReference rawRefundId
  mExisting <- liftIO $ flip runSqlPool envPool $ Refund.loadRefund refundRef
  existing <- maybe (throwError err404 { errBody = "Refund not found" }) pure mExisting
  orderNumber <- liftIO (flip runSqlPool envPool
      (loadServiceOrderNumberForCheckout (Refund.rrCheckout existing)))
    >>= maybe (throwError err404 { errBody = "Service order not found" }) pure
  unless (Refund.rrProvider existing == "paypal") $
    throwError err503
      { errBody = "Refund adapter is not enabled for this payment provider" }
  storedEnvironment <- either (throwError . configurationError) pure $
    checkoutEnvironmentFromText (Refund.rrEnvironment existing)
  capabilityEnabled <- liftIO $ flip runSqlPool envPool $
    Checkout.capabilityEnabledForEnvironment storedEnvironment "checkout.paypal.refunds"
  unless capabilityEnabled $
    throwError err503 { errBody = "PayPal refunds are disabled for this environment" }
  paidCheckout <- liftIO (flip runSqlPool envPool
      (loadServicePaidCheckoutByCheckout (Refund.rrCheckout existing)))
    >>= either (throwError . refundLookupError) pure
  unless (spcAttemptId paidCheckout
      == Checkout.paymentAttemptReferenceId (Refund.rrPaymentAttempt existing)) $
    throwError err409 { errBody = "Refund payment binding is inconsistent" }
  (cid, sec, baseUrl, configuredEnvironment, configuredMerchant) <-
    loadPaypalEnvForService
  unless (configuredEnvironment == storedEnvironment
      && configuredMerchant == Refund.rrMerchantRef existing) $
    throwError err503
      { errBody = "PayPal configuration does not match immutable refund evidence" }
  approved <- liftIO $ flip runSqlPool envPool $
    Refund.approveRefundForProcessing refundRef (fromSqlKey (auPartyId user)) now
  (refundRecord, shouldIssue) <- either (throwError . refundConflictError) pure approved
  if not shouldIssue
    then pure (serviceRefundToDTO orderNumber refundRecord)
    else do
      manager <- liftIO newTlsManager
      outcome <- issuePaypalRefundRemote
        manager cid sec baseUrl (spcProviderResource paidCheckout) refundRecord
      let actualMinor = fromIntegral <$> either (const Nothing) Just
            (parseDatafastCents (proAmount outcome))
          outcomeMatches = actualMinor == Just (Refund.rrAmountMinor refundRecord)
            && proCurrency outcome == Refund.rrCurrency refundRecord
      unless outcomeMatches $ do
        liftIO $ flip runSqlPool envPool $ do
          Refund.recordRefundFailure refundRef "provider_verification_mismatch" now
          Checkout.recordReconciliationException
            Checkout.ProviderPayPal storedEnvironment configuredMerchant
            "refund_verification_mismatch"
            (Refund.refundReferenceId refundRef) (proRefundId outcome)
            (Refund.rrAmountMinor refundRecord) actualMinor
            (Refund.rrCurrency refundRecord) now
        throwError (providerValidationError
          "PayPal refund amount or currency does not match the immutable request")
      case proStatus outcome of
        "COMPLETED" -> do
          verified <- liftIO $ flip runSqlPool envPool $
            Refund.recordVerifiedRefund Refund.VerifiedRefund
              { Refund.vrRefund = refundRef
              , Refund.vrProviderRefund = proRefundId outcome
              , Refund.vrAmountMinor = Refund.rrAmountMinor refundRecord
              , Refund.vrCurrency = Refund.rrCurrency refundRecord
              , Refund.vrOccurredAt = now
              , Refund.vrCorrelationId =
                  "paypal-refund:" <> Refund.refundReferenceId refundRef
              }
          either (throwError . refundConflictError) (const (pure ())) verified
        "PENDING" -> do
          pending <- liftIO $ flip runSqlPool envPool $
            Refund.recordRefundPending refundRef (proRefundId outcome) now
          either (throwError . refundConflictError) (const (pure ())) pending
        providerStatus -> do
          liftIO $ flip runSqlPool envPool $
            Refund.recordRefundFailure
              refundRef ("paypal_" <> T.toLower providerStatus) now
          throwError err502 { errBody = "PayPal did not complete the refund" }
      updated <- liftIO $ flip runSqlPool envPool $ Refund.loadRefund refundRef
      maybe (throwError err500 { errBody = "Refund could not be reloaded" })
        (pure . serviceRefundToDTO orderNumber) updated

reconcileServiceOrderHandler
  :: Text
  -> AppM ServiceStorefrontReconciliationDTO
reconcileServiceOrderHandler orderNumber = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  mOrder <- liftIO $ flip runSqlPool envPool $
    getBy (ME.UniqueServiceStorefrontOrderNumber orderNumber)
  (oid, order) <- case mOrder of
    Nothing -> throwError err404 { errBody = "Order not found" }
    Just (Entity orderId value) -> pure (orderId, value)
  paidCheckout <- liftIO (flip runSqlPool envPool
      (loadServicePaidCheckoutByOrder orderNumber))
    >>= either (throwError . refundLookupError) pure
  case spcProvider paidCheckout of
    "paypal" -> do
      paypalOrderId <- maybe
        (throwError err409 { errBody = "Order has no PayPal order reference" })
        pure (ME.serviceStorefrontOrderPaypalOrderId order)
      (cid, sec, baseUrl, environment, merchantRef) <- loadPaypalEnvForService
      requireReconciliationBinding paidCheckout environment merchantRef
      manager <- liftIO newTlsManager
      outcome <- getPaypalOrderRemoteForService manager cid sec baseUrl paypalOrderId
      let actualMinor = spcoAmount outcome >>= either (const Nothing) Just . parseDatafastCents
          matched = spcoStatus outcome == "COMPLETED"
            && either (const False) (const True)
              (validatePaypalSuccessfulCapture
                (toPathPiece oid)
                (fromIntegral (spcPaidMinor paidCheckout))
                (spcCurrency paidCheckout) merchantRef outcome)
          providerReference = fromMaybe paypalOrderId (spcoCaptureId outcome)
      unless matched $ liftIO $ flip runSqlPool envPool $
        Checkout.recordReconciliationException
          Checkout.ProviderPayPal environment merchantRef
          "manual_reconciliation_mismatch" (toPathPiece oid) providerReference
          (spcPaidMinor paidCheckout) (fromIntegral <$> actualMinor)
          (spcCurrency paidCheckout) now
      pure ServiceStorefrontReconciliationDTO
        { ssrecOrderId = orderNumber
        , ssrecProvider = "paypal"
        , ssrecProviderReference = providerReference
        , ssrecExpectedAmount = fromIntegral (spcPaidMinor paidCheckout)
        , ssrecActualAmount = actualMinor
        , ssrecCurrency = spcCurrency paidCheckout
        , ssrecMatched = matched
        , ssrecCheckedAt = now
        }
    "datafast" -> do
      resourcePath <- maybe
        (throwError err409 { errBody = "Order has no Datafast resource path" })
        pure (ME.serviceStorefrontOrderDatafastResourcePath order)
      datafast <- loadServiceDatafastEnv
      requireReconciliationBinding paidCheckout
        (sdfEnvironment datafast) (sdfEntityId datafast)
      providerStatus <- checkDatafastPaymentStatus resourcePath
      let actualMinor = sdfpsAmount providerStatus
            >>= either (const Nothing) Just . parseDatafastCents
          matched = isDatafastPaymentSuccess
              (sdfEnvironment datafast) (sdfpsResultCode providerStatus)
            && either (const False) (const True)
              (validateDatafastSuccessfulPayment
                (toPathPiece oid)
                (fromIntegral (spcPaidMinor paidCheckout))
                (spcCurrency paidCheckout) providerStatus)
          providerReference = fromMaybe resourcePath (sdfpsPaymentId providerStatus)
      unless matched $ liftIO $ flip runSqlPool envPool $
        Checkout.recordReconciliationException
          Checkout.ProviderDatafast (sdfEnvironment datafast) (sdfEntityId datafast)
          "manual_reconciliation_mismatch" (toPathPiece oid) providerReference
          (spcPaidMinor paidCheckout) (fromIntegral <$> actualMinor)
          (spcCurrency paidCheckout) now
      pure ServiceStorefrontReconciliationDTO
        { ssrecOrderId = orderNumber
        , ssrecProvider = "datafast"
        , ssrecProviderReference = providerReference
        , ssrecExpectedAmount = fromIntegral (spcPaidMinor paidCheckout)
        , ssrecActualAmount = actualMinor
        , ssrecCurrency = spcCurrency paidCheckout
        , ssrecMatched = matched
        , ssrecCheckedAt = now
        }
    _ -> throwError err503
      { errBody = "No automated reconciliation adapter is enabled for this provider" }

requireReconciliationBinding
  :: ServicePaidCheckout
  -> Checkout.CheckoutEnvironment
  -> Text
  -> AppM ()
requireReconciliationBinding paidCheckout environment merchantRef =
  unless (spcEnvironment paidCheckout == Checkout.checkoutEnvironmentText environment
      && spcMerchantRef paidCheckout == merchantRef) $
    throwError err503
      { errBody = "Provider configuration does not match immutable payment evidence" }

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

loadServicePaidCheckoutByOrder
  :: Text
  -> SqlPersistT IO (Either Text ServicePaidCheckout)
loadServicePaidCheckoutByOrder orderNumber =
  loadServicePaidCheckoutBy
    "service_order.order_number = ?" [PersistText orderNumber]

loadServicePaidCheckoutByCheckout
  :: Checkout.CheckoutReference
  -> SqlPersistT IO (Either Text ServicePaidCheckout)
loadServicePaidCheckoutByCheckout checkout =
  loadServicePaidCheckoutBy
    "checkout.id = ?::uuid"
    [PersistText (Checkout.checkoutReferenceId checkout)]

loadServicePaidCheckoutBy
  :: Text
  -> [PersistValue]
  -> SqlPersistT IO (Either Text ServicePaidCheckout)
loadServicePaidCheckoutBy predicate params = do
  rows <- (rawSql
    ("SELECT checkout.id::text, attempt.id::text, attempt.provider,\
     \ attempt.environment, attempt.merchant_account_ref, checkout.paid_minor,\
     \ checkout.refunded_minor,\
     \ COALESCE((SELECT SUM(refund.amount_minor) FROM commerce_refund refund\
     \   WHERE refund.checkout_id = checkout.id\
     \   AND refund.status IN ('requested','approved','processing')), 0),\
     \ checkout.currency, binding.provider_resource_id\
     \ FROM service_storefront_order service_order\
     \ JOIN commerce_checkout_session checkout ON checkout.id = service_order.checkout_id\
     \ JOIN commerce_payment_attempt attempt ON attempt.checkout_id = checkout.id\
     \   AND attempt.status = 'succeeded'\
     \ JOIN commerce_provider_binding binding ON binding.payment_attempt_id = attempt.id\
     \   AND ((attempt.provider = 'paypal' AND binding.resource_type = 'capture')\
     \     OR (attempt.provider = 'datafast' AND binding.resource_type = 'payment'))\
     \ WHERE checkout.domain_type = 'mixing_mastering' AND " <> predicate)
    params :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Text, Single Text
       , Single Int64, Single Int64, Single Int64, Single Text, Single Text
       )])
  pure $ case rows of
    [( Single checkoutId, Single attemptId, Single provider, Single environment
     , Single merchantRef, Single paidMinor, Single refundedMinor
     , Single reservedMinor, Single currency, Single providerResource
     )] -> Right ServicePaidCheckout
        { spcCheckoutId = checkoutId
        , spcAttemptId = attemptId
        , spcProvider = provider
        , spcEnvironment = environment
        , spcMerchantRef = merchantRef
        , spcPaidMinor = paidMinor
        , spcRefundedMinor = refundedMinor
        , spcReservedMinor = reservedMinor
        , spcCurrency = currency
        , spcProviderResource = providerResource
        }
    [] -> Left "Service order does not have a succeeded canonical payment"
    _ -> Left "Service payment binding is ambiguous"

loadServiceOrderNumberForCheckout
  :: Checkout.CheckoutReference
  -> SqlPersistT IO (Maybe Text)
loadServiceOrderNumberForCheckout checkout = do
  rows <- (rawSql
    "SELECT order_number FROM service_storefront_order\
    \ WHERE checkout_id = ?::uuid"
    [PersistText (Checkout.checkoutReferenceId checkout)]
    :: SqlPersistT IO [Single Text])
  pure $ case rows of
    [Single orderNumber] -> Just orderNumber
    _ -> Nothing

serviceRefundToDTO :: Text -> Refund.RefundRecord -> ServiceStorefrontRefundDTO
serviceRefundToDTO orderNumber record = ServiceStorefrontRefundDTO
  { ssrfId = Refund.refundReferenceId (Refund.rrReference record)
  , ssrfOrderId = orderNumber
  , ssrfProvider = Refund.rrProvider record
  , ssrfProviderRefundId = Refund.rrProviderRefundId record
  , ssrfStatus = Refund.rrStatus record
  , ssrfAmountUsdCents = fromIntegral (Refund.rrAmountMinor record)
  , ssrfCurrency = Refund.rrCurrency record
  , ssrfReasonCode = Refund.rrReasonCode record
  , ssrfRequestedBy = Refund.rrRequestedBy record
  , ssrfApprovedBy = Refund.rrApprovedBy record
  , ssrfCreatedAt = Refund.rrCreatedAt record
  , ssrfCompletedAt = Refund.rrCompletedAt record
  }

parseRefundReference :: Text -> AppM Refund.RefundReference
parseRefundReference rawRefundId =
  case fromText (T.strip rawRefundId) of
    Just refundId -> pure (Refund.RefundReference (toText refundId))
    Nothing -> throwError err400 { errBody = "Refund ID must be a UUID" }

checkoutEnvironmentFromText
  :: Text
  -> Either Text Checkout.CheckoutEnvironment
checkoutEnvironmentFromText rawEnvironment =
  case T.toLower (T.strip rawEnvironment) of
    "sandbox" -> Right Checkout.CheckoutSandbox
    "production" -> Right Checkout.CheckoutProduction
    _ -> Left "Stored checkout environment is invalid"

paymentProviderFromText :: Text -> Either Text Checkout.PaymentProvider
paymentProviderFromText rawProvider =
  case T.toLower (T.strip rawProvider) of
    "paypal" -> Right Checkout.ProviderPayPal
    "datafast" -> Right Checkout.ProviderDatafast
    "stripe" -> Right Checkout.ProviderStripe
    "bank_transfer" -> Right Checkout.ProviderBankTransfer
    "cash" -> Right Checkout.ProviderCash
    "pos" -> Right Checkout.ProviderPos
    "cardano" -> Right Checkout.ProviderCardano
    _ -> Left "Stored payment provider is invalid"

refundLookupError :: Text -> ServerError
refundLookupError message =
  err409 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

refundConflictError :: Text -> ServerError
refundConflictError message =
  err409 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

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

validateServiceFulfillmentTransition :: Text -> Text -> Either Text ()
validateServiceFulfillmentTransition rawCurrent rawNext
  | current == next = Right ()
  | (current, next) `elem` allowedTransitions = Right ()
  | next `elem` paymentManagedStatuses =
      Left "Payment states are provider/manual-verification managed and cannot be set by the generic admin update"
  | otherwise = Left ("Invalid service fulfillment transition from " <> current <> " to " <> next)
  where
    current = T.toLower (T.strip rawCurrent)
    next = T.toLower (T.strip rawNext)
    allowedTransitions =
      [ ("paid", "in_progress")
      , ("in_progress", "v1_delivered")
      , ("v1_delivered", "revisions")
      , ("v1_delivered", "approved")
      , ("revisions", "v1_delivered")
      , ("revisions", "approved")
      , ("approved", "delivered")
      , ("delivered", "completed")
      ]
    paymentManagedStatuses =
      [ "awaiting_payment", "pending_payment", "datafast_pending", "paypal_pending"
      , "awaiting_manual_confirmation", "payment_failed", "paid"
      , "partially_refunded", "refunded", "disputed", "chargeback"
      ]

isServicePaymentConfirmed :: ME.ServiceStorefrontOrder -> Bool
isServicePaymentConfirmed order =
  ME.serviceStorefrontOrderPaidAt order /= Nothing
    || ME.serviceStorefrontOrderStatus order `elem`
      [ "paid", "in_progress", "v1_delivered", "revisions", "approved"
      , "delivered", "completed", "partially_refunded", "refunded"
      , "disputed", "chargeback"
      ]

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

loadConfiguredCheckoutEnvironment :: AppM Checkout.CheckoutEnvironment
loadConfiguredCheckoutEnvironment = do
  rawEnvironment <- liftIO $ lookupEnv "COMMERCE_CHECKOUT_ENV"
  either (throwError . configurationError) pure
    (Checkout.resolveCheckoutEnvironment rawEnvironment)

configurationError :: Text -> ServerError
configurationError message =
  err500 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

beginCanonicalPaymentAttempt
  :: ME.ServiceStorefrontOrderId
  -> ME.ServiceStorefrontOrder
  -> Checkout.CheckoutEnvironment
  -> Checkout.PaymentProvider
  -> Checkout.PaymentOperation
  -> Text
  -> Text
  -> AppM (Checkout.CheckoutReference, Checkout.PaymentAttemptReference)
beginCanonicalPaymentAttempt orderId order providerEnvironment provider operation merchantRef suffix = do
  Env{..} <- ask
  checkoutUuid <- maybe
    (throwError err409 { errBody = "Order is not linked to a canonical checkout; staff reconciliation is required" })
    pure
    (ME.serviceStorefrontOrderCheckoutId order)
  let checkout = Checkout.CheckoutReference (toText checkoutUuid)
      correlationId = paymentCorrelationId orderId (Checkout.paymentProviderText provider) suffix
  storedEnvironment <- liftIO (flip runSqlPool envPool (Checkout.loadCheckoutEnvironment checkout))
    >>= either (throwError . providerValidationError) pure
  unless (storedEnvironment == providerEnvironment) $
    throwError err503
      { errBody = "Configured provider environment does not match this immutable checkout" }
  domainEnabled <- liftIO $ flip runSqlPool envPool $
    Checkout.domainEnabledForEnvironment storedEnvironment "mixing_mastering"
  unless domainEnabled $
    throwError err503
      { errBody = "Mixing/mastering checkout is disabled for this environment" }
  enabled <- liftIO $ flip runSqlPool envPool $
    Checkout.providerEnabledForEnvironment storedEnvironment provider
  unless enabled $
    throwError err503
      { errBody = "Payment provider is disabled for this checkout environment" }
  now <- liftIO getCurrentTime
  result <- liftIO $ flip runSqlPool envPool $
    Checkout.beginPaymentAttempt Checkout.PaymentAttemptCreation
      { Checkout.pacCheckout = checkout
      , Checkout.pacProvider = provider
      , Checkout.pacEnvironment = storedEnvironment
      , Checkout.pacOperation = operation
      , Checkout.pacAmountMinor = fromIntegral (ME.serviceStorefrontOrderPriceUsdCents order)
      , Checkout.pacCurrency = ME.serviceStorefrontOrderCurrency order
      , Checkout.pacMerchantRef = merchantRef
      , Checkout.pacIdempotencyKey = correlationId
      , Checkout.pacCreatedAt = now
      , Checkout.pacCorrelationId = correlationId
      }
  attempt <- either (throwError . providerValidationError) pure result
  pure (checkout, attempt)

failCanonicalPaymentAttempt
  :: Checkout.CheckoutReference
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Text
  -> ServerError
  -> AppM a
failCanonicalPaymentAttempt checkout attempt provider failureCode providerError = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  liftIO $ flip runSqlPool envPool $
    Checkout.recordPaymentFailure
      checkout attempt provider failureCode
      ("provider-error:" <> Checkout.paymentAttemptReferenceId attempt)
      now
  throwError providerError

providerVerificationMismatch
  :: Checkout.CheckoutReference
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Checkout.CheckoutEnvironment
  -> Text
  -> Text
  -> Text
  -> Int64
  -> Maybe Int64
  -> Text
  -> Text
  -> AppM a
providerVerificationMismatch checkout attempt provider environment merchantRef internalRef providerRef expectedAmount actualAmount currency message = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  liftIO $ flip runSqlPool envPool $ do
    Checkout.recordPaymentFailure checkout attempt provider "provider_verification_mismatch"
      ("provider-verification:" <> Checkout.paymentAttemptReferenceId attempt) now
    Checkout.recordReconciliationException
      provider environment merchantRef "provider_verification_mismatch"
      internalRef providerRef expectedAmount actualAmount currency now
  throwError (providerValidationError message)

paymentCorrelationId :: ME.ServiceStorefrontOrderId -> Text -> Text -> Text
paymentCorrelationId orderId provider suffix =
  "service-storefront:" <> toPathPiece orderId <> ":" <> provider <> ":" <> suffix

insertServiceStatusChange
  :: ME.ServiceStorefrontOrderId
  -> Text
  -> Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
insertServiceStatusChange orderId status notes changedBy createdAt = do
  _ <- insert ME.ServiceStorefrontOrderStatusChange
    { ME.serviceStorefrontOrderStatusChangeOrderId = orderId
    , ME.serviceStorefrontOrderStatusChangeStatus = status
    , ME.serviceStorefrontOrderStatusChangeNotes = Just notes
    , ME.serviceStorefrontOrderStatusChangeChangedBy = Just changedBy
    , ME.serviceStorefrontOrderStatusChangeCreatedAt = createdAt
    }
  pure ()

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
  , sdfEnvironment :: Checkout.CheckoutEnvironment
  } deriving (Show)

validateDatafastEnvironmentBase
  :: Checkout.CheckoutEnvironment
  -> String
  -> Either Text ()
validateDatafastEnvironmentBase environment rawBaseUrl = do
  let baseUrl = T.toLower (T.strip (T.pack rawBaseUrl))
      mAfterScheme = T.stripPrefix "https://" baseUrl
      authority = maybe "" (T.takeWhile (`notElem` ("/?#" :: String))) mAfterScheme
      suffix = maybe "" (T.dropWhile (`notElem` ("/?#" :: String))) mAfterScheme
      validSuffix = T.null suffix || suffix == "/"
      validAuthority = not (T.null authority)
        && not ("@" `T.isInfixOf` authority)
        && not (":" `T.isInfixOf` authority)
      isOppwa = authority == "oppwa.com" || ".oppwa.com" `T.isSuffixOf` authority
      isTest = "test.oppwa.com" `T.isSuffixOf` authority
  unless (mAfterScheme /= Nothing && validAuthority && validSuffix && isOppwa) $
    Left "DATAFAST_BASE_URL must be an HTTPS oppwa.com endpoint"
  case environment of
    Checkout.CheckoutSandbox ->
      unless isTest (Left "DATAFAST_ENV=sandbox requires a test.oppwa.com endpoint")
    Checkout.CheckoutProduction ->
      when isTest (Left "DATAFAST_ENV=production cannot use a test.oppwa.com endpoint")

-- | Load Datafast environment from env vars.
loadServiceDatafastEnv :: AppM ServiceDatafastEnv
loadServiceDatafastEnv = do
  mEntity <- liftIO $ lookupEnv "DATAFAST_ENTITY_ID"
  mBearer <- liftIO $ lookupEnv "DATAFAST_BEARER_TOKEN"
  mBase   <- liftIO $ lookupEnv "DATAFAST_BASE_URL"
  mTest   <- liftIO $ lookupEnv "DATAFAST_TEST_MODE"
  mEnvironment <- liftIO $ lookupEnv "DATAFAST_ENV"
  entityId <- maybe (throwError err500 { errBody = "DATAFAST_ENTITY_ID not set" }) (pure . T.pack) mEntity
  bearer   <- maybe (throwError err500 { errBody = "DATAFAST_BEARER_TOKEN not set" }) (pure . T.pack) mBearer
  baseUrl  <- maybe (throwError err500 { errBody = "DATAFAST_BASE_URL not set" }) (pure) mBase
  environment <- either (throwError . configurationError) pure
    (Checkout.resolveCheckoutEnvironment mEnvironment)
  either (throwError . configurationError) pure
    (validateDatafastEnvironmentBase environment baseUrl)
  let testMode = T.pack <$> mTest
  pure ServiceDatafastEnv
    { sdfEntityId = entityId
    , sdfBearerToken = bearer
    , sdfBaseUrl = baseUrl
    , sdfTestMode = testMode
    , sdfEnvironment = environment
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
        Just code | isDatafastCheckoutCreationSuccess code -> pure ()
        Just code ->
          throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 ("Datafast rejected checkout: " <> code)) }
        Nothing -> throwError err502 { errBody = "Datafast checkout response omitted the result code" }
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
isDatafastCheckoutCreationSuccess :: Text -> Bool
isDatafastCheckoutCreationSuccess code = T.strip code == "000.200.100"

isDatafastPaymentSuccess :: Checkout.CheckoutEnvironment -> Text -> Bool
isDatafastPaymentSuccess environment rawCode =
  case environment of
    Checkout.CheckoutProduction -> code == "000.000.000"
    Checkout.CheckoutSandbox -> code `elem` ["000.100.110", "000.100.112"]
  where
    code = T.strip rawCode

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

data PaypalWebhookHeaders = PaypalWebhookHeaders
  { pwhTransmissionId   :: Text
  , pwhTransmissionTime :: Text
  , pwhTransmittedAt    :: UTCTime
  , pwhCertUrl           :: Text
  , pwhAuthAlgo          :: Text
  , pwhTransmissionSig   :: Text
  } deriving (Eq, Show)

data PaypalWebhookEnvelope = PaypalWebhookEnvelope
  { pweEventId   :: Text
  , pweEventType :: Text
  , pweCreatedAt :: UTCTime
  , pweResource  :: Value
  } deriving (Show)

data PaypalWebhookCapture = PaypalWebhookCapture
  { pwcCaptureId    :: Text
  , pwcStatus       :: Text
  , pwcAmount       :: Text
  , pwcCurrency     :: Text
  , pwcMerchantId   :: Text
  , pwcPaypalOrderId :: Text
  } deriving (Eq, Show)

data PaypalRefundOutcome = PaypalRefundOutcome
  { proRefundId :: Text
  , proStatus   :: Text
  , proAmount   :: Text
  , proCurrency :: Text
  } deriving (Eq, Show)

data PaypalEventProcessResult
  = PaypalEventProcessed (Maybe Text) (Maybe Text) (Maybe Text)
  | PaypalEventIgnored
  | PaypalEventPermanentFailure Text (Maybe Text) (Maybe Text) (Maybe Text)
  | PaypalEventRetry Text

data BoundPaypalCapture = BoundPaypalCapture
  { bpcCheckoutId     :: Text
  , bpcAttemptId      :: Text
  , bpcDomainOrderId  :: Text
  , bpcExpectedAmount :: Int64
  , bpcCurrency       :: Text
  , bpcMerchantRef    :: Text
  , bpcPaypalOrderId  :: Text
  }

data ServicePaidCheckout = ServicePaidCheckout
  { spcCheckoutId       :: Text
  , spcAttemptId        :: Text
  , spcProvider         :: Text
  , spcEnvironment      :: Text
  , spcMerchantRef      :: Text
  , spcPaidMinor        :: Int64
  , spcRefundedMinor    :: Int64
  , spcReservedMinor    :: Int64
  , spcCurrency         :: Text
  , spcProviderResource :: Text
  }

validatePaypalWebhookHeaders
  :: UTCTime
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Either Text PaypalWebhookHeaders
validatePaypalWebhookHeaders now mTransmissionId mTransmissionTime mCertUrl mAuthAlgo mSignature = do
  transmissionId <- requireSafeHeader "PayPal-Transmission-Id" 100 mTransmissionId
  transmissionTime <- requireSafeHeader "PayPal-Transmission-Time" 100 mTransmissionTime
  certUrl <- requireSafeHeader "PayPal-Cert-Url" 500 mCertUrl
  authAlgo <- requireSafeHeader "PayPal-Auth-Algo" 100 mAuthAlgo
  signature <- requireSafeHeader "PayPal-Transmission-Sig" 500 mSignature
  unless (authAlgo == "SHA256withRSA") $
    Left "Unsupported PayPal webhook signature algorithm"
  unless (isPaypalCertificateUrl certUrl) $
    Left "PayPal certificate URL is not an approved HTTPS provider URL"
  transmittedAt <- parseAesonUtc "PayPal transmission time" transmissionTime
  ProviderEvent.validateProviderEventTimestamp now transmittedAt
  pure PaypalWebhookHeaders
    { pwhTransmissionId = transmissionId
    , pwhTransmissionTime = transmissionTime
    , pwhTransmittedAt = transmittedAt
    , pwhCertUrl = certUrl
    , pwhAuthAlgo = authAlgo
    , pwhTransmissionSig = signature
    }

parsePaypalWebhookEnvelope :: BL.ByteString -> Either Text PaypalWebhookEnvelope
parsePaypalWebhookEnvelope rawBody
  | BL.null rawBody = Left "PayPal webhook body is empty"
  | BL.length rawBody > 1024 * 1024 = Left "PayPal webhook body exceeds 1048576 bytes"
  | otherwise = do
      value <- either (Left . ("Invalid PayPal webhook JSON: " <>) . T.pack) Right
        (eitherDecode rawBody :: Either String Value)
      case value of
        Object obj -> do
          eventId <- requiredObjectText "id" obj
          eventType <- requiredObjectText "event_type" obj
          createTime <- requiredObjectText "create_time" obj
          createdAt <- parseAesonUtc "PayPal event create_time" createTime
          resource <- maybe (Left "PayPal webhook omitted resource") Right
            (KM.lookup "resource" obj)
          unless (isProviderReference eventId && T.length eventId <= 128) $
            Left "PayPal webhook event ID is invalid"
          unless (validPaypalEventType eventType) $
            Left "PayPal webhook event type is invalid"
          pure PaypalWebhookEnvelope
            { pweEventId = eventId
            , pweEventType = eventType
            , pweCreatedAt = createdAt
            , pweResource = resource
            }
        _ -> Left "PayPal webhook must be a JSON object"

parsePaypalWebhookCapture :: PaypalWebhookEnvelope -> Either Text PaypalWebhookCapture
parsePaypalWebhookCapture PaypalWebhookEnvelope{pweResource = Object resource} = do
  captureId <- requiredObjectText "id" resource
  status <- requiredObjectText "status" resource
  amountObject <- maybe (Left "PayPal webhook omitted capture amount") Right
    (lookupObject "amount" resource)
  amount <- requiredObjectText "value" amountObject
  currency <- requiredObjectText "currency_code" amountObject
  payee <- maybe (Left "PayPal webhook omitted capture payee") Right
    (lookupObject "payee" resource)
  merchantId <- requiredObjectText "merchant_id" payee
  supplementary <- maybe (Left "PayPal webhook omitted supplementary_data") Right
    (lookupObject "supplementary_data" resource)
  related <- maybe (Left "PayPal webhook omitted related_ids") Right
    (lookupObject "related_ids" supplementary)
  paypalOrderId <- requiredObjectText "order_id" related
  unless (all isProviderReference [captureId, merchantId, paypalOrderId]) $
    Left "PayPal webhook contains an invalid provider reference"
  pure PaypalWebhookCapture
    { pwcCaptureId = captureId
    , pwcStatus = T.toUpper (T.strip status)
    , pwcAmount = T.strip amount
    , pwcCurrency = T.toUpper (T.strip currency)
    , pwcMerchantId = merchantId
    , pwcPaypalOrderId = paypalOrderId
    }
parsePaypalWebhookCapture _ = Left "PayPal webhook capture resource must be an object"

parsePaypalRefundOutcome :: Value -> Either Text PaypalRefundOutcome
parsePaypalRefundOutcome (Object obj) = do
  refundId <- requiredObjectText "id" obj
  status <- requiredObjectText "status" obj
  amountObject <- maybe (Left "PayPal refund response omitted amount") Right
    (lookupObject "amount" obj)
  amount <- requiredObjectText "value" amountObject
  currency <- requiredObjectText "currency_code" amountObject
  unless (isProviderReference refundId) $
    Left "PayPal refund response contains an invalid refund ID"
  pure PaypalRefundOutcome
    { proRefundId = refundId
    , proStatus = T.toUpper (T.strip status)
    , proAmount = T.strip amount
    , proCurrency = T.toUpper (T.strip currency)
    }
parsePaypalRefundOutcome _ = Left "PayPal refund response must be an object"

buildPaypalWebhookVerificationBody
  :: Text
  -> PaypalWebhookHeaders
  -> BL.ByteString
  -> BL.ByteString
buildPaypalWebhookVerificationBody webhookId PaypalWebhookHeaders{..} rawBody =
  BL.concat
    [ "{\"transmission_id\":"
    , Aeson.encode pwhTransmissionId
    , ",\"transmission_time\":"
    , Aeson.encode pwhTransmissionTime
    , ",\"cert_url\":"
    , Aeson.encode pwhCertUrl
    , ",\"auth_algo\":"
    , Aeson.encode pwhAuthAlgo
    , ",\"transmission_sig\":"
    , Aeson.encode pwhTransmissionSig
    , ",\"webhook_id\":"
    , Aeson.encode webhookId
    , ",\"webhook_event\":"
    , rawBody
    , "}"
    ]

paypalWebhookResourceId :: PaypalWebhookEnvelope -> Maybe Text
paypalWebhookResourceId PaypalWebhookEnvelope{pweResource = Object resource} =
  lookupObjectText "id" resource
paypalWebhookResourceId _ = Nothing

requiredObjectText :: Text -> Aeson.Object -> Either Text Text
requiredObjectText key obj =
  case lookupObjectText key obj of
    Just value | not (T.null (T.strip value)) -> Right (T.strip value)
    _ -> Left ("PayPal payload omitted " <> key)

parseAesonUtc :: Text -> Text -> Either Text UTCTime
parseAesonUtc label rawTimestamp =
  case Aeson.fromJSON (String (T.strip rawTimestamp)) of
    Success value -> Right value
    Error _ -> Left (label <> " is not a valid RFC 3339 timestamp")

requireSafeHeader :: Text -> Int -> Maybe Text -> Either Text Text
requireSafeHeader name maxLength mRawValue = do
  value <- maybe (Left ("Missing " <> name <> " header")) (Right . T.strip) mRawValue
  unless (not (T.null value) && T.length value <= maxLength) $
    Left (name <> " header is invalid")
  unless (T.all (\character -> character >= '!' && character <= '~') value) $
    Left (name <> " header contains unsafe characters")
  pure value

validPaypalEventType :: Text -> Bool
validPaypalEventType value =
  not (T.null value)
    && T.length value <= 100
    && T.all (\character ->
      (character >= 'A' && character <= 'Z')
        || (character >= '0' && character <= '9')
        || character `elem` ("._-" :: String)) value

isPaypalCertificateUrl :: Text -> Bool
isPaypalCertificateUrl rawUrl =
  let normalized = T.toLower (T.strip rawUrl)
      allowedPrefixes =
        [ "https://api-m.paypal.com/"
        , "https://api.paypal.com/"
        , "https://api-m.sandbox.paypal.com/"
        , "https://api.sandbox.paypal.com/"
        ]
  in any (`T.isPrefixOf` normalized) allowedPrefixes
      && not ("@" `T.isInfixOf` normalized)
      && not ("#" `T.isInfixOf` normalized)

paypalCertUrlMatchesEnvironment :: Checkout.CheckoutEnvironment -> Text -> Bool
paypalCertUrlMatchesEnvironment environment rawUrl =
  let normalized = T.toLower (T.strip rawUrl)
      isSandbox = ".sandbox.paypal.com/" `T.isInfixOf` normalized
  in case environment of
    Checkout.CheckoutSandbox -> isSandbox
    Checkout.CheckoutProduction -> not isSandbox

-- | Load PayPal environment configuration.
loadPaypalEnvForService
  :: AppM (Text, Text, String, Checkout.CheckoutEnvironment, Text)
loadPaypalEnvForService = do
  mCid <- liftIO $ lookupEnv "PAYPAL_CLIENT_ID"
  mSecret <- liftIO $ lookupEnv "PAYPAL_CLIENT_SECRET"
  mEnv <- liftIO $ lookupEnv "PAYPAL_ENV"
  mMerchant <- liftIO $ lookupEnv "PAYPAL_MERCHANT_ID"
  cid <- maybe (throwError err500 { errBody = "PAYPAL_CLIENT_ID not set" }) (pure . T.pack) mCid
  secret <- maybe (throwError err500 { errBody = "PAYPAL_CLIENT_SECRET not set" }) (pure . T.pack) mSecret
  environment <- either (throwError . configurationError) pure
    (Checkout.resolveCheckoutEnvironment mEnv)
  merchantRef <- case T.strip . T.pack <$> mMerchant of
    Just value | isProviderReference value -> pure value
    _ -> throwError err500
      { errBody = "PAYPAL_MERCHANT_ID must be configured with the provider merchant account ID" }
  let baseUrl = case environment of
        Checkout.CheckoutSandbox -> "https://api-m.sandbox.paypal.com"
        Checkout.CheckoutProduction -> "https://api-m.paypal.com"
  pure (cid, secret, baseUrl, environment, merchantRef)

loadRequiredSafeEnv :: String -> Int -> AppM Text
loadRequiredSafeEnv variableName maxLength = do
  rawValue <- liftIO (lookupEnv variableName)
  case T.strip . T.pack <$> rawValue of
    Just value
      | not (T.null value)
      , T.length value <= maxLength
      , T.all (\character -> character >= '!' && character <= '~') value -> pure value
    _ -> throwError err500
      { errBody = BL.fromStrict (TE.encodeUtf8
          (T.pack variableName <> " must be configured with safe visible ASCII")) }

loadProviderEventEncryptionKey :: AppM Text
loadProviderEventEncryptionKey = do
  encryptionKey <- loadRequiredSafeEnv "COMMERCE_EVENT_ENCRYPTION_KEY" 256
  when (T.length encryptionKey < 32) $
    throwError err500
      { errBody = "COMMERCE_EVENT_ENCRYPTION_KEY must contain at least 32 characters" }
  pure encryptionKey

verifyPaypalWebhookRemote
  :: Manager
  -> Text
  -> Text
  -> String
  -> Text
  -> PaypalWebhookHeaders
  -> BL.ByteString
  -> AppM Bool
verifyPaypalWebhookRemote manager cid sec baseUrl webhookId headers rawBody = do
  token <- paypalAccessTokenForService manager cid sec baseUrl
  req0 <- liftIO $ parseRequest (baseUrl ++ "/v1/notifications/verify-webhook-signature")
  let req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS
            (buildPaypalWebhookVerificationBody webhookId headers rawBody)
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 token)
            ]
        }
  result <- liftIO (tryAny (httpLbs req manager))
  resp <- case result of
    Left _ -> throwError err503 { errBody = "PayPal webhook verification is temporarily unavailable" }
    Right value -> pure value
  let responseCode = statusCode (responseStatus resp)
  when (responseCode >= 500) $
    throwError err503 { errBody = "PayPal webhook verification is temporarily unavailable" }
  when (responseCode >= 400) $
    throwError err502 { errBody = "PayPal rejected the webhook verification request" }
  case eitherDecode (responseBody resp) of
    Right (Object obj) ->
      pure (lookupObjectText "verification_status" obj == Just "SUCCESS")
    _ -> throwError err502 { errBody = "Invalid PayPal webhook verification response" }

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
  , spcoMerchantId      :: Maybe Text
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
    Right value -> either
      (throwError . providerValidationError)
      pure
      (parsePaypalCaptureOutcome value)

getPaypalOrderRemoteForService
  :: Manager
  -> Text
  -> Text
  -> String
  -> Text
  -> AppM ServicePaypalCaptureOutcome
getPaypalOrderRemoteForService manager cid sec baseUrl paypalOrderId = do
  token <- paypalAccessTokenForService manager cid sec baseUrl
  req0 <- liftIO $ parseRequest
    (baseUrl ++ "/v2/checkout/orders/" ++ T.unpack paypalOrderId)
  let req = req0
        { method = "GET"
        , requestHeaders =
            [("Authorization", "Bearer " <> TE.encodeUtf8 token)]
        }
  result <- liftIO (tryAny (httpLbs req manager))
  resp <- case result of
    Left _ -> throwError err503
      { errBody = "PayPal reconciliation is temporarily unavailable" }
    Right value -> pure value
  let responseCode = statusCode (responseStatus resp)
  when (responseCode >= 500) $
    throwError err503 { errBody = "PayPal reconciliation is temporarily unavailable" }
  when (responseCode >= 400) $
    throwError err502 { errBody = "PayPal rejected the reconciliation request" }
  value <- either
    (const (throwError err502 { errBody = "Invalid PayPal reconciliation response" }))
    pure
    (eitherDecode (responseBody resp) :: Either String Value)
  either (throwError . providerValidationError) pure
    (parsePaypalCaptureOutcome value)

issuePaypalRefundRemote
  :: Manager
  -> Text
  -> Text
  -> String
  -> Text
  -> Refund.RefundRecord
  -> AppM PaypalRefundOutcome
issuePaypalRefundRemote manager cid sec baseUrl captureId refundRecord = do
  token <- paypalAccessTokenForService manager cid sec baseUrl
  req0 <- liftIO $ parseRequest
    (baseUrl ++ "/v2/payments/captures/" ++ T.unpack captureId ++ "/refund")
  let body = object
        [ "amount" .= object
            [ "currency_code" .= Refund.rrCurrency refundRecord
            , "value" .= formatMinorUnitsDecimal
                (Refund.rrCurrency refundRecord)
                (fromIntegral (Refund.rrAmountMinor refundRecord))
            ]
        ]
      requestId = Refund.refundReferenceId (Refund.rrReference refundRecord)
      req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS (Aeson.encode body)
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 token)
            , ("PayPal-Request-Id", TE.encodeUtf8 requestId)
            , ("Prefer", "return=representation")
            ]
        }
  result <- liftIO (tryAny (httpLbs req manager))
  resp <- case result of
    Left _ -> throwError err503
      { errBody = "PayPal refund is temporarily unavailable; retry with the same refund ID" }
    Right value -> pure value
  let responseCode = statusCode (responseStatus resp)
  when (responseCode >= 500) $
    throwError err503
      { errBody = "PayPal refund is temporarily unavailable; retry with the same refund ID" }
  when (responseCode >= 400) $
    throwError err502 { errBody = "PayPal rejected the refund request" }
  value <- either
    (const (throwError err502 { errBody = "Invalid PayPal refund response" }))
    pure
    (eitherDecode (responseBody resp) :: Either String Value)
  either (throwError . providerValidationError) pure (parsePaypalRefundOutcome value)

parsePaypalCaptureOutcome :: Value -> Either Text ServicePaypalCaptureOutcome
parsePaypalCaptureOutcome (Object obj) =
  let payerEmail = case KM.lookup "payer" obj of
        Just (Object payerObj) -> case KM.lookup "email_address" payerObj of
          Just (String email) -> Just email
          _ -> Nothing
        _ -> Nothing
      purchaseUnit = onlyObject "purchase_units" obj
      captureObject = purchaseUnit >>= lookupObject "payments" >>= onlyObject "captures"
      captureStatus = fromMaybe "UNKNOWN" (captureObject >>= lookupObjectText "status")
      captureId = captureObject >>= lookupObjectText "id"
      amountObject = captureObject >>= lookupObject "amount"
      capturedValue = amountObject >>= lookupObjectText "value"
      capturedCurrency = amountObject >>= lookupObjectText "currency_code"
      internalOrderId = purchaseUnit >>= lookupObjectText "custom_id"
      merchantId = purchaseUnit >>= lookupObject "payee" >>= lookupObjectText "merchant_id"
  in Right ServicePaypalCaptureOutcome
    { spcoStatus = captureStatus
    , spcoPayerEmail = payerEmail
    , spcoCaptureId = captureId
    , spcoAmount = capturedValue
    , spcoCurrency = capturedCurrency
    , spcoInternalOrderId = internalOrderId
    , spcoMerchantId = merchantId
    }
parsePaypalCaptureOutcome _ = Left "Invalid PayPal capture response format"

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

onlyObject :: Text -> Aeson.Object -> Maybe Aeson.Object
onlyObject key obj = case KM.lookup (AesonKey.fromText key) obj of
  Just (Array values) -> case foldr (:) [] values of
    [Object value] -> Just value
    _ -> Nothing
  _ -> Nothing

validatePaypalSuccessfulCapture
  :: Text
  -> Int
  -> Text
  -> Text
  -> ServicePaypalCaptureOutcome
  -> Either Text ()
validatePaypalSuccessfulCapture expectedOrderId expectedCents expectedCurrency expectedMerchant outcome = do
  captureId <- maybe (Left "PayPal response did not include a capture ID") (Right . T.strip) (spcoCaptureId outcome)
  capturedCents <- maybe (Left "PayPal response did not include a captured amount") parseDatafastCents (spcoAmount outcome)
  capturedCurrency <- maybe (Left "PayPal response did not include a captured currency") (Right . T.toUpper . T.strip) (spcoCurrency outcome)
  customId <- maybe (Left "PayPal response did not include the internal order binding") (Right . T.strip) (spcoInternalOrderId outcome)
  merchantId <- maybe (Left "PayPal response did not include the payee merchant ID") (Right . T.strip) (spcoMerchantId outcome)
  unless (not (T.null captureId) && T.length captureId <= 128) (Left "PayPal capture ID is invalid")
  unless (capturedCents == expectedCents) (Left "PayPal captured amount does not match the immutable order total")
  unless (capturedCurrency == T.toUpper (T.strip expectedCurrency)) (Left "PayPal captured currency does not match the immutable order currency")
  unless (customId == expectedOrderId) (Left "PayPal custom_id does not match the internal order")
  unless (merchantId == T.strip expectedMerchant) (Left "PayPal payee merchant ID does not match the configured merchant")

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
