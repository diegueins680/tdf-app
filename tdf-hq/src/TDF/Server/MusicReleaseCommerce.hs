{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.MusicReleaseCommerce
  ( createMusicPurchase
  , listMusicPurchases
  , createMusicDatafastCheckout
  , confirmMusicDatafastPayment
  , createMusicPaypalOrder
  , captureMusicPaypalOrder
  , listMusicEntitlements
  ) where

import Control.Monad (unless)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
import System.Environment (lookupEnv)

import TDF.API.MusicRelease
import TDF.Auth (AuthedUser(..))
import qualified TDF.CMS.Models as CMS
import qualified TDF.Commerce.CheckoutStore as Checkout
import TDF.DB (Env(..))
import qualified TDF.Server.ServiceStorefront as Provider

type AppM = ReaderT Env Handler

data PurchaseContext = PurchaseContext
  { purchaseId :: UUID
  , purchaseCheckoutId :: UUID
  , purchaseState :: Text
  , purchaseAmountMinor :: Int64
  , purchaseCurrency :: Text
  , purchaseBuyerName :: Text
  , purchaseBuyerEmail :: Text
  }

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

currentPartyId :: AuthedUser -> Int64
currentPartyId = fromSqlKey . auPartyId

badRequest :: Text -> ServerError
badRequest message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

providerError :: Text -> ServerError
providerError message = err502 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

featureEnvironment :: IO Text
featureEnvironment = do
  raw <- lookupEnv "APP_ENV"
  pure $ case fmap (T.toLower . T.strip . T.pack) raw of
    Just "production" -> "production"
    Just "prod" -> "production"
    _ -> "sandbox"

requireCommerceFeature :: AppM ()
requireCommerceFeature = do
  environment <- liftIO featureEnvironment
  rows <- runDB (rawSql
    "SELECT enabled FROM revenue_feature_flag WHERE flag_key='music_releases.commerce' AND environment=?"
    [PersistText environment] :: SqlPersistT IO [Single Bool])
  unless (rows == [Single True]) $ throwError err503
    { errBody = "Music release commerce is not enabled in this environment" }

requireIdempotencyKey :: Maybe Text -> AppM Text
requireIdempotencyKey supplied = do
  let value = T.strip (fromMaybe "" supplied)
  unless (T.length value >= 8 && T.length value <= 160 && T.all safe value) $
    throwError (badRequest "Idempotency-Key must contain 8-160 visible ASCII characters")
  pure value
  where
    safe character = character >= '!' && character <= '~'

createMusicPurchase :: AuthedUser -> Maybe Text -> Maybe Text -> MusicPurchaseCreateRequest -> AppM Value
createMusicPurchase user idempotencyHeader edgeCountry MusicPurchaseCreateRequest{..} = do
  requireCommerceFeature
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  trustedTerritory <- trustedEdgeTerritory edgeCountry
  let actor = currentPartyId user
      declaredTerritory = T.toUpper (T.strip musicPurchaseTerritoryCode)
      territory = fromMaybe "ZZ" trustedTerritory
      termsVersion = T.strip musicPurchaseTermsVersion
  unless (T.length declaredTerritory == 2 && T.all isAsciiUpper declaredTerritory) $
    throwError (badRequest "territoryCode must be an ISO 3166-1 alpha-2 code")
  unless (not (T.null termsVersion) && T.length termsVersion <= 160) $
    throwError (badRequest "termsVersion is required and limited to 160 characters")
  offerRows <- runDB (rawSql
    "SELECT public.release_version_id,rule.price_minor,rule.currency,rule.downloadable_asset_id,public.title,party.display_name,party.primary_email FROM music_availability_rule rule JOIN music_public_release public ON public.release_version_id=rule.release_version_id JOIN party ON party.id=? WHERE rule.id=?::uuid AND rule.purchasable=TRUE AND rule.download_policy='purchase' AND rule.price_minor>0 AND rule.currency IS NOT NULL AND rule.downloadable_asset_id IS NOT NULL AND (rule.starts_at IS NULL OR rule.starts_at<=NOW()) AND (rule.ends_at IS NULL OR rule.ends_at>NOW()) AND ((rule.territory_mode='include' AND ('Worldwide'=ANY(rule.territories) OR ?::text=ANY(rule.territories))) OR (rule.territory_mode='exclude' AND ?::text IS NOT NULL AND NOT (?::text=ANY(rule.territories))))"
    [ PersistInt64 actor, toPersistValue musicPurchaseAvailabilityRuleId
    , optionalText trustedTerritory, optionalText trustedTerritory, optionalText trustedTerritory
    ]
    :: SqlPersistT IO [(Single UUID, Single Int64, Single Text, Single UUID, Single Text, Single Text, Single (Maybe Text))])
  (releaseVersionId, amountMinor, currency, assetId, releaseTitle, _buyerName, buyerEmail) <- case offerRows of
    [(Single versionId, Single amount, Single currencyCode, Single downloadableAsset, Single title, Single name, Single (Just email))]
      | not (T.null (T.strip email)) -> pure (versionId, amount, currencyCode, downloadableAsset, title, name, T.toLower (T.strip email))
    [(Single _, Single _, Single _, Single _, Single _, Single _, Single Nothing)] ->
      throwError (badRequest "Your account needs a verified billing email before purchase")
    _ -> throwError err404
  configuredEnvironment <- liftIO (lookupEnv "COMMERCE_CHECKOUT_ENV")
  checkoutEnvironment <- either (throwError . badRequest) pure
    (Checkout.resolveCheckoutEnvironment configuredEnvironment)
  now <- liftIO getCurrentTime
  candidateOrderId <- liftIO nextRandom
  createdOrderId <- runDB $ do
    existing <- rawSql
      "SELECT id FROM music_purchase_order WHERE buyer_party_id=? AND idempotency_key=?"
      [PersistInt64 actor, PersistText idempotencyKey] :: SqlPersistT IO [Single UUID]
    case existing of
      [Single existingId] -> pure existingId
      [] -> do
        inserted <- rawSql
          "INSERT INTO music_purchase_order(id,buyer_party_id,release_version_id,availability_rule_id,state,gross_minor,net_minor,currency,idempotency_key) VALUES(?::uuid,?,?,?::uuid,'awaiting_payment',?,?,?,?) ON CONFLICT(buyer_party_id,idempotency_key) DO NOTHING RETURNING id"
          [ toPersistValue candidateOrderId, PersistInt64 actor, toPersistValue releaseVersionId
          , toPersistValue musicPurchaseAvailabilityRuleId, PersistInt64 amountMinor, PersistInt64 amountMinor
          , PersistText currency, PersistText idempotencyKey
          ] :: SqlPersistT IO [Single UUID]
        case inserted of
          [Single newOrderId] -> do
            let orderReference = UUID.toText newOrderId
                checkoutKey = T.pack (show actor) <> ":" <> idempotencyKey
                snapshot = object
                  [ "releaseVersionId" .= releaseVersionId
                  , "availabilityRuleId" .= musicPurchaseAvailabilityRuleId
                  , "assetId" .= assetId
                  , "territoryCode" .= territory
                  , "declaredTerritoryCode" .= declaredTerritory
                  , "termsVersion" .= termsVersion
                  ]
            checkout <- Checkout.createCheckout Checkout.CheckoutCreation
              { Checkout.ccDomainType = "music_download"
              , Checkout.ccDomainOrderId = orderReference
              , Checkout.ccEnvironment = checkoutEnvironment
              , Checkout.ccCurrency = currency
              , Checkout.ccAmountMinor = amountMinor
              , Checkout.ccCustomerEmail = buyerEmail
              , Checkout.ccLookupTokenHash = sha256Text (orderReference <> ":" <> checkoutKey)
              , Checkout.ccIdempotencyKey = checkoutKey
              , Checkout.ccExpiresAt = addUTCTime 1800 now
              , Checkout.ccProductType = "music_download"
              , Checkout.ccProductId = UUID.toText assetId
              , Checkout.ccProductVersion = UUID.toText releaseVersionId
              , Checkout.ccDescription = T.take 500 releaseTitle
              , Checkout.ccSnapshot = snapshot
              , Checkout.ccCorrelationId = "music-purchase:" <> orderReference
              }
            rawExecute
              "UPDATE music_purchase_order SET checkout_id=?::uuid WHERE id=?::uuid AND checkout_id IS NULL"
              [PersistText (Checkout.checkoutReferenceId checkout), toPersistValue newOrderId]
            rawExecute
              "INSERT INTO music_terms_acceptance(release_version_id,terms_kind,terms_version,accepted_by,evidence) VALUES(?::uuid,'download_sale',?,?,jsonb_build_object('purchase_order_id',?::text,'territory_code',?::text)) ON CONFLICT DO NOTHING"
              [toPersistValue releaseVersionId, PersistText termsVersion, PersistInt64 actor, PersistText orderReference, PersistText territory]
            pure newOrderId
          [] -> do
            raced <- rawSql
              "SELECT id FROM music_purchase_order WHERE buyer_party_id=? AND idempotency_key=?"
              [PersistInt64 actor, PersistText idempotencyKey] :: SqlPersistT IO [Single UUID]
            case raced of
              [Single racedId] -> pure racedId
              _ -> fail "music purchase idempotency conflict could not be resolved"
          _ -> fail "music purchase insert returned an ambiguous result"
      _ -> fail "music purchase idempotency lookup was ambiguous"
  purchaseJson actor createdOrderId

-- Purchase availability is authorization input. A browser-declared country
-- is never trusted; without a protected edge header only Worldwide offers are
-- eligible and the immutable order records ZZ as unknown territory.
trustedEdgeTerritory :: Maybe Text -> AppM (Maybe Text)
trustedEdgeTerritory supplied = do
  trustHeader <- fmap (maybe False parseBoolean) (liftIO (lookupEnv "MUSIC_TRUST_CF_IPCOUNTRY"))
  pure $ if trustHeader then supplied >>= normalizeCountry else Nothing
  where
    parseBoolean value = map asciiLower value `elem` ["1", "true", "yes", "on"]
    asciiLower character
      | character >= 'A' && character <= 'Z' = toEnum (fromEnum character + 32)
      | otherwise = character
    normalizeCountry raw =
      let territory = T.toUpper (T.strip raw)
      in if T.length territory == 2 && T.all isAsciiUpper territory && territory `notElem` ["XX", "T1"]
         then Just territory else Nothing

listMusicPurchases :: AuthedUser -> AppM [Value]
listMusicPurchases user = do
  requireCommerceFeature
  jsonRows
    "SELECT jsonb_build_object('id',purchase.id,'releaseVersionId',purchase.release_version_id,'availabilityRuleId',purchase.availability_rule_id,'state',purchase.state,'grossMinor',purchase.gross_minor,'discountMinor',purchase.discount_minor,'taxMinor',purchase.tax_minor,'feeMinor',purchase.fee_minor,'netMinor',purchase.net_minor,'currency',purchase.currency,'checkoutId',purchase.checkout_id,'providerReference',purchase.provider_reference,'createdAt',purchase.created_at,'paidAt',purchase.paid_at,'releaseTitle',version.title) FROM music_purchase_order purchase JOIN music_release_version version ON version.id=purchase.release_version_id WHERE purchase.buyer_party_id=? ORDER BY purchase.created_at DESC"
    [PersistInt64 (currentPartyId user)]

listMusicEntitlements :: AuthedUser -> AppM [Value]
listMusicEntitlements user = do
  requireCommerceFeature
  jsonRows
    "SELECT jsonb_build_object('id',entitlement.id,'releaseVersionId',entitlement.release_version_id,'assetId',entitlement.asset_id,'sourceKind',entitlement.source_kind,'status',entitlement.status,'maxDownloads',entitlement.max_downloads,'downloadCount',(SELECT count(*) FROM music_download_event event WHERE event.entitlement_id=entitlement.id),'expiresAt',entitlement.expires_at,'grantedAt',entitlement.granted_at,'revokedAt',entitlement.revoked_at) FROM music_entitlement entitlement WHERE entitlement.buyer_party_id=? ORDER BY entitlement.granted_at DESC"
    [PersistInt64 (currentPartyId user)]

createMusicDatafastCheckout :: AuthedUser -> UUID -> Maybe Text -> AppM Value
createMusicDatafastCheckout user orderId idempotencyHeader = do
  requireCommerceFeature
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  context <- loadPurchaseContext user orderId
  requirePayable context
  datafast <- Provider.loadServiceDatafastEnv
  checkout <- checkoutFor context (Provider.sdfEnvironment datafast)
  existing <- providerBinding (purchaseCheckoutId context) "datafast" "checkout"
  case existing of
    Just (providerId, _) -> datafastCheckoutJson context datafast providerId
    Nothing -> do
      now <- liftIO getCurrentTime
      attempt <- beginAttempt context checkout Checkout.ProviderDatafast (Provider.sdfEnvironment datafast)
        (Provider.sdfEntityId datafast) Checkout.OperationCreate idempotencyKey now
      amount <- safeIntAmount (purchaseAmountMinor context)
      (providerId, widgetUrl) <- Provider.requestDatafastCheckoutForService
        (UUID.toText orderId) amount (purchaseCurrency context)
        (purchaseBuyerName context) (purchaseBuyerEmail context) Nothing
        `catchError` failAttempt checkout attempt Checkout.ProviderDatafast "datafast_create" now
      bindResult <- runDB $ Checkout.bindProviderResource Checkout.ProviderBindingCreation
        { Checkout.pbcAttempt = attempt
        , Checkout.pbcCheckout = checkout
        , Checkout.pbcProvider = Checkout.ProviderDatafast
        , Checkout.pbcEnvironment = Provider.sdfEnvironment datafast
        , Checkout.pbcMerchantRef = Provider.sdfEntityId datafast
        , Checkout.pbcResourceType = "checkout"
        , Checkout.pbcProviderResource = providerId
        , Checkout.pbcResourcePath = Just ("/v1/checkouts/" <> providerId)
        , Checkout.pbcOrderReference = UUID.toText orderId
        , Checkout.pbcAmountMinor = purchaseAmountMinor context
        , Checkout.pbcCurrency = purchaseCurrency context
        , Checkout.pbcStage = Checkout.AttemptRequiresCustomerAction
        , Checkout.pbcOccurredAt = now
        , Checkout.pbcCorrelationId = "music-datafast-create:" <> UUID.toText orderId
        }
      either (throwError . providerError) pure bindResult
      pure (object
        [ "purchaseId" .= orderId, "checkoutId" .= purchaseCheckoutId context
        , "providerCheckoutId" .= providerId, "widgetUrl" .= T.pack widgetUrl
        , "amountMinor" .= purchaseAmountMinor context, "currency" .= purchaseCurrency context
        ])

confirmMusicDatafastPayment :: AuthedUser -> UUID -> Maybe Text -> MusicDatafastStatusRequest -> AppM Value
confirmMusicDatafastPayment user orderId idempotencyHeader MusicDatafastStatusRequest{..} = do
  requireCommerceFeature
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  context <- loadPurchaseContext user orderId
  if purchaseState context == "paid" then purchaseJson (currentPartyId user) orderId else do
    requirePayable context
    datafast <- Provider.loadServiceDatafastEnv
    checkout <- checkoutFor context (Provider.sdfEnvironment datafast)
    createdBinding <- providerBinding (purchaseCheckoutId context) "datafast" "checkout"
    providerCheckoutId <- maybe (throwError (badRequest "Create the Datafast checkout first")) (pure . fst) createdBinding
    resourcePath <- either (throwError . badRequest) pure
      (Provider.validateDatafastOrderResourcePath (Just providerCheckoutId) musicDatafastResourcePath)
    status <- Provider.checkDatafastPaymentStatus resourcePath
    now <- liftIO getCurrentTime
    attempt <- beginAttempt context checkout Checkout.ProviderDatafast (Provider.sdfEnvironment datafast)
      (Provider.sdfEntityId datafast) Checkout.OperationCapture idempotencyKey now
    if Provider.isDatafastPaymentSuccess (Provider.sdfEnvironment datafast) (Provider.sdfpsResultCode status)
      then do
        amount <- safeIntAmount (purchaseAmountMinor context)
        either (throwError . providerError) pure
          (Provider.validateDatafastSuccessfulPayment (UUID.toText orderId) amount (purchaseCurrency context) status)
        paymentId <- maybe (throwError (providerError "Datafast response omitted the payment ID")) pure
          (Provider.sdfpsPaymentId status)
        bindAndVerify context checkout attempt Checkout.ProviderDatafast (Provider.sdfEnvironment datafast)
          (Provider.sdfEntityId datafast) "payment" paymentId (Just resourcePath) now
      else if Provider.sdfpsResultCode status == "000.200.000"
        then runDB (Checkout.recordPaymentProcessing checkout attempt Checkout.ProviderDatafast
          ("music-datafast-status:" <> UUID.toText orderId) now)
        else runDB (Checkout.recordPaymentFailure checkout attempt Checkout.ProviderDatafast
          ("datafast_" <> Provider.sdfpsResultCode status) ("music-datafast-status:" <> UUID.toText orderId) now)
    purchaseJson (currentPartyId user) orderId

createMusicPaypalOrder :: AuthedUser -> UUID -> Maybe Text -> AppM Value
createMusicPaypalOrder user orderId idempotencyHeader = do
  requireCommerceFeature
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  context <- loadPurchaseContext user orderId
  requirePayable context
  (clientId, secret, baseUrl, environment, merchantRef) <- Provider.loadPaypalEnvForService
  checkout <- checkoutFor context environment
  existing <- providerBinding (purchaseCheckoutId context) "paypal" "order"
  case existing of
    Just (providerOrderId, _) -> pure (object ["purchaseId" .= orderId, "paypalOrderId" .= providerOrderId, "approvalUrl" .= (Nothing :: Maybe Text)])
    Nothing -> do
      now <- liftIO getCurrentTime
      attempt <- beginAttempt context checkout Checkout.ProviderPayPal environment merchantRef Checkout.OperationCreate idempotencyKey now
      amount <- safeIntAmount (purchaseAmountMinor context)
      manager <- liftIO newTlsManager
      (providerOrderId, approvalUrl) <- Provider.createPaypalOrderRemoteForService manager clientId secret baseUrl
        (UUID.toText orderId) amount (purchaseCurrency context) (purchaseBuyerName context) (purchaseBuyerEmail context)
        `catchError` failAttempt checkout attempt Checkout.ProviderPayPal "paypal_create" now
      bindResult <- runDB $ Checkout.bindProviderResource Checkout.ProviderBindingCreation
        { Checkout.pbcAttempt = attempt, Checkout.pbcCheckout = checkout, Checkout.pbcProvider = Checkout.ProviderPayPal
        , Checkout.pbcEnvironment = environment, Checkout.pbcMerchantRef = merchantRef
        , Checkout.pbcResourceType = "order", Checkout.pbcProviderResource = providerOrderId
        , Checkout.pbcResourcePath = Just ("/v2/checkout/orders/" <> providerOrderId)
        , Checkout.pbcOrderReference = UUID.toText orderId, Checkout.pbcAmountMinor = purchaseAmountMinor context
        , Checkout.pbcCurrency = purchaseCurrency context, Checkout.pbcStage = Checkout.AttemptRequiresCustomerAction
        , Checkout.pbcOccurredAt = now, Checkout.pbcCorrelationId = "music-paypal-create:" <> UUID.toText orderId
        }
      either (throwError . providerError) pure bindResult
      pure (object ["purchaseId" .= orderId, "paypalOrderId" .= providerOrderId, "approvalUrl" .= approvalUrl])

captureMusicPaypalOrder :: AuthedUser -> UUID -> Maybe Text -> MusicPaypalCaptureRequest -> AppM Value
captureMusicPaypalOrder user orderId idempotencyHeader MusicPaypalCaptureRequest{..} = do
  requireCommerceFeature
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  context <- loadPurchaseContext user orderId
  if purchaseState context == "paid" then purchaseJson (currentPartyId user) orderId else do
    requirePayable context
    (clientId, secret, baseUrl, environment, merchantRef) <- Provider.loadPaypalEnvForService
    checkout <- checkoutFor context environment
    stored <- providerBinding (purchaseCheckoutId context) "paypal" "order"
    unless (fmap fst stored == Just (T.strip musicPaypalOrderId)) $
      throwError (badRequest "PayPal order ID does not match the immutable provider binding")
    now <- liftIO getCurrentTime
    attempt <- beginAttempt context checkout Checkout.ProviderPayPal environment merchantRef Checkout.OperationCapture idempotencyKey now
    manager <- liftIO newTlsManager
    outcome <- Provider.capturePaypalOrderRemoteForService manager clientId secret baseUrl (T.strip musicPaypalOrderId)
      `catchError` failAttempt checkout attempt Checkout.ProviderPayPal "paypal_capture" now
    case Provider.spcoStatus outcome of
      "COMPLETED" -> do
        amount <- safeIntAmount (purchaseAmountMinor context)
        either (throwError . providerError) pure
          (Provider.validatePaypalSuccessfulCapture (UUID.toText orderId) amount (purchaseCurrency context) merchantRef outcome)
        captureId <- maybe (throwError (providerError "PayPal response omitted the capture ID")) pure
          (Provider.spcoCaptureId outcome)
        bindAndVerify context checkout attempt Checkout.ProviderPayPal environment merchantRef
          "capture" captureId (Just ("/v2/payments/captures/" <> captureId)) now
      status | status `elem` ["APPROVED","PENDING"] ->
        runDB (Checkout.recordPaymentProcessing checkout attempt Checkout.ProviderPayPal
          ("music-paypal-capture:" <> UUID.toText orderId) now)
      status -> runDB (Checkout.recordPaymentFailure checkout attempt Checkout.ProviderPayPal
        ("paypal_" <> T.toLower status) ("music-paypal-capture:" <> UUID.toText orderId) now)
    purchaseJson (currentPartyId user) orderId

bindAndVerify
  :: PurchaseContext -> Checkout.CheckoutReference -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider -> Checkout.CheckoutEnvironment -> Text -> Text -> Text
  -> Maybe Text -> UTCTime -> AppM ()
bindAndVerify context checkout attempt provider environment merchantRef resourceType providerResource resourcePath now = do
  result <- runDB $ do
    bound <- Checkout.bindProviderResource Checkout.ProviderBindingCreation
      { Checkout.pbcAttempt = attempt, Checkout.pbcCheckout = checkout, Checkout.pbcProvider = provider
      , Checkout.pbcEnvironment = environment, Checkout.pbcMerchantRef = merchantRef
      , Checkout.pbcResourceType = resourceType, Checkout.pbcProviderResource = providerResource
      , Checkout.pbcResourcePath = resourcePath, Checkout.pbcOrderReference = UUID.toText (purchaseId context)
      , Checkout.pbcAmountMinor = purchaseAmountMinor context, Checkout.pbcCurrency = purchaseCurrency context
      , Checkout.pbcStage = Checkout.AttemptProcessing, Checkout.pbcOccurredAt = now
      , Checkout.pbcCorrelationId = "music-payment-bind:" <> UUID.toText (purchaseId context)
      }
    case bound of
      Left message -> pure (Left message)
      Right () -> Checkout.recordVerifiedPayment Checkout.VerifiedPayment
        { Checkout.vpAttempt = attempt, Checkout.vpCheckout = checkout, Checkout.vpProvider = provider
        , Checkout.vpEnvironment = environment, Checkout.vpMerchantRef = merchantRef
        , Checkout.vpResourceType = resourceType, Checkout.vpProviderResource = providerResource
        , Checkout.vpProviderResourcePath = resourcePath, Checkout.vpOrderReference = UUID.toText (purchaseId context)
        , Checkout.vpAmountMinor = purchaseAmountMinor context, Checkout.vpCurrency = purchaseCurrency context
        , Checkout.vpEvidence = "server_to_server", Checkout.vpOccurredAt = now
        , Checkout.vpCorrelationId = "music-payment-verified:" <> UUID.toText (purchaseId context)
        }
  either (throwError . providerError) (const (pure ())) result

beginAttempt
  :: PurchaseContext
  -> Checkout.CheckoutReference
  -> Checkout.PaymentProvider
  -> Checkout.CheckoutEnvironment
  -> Text
  -> Checkout.PaymentOperation
  -> Text
  -> UTCTime
  -> AppM Checkout.PaymentAttemptReference
beginAttempt context checkout provider environment merchantRef operation idempotencyKey now = do
  result <- runDB $ Checkout.beginPaymentAttempt Checkout.PaymentAttemptCreation
    { Checkout.pacCheckout = checkout, Checkout.pacProvider = provider, Checkout.pacEnvironment = environment
    , Checkout.pacOperation = operation, Checkout.pacAmountMinor = purchaseAmountMinor context
    , Checkout.pacCurrency = purchaseCurrency context, Checkout.pacMerchantRef = merchantRef
    , Checkout.pacIdempotencyKey = idempotencyKey, Checkout.pacCreatedAt = now
    , Checkout.pacCorrelationId = "music-payment-attempt:" <> UUID.toText (purchaseId context)
    }
  either (throwError . providerError) pure result

failAttempt
  :: Checkout.CheckoutReference
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Text
  -> UTCTime
  -> ServerError
  -> AppM a
failAttempt checkout attempt provider failureCode now originalError = do
  runDB (Checkout.recordPaymentFailure checkout attempt provider failureCode failureCode now)
  throwError originalError

checkoutFor :: PurchaseContext -> Checkout.CheckoutEnvironment -> AppM Checkout.CheckoutReference
checkoutFor context expected = do
  let reference = Checkout.CheckoutReference (UUID.toText (purchaseCheckoutId context))
  actual <- runDB (Checkout.loadCheckoutEnvironment reference)
  case actual of
    Right environment | environment == expected -> pure reference
    Right _ -> throwError (providerError "Provider environment does not match the immutable checkout environment")
    Left message -> throwError (providerError message)

loadPurchaseContext :: AuthedUser -> UUID -> AppM PurchaseContext
loadPurchaseContext user orderId = do
  rows <- runDB (rawSql
    "SELECT purchase.id,purchase.checkout_id,purchase.state,purchase.gross_minor,purchase.currency,party.display_name,party.primary_email FROM music_purchase_order purchase JOIN party ON party.id=purchase.buyer_party_id WHERE purchase.id=?::uuid AND purchase.buyer_party_id=? AND purchase.checkout_id IS NOT NULL"
    [toPersistValue orderId, PersistInt64 (currentPartyId user)]
    :: SqlPersistT IO [(Single UUID, Single UUID, Single Text, Single Int64, Single Text, Single Text, Single (Maybe Text))])
  case rows of
    [(Single oid, Single checkoutId, Single state, Single amount, Single currency, Single name, Single (Just email))]
      | not (T.null (T.strip email)) -> pure PurchaseContext
          { purchaseId=oid, purchaseCheckoutId=checkoutId, purchaseState=state
          , purchaseAmountMinor=amount, purchaseCurrency=currency
          , purchaseBuyerName=name, purchaseBuyerEmail=T.toLower (T.strip email)
          }
    [_] -> throwError (badRequest "Purchase buyer account has no billing email")
    _ -> throwError err404

requirePayable :: PurchaseContext -> AppM ()
requirePayable context = unless (purchaseState context `elem` ["pending","awaiting_payment"]) $
  throwError err409 { errBody = "Purchase is not in a payable state" }

providerBinding :: UUID -> Text -> Text -> AppM (Maybe (Text, Maybe Text))
providerBinding checkoutId provider resourceType = do
  rows <- runDB (rawSql
    "SELECT binding.provider_resource_id,binding.provider_resource_path FROM commerce_provider_binding binding JOIN commerce_payment_attempt attempt ON attempt.id=binding.payment_attempt_id WHERE attempt.checkout_id=?::uuid AND binding.provider=? AND binding.resource_type=? ORDER BY binding.created_at DESC LIMIT 1"
    [toPersistValue checkoutId, PersistText provider, PersistText resourceType]
    :: SqlPersistT IO [(Single Text, Single (Maybe Text))])
  pure $ case rows of
    [(Single resourceId, Single resourcePath)] -> Just (resourceId, resourcePath)
    _ -> Nothing

purchaseJson :: Int64 -> UUID -> AppM Value
purchaseJson actor orderId = do
  values <- jsonRows
    "SELECT jsonb_build_object('id',purchase.id,'releaseVersionId',purchase.release_version_id,'availabilityRuleId',purchase.availability_rule_id,'state',purchase.state,'grossMinor',purchase.gross_minor,'discountMinor',purchase.discount_minor,'taxMinor',purchase.tax_minor,'feeMinor',purchase.fee_minor,'netMinor',purchase.net_minor,'currency',purchase.currency,'checkoutId',purchase.checkout_id,'providerReference',purchase.provider_reference,'createdAt',purchase.created_at,'paidAt',purchase.paid_at) FROM music_purchase_order purchase WHERE purchase.id=?::uuid AND purchase.buyer_party_id=?"
    [toPersistValue orderId, PersistInt64 actor]
  maybe (throwError err404) pure (listToMaybe values)

datafastCheckoutJson :: PurchaseContext -> Provider.ServiceDatafastEnv -> Text -> AppM Value
datafastCheckoutJson context environment providerId = pure (object
  [ "purchaseId" .= purchaseId context, "checkoutId" .= purchaseCheckoutId context
  , "providerCheckoutId" .= providerId
  , "widgetUrl" .= T.pack (stripSlash (Provider.sdfBaseUrl environment) <> "/v1/paymentWidgets.js?checkoutId=" <> T.unpack providerId)
  , "amountMinor" .= purchaseAmountMinor context, "currency" .= purchaseCurrency context
  ])

jsonRows :: Text -> [PersistValue] -> AppM [Value]
jsonRows statement parameters = do
  rows <- runDB (rawSql statement parameters :: SqlPersistT IO [Single CMS.AesonValue])
  pure [CMS.unAesonValue value | Single value <- rows]

safeIntAmount :: Int64 -> AppM Int
safeIntAmount amount
  | amount > 0 && amount <= fromIntegral (maxBound :: Int) = pure (fromIntegral amount)
  | otherwise = throwError (providerError "Purchase amount is outside the provider-supported range")

sha256Text :: Text -> Text
sha256Text value = T.pack (show (hash (TE.encodeUtf8 value) :: Digest SHA256))

isAsciiUpper :: Char -> Bool
isAsciiUpper character = character >= 'A' && character <= 'Z'

optionalText :: Maybe Text -> PersistValue
optionalText = maybe PersistNull PersistText

stripSlash :: String -> String
stripSlash = reverse . dropWhile (== '/') . reverse
