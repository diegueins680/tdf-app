{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.ProviderExecution
  ( providerExecutionServer
  , providerReference
  , checkoutMoneyBreakdown
  ) where

import           Control.Exception.Safe (tryAny)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as A
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import           Database.Persist.Sql (ConnectionPool, runSqlPool)
import           Network.Socket (NameInfoFlag(NI_NUMERICHOST), SockAddr, getNameInfo)
import           Servant
import           System.Entropy (getEntropy)

import           TDF.API.ProviderExecution
import qualified TDF.Commerce.CheckoutStore as Checkout
import           TDF.Commerce.ProviderAdapter
import           TDF.Commerce.ProviderAdapter.Http
  ( AdapterTransportError(..), executeAdapterRequest, sharedProviderManager )
import qualified TDF.Commerce.ProviderAdapter.PlaceToPay as PlaceToPay
import           TDF.Commerce.ProviderCapabilities
import qualified TDF.Commerce.PaymentRuntimeStore as PaymentRuntime
import qualified TDF.Commerce.ProviderEventStore as ProviderEvent
import qualified TDF.Commerce.ProviderExecutionStore as Store
import           TDF.Commerce.ProviderRuntimeConfig
import           TDF.DB (Env(..))
import           TDF.Server.PaymentAvailability (loadRuntimeReadyRoutes)
import           TDF.Server.PaymentCapabilities (parsePaymentMethod)

type AppM = ReaderT Env Handler

providerExecutionServer :: ServerT ProviderExecutionAPI AppM
providerExecutionServer =
       createPaymentSession
  :<|> getPaymentSession
  :<|> placeToPayNotification
  :<|> payPhoneNotification
  :<|> payPhoneNotification

createPaymentSession
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> SockAddr
  -> PaymentSessionCreateDTO
  -> AppM PaymentSessionDTO
createPaymentSession rawCheckoutId rawLookupToken rawIdempotencyKey rawUserAgent
    remoteAddress request = do
  Env{envPool} <- ask
  checkoutId <- either (throwError . badRequest) pure (validatedUuid "checkoutId" rawCheckoutId)
  lookupToken <- requireLookupToken rawLookupToken
  idempotencyKey <- requireIdempotencyKey rawIdempotencyKey
  userAgent <- requireUserAgent rawUserAgent
  provider <- either (throwError . badRequest) pure (parseProvider (pscProvider request))
  paymentMethod <- either (throwError . badRequest) pure
    (parsePaymentMethod (pscPaymentMethod request))
  let lookupHash = sha256Text lookupToken
  encryptionKey <- liftIO loadProviderOperationEncryptionKey
    >>= either (throwError . unavailableText) pure
  replay <- liftIO (runSqlPool
      (Store.loadAuthorizedCreateReplay checkoutId lookupHash provider paymentMethod
        idempotencyKey (pscBuyerPhone request) (pscBuyerCountryCode request) encryptionKey) envPool)
    >>= either (throwError . conflict) pure
  case replay of
    Just known | Store.porStatus known /= "prepared" -> pure (operationToDTO checkoutId known)
    -- A prepared operation has not been contacted. Starting it still needs all
    -- current checkout, account, capability and secret gates below.
    _ -> startPaymentSession checkoutId lookupHash idempotencyKey userAgent provider
      paymentMethod remoteAddress request encryptionKey

startPaymentSession
  :: Text -> Text -> Text -> Text -> Checkout.PaymentProvider -> PaymentMethod
  -> SockAddr -> PaymentSessionCreateDTO -> Text -> AppM PaymentSessionDTO
startPaymentSession checkoutId lookupHash idempotencyKey userAgent provider paymentMethod
    remoteAddress request encryptionKey = do
  Env{envPool} <- ask
  now <- liftIO getCurrentTime
  checkout <- liftIO (runSqlPool
      (Store.loadAuthorizedCheckout checkoutId lookupHash now) envPool)
    >>= either (const (throwError err404)) pure
  flow <- maybe
    (throwError (conflict "Checkout domain does not support shared provider execution"))
    pure
    (PaymentRuntime.productFlowForDomain (Store.ceDomainType checkout))
  let routeRequest = PaymentRouteRequest
        { prEnvironment = Store.ceEnvironment checkout
        , prBuyerCountry = "ZZ"
        , prCurrency = Store.ceCurrency checkout
        , prAmountMinor = Store.ceTotalMinor checkout
        , prMethod = paymentMethod
        , prFlow = flow
        , prRequiredCapabilities = PaymentRuntime.operationCapabilities
            flow Checkout.OperationCreate
        }
  routes <- loadRuntimeReadyRoutes routeRequest
  unless (any ((== provider) . routeProvider) routes) $
    throwError unavailable
      { errBody = "Requested provider and payment method are not operationally available" }
  merchantRef <- liftIO (runSqlPool
      (Store.loadReadyMerchantAccount (Store.ceEnvironment checkout) provider) envPool)
    >>= either (throwError . unavailableText) pure
  runtime <- liftIO (loadRuntimeProviderAdapter (Store.ceEnvironment checkout) provider)
    >>= either (throwError . unavailableText) pure
  clientIp <- liftIO (numericHost remoteAddress)
    >>= either (throwError . unavailableText) pure
  nonce <- liftIO (getEntropy 32)
  let correlationId = "provider-create:" <> checkoutId <> ":" <> idempotencyKey
      preflightPayment = CreatePayment
        { cpPaymentMethod = paymentMethod
        , cpReference = providerReference provider checkoutId
        , cpDescription = checkoutDescription checkout
        , cpMoney = checkoutMoneyBreakdown checkout
        , cpReturnUrl = rpaReturnUrl runtime
        , cpNotificationUrl = rpaNotificationUrl runtime
        , cpBuyerPhone = T.strip <$> pscBuyerPhone request
        , cpBuyerCountryCode = T.strip <$> pscBuyerCountryCode request
        , cpIpAddress = clientIp
        , cpUserAgent = userAgent
        }
      context = AdapterContext now nonce
      adapter = rpaAdapter runtime
  _ <- either (throwError . badRequest . adapterErrorPublicMessage) pure
    (adapterBuildCreate adapter context preflightPayment)
  let attemptCreation = Checkout.PaymentAttemptCreation
        { Checkout.pacCheckout = Store.ceCheckout checkout
        , Checkout.pacProvider = provider
        , Checkout.pacEnvironment = Store.ceEnvironment checkout
        , Checkout.pacOperation = Checkout.OperationCreate
        , Checkout.pacAmountMinor = Store.ceTotalMinor checkout
        , Checkout.pacCurrency = Store.ceCurrency checkout
        , Checkout.pacMerchantRef = merchantRef
        , Checkout.pacIdempotencyKey = idempotencyKey
        , Checkout.pacCreatedAt = now
        , Checkout.pacCorrelationId = correlationId
        }
  attempt <- liftIO (runSqlPool
      (PaymentRuntime.beginPaymentAttemptForMethod paymentMethod attemptCreation) envPool)
    >>= either (throwError . conflict) pure
  previousReference <- liftIO (runSqlPool (Store.loadAttemptProviderReference attempt) envPool)
    >>= either (throwError . conflict) pure
  let merchantReference = fromMaybe
        (providerReference provider (Checkout.paymentAttemptReferenceId attempt)) previousReference
      payment = preflightPayment { cpReference = merchantReference }
      expected = ExpectedPayment merchantReference
        (Store.ceTotalMinor checkout) (Store.ceCurrency checkout)
      locator = PaymentLocator "pending" expected
      fingerprint = Store.providerCreateRequestFingerprint (Store.ceCheckout checkout)
        provider paymentMethod merchantReference (Store.ceTotalMinor checkout)
        (Store.ceCurrency checkout) (cpBuyerPhone payment) (cpBuyerCountryCode payment)
  adapterRequest <- either (throwError . badRequest . adapterErrorPublicMessage) pure
    (adapterBuildCreate adapter context payment)
  operation <- liftIO (runSqlPool
      (Store.prepareProviderOperation Store.ProviderOperationPreparation
        { Store.popAttempt = attempt
        , Store.popProvider = provider
        , Store.popEnvironment = Store.ceEnvironment checkout
        , Store.popMerchantRef = merchantRef
        , Store.popProviderReference = merchantReference
        , Store.popOperation = AdapterCreate
        , Store.popIdempotencyKey = idempotencyKey
        , Store.popRequestSha256 = fingerprint
        , Store.popOccurredAt = now
        }) envPool)
    >>= either (throwError . conflict) pure
  claim <- liftIO (runSqlPool
      (Store.claimProviderOperation (Store.porReference operation) now encryptionKey) envPool)
    >>= either (throwError . conflict) pure
  case claim of
    Store.ProviderOperationKnown known ->
      pure (operationToDTO checkoutId known)
    Store.ProviderOperationBusy ->
      pure (operationToDTO checkoutId (operation
        { Store.porStatus = "in_flight"
        , Store.porOutcomeCertainty = ProviderAmbiguous
        }))
    Store.ProviderOperationClaimed operationRef -> do
      transport <- liftIO (executeAdapterRequest sharedProviderManager adapterRequest)
      case transport of
        Left AdapterTransportError{adapterTransportPublicMessage} -> do
          markAmbiguous envPool operationRef checkout attempt provider
            "provider_transport_ambiguous" correlationId
          throwError unavailable { errBody = textBody adapterTransportPublicMessage }
        Right providerValue ->
          case adapterParseResponse adapter AdapterCreate locator providerValue of
            Left _ -> do
              markAmbiguous envPool operationRef checkout attempt provider
                "provider_response_ambiguous" correlationId
              throwError unavailable
                { errBody = "Payment provider response requires reconciliation" }
            Right result -> do
              completed <- liftIO (runSqlPool
                  (Store.recordCreateResult operationRef checkout attempt provider
                    merchantRef merchantReference encryptionKey result correlationId now)
                  envPool)
              case completed of
                Left _ -> do
                  markAmbiguous envPool operationRef checkout attempt provider
                    "provider_persistence_ambiguous" correlationId
                  throwError unavailable
                    { errBody = "Payment provider result requires reconciliation" }
                Right stored -> pure (operationToDTO checkoutId stored)

getPaymentSession
  :: Text
  -> Text
  -> Maybe Text
  -> AppM PaymentSessionDTO
getPaymentSession rawCheckoutId rawAttemptId rawLookupToken = do
  Env{envPool} <- ask
  checkoutId <- either (throwError . badRequest) pure (validatedUuid "checkoutId" rawCheckoutId)
  attemptId <- either (throwError . badRequest) pure (validatedUuid "attemptId" rawAttemptId)
  lookupToken <- requireLookupToken rawLookupToken
  encryptionKey <- liftIO loadProviderOperationEncryptionKey
    >>= either (throwError . unavailableText) pure
  loaded <- liftIO $ runSqlPool
    (Store.loadAuthorizedCreateOperation checkoutId attemptId
      (sha256Text lookupToken) encryptionKey) envPool
  operation <- either (const (throwError err404)) pure loaded
  pure (operationToDTO checkoutId operation)

placeToPayNotification :: BL.ByteString -> AppM NoContent
placeToPayNotification rawBody = do
  Env{envPool} <- ask
  (environment, merchantRef, runtime, encryptionKey) <-
    loadNotificationContext Checkout.ProviderPlaceToPay
      "checkout.placetopay.webhooks"
  value <- either (throwError . badRequest) pure
    (decodeProviderNotification rawBody)
  assessment <- either
    (throwError . unauthorizedNotification . adapterErrorPublicMessage)
    pure
    (adapterAssessNotification (rpaAdapter runtime) value)
  unless (notificationAuthenticated assessment
      && notificationRequiresQuery assessment) $
    throwError (unauthorizedNotification "PlaceToPay notification is not authoritative")
  eventId <- either (throwError . badRequest) pure
    (ProviderEvent.placeToPayNotificationEventId (BL.toStrict rawBody))
  now <- liftIO getCurrentTime
  stored <- liftIO $ runSqlPool
      (ProviderEvent.storeVerifiedProviderEvent ProviderEvent.ProviderEventCreation
        { ProviderEvent.pecProvider = Checkout.ProviderPlaceToPay
        , ProviderEvent.pecEnvironment = environment
        , ProviderEvent.pecMerchantRef = merchantRef
        , ProviderEvent.pecProviderEventId = eventId
        , ProviderEvent.pecEventType = "SESSION_STATUS"
        , ProviderEvent.pecProviderCreatedAt = Nothing
        , ProviderEvent.pecProviderResource =
            Just (notificationExternalId assessment)
        , ProviderEvent.pecRawPayload = BL.toStrict rawBody
        , ProviderEvent.pecEncryptionKey = encryptionKey
        , ProviderEvent.pecReceivedAt = now
        }) envPool
  _ <- either (throwError . conflict) pure stored
  pure NoContent

payPhoneNotification :: BL.ByteString -> AppM PayPhoneNotificationAck
payPhoneNotification rawBody = do
  Env{envPool} <- ask
  (environment, merchantRef, runtime, encryptionKey) <-
    loadNotificationContext Checkout.ProviderPayPhone
      "checkout.payphone.notifications"
  value <- either (throwError . badRequest) pure
    (decodeProviderNotification rawBody)
  assessment <- either
    (throwError . badRequest . adapterErrorPublicMessage)
    pure
    (adapterAssessNotification (rpaAdapter runtime) value)
  unless (not (notificationAuthenticated assessment)
      && notificationRequiresQuery assessment) $
    throwError (badRequest "PayPhone notification trust classification is invalid")
  bound <- liftIO (runSqlPool
      (Store.loadBoundProviderPayment
        Checkout.ProviderPayPhone environment merchantRef
        (notificationExternalId assessment)
        (notificationMerchantReference assessment)) envPool)
  _ <- either (const (throwError err404)) pure bound
  now <- liftIO getCurrentTime
  stored <- liftIO $ runSqlPool
    (ProviderEvent.storeUntrustedProviderEvent ProviderEvent.ProviderEventCreation
      { ProviderEvent.pecProvider = Checkout.ProviderPayPhone
      , ProviderEvent.pecEnvironment = environment
      , ProviderEvent.pecMerchantRef = merchantRef
      , ProviderEvent.pecProviderEventId =
          "payphone-" <> notificationExternalId assessment
      , ProviderEvent.pecEventType = "PAYMENT_NOTIFICATION"
      , ProviderEvent.pecProviderCreatedAt = Nothing
      , ProviderEvent.pecProviderResource =
          Just (notificationExternalId assessment)
      , ProviderEvent.pecRawPayload = BL.toStrict rawBody
      , ProviderEvent.pecEncryptionKey = encryptionKey
      , ProviderEvent.pecReceivedAt = now
      }) envPool
  _ <- either (throwError . conflict) pure stored
  pure PayPhoneNotificationAck
    { ppnAccepted = True
    , ppnErrorCode = "000"
    }

loadNotificationContext
  :: Checkout.PaymentProvider
  -> Text
  -> AppM
      ( Checkout.CheckoutEnvironment
      , Text
      , RuntimeProviderAdapter
      , Text
      )
loadNotificationContext provider featureFlag = do
  Env{envPool} <- ask
  environment <- liftIO loadConfiguredCheckoutEnvironment
    >>= either (throwError . unavailableText) pure
  enabled <- liftIO $ runSqlPool
    (Checkout.capabilityEnabledForEnvironment environment featureFlag) envPool
  unless enabled $
    throwError unavailable { errBody = "Provider notification processing is disabled" }
  merchantRef <- liftIO (runSqlPool
      (Store.loadReadyMerchantAccount environment provider) envPool)
    >>= either (throwError . unavailableText) pure
  runtime <- liftIO (loadRuntimeProviderAdapter environment provider)
    >>= either (throwError . unavailableText) pure
  encryptionKey <- liftIO loadProviderOperationEncryptionKey
    >>= either (throwError . unavailableText) pure
  pure (environment, merchantRef, runtime, encryptionKey)

decodeProviderNotification :: BL.ByteString -> Either Text A.Value
decodeProviderNotification rawBody
  | BL.null rawBody = Left "Provider notification body is required"
  | BL.length rawBody > 1024 * 1024 = Left "Provider notification body is too large"
  | otherwise = either (const (Left "Provider notification body is invalid")) Right
      (A.eitherDecode rawBody)

unauthorizedNotification :: Text -> ServerError
unauthorizedNotification message = err401 { errBody = textBody message }

markAmbiguous
  :: ConnectionPool
  -> Store.ProviderOperationReference
  -> Store.CheckoutExecution
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Text
  -> Text
  -> AppM ()
markAmbiguous envPool operationRef checkout attempt provider errorCode correlationId = do
  observedAt <- liftIO getCurrentTime
  liftIO $ runSqlPool
    (Store.recordAmbiguousOperation operationRef checkout attempt provider
      errorCode correlationId observedAt) envPool

operationToDTO :: Text -> Store.ProviderOperationRecord -> PaymentSessionDTO
operationToDTO checkoutId operation = PaymentSessionDTO
  { pssCheckoutId = checkoutId
  , pssAttemptId = Checkout.paymentAttemptReferenceId (Store.porAttempt operation)
  , pssOperationId = Store.providerOperationReferenceId (Store.porReference operation)
  , pssProvider = Checkout.paymentProviderText (Store.porProvider operation)
  , pssState = Store.porStatus operation
  , pssExternalId = Store.porProviderResourceId operation
  , pssRedirectUrl = Store.porRedirectUrl operation
  , pssOutcomeCertainty = outcomeCertaintyText (Store.porOutcomeCertainty operation)
  , pssCanRetryOrFallback = safeToFallback (Store.porOutcomeCertainty operation)
  }

checkoutMoneyBreakdown :: Store.CheckoutExecution -> MoneyBreakdown
checkoutMoneyBreakdown checkout = MoneyBreakdown
  { mbCurrency = Store.ceCurrency checkout
  , mbTotalMinor = Store.ceTotalMinor checkout
  , mbWithoutTaxMinor = if Store.ceTaxMinor checkout == 0 then netSubtotal else 0
  , mbTaxableBaseMinor = if Store.ceTaxMinor checkout == 0 then 0 else netSubtotal
  , mbTaxMinor = Store.ceTaxMinor checkout
  , mbServiceMinor = Store.ceFeeMinor checkout
  , mbTipMinor = 0
  }
  where
    netSubtotal = Store.ceSubtotalMinor checkout - Store.ceDiscountMinor checkout

providerReference :: Checkout.PaymentProvider -> Text -> Text
providerReference provider attemptId = case provider of
  Checkout.ProviderPlaceToPay -> PlaceToPay.placeToPayReference scopedReference
  Checkout.ProviderPayPhone -> "TDF-" <> T.take 28 (sha256Text scopedReference)
  _ -> "TDF-" <> T.take 28 (sha256Text scopedReference)
  where
    scopedReference = Checkout.paymentProviderText provider <> ":" <> attemptId

checkoutDescription :: Store.CheckoutExecution -> Text
checkoutDescription checkout = T.take 120
  ("TDF " <> T.replace "_" " " (Store.ceDomainType checkout) <> " payment")

parseProvider :: Text -> Either Text Checkout.PaymentProvider
parseProvider rawProvider = case T.toLower (T.strip rawProvider) of
  "placetopay" -> Right Checkout.ProviderPlaceToPay
  "payphone" -> Right Checkout.ProviderPayPhone
  _ -> Left "Shared payment sessions support placetopay or payphone"

validatedUuid :: Text -> Text -> Either Text Text
validatedUuid field rawValue = maybe
  (Left (field <> " must be a UUID"))
  (Right . UUID.toText)
  (UUID.fromText (T.strip rawValue))

requireLookupToken :: Maybe Text -> AppM Text
requireLookupToken rawToken = case T.strip <$> rawToken of
  Just token
    | T.length token >= 16
    , T.length token <= 512
    , T.all (\character -> character >= '!' && character <= '~') token -> pure token
  _ -> throwError err404

requireIdempotencyKey :: Maybe Text -> AppM Text
requireIdempotencyKey rawKey = case T.strip <$> rawKey of
  Just key
    | T.length key >= 16
    , T.length key <= 128
    , T.all (\character -> character >= '!' && character <= '~') key -> pure key
  _ -> throwError (badRequest "Idempotency-Key must contain 16 to 128 visible ASCII characters")

requireUserAgent :: Maybe Text -> AppM Text
requireUserAgent rawUserAgent = case T.strip <$> rawUserAgent of
  Just userAgent
    | not (T.null userAgent)
    , T.length userAgent <= 512
    , T.all (\character -> character >= ' ' && character < '\DEL') userAgent -> pure userAgent
  _ -> throwError (badRequest "A valid User-Agent header is required")

numericHost :: SockAddr -> IO (Either Text Text)
numericHost address = do
  result <- tryAny (getNameInfo [NI_NUMERICHOST] True False address)
  pure $ case result of
    Right (Just host, _) | not (null host) -> Right (T.pack host)
    _ -> Left "Unable to determine the payment client network address"

sha256Text :: Text -> Text
sha256Text value = TE.decodeUtf8
  (BAE.convertToBase BAE.Base16 (hash (TE.encodeUtf8 value) :: Digest SHA256))

outcomeCertaintyText :: ProviderOutcomeCertainty -> Text
outcomeCertaintyText certainty = case certainty of
  ProviderNotContacted -> "not_contacted"
  ProviderRejectedBeforeCreation -> "rejected_before_creation"
  ProviderConfirmedNoCharge -> "confirmed_no_charge"
  ProviderAmbiguous -> "ambiguous"
  ProviderSucceeded -> "succeeded"

badRequest :: Text -> ServerError
badRequest message = err400 { errBody = textBody message }

conflict :: Text -> ServerError
conflict message = err409 { errBody = textBody message }

unavailable :: ServerError
unavailable = err503

unavailableText :: Text -> ServerError
unavailableText message = unavailable { errBody = textBody message }

textBody :: Text -> BL.ByteString
textBody = BL.fromStrict . TE.encodeUtf8
