{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.DomoQuoteCheckout
  ( publicDomoQuotesServer
  , deriveDomoLookupToken
  ) where

import           Control.Exception
  ( SomeAsyncException, SomeException, fromException, throwIO, try )
import           Control.Monad (forM_, unless, void, when)
import           Control.Monad.Except (catchError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Crypto.Hash (Digest, SHA256, hash)
import           Crypto.MAC.HMAC (HMAC, hmac)
import           Data.Aeson
  ( FromJSON(..), Value, eitherDecodeStrict', encode, object, withObject
  , (.:), (.:?), (.!=), (.=)
  )
import           Data.ByteArray (constEq)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isAlphaNum)
import           Data.Int (Int64)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, addUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Database.Persist
import           Database.Persist.Sql
  ( Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool )
import           Database.PostgreSQL.Simple (SqlError(..))
import           Servant
import           System.Environment (lookupEnv)

import qualified TDF.API.Types as APITypes
import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.DomoQuotes as Domo
import           TDF.DB (Env(..), sharedTlsManager)
import qualified TDF.Internationalization as Internationalization
import qualified TDF.Routes.DomoQuotes as Routes
import qualified TDF.Server.ServiceStorefront as ServiceStorefront
import qualified TDF.Server.SocialEventsHandlers as SocialEvents

type AppM = ReaderT Env Handler

data DomoPricingRules = DomoPricingRules
  { dprEventTypes            :: Map.Map Text Domo.DomoEventRate
  , dprHourMinor             :: Int64
  , dprSetupHourMinor        :: Int64
  , dprCateringMinimumMinor  :: Int64
  , dprCateringPerGuestMinor :: Int64
  , dprProductionMinor       :: Int64
  , dprTransportMinor        :: Int64
  , dprTaxBasisPoints        :: Int
  , dprMaximumGuests         :: Int
  , dprMaximumDurationHours  :: Int
  , dprMaximumSetupHours     :: Int
  } deriving (Eq, Show)

instance FromJSON DomoPricingRules where
  parseJSON = withObject "DomoPricingRules" $ \value -> DomoPricingRules
    <$> value .: "event_types"
    <*> value .: "hour_minor"
    <*> value .: "setup_hour_minor"
    <*> value .: "catering_minimum_minor"
    <*> value .: "catering_per_guest_minor"
    <*> value .: "production_minor"
    <*> value .: "transport_minor"
    <*> value .: "legacy_tax_basis_points"
    <*> value .: "max_guests"
    <*> value .: "max_duration_hours"
    <*> value .: "max_setup_hours"

data DomoPolicySnapshot = DomoPolicySnapshot
  { dpsTermsVersion       :: Text
  , dpsQuoteExpiryMinutes :: Int
  , dpsTimezone           :: Text
  , dpsMinimumLeadHours   :: Int
  , dpsMaximumAdvanceDays :: Int
  } deriving (Eq, Show)

instance FromJSON DomoPolicySnapshot where
  parseJSON = withObject "DomoPolicySnapshot" $ \value -> DomoPolicySnapshot
    <$> value .: "terms_version"
    <*> value .: "quote_expiry_minutes"
    <*> value .: "timezone"
    <*> value .:? "minimum_lead_hours" .!= 24
    <*> value .:? "maximum_advance_days" .!= 730

data ApprovedDomoRateCard = ApprovedDomoRateCard
  { adrcId                 :: Text
  , adrcProductKey         :: Text
  , adrcVersion            :: Int
  , adrcVersionText        :: Text
  , adrcCurrency           :: Text
  , adrcDepositBasisPoints :: Int
  , adrcRulesText          :: Text
  , adrcRulesHash          :: Text
  , adrcRules              :: DomoPricingRules
  , adrcPolicy             :: DomoPolicySnapshot
  } deriving (Eq, Show)

data DomoRuntimeView = DomoRuntimeView
  { drvQuoteId           :: Text
  , drvCheckoutId        :: Text
  , drvQuoteStatus       :: Text
  , drvPaymentStatus     :: Text
  , drvFulfillmentStatus :: Text
  , drvRateCardVersion   :: Text
  , drvCurrency          :: Text
  , drvEventType         :: Text
  , drvGuests            :: Int
  , drvStartsAt          :: UTCTime
  , drvEndsAt            :: UTCTime
  , drvSetupStartsAt     :: UTCTime
  , drvSubtotalMinor     :: Int64
  , drvTaxMinor          :: Int64
  , drvTotalMinor        :: Int64
  , drvDepositMinor      :: Int64
  , drvBalanceMinor      :: Int64
  , drvTimezone          :: Text
  , drvTermsVersion      :: Text
  , drvHoldExpiresAt     :: UTCTime
  , drvTermsAcceptedAt   :: Maybe UTCTime
  , drvDepositPaidAt     :: Maybe UTCTime
  } deriving (Eq, Show)

data DomoPaymentContext = DomoPaymentContext
  { dpcQuoteId             :: Text
  , dpcCheckout            :: Checkout.CheckoutReference
  , dpcCreateIdempotencyKey :: Text
  , dpcQuoteStatus         :: Text
  , dpcCheckoutStatus      :: Text
  , dpcEnvironment         :: Checkout.CheckoutEnvironment
  , dpcAmountMinor         :: Int64
  , dpcCurrency            :: Text
  , dpcHoldExpiresAt       :: UTCTime
  , dpcCustomerName        :: Text
  , dpcCustomerEmail       :: Text
  , dpcCustomerPhone       :: Maybe Text
  } deriving (Eq, Show)

publicDomoQuotesServer :: ServerT Routes.PublicDomoQuotesAPI AppM
publicDomoQuotesServer =
       getPublicDomoStorefront
  :<|> createPublicDomoQuote
  :<|> getPublicDomoQuote
  :<|> acceptPublicDomoQuote
  :<|> createPublicDomoDatafastCheckout
  :<|> confirmPublicDomoDatafastStatus
  :<|> createPublicDomoPaypalOrder
  :<|> capturePublicDomoPaypalOrder

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  Env{ envPool } <- ask
  liftIO (runSqlPool action envPool)

textBody :: Text -> BL.ByteString
textBody = BL.fromStrict . TE.encodeUtf8

badRequest :: Text -> ServerError
badRequest message = err400 { errBody = textBody message }

conflict :: Text -> ServerError
conflict message = err409 { errBody = textBody message }

internal :: Text -> ServerError
internal message = err500 { errBody = textBody message }

notFound :: ServerError
notFound = err404 { errBody = "Domo quote not found" }

sha256Text :: Text -> Text
sha256Text value = TE.decodeUtf8 $
  BAE.convertToBase BAE.Base16
    (hash (TE.encodeUtf8 value) :: Digest SHA256)

hmacSha256Text :: BS.ByteString -> Text -> Text
hmacSha256Text secret value = TE.decodeUtf8 $
  BAE.convertToBase BAE.Base16
    (hmac secret (TE.encodeUtf8 value) :: HMAC SHA256)

loadLookupSecret :: AppM BS.ByteString
loadLookupSecret = do
  configured <- liftIO (lookupEnv "COMMERCE_LOOKUP_TOKEN_SECRET")
  case TE.encodeUtf8 . T.pack <$> configured of
    Just secret | BS.length secret >= 32 -> pure secret
    _ -> throwError err503
      { errBody = "Secure guest Domo quote lookup is not configured in this environment" }

deriveDomoLookupToken :: BS.ByteString -> Text -> Either Text Text
deriveDomoLookupToken secret idempotencyKey
  | BS.length secret < 32 = Left "Domo lookup secret must contain at least 32 bytes"
  | otherwise = Right $ hmacSha256Text secret
      ("domo-quote-lookup:" <> idempotencyKey)

loadCheckoutEnvironment :: AppM Checkout.CheckoutEnvironment
loadCheckoutEnvironment = do
  configured <- liftIO (lookupEnv "COMMERCE_CHECKOUT_ENV")
  either (throwError . internal) pure $
    Checkout.resolveCheckoutEnvironment configured

decodeTextJSON :: FromJSON value => Text -> Either Text value
decodeTextJSON raw = case eitherDecodeStrict' (TE.encodeUtf8 raw) of
  Left message -> Left (T.pack message)
  Right value -> Right value

loadApprovedDomoRateCard :: SqlPersistT IO (Either Text (Maybe ApprovedDomoRateCard))
loadApprovedDomoRateCard = do
  rows <- (rawSql
    "SELECT product.id::text, product.product_key, product.version,\
    \ upper(product.currency), product.deposit_basis_points,\
    \ product.pricing_rules::text, product.policy_snapshot::text\
    \ FROM commerce_product_version product\
    \ JOIN commerce_rate_card_review review ON review.product_version_id = product.id\
    \ WHERE product.domain_type = 'domo' AND product.status = 'active'\
    \ AND product.approved_by IS NOT NULL AND product.approved_at IS NOT NULL\
    \ AND review.domain_type = product.domain_type\
    \ AND review.status = 'approved' AND review.reviewed_by IS NOT NULL\
    \ AND review.reviewed_at IS NOT NULL"
    [] :: SqlPersistT IO
      [( Single Text, Single Text, Single Int, Single Text, Single Int
       , Single Text, Single Text )])
  pure $ case rows of
    [] -> Right Nothing
    [( Single adrcId, Single adrcProductKey, Single adrcVersion
     , Single adrcCurrency, Single adrcDepositBasisPoints
     , Single adrcRulesText, Single policyText )] -> do
        adrcRules <- decodeTextJSON adrcRulesText
        adrcPolicy <- decodeTextJSON policyText
        let adrcVersionText = adrcProductKey <> "-v" <> T.pack (show adrcVersion)
            adrcRulesHash = sha256Text adrcRulesText
        Right (Just ApprovedDomoRateCard{..})
    _ -> Left "More than one approved active Domo rate card is configured"

approvedRateCard :: AppM (Maybe ApprovedDomoRateCard)
approvedRateCard = runDB loadApprovedDomoRateCard >>= either
  (throwError . internal . ("Invalid approved Domo rate card: " <>)) pure

toRateCard :: ApprovedDomoRateCard -> Domo.DomoRateCard
toRateCard ApprovedDomoRateCard{adrcRules = DomoPricingRules{..}, ..} =
  Domo.DomoRateCard
    { Domo.drcEventRates = dprEventTypes
    , Domo.drcHourMinor = dprHourMinor
    , Domo.drcSetupHourMinor = dprSetupHourMinor
    , Domo.drcCateringMinimumMinor = dprCateringMinimumMinor
    , Domo.drcCateringPerGuestMinor = dprCateringPerGuestMinor
    , Domo.drcProductionMinor = dprProductionMinor
    , Domo.drcTransportMinor = dprTransportMinor
    , Domo.drcTaxBasisPoints = dprTaxBasisPoints
    , Domo.drcDepositBasisPoints = adrcDepositBasisPoints
    , Domo.drcMaximumGuests = dprMaximumGuests
    , Domo.drcMaximumDurationHours = dprMaximumDurationHours
    , Domo.drcMaximumSetupHours = dprMaximumSetupHours
    }

getPublicDomoStorefront :: AppM Routes.PublicDomoStorefrontDTO
getPublicDomoStorefront = do
  environment <- loadCheckoutEnvironment
  domainEnabled <- runDB $ Checkout.domainEnabledForEnvironment environment "domo_quotes"
  quoteEnabled <- runDB $ Checkout.capabilityEnabledForEnvironment
    environment "domo.authoritative_quotes"
  rateCard <- approvedRateCard
  let available = domainEnabled && quoteEnabled && isJust rateCard
      reason
        | not domainEnabled = Just "Public Domo quotes are disabled in this environment"
        | not quoteEnabled = Just "Authoritative Domo quotes are disabled in this environment"
        | not (isJust rateCard) = Just "No independently approved active Domo rate card is available"
        | otherwise = Nothing
  pure $ case rateCard of
    Nothing -> Routes.PublicDomoStorefrontDTO
      { Routes.checkoutAvailable = available
      , Routes.unavailableReason = reason
      , Routes.rateCardVersion = Nothing
      , Routes.currency = Nothing
      , Routes.eventTypes = []
      , Routes.maximumGuests = Nothing
      , Routes.maximumDurationHours = Nothing
      , Routes.maximumSetupHours = Nothing
      , Routes.quoteHoldMinutes = Nothing
      , Routes.timezone = "America/Guayaquil"
      }
    Just ApprovedDomoRateCard{adrcRules = DomoPricingRules{..}, adrcPolicy, ..} ->
      Routes.PublicDomoStorefrontDTO
        { Routes.checkoutAvailable = available
        , Routes.unavailableReason = reason
        , Routes.rateCardVersion = Just adrcVersionText
        , Routes.currency = Just adrcCurrency
        , Routes.eventTypes = Map.keys dprEventTypes
        , Routes.maximumGuests = Just dprMaximumGuests
        , Routes.maximumDurationHours = Just dprMaximumDurationHours
        , Routes.maximumSetupHours = Just dprMaximumSetupHours
        , Routes.quoteHoldMinutes = Just (dpsQuoteExpiryMinutes adrcPolicy)
        , Routes.timezone = dpsTimezone adrcPolicy
        }

normalizePhone :: Maybe Text -> Either ServerError (Maybe Text)
normalizePhone raw = case T.strip <$> raw of
  Nothing -> Right Nothing
  Just "" -> Right Nothing
  Just clean
    | T.length clean > 24 -> Left (badRequest "customerPhone is too long")
    | T.any (\char -> not (isAlphaNum char || char `elem` ['+','-',' ','(',')'])) clean ->
        Left (badRequest "customerPhone contains unsupported characters")
    | otherwise -> Right (Just clean)

normalizeNotes :: Maybe Text -> Either ServerError (Maybe Text)
normalizeNotes raw = case T.strip <$> raw of
  Nothing -> Right Nothing
  Just "" -> Right Nothing
  Just clean
    | T.length clean > 4000 -> Left (badRequest "notes is too long")
    | otherwise -> Right (Just clean)

validateQuoteId :: Text -> Either ServerError Text
validateQuoteId raw = case UUID.fromText (T.strip raw) of
  Nothing -> Left notFound
  Just value -> Right (UUID.toText value)

domoRequestEventType :: Routes.PublicDomoQuoteCreateRequest -> Text
domoRequestEventType Routes.PublicDomoQuoteCreateRequest{Routes.eventType = value} = value

domoRequestGuests :: Routes.PublicDomoQuoteCreateRequest -> Int
domoRequestGuests Routes.PublicDomoQuoteCreateRequest{Routes.guests = value} = value

domoRequestStartsAt :: Routes.PublicDomoQuoteCreateRequest -> UTCTime
domoRequestStartsAt Routes.PublicDomoQuoteCreateRequest{Routes.startsAt = value} = value

createPublicDomoQuote
  :: Maybe Text
  -> Routes.PublicDomoQuoteCreateRequest
  -> AppM Routes.PublicDomoQuoteDTO
createPublicDomoQuote mIdempotency request = do
  idempotencyKey <- either (throwError . badRequest) pure $
    ServiceStorefront.validateIdempotencyKey mIdempotency
  validatedName <- either throwError pure $
    SocialEvents.validateTicketPurchaseBuyerName (Just (Routes.customerName request))
  customerName <- maybe (throwError (badRequest "customerName is required")) pure validatedName
  validatedEmail <- either throwError pure $
    SocialEvents.validateTicketPurchaseBuyerEmail (Just (Routes.customerEmail request))
  customerEmail <- maybe (throwError (badRequest "customerEmail is required")) pure validatedEmail
  customerPhone <- either throwError pure (normalizePhone (Routes.customerPhone request))
  customerNotes <- either throwError pure (normalizeNotes (Routes.notes request))
  environment <- loadCheckoutEnvironment
  domainEnabled <- runDB $ Checkout.domainEnabledForEnvironment environment "domo_quotes"
  quoteEnabled <- runDB $ Checkout.capabilityEnabledForEnvironment
    environment "domo.authoritative_quotes"
  unless (domainEnabled && quoteEnabled) $
    throwError err503 { errBody = "Public Domo quotes are disabled in this environment" }
  rateCard <- approvedRateCard >>= maybe
    (throwError (conflict "No independently approved active Domo rate card is available")) pure
  now <- liftIO getCurrentTime
  let policy = adrcPolicy rateCard
      startsAt = domoRequestStartsAt request
      earliest = addUTCTime (fromIntegral (dpsMinimumLeadHours policy) * 3600) now
      latest = addUTCTime (fromIntegral (dpsMaximumAdvanceDays policy) * 86400) now
  when (startsAt < earliest || startsAt > latest) $
    throwError (badRequest "Domo event date is outside the approved booking window")
  let quoteInput = Domo.DomoQuoteInput
        { Domo.dqiEventType = domoRequestEventType request
        , Domo.dqiGuests = domoRequestGuests request
        , Domo.dqiDurationHours = Routes.durationHours request
        , Domo.dqiSetupHours = Routes.setupHours request
        , Domo.dqiCatering = Routes.catering request
        , Domo.dqiProduction = Routes.production request
        , Domo.dqiTransport = Routes.transport request
        }
  breakdown <- either (throwError . badRequest) pure $
    Domo.calculateDomoQuote (toRateCard rateCard) quoteInput
  lookupSecret <- loadLookupSecret
  lookupToken <- either (throwError . internal) pure $
    deriveDomoLookupToken lookupSecret idempotencyKey
  let endsAt = addUTCTime (fromIntegral (Domo.dqbBillableHours breakdown) * 3600) startsAt
      setupStartsAt = addUTCTime (negate (fromIntegral (Routes.setupHours request) * 3600)) startsAt
      holdExpiresAt = addUTCTime
        (fromIntegral (dpsQuoteExpiryMinutes policy) * 60) now
      requestHash = sha256Text . TE.decodeUtf8 . BL.toStrict . encode $ object
        [ "customer_name" .= customerName, "customer_email" .= customerEmail
        , "customer_phone" .= customerPhone, "event_type" .= domoRequestEventType request
        , "guests" .= domoRequestGuests request, "starts_at" .= startsAt
        , "duration_hours" .= Routes.durationHours request
        , "setup_hours" .= Routes.setupHours request
        , "catering" .= Routes.catering request
        , "production" .= Routes.production request
        , "transport" .= Routes.transport request, "notes" .= customerNotes
        , "product_version_id" .= adrcId rateCard
        , "rate_card_rules_sha256" .= adrcRulesHash rateCard
        ]
      lookupHash = sha256Text lookupToken
  existing <- lookupDomoIdempotency idempotencyKey
  case existing of
    Just (quoteId, storedHash)
      | storedHash == requestHash -> loadDomoQuoteDTO quoteId (Just lookupToken)
      | otherwise -> throwError (conflict
          "Idempotency key was already used for a different Domo quote")
    Nothing -> do
      consumeDomoQuoteRateLimit lookupSecret customerEmail
      quoteId <- createDomoQuoteTransaction
        environment now holdExpiresAt endsAt setupStartsAt idempotencyKey requestHash
        lookupHash customerName customerEmail customerPhone customerNotes
        request rateCard breakdown
      loadDomoQuoteDTO quoteId (Just lookupToken)

lookupDomoIdempotency :: Text -> AppM (Maybe (Text, Text))
lookupDomoIdempotency idempotencyKey = do
  rows <- runDB (rawSql
    "SELECT id::text, create_request_sha256 FROM domo_event_quote_runtime\
    \ WHERE create_idempotency_key = ?"
    [PersistText idempotencyKey] :: SqlPersistT IO [(Single Text, Single Text)])
  pure $ case rows of
    [(Single quoteId, Single requestHash)] -> Just (quoteId, requestHash)
    _ -> Nothing

consumeDomoQuoteRateLimit :: BS.ByteString -> Text -> AppM ()
consumeDomoQuoteRateLimit secret customerEmail = do
  let subjectHash = hmacSha256Text secret
        ("domo-quote-rate-limit:" <> T.toLower (T.strip customerEmail))
  rows <- runDB (rawSql
    "INSERT INTO domo_quote_rate_limit(\
    \ scope, subject_hash, window_started_at, request_count, updated_at\
    \) VALUES ('public-create', ?, date_trunc('hour', NOW()), 1, NOW())\
    \ ON CONFLICT(scope, subject_hash, window_started_at) DO UPDATE\
    \ SET request_count = domo_quote_rate_limit.request_count + 1, updated_at = NOW()\
    \ RETURNING request_count"
    [PersistText subjectHash] :: SqlPersistT IO [Single Int])
  case rows of
    [Single requestCount] | requestCount <= 6 -> pure ()
    _ -> throwError err429
      { errBody = "Domo quote rate limit exceeded; try again after the current hour" }

createDomoQuoteTransaction
  :: Checkout.CheckoutEnvironment
  -> UTCTime
  -> UTCTime
  -> UTCTime
  -> UTCTime
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Routes.PublicDomoQuoteCreateRequest
  -> ApprovedDomoRateCard
  -> Domo.DomoQuoteBreakdown
  -> AppM Text
createDomoQuoteTransaction environment now holdExpiresAt endsAt setupStartsAt
    idempotencyKey requestHash lookupHash customerName customerEmail
    customerPhone customerNotes request rateCard breakdown = do
  generatedId <- liftIO (UUID.toText <$> UUID.nextRandom)
  generatedCommerceQuoteId <- liftIO (UUID.toText <$> UUID.nextRandom)
  Env{ envPool } <- ask
  result <- liftIO $
    (try (runSqlPool (transactionBody generatedId generatedCommerceQuoteId) envPool)
      :: IO (Either SomeException (Either ServerError Text)))
  case result of
    Right (Right quoteId) -> pure quoteId
    Right (Left serverError) -> throwError serverError
    Left exception -> case fromException exception :: Maybe SomeAsyncException of
      Just _ -> liftIO (throwIO exception)
      Nothing -> case fromException exception :: Maybe SqlError of
        Just sqlError | sqlState sqlError == "23P01" ->
          throwError (conflict "The requested Domo date is already held or reserved")
        Just sqlError | sqlState sqlError == "23505" ->
          throwError (conflict "Domo quote conflicts with an existing request")
        _ -> liftIO (throwIO exception)
  where
    transactionBody generatedId commerceQuoteId = do
      _ <- (rawSql
        "SELECT 1::bigint FROM (SELECT pg_advisory_xact_lock(hashtextextended(?, 0))) locked"
        [PersistText ("domo-quote:" <> idempotencyKey)]
        :: SqlPersistT IO [Single Int64])
      existing <- (rawSql
        "SELECT id::text, create_request_sha256 FROM domo_event_quote_runtime\
        \ WHERE create_idempotency_key = ?"
        [PersistText idempotencyKey]
        :: SqlPersistT IO [(Single Text, Single Text)])
      case existing of
        [(Single quoteId, Single storedHash)]
          | storedHash == requestHash -> pure (Right quoteId)
          | otherwise -> pure (Left (conflict
              "Idempotency key was already used for a different Domo quote"))
        [] -> createNew generatedId commerceQuoteId
        _ -> pure (Left (internal "Domo quote idempotency lookup was ambiguous"))
    createNew generatedId commerceQuoteId = do
      _ <- (rawSql "SELECT domo_quote_expire_holds(?, NULL)"
        [PersistUTCTime now] :: SqlPersistT IO [Single Int])
      productRows <- (rawSql
        "SELECT pricing_rules::text FROM commerce_product_version product\
        \ WHERE product.id = ?::uuid AND product.status = 'active'\
        \ AND product.approved_by IS NOT NULL AND product.approved_at IS NOT NULL\
        \ AND EXISTS (SELECT 1 FROM commerce_rate_card_review review\
        \   WHERE review.product_version_id = product.id\
        \   AND review.domain_type = product.domain_type AND review.status = 'approved'\
        \   AND review.reviewed_by IS NOT NULL AND review.reviewed_at IS NOT NULL)\
        \ FOR SHARE"
        [PersistText (adrcId rateCard)] :: SqlPersistT IO [Single Text])
      case productRows of
        [Single rulesText] | sha256Text rulesText == adrcRulesHash rateCard -> do
          rawExecute
            "INSERT INTO commerce_quote(\
            \ id, domain_type, domain_subject_id, version, status, currency,\
            \ subtotal_minor, tax_minor, total_minor, deposit_minor, expires_at\
            \) VALUES (?::uuid, 'domo_event_quote', ?, 1, 'sent', ?, ?, ?, ?, ?, ?)"
            [ PersistText commerceQuoteId, PersistText generatedId
            , PersistText (adrcCurrency rateCard)
            , PersistInt64 (Domo.dqbSubtotalMinor breakdown)
            , PersistInt64 (Domo.dqbTaxMinor breakdown)
            , PersistInt64 (Domo.dqbTotalMinor breakdown)
            , PersistInt64 (Domo.dqbDepositMinor breakdown)
            , PersistUTCTime holdExpiresAt
            ]
          forM_ (zip [1 :: Int64 ..] (Domo.dqbLines breakdown)) $ \(lineNumber, line) ->
            rawExecute
              "INSERT INTO commerce_quote_line(\
              \ quote_id, line_number, product_type, product_id, product_version,\
              \ description, quantity, unit_amount_minor, subtotal_minor, configuration\
              \) VALUES (?::uuid, ?, 'domo_rate_component', ?, ?, ?, ?, ?, ?, ?::jsonb)"
              [ PersistText commerceQuoteId, PersistInt64 lineNumber
              , PersistText (Domo.dqlCode line), PersistText (adrcVersionText rateCard)
              , PersistText (Domo.dqlDescription line)
              , PersistInt64 (fromIntegral (Domo.dqlQuantity line))
              , PersistInt64 (Domo.dqlUnitAmountMinor line)
              , PersistInt64 (Domo.dqlSubtotalMinor line)
              , PersistText (jsonText (object
                  [ "event_type" .= domoRequestEventType request
                  , "rate_card_rules_sha256" .= adrcRulesHash rateCard ]))
              ]
          let snapshot = domoCheckoutSnapshot generatedId request rateCard breakdown
          checkout <- Checkout.createHoldingCheckout Checkout.CheckoutCreation
            { Checkout.ccDomainType = "domo_event_quote"
            , Checkout.ccDomainOrderId = "domo-quote:" <> generatedId
            , Checkout.ccEnvironment = environment
            , Checkout.ccCurrency = adrcCurrency rateCard
            , Checkout.ccAmountMinor = Domo.dqbDepositMinor breakdown
            , Checkout.ccCustomerEmail = customerEmail
            , Checkout.ccLookupTokenHash = lookupHash
            , Checkout.ccIdempotencyKey = idempotencyKey
            , Checkout.ccExpiresAt = holdExpiresAt
            , Checkout.ccProductType = "domo_initial_deposit"
            , Checkout.ccProductId = adrcProductKey rateCard
            , Checkout.ccProductVersion = adrcVersionText rateCard
            , Checkout.ccDescription = "Domo del Pululahua initial deposit"
            , Checkout.ccSnapshot = snapshot
            , Checkout.ccCorrelationId = "domo-quote-create:" <> generatedId
            }
          rawExecute
            "UPDATE commerce_checkout_session SET quote_id = ?::uuid WHERE id = ?::uuid"
            [ PersistText commerceQuoteId
            , PersistText (Checkout.checkoutReferenceId checkout)
            ]
          rawExecute
            "INSERT INTO domo_event_quote_runtime(\
            \ id, quote_id, checkout_id, product_version_id, lookup_token_hash,\
            \ create_idempotency_key, create_request_sha256, customer_name, customer_email,\
            \ customer_phone, event_type, guests, starts_at, ends_at, setup_starts_at,\
            \ duration_hours, setup_hours, catering, production, transport, customer_notes,\
            \ quote_status, fulfillment_status, currency, subtotal_minor, tax_minor,\
            \ total_minor, deposit_minor, balance_minor, tax_basis_points,\
            \ deposit_basis_points, rate_card_version, rate_card_rules_sha256,\
            \ timezone, terms_version, hold_expires_at\
            \) VALUES (?::uuid, ?::uuid, ?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,\
            \ ?, ?, ?, ?, ?, ?, 'sent', 'date_held', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
            [ PersistText generatedId, PersistText commerceQuoteId
            , PersistText (Checkout.checkoutReferenceId checkout), PersistText (adrcId rateCard)
            , PersistText lookupHash, PersistText idempotencyKey, PersistText requestHash
            , PersistText customerName, PersistText customerEmail
            , maybe PersistNull PersistText customerPhone
            , PersistText (T.toLower (T.strip (domoRequestEventType request)))
            , PersistInt64 (fromIntegral (domoRequestGuests request))
            , PersistUTCTime (domoRequestStartsAt request), PersistUTCTime endsAt
            , PersistUTCTime setupStartsAt
            , PersistInt64 (fromIntegral (Routes.durationHours request))
            , PersistInt64 (fromIntegral (Routes.setupHours request))
            , PersistBool (Routes.catering request), PersistBool (Routes.production request)
            , PersistBool (Routes.transport request), maybe PersistNull PersistText customerNotes
            , PersistText (adrcCurrency rateCard)
            , PersistInt64 (Domo.dqbSubtotalMinor breakdown)
            , PersistInt64 (Domo.dqbTaxMinor breakdown)
            , PersistInt64 (Domo.dqbTotalMinor breakdown)
            , PersistInt64 (Domo.dqbDepositMinor breakdown)
            , PersistInt64 (Domo.dqbBalanceMinor breakdown)
            , PersistInt64 (fromIntegral (dprTaxBasisPoints (adrcRules rateCard)))
            , PersistInt64 (fromIntegral (adrcDepositBasisPoints rateCard))
            , PersistText (adrcVersionText rateCard), PersistText (adrcRulesHash rateCard)
            , PersistText (dpsTimezone (adrcPolicy rateCard))
            , PersistText (dpsTermsVersion (adrcPolicy rateCard))
            , PersistUTCTime holdExpiresAt
            ]
          rawExecute
            "INSERT INTO commerce_reservation_hold(\
            \ checkout_id, resource_type, resource_id, starts_at, ends_at, quantity, status, expires_at\
            \) VALUES (?::uuid, 'domo_venue', 'domo-del-pululahua', ?, ?, 1, 'active', ?)"
            [ PersistText (Checkout.checkoutReferenceId checkout)
            , PersistUTCTime setupStartsAt, PersistUTCTime endsAt
            , PersistUTCTime holdExpiresAt
            ]
          rawExecute
            "INSERT INTO domo_quote_state_event(\
            \ domo_quote_id, from_status, to_status, actor_type, reason_code, notes\
            \) VALUES (?::uuid, NULL, 'sent', 'system', 'authoritative_quote_created',\
            \ 'Server rate snapshot and expiring venue hold created atomically')"
            [PersistText generatedId]
          pure (Right generatedId)
        _ -> pure (Left (conflict "Approved Domo rate card changed before the date hold was created"))

jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode

domoCheckoutSnapshot
  :: Text
  -> Routes.PublicDomoQuoteCreateRequest
  -> ApprovedDomoRateCard
  -> Domo.DomoQuoteBreakdown
  -> Value
domoCheckoutSnapshot quoteId request rateCard breakdown = object
  [ "domain" .= ("domo_event_quote" :: Text)
  , "quote_id" .= quoteId
  , "event_type" .= domoRequestEventType request
  , "guests" .= domoRequestGuests request
  , "starts_at" .= domoRequestStartsAt request
  , "duration_hours" .= Routes.durationHours request
  , "setup_hours" .= Routes.setupHours request
  , "catering" .= Routes.catering request
  , "production" .= Routes.production request
  , "transport" .= Routes.transport request
  , "product_version_id" .= adrcId rateCard
  , "rate_card_version" .= adrcVersionText rateCard
  , "rate_card_rules_sha256" .= adrcRulesHash rateCard
  , "subtotal_minor" .= Domo.dqbSubtotalMinor breakdown
  , "tax_minor" .= Domo.dqbTaxMinor breakdown
  , "total_minor" .= Domo.dqbTotalMinor breakdown
  , "deposit_minor" .= Domo.dqbDepositMinor breakdown
  , "balance_minor" .= Domo.dqbBalanceMinor breakdown
  , "timezone" .= dpsTimezone (adrcPolicy rateCard)
  , "terms_version" .= dpsTermsVersion (adrcPolicy rateCard)
  ]

loadDomoRuntimeView :: Text -> SqlPersistT IO (Maybe DomoRuntimeView)
loadDomoRuntimeView quoteId = do
  rows <- (rawSql
    "SELECT runtime.id::text, runtime.checkout_id::text, runtime.quote_status,\
    \ checkout.status, runtime.fulfillment_status, runtime.rate_card_version,\
    \ runtime.currency, runtime.event_type, runtime.guests, runtime.starts_at,\
    \ runtime.ends_at, runtime.setup_starts_at, runtime.subtotal_minor,\
    \ runtime.tax_minor, runtime.total_minor, runtime.deposit_minor,\
    \ runtime.balance_minor, runtime.timezone, runtime.terms_version, runtime.hold_expires_at,\
    \ runtime.terms_accepted_at, runtime.deposit_paid_at\
    \ FROM domo_event_quote_runtime runtime\
    \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
    \ JOIN commerce_quote quote ON quote.id = runtime.quote_id\
    \ WHERE runtime.id = ?::uuid AND checkout.domain_type = 'domo_event_quote'\
    \ AND checkout.domain_order_id = 'domo-quote:' || runtime.id::text\
    \ AND checkout.quote_id = runtime.quote_id\
    \ AND checkout.total_minor = runtime.deposit_minor\
    \ AND checkout.currency = runtime.currency\
    \ AND quote.domain_type = 'domo_event_quote'\
    \ AND quote.domain_subject_id = runtime.id::text\
    \ AND quote.total_minor = runtime.total_minor\
    \ AND quote.deposit_minor = runtime.deposit_minor"
    [PersistText quoteId]
    :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Text, Single Text
       , Single Text, Single Text, Single Text, Single Int, Single UTCTime
       , Single UTCTime, Single UTCTime, Single Int64, Single Int64, Single Int64
       , Single Int64, Single Int64, Single Text, Single Text, Single UTCTime
       , Single (Maybe UTCTime), Single (Maybe UTCTime) )])
  pure $ case rows of
    [( Single drvQuoteId, Single drvCheckoutId, Single drvQuoteStatus
     , Single drvPaymentStatus, Single drvFulfillmentStatus
     , Single drvRateCardVersion, Single drvCurrency, Single drvEventType
     , Single drvGuests, Single drvStartsAt, Single drvEndsAt, Single drvSetupStartsAt
     , Single drvSubtotalMinor, Single drvTaxMinor, Single drvTotalMinor
     , Single drvDepositMinor, Single drvBalanceMinor, Single drvTimezone
     , Single drvTermsVersion
     , Single drvHoldExpiresAt, Single drvTermsAcceptedAt, Single drvDepositPaidAt
     )] -> Just DomoRuntimeView{..}
    _ -> Nothing

loadDomoQuoteLines :: Text -> SqlPersistT IO [Routes.PublicDomoQuoteLineDTO]
loadDomoQuoteLines quoteId = do
  rows <- (rawSql
    "SELECT line.product_id, line.description, line.quantity,\
    \ line.unit_amount_minor, line.subtotal_minor\
    \ FROM domo_event_quote_runtime runtime\
    \ JOIN commerce_quote_line line ON line.quote_id = runtime.quote_id\
    \ WHERE runtime.id = ?::uuid ORDER BY line.line_number"
    [PersistText quoteId]
    :: SqlPersistT IO
      [(Single Text, Single Text, Single Int, Single Int64, Single Int64)])
  pure
    [ Routes.PublicDomoQuoteLineDTO code description quantity unitAmount subtotal
    | (Single code, Single description, Single quantity, Single unitAmount, Single subtotal) <- rows
    ]

loadDomoQuoteDTO :: Text -> Maybe Text -> AppM Routes.PublicDomoQuoteDTO
loadDomoQuoteDTO quoteId lookupToken = do
  runtime <- runDB (loadDomoRuntimeView quoteId) >>= maybe (throwError notFound) pure
  quoteLines <- runDB (loadDomoQuoteLines quoteId)
  paymentMethods <- loadDomoPaymentMethods runtime
  pure Routes.PublicDomoQuoteDTO
    { Routes.quoteId = drvQuoteId runtime
    , Routes.checkoutId = drvCheckoutId runtime
    , Routes.lookupToken = lookupToken
    , Routes.quoteStatus = drvQuoteStatus runtime
    , Routes.paymentStatus = drvPaymentStatus runtime
    , Routes.fulfillmentStatus = drvFulfillmentStatus runtime
    , Routes.rateCardVersion = drvRateCardVersion runtime
    , Routes.currency = drvCurrency runtime
    , Routes.eventType = drvEventType runtime
    , Routes.guests = drvGuests runtime
    , Routes.startsAt = drvStartsAt runtime
    , Routes.endsAt = drvEndsAt runtime
    , Routes.setupStartsAt = drvSetupStartsAt runtime
    , Routes.lines = quoteLines
    , Routes.subtotalMinor = drvSubtotalMinor runtime
    , Routes.taxMinor = drvTaxMinor runtime
    , Routes.totalMinor = drvTotalMinor runtime
    , Routes.depositMinor = drvDepositMinor runtime
    , Routes.balanceMinor = drvBalanceMinor runtime
    , Routes.timezone = drvTimezone runtime
    , Routes.termsVersion = drvTermsVersion runtime
    , Routes.holdExpiresAt = drvHoldExpiresAt runtime
    , Routes.termsAcceptedAt = drvTermsAcceptedAt runtime
    , Routes.depositPaidAt = drvDepositPaidAt runtime
    , Routes.paymentMethods = paymentMethods
    }

loadDomoPaymentMethods :: DomoRuntimeView -> AppM [Text]
loadDomoPaymentMethods runtime = do
  now <- liftIO getCurrentTime
  if drvQuoteStatus runtime /= "deposit_due"
      || drvPaymentStatus runtime `notElem` ["awaiting_payment", "failed", "processing"]
      || drvHoldExpiresAt runtime <= now
    then pure []
    else do
      let checkout = Checkout.CheckoutReference (drvCheckoutId runtime)
      environmentResult <- runDB (Checkout.loadCheckoutEnvironment checkout)
      case environmentResult of
        Left _ -> pure []
        Right environment -> do
          domainEnabled <- runDB $ Checkout.domainEnabledForEnvironment environment "domo_quotes"
          checkoutEnabled <- runDB $ Checkout.capabilityEnabledForEnvironment
            environment "domo.checkout"
          if not (domainEnabled && checkoutEnabled) then pure [] else do
            datafastEnabled <- ((\datafast -> do
                if ServiceStorefront.sdfEnvironment datafast /= environment
                  then pure False
                  else runDB $ Checkout.providerEnabledForEnvironment
                    environment Checkout.ProviderDatafast)
              =<< ServiceStorefront.loadServiceDatafastEnv)
              `catchError` const (pure False)
            paypalEnabled <- ((\(_, _, _, configuredEnvironment, _) -> do
                if configuredEnvironment /= environment
                  then pure False
                  else runDB $ Checkout.providerEnabledForEnvironment
                    environment Checkout.ProviderPayPal)
              =<< ServiceStorefront.loadPaypalEnvForService)
              `catchError` const (pure False)
            pure $ ["datafast" | datafastEnabled] <> ["paypal" | paypalEnabled]

requireLookupToken :: Text -> Maybe Text -> AppM ()
requireLookupToken quoteId mLookupToken = do
  supplied <- case T.strip <$> mLookupToken of
    Just value | not (T.null value) -> pure value
    _ -> throwError notFound
  hashes <- runDB (rawSql
    "SELECT lookup_token_hash FROM domo_event_quote_runtime WHERE id = ?::uuid"
    [PersistText quoteId] :: SqlPersistT IO [Single Text])
  stored <- case hashes of
    [Single value] -> pure value
    _ -> throwError notFound
  unless (constEq (TE.encodeUtf8 stored) (TE.encodeUtf8 (sha256Text supplied))) $
    throwError notFound

authorizeDomoQuote :: Text -> Maybe Text -> AppM (Text, DomoRuntimeView)
authorizeDomoQuote rawQuoteId mLookupToken = do
  quoteId <- either throwError pure (validateQuoteId rawQuoteId)
  requireLookupToken quoteId mLookupToken
  runtime <- runDB (loadDomoRuntimeView quoteId) >>= maybe (throwError notFound) pure
  pure (quoteId, runtime)

expireDomoQuoteIfNeeded :: Text -> DomoRuntimeView -> AppM ()
expireDomoQuoteIfNeeded quoteId runtime = do
  now <- liftIO getCurrentTime
  when (drvHoldExpiresAt runtime <= now
      && drvQuoteStatus runtime `elem` ["sent","viewed","accepted","deposit_due"]
      && drvPaymentStatus runtime `elem` ["holding","awaiting_payment","processing","failed"]) $
    void $ runDB (rawSql "SELECT domo_quote_expire_holds(?, ?::uuid)"
      [PersistUTCTime now, PersistText quoteId] :: SqlPersistT IO [Single Int])

getPublicDomoQuote :: Text -> Maybe Text -> AppM Routes.PublicDomoQuoteDTO
getPublicDomoQuote rawQuoteId mLookupToken = do
  (quoteId, runtime) <- authorizeDomoQuote rawQuoteId mLookupToken
  expireDomoQuoteIfNeeded quoteId runtime
  now <- liftIO getCurrentTime
  when (drvQuoteStatus runtime == "sent" && drvHoldExpiresAt runtime > now) $ do
    changed <- runDB (rawSql
      "UPDATE domo_event_quote_runtime SET quote_status = 'viewed', updated_at = ?\
      \ WHERE id = ?::uuid AND quote_status = 'sent' RETURNING quote_id::text"
      [PersistUTCTime now, PersistText quoteId] :: SqlPersistT IO [Single Text])
    forM_ changed $ \(Single commerceQuoteId) -> runDB $ do
      rawExecute "UPDATE commerce_quote SET status = 'viewed' WHERE id = ?::uuid AND status = 'sent'"
        [PersistText commerceQuoteId]
      rawExecute
        "INSERT INTO domo_quote_state_event(\
        \ domo_quote_id, from_status, to_status, actor_type, reason_code\
        \) VALUES (?::uuid, 'sent', 'viewed', 'customer', 'secure_quote_viewed')"
        [PersistText quoteId]
  loadDomoQuoteDTO quoteId Nothing

acceptPublicDomoQuote
  :: Text
  -> Maybe Text
  -> Routes.PublicDomoQuoteAcceptRequest
  -> AppM Routes.PublicDomoQuoteDTO
acceptPublicDomoQuote rawQuoteId mLookupToken request = do
  unless (Routes.termsAccepted request) $
    throwError (badRequest "Versioned Domo terms must be accepted before deposit payment")
  (quoteId, runtime) <- authorizeDomoQuote rawQuoteId mLookupToken
  expireDomoQuoteIfNeeded quoteId runtime
  now <- liftIO getCurrentTime
  Env{ envPool } <- ask
  result <- liftIO $ runSqlPool (do
    rows <- (rawSql
      "SELECT runtime.quote_status, runtime.hold_expires_at, runtime.quote_id::text,\
      \ runtime.checkout_id::text, checkout.status\
      \ FROM domo_event_quote_runtime runtime\
      \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
      \ WHERE runtime.id = ?::uuid FOR UPDATE OF runtime, checkout"
      [PersistText quoteId]
      :: SqlPersistT IO
        [(Single Text, Single UTCTime, Single Text, Single Text, Single Text)])
    case rows of
      [(Single quoteStatus, Single expiresAt, Single commerceQuoteId
       , Single checkoutId, Single checkoutStatus)]
        | quoteStatus == "deposit_due" && checkoutStatus `elem` ["awaiting_payment","processing","failed"] ->
            pure (Right ())
        | quoteStatus `elem` ["sent","viewed"]
        , checkoutStatus == "holding"
        , expiresAt > now -> do
            rawExecute
              "UPDATE domo_event_quote_runtime SET quote_status = 'accepted',\
              \ terms_accepted_at = ?, updated_at = ? WHERE id = ?::uuid"
              [PersistUTCTime now, PersistUTCTime now, PersistText quoteId]
            rawExecute
              "UPDATE commerce_quote SET status = 'accepted', accepted_at = ?,\
              \ accepted_terms_version = (SELECT terms_version FROM domo_event_quote_runtime\
              \ WHERE id = ?::uuid) WHERE id = ?::uuid"
              [PersistUTCTime now, PersistText quoteId, PersistText commerceQuoteId]
            rawExecute
              "INSERT INTO domo_quote_state_event(\
              \ domo_quote_id, from_status, to_status, actor_type, reason_code\
              \) VALUES (?::uuid, ?, 'accepted', 'customer', 'versioned_terms_accepted')"
              [PersistText quoteId, PersistText quoteStatus]
            rawExecute
              "UPDATE domo_event_quote_runtime SET quote_status = 'deposit_due', updated_at = ?\
              \ WHERE id = ?::uuid AND quote_status = 'accepted'"
              [PersistUTCTime now, PersistText quoteId]
            rawExecute
              "UPDATE commerce_checkout_session SET status = 'awaiting_payment', updated_at = ?\
              \ WHERE id = ?::uuid AND status = 'holding'"
              [PersistUTCTime now, PersistText checkoutId]
            rawExecute
              "INSERT INTO domo_quote_state_event(\
              \ domo_quote_id, from_status, to_status, actor_type, reason_code, notes\
              \) VALUES (?::uuid, 'accepted', 'deposit_due', 'system',\
              \ 'deposit_checkout_opened', 'Quote acceptance does not mean payment')"
              [PersistText quoteId]
            rawExecute
              "INSERT INTO commerce_checkout_audit_event(\
              \ checkout_id, event_type, from_status, to_status, actor_type,\
              \ correlation_id, metadata\
              \) VALUES (?::uuid, 'quote_accepted', 'holding', 'awaiting_payment',\
              \ 'customer', ?, jsonb_build_object('terms_accepted', true))"
              [PersistText checkoutId, PersistText ("domo-quote-accept:" <> quoteId)]
            pure (Right ())
        | expiresAt <= now -> pure (Left (conflict "Domo quote hold has expired"))
        | otherwise -> pure (Left (conflict "Domo quote cannot be accepted in its current state"))
      _ -> pure (Left notFound)) envPool
  either throwError pure result
  loadDomoQuoteDTO quoteId Nothing

loadDomoPaymentContext :: Text -> AppM DomoPaymentContext
loadDomoPaymentContext quoteId = do
  rows <- runDB (rawSql
    "SELECT runtime.checkout_id::text, runtime.create_idempotency_key,\
    \ runtime.quote_status, checkout.status, checkout.environment, runtime.deposit_minor,\
    \ runtime.currency, runtime.hold_expires_at, runtime.customer_name,\
    \ runtime.customer_email, runtime.customer_phone\
    \ FROM domo_event_quote_runtime runtime\
    \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
    \ WHERE runtime.id = ?::uuid AND checkout.domain_type = 'domo_event_quote'\
    \ AND checkout.domain_order_id = 'domo-quote:' || runtime.id::text\
    \ AND checkout.total_minor = runtime.deposit_minor AND checkout.currency = runtime.currency"
    [PersistText quoteId]
    :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Text, Single Text
       , Single Int64, Single Text, Single UTCTime, Single Text, Single Text
       , Single (Maybe Text) )])
  case rows of
    [( Single checkoutId, Single createKey, Single quoteStatus, Single checkoutStatus
     , Single environmentText, Single amountMinor, Single currency, Single expiresAt
     , Single customerName, Single customerEmail, Single customerPhone )] -> do
      environment <- either (throwError . internal) pure $
        Checkout.resolveCheckoutEnvironment (Just (T.unpack environmentText))
      pure DomoPaymentContext
        { dpcQuoteId = quoteId
        , dpcCheckout = Checkout.CheckoutReference checkoutId
        , dpcCreateIdempotencyKey = createKey
        , dpcQuoteStatus = quoteStatus
        , dpcCheckoutStatus = checkoutStatus
        , dpcEnvironment = environment
        , dpcAmountMinor = amountMinor
        , dpcCurrency = currency
        , dpcHoldExpiresAt = expiresAt
        , dpcCustomerName = customerName
        , dpcCustomerEmail = customerEmail
        , dpcCustomerPhone = customerPhone
        }
    _ -> throwError notFound

requireDomoPaymentContext :: Text -> Maybe Text -> AppM DomoPaymentContext
requireDomoPaymentContext rawQuoteId mLookupToken = do
  (quoteId, runtime) <- authorizeDomoQuote rawQuoteId mLookupToken
  expireDomoQuoteIfNeeded quoteId runtime
  context <- loadDomoPaymentContext quoteId
  now <- liftIO getCurrentTime
  unless (dpcQuoteStatus context `elem` ["deposit_due","deposit_paid"]
      && dpcCheckoutStatus context `elem` ["awaiting_payment","processing","failed","paid"]
      && (dpcCheckoutStatus context == "paid" || dpcHoldExpiresAt context > now)) $
    throwError (conflict "Domo quote is not eligible for deposit payment")
  pure context

requireDomoDatafastStatusContext :: Text -> Maybe Text -> AppM DomoPaymentContext
requireDomoDatafastStatusContext rawQuoteId mLookupToken = do
  (quoteId, runtime) <- authorizeDomoQuote rawQuoteId mLookupToken
  expireDomoQuoteIfNeeded quoteId runtime
  context <- loadDomoPaymentContext quoteId
  now <- liftIO getCurrentTime
  let active = dpcQuoteStatus context `elem` ["deposit_due","deposit_paid"]
        && dpcCheckoutStatus context `elem` ["awaiting_payment","processing","failed","paid"]
        && (dpcCheckoutStatus context == "paid" || dpcHoldExpiresAt context > now)
      expired = dpcQuoteStatus context == "expired" && dpcCheckoutStatus context == "expired"
  unless (active || expired) $
    throwError (conflict "Domo quote is not eligible for provider status verification")
  pure context

domoReference :: DomoPaymentContext -> Text
domoReference context = "domo-quote:" <> dpcQuoteId context

requireDomoProvider
  :: DomoPaymentContext
  -> Checkout.CheckoutEnvironment
  -> Checkout.PaymentProvider
  -> AppM ()
requireDomoProvider context configuredEnvironment provider = do
  unless (configuredEnvironment == dpcEnvironment context) $
    throwError err503 { errBody = "Provider environment does not match this immutable Domo checkout" }
  domainEnabled <- runDB $ Checkout.domainEnabledForEnvironment
    (dpcEnvironment context) "domo_quotes"
  checkoutEnabled <- runDB $ Checkout.capabilityEnabledForEnvironment
    (dpcEnvironment context) "domo.checkout"
  providerEnabled <- runDB $ Checkout.providerEnabledForEnvironment
    (dpcEnvironment context) provider
  unless (domainEnabled && checkoutEnabled && providerEnabled) $
    throwError err503 { errBody = "Domo deposit checkout is disabled in this environment" }

domoPaymentIdempotencyKey
  :: DomoPaymentContext
  -> Checkout.PaymentProvider
  -> Checkout.PaymentOperation
  -> Text
domoPaymentIdempotencyKey context provider operation =
  dpcCreateIdempotencyKey context <> ":" <> Checkout.paymentProviderText provider
    <> ":" <> case operation of
      Checkout.OperationCreate -> "create"
      Checkout.OperationAuthorize -> "authorize"
      Checkout.OperationCapture -> "capture"
      Checkout.OperationManualVerify -> "manual-verify"

domoPaymentCorrelationId :: DomoPaymentContext -> Checkout.PaymentProvider -> Text -> Text
domoPaymentCorrelationId context provider operation =
  "domo-quote:" <> dpcQuoteId context <> ":"
    <> Checkout.paymentProviderText provider <> ":" <> operation

beginDomoPaymentAttempt
  :: DomoPaymentContext
  -> Checkout.PaymentProvider
  -> Checkout.PaymentOperation
  -> Text
  -> Text
  -> AppM Checkout.PaymentAttemptReference
beginDomoPaymentAttempt context provider operation merchantRef operationLabel = do
  now <- liftIO getCurrentTime
  result <- runDB $ Checkout.beginPaymentAttempt Checkout.PaymentAttemptCreation
    { Checkout.pacCheckout = dpcCheckout context
    , Checkout.pacProvider = provider
    , Checkout.pacEnvironment = dpcEnvironment context
    , Checkout.pacOperation = operation
    , Checkout.pacAmountMinor = dpcAmountMinor context
    , Checkout.pacCurrency = dpcCurrency context
    , Checkout.pacMerchantRef = merchantRef
    , Checkout.pacIdempotencyKey = domoPaymentIdempotencyKey context provider operation
    , Checkout.pacCreatedAt = now
    , Checkout.pacCorrelationId = domoPaymentCorrelationId context provider operationLabel
    }
  either (throwError . conflict) pure result

failDomoPaymentAttempt
  :: DomoPaymentContext
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Text
  -> ServerError
  -> AppM a
failDomoPaymentAttempt context attempt provider failureCode providerError = do
  now <- liftIO getCurrentTime
  runDB $ Checkout.recordPaymentFailure
    (dpcCheckout context) attempt provider failureCode
    (domoPaymentCorrelationId context provider "provider-error") now
  throwError providerError

loadDomoProviderBinding
  :: DomoPaymentContext
  -> Checkout.PaymentProvider
  -> Text
  -> Text
  -> AppM (Maybe (Text, Maybe Text))
loadDomoProviderBinding context provider merchantRef resourceType = do
  rows <- runDB (rawSql
    "SELECT binding.provider_resource_id, binding.provider_resource_path\
    \ FROM commerce_provider_binding binding\
    \ JOIN commerce_payment_attempt attempt ON attempt.id = binding.payment_attempt_id\
    \ WHERE attempt.checkout_id = ?::uuid AND attempt.provider = ?\
    \ AND attempt.environment = ? AND attempt.merchant_account_ref = ?\
    \ AND binding.resource_type = ? AND binding.merchant_reference = ?\
    \ AND binding.amount_minor = ? AND binding.currency = ?"
    [ PersistText (Checkout.checkoutReferenceId (dpcCheckout context))
    , PersistText (Checkout.paymentProviderText provider)
    , PersistText (Checkout.checkoutEnvironmentText (dpcEnvironment context))
    , PersistText merchantRef, PersistText resourceType
    , PersistText (domoReference context), PersistInt64 (dpcAmountMinor context)
    , PersistText (dpcCurrency context)
    ] :: SqlPersistT IO [(Single Text, Single (Maybe Text))])
  case rows of
    [] -> pure Nothing
    [(Single resourceId, Single resourcePath)] -> pure (Just (resourceId, resourcePath))
    _ -> throwError (internal "Domo provider binding is ambiguous")

bindDomoProviderResource
  :: DomoPaymentContext
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Checkout.CheckoutEnvironment
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Checkout.PaymentAttemptStage
  -> Text
  -> AppM ()
bindDomoProviderResource context attempt provider environment merchantRef resourceType
    resourceId resourcePath stage operationLabel = do
  now <- liftIO getCurrentTime
  result <- runDB $ Checkout.bindProviderResource Checkout.ProviderBindingCreation
    { Checkout.pbcAttempt = attempt
    , Checkout.pbcCheckout = dpcCheckout context
    , Checkout.pbcProvider = provider
    , Checkout.pbcEnvironment = environment
    , Checkout.pbcMerchantRef = merchantRef
    , Checkout.pbcResourceType = resourceType
    , Checkout.pbcProviderResource = resourceId
    , Checkout.pbcResourcePath = resourcePath
    , Checkout.pbcOrderReference = domoReference context
    , Checkout.pbcAmountMinor = dpcAmountMinor context
    , Checkout.pbcCurrency = dpcCurrency context
    , Checkout.pbcStage = stage
    , Checkout.pbcOccurredAt = now
    , Checkout.pbcCorrelationId = domoPaymentCorrelationId context provider operationLabel
    }
  either (throwError . conflict) pure result

ensureNoOtherActiveAttempt :: DomoPaymentContext -> Checkout.PaymentProvider -> AppM ()
ensureNoOtherActiveAttempt context provider = do
  rows <- runDB (rawSql
    "SELECT EXISTS (SELECT 1 FROM commerce_payment_attempt\
    \ WHERE checkout_id = ?::uuid AND provider <> ?\
    \ AND status IN ('requires_customer_action','processing'))"
    [ PersistText (Checkout.checkoutReferenceId (dpcCheckout context))
    , PersistText (Checkout.paymentProviderText provider)
    ] :: SqlPersistT IO [Single Bool])
  when (rows == [Single True]) $
    throwError (conflict "Another online payment rail is active for this Domo deposit")

createPublicDomoDatafastCheckout
  :: Text -> Maybe Text -> AppM APITypes.DatafastCheckoutDTO
createPublicDomoDatafastCheckout rawQuoteId mLookupToken = do
  context <- requireDomoPaymentContext rawQuoteId mLookupToken
  when (dpcCheckoutStatus context == "paid") $
    throwError (conflict "This Domo deposit is already paid")
  ensureNoOtherActiveAttempt context Checkout.ProviderDatafast
  datafast <- ServiceStorefront.loadServiceDatafastEnv
  requireDomoProvider context (ServiceStorefront.sdfEnvironment datafast) Checkout.ProviderDatafast
  attempt <- beginDomoPaymentAttempt context Checkout.ProviderDatafast
    Checkout.OperationCreate (ServiceStorefront.sdfEntityId datafast) "create"
  existing <- loadDomoProviderBinding context Checkout.ProviderDatafast
    (ServiceStorefront.sdfEntityId datafast) "checkout"
  (checkoutId, widgetUrl) <- case existing of
    Just (storedCheckoutId, _) -> pure
      ( storedCheckoutId
      , stripTrailingSlash (ServiceStorefront.sdfBaseUrl datafast)
          <> "/v1/paymentWidgets.js?checkoutId=" <> T.unpack storedCheckoutId
      )
    Nothing -> ServiceStorefront.requestDatafastCheckoutForService
      (domoReference context) (fromIntegral (dpcAmountMinor context))
      (dpcCurrency context) (dpcCustomerName context) (dpcCustomerEmail context)
      (dpcCustomerPhone context)
      `catchError` failDomoPaymentAttempt context attempt
        Checkout.ProviderDatafast "datafast_checkout_create"
  let resourcePath = "/v1/checkouts/" <> checkoutId <> "/payment"
  bindDomoProviderResource context attempt Checkout.ProviderDatafast
    (ServiceStorefront.sdfEnvironment datafast) (ServiceStorefront.sdfEntityId datafast)
    "checkout" checkoutId (Just resourcePath)
    Checkout.AttemptRequiresCustomerAction "create"
  pure APITypes.DatafastCheckoutDTO
    { APITypes.dcOrderId = domoReference context
    , APITypes.dcCheckoutId = checkoutId
    , APITypes.dcWidgetUrl = T.pack widgetUrl
    , APITypes.dcAmount = Internationalization.formatMinorUnitsDecimal
        (dpcCurrency context) (fromIntegral (dpcAmountMinor context))
    , APITypes.dcCurrency = dpcCurrency context
    , APITypes.dcLookupToken = Nothing
    }

stripTrailingSlash :: String -> String
stripTrailingSlash = reverse . dropWhile (== '/') . reverse

confirmPublicDomoDatafastStatus
  :: Text -> Maybe Text -> Text -> AppM Routes.PublicDomoQuoteDTO
confirmPublicDomoDatafastStatus rawQuoteId mLookupToken rawResourcePath = do
  context <- requireDomoDatafastStatusContext rawQuoteId mLookupToken
  if dpcCheckoutStatus context == "paid"
    then finalizeVerifiedDomoDeposit context >> loadDomoQuoteDTO (dpcQuoteId context) Nothing
    else do
      datafast <- ServiceStorefront.loadServiceDatafastEnv
      unless (ServiceStorefront.sdfEnvironment datafast == dpcEnvironment context) $
        throwError err503 { errBody = "Configured Datafast environment does not match this Domo checkout" }
      existing <- loadDomoProviderBinding context Checkout.ProviderDatafast
        (ServiceStorefront.sdfEntityId datafast) "checkout"
      (checkoutId, storedPath) <- maybe
        (throwError (conflict "This Domo quote has no bound Datafast checkout")) pure existing
      resourcePath <- either (throwError . badRequest) pure $
        ServiceStorefront.validateDatafastOrderResourcePath (Just checkoutId) rawResourcePath
      unless (storedPath == Just resourcePath) $
        throwError (conflict "Datafast resource path does not match the immutable Domo binding")
      attempt <- beginDomoPaymentAttempt context Checkout.ProviderDatafast
        Checkout.OperationCreate (ServiceStorefront.sdfEntityId datafast) "status"
      providerStatus <- ServiceStorefront.checkDatafastPaymentStatus resourcePath
        `catchError` failDomoPaymentAttempt context attempt
          Checkout.ProviderDatafast "datafast_status_request"
      now <- liftIO getCurrentTime
      let resultCode = ServiceStorefront.sdfpsResultCode providerStatus
          success = ServiceStorefront.isDatafastPaymentSuccess
            (ServiceStorefront.sdfEnvironment datafast) resultCode
          pending = resultCode == "000.200.000"
      if success then do
        case ServiceStorefront.validateDatafastSuccessfulPayment
            (domoReference context) (fromIntegral (dpcAmountMinor context))
            (dpcCurrency context) providerStatus of
          Left validationMessage -> do
            let actualAmount = ServiceStorefront.sdfpsAmount providerStatus
                  >>= either (const Nothing) (Just . fromIntegral)
                    . ServiceStorefront.parseDatafastCents
                providerRef = fromMaybe checkoutId (ServiceStorefront.sdfpsPaymentId providerStatus)
            runDB $ do
              Checkout.recordReconciliationException Checkout.ProviderDatafast
                (dpcEnvironment context) (ServiceStorefront.sdfEntityId datafast)
                "provider_binding_mismatch" (domoReference context) providerRef
                (dpcAmountMinor context) actualAmount (dpcCurrency context) now
              Checkout.recordPaymentFailure (dpcCheckout context) attempt
                Checkout.ProviderDatafast "provider_binding_mismatch"
                (domoPaymentCorrelationId context Checkout.ProviderDatafast "status") now
            throwError err502 { errBody = textBody validationMessage }
          Right () -> pure ()
        paymentId <- maybe (throwError err502 { errBody = "Datafast payment ID is missing" }) pure
          (ServiceStorefront.sdfpsPaymentId providerStatus)
        bindDomoProviderResource context attempt Checkout.ProviderDatafast
          (dpcEnvironment context) (ServiceStorefront.sdfEntityId datafast)
          "payment" paymentId (Just resourcePath) Checkout.AttemptProcessing "status"
        when (dpcHoldExpiresAt context <= now || dpcCheckoutStatus context == "expired") $ do
          runDB $ do
            Checkout.recordReconciliationException Checkout.ProviderDatafast
              (dpcEnvironment context) (ServiceStorefront.sdfEntityId datafast)
              "payment_after_domo_hold_expiry" (domoReference context) paymentId
              (dpcAmountMinor context) Nothing (dpcCurrency context) now
            void (rawSql "SELECT domo_quote_expire_holds(?, ?::uuid)"
              [PersistUTCTime now, PersistText (dpcQuoteId context)] :: SqlPersistT IO [Single Int])
          throwError (conflict
            "Datafast reports payment after this Domo hold expired; reconciliation is required and the date was not reserved")
        verified <- runDB $ Checkout.recordVerifiedPayment Checkout.VerifiedPayment
          { Checkout.vpAttempt = attempt
          , Checkout.vpCheckout = dpcCheckout context
          , Checkout.vpProvider = Checkout.ProviderDatafast
          , Checkout.vpEnvironment = dpcEnvironment context
          , Checkout.vpMerchantRef = ServiceStorefront.sdfEntityId datafast
          , Checkout.vpResourceType = "checkout"
          , Checkout.vpProviderResource = checkoutId
          , Checkout.vpProviderResourcePath = Just resourcePath
          , Checkout.vpOrderReference = domoReference context
          , Checkout.vpAmountMinor = dpcAmountMinor context
          , Checkout.vpCurrency = dpcCurrency context
          , Checkout.vpEvidence = "server_to_server"
          , Checkout.vpOccurredAt = now
          , Checkout.vpCorrelationId = domoPaymentCorrelationId
              context Checkout.ProviderDatafast "status"
          }
        either (throwError . conflict) (const (pure ())) verified
        finalizeVerifiedDomoDeposit context
      else if pending
        then runDB $ Checkout.recordPaymentProcessing
          (dpcCheckout context) attempt Checkout.ProviderDatafast
          (domoPaymentCorrelationId context Checkout.ProviderDatafast "status") now
        else runDB $ Checkout.recordPaymentFailure
          (dpcCheckout context) attempt Checkout.ProviderDatafast resultCode
          (domoPaymentCorrelationId context Checkout.ProviderDatafast "status") now
      loadDomoQuoteDTO (dpcQuoteId context) Nothing

createPublicDomoPaypalOrder :: Text -> Maybe Text -> AppM APITypes.PaypalCreateDTO
createPublicDomoPaypalOrder rawQuoteId mLookupToken = do
  context <- requireDomoPaymentContext rawQuoteId mLookupToken
  when (dpcCheckoutStatus context == "paid") $
    throwError (conflict "This Domo deposit is already paid")
  ensureNoOtherActiveAttempt context Checkout.ProviderPayPal
  (clientId, clientSecret, baseUrl, paypalEnvironment, merchantRef) <-
    ServiceStorefront.loadPaypalEnvForService
  requireDomoProvider context paypalEnvironment Checkout.ProviderPayPal
  attempt <- beginDomoPaymentAttempt context Checkout.ProviderPayPal
    Checkout.OperationCreate merchantRef "create"
  existing <- loadDomoProviderBinding context Checkout.ProviderPayPal merchantRef "order"
  (paypalOrderId, approvalUrl) <- case existing of
    Just (storedOrderId, _) -> pure (storedOrderId, Nothing)
    Nothing -> ServiceStorefront.createPaypalOrderRemoteForService
      sharedTlsManager clientId clientSecret baseUrl (domoReference context)
      (fromIntegral (dpcAmountMinor context)) (dpcCurrency context)
      (dpcCustomerName context) (dpcCustomerEmail context)
      `catchError` failDomoPaymentAttempt context attempt
        Checkout.ProviderPayPal "paypal_create_order"
  bindDomoProviderResource context attempt Checkout.ProviderPayPal paypalEnvironment
    merchantRef "order" paypalOrderId
    (Just ("/v2/checkout/orders/" <> paypalOrderId))
    Checkout.AttemptRequiresCustomerAction "create"
  pure APITypes.PaypalCreateDTO
    { APITypes.pcOrderId = domoReference context
    , APITypes.pcPaypalOrderId = paypalOrderId
    , APITypes.pcApprovalUrl = approvalUrl
    , APITypes.pcLookupToken = Nothing
    }

validatePaypalOrderId :: Text -> Either ServerError Text
validatePaypalOrderId raw
  | T.length clean < 6 || T.length clean > 80 = Left (badRequest "PayPal order ID is invalid")
  | T.any (\char -> not (isAlphaNum char || char `elem` ['-','_'])) clean =
      Left (badRequest "PayPal order ID is invalid")
  | otherwise = Right clean
  where clean = T.strip raw

capturePublicDomoPaypalOrder
  :: Text
  -> Maybe Text
  -> Routes.PublicDomoPaypalCaptureRequest
  -> AppM Routes.PublicDomoQuoteDTO
capturePublicDomoPaypalOrder rawQuoteId mLookupToken request = do
  context <- requireDomoPaymentContext rawQuoteId mLookupToken
  if dpcCheckoutStatus context == "paid"
    then finalizeVerifiedDomoDeposit context >> loadDomoQuoteDTO (dpcQuoteId context) Nothing
    else do
      suppliedOrderId <- either throwError pure $
        validatePaypalOrderId (Routes.paypalOrderId request)
      (clientId, clientSecret, baseUrl, paypalEnvironment, merchantRef) <-
        ServiceStorefront.loadPaypalEnvForService
      requireDomoProvider context paypalEnvironment Checkout.ProviderPayPal
      existing <- loadDomoProviderBinding context Checkout.ProviderPayPal merchantRef "order"
      storedOrderId <- maybe
        (throwError (conflict "This Domo quote has no bound PayPal order"))
        (pure . fst) existing
      unless (storedOrderId == suppliedOrderId) $
        throwError (conflict "PayPal order does not match the immutable Domo binding")
      attempt <- beginDomoPaymentAttempt context Checkout.ProviderPayPal
        Checkout.OperationCapture merchantRef "capture"
      outcome <- ServiceStorefront.capturePaypalOrderRemoteForService
        sharedTlsManager clientId clientSecret baseUrl suppliedOrderId
        `catchError` failDomoPaymentAttempt context attempt
          Checkout.ProviderPayPal "paypal_capture_request"
      now <- liftIO getCurrentTime
      case ServiceStorefront.spcoStatus outcome of
        "COMPLETED" -> do
          case ServiceStorefront.validatePaypalSuccessfulCapture
              (domoReference context) (fromIntegral (dpcAmountMinor context))
              (dpcCurrency context) merchantRef outcome of
            Left validationMessage -> do
              let actualAmount = ServiceStorefront.spcoAmount outcome
                    >>= either (const Nothing) (Just . fromIntegral)
                      . ServiceStorefront.parseDatafastCents
              runDB $ do
                Checkout.recordReconciliationException Checkout.ProviderPayPal
                  paypalEnvironment merchantRef "provider_binding_mismatch"
                  (domoReference context) suppliedOrderId (dpcAmountMinor context)
                  actualAmount (dpcCurrency context) now
                Checkout.recordPaymentFailure (dpcCheckout context) attempt
                  Checkout.ProviderPayPal "provider_binding_mismatch"
                  (domoPaymentCorrelationId context Checkout.ProviderPayPal "capture") now
              throwError err502 { errBody = textBody validationMessage }
            Right () -> pure ()
          captureId <- maybe (throwError err502 { errBody = "PayPal capture ID is missing" }) pure
            (ServiceStorefront.spcoCaptureId outcome)
          bindDomoProviderResource context attempt Checkout.ProviderPayPal paypalEnvironment
            merchantRef "capture" captureId
            (Just ("/v2/checkout/orders/" <> suppliedOrderId <> "/capture"))
            Checkout.AttemptProcessing "capture"
          when (dpcHoldExpiresAt context <= now || dpcCheckoutStatus context == "expired") $ do
            runDB $ do
              Checkout.recordReconciliationException Checkout.ProviderPayPal
                paypalEnvironment merchantRef "payment_after_domo_hold_expiry"
                (domoReference context) captureId (dpcAmountMinor context)
                Nothing (dpcCurrency context) now
              void (rawSql "SELECT domo_quote_expire_holds(?, ?::uuid)"
                [PersistUTCTime now, PersistText (dpcQuoteId context)] :: SqlPersistT IO [Single Int])
            throwError (conflict
              "PayPal captured after this Domo hold expired; reconciliation is required and the date was not reserved")
          verified <- runDB $ Checkout.recordVerifiedPayment Checkout.VerifiedPayment
            { Checkout.vpAttempt = attempt
            , Checkout.vpCheckout = dpcCheckout context
            , Checkout.vpProvider = Checkout.ProviderPayPal
            , Checkout.vpEnvironment = paypalEnvironment
            , Checkout.vpMerchantRef = merchantRef
            , Checkout.vpResourceType = "capture"
            , Checkout.vpProviderResource = captureId
            , Checkout.vpProviderResourcePath = Just
                ("/v2/checkout/orders/" <> suppliedOrderId <> "/capture")
            , Checkout.vpOrderReference = domoReference context
            , Checkout.vpAmountMinor = dpcAmountMinor context
            , Checkout.vpCurrency = dpcCurrency context
            , Checkout.vpEvidence = "server_to_server"
            , Checkout.vpOccurredAt = now
            , Checkout.vpCorrelationId = domoPaymentCorrelationId
                context Checkout.ProviderPayPal "capture"
            }
          either (throwError . conflict) (const (pure ())) verified
          finalizeVerifiedDomoDeposit context
        "APPROVED" -> runDB $ Checkout.recordPaymentProcessing
          (dpcCheckout context) attempt Checkout.ProviderPayPal
          (domoPaymentCorrelationId context Checkout.ProviderPayPal "capture") now
        "PENDING" -> runDB $ Checkout.recordPaymentProcessing
          (dpcCheckout context) attempt Checkout.ProviderPayPal
          (domoPaymentCorrelationId context Checkout.ProviderPayPal "capture") now
        providerStatus -> runDB $ Checkout.recordPaymentFailure
          (dpcCheckout context) attempt Checkout.ProviderPayPal
          ("paypal_" <> T.toLower providerStatus)
          (domoPaymentCorrelationId context Checkout.ProviderPayPal "capture") now
      loadDomoQuoteDTO (dpcQuoteId context) Nothing

finalizeVerifiedDomoDeposit :: DomoPaymentContext -> AppM ()
finalizeVerifiedDomoDeposit context = do
  now <- liftIO getCurrentTime
  Env{ envPool } <- ask
  result <- liftIO $ runSqlPool (do
    rows <- (rawSql
      "SELECT runtime.quote_status, runtime.fulfillment_status, checkout.status\
      \ FROM domo_event_quote_runtime runtime\
      \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
      \ WHERE runtime.id = ?::uuid FOR UPDATE OF runtime, checkout"
      [PersistText (dpcQuoteId context)]
      :: SqlPersistT IO [(Single Text, Single Text, Single Text)])
    case rows of
      [(Single "deposit_paid", Single "date_reserved", Single "paid")] -> pure (Right ())
      [(Single "deposit_due", Single "date_held", Single "paid")] -> do
        rawExecute
          "UPDATE domo_event_quote_runtime SET quote_status = 'deposit_paid',\
          \ fulfillment_status = 'date_reserved', deposit_paid_at = ?, updated_at = ?\
          \ WHERE id = ?::uuid AND quote_status = 'deposit_due'"
          [PersistUTCTime now, PersistUTCTime now, PersistText (dpcQuoteId context)]
        rawExecute
          "UPDATE commerce_reservation_hold SET status = 'consumed'\
          \ WHERE checkout_id = ?::uuid AND status = 'active'"
          [PersistText (Checkout.checkoutReferenceId (dpcCheckout context))]
        rawExecute
          "INSERT INTO domo_quote_state_event(\
          \ domo_quote_id, from_status, to_status, actor_type, reason_code, notes\
          \) VALUES (?::uuid, 'deposit_due', 'deposit_paid', 'provider',\
          \ 'verified_deposit', 'Deposit verified independently; event fulfillment remains date_reserved')\
          \ ON CONFLICT DO NOTHING"
          [PersistText (dpcQuoteId context)]
        pure (Right ())
      [(Single _, Single _, Single checkoutStatus)]
        | checkoutStatus /= "paid" -> pure (Left (conflict
            "Domo date reservation requires a verified paid checkout"))
      _ -> pure (Left (conflict "Domo deposit cannot be finalized in its current state"))) envPool
  either throwError pure result
