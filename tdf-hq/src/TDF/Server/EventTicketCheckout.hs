{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.EventTicketCheckout
  ( publicEventTicketsServer
  , deriveTicketLookupToken
  ) where

import           Control.Exception
  ( SomeAsyncException, SomeException, fromException, throwIO, try )
import           Control.Monad (forM, forM_, unless, void, when)
import           Control.Monad.Except (catchError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Crypto.Hash (Digest, SHA256, hash)
import           Crypto.MAC.HMAC (HMAC, hmac)
import           Data.Aeson (encode, object, (.=))
import           Data.ByteArray (constEq)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isAlphaNum)
import           Data.Either (isRight)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, addUTCTime, getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql
  ( Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool
  , toSqlKey, updateWhereCount
  )
import           Database.PostgreSQL.Simple (SqlError(..))
import           Servant
import           System.Environment (lookupEnv)

import qualified TDF.API.Types as APITypes
import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.EventTickets as TicketDomain
import           TDF.DB (Env(..), sharedTlsManager)
import qualified TDF.Internationalization as Internationalization
import qualified TDF.Models.SocialEventsModels as SM
import qualified TDF.Routes.EventTickets as Routes
import qualified TDF.Server.ServiceStorefront as ServiceStorefront
import qualified TDF.Server.SocialEventsHandlers as SocialEvents

type AppM = ReaderT Env Handler

data ApprovedTicketPolicy = ApprovedTicketPolicy
  { atpId              :: Text
  , atpVersion         :: Text
  , atpCurrency        :: Text
  , atpBuyerFeeBps     :: Int
  , atpOrganizerFeeBps :: Int
  , atpTaxBps          :: Int
  , atpHoldMinutes     :: Int
  , atpTermsVersion    :: Text
  , atpRefundPolicy    :: Text
  , atpTransferAllowed :: Bool
  } deriving (Eq, Show)

data TicketRuntimeView = TicketRuntimeView
  { trvOrderId            :: Int64
  , trvEventId            :: Int64
  , trvCheckoutId         :: Text
  , trvPaymentStatus      :: Text
  , trvFulfillmentStatus  :: Text
  , trvHoldExpiresAt      :: UTCTime
  , trvIssuedAt           :: Maybe UTCTime
  , trvPolicyVersion      :: Text
  , trvCurrency           :: Text
  , trvQuantity           :: Int
  , trvUnitPriceMinor     :: Int64
  , trvGrossMinor         :: Int64
  , trvDiscountMinor      :: Int64
  , trvNetMinor           :: Int64
  , trvBuyerFeeMinor      :: Int64
  , trvOrganizerFeeMinor  :: Int64
  , trvTaxMinor           :: Int64
  , trvCheckoutTotalMinor :: Int64
  , trvOrganizerPayable   :: Int64
  , trvPlatformFeeMinor   :: Int64
  , trvTermsVersion       :: Text
  } deriving (Eq, Show)

data TicketPaymentContext = TicketPaymentContext
  { tpcOrderKey             :: SM.EventTicketOrderId
  , tpcEventKey             :: SM.SocialEventId
  , tpcCheckout             :: Checkout.CheckoutReference
  , tpcCreateIdempotencyKey :: Text
  , tpcCheckoutStatus       :: Text
  , tpcEnvironment          :: Checkout.CheckoutEnvironment
  , tpcAmountMinor          :: Int64
  , tpcCurrency             :: Text
  , tpcHoldExpiresAt        :: UTCTime
  , tpcBuyerName            :: Text
  , tpcBuyerEmail           :: Text
  , tpcBuyerPhone           :: Maybe Text
  } deriving (Eq, Show)

publicEventTicketsServer :: ServerT Routes.PublicEventTicketsAPI AppM
publicEventTicketsServer =
       getPublicEventTicketStorefront
  :<|> createPublicEventTicketCheckout
  :<|> getPublicEventTicketCheckout
  :<|> createPublicEventTicketDatafastCheckout
  :<|> confirmPublicEventTicketDatafastStatus
  :<|> createPublicEventTicketPaypalOrder
  :<|> capturePublicEventTicketPaypalOrder

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
notFound = err404 { errBody = "Event ticket order not found" }

sha256Text :: Text -> Text
sha256Text value = TE.decodeUtf8 $
  BAE.convertToBase BAE.Base16
    (hash (TE.encodeUtf8 value) :: Digest SHA256)

hmacSha256Text :: BS.ByteString -> Text -> Text
hmacSha256Text secret value = TE.decodeUtf8 $
  BAE.convertToBase BAE.Base16
    (hmac secret (TE.encodeUtf8 value) :: HMAC SHA256)

loadTicketLookupTokenSecret :: AppM BS.ByteString
loadTicketLookupTokenSecret = do
  configured <- liftIO (lookupEnv "COMMERCE_LOOKUP_TOKEN_SECRET")
  case TE.encodeUtf8 . T.pack <$> configured of
    Just secret | BS.length secret >= 32 -> pure secret
    _ -> throwError err503
      { errBody = "Secure guest ticket lookup is not configured in this environment" }

deriveTicketLookupToken :: BS.ByteString -> Text -> Int64 -> Either Text Text
deriveTicketLookupToken secret idempotencyKey eventId
  | BS.length secret < 32 = Left "Ticket lookup secret must contain at least 32 bytes"
  | otherwise = Right $ hmacSha256Text secret
      ("event-ticket-order-lookup:" <> idempotencyKey <> ":" <> T.pack (show eventId))

validatePositiveKey :: Text -> Int64 -> Either ServerError Int64
validatePositiveKey fieldName value
  | value <= 0 = Left (badRequest (fieldName <> " must be positive"))
  | otherwise = Right value

normalizePhone :: Maybe Text -> Either ServerError (Maybe Text)
normalizePhone raw = case T.strip <$> raw of
  Nothing -> Right Nothing
  Just "" -> Right Nothing
  Just clean
    | T.length clean > 24 -> Left (badRequest "buyerPhone is too long")
    | T.any (\char -> not (isAlphaNum char || char `elem` ['+','-',' ','(',')'])) clean ->
        Left (badRequest "buyerPhone contains unsupported characters")
    | otherwise -> Right (Just clean)

normalizePromoCode :: Maybe Text -> Either ServerError (Maybe Text)
normalizePromoCode raw = case T.toUpper . T.strip <$> raw of
  Nothing -> Right Nothing
  Just "" -> Right Nothing
  Just clean
    | T.length clean > 50 -> Left (badRequest "promoCode is too long")
    | T.any (\char -> not (isAlphaNum char || char == '-')) clean ->
        Left (badRequest "promoCode contains unsupported characters")
    | otherwise -> Right (Just clean)

loadCheckoutEnvironment :: AppM Checkout.CheckoutEnvironment
loadCheckoutEnvironment = do
  configured <- liftIO (lookupEnv "COMMERCE_CHECKOUT_ENV")
  either (throwError . internal) pure $
    Checkout.resolveCheckoutEnvironment configured

loadApprovedTicketPolicy
  :: UTCTime
  -> SM.SocialEventId
  -> SqlPersistT IO (Maybe ApprovedTicketPolicy)
loadApprovedTicketPolicy now eventKey = do
  rows <- (rawSql
    "SELECT id::text, policy_version, currency, buyer_fee_bps,\
    \ organizer_fee_bps, tax_bps, hold_minutes, terms_version,\
    \ refund_policy, transfer_allowed\
    \ FROM event_ticket_checkout_policy\
    \ WHERE event_id = ? AND active AND approval_status = 'approved'\
    \ AND approved_at IS NOT NULL AND approved_by IS NOT NULL\
    \ AND (effective_from IS NULL OR effective_from <= ?)\
    \ AND (effective_until IS NULL OR effective_until > ?)"
    [toPersistValue eventKey, PersistUTCTime now, PersistUTCTime now]
    :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Int, Single Int
       , Single Int, Single Int, Single Text, Single Text, Single Bool
       )])
  pure $ case rows of
    [( Single atpId, Single atpVersion, Single atpCurrency
     , Single atpBuyerFeeBps, Single atpOrganizerFeeBps, Single atpTaxBps
     , Single atpHoldMinutes, Single atpTermsVersion, Single atpRefundPolicy
     , Single atpTransferAllowed
     )] -> Just ApprovedTicketPolicy{..}
    _ -> Nothing

requirePublicEvent :: SM.SocialEventId -> AppM SM.SocialEvent
requirePublicEvent eventKey = do
  eventRow <- runDB (get eventKey) >>= maybe (throwError err404) pure
  purchaseEnabled <- runDB (SocialEvents.eventTicketPurchaseEnabledFor eventRow)
  case SocialEvents.validateTicketPurchaseEventEligibility
      (SM.socialEventMetadata eventRow) purchaseEnabled of
    Left _ -> throwError err404
    Right () -> pure eventRow

getPublicEventTicketStorefront :: Int64 -> AppM Routes.PublicEventTicketStorefrontDTO
getPublicEventTicketStorefront rawEventId = do
  eventId <- either throwError pure (validatePositiveKey "eventId" rawEventId)
  let eventKey = toSqlKey eventId
  eventRow <- requirePublicEvent eventKey
  now <- liftIO getCurrentTime
  policy <- runDB (loadApprovedTicketPolicy now eventKey)
  checkoutEnvironment <- loadCheckoutEnvironment
  domainEnabled <- runDB $
    Checkout.domainEnabledForEnvironment checkoutEnvironment "event_tickets"
  tierEntities <- runDB $
    selectList
      [ SM.EventTicketTierEventId ==. eventKey
      ]
      [Asc SM.EventTicketTierPosition, Asc SM.EventTicketTierId]
  let totalSold = sum (map (SM.eventTicketTierQuantitySold . entityVal) tierEntities)
      eventRemaining = (\capacity -> max 0 (capacity - totalSold))
        <$> SM.socialEventCapacity eventRow
      availableTiers = filter
        (\entity -> SM.eventTicketTierIsActive (entityVal entity)
          && SM.eventTicketTierPriceCents (entityVal entity) > 0
          && SocialEvents.isTicketTierSaleOpen now (entityVal entity))
        tierEntities
  publicTiers <- forM availableTiers $ \(Entity tierKey tier) -> pure
    Routes.PublicEventTicketTierDTO
      { Routes.tierId = fromSqlKey tierKey
      , Routes.code = SM.eventTicketTierCode tier
      , Routes.name = SM.eventTicketTierName tier
      , Routes.description = SM.eventTicketTierDescription tier
      , Routes.unitPriceMinor = fromIntegral (SM.eventTicketTierPriceCents tier)
      , Routes.currency = T.toUpper (SM.eventTicketTierCurrency tier)
      , Routes.remaining =
          let tierRemaining = max 0
                (SM.eventTicketTierQuantityTotal tier - SM.eventTicketTierQuantitySold tier)
          in maybe tierRemaining (min tierRemaining) eventRemaining
      , Routes.salesStart = SM.eventTicketTierSalesStart tier
      , Routes.salesEnd = SM.eventTicketTierSalesEnd tier
      , Routes.transfersAllowed = SM.eventTicketTierAllowTransfers tier
      }
  (venueName, venueAddress) <- case SM.socialEventVenueId eventRow of
    Nothing -> pure (Nothing, Nothing)
    Just venueKey -> do
      venue <- runDB (get venueKey)
      pure (SM.venueName <$> venue, venue >>= SM.venueAddress)
  let hasInventory = any ((> 0) . Routes.remaining) publicTiers
      available = domainEnabled && isJust policy && hasInventory
      reason
        | not domainEnabled = Just "Public ticket checkout is disabled in this environment"
        | not (isJust policy) = Just "This event has no approved active ticket price and fee policy"
        | null publicTiers = Just "No ticket tiers are currently on sale"
        | not hasInventory = Just "Ticket inventory is currently exhausted"
        | otherwise = Nothing
  pure Routes.PublicEventTicketStorefrontDTO
    { Routes.eventId = eventId
    , Routes.title = SM.socialEventTitle eventRow
    , Routes.description = SM.socialEventDescription eventRow
    , Routes.startsAt = SM.socialEventStartTime eventRow
    , Routes.endsAt = SM.socialEventEndTime eventRow
    , Routes.timezone = SM.socialEventTimezone eventRow
    , Routes.venueName = venueName
    , Routes.venueAddress = venueAddress
    , Routes.tiers = publicTiers
    , Routes.checkoutAvailable = available
    , Routes.unavailableReason = reason
    }

data ValidPromo = ValidPromo
  { vpEntity :: Entity SM.PromoCode
  , vpDiscountMinor :: Int64
  }

promoSnapshotUnchanged :: SM.PromoCode -> SM.PromoCode -> Bool
promoSnapshotUnchanged expected actual =
     SM.promoCodeEventId actual == SM.promoCodeEventId expected
  && SM.promoCodeDiscountType actual == SM.promoCodeDiscountType expected
  && SM.promoCodeDiscountValue actual == SM.promoCodeDiscountValue expected
  && T.toUpper (SM.promoCodeCurrency actual) == T.toUpper (SM.promoCodeCurrency expected)
  && SM.promoCodeMaxRedemptions actual == SM.promoCodeMaxRedemptions expected
  && SM.promoCodeValidFrom actual == SM.promoCodeValidFrom expected
  && SM.promoCodeValidUntil actual == SM.promoCodeValidUntil expected
  && SM.promoCodeTierIds actual == SM.promoCodeTierIds expected
  && SM.promoCodeMinPurchaseAmountCents actual
      == SM.promoCodeMinPurchaseAmountCents expected
  && SM.promoCodeIsActive actual == SM.promoCodeIsActive expected

loadValidPromo
  :: UTCTime
  -> SM.SocialEventId
  -> SM.EventTicketTierId
  -> Int64
  -> Text
  -> AppM ValidPromo
loadValidPromo now eventKey tierKey baseMinor code = do
  promoEntity@(Entity _ promo) <- runDB (getBy (SM.UniquePromoCode code))
    >>= maybe (throwError (badRequest "Promo code is invalid")) pure
  unless (SM.promoCodeEventId promo == Just eventKey && SM.promoCodeIsActive promo) $
    throwError (badRequest "Promo code is invalid")
  either (throwError . badRequest) pure $
    SocialEvents.validatePromoCodeDateWindow now
      (SM.promoCodeValidFrom promo) (SM.promoCodeValidUntil promo)
  either (throwError . badRequest) pure $
    SocialEvents.validatePromoCodeRedemptionLimit
      (SM.promoCodeCurrentRedemptions promo) (SM.promoCodeMaxRedemptions promo)
  either (throwError . badRequest) pure $
    SocialEvents.validatePromoCodeTierEligibility
      (SM.promoCodeTierIds promo) (Just (T.pack (show (fromSqlKey tierKey))))
  when (baseMinor > fromIntegral (maxBound :: Int)) $
    throwError (conflict "Ticket face value exceeds supported promotion range")
  either (throwError . badRequest) pure $
    SocialEvents.validatePromoCodeMinimumPurchaseCents
      (SM.promoCodeMinPurchaseAmountCents promo) (fromIntegral baseMinor)
  discount <- either (throwError . badRequest) pure $
    SocialEvents.promoCodeDiscountAmountEither
      (fromIntegral baseMinor)
      (SM.promoCodeDiscountType promo)
      (SM.promoCodeDiscountValue promo)
  pure ValidPromo
    { vpEntity = promoEntity
    , vpDiscountMinor = fromIntegral discount
    }

createPublicEventTicketCheckout
  :: Int64
  -> Maybe Text
  -> Routes.PublicEventTicketCheckoutRequest
  -> AppM Routes.PublicEventTicketCheckoutResponse
createPublicEventTicketCheckout rawEventId mIdempotency
    request@Routes.PublicEventTicketCheckoutRequest
      { Routes.tierId = requestedTierId
      , Routes.quantity = requestedQuantity
      } = do
  eventId <- either throwError pure (validatePositiveKey "eventId" rawEventId)
  tierId <- either throwError pure (validatePositiveKey "tierId" requestedTierId)
  unless (Routes.termsAccepted request) $
    throwError (badRequest "Ticket terms must be accepted before seats can be held")
  idempotencyKey <- either (throwError . badRequest) pure $
    ServiceStorefront.validateIdempotencyKey mIdempotency
  validatedBuyerName <- either throwError pure $
    SocialEvents.validateTicketPurchaseBuyerName (Just (Routes.buyerName request))
  buyerName <- maybe (throwError (badRequest "buyerName is required")) pure
    validatedBuyerName
  validatedBuyerEmail <- either throwError pure $
    SocialEvents.validateTicketPurchaseBuyerEmail (Just (Routes.buyerEmail request))
  buyerEmail <- maybe (throwError (badRequest "buyerEmail is required")) pure
    validatedBuyerEmail
  buyerPhone <- either throwError pure (normalizePhone (Routes.buyerPhone request))
  promoCode <- either throwError pure (normalizePromoCode (Routes.promoCode request))
  let eventKey = toSqlKey eventId
      tierKey = toSqlKey tierId
  _ <- requirePublicEvent eventKey
  now <- liftIO getCurrentTime
  tier <- runDB (get tierKey) >>= maybe (throwError err404) pure
  unless (SM.eventTicketTierEventId tier == eventKey
      && SM.eventTicketTierIsActive tier
      && SocialEvents.isTicketTierSaleOpen now tier) $
    throwError err404
  policy <- runDB (loadApprovedTicketPolicy now eventKey)
    >>= maybe (throwError (conflict
      "This event has no approved active ticket price and fee policy")) pure
  unless (T.toUpper (SM.eventTicketTierCurrency tier) == atpCurrency policy) $
    throwError (conflict "Ticket tier currency does not match the approved event policy")
  checkoutEnvironment <- loadCheckoutEnvironment
  domainEnabled <- runDB $
    Checkout.domainEnabledForEnvironment checkoutEnvironment "event_tickets"
  unless domainEnabled $
    throwError err503 { errBody = "Public ticket checkout is disabled in this environment" }
  let baseMinor = toInteger (SM.eventTicketTierPriceCents tier)
        * toInteger requestedQuantity
  when (baseMinor > toInteger (maxBound :: Int64)) $
    throwError (badRequest "Ticket quantity or price is too large")
  validPromo <- traverse
    (loadValidPromo now eventKey tierKey (fromInteger baseMinor)) promoCode
  forM_ validPromo $ \(ValidPromo (Entity _ promo) _) ->
    when (SM.promoCodeDiscountType promo `elem` ["fixed_amount", "fixed"]
        && T.toUpper (SM.promoCodeCurrency promo) /= atpCurrency policy) $
      throwError (badRequest "Fixed promotion currency does not match the approved event policy")
  let discountMinor = maybe 0 vpDiscountMinor validPromo
  price <- either (throwError . badRequest) pure $
    TicketDomain.calculateTicketPrice
      (fromIntegral (SM.eventTicketTierPriceCents tier))
      requestedQuantity
      discountMinor
      (atpBuyerFeeBps policy)
      (atpOrganizerFeeBps policy)
      (atpTaxBps policy)
  when (TicketDomain.tpbCheckoutTotalMinor price <= 0) $
    throwError (badRequest
      "A fully discounted ticket requires an explicit no-payment entitlement workflow")
  lookupSecret <- loadTicketLookupTokenSecret
  lookupToken <- either (throwError . internal) pure $
    deriveTicketLookupToken lookupSecret idempotencyKey eventId
  let requestHash = sha256Text . TE.decodeUtf8 . BL.toStrict . encode $ object
        [ "event_id" .= eventId
        , "tier_id" .= tierId
        , "quantity" .= requestedQuantity
        , "buyer_name" .= buyerName
        , "buyer_email" .= buyerEmail
        , "buyer_phone" .= buyerPhone
        , "promo_code" .= promoCode
        , "policy_id" .= atpId policy
        , "policy_version" .= atpVersion policy
        , "terms_version" .= atpTermsVersion policy
        ]
      lookupHash = sha256Text lookupToken
      holdExpiresAt = addUTCTime (fromIntegral (atpHoldMinutes policy) * 60) now
  existing <- lookupTicketCheckoutIdempotency idempotencyKey
  case existing of
    Just (orderKey, storedHash)
      | storedHash == requestHash -> loadTicketCheckoutDTO orderKey (Just lookupToken)
      | otherwise -> throwError (conflict
          "Idempotency key was already used for a different ticket checkout")
    Nothing -> do
      consumePublicTicketCheckoutRateLimit lookupSecret eventId buyerEmail
      orderKey <- createTicketCheckoutTransaction
        checkoutEnvironment now holdExpiresAt idempotencyKey requestHash lookupHash
        buyerName buyerEmail buyerPhone eventKey tierKey tier
        requestedQuantity policy validPromo price
      loadTicketCheckoutDTO orderKey (Just lookupToken)

lookupTicketCheckoutIdempotency
  :: Text
  -> AppM (Maybe (SM.EventTicketOrderId, Text))
lookupTicketCheckoutIdempotency idempotencyKey = do
  rows <- runDB (rawSql
    "SELECT order_id, create_request_sha256 FROM event_ticket_checkout_runtime\
    \ WHERE create_idempotency_key = ?"
    [PersistText idempotencyKey]
    :: SqlPersistT IO [(Single Int64, Single Text)])
  pure $ case rows of
    [(Single orderId, Single requestHash)] -> Just (toSqlKey orderId, requestHash)
    _ -> Nothing

consumePublicTicketCheckoutRateLimit :: BS.ByteString -> Int64 -> Text -> AppM ()
consumePublicTicketCheckoutRateLimit secret eventId buyerEmail = do
  let scope = "event-ticket-create:" <> T.pack (show eventId)
      subjectHash = hmacSha256Text secret
        ("event-ticket-rate-limit:" <> T.toLower (T.strip buyerEmail))
  rows <- runDB (rawSql
    "INSERT INTO event_ticket_checkout_rate_limit(\
    \ scope, subject_hash, window_started_at, request_count, updated_at\
    \) VALUES (?, ?, date_trunc('hour', NOW()), 1, NOW())\
    \ ON CONFLICT(scope, subject_hash, window_started_at) DO UPDATE\
    \ SET request_count = event_ticket_checkout_rate_limit.request_count + 1,\
    \ updated_at = NOW() RETURNING request_count"
    [PersistText scope, PersistText subjectHash] :: SqlPersistT IO [Single Int])
  case rows of
    [Single requestCount] | requestCount <= 10 -> pure ()
    _ -> throwError err429
      { errBody = "Ticket checkout rate limit exceeded; try again after the current hour" }

createTicketCheckoutTransaction
  :: Checkout.CheckoutEnvironment
  -> UTCTime
  -> UTCTime
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> SM.SocialEventId
  -> SM.EventTicketTierId
  -> SM.EventTicketTier
  -> Int
  -> ApprovedTicketPolicy
  -> Maybe ValidPromo
  -> TicketDomain.TicketPriceBreakdown
  -> AppM SM.EventTicketOrderId
createTicketCheckoutTransaction
    checkoutEnvironment now holdExpiresAt idempotencyKey requestHash lookupHash
    buyerName buyerEmail buyerPhone eventKey tierKey tier quantityInt
    policy validPromo price = do
  Env{ envPool } <- ask
  result <- liftIO $
    (try (runSqlPool transactionBody envPool)
      :: IO (Either SomeException (Either ServerError SM.EventTicketOrderId)))
  case result of
    Right (Right orderKey) -> pure orderKey
    Right (Left serverError) -> throwError serverError
    Left exception -> case fromException exception :: Maybe SomeAsyncException of
      Just _ -> liftIO (throwIO exception)
      Nothing -> case fromException exception :: Maybe SqlError of
        Just sqlError | sqlState sqlError == "23505" ->
          throwError (conflict "Ticket checkout conflicts with an existing request")
        _ -> liftIO (throwIO exception)
  where
    transactionBody = do
      _ <- (rawSql
        "SELECT 1::bigint FROM (SELECT pg_advisory_xact_lock(hashtextextended(?, 0))) locked"
        [PersistText ("event-ticket-checkout:" <> idempotencyKey)]
        :: SqlPersistT IO [Single Int64])
      existing <- (rawSql
        "SELECT order_id, create_request_sha256 FROM event_ticket_checkout_runtime\
        \ WHERE create_idempotency_key = ?"
        [PersistText idempotencyKey]
        :: SqlPersistT IO [(Single Int64, Single Text)])
      case existing of
        [(Single orderId, Single storedHash)]
          | storedHash == requestHash -> pure (Right (toSqlKey orderId))
          | otherwise -> pure (Left (conflict
              "Idempotency key was already used for a different ticket checkout"))
        [] -> createNew
        _ -> pure (Left (internal "Ticket checkout idempotency lookup was ambiguous"))
    createNew = do
      lockedEvents <- (rawSql "SELECT ?? FROM social_event WHERE id = ? FOR UPDATE"
        [toPersistValue eventKey] :: SqlPersistT IO [Entity SM.SocialEvent])
      -- Expire this event's old holds while the event lock is authoritative,
      -- before taking tier or promotion locks. This keeps checkout -> runtime
      -- -> tier trigger locking consistent with concurrent status polling.
      _ <- (rawSql "SELECT event_ticket_checkout_expire_holds(?, NULL, ?)"
        [PersistUTCTime now, toPersistValue eventKey] :: SqlPersistT IO [Single Int])
      lockedTiers <- (rawSql "SELECT ?? FROM event_ticket_tier WHERE id = ? FOR UPDATE"
        [toPersistValue tierKey] :: SqlPersistT IO [Entity SM.EventTicketTier])
      _ <- (rawSql
        "SELECT id::text FROM event_ticket_checkout_policy WHERE id = ?::uuid FOR SHARE"
        [PersistText (atpId policy)] :: SqlPersistT IO [Single Text])
      lockedPromo <- case validPromo of
        Nothing -> pure (Right Nothing)
        Just expected@(ValidPromo (Entity promoKey expectedPromo) _) -> do
          rows <- (rawSql "SELECT ?? FROM promo_code WHERE id = ? FOR UPDATE"
            [toPersistValue promoKey] :: SqlPersistT IO [Entity SM.PromoCode])
          pure $ case rows of
            [Entity _ currentPromo]
              | promoSnapshotUnchanged expectedPromo currentPromo
              , isRight (SocialEvents.validatePromoCodeRedemptionLimit
                  (SM.promoCodeCurrentRedemptions currentPromo)
                  (SM.promoCodeMaxRedemptions currentPromo)) ->
                  Right (Just expected { vpEntity = Entity promoKey currentPromo })
              | otherwise -> Left (conflict
                  "Promotion changed or became unavailable while ticket inventory was being held")
            _ -> Left (conflict "Promotion is no longer available")
      case (lockedEvents, lockedTiers, lockedPromo) of
        ([_], [Entity _ lockedTier], Right transactionPromo)
          | SM.eventTicketTierEventId lockedTier == eventKey
          , SM.eventTicketTierIsActive lockedTier
          , SocialEvents.isTicketTierSaleOpen now lockedTier ->
              reserveAndCreate lockedTier transactionPromo
        (_, _, Left promoError) -> pure (Left promoError)
        _ -> pure (Left (conflict "Ticket tier is no longer available"))
    reserveAndCreate lockedTier transactionPromo = do
      eventCapacityRows <- (rawSql
        "SELECT event.capacity, COALESCE(sum(tier.quantity_sold), 0)::bigint\
        \ FROM social_event event\
        \ LEFT JOIN event_ticket_tier tier ON tier.event_id = event.id\
        \ WHERE event.id = ? GROUP BY event.capacity"
        [toPersistValue eventKey]
        :: SqlPersistT IO [(Single (Maybe Int), Single Int64)])
      let capacityAvailable = case eventCapacityRows of
            [(Single Nothing, _)] -> True
            [(Single (Just capacity), Single sold)] ->
              sold + fromIntegral quantityInt <= fromIntegral capacity
            _ -> False
      if not capacityAvailable
        then pure (Left (conflict "Event capacity is exhausted"))
        else do
          reserved <- updateWhereCount
            [ SM.EventTicketTierId ==. tierKey
            , SM.EventTicketTierIsActive ==. True
            , SM.EventTicketTierQuantitySold
                <=. SM.eventTicketTierQuantityTotal lockedTier - quantityInt
            ]
            [ SM.EventTicketTierQuantitySold +=. quantityInt
            , SM.EventTicketTierUpdatedAt =. now
            ]
          if reserved == 0
            then pure (Left (conflict "Ticket tier inventory is exhausted"))
            else claimPromoAndCreate transactionPromo
    claimPromoAndCreate transactionPromo = case transactionPromo of
      Nothing -> createOrder Nothing
      Just (ValidPromo (Entity promoKey promo) _) -> do
        let filters =
              [ SM.PromoCodeId ==. promoKey
              , SM.PromoCodeIsActive ==. True
              ] <> maybe [] (\limit -> [SM.PromoCodeCurrentRedemptions <. limit])
                (SM.promoCodeMaxRedemptions promo)
        claimed <- updateWhereCount filters [SM.PromoCodeCurrentRedemptions +=. 1]
        if claimed == 0
          then do
            update tierKey
              [ SM.EventTicketTierQuantitySold -=. quantityInt
              , SM.EventTicketTierUpdatedAt =. now
              ]
            pure (Left (conflict "Promo code is no longer available"))
          else createOrder (Just promoKey)
    createOrder promoKey = do
      let orderRecord = SM.EventTicketOrder
            { SM.eventTicketOrderEventId = eventKey
            , SM.eventTicketOrderTierId = tierKey
            , SM.eventTicketOrderBuyerPartyId = Nothing
            , SM.eventTicketOrderBuyerName = Just buyerName
            , SM.eventTicketOrderBuyerEmail = Just buyerEmail
            , SM.eventTicketOrderQuantity = quantityInt
            , SM.eventTicketOrderAmountCents = fromIntegral
                (TicketDomain.tpbCheckoutTotalMinor price)
            , SM.eventTicketOrderCurrency = atpCurrency policy
            , SM.eventTicketOrderStatus = "pending"
            , SM.eventTicketOrderMetadata = Just . TE.decodeUtf8 . BL.toStrict . encode $ object
                [ "runtime" .= ("canonical_public_checkout" :: Text)
                , "policy_version" .= atpVersion policy
                , "buyer_phone" .= buyerPhone
                , "buyer_fee_minor" .= TicketDomain.tpbBuyerFeeMinor price
                , "organizer_fee_minor" .= TicketDomain.tpbOrganizerFeeMinor price
                ]
            , SM.eventTicketOrderCheckoutIdempotencyKey = Nothing
            , SM.eventTicketOrderPurchasedAt = now
            , SM.eventTicketOrderStripePaymentIntentId = Nothing
            , SM.eventTicketOrderPromoCodeId = promoKey
            , SM.eventTicketOrderOriginalAmountCents = Just (fromIntegral
                (TicketDomain.tpbGrossFaceValueMinor price))
            , SM.eventTicketOrderPaymentMethod = Nothing
            , SM.eventTicketOrderCreatedAt = now
            , SM.eventTicketOrderUpdatedAt = now
            }
      orderKey <- insert orderRecord
      let orderIdText = T.pack (show (fromSqlKey orderKey))
          snapshot = object
            [ "domain" .= ("event_ticket_order" :: Text)
            , "order_id" .= fromSqlKey orderKey
            , "event_id" .= fromSqlKey eventKey
            , "tier_id" .= fromSqlKey tierKey
            , "tier_name" .= SM.eventTicketTierName tier
            , "quantity" .= quantityInt
            , "policy_id" .= atpId policy
            , "policy_version" .= atpVersion policy
            , "unit_price_minor" .=
                (fromIntegral (SM.eventTicketTierPriceCents tier) :: Int64)
            , "gross_face_value_minor" .= TicketDomain.tpbGrossFaceValueMinor price
            , "discount_minor" .= TicketDomain.tpbDiscountMinor price
            , "buyer_fee_minor" .= TicketDomain.tpbBuyerFeeMinor price
            , "organizer_fee_minor" .= TicketDomain.tpbOrganizerFeeMinor price
            , "tax_minor" .= TicketDomain.tpbTaxMinor price
            , "checkout_total_minor" .= TicketDomain.tpbCheckoutTotalMinor price
            , "organizer_payable_minor" .= TicketDomain.tpbOrganizerPayableMinor price
            , "terms_version" .= atpTermsVersion policy
            , "refund_policy" .= atpRefundPolicy policy
            , "transfer_allowed" .= atpTransferAllowed policy
            ]
      checkout <- Checkout.createCheckout Checkout.CheckoutCreation
        { Checkout.ccDomainType = "event_ticket_order"
        , Checkout.ccDomainOrderId = orderIdText
        , Checkout.ccEnvironment = checkoutEnvironment
        , Checkout.ccCurrency = atpCurrency policy
        , Checkout.ccAmountMinor = TicketDomain.tpbCheckoutTotalMinor price
        , Checkout.ccCustomerEmail = buyerEmail
        , Checkout.ccLookupTokenHash = lookupHash
        , Checkout.ccIdempotencyKey = idempotencyKey
        , Checkout.ccExpiresAt = holdExpiresAt
        , Checkout.ccProductType = "event_ticket_tier"
        , Checkout.ccProductId = T.pack (show (fromSqlKey tierKey))
        , Checkout.ccProductVersion = atpVersion policy
        , Checkout.ccDescription = SM.eventTicketTierName tier
        , Checkout.ccSnapshot = snapshot
        , Checkout.ccCorrelationId = "event-ticket-create:" <> orderIdText
        }
      rawExecute
        "INSERT INTO event_ticket_checkout_runtime(\
        \ order_id, event_id, tier_id, checkout_id, policy_id, policy_version,\
        \ lookup_token_hash, create_idempotency_key, create_request_sha256,\
        \ quantity, currency, unit_price_minor, gross_face_value_minor,\
        \ discount_minor, net_face_value_minor, buyer_fee_bps, buyer_fee_minor,\
        \ organizer_fee_bps, organizer_fee_minor, tax_bps, tax_minor,\
        \ checkout_total_minor, organizer_payable_minor, platform_fee_minor,\
        \ promo_code_id, terms_version, terms_accepted_at, hold_expires_at\
        \) VALUES (?, ?, ?, ?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,\
        \ ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        [ toPersistValue orderKey, toPersistValue eventKey, toPersistValue tierKey
        , PersistText (Checkout.checkoutReferenceId checkout), PersistText (atpId policy)
        , PersistText (atpVersion policy), PersistText lookupHash
        , PersistText idempotencyKey, PersistText requestHash, PersistInt64 (fromIntegral quantityInt)
        , PersistText (atpCurrency policy), PersistInt64 (fromIntegral (SM.eventTicketTierPriceCents tier))
        , PersistInt64 (TicketDomain.tpbGrossFaceValueMinor price)
        , PersistInt64 (TicketDomain.tpbDiscountMinor price)
        , PersistInt64 (TicketDomain.tpbNetFaceValueMinor price)
        , PersistInt64 (fromIntegral (atpBuyerFeeBps policy))
        , PersistInt64 (TicketDomain.tpbBuyerFeeMinor price)
        , PersistInt64 (fromIntegral (atpOrganizerFeeBps policy))
        , PersistInt64 (TicketDomain.tpbOrganizerFeeMinor price)
        , PersistInt64 (fromIntegral (atpTaxBps policy))
        , PersistInt64 (TicketDomain.tpbTaxMinor price)
        , PersistInt64 (TicketDomain.tpbCheckoutTotalMinor price)
        , PersistInt64 (TicketDomain.tpbOrganizerPayableMinor price)
        , PersistInt64 (TicketDomain.tpbPlatformFeeMinor price)
        , maybe PersistNull toPersistValue promoKey, PersistText (atpTermsVersion policy)
        , PersistUTCTime now, PersistUTCTime holdExpiresAt
        ]
      rawExecute
        "INSERT INTO event_ticket_fulfillment_event(\
        \ order_id, from_status, to_status, actor_type, reason_code, notes\
        \) VALUES (?, NULL, 'seat_held', 'system', 'checkout_created',\
        \ 'Atomic expiring seat hold created; payment and issuance remain separate')"
        [toPersistValue orderKey]
      pure (Right orderKey)

loadTicketRuntimeView
  :: SM.EventTicketOrderId
  -> SqlPersistT IO (Maybe TicketRuntimeView)
loadTicketRuntimeView orderKey = do
  rows <- (rawSql
    "SELECT runtime.order_id, runtime.event_id, runtime.checkout_id::text,\
    \ checkout.status, runtime.fulfillment_status, runtime.hold_expires_at, runtime.issued_at,\
    \ runtime.policy_version, runtime.currency, runtime.quantity,\
    \ runtime.unit_price_minor, runtime.gross_face_value_minor,\
    \ runtime.discount_minor, runtime.net_face_value_minor, runtime.buyer_fee_minor,\
    \ runtime.organizer_fee_minor, runtime.tax_minor, runtime.checkout_total_minor,\
    \ runtime.organizer_payable_minor, runtime.platform_fee_minor, runtime.terms_version\
    \ FROM event_ticket_checkout_runtime runtime\
    \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
    \ WHERE runtime.order_id = ? AND checkout.domain_type = 'event_ticket_order'\
    \ AND checkout.domain_order_id = runtime.order_id::text\
    \ AND checkout.total_minor = runtime.checkout_total_minor\
    \ AND checkout.currency = runtime.currency"
    [toPersistValue orderKey]
    :: SqlPersistT IO
      [( Single Int64, Single Int64, Single Text, Single Text, Single Text
       , Single UTCTime, Single (Maybe UTCTime), Single Text, Single Text, Single Int, Single Int64
       , Single Int64, Single Int64, Single Int64, Single Int64, Single Int64
       , Single Int64, Single Int64, Single Int64, Single Int64, Single Text
       )])
  pure $ case rows of
    [( Single trvOrderId, Single trvEventId, Single trvCheckoutId
     , Single trvPaymentStatus, Single trvFulfillmentStatus
     , Single trvHoldExpiresAt, Single trvIssuedAt, Single trvPolicyVersion, Single trvCurrency
     , Single trvQuantity, Single trvUnitPriceMinor, Single trvGrossMinor
     , Single trvDiscountMinor, Single trvNetMinor, Single trvBuyerFeeMinor
     , Single trvOrganizerFeeMinor, Single trvTaxMinor
     , Single trvCheckoutTotalMinor, Single trvOrganizerPayable
     , Single trvPlatformFeeMinor, Single trvTermsVersion
     )] -> Just TicketRuntimeView{..}
    _ -> Nothing

loadTicketCheckoutDTO
  :: SM.EventTicketOrderId
  -> Maybe Text
  -> AppM Routes.PublicEventTicketCheckoutResponse
loadTicketCheckoutDTO orderKey lookupToken = do
  runtime <- runDB (loadTicketRuntimeView orderKey)
    >>= maybe (throwError notFound) pure
  paymentMethods <- loadPublicTicketPaymentMethods runtime
  ticketEntities <- if isJust (trvIssuedAt runtime)
    then runDB $ selectList [SM.EventTicketOrderRefId ==. orderKey] [Asc SM.EventTicketId]
    else pure []
  let publicTickets = map toPublicTicket ticketEntities
  pure Routes.PublicEventTicketCheckoutResponse
    { Routes.orderId = trvOrderId runtime
    , Routes.eventId = trvEventId runtime
    , Routes.checkoutId = trvCheckoutId runtime
    , Routes.lookupToken = lookupToken
    , Routes.paymentStatus = trvPaymentStatus runtime
    , Routes.fulfillmentStatus = trvFulfillmentStatus runtime
    , Routes.holdExpiresAt = trvHoldExpiresAt runtime
    , Routes.quote = Routes.PublicEventTicketQuoteDTO
        { Routes.policyVersion = trvPolicyVersion runtime
        , Routes.currency = trvCurrency runtime
        , Routes.quantity = trvQuantity runtime
        , Routes.unitPriceMinor = trvUnitPriceMinor runtime
        , Routes.grossFaceValueMinor = trvGrossMinor runtime
        , Routes.discountMinor = trvDiscountMinor runtime
        , Routes.netFaceValueMinor = trvNetMinor runtime
        , Routes.buyerPlatformFeeMinor = trvBuyerFeeMinor runtime
        , Routes.organizerPlatformFeeMinor = trvOrganizerFeeMinor runtime
        , Routes.taxMinor = trvTaxMinor runtime
        , Routes.checkoutTotalMinor = trvCheckoutTotalMinor runtime
        , Routes.organizerPayableMinor = trvOrganizerPayable runtime
        , Routes.platformFeeMinor = trvPlatformFeeMinor runtime
        , Routes.termsVersion = trvTermsVersion runtime
        }
    , Routes.paymentMethods = paymentMethods
    , Routes.tickets = publicTickets
    }
  where
    toPublicTicket (Entity ticketKey ticket) = Routes.PublicEventTicketDTO
      { Routes.ticketId = fromSqlKey ticketKey
      , Routes.ticketCode = SM.eventTicketCode ticket
      , Routes.status = SM.eventTicketStatus ticket
      , Routes.holderName = SM.eventTicketHolderName ticket
      }

loadPublicTicketPaymentMethods :: TicketRuntimeView -> AppM [Text]
loadPublicTicketPaymentMethods runtime = do
  now <- liftIO getCurrentTime
  if trvPaymentStatus runtime `notElem` ["holding", "awaiting_payment", "failed"]
      || trvHoldExpiresAt runtime <= now
    then pure []
    else do
      let checkout = Checkout.CheckoutReference (trvCheckoutId runtime)
      environmentResult <- runDB (Checkout.loadCheckoutEnvironment checkout)
      case environmentResult of
        Left _ -> pure []
        Right environment -> do
          domainEnabled <- runDB $
            Checkout.domainEnabledForEnvironment environment "event_tickets"
          if not domainEnabled then pure [] else do
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

requireLookupToken :: SM.EventTicketOrderId -> Maybe Text -> AppM ()
requireLookupToken orderKey mLookupToken = do
  supplied <- case T.strip <$> mLookupToken of
    Just value | not (T.null value) -> pure value
    _ -> throwError notFound
  hashes <- runDB (rawSql
    "SELECT lookup_token_hash FROM event_ticket_checkout_runtime WHERE order_id = ?"
    [toPersistValue orderKey] :: SqlPersistT IO [Single Text])
  stored <- case hashes of
    [Single value] -> pure value
    _ -> throwError notFound
  unless (constEq (TE.encodeUtf8 stored) (TE.encodeUtf8 (sha256Text supplied))) $
    throwError notFound

authorizeTicketCheckout
  :: Int64
  -> Int64
  -> Maybe Text
  -> AppM (SM.EventTicketOrderId, TicketRuntimeView)
authorizeTicketCheckout rawEventId rawOrderId mLookupToken = do
  eventId <- either throwError pure (validatePositiveKey "eventId" rawEventId)
  orderId <- either throwError pure (validatePositiveKey "orderId" rawOrderId)
  let orderKey = toSqlKey orderId
  requireLookupToken orderKey mLookupToken
  runtime <- runDB (loadTicketRuntimeView orderKey)
    >>= maybe (throwError notFound) pure
  unless (trvEventId runtime == eventId) (throwError notFound)
  pure (orderKey, runtime)

getPublicEventTicketCheckout
  :: Int64
  -> Int64
  -> Maybe Text
  -> AppM Routes.PublicEventTicketCheckoutResponse
getPublicEventTicketCheckout rawEventId rawOrderId mLookupToken = do
  (orderKey, runtime) <- authorizeTicketCheckout rawEventId rawOrderId mLookupToken
  now <- liftIO getCurrentTime
  when (trvHoldExpiresAt runtime <= now
      && trvPaymentStatus runtime `elem` ["holding", "awaiting_payment", "failed"]) $
    void $ runDB (rawSql "SELECT event_ticket_checkout_expire_holds(?, NULL, ?)"
      [PersistUTCTime now, PersistInt64 (trvEventId runtime)]
      :: SqlPersistT IO [Single Int])
  loadTicketCheckoutDTO orderKey Nothing

loadTicketPaymentContext :: SM.EventTicketOrderId -> AppM TicketPaymentContext
loadTicketPaymentContext orderKey = do
  rows <- runDB (rawSql
    "SELECT runtime.event_id, runtime.checkout_id::text, runtime.create_idempotency_key,\
    \ checkout.status, checkout.environment, runtime.checkout_total_minor,\
    \ runtime.currency, runtime.hold_expires_at, ticket_order.buyer_name,\
    \ ticket_order.buyer_email, ticket_order.metadata\
    \ FROM event_ticket_checkout_runtime runtime\
    \ JOIN event_ticket_order ticket_order ON ticket_order.id = runtime.order_id\
    \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
    \ WHERE runtime.order_id = ? AND checkout.domain_type = 'event_ticket_order'\
    \ AND checkout.domain_order_id = runtime.order_id::text\
    \ AND checkout.total_minor = runtime.checkout_total_minor\
    \ AND checkout.currency = runtime.currency"
    [toPersistValue orderKey]
    :: SqlPersistT IO
      [( Single Int64, Single Text, Single Text, Single Text, Single Text
       , Single Int64, Single Text, Single UTCTime, Single (Maybe Text)
       , Single (Maybe Text), Single (Maybe Text)
       )])
  case rows of
    [( Single eventId, Single checkoutId, Single createKey, Single checkoutStatus
     , Single environmentText, Single amountMinor, Single currency
     , Single holdExpiresAt, Single mBuyerName, Single mBuyerEmail, _
     )] -> do
      environment <- either (throwError . internal) pure $
        Checkout.resolveCheckoutEnvironment (Just (T.unpack environmentText))
      buyerName <- maybe (throwError (internal "Ticket buyer name is missing")) pure mBuyerName
      buyerEmail <- maybe (throwError (internal "Ticket buyer email is missing")) pure mBuyerEmail
      pure TicketPaymentContext
        { tpcOrderKey = orderKey
        , tpcEventKey = toSqlKey eventId
        , tpcCheckout = Checkout.CheckoutReference checkoutId
        , tpcCreateIdempotencyKey = createKey
        , tpcCheckoutStatus = checkoutStatus
        , tpcEnvironment = environment
        , tpcAmountMinor = amountMinor
        , tpcCurrency = currency
        , tpcHoldExpiresAt = holdExpiresAt
        , tpcBuyerName = buyerName
        , tpcBuyerEmail = buyerEmail
        , tpcBuyerPhone = Nothing
        }
    [] -> throwError notFound
    _ -> throwError (internal "Ticket payment context is ambiguous")

requireTicketPaymentContext
  :: Int64
  -> Int64
  -> Maybe Text
  -> AppM TicketPaymentContext
requireTicketPaymentContext rawEventId rawOrderId mLookupToken = do
  (orderKey, _) <- authorizeTicketCheckout rawEventId rawOrderId mLookupToken
  context <- loadTicketPaymentContext orderKey
  now <- liftIO getCurrentTime
  when (tpcHoldExpiresAt context <= now
      && tpcCheckoutStatus context `elem` ["holding", "awaiting_payment", "failed"]) $ do
    void $ runDB (rawSql "SELECT event_ticket_checkout_expire_holds(?, NULL, ?)"
      [PersistUTCTime now, toPersistValue (tpcEventKey context)]
      :: SqlPersistT IO [Single Int])
    throwError (conflict "This ticket seat hold expired; start a new checkout")
  unless (tpcCheckoutStatus context `elem`
      ["holding", "awaiting_payment", "processing", "failed", "paid"]) $
    throwError (conflict "This ticket checkout no longer accepts payment actions")
  pure context

ticketReference :: TicketPaymentContext -> Text
ticketReference = T.pack . show . fromSqlKey . tpcOrderKey

requireTicketProvider
  :: TicketPaymentContext
  -> Checkout.CheckoutEnvironment
  -> Checkout.PaymentProvider
  -> AppM ()
requireTicketProvider context configuredEnvironment provider = do
  unless (configuredEnvironment == tpcEnvironment context) $
    throwError err503
      { errBody = "Configured provider environment does not match this immutable ticket checkout" }
  domainEnabled <- runDB $
    Checkout.domainEnabledForEnvironment configuredEnvironment "event_tickets"
  unless domainEnabled $
    throwError err503 { errBody = "Public ticket checkout is disabled in this environment" }
  providerEnabled <- runDB $
    Checkout.providerEnabledForEnvironment configuredEnvironment provider
  unless providerEnabled $
    throwError err503 { errBody = "Payment provider is disabled for this checkout environment" }

ticketPaymentIdempotencyKey
  :: TicketPaymentContext
  -> Checkout.PaymentProvider
  -> Checkout.PaymentOperation
  -> Text
ticketPaymentIdempotencyKey context provider operation = sha256Text $
  "event-ticket-payment:" <> tpcCreateIdempotencyKey context
    <> ":" <> Checkout.paymentProviderText provider
    <> ":" <> case operation of
      Checkout.OperationCreate -> "create"
      Checkout.OperationAuthorize -> "authorize"
      Checkout.OperationCapture -> "capture"
      Checkout.OperationManualVerify -> "manual-verify"

ticketPaymentCorrelationId
  :: TicketPaymentContext
  -> Checkout.PaymentProvider
  -> Text
  -> Text
ticketPaymentCorrelationId context provider operation =
  "event-ticket:" <> ticketReference context <> ":"
    <> Checkout.paymentProviderText provider <> ":" <> operation

beginTicketPaymentAttempt
  :: TicketPaymentContext
  -> Checkout.PaymentProvider
  -> Checkout.PaymentOperation
  -> Text
  -> Text
  -> AppM Checkout.PaymentAttemptReference
beginTicketPaymentAttempt context provider operation merchantRef operationLabel = do
  now <- liftIO getCurrentTime
  result <- runDB $ Checkout.beginPaymentAttempt Checkout.PaymentAttemptCreation
    { Checkout.pacCheckout = tpcCheckout context
    , Checkout.pacProvider = provider
    , Checkout.pacEnvironment = tpcEnvironment context
    , Checkout.pacOperation = operation
    , Checkout.pacAmountMinor = tpcAmountMinor context
    , Checkout.pacCurrency = tpcCurrency context
    , Checkout.pacMerchantRef = merchantRef
    , Checkout.pacIdempotencyKey = ticketPaymentIdempotencyKey context provider operation
    , Checkout.pacCreatedAt = now
    , Checkout.pacCorrelationId = ticketPaymentCorrelationId context provider operationLabel
    }
  either (throwError . conflict) pure result

failTicketPaymentAttempt
  :: TicketPaymentContext
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Text
  -> ServerError
  -> AppM a
failTicketPaymentAttempt context attempt provider failureCode providerError = do
  now <- liftIO getCurrentTime
  runDB $ Checkout.recordPaymentFailure
    (tpcCheckout context) attempt provider failureCode
    (ticketPaymentCorrelationId context provider "provider-error") now
  throwError providerError

loadTicketProviderBinding
  :: TicketPaymentContext
  -> Checkout.PaymentProvider
  -> Text
  -> Text
  -> AppM (Maybe (Text, Maybe Text))
loadTicketProviderBinding context provider merchantRef resourceType = do
  rows <- runDB (rawSql
    "SELECT binding.provider_resource_id, binding.provider_resource_path\
    \ FROM commerce_provider_binding binding\
    \ JOIN commerce_payment_attempt attempt ON attempt.id = binding.payment_attempt_id\
    \ WHERE attempt.checkout_id = ?::uuid AND attempt.provider = ?\
    \ AND attempt.environment = ? AND attempt.merchant_account_ref = ?\
    \ AND binding.provider = attempt.provider AND binding.environment = attempt.environment\
    \ AND binding.merchant_account_ref = attempt.merchant_account_ref\
    \ AND binding.resource_type = ? AND binding.merchant_reference = ?\
    \ AND binding.amount_minor = ? AND binding.currency = ?"
    [ PersistText (Checkout.checkoutReferenceId (tpcCheckout context))
    , PersistText (Checkout.paymentProviderText provider)
    , PersistText (Checkout.checkoutEnvironmentText (tpcEnvironment context))
    , PersistText merchantRef, PersistText resourceType
    , PersistText (ticketReference context), PersistInt64 (tpcAmountMinor context)
    , PersistText (tpcCurrency context)
    ] :: SqlPersistT IO [(Single Text, Single (Maybe Text))])
  case rows of
    [] -> pure Nothing
    [(Single resourceId, Single resourcePath)] -> pure (Just (resourceId, resourcePath))
    _ -> throwError (internal "Ticket provider binding is ambiguous")

bindTicketProviderResource
  :: TicketPaymentContext
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
bindTicketProviderResource context attempt provider environment merchantRef
    resourceType resourceId resourcePath stage operationLabel = do
  now <- liftIO getCurrentTime
  result <- runDB $ Checkout.bindProviderResource Checkout.ProviderBindingCreation
    { Checkout.pbcAttempt = attempt
    , Checkout.pbcCheckout = tpcCheckout context
    , Checkout.pbcProvider = provider
    , Checkout.pbcEnvironment = environment
    , Checkout.pbcMerchantRef = merchantRef
    , Checkout.pbcResourceType = resourceType
    , Checkout.pbcProviderResource = resourceId
    , Checkout.pbcResourcePath = resourcePath
    , Checkout.pbcOrderReference = ticketReference context
    , Checkout.pbcAmountMinor = tpcAmountMinor context
    , Checkout.pbcCurrency = tpcCurrency context
    , Checkout.pbcStage = stage
    , Checkout.pbcOccurredAt = now
    , Checkout.pbcCorrelationId = ticketPaymentCorrelationId context provider operationLabel
    }
  either (throwError . conflict) pure result

ensureNoOtherActiveTicketAttempt
  :: TicketPaymentContext
  -> Checkout.PaymentProvider
  -> AppM ()
ensureNoOtherActiveTicketAttempt context provider = do
  rows <- runDB (rawSql
    "SELECT EXISTS (SELECT 1 FROM commerce_payment_attempt\
    \ WHERE checkout_id = ?::uuid AND provider <> ?\
    \ AND status IN ('requires_customer_action','processing'))"
    [ PersistText (Checkout.checkoutReferenceId (tpcCheckout context))
    , PersistText (Checkout.paymentProviderText provider)
    ] :: SqlPersistT IO [Single Bool])
  when (rows == [Single True]) $
    throwError (conflict "Another online payment rail is active for this ticket checkout")

createPublicEventTicketDatafastCheckout
  :: Int64
  -> Int64
  -> Maybe Text
  -> AppM APITypes.DatafastCheckoutDTO
createPublicEventTicketDatafastCheckout rawEventId rawOrderId mLookupToken = do
  context <- requireTicketPaymentContext rawEventId rawOrderId mLookupToken
  when (tpcCheckoutStatus context == "paid") $
    throwError (conflict "This ticket order is already paid")
  ensureNoOtherActiveTicketAttempt context Checkout.ProviderDatafast
  datafast <- ServiceStorefront.loadServiceDatafastEnv
  requireTicketProvider context
    (ServiceStorefront.sdfEnvironment datafast) Checkout.ProviderDatafast
  attempt <- beginTicketPaymentAttempt context Checkout.ProviderDatafast
    Checkout.OperationCreate (ServiceStorefront.sdfEntityId datafast) "create"
  existing <- loadTicketProviderBinding context Checkout.ProviderDatafast
    (ServiceStorefront.sdfEntityId datafast) "checkout"
  (checkoutId, widgetUrl) <- case existing of
    Just (storedCheckoutId, _) -> pure
      ( storedCheckoutId
      , stripTrailingSlash (ServiceStorefront.sdfBaseUrl datafast)
          <> "/v1/paymentWidgets.js?checkoutId=" <> T.unpack storedCheckoutId
      )
    Nothing -> ServiceStorefront.requestDatafastCheckoutForService
      (ticketReference context)
      (fromIntegral (tpcAmountMinor context))
      (tpcCurrency context)
      (tpcBuyerName context)
      (tpcBuyerEmail context)
      (tpcBuyerPhone context)
      `catchError` failTicketPaymentAttempt context attempt
        Checkout.ProviderDatafast "datafast_checkout_create"
  let resourcePath = "/v1/checkouts/" <> checkoutId <> "/payment"
  bindTicketProviderResource context attempt Checkout.ProviderDatafast
    (ServiceStorefront.sdfEnvironment datafast)
    (ServiceStorefront.sdfEntityId datafast) "checkout" checkoutId
    (Just resourcePath) Checkout.AttemptRequiresCustomerAction "create"
  runDB $ update (tpcOrderKey context)
    [SM.EventTicketOrderPaymentMethod =. Just "datafast"]
  pure APITypes.DatafastCheckoutDTO
    { APITypes.dcOrderId = ticketReference context
    , APITypes.dcCheckoutId = checkoutId
    , APITypes.dcWidgetUrl = T.pack widgetUrl
    , APITypes.dcAmount = Internationalization.formatMinorUnitsDecimal
        (tpcCurrency context) (fromIntegral (tpcAmountMinor context))
    , APITypes.dcCurrency = tpcCurrency context
    , APITypes.dcLookupToken = Nothing
    }

stripTrailingSlash :: String -> String
stripTrailingSlash = reverse . dropWhile (== '/') . reverse

confirmPublicEventTicketDatafastStatus
  :: Int64
  -> Int64
  -> Maybe Text
  -> Text
  -> AppM Routes.PublicEventTicketCheckoutResponse
confirmPublicEventTicketDatafastStatus rawEventId rawOrderId mLookupToken rawResourcePath = do
  (orderKey, _) <- authorizeTicketCheckout rawEventId rawOrderId mLookupToken
  initialContext <- loadTicketPaymentContext orderKey
  nowBefore <- liftIO getCurrentTime
  when (tpcHoldExpiresAt initialContext <= nowBefore
      && tpcCheckoutStatus initialContext `elem` ["holding", "awaiting_payment", "failed"]) $
    void $ runDB (rawSql "SELECT event_ticket_checkout_expire_holds(?, NULL, ?)"
      [PersistUTCTime nowBefore, toPersistValue (tpcEventKey initialContext)]
      :: SqlPersistT IO [Single Int])
  context <- loadTicketPaymentContext orderKey
  if tpcCheckoutStatus context == "paid"
    then finalizeVerifiedTicketOrder context >> loadTicketCheckoutDTO orderKey Nothing
    else do
      datafast <- ServiceStorefront.loadServiceDatafastEnv
      unless (ServiceStorefront.sdfEnvironment datafast == tpcEnvironment context) $
        throwError err503
          { errBody = "Configured Datafast environment does not match this immutable ticket checkout" }
      existing <- loadTicketProviderBinding context Checkout.ProviderDatafast
        (ServiceStorefront.sdfEntityId datafast) "checkout"
      (checkoutId, storedPath) <- maybe
        (throwError (conflict "This ticket order has no bound Datafast checkout")) pure existing
      resourcePath <- either (throwError . badRequest) pure $
        ServiceStorefront.validateDatafastOrderResourcePath (Just checkoutId) rawResourcePath
      unless (storedPath == Just resourcePath) $
        throwError (conflict "Datafast resource path does not match the immutable ticket binding")
      attempt <- beginTicketPaymentAttempt context Checkout.ProviderDatafast
        Checkout.OperationCreate (ServiceStorefront.sdfEntityId datafast) "create"
      providerStatus <- ServiceStorefront.checkDatafastPaymentStatus resourcePath
        `catchError` failTicketPaymentAttempt context attempt
          Checkout.ProviderDatafast "datafast_status_request"
      now <- liftIO getCurrentTime
      let resultCode = ServiceStorefront.sdfpsResultCode providerStatus
          success = ServiceStorefront.isDatafastPaymentSuccess
            (ServiceStorefront.sdfEnvironment datafast) resultCode
          pending = resultCode == "000.200.000"
      if success then do
        case ServiceStorefront.validateDatafastSuccessfulPayment
            (ticketReference context) (fromIntegral (tpcAmountMinor context))
            (tpcCurrency context) providerStatus of
          Left validationMessage -> do
            let actualAmount = ServiceStorefront.sdfpsAmount providerStatus
                  >>= either (const Nothing) (Just . fromIntegral)
                    . ServiceStorefront.parseDatafastCents
                providerRef = fromMaybe checkoutId
                  (ServiceStorefront.sdfpsPaymentId providerStatus)
            runDB $ do
              Checkout.recordReconciliationException Checkout.ProviderDatafast
                (tpcEnvironment context) (ServiceStorefront.sdfEntityId datafast)
                "provider_binding_mismatch" (ticketReference context) providerRef
                (tpcAmountMinor context) actualAmount (tpcCurrency context) now
              Checkout.recordPaymentFailure (tpcCheckout context) attempt
                Checkout.ProviderDatafast "provider_binding_mismatch"
                (ticketPaymentCorrelationId context Checkout.ProviderDatafast "status") now
            throwError err502 { errBody = textBody validationMessage }
          Right () -> pure ()
        when (tpcCheckoutStatus context == "expired" || tpcHoldExpiresAt context <= now) $ do
          runDB $ Checkout.recordReconciliationException Checkout.ProviderDatafast
            (tpcEnvironment context) (ServiceStorefront.sdfEntityId datafast)
            "payment_after_ticket_hold_expiry" (ticketReference context) checkoutId
            (tpcAmountMinor context) Nothing (tpcCurrency context) now
          throwError (conflict
            "Datafast reports payment after this ticket hold expired; reconciliation is required and no ticket was issued")
        paymentId <- maybe (throwError err502 { errBody = "Datafast payment ID is missing" }) pure
          (ServiceStorefront.sdfpsPaymentId providerStatus)
        bindTicketProviderResource context attempt Checkout.ProviderDatafast
          (tpcEnvironment context) (ServiceStorefront.sdfEntityId datafast)
          "payment" paymentId (Just resourcePath) Checkout.AttemptProcessing "status"
        verified <- runDB $ Checkout.recordVerifiedPayment Checkout.VerifiedPayment
          { Checkout.vpAttempt = attempt
          , Checkout.vpCheckout = tpcCheckout context
          , Checkout.vpProvider = Checkout.ProviderDatafast
          , Checkout.vpEnvironment = tpcEnvironment context
          , Checkout.vpMerchantRef = ServiceStorefront.sdfEntityId datafast
          , Checkout.vpResourceType = "checkout"
          , Checkout.vpProviderResource = checkoutId
          , Checkout.vpProviderResourcePath = Just resourcePath
          , Checkout.vpOrderReference = ticketReference context
          , Checkout.vpAmountMinor = tpcAmountMinor context
          , Checkout.vpCurrency = tpcCurrency context
          , Checkout.vpEvidence = "server_to_server"
          , Checkout.vpOccurredAt = now
          , Checkout.vpCorrelationId = ticketPaymentCorrelationId
              context Checkout.ProviderDatafast "status"
          }
        either (throwError . conflict) (const (pure ())) verified
        finalizeVerifiedTicketOrder context
      else if tpcCheckoutStatus context == "expired"
        then pure ()
        else if pending
          then runDB $ Checkout.recordPaymentProcessing
            (tpcCheckout context) attempt Checkout.ProviderDatafast
            (ticketPaymentCorrelationId context Checkout.ProviderDatafast "status") now
          else runDB $ Checkout.recordPaymentFailure
            (tpcCheckout context) attempt Checkout.ProviderDatafast resultCode
            (ticketPaymentCorrelationId context Checkout.ProviderDatafast "status") now
      loadTicketCheckoutDTO orderKey Nothing
createPublicEventTicketPaypalOrder
  :: Int64
  -> Int64
  -> Maybe Text
  -> AppM APITypes.PaypalCreateDTO
createPublicEventTicketPaypalOrder rawEventId rawOrderId mLookupToken = do
  context <- requireTicketPaymentContext rawEventId rawOrderId mLookupToken
  when (tpcCheckoutStatus context == "paid") $
    throwError (conflict "This ticket order is already paid")
  ensureNoOtherActiveTicketAttempt context Checkout.ProviderPayPal
  (clientId, clientSecret, baseUrl, paypalEnvironment, merchantRef) <-
    ServiceStorefront.loadPaypalEnvForService
  requireTicketProvider context paypalEnvironment Checkout.ProviderPayPal
  attempt <- beginTicketPaymentAttempt context Checkout.ProviderPayPal
    Checkout.OperationCreate merchantRef "create"
  existing <- loadTicketProviderBinding context Checkout.ProviderPayPal merchantRef "order"
  (paypalOrderId, approvalUrl) <- case existing of
    Just (storedOrderId, _) -> pure (storedOrderId, Nothing)
    Nothing -> ServiceStorefront.createPaypalOrderRemoteForService
      sharedTlsManager clientId clientSecret baseUrl (ticketReference context)
      (fromIntegral (tpcAmountMinor context)) (tpcCurrency context)
      (tpcBuyerName context) (tpcBuyerEmail context)
      `catchError` failTicketPaymentAttempt context attempt
        Checkout.ProviderPayPal "paypal_create_order"
  bindTicketProviderResource context attempt Checkout.ProviderPayPal paypalEnvironment
    merchantRef "order" paypalOrderId
    (Just ("/v2/checkout/orders/" <> paypalOrderId))
    Checkout.AttemptRequiresCustomerAction "create"
  runDB $ update (tpcOrderKey context)
    [SM.EventTicketOrderPaymentMethod =. Just "paypal"]
  pure APITypes.PaypalCreateDTO
    { APITypes.pcOrderId = ticketReference context
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

capturePublicEventTicketPaypalOrder
  :: Int64
  -> Int64
  -> Maybe Text
  -> Routes.PublicEventTicketPaypalCaptureRequest
  -> AppM Routes.PublicEventTicketCheckoutResponse
capturePublicEventTicketPaypalOrder rawEventId rawOrderId mLookupToken request = do
  context <- requireTicketPaymentContext rawEventId rawOrderId mLookupToken
  if tpcCheckoutStatus context == "paid"
    then finalizeVerifiedTicketOrder context >> loadTicketCheckoutDTO (tpcOrderKey context) Nothing
    else do
      suppliedOrderId <- either throwError pure $
        validatePaypalOrderId (Routes.paypalOrderId request)
      (clientId, clientSecret, baseUrl, paypalEnvironment, merchantRef) <-
        ServiceStorefront.loadPaypalEnvForService
      requireTicketProvider context paypalEnvironment Checkout.ProviderPayPal
      existing <- loadTicketProviderBinding context Checkout.ProviderPayPal merchantRef "order"
      storedOrderId <- maybe
        (throwError (conflict "This ticket order has no bound PayPal order"))
        (pure . fst) existing
      unless (storedOrderId == suppliedOrderId) $
        throwError (conflict "PayPal order does not match the immutable ticket binding")
      attempt <- beginTicketPaymentAttempt context Checkout.ProviderPayPal
        Checkout.OperationCapture merchantRef "capture"
      outcome <- ServiceStorefront.capturePaypalOrderRemoteForService
        sharedTlsManager clientId clientSecret baseUrl suppliedOrderId
        `catchError` failTicketPaymentAttempt context attempt
          Checkout.ProviderPayPal "paypal_capture_request"
      now <- liftIO getCurrentTime
      case ServiceStorefront.spcoStatus outcome of
        "COMPLETED" -> do
          case ServiceStorefront.validatePaypalSuccessfulCapture
              (ticketReference context) (fromIntegral (tpcAmountMinor context))
              (tpcCurrency context) merchantRef outcome of
            Left validationMessage -> do
              let actualAmount = ServiceStorefront.spcoAmount outcome
                    >>= either (const Nothing) (Just . fromIntegral)
                      . ServiceStorefront.parseDatafastCents
              runDB $ do
                Checkout.recordReconciliationException Checkout.ProviderPayPal
                  paypalEnvironment merchantRef "provider_binding_mismatch"
                  (ticketReference context) suppliedOrderId (tpcAmountMinor context)
                  actualAmount (tpcCurrency context) now
                Checkout.recordPaymentFailure (tpcCheckout context) attempt
                  Checkout.ProviderPayPal "provider_binding_mismatch"
                  (ticketPaymentCorrelationId context Checkout.ProviderPayPal "capture") now
              throwError err502 { errBody = textBody validationMessage }
            Right () -> pure ()
          when (tpcHoldExpiresAt context <= now || tpcCheckoutStatus context == "expired") $ do
            runDB $ Checkout.recordReconciliationException Checkout.ProviderPayPal
              paypalEnvironment merchantRef "payment_after_ticket_hold_expiry"
              (ticketReference context) suppliedOrderId (tpcAmountMinor context)
              Nothing (tpcCurrency context) now
            throwError (conflict
              "PayPal captured after this ticket hold expired; reconciliation is required and no ticket was issued")
          captureId <- maybe (throwError err502 { errBody = "PayPal capture ID is missing" }) pure
            (ServiceStorefront.spcoCaptureId outcome)
          bindTicketProviderResource context attempt Checkout.ProviderPayPal paypalEnvironment
            merchantRef "capture" captureId
            (Just ("/v2/checkout/orders/" <> suppliedOrderId <> "/capture"))
            Checkout.AttemptProcessing "capture"
          verified <- runDB $ Checkout.recordVerifiedPayment Checkout.VerifiedPayment
            { Checkout.vpAttempt = attempt
            , Checkout.vpCheckout = tpcCheckout context
            , Checkout.vpProvider = Checkout.ProviderPayPal
            , Checkout.vpEnvironment = paypalEnvironment
            , Checkout.vpMerchantRef = merchantRef
            , Checkout.vpResourceType = "capture"
            , Checkout.vpProviderResource = captureId
            , Checkout.vpProviderResourcePath = Just
                ("/v2/checkout/orders/" <> suppliedOrderId <> "/capture")
            , Checkout.vpOrderReference = ticketReference context
            , Checkout.vpAmountMinor = tpcAmountMinor context
            , Checkout.vpCurrency = tpcCurrency context
            , Checkout.vpEvidence = "server_to_server"
            , Checkout.vpOccurredAt = now
            , Checkout.vpCorrelationId = ticketPaymentCorrelationId
                context Checkout.ProviderPayPal "capture"
            }
          either (throwError . conflict) (const (pure ())) verified
          finalizeVerifiedTicketOrder context
        "APPROVED" -> runDB $ Checkout.recordPaymentProcessing
          (tpcCheckout context) attempt Checkout.ProviderPayPal
          (ticketPaymentCorrelationId context Checkout.ProviderPayPal "capture") now
        "PENDING" -> runDB $ Checkout.recordPaymentProcessing
          (tpcCheckout context) attempt Checkout.ProviderPayPal
          (ticketPaymentCorrelationId context Checkout.ProviderPayPal "capture") now
        providerStatus -> runDB $ Checkout.recordPaymentFailure
          (tpcCheckout context) attempt Checkout.ProviderPayPal
          ("paypal_" <> T.toLower providerStatus)
          (ticketPaymentCorrelationId context Checkout.ProviderPayPal "capture") now
      loadTicketCheckoutDTO (tpcOrderKey context) Nothing

finalizeVerifiedTicketOrder :: TicketPaymentContext -> AppM ()
finalizeVerifiedTicketOrder context = do
  now <- liftIO getCurrentTime
  Env{ envPool } <- ask
  (order, ticketCodes, newlyIssued) <- liftIO $ runSqlPool (do
    runtimeRows <- (rawSql
      "SELECT runtime.fulfillment_status, runtime.payment_status,\
      \ runtime.promo_code_id, runtime.discount_minor\
      \ FROM event_ticket_checkout_runtime runtime\
      \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
      \ WHERE runtime.order_id = ? AND checkout.status = 'paid' FOR UPDATE OF runtime"
      [toPersistValue (tpcOrderKey context)]
      :: SqlPersistT IO
        [(Single Text, Single Text, Single (Maybe Int64), Single Int64)])
    (fulfillmentStatus, paymentStatus, promoKey, discountMinor) <- case runtimeRows of
      [(Single status, Single payment, Single promoId, Single discount)] ->
        pure (status, payment, toSqlKey <$> promoId, discount)
      _ -> fail "Ticket issuance requires one locked canonical checkout runtime"
    unless (paymentStatus == "paid") $
      fail "Ticket issuance requires a paid canonical checkout"
    order <- getJust (tpcOrderKey context)
    when (SM.eventTicketOrderStatus order `notElem` ["pending", "paid"]) $
      fail "Closed ticket order cannot be issued"
    updateWhere
      [ SM.EventTicketOrderId ==. tpcOrderKey context
      , SM.EventTicketOrderStatus ==. "pending"
      ]
      [ SM.EventTicketOrderStatus =. "paid"
      , SM.EventTicketOrderUpdatedAt =. now
      ]
    case fulfillmentStatus of
      "seat_held" -> do
        ticketCodes <- SocialEvents.issueMissingTicketsForOrder
          now (tpcOrderKey context) order
        forM_ promoKey $ \key -> do
          existingRedemption <- selectFirst
            [SM.PromoCodeRedemptionOrderId ==. tpcOrderKey context] []
          when (not (isJust existingRedemption)) $ insert_ SM.PromoCodeRedemption
            { SM.promoCodeRedemptionPromoCodeId = key
            , SM.promoCodeRedemptionOrderId = tpcOrderKey context
            , SM.promoCodeRedemptionDiscountAmountCents = fromIntegral discountMinor
            , SM.promoCodeRedemptionRedeemedAt = now
            }
        rawExecute
          "UPDATE event_ticket_checkout_runtime\
          \ SET fulfillment_status='issued', issued_at=?, updated_at=? WHERE order_id=?"
          [ PersistUTCTime now, PersistUTCTime now
          , toPersistValue (tpcOrderKey context)
          ]
        rawExecute
          "INSERT INTO event_ticket_fulfillment_event(\
          \ order_id, from_status, to_status, actor_type, reason_code, notes\
          \) VALUES (?, 'seat_held', 'issued', 'provider', 'verified_payment',\
          \ 'Tickets issued only after canonical provider verification')"
          [toPersistValue (tpcOrderKey context)]
        pure (order, ticketCodes, True)
      "issued" -> do
        existingTickets <- selectList
          [SM.EventTicketOrderRefId ==. tpcOrderKey context] [Asc SM.EventTicketId]
        pure (order, map (SM.eventTicketCode . entityVal) existingTickets, False)
      _ -> fail "Paid ticket checkout is not in an issuable fulfillment state") envPool
  when newlyIssued $
    SocialEvents.sendTicketConfirmationForOrder order ticketCodes
