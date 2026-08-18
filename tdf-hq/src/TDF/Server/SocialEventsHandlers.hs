{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Server.SocialEventsHandlers (
    socialEventsServer,
    stripeWebhookServer,
    validateRsvpStatus,
    validateInvitationToPartyId,
    validateInvitationFromPartyId,
    validateInvitationStatusInput,
    validateInvitationStatusUpdateInput,
    normalizeInvitationStatus,
    normalizeArtistGenres,
    parseInvitationIdsEither,
    parseVenueIdEither,
    parseFollowerQueryParamEither,
    parseNearQueryEither,
    followArtistDb,
    normalizeTicketOrderStatus,
    ticketOrderInventoryAdjustment,
    validateDirectTicketOrderPricing,
    validateTicketPurchaseEventEligibility,
    eventTicketPurchaseEnabledFor,
    isTicketTierSaleOpen,
    issueMissingTicketsForOrder,
    sendTicketConfirmationForOrder,
    normalizeTicketStatus,
    validateEventMetadataUpdate,
    validateEventMetadataUrlField,
    validateBudgetLineTypeInput,
    normalizeBudgetLineType,
    normalizeFinanceDirection,
    normalizeFinanceSource,
    normalizeFinanceEntryStatus,
    validateFinanceEntryCurrencyInput,
    validateOptionalBudgetLineIdInput,
    validateStoredBudgetLineDimensions,
    validateStoredFinanceEntryDimensions,
    validateStoredEventFinanceMetadata,
    normalizePositivePartyIdText,
    resolveExistingPartyIdText,
    resolveUniqueRsvpRow,
    validateEventArtistIds,
    normalizeMomentMediaType,
    normalizeMomentCaption,
    normalizeMomentCommentBody,
    normalizeLiveBroadcastTitle,
    normalizeLiveBroadcastDescription,
    normalizeLiveBroadcastQuality,
    validateMomentMediaDimension,
    validateMomentMediaDuration,
    validateEventCreateUpdateDimensions,
    validateSocialEventsListOffset,
    validateSocialEventsListFilter,
    validateDiscoverySourceWrite,
    validateVenueCreateUpdateFields,
    validateEventCurrencyInput,
    TicketCheckInLookup (..),
    validateTicketCheckInLookup,
    validateStoredTicketOrderStatus,
    validateTicketCheckInOrderStatus,
    validateTicketCheckInTicketStatus,
    storedTicketOrderSummaryFields,
    ticketOrderAccountingEntriesEither,
    findTicketForCheckIn,
    validateOptionalTicketBuyerPartyId,
    validateTicketPurchaseBuyerName,
    validateTicketPurchaseBuyerEmail,
    validateTicketTierCodeInput,
    validateTicketTierCurrencyInput,
    encodePromoCodeTierIds,
    decodeStoredPromoCodeTierIds,
    validatePromoCodeDateWindow,
    validatePromoCodeRedemptionLimit,
    validatePromoCodeTierEligibility,
    validatePromoCodeMinimumPurchaseParam,
    validatePromoCodeMinimumPurchaseCents,
    promoCodeDiscountAmountEither,
    validateTicketCheckoutAmount,
    isImageUpload,
    validateEventImageUploadSize,
    validateEventTitleInput,
    validateArtistName,
    validateSocialEventsFeatureAction,
    validateArtistProfileCreateParty,
    validateArtistProfileWriteAccess,
    validateAuthenticatedPartyReference,
    parseStripePaymentIntentResponse,
    parseStripeWebhookEventEnvelope,
    verifyAndDecodeStripeWebhook,
    parseStripeWebhookPaymentIntentId,
    StripeTicketPaymentEvidence (..),
    parseStripeTicketPaymentEvidence,
    validateStripeTicketPaymentEvidence,
    parseStripeWebhookMarketplaceOrderId,
    canRecoverMarketplaceStripeOrder,
    parseCheckoutSessionCourseSubscription,
    parseSubscriptionEvent,
    parseStripeRefundResponse,
    parseStripeCustomerId,
    parseStripeEphemeralKeySecret,
    resolveStripeCustomerForBuyer,
    eitherStripeServerError,
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Control.Monad (filterM, forM, forM_, join, unless, void, when)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Data.Aeson.Types (Object, Parser, parseMaybe)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (
    GeneralCategory (Format, LineSeparator, ParagraphSeparator),
    generalCategory,
    isAlphaNum,
    isAscii,
    isAsciiLower,
    isAsciiUpper,
    isControl,
    isHexDigit,
 )
import Data.Int (Int64)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, utctDay)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import System.Directory (copyFile, createDirectoryIfMissing, getFileSize)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Web.PathPieces (fromPathPiece)

import Servant
import Servant.Multipart (FileData (..))

-- Pull in full Persistent surface so TH-generated field constructors
-- (EventRsvpEventId, SocialEventStartTime, etc.) are available.
import Database.Persist
import Database.Persist.Sql (ConnectionPool, SqlBackend, SqlPersistT, fromSqlKey, rawSql, runSqlPool, toSqlKey, updateWhereCount)
import Database.PostgreSQL.Simple (SqlError (..))

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Time.Clock (addUTCTime)
import qualified System.Random as Random
import TDF.API.SocialEventsAPI
import qualified TDF.Server.EventResearch as EventResearch
import TDF.Auth (AuthedUser (..), hasStrictAdminAccess, moduleName)
import qualified TDF.Catalog.Models as Catalog
import TDF.Config (AppConfig (..), EmailConfig, assetsRootDir, resolveConfiguredAssetsBase)
import TDF.Internationalization (normalizeCurrencyCode)
import TDF.DB (Env (..))
import TDF.FeatureRegistry (findRegistryFeature, registryFeatureAllows)
import TDF.DTO.SocialEventsDTO (
    ArtistDTO (..),
    ArtistFollowRequest (..),
    ArtistFollowerDTO (..),
    ArtistSocialLinksDTO (..),
    EventBudgetLineDTO (..),
    EventCityDTO (..),
    EventCityInputDTO (..),
    EventCitySubscriptionUpdateDTO (..),
    DiscoverySourceDTO (..),
    DiscoverySourceWriteDTO (..),
    EventDTO (..),
    EventFinanceEntryDTO (..),
    EventFinanceSummaryDTO (..),
    EventLogisticsActivityDTO (..),
    EventLogisticsAssignmentDTO (..),
    EventLogisticsMemberDTO (..),
    EventLogisticsPlaceDTO (..),
    EventLogisticsPlanDTO (..),
    EventLogisticsSettingsDTO (..),
    EventRouteVerificationDTO (..),
    EventScheduleIssueDTO (..),
    EventLiveBroadcastCreateDTO (..),
    EventLiveBroadcastDTO (..),
    EventLiveBroadcastEndDTO (..),
    EventLiveBroadcastHeartbeatDTO (..),
    EventMetadataUpdateDTO (..),
    EventMomentCommentCreateDTO (..),
    EventMomentCommentDTO (..),
    EventMomentCreateDTO (..),
    EventMomentDTO (..),
    EventMomentReactionDTO (..),
    EventMomentReactionRequestDTO (..),
    EventSourceDTO (..),
    EventUpdateDTO (..),
    InvitationDTO (..),
    InvitationUpdateDTO (..),
    NullableFieldUpdate (..),
    PaymentSheetParamsDTO (..),
    PromoCodeDTO (..),
    RefundDTO (..),
    RefundRequestDTO (..),
    RejectionReasonDTO (..),
    RsvpCreateDTO (..),
    RsvpDTO (..),
    StripePaymentIntentDTO (..),
    TicketCheckInRequestDTO (..),
    TicketDTO (..),
    TicketOrderDTO (..),
    TicketOrderStatusUpdateDTO (..),
    TicketPurchaseRequestDTO (..),
    TicketPurchaseWithPromoDTO (..),
    TicketTierDTO (..),
    TicketTransferCreateDTO (..),
    TicketTransferDTO (..),
    TicketWithQRDTO (..),
    VenueContactUpdateDTO (..),
    VenueDTO (..),
    VenueUpdateDTO (..),
    WaitlistEntryDTO (..),
    WaitlistJoinDTO (..),
 )
import qualified TDF.Email as Email
import TDF.Models (EntityField (PartyStripeCustomerId), Party (..), PartyId)
import TDF.Models.SocialEventsModels hiding (venueAddress, venueCapacity, venueCity, venueContact, venueCountry, venueCreatedAt, venueName, venueUpdatedAt)
import qualified TDF.Models.SocialEventsModels as SM
import qualified TDF.ModelsExtra as ME
import qualified TDF.SocialEventLifecycle as EventLifecycle
import TDF.ServerRadio (
    resolveRadioTransmissionEnvBase,
    validateRadioTransmissionIngestBase,
    validateRadioTransmissionPublicBase,
    validateRadioTransmissionWhipBase,
 )
import qualified TDF.Services.Stripe as Stripe
import TDF.Services.EventLogisticsRoutes
    ( RouteEstimateInput (..)
    , RouteEstimateResult (..)
    , computeGoogleRoute
    )
import qualified TDF.Trials.Server as TrialsServer (isValidHttpUrl)

type AppM = ReaderT Env Handler

validateSocialEventsFeatureAction :: T.Text -> T.Text -> AuthedUser -> Either ServerError ()
validateSocialEventsFeatureAction featureId action user =
    let moduleNames = map moduleName (Set.toList (auModules user))
        allowed = maybe False
            (\feature -> registryFeatureAllows (auRoles user) moduleNames feature action)
            (findRegistryFeature featureId)
     in if allowed
            then Right ()
            else Left err403{errBody = "This feature action is not permitted"}

validateArtistProfileCreateParty :: AuthedUser -> Maybe T.Text -> Either ServerError T.Text
validateArtistProfileCreateParty user requestedPartyId
    | hasStrictAdminAccess user = Right (fromMaybe authenticatedParty cleanedRequest)
    | maybe True (== authenticatedParty) cleanedRequest = Right authenticatedParty
    | otherwise = Left err403{errBody = "Artist profiles can only be created for the authenticated party"}
  where
    authenticatedParty = renderPartyId user
    cleanedRequest = cleanMaybeText requestedPartyId

validateArtistProfileWriteAccess :: AuthedUser -> Maybe T.Text -> Either ServerError ()
validateArtistProfileWriteAccess user ownerPartyId
    | ownerPartyId == Just (renderPartyId user) || hasStrictAdminAccess user = Right ()
    | otherwise = Left err403{errBody = "Artist profile ownership is required"}

validateAuthenticatedPartyReference :: AuthedUser -> T.Text -> Either ServerError ()
validateAuthenticatedPartyReference user referencedPartyId
    | normalizePositivePartyIdText referencedPartyId == Just (renderPartyId user) = Right ()
    | otherwise = Left err403{errBody = "Followers can only be changed for the authenticated party"}

parseStripePaymentIntentResponse :: Aeson.Value -> Either T.Text (T.Text, T.Text)
parseStripePaymentIntentResponse paymentIntent =
    (,)
        <$> parseStripeRequiredText
            "Could not parse Stripe response"
            (Aeson.withObject "payment_intent" (Aeson..: "id"))
            paymentIntent
        <*> parseStripeRequiredText
            "Could not parse Stripe client secret"
            (Aeson.withObject "payment_intent" (Aeson..: "client_secret"))
            paymentIntent

parseStripeWebhookEventEnvelope :: Aeson.Value -> Either T.Text (T.Text, T.Text)
parseStripeWebhookEventEnvelope payload =
    (,)
        <$> parseStripeRequiredText
            "Invalid webhook payload"
            (Aeson.withObject "event" (Aeson..: "id"))
            payload
        <*> parseStripeRequiredText
            "Invalid webhook event type"
            (Aeson.withObject "event" (Aeson..: "type"))
            payload

{- | Verify the signature against the exact request bytes before decoding JSON.
Re-encoding an Aeson 'Value' changes insignificant whitespace and can make a
legitimate Stripe signature fail.
-}
verifyAndDecodeStripeWebhook ::
    UTCTime ->
    Stripe.StripeConfig ->
    Maybe T.Text ->
    BL.ByteString ->
    Either T.Text Aeson.Value
verifyAndDecodeStripeWebhook now stripeCfg mSignature rawBody = do
    signature <- maybe (Left "Missing Stripe-Signature header") Right mSignature
    unless (Stripe.verifyWebhookSignatureAt now stripeCfg signature (BL.toStrict rawBody)) $
        Left "Invalid webhook signature"
    case Aeson.eitherDecode rawBody of
        Left _ -> Left "Invalid webhook JSON"
        Right payload -> Right payload

parseStripeWebhookPaymentIntentId :: Aeson.Value -> Maybe T.Text
parseStripeWebhookPaymentIntentId =
    parseMaybe $
        Aeson.withObject "event" $ \eventObject -> do
            dataObject <- eventObject Aeson..: "data" :: Parser Object
            paymentIntentObject <- dataObject Aeson..: "object" :: Parser Object
            paymentIntentObject Aeson..: "id"

data StripeTicketPaymentEvidence = StripeTicketPaymentEvidence
    { stpePaymentIntentId :: T.Text
    , stpeStatus :: T.Text
    , stpeAmountReceived :: Int
    , stpeCurrency :: T.Text
    , stpeOrderId :: T.Text
    , stpeEventId :: T.Text
    }
    deriving (Eq, Show)

-- | Decode the immutable ticket binding from the signed PaymentIntent event.
-- Stripe stores the JSON snapshot in @metadata.tdf_context@. A succeeded event
-- without every field is not enough evidence to issue tickets.
parseStripeTicketPaymentEvidence :: Aeson.Value -> Either T.Text StripeTicketPaymentEvidence
parseStripeTicketPaymentEvidence payload = do
    (paymentIntentId, status, amountReceived, currency, contextJson) <-
        maybe (Left "Stripe ticket payment evidence is incomplete") Right $
            parseMaybe
                ( Aeson.withObject "event" $ \eventObject -> do
                    dataObject <- eventObject Aeson..: "data" :: Parser Object
                    paymentIntentObject <- dataObject Aeson..: "object" :: Parser Object
                    (,,,,)
                        <$> paymentIntentObject Aeson..: "id"
                        <*> paymentIntentObject Aeson..: "status"
                        <*> paymentIntentObject Aeson..: "amount_received"
                        <*> paymentIntentObject Aeson..: "currency"
                        <*> (paymentIntentObject Aeson..: "metadata" >>= (Aeson..: "tdf_context"))
                )
                payload
    contextValue <-
        maybe (Left "Stripe ticket payment context is invalid") Right $
            Aeson.decodeStrict' (TE.encodeUtf8 contextJson)
    (orderId, eventId) <-
        maybe (Left "Stripe ticket payment context is incomplete") Right $
            parseMaybe
                ( Aeson.withObject "ticket_payment_context" $ \contextObject ->
                    (,)
                        <$> contextObject Aeson..: "order_id"
                        <*> contextObject Aeson..: "event_id"
                )
                contextValue
    pure
        StripeTicketPaymentEvidence
            { stpePaymentIntentId = paymentIntentId
            , stpeStatus = T.toLower (T.strip status)
            , stpeAmountReceived = amountReceived
            , stpeCurrency = T.toUpper (T.strip currency)
            , stpeOrderId = T.strip orderId
            , stpeEventId = T.strip eventId
            }

validateStripeTicketPaymentEvidence ::
    T.Text ->
    T.Text ->
    T.Text ->
    Int ->
    T.Text ->
    StripeTicketPaymentEvidence ->
    Either T.Text ()
validateStripeTicketPaymentEvidence expectedOrderId expectedEventId expectedPaymentIntentId expectedAmount expectedCurrency evidence
    | stpeStatus evidence /= "succeeded" =
        Left "Stripe ticket PaymentIntent is not succeeded"
    | stpePaymentIntentId evidence /= expectedPaymentIntentId =
        Left "Stripe ticket PaymentIntent does not match the stored order"
    | stpeOrderId evidence /= expectedOrderId =
        Left "Stripe ticket order metadata does not match the stored order"
    | stpeEventId evidence /= expectedEventId =
        Left "Stripe ticket event metadata does not match the stored event"
    | stpeAmountReceived evidence /= expectedAmount =
        Left "Stripe ticket amount does not match the immutable order total"
    | stpeCurrency evidence /= T.toUpper (T.strip expectedCurrency) =
        Left "Stripe ticket currency does not match the immutable order currency"
    | otherwise = Right ()

parseStripeWebhookMarketplaceOrderId :: Aeson.Value -> Maybe T.Text
parseStripeWebhookMarketplaceOrderId payload = do
    contextJson <-
        parseMaybe
            ( Aeson.withObject "event" $ \eventObject -> do
                dataObject <- eventObject Aeson..: "data" :: Parser Object
                paymentIntentObject <- dataObject Aeson..: "object" :: Parser Object
                metadataObject <- paymentIntentObject Aeson..: "metadata" :: Parser Object
                metadataObject Aeson..: "tdf_context"
            )
            payload
    contextValue <- Aeson.decodeStrict' (TE.encodeUtf8 contextJson)
    parseMaybe
        ( Aeson.withObject "tdf_context" $ \contextObject -> do
            purpose <- contextObject Aeson..: "purpose" :: Parser T.Text
            unless (purpose == "marketplace_order") $
                fail "Unexpected Stripe metadata purpose"
            contextObject Aeson..: "marketplace_order_id"
        )
        contextValue

canRecoverMarketplaceStripeOrder :: T.Text -> Maybe T.Text -> Maybe T.Text -> Bool
canRecoverMarketplaceStripeOrder status provider paymentIntentId =
    status == "stripe_pending"
        && provider == Just "stripe"
        && isNothing paymentIntentId

parseStripeRefundResponse :: Aeson.Value -> Either T.Text T.Text
parseStripeRefundResponse =
    parseStripeRequiredText
        "Could not parse Stripe refund response"
        (Aeson.withObject "refund" (Aeson..: "id"))

parseStripeCustomerId :: Aeson.Value -> Either T.Text T.Text
parseStripeCustomerId =
    parseStripeRequiredText
        "Could not parse Stripe customer response"
        (Aeson.withObject "customer" (Aeson..: "id"))

parseStripeEphemeralKeySecret :: Aeson.Value -> Either T.Text T.Text
parseStripeEphemeralKeySecret =
    parseStripeRequiredText
        "Could not parse Stripe ephemeral key response"
        (Aeson.withObject "ephemeral_key" (Aeson..: "secret"))

parseStripeRequiredText ::
    T.Text ->
    (Aeson.Value -> Parser T.Text) ->
    Aeson.Value ->
    Either T.Text T.Text
parseStripeRequiredText errorMessage parser =
    maybe (Left errorMessage) Right . parseMaybe parser

eitherStripeServerError :: Either T.Text a -> AppM a
eitherStripeServerError =
    either
        (\errText -> throwError err500{errBody = textErrBody errText})
        pure

eitherStripeWebhookError :: Either T.Text a -> AppM a
eitherStripeWebhookError =
    either
        (\errText -> throwError err400{errBody = textErrBody errText})
        pure

{- | Look up the Stripe Customer for a Party, creating one (and persisting the
id) if none exists yet. This is the foundation for PaymentSheet's saved-cards
UX: subsequent purchases by the same Party reuse the existing customer so
their saved methods appear automatically.
-}
resolveStripeCustomerForBuyer ::
    Stripe.StripeConfig ->
    PartyId ->
    -- | buyer email passed through to Stripe when creating new
    Maybe T.Text ->
    -- | buyer display name passed through to Stripe when creating new
    Maybe T.Text ->
    AppM T.Text
resolveStripeCustomerForBuyer stripeCfg pid mBuyerEmail mBuyerName = do
    Env{..} <- ask
    mParty <- liftIO $ runSqlPool (get pid) envPool
    party <- maybe (throwError err404{errBody = "Buyer party not found"}) pure mParty
    maybe
        (createAndPersistStripeCustomer envPool party)
        pure
        (partyStripeCustomerId party)
  where
    createAndPersistStripeCustomer envPool party = do
        let emailForCustomer = mBuyerEmail <|> partyPrimaryEmail party
            nameForCustomer = mBuyerName <|> Just (partyDisplayName party)
        result <-
            liftIO $
                runStripeRequestSafely $
                    Stripe.createCustomer stripeCfg emailForCustomer nameForCustomer Nothing
        customerJson <-
            either
                ( \err ->
                    throwError
                        err500
                            { errBody = BL.fromStrict (TE.encodeUtf8 ("Stripe customer error: " <> err))
                            }
                )
                pure
                result
        customerId <- eitherStripeServerError $ parseStripeCustomerId customerJson
        liftIO $
            runSqlPool
                (update pid [PartyStripeCustomerId =. Just customerId])
                envPool
        pure customerId

eitherPromoCodeBadRequest :: Either T.Text a -> AppM a
eitherPromoCodeBadRequest =
    either
        (\errText -> throwError err400{errBody = textErrBody errText})
        pure

textErrBody :: T.Text -> BL.ByteString
textErrBody = BL.fromStrict . TE.encodeUtf8

runStripeRequestSafely :: IO (Either T.Text a) -> IO (Either T.Text a)
runStripeRequestSafely action = do
    result <- try action
    case result of
        Right stripeResult -> pure stripeResult
        Left err ->
            case fromException err :: Maybe SomeAsyncException of
                Just _ -> throwIO (err :: SomeException)
                Nothing -> pure (Left "Stripe request failed")

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

isEventTicketCheckoutConflict :: SomeException -> Bool
isEventTicketCheckoutConflict exception =
    case fromException exception of
        Just sqlErr ->
            sqlState sqlErr == "23505"
                && "unique_event_ticket_checkout"
                    `BS8.isInfixOf` (sqlErrorMsg sqlErr <> " " <> sqlErrorDetail sqlErr)
        Nothing -> False

{- | Public Stripe webhook handler. Stripe authenticates this endpoint with
its signature header, so it intentionally has no 'AuthedUser' dependency.
-}
stripeWebhookServer :: Maybe T.Text -> BL.ByteString -> AppM NoContent
stripeWebhookServer mSignature rawBody = do
    Env{..} <- ask
    now <- liftIO getCurrentTime
    case (stripeSecretKey envConfig, stripeWebhookSecret envConfig) of
        (Just secretKey, Just webhookSecret) -> do
            let stripeCfg =
                    Stripe.StripeConfig
                        { Stripe.stripeSecretKey = secretKey
                        , Stripe.stripeWebhookSecret = webhookSecret
                        , Stripe.stripeApiVersion = Stripe.defaultStripeApiVersion
                        }
            payload <-
                eitherStripeWebhookError $
                    verifyAndDecodeStripeWebhook now stripeCfg mSignature rawBody
            (eventId, eventType) <-
                eitherStripeWebhookError $
                    parseStripeWebhookEventEnvelope payload
            mExisting <- liftIO $ runSqlPool (getBy (UniqueStripeWebhookEvent eventId)) envPool
            case mExisting of
                Just _ -> pure NoContent
                Nothing -> do
                    dispatchResult <- case eventType of
                        "payment_intent.succeeded" ->
                            handleStripePaymentIntentSucceeded now payload
                        -- A failed attempt can be retried on the same intent.
                        "payment_intent.payment_failed" -> pure NoContent
                        "payment_intent.canceled" ->
                            handleStripePaymentIntentCanceled now payload
                        "charge.refunded" -> pure NoContent
                        "checkout.session.completed" ->
                            handleStripeCheckoutSessionCompleted now payload
                        "customer.subscription.updated" ->
                            handleStripeSubscriptionUpdated now payload
                        "customer.subscription.deleted" ->
                            handleStripeSubscriptionDeleted now payload
                        "invoice.paid" -> pure NoContent
                        _ -> pure NoContent
                    _ <-
                        liftIO $
                            runSqlPool
                                ( insertUnique
                                    StripeWebhookEvent
                                        { stripeWebhookEventStripeEventId = eventId
                                        , stripeWebhookEventEventType = eventType
                                        , stripeWebhookEventPayload = TE.decodeUtf8 (BL.toStrict rawBody)
                                        , stripeWebhookEventProcessedAt = now
                                        }
                                )
                                envPool
                    pure dispatchResult
        _ -> throwError err500{errBody = "Stripe is not configured"}

handleStripePaymentIntentSucceeded :: UTCTime -> Aeson.Value -> AppM NoContent
handleStripePaymentIntentSucceeded now payload =
    maybe (pure NoContent) markOrderPaid (parseStripeWebhookPaymentIntentId payload)
  where
    markOrderPaid piId = do
        Env{..} <- ask
        mOrder <-
            liftIO $
                runSqlPool
                    (selectFirst [EventTicketOrderStripePaymentIntentId ==. Just piId] [])
                    envPool
        case mOrder of
            Nothing ->
                markCourseRegistrationStatus
                    now
                    "paid"
                    piId
                    (parseStripeWebhookMarketplaceOrderId payload)
            Just (Entity orderKey order) -> do
                paymentEvidence <-
                    eitherStripeWebhookError (parseStripeTicketPaymentEvidence payload)
                eitherStripeWebhookError $
                    validateStripeTicketPaymentEvidence
                        (renderKeyText orderKey)
                        (renderKeyText (eventTicketOrderEventId order))
                        piId
                        (eventTicketOrderAmountCents order)
                        (eventTicketOrderCurrency order)
                        paymentEvidence
                (statusChanged, ticketCodes) <-
                    liftIO $
                        runSqlPool
                            ( do
                                changedCount <-
                                    updateWhereCount
                                        [ EventTicketOrderId ==. orderKey
                                        , EventTicketOrderStatus ==. "pending"
                                        ]
                                        [ EventTicketOrderStatus =. "paid"
                                        , EventTicketOrderUpdatedAt =. now
                                        ]
                                if changedCount > 0
                                    then do
                                        codes <- issueMissingTicketsForOrder now orderKey order
                                        pure (True, codes)
                                    else pure (False, [])
                            )
                            envPool
                when statusChanged $
                    sendTicketConfirmationForOrder order ticketCodes
                pure NoContent

issueMissingTicketsForOrder ::
    UTCTime ->
    EventTicketOrderId ->
    EventTicketOrder ->
    SqlPersistT IO [T.Text]
issueMissingTicketsForOrder now orderKey order = do
    existingTickets <-
        selectList [EventTicketOrderRefId ==. orderKey] [Asc EventTicketId]
    let missingCount =
            max 0 (eventTicketOrderQuantity order - length existingTickets)
    forM_ [1 .. missingCount] $ \_ -> do
        ticketCodeValue <- generateUniqueTicketCode
        insert_
            EventTicket
                { eventTicketEventId = eventTicketOrderEventId order
                , eventTicketTierRefId = eventTicketOrderTierId order
                , eventTicketOrderRefId = orderKey
                , eventTicketHolderName = eventTicketOrderBuyerName order
                , eventTicketHolderEmail = eventTicketOrderBuyerEmail order
                , eventTicketCode = ticketCodeValue
                , eventTicketStatus = "issued"
                , eventTicketCheckedInAt = Nothing
                , eventTicketCurrentHolderPartyId = eventTicketOrderBuyerPartyId order
                , eventTicketCurrentHolderEmail = eventTicketOrderBuyerEmail order
                , eventTicketCurrentHolderName = eventTicketOrderBuyerName order
                , eventTicketOriginalHolderPartyId = eventTicketOrderBuyerPartyId order
                , eventTicketTransferHistory = Nothing
                , eventTicketCreatedAt = now
                , eventTicketUpdatedAt = now
                }
    allTickets <- selectList [EventTicketOrderRefId ==. orderKey] [Asc EventTicketId]
    pure (map (eventTicketCode . entityVal) allTickets)

sendTicketConfirmationForOrder :: EventTicketOrder -> [T.Text] -> AppM ()
sendTicketConfirmationForOrder order ticketCodes = do
    Env{..} <- ask
    (mEvent, mTier, mParty) <-
        liftIO $
            runSqlPool
                ( do
                    eventRow <- get (eventTicketOrderEventId order)
                    tierRow <- get (eventTicketOrderTierId order)
                    partyRow <- case partyKey of
                        Nothing -> pure Nothing
                        Just key -> get key
                    pure (eventRow, tierRow, partyRow)
                )
                envPool
    case (mEvent, mTier, recipientEmail mParty) of
        (Just eventRow, Just tierRow, Just emailAddress) ->
            liftIO $
                sendTicketConfirmationEmailBestEffort
                    (emailConfig envConfig)
                    (recipientName mParty)
                    emailAddress
                    (socialEventTitle eventRow)
                    (formatTicketEventDate (socialEventStartTime eventRow))
                    (eventTicketOrderQuantity order)
                    (eventTicketTierName tierRow)
                    (formatTicketOrderTotal order)
                    ticketCodes
                    (Just ticketMobileUrl)
        _ -> pure ()
  where
    partyKey =
        eventTicketOrderBuyerPartyId order
            >>= (fromPathPiece :: T.Text -> Maybe PartyId)
    recipientName mParty =
        fromMaybe
            "Invitado TDF"
            ( cleanMaybeText (eventTicketOrderBuyerName order)
                <|> cleanMaybeText (partyDisplayName <$> mParty)
            )
    recipientEmail mParty =
        cleanMaybeText (eventTicketOrderBuyerEmail order)
            <|> cleanMaybeText (mParty >>= partyPrimaryEmail)

sendTicketConfirmationEmailBestEffort ::
    Maybe EmailConfig ->
    T.Text ->
    T.Text ->
    T.Text ->
    T.Text ->
    Int ->
    T.Text ->
    T.Text ->
    [T.Text] ->
    Maybe T.Text ->
    IO ()
sendTicketConfirmationEmailBestEffort
    smtpConfig
    buyerName
    buyerEmail
    eventTitle
    eventDate
    quantity
    tierName
    total
    ticketCodes
    ticketAppUrl = do
        result <-
            ( try
                    ( Email.sendTicketConfirmationEmail
                        smtpConfig
                        buyerName
                        buyerEmail
                        eventTitle
                        eventDate
                        quantity
                        tierName
                        total
                        ticketCodes
                        ticketAppUrl
                    ) ::
                    IO (Either SomeException ())
                )
        case result of
            Left err ->
                hPutStrLn
                    stderr
                    ( "[TicketConfirmation] Email delivery failed after ticket issuance: "
                        <> displayException err
                    )
            Right () -> pure ()

formatTicketEventDate :: UTCTime -> T.Text
formatTicketEventDate =
    T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC"

ticketMobileUrl :: T.Text
ticketMobileUrl = "tdf://tickets"

formatTicketOrderTotal :: EventTicketOrder -> T.Text
formatTicketOrderTotal order =
    T.pack (printf "%.2f" amount) <> " " <> T.toUpper (eventTicketOrderCurrency order)
  where
    amount = fromIntegral (eventTicketOrderAmountCents order) / 100 :: Double

data TicketPlatformFeeBreakdown = TicketPlatformFeeBreakdown
    { ticketFaceValueCents :: Int
    , ticketBuyerPlatformFeeCents :: Int
    , ticketOrganizerPlatformFeeCents :: Int
    , ticketCheckoutTotalCents :: Int
    }

ticketPlatformFeeBps :: Int
-- 4% total is the platform's configurable self-service ticketing baseline.
-- rates while still covering TDF's platform operations. It is split evenly
-- between buyer checkout and organizer proceeds.
ticketPlatformFeeBps = 400

ticketPlatformFeeBreakdown :: Int -> TicketPlatformFeeBreakdown
ticketPlatformFeeBreakdown faceValue =
    let totalFee = max 0 (faceValue * ticketPlatformFeeBps `div` 10000)
        buyerFee = (totalFee + 1) `div` 2
        organizerFee = totalFee - buyerFee
     in TicketPlatformFeeBreakdown
            { ticketFaceValueCents = faceValue
            , ticketBuyerPlatformFeeCents = buyerFee
            , ticketOrganizerPlatformFeeCents = organizerFee
            , ticketCheckoutTotalCents = faceValue + buyerFee
            }

encodeTicketCheckoutMetadata :: Maybe T.Text -> Maybe T.Text -> TicketPlatformFeeBreakdown -> Maybe T.Text
encodeTicketCheckoutMetadata mIdempotencyKey mPromoCode TicketPlatformFeeBreakdown{..} =
    Just . TE.decodeUtf8 . BL.toStrict . Aeson.encode $
        Aeson.object
            [ "checkout_idempotency_key" Aeson..= mIdempotencyKey
            , "promo_code" Aeson..= mPromoCode
            , "face_value_cents" Aeson..= ticketFaceValueCents
            , "buyer_platform_fee_cents" Aeson..= ticketBuyerPlatformFeeCents
            , "organizer_platform_fee_cents" Aeson..= ticketOrganizerPlatformFeeCents
            ]

decodeTicketCheckoutMetadata :: Maybe T.Text -> Maybe (T.Text, Maybe T.Text)
decodeTicketCheckoutMetadata mRawMetadata = do
    rawMetadata <- mRawMetadata
    metadata <- Aeson.decodeStrict' (TE.encodeUtf8 rawMetadata)
    parseMaybe
        ( Aeson.withObject "ticket_checkout_metadata" $ \obj ->
            (,)
                <$> obj Aeson..: "checkout_idempotency_key"
                <*> obj Aeson..:? "promo_code"
        )
        metadata

decodeTicketPlatformFeeBreakdown :: EventTicketOrder -> TicketPlatformFeeBreakdown
decodeTicketPlatformFeeBreakdown order =
    fromMaybe fallback $ do
        rawMetadata <- eventTicketOrderMetadata order
        metadata <- Aeson.decodeStrict' (TE.encodeUtf8 rawMetadata)
        parseMaybe
            ( Aeson.withObject "ticket_fee_metadata" $ \obj ->
                TicketPlatformFeeBreakdown
                    <$> obj Aeson..: "face_value_cents"
                    <*> obj Aeson..: "buyer_platform_fee_cents"
                    <*> obj Aeson..: "organizer_platform_fee_cents"
                    <*> pure (eventTicketOrderAmountCents order)
            )
            metadata
  where
    -- Orders created before the platform-fee rollout retain their historical totals.
    fallback = TicketPlatformFeeBreakdown (eventTicketOrderAmountCents order) 0 0 (eventTicketOrderAmountCents order)

reuseStripeTicketCheckout ::
    Maybe T.Text ->
    EventTicketOrderId ->
    EventTicketOrder ->
    AppM StripePaymentIntentDTO
reuseStripeTicketCheckout mMobileSdkVersion orderKey order = do
    Env{..} <- ask
    stripeCfg <-
        case (stripeSecretKey envConfig, stripeWebhookSecret envConfig) of
            (Just secretKey, Just webhookSecret) ->
                pure
                    Stripe.StripeConfig
                        { Stripe.stripeSecretKey = secretKey
                        , Stripe.stripeWebhookSecret = webhookSecret
                        , Stripe.stripeApiVersion = Stripe.defaultStripeApiVersion
                        }
            _ -> throwError err500{errBody = "Stripe is not configured"}
    paymentIntentId <-
        maybe
            (throwError err409{errBody = "Ticket checkout is still initializing; retry shortly"})
            pure
            (eventTicketOrderStripePaymentIntentId order)
    retrieved <-
        liftIO $
            runStripeRequestSafely $
                Stripe.retrievePaymentIntent stripeCfg paymentIntentId
    paymentIntent <-
        either
            (\err -> throwError err502{errBody = textErrBody ("Stripe retrieval error: " <> err)})
            pure
            retrieved
    (_, clientSecret) <- eitherStripeServerError $ parseStripePaymentIntentResponse paymentIntent
    paymentSheet <- case mMobileSdkVersion of
        Nothing -> pure Nothing
        Just mobileSdkVersion -> do
            publishableKey <-
                maybe
                    (throwError err500{errBody = "Stripe publishable key not configured"})
                    pure
                    (stripePublishableKey envConfig)
            buyerKey <-
                maybe
                    (throwError err500{errBody = "Ticket order buyer is invalid"})
                    pure
                    ( eventTicketOrderBuyerPartyId order
                        >>= (fromPathPiece :: T.Text -> Maybe PartyId)
                    )
            customerId <-
                resolveStripeCustomerForBuyer
                    stripeCfg
                    buyerKey
                    (eventTicketOrderBuyerEmail order)
                    (eventTicketOrderBuyerName order)
            ephemeralResult <-
                liftIO $
                    runStripeRequestSafely $
                        Stripe.createEphemeralKey stripeCfg customerId mobileSdkVersion
            ephemeralSecret <-
                either
                    (\err -> throwError err502{errBody = textErrBody ("Stripe ephemeral key error: " <> err)})
                    (eitherStripeServerError . parseStripeEphemeralKeySecret)
                    ephemeralResult
            pure . Just $
                PaymentSheetParamsDTO
                    { psCustomerId = customerId
                    , psEphemeralKeySecret = ephemeralSecret
                    , psPaymentIntentClientSecret = clientSecret
                    , psPublishableKey = publishableKey
                    }
    pure
        StripePaymentIntentDTO
            { spiClientSecret = clientSecret
            , spiOrderId = renderKeyText orderKey
            , spiAmountCents = eventTicketOrderAmountCents order
            , spiCurrency = eventTicketOrderCurrency order
            , spiPaymentSheet = paymentSheet
            , spiLookupToken = Nothing
            }

handleStripePaymentIntentCanceled :: UTCTime -> Aeson.Value -> AppM NoContent
handleStripePaymentIntentCanceled now payload =
    maybe (pure NoContent) cancelOrder (parseStripeWebhookPaymentIntentId payload)
  where
    cancelOrder piId = do
        Env{..} <- ask
        mOrder <-
            liftIO $
                runSqlPool
                    (selectFirst [EventTicketOrderStripePaymentIntentId ==. Just piId] [])
                    envPool
        case mOrder of
            Nothing ->
                markCourseRegistrationStatus
                    now
                    "cancelled"
                    piId
                    (parseStripeWebhookMarketplaceOrderId payload)
            Just (Entity orderKey order) -> do
                liftIO $
                    runSqlPool
                        ( do
                            changedCount <-
                                updateWhereCount
                                    [ EventTicketOrderId ==. orderKey
                                    , EventTicketOrderStatus ==. "pending"
                                    ]
                                    [ EventTicketOrderStatus =. "cancelled"
                                    , EventTicketOrderUpdatedAt =. now
                                    ]
                            when (changedCount > 0) $ do
                                update
                                    (eventTicketOrderTierId order)
                                    [EventTicketTierQuantitySold +=. (negate (eventTicketOrderQuantity order))]
                                case eventTicketOrderPromoCodeId order of
                                    Nothing -> pure ()
                                    Just promoKey -> update promoKey [PromoCodeCurrentRedemptions +=. (-1)]
                        )
                        envPool
                pure NoContent

{- | Webhook fallback that flips a course registration to @newStatus@ when its
PaymentIntent is the one the webhook fired for. Idempotent: only acts when
the registration is still in @pending_payment@ (Stripe replays events).
Falls through to 'markArtistTipStatus' when no matching course registration
exists, so a single webhook handler can route both course payments and
artist tips.
-}
markCourseRegistrationStatus :: UTCTime -> T.Text -> T.Text -> Maybe T.Text -> AppM NoContent
markCourseRegistrationStatus now newStatus piId mMarketplaceOrderId = do
    Env{..} <- ask
    mReg <-
        liftIO $
            runSqlPool
                (selectFirst [ME.CourseRegistrationStripePaymentIntentId ==. Just piId] [])
                envPool
    case mReg of
        Nothing ->
            markArtistTipStatus
                now
                (translateToArtistTipStatus newStatus)
                piId
                mMarketplaceOrderId
        Just (Entity regKey reg) -> do
            when (ME.courseRegistrationStatus reg == "pending_payment") $
                liftIO $
                    runSqlPool
                        ( update
                            regKey
                            [ ME.CourseRegistrationStatus =. newStatus
                            , ME.CourseRegistrationUpdatedAt =. now
                            ]
                        )
                        envPool
            pure NoContent

{- | Webhook fallback for artist tip PaymentIntents. Same shape as
'markCourseRegistrationStatus' but operates on the @artist_tip@ table.
Idempotent: only acts on tips still in @pending@.
-}
markArtistTipStatus :: UTCTime -> T.Text -> T.Text -> Maybe T.Text -> AppM NoContent
markArtistTipStatus now newStatus piId mMarketplaceOrderId = do
    Env{..} <- ask
    mTip <-
        liftIO $
            runSqlPool
                (selectFirst [ME.ArtistTipStripePaymentIntentId ==. Just piId] [])
                envPool
    case mTip of
        Nothing ->
            markMarketplaceOrderStatus
                now
                (translateToMarketplaceOrderStatus newStatus)
                piId
                mMarketplaceOrderId
        Just (Entity tipKey tip) -> do
            when (ME.artistTipStatus tip == "pending") $
                liftIO $
                    runSqlPool
                        ( update
                            tipKey
                            [ ME.ArtistTipStatus =. newStatus
                            , ME.ArtistTipUpdatedAt =. now
                            ]
                        )
                        envPool
            pure NoContent

{- | Webhook fallback for marketplace Stripe PaymentIntents. Idempotent: only
acts on orders still waiting for Stripe confirmation.
-}
markMarketplaceOrderStatus :: UTCTime -> T.Text -> T.Text -> Maybe T.Text -> AppM NoContent
markMarketplaceOrderStatus now newStatus piId mOrderId = do
    Env{..} <- ask
    mOrder <-
        liftIO $
            runSqlPool
                (selectFirst [ME.MarketplaceOrderStripePaymentIntentId ==. Just piId] [])
                envPool
    case mOrder of
        Just (Entity orderKey order) -> do
            when (ME.marketplaceOrderStatus order `elem` ["stripe_pending", "pending"]) $
                liftIO $
                    runSqlPool
                        ( update
                            orderKey
                            [ ME.MarketplaceOrderStatus =. newStatus
                            , ME.MarketplaceOrderPaymentProvider =. Just "stripe"
                            , ME.MarketplaceOrderStripePaymentIntentId =. Just piId
                            , ME.MarketplaceOrderPaidAt =. if newStatus == "paid" then Just now else ME.marketplaceOrderPaidAt order
                            , ME.MarketplaceOrderUpdatedAt =. now
                            ]
                        )
                        envPool
            pure NoContent
        Nothing -> do
            mOrderByMetadata <-
                case mOrderId >>= fromPathPiece of
                    Nothing -> pure Nothing
                    Just orderKey -> liftIO $ runSqlPool (getEntity orderKey) envPool
            case mOrderByMetadata of
                Just (Entity orderKey order)
                    | canRecoverMarketplaceStripeOrder
                        (ME.marketplaceOrderStatus order)
                        (ME.marketplaceOrderPaymentProvider order)
                        (ME.marketplaceOrderStripePaymentIntentId order) ->
                        liftIO $
                            runSqlPool
                                ( updateWhere
                                    [ ME.MarketplaceOrderId ==. orderKey
                                    , ME.MarketplaceOrderStatus ==. "stripe_pending"
                                    , ME.MarketplaceOrderPaymentProvider ==. Just "stripe"
                                    , ME.MarketplaceOrderStripePaymentIntentId ==. Nothing
                                    ]
                                    [ ME.MarketplaceOrderStatus =. newStatus
                                    , ME.MarketplaceOrderStripePaymentIntentId =. Just piId
                                    , ME.MarketplaceOrderPaidAt
                                        =. if newStatus == "paid"
                                            then Just now
                                            else ME.marketplaceOrderPaidAt order
                                    , ME.MarketplaceOrderUpdatedAt =. now
                                    ]
                                )
                                envPool
                _ -> pure ()
            pure NoContent

{- | The course registration status enum uses @cancelled@ for the failure path;
the artist_tip status enum uses @failed@. This mapping keeps both surfaces
aligned with their own conventions while routing through one webhook.
-}
translateToArtistTipStatus :: T.Text -> T.Text
translateToArtistTipStatus "cancelled" = "failed"
translateToArtistTipStatus s = s

translateToMarketplaceOrderStatus :: T.Text -> T.Text
translateToMarketplaceOrderStatus "cancelled" = "stripe_failed"
translateToMarketplaceOrderStatus "failed" = "stripe_failed"
translateToMarketplaceOrderStatus s = s

{- | Webhook handler for `checkout.session.completed`. Records the
subscription id on the course registration and flips it to @paid@. Looks up
the registration by @metadata.course_registration_id@ on the session, which
our subscription-checkout handler always sets.
-}
handleStripeCheckoutSessionCompleted :: UTCTime -> Aeson.Value -> AppM NoContent
handleStripeCheckoutSessionCompleted now payload =
    maybe (pure NoContent) markRegistrationPaid $
        parseCheckoutSessionCourseSubscription payload
  where
    markRegistrationPaid (regIdNum, subId) = do
        Env{..} <- ask
        let regKey = toSqlKey regIdNum :: ME.CourseRegistrationId
        mReg <- liftIO $ runSqlPool (get regKey) envPool
        maybe
            (pure NoContent)
            ( \reg -> do
                when (ME.courseRegistrationStatus reg == "pending_payment") $
                    liftIO $
                        runSqlPool
                            ( update
                                regKey
                                [ ME.CourseRegistrationStatus =. "paid"
                                , ME.CourseRegistrationStripeSubscriptionId =. Just subId
                                , ME.CourseRegistrationSubscriptionStatus =. Just "active"
                                , ME.CourseRegistrationUpdatedAt =. now
                                ]
                            )
                            envPool
                pure NoContent
            )
            mReg

{- | Parse the course registration id and subscription id from a completed
checkout session webhook payload.
-}
parseCheckoutSessionCourseSubscription :: Aeson.Value -> Maybe (Int64, T.Text)
parseCheckoutSessionCourseSubscription payload = do
    regIdText <-
        parseMaybe
            ( Aeson.withObject "event" $ \evt -> do
                dataObj <- evt Aeson..: "data" :: Parser Object
                obj <- dataObj Aeson..: "object" :: Parser Object
                metadataObj <- obj Aeson..: "metadata" :: Parser Object
                metadataObj Aeson..: "course_registration_id" :: Parser T.Text
            )
            payload
    subId <-
        parseMaybe
            ( Aeson.withObject "event" $ \evt -> do
                dataObj <- evt Aeson..: "data" :: Parser Object
                obj <- dataObj Aeson..: "object" :: Parser Object
                obj Aeson..: "subscription" :: Parser T.Text
            )
            payload
    regIdNum <- readMaybe (T.unpack regIdText)
    pure (regIdNum, subId)

{- | Webhook handler for `customer.subscription.updated`. Mirrors the
subscription status onto the registration. Terminal statuses also flip the
registration status itself so the application treats it as cancelled.
-}
handleStripeSubscriptionUpdated :: UTCTime -> Aeson.Value -> AppM NoContent
handleStripeSubscriptionUpdated now payload =
    maybe (pure NoContent) updateSubscriptionRegistration $
        parseSubscriptionEvent payload
  where
    updateSubscriptionRegistration (subId, subStatus) = do
        Env{..} <- ask
        mReg <-
            liftIO $
                runSqlPool
                    (selectFirst [ME.CourseRegistrationStripeSubscriptionId ==. Just subId] [])
                    envPool
        maybe
            (pure NoContent)
            ( \(Entity regKey _) -> do
                let terminalStatuses = ["canceled", "unpaid", "incomplete_expired"]
                    registrationUpdates =
                        (ME.CourseRegistrationSubscriptionStatus =. Just subStatus)
                            : (ME.CourseRegistrationUpdatedAt =. now)
                            : [ ME.CourseRegistrationStatus =. "cancelled"
                              | subStatus `elem` terminalStatuses
                              ]
                liftIO $ runSqlPool (update regKey registrationUpdates) envPool
                pure NoContent
            )
            mReg

{- | Webhook handler for `customer.subscription.deleted`. Always flips the
registration to @cancelled@.
-}
handleStripeSubscriptionDeleted :: UTCTime -> Aeson.Value -> AppM NoContent
handleStripeSubscriptionDeleted now payload =
    maybe (pure NoContent) cancelSubscriptionRegistration $
        parseSubscriptionEvent payload
  where
    cancelSubscriptionRegistration (subId, _) = do
        Env{..} <- ask
        mReg <-
            liftIO $
                runSqlPool
                    (selectFirst [ME.CourseRegistrationStripeSubscriptionId ==. Just subId] [])
                    envPool
        maybe
            (pure NoContent)
            ( \(Entity regKey _) -> do
                liftIO $
                    runSqlPool
                        ( update
                            regKey
                            [ ME.CourseRegistrationStatus =. "cancelled"
                            , ME.CourseRegistrationSubscriptionStatus =. Just "canceled"
                            , ME.CourseRegistrationUpdatedAt =. now
                            ]
                        )
                        envPool
                pure NoContent
            )
            mReg

-- | Parse (@id@, @status@) from a customer.subscription.* event payload.
parseSubscriptionEvent :: Aeson.Value -> Maybe (T.Text, T.Text)
parseSubscriptionEvent =
    parseMaybe $
        Aeson.withObject "event" $ \evt -> do
            dataObj <- evt Aeson..: "data" :: Parser Object
            obj <- dataObj Aeson..: "object" :: Parser Object
            sid <- obj Aeson..: "id" :: Parser T.Text
            st <- obj Aeson..: "status" :: Parser T.Text
            pure (sid, st)

data TicketCheckInLookup
    = TicketCheckInLookupById Int64
    | TicketCheckInLookupByCode T.Text
    deriving (Show, Eq)

decodeStoredArtistSocialLinks :: Maybe T.Text -> Either T.Text (Maybe ArtistSocialLinksDTO)
decodeStoredArtistSocialLinks Nothing = Right Nothing
decodeStoredArtistSocialLinks (Just raw)
    | T.null (T.strip raw) = Right Nothing
    | otherwise =
        case Aeson.eitherDecodeStrict' (TE.encodeUtf8 raw) of
            Right links -> Right (Just links)
            Left _ -> Left "Stored artist social links are invalid or contain unsupported fields"

encodeSocialLinks :: Maybe ArtistSocialLinksDTO -> Maybe T.Text
encodeSocialLinks mLinks =
    fmap (TE.decodeUtf8 . BL.toStrict . Aeson.encode) mLinks

data EventMetadataDTO = EventMetadataDTO
    { emTicketUrl :: Maybe T.Text
    , emImageUrl :: Maybe T.Text
    , emIsPublic :: Maybe Bool
    , emCurrency :: Maybe T.Text
    , emBudgetCents :: Maybe Int
    }

emptyEventMetadata :: EventMetadataDTO
emptyEventMetadata =
    EventMetadataDTO
        { emTicketUrl = Nothing
        , emImageUrl = Nothing
        , emIsPublic = Nothing
        , emCurrency = Nothing
        , emBudgetCents = Nothing
        }

instance Aeson.ToJSON EventMetadataDTO where
    toJSON EventMetadataDTO{..} =
        Aeson.object
            [ "ticketUrl" Aeson..= emTicketUrl
            , "imageUrl" Aeson..= emImageUrl
            , "isPublic" Aeson..= emIsPublic
            , "currency" Aeson..= emCurrency
            , "budgetCents" Aeson..= emBudgetCents
            ]

instance Aeson.FromJSON EventMetadataDTO where
    parseJSON = Aeson.withObject "EventMetadataDTO" $ \o -> do
        rejectUnknownStoredEventMetadataFields o
        EventMetadataDTO
            <$> o Aeson..:? "ticketUrl"
            <*> o Aeson..:? "imageUrl"
            <*> o Aeson..:? "isPublic"
            <*> o Aeson..:? "currency"
            <*> o Aeson..:? "budgetCents"

rejectUnknownStoredEventMetadataFields :: Object -> Parser ()
rejectUnknownStoredEventMetadataFields obj =
    case filter (`notElem` storedEventMetadataAllowedKeys) providedKeys of
        [] -> pure ()
        unexpected ->
            fail
                ( "Stored event metadata contains unknown fields: "
                    <> T.unpack (T.intercalate ", " unexpected)
                )
  where
    providedKeys = map AesonKey.toText (AesonKeyMap.keys obj)

storedEventMetadataAllowedKeys :: [T.Text]
storedEventMetadataAllowedKeys =
    [ "ticketUrl"
    , "imageUrl"
    , "isPublic"
    , "currency"
    , "budgetCents"
    ]

encodeEventMetadata :: EventMetadataDTO -> Maybe T.Text
encodeEventMetadata EventMetadataDTO{..}
    | isNothing emTicketUrl
        && isNothing emImageUrl
        && isNothing emIsPublic
        && isNothing emCurrency
        && isNothing emBudgetCents =
        Nothing
    | otherwise =
        Just
            ( TE.decodeUtf8 . BL.toStrict . Aeson.encode $
                EventMetadataDTO
                    { emTicketUrl = emTicketUrl
                    , emImageUrl = emImageUrl
                    , emIsPublic = emIsPublic
                    , emCurrency = emCurrency
                    , emBudgetCents = emBudgetCents
                    }
            )

applyNullableTextUpdate :: NullableFieldUpdate T.Text -> Maybe T.Text -> Maybe T.Text
applyNullableTextUpdate field existing =
    case field of
        FieldMissing -> existing
        FieldNull -> Nothing
        FieldValue value -> cleanMaybeText (Just value)

applyNullableBoolUpdate :: NullableFieldUpdate Bool -> Maybe Bool -> Maybe Bool
applyNullableBoolUpdate field existing =
    case field of
        FieldMissing -> existing
        FieldNull -> Nothing
        FieldValue value -> Just value

applyNullableIntUpdate :: (a -> Maybe b) -> NullableFieldUpdate a -> Maybe b -> Maybe b
applyNullableIntUpdate normalizeValue field existing =
    case field of
        FieldMissing -> existing
        FieldNull -> Nothing
        FieldValue value ->
            case normalizeValue value of
                Just normalized -> Just normalized
                Nothing -> existing

applyNullableNormalizedTextUpdate :: (Maybe T.Text -> Maybe T.Text) -> NullableFieldUpdate T.Text -> Maybe T.Text -> Maybe T.Text
applyNullableNormalizedTextUpdate normalizeValue field existing =
    case field of
        FieldMissing -> existing
        FieldNull -> Nothing
        FieldValue value ->
            case normalizeValue (Just value) of
                Just normalized -> Just normalized
                Nothing
                    | T.null (T.strip value) -> Nothing
                    | otherwise -> existing

normalizeNullableTextUpdate ::
    BL.ByteString ->
    (Maybe T.Text -> Maybe T.Text) ->
    NullableFieldUpdate T.Text ->
    Either ServerError (NullableFieldUpdate T.Text)
normalizeNullableTextUpdate _ _ FieldMissing = Right FieldMissing
normalizeNullableTextUpdate _ _ FieldNull = Right FieldNull
normalizeNullableTextUpdate invalidMessage normalizeValue (FieldValue value) =
    case cleanMaybeText (Just value) of
        Nothing -> Right FieldNull
        Just cleaned ->
            case normalizeValue (Just cleaned) of
                Just normalized -> Right (FieldValue normalized)
                Nothing -> Left err400{errBody = invalidMessage}

validateEventMetadataUpdate :: EventMetadataUpdateDTO -> Either ServerError EventMetadataUpdateDTO
validateEventMetadataUpdate EventMetadataUpdateDTO{..} = do
    normalizedTicketUrl <-
        normalizeNullableTextUpdate
            "eventTicketUrl must be an absolute https URL"
            normalizeEventMetadataUrl
            emuTicketUrl
    normalizedImageUrl <-
        normalizeNullableTextUpdate
            "eventImageUrl must be an absolute https URL"
            normalizeEventMetadataUrl
            emuImageUrl
    normalizedCurrency <-
        normalizeNullableTextUpdate
            "eventCurrency must be a 3-letter ISO code"
            normalizeEventCurrencyMaybe
            emuCurrency
    pure
        EventMetadataUpdateDTO
            { emuTicketUrl = normalizedTicketUrl
            , emuImageUrl = normalizedImageUrl
            , emuIsPublic = emuIsPublic
            , emuCurrency = normalizedCurrency
            , emuBudgetCents = emuBudgetCents
            }

validateEventMetadataUrlField :: T.Text -> Maybe T.Text -> Either ServerError (Maybe T.Text)
validateEventMetadataUrlField _ Nothing = Right Nothing
validateEventMetadataUrlField fieldName (Just rawUrl) =
    case cleanMaybeText (Just rawUrl) of
        Nothing -> Right Nothing
        Just urlVal
            | T.length urlVal > maxEventMetadataUrlChars ->
                Left
                    err400
                        { errBody =
                            BL.fromStrict . TE.encodeUtf8 $
                                fieldName
                                    <> " must be "
                                    <> T.pack (show maxEventMetadataUrlChars)
                                    <> " characters or fewer"
                        }
            | Just normalizedUrl <- normalizeEventMetadataUrl (Just urlVal) ->
                Right (Just normalizedUrl)
            | otherwise ->
                Left
                    err400
                        { errBody =
                            BL.fromStrict . TE.encodeUtf8 $
                                fieldName <> " must be an absolute https URL"
                        }

normalizeEventMetadataUrl :: Maybe T.Text -> Maybe T.Text
normalizeEventMetadataUrl rawValue = do
    urlVal <- cleanMaybeText rawValue
    if "https://" `T.isPrefixOf` T.toLower urlVal
        && T.length urlVal <= maxEventMetadataUrlChars
        && TrialsServer.isValidHttpUrl urlVal
        then Just urlVal
        else Nothing

maxEventMetadataUrlChars :: Int
maxEventMetadataUrlChars = 2048

applyEventMetadataUpdate :: EventMetadataUpdateDTO -> EventMetadataDTO -> EventMetadataDTO
applyEventMetadataUpdate EventMetadataUpdateDTO{..} existing =
    EventMetadataDTO
        { emTicketUrl = applyNullableTextUpdate emuTicketUrl (emTicketUrl existing)
        , emImageUrl = applyNullableTextUpdate emuImageUrl (emImageUrl existing)
        , emIsPublic = applyNullableBoolUpdate emuIsPublic (emIsPublic existing)
        , emCurrency = applyNullableNormalizedTextUpdate normalizeCurrencyMaybe emuCurrency (emCurrency existing)
        , emBudgetCents = applyNullableIntUpdate (\value -> normalizeBudgetCentsMaybe (Just value)) emuBudgetCents (emBudgetCents existing)
        }

data VenueContactMetadata = VenueContactMetadata
    { vcmPhone :: Maybe T.Text
    , vcmWebsite :: Maybe T.Text
    , vcmState :: Maybe T.Text
    , vcmZipCode :: Maybe T.Text
    , vcmImageUrl :: Maybe T.Text
    }

emptyVenueContactMetadata :: VenueContactMetadata
emptyVenueContactMetadata =
    VenueContactMetadata
        { vcmPhone = Nothing
        , vcmWebsite = Nothing
        , vcmState = Nothing
        , vcmZipCode = Nothing
        , vcmImageUrl = Nothing
        }

instance Aeson.ToJSON VenueContactMetadata where
    toJSON VenueContactMetadata{..} =
        Aeson.object
            [ "phone" Aeson..= vcmPhone
            , "website" Aeson..= vcmWebsite
            , "state" Aeson..= vcmState
            , "zipCode" Aeson..= vcmZipCode
            , "imageUrl" Aeson..= vcmImageUrl
            ]

instance Aeson.FromJSON VenueContactMetadata where
    parseJSON = Aeson.withObject "VenueContactMetadata" $ \o ->
        VenueContactMetadata
            <$> o Aeson..:? "phone"
            <*> o Aeson..:? "website"
            <*> o Aeson..:? "state"
            <*> o Aeson..:? "zipCode"
            <*> o Aeson..:? "imageUrl"

decodeVenueContactMetadata :: Maybe T.Text -> VenueContactMetadata
decodeVenueContactMetadata mTxt =
    case cleanMaybeText mTxt of
        Nothing -> emptyVenueContactMetadata
        Just raw ->
            case Aeson.decodeStrict (TE.encodeUtf8 raw) of
                Just meta -> meta
                Nothing -> emptyVenueContactMetadata{vcmPhone = Just raw}

encodeVenueContactMetadata :: VenueContactMetadata -> Maybe T.Text
encodeVenueContactMetadata VenueContactMetadata{..}
    | isNothing vcmPhone && isNothing vcmWebsite && isNothing vcmState && isNothing vcmZipCode && isNothing vcmImageUrl = Nothing
    | isJust vcmPhone && isNothing vcmWebsite && isNothing vcmState && isNothing vcmZipCode && isNothing vcmImageUrl = vcmPhone
    | otherwise =
        Just
            ( TE.decodeUtf8 . BL.toStrict . Aeson.encode $
                VenueContactMetadata
                    { vcmPhone = vcmPhone
                    , vcmWebsite = vcmWebsite
                    , vcmState = vcmState
                    , vcmZipCode = vcmZipCode
                    , vcmImageUrl = vcmImageUrl
                    }
            )

venueContactMetadataFromDTO :: VenueDTO -> VenueContactMetadata
venueContactMetadataFromDTO dto =
    let parsedContact = decodeVenueContactMetadata (venueContact dto)
     in VenueContactMetadata
            { vcmPhone = cleanMaybeText (venuePhone dto) <|> vcmPhone parsedContact
            , vcmWebsite = cleanMaybeText (venueWebsite dto) <|> vcmWebsite parsedContact
            , vcmState = cleanMaybeText (venueState dto) <|> vcmState parsedContact
            , vcmZipCode = cleanMaybeText (venueZipCode dto) <|> vcmZipCode parsedContact
            , vcmImageUrl = cleanMaybeText (venueImageUrl dto) <|> vcmImageUrl parsedContact
            }

applyVenueContactUpdate :: VenueContactUpdateDTO -> VenueContactMetadata -> VenueContactMetadata
applyVenueContactUpdate VenueContactUpdateDTO{..} existing =
    VenueContactMetadata
        { vcmPhone = applyNullableTextUpdate vcuPhone (vcmPhone existing)
        , vcmWebsite = applyNullableTextUpdate vcuWebsite (vcmWebsite existing)
        , vcmState = applyNullableTextUpdate vcuState (vcmState existing)
        , vcmZipCode = applyNullableTextUpdate vcuZipCode (vcmZipCode existing)
        , vcmImageUrl = applyNullableTextUpdate vcuImageUrl (vcmImageUrl existing)
        }

validateSocialEventsListOffset :: Maybe Int -> Either ServerError Int
validateSocialEventsListOffset Nothing = Right 0
validateSocialEventsListOffset (Just rawOffset)
    | rawOffset < 0 =
        Left err400{errBody = "offset must be greater than or equal to 0"}
    | rawOffset > maxSocialEventsListOffset =
        Left err400{errBody = "offset must be 10000 or fewer"}
    | otherwise =
        Right rawOffset

maxSocialEventsListOffset :: Int
maxSocialEventsListOffset = 10000

validateSocialEventsListFilter :: T.Text -> Maybe T.Text -> Either ServerError (Maybe T.Text)
validateSocialEventsListFilter _ Nothing = Right Nothing
validateSocialEventsListFilter fieldName (Just rawFilter) =
    let trimmed = T.strip rawFilter
        err message =
            Left
                err400
                    { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " " <> message))
                    }
     in if T.null trimmed
            then Right Nothing
            else
                if T.length trimmed > maxSocialEventsListFilterChars
                    then
                        err
                            ( "must be "
                                <> T.pack (show maxSocialEventsListFilterChars)
                                <> " characters or fewer"
                            )
                    else
                        if T.any isUnsafeSocialEventsListFilterChar rawFilter
                            then err "must not contain control characters or hidden formatting characters"
                            else Right (Just trimmed)

maxSocialEventsListFilterChars :: Int
maxSocialEventsListFilterChars = 120

isUnsafeSocialEventsListFilterChar :: Char -> Bool
isUnsafeSocialEventsListFilterChar ch =
    isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateEventListScope :: Maybe T.Text -> Either ServerError T.Text
validateEventListScope Nothing = Right "all"
validateEventListScope (Just rawScope) =
    case T.toCaseFold (T.strip rawScope) of
        "" -> Right "all"
        "all" -> Right "all"
        "subscribed" -> Right "subscribed"
        _ -> Left err400{errBody = "scope must be one of: all, subscribed"}

validateEventCountryCode :: T.Text -> Either ServerError T.Text
validateEventCountryCode rawCountry =
    let country = T.toUpper (T.strip rawCountry)
     in if T.length country == 2 && T.all isAsciiUpper country
            then Right country
            else Left err400{errBody = "country code must use two ISO-2 letters"}

validateEventCityInputs ::
    [EventCityInputDTO] ->
    Either ServerError [(T.Text, T.Text, T.Text, Maybe T.Text)]
validateEventCityInputs rawCities
    | length rawCities > maxEventCitySubscriptions =
        Left err400{errBody = "A user may subscribe to at most 20 cities"}
    | otherwise =
        fmap reverse (go Set.empty [] rawCities)
  where
    go _ acc [] = Right acc
    go seen acc (EventCityInputDTO{..} : remaining) = do
        let name = T.unwords (T.words (T.strip eventCityInputName))
            normalizedName = normalizeEventCityName name
        whenEither
            ( T.null name
                || T.length name > maxSocialEventsListFilterChars
                || T.any isUnsafeSocialEventsListFilterChar eventCityInputName
            )
            err400{errBody = "city name must be a safe value between 1 and 120 characters"}
        countryCode <- validateEventCountryCode eventCityInputCountryCode
        timeZone <- traverse validateEventTimeZone eventCityInputTimeZone
        let cityKey = (normalizedName, countryCode)
        if Set.member cityKey seen
            then go seen acc remaining
            else
                go
                    (Set.insert cityKey seen)
                    ((name, normalizedName, countryCode, timeZone) : acc)
                    remaining

    whenEither True err = Left err
    whenEither False _ = Right ()

maxEventCitySubscriptions :: Int
maxEventCitySubscriptions = 20

validateEventTimeZone :: T.Text -> Either ServerError T.Text
validateEventTimeZone rawTimeZone =
    let timeZone = T.strip rawTimeZone
        allowed ch = isAlphaNum ch || ch `elem` ("_+-/" :: String)
     in if T.null timeZone
            then Left err400{errBody = "time zone must be omitted instead of blank"}
            else
                if T.length timeZone <= 64 && T.all allowed timeZone
                    then Right timeZone
                    else Left err400{errBody = "time zone must be a valid IANA-style identifier"}

normalizeEventCityName :: T.Text -> T.Text
normalizeEventCityName = T.toCaseFold . T.unwords . T.words . T.strip

loadSubscribedEventCities ::
    ConnectionPool ->
    T.Text ->
    IO [Entity EventCity]
loadSubscribedEventCities pool partyId = do
    subscriptions <-
        runSqlPool
            (selectList [EventCitySubscriptionPartyId ==. partyId] [])
            pool
    let cityKeys = map (eventCitySubscriptionCityId . entityVal) subscriptions
    if null cityKeys
        then pure []
        else runSqlPool (selectList [EventCityId <-. cityKeys] [Asc EventCityName]) pool

eventCityEntityToDTO :: Bool -> Entity EventCity -> EventCityDTO
eventCityEntityToDTO subscribed (Entity cityKey city) =
    EventCityDTO
        { eventCityId = renderKeyText cityKey
        , eventCityName = SM.eventCityName city
        , eventCityCountryCode = SM.eventCityCountryCode city
        , eventCityTimeZone = SM.eventCityTimeZone city
        , eventCitySubscribed = subscribed
        }

type ValidatedDiscoverySourceWrite =
    (T.Text, T.Text, T.Text, Maybe T.Text, Maybe EventCityId, Bool, Int)

validateDiscoverySourceWrite ::
    DiscoverySourceWriteDTO ->
    Either ServerError ValidatedDiscoverySourceWrite
validateDiscoverySourceWrite DiscoverySourceWriteDTO{..} = do
    let sourceKey = T.toLower (T.strip discoverySourceWriteKey)
        sourceName = T.unwords (T.words (T.strip discoverySourceWriteName))
        sourceType = T.toLower (T.strip discoverySourceWriteType)
        feedUrl = cleanMaybeText discoverySourceWriteFeedUrl
    unlessEither
        ( not (T.null sourceKey)
            && T.length sourceKey <= 80
            && T.all (\ch -> isAsciiLower ch || ch `elem` ("0123456789_-" :: String)) sourceKey
        )
        "source key must use lowercase letters, digits, underscores, or hyphens"
    unlessEither
        ( not (T.null sourceName)
            && T.length sourceName <= 160
            && not (T.any isUnsafeSocialEventsListFilterChar discoverySourceWriteName)
        )
        "source name must be a safe value between 1 and 160 characters"
    unlessEither
        (sourceType `elem` ["ticketmaster", "buenplan", "ical", "json", "web"])
        "source type must be one of: ticketmaster, buenplan, ical, json, web"
    unlessEither
        (discoverySourceWritePriority >= 0 && discoverySourceWritePriority <= 10000)
        "source priority must be between 0 and 10000"
    cityKey <-
        traverse
            (fmap toSqlKey . parseInt64Either "event city")
            (cleanMaybeText discoverySourceWriteCityId)
    case sourceType of
        "ticketmaster" -> do
            unlessEither (sourceKey == "ticketmaster") "Ticketmaster must use the ticketmaster source key"
            unlessEither (isNothing feedUrl && isNothing cityKey) "Ticketmaster does not accept a feed URL or city"
        "buenplan" -> do
            unlessEither (sourceKey == "buenplan") "Buen Plan must use the buenplan source key"
            unlessEither (isNothing feedUrl && isNothing cityKey) "Buen Plan does not accept a feed URL or city"
        "web" -> do
            url <-
                maybe
                    (Left err400{errBody = "Web research sources require an HTTPS homepage URL"})
                    Right
                    feedUrl
            unlessEither
                ( T.length url <= 2048
                    && "https://" `T.isPrefixOf` T.toLower url
                    && TrialsServer.isValidHttpUrl url
                )
                "Web research source URL must be a valid HTTPS URL"
            unlessEither (isNothing cityKey) "Web research sources are not tied to one event city"
            unlessEither (not discoverySourceWriteEnabled) "Web research sources must remain disabled for automated cron ingestion"
        _ -> do
            url <-
                maybe
                    (Left err400{errBody = "Venue feeds require an HTTPS feed URL"})
                    Right
                    feedUrl
            unlessEither
                ( T.length url <= 2048
                    && "https://" `T.isPrefixOf` T.toLower url
                    && TrialsServer.isValidHttpUrl url
                )
                "Venue feed URL must be a valid HTTPS URL"
            unlessEither (isJust cityKey) "Venue feeds require an event city"
    pure
        ( sourceKey
        , sourceName
        , sourceType
        , feedUrl
        , cityKey
        , discoverySourceWriteEnabled
        , discoverySourceWritePriority
        )
  where
    unlessEither True _ = Right ()
    unlessEither False message =
        Left err400{errBody = BL.fromStrict (TE.encodeUtf8 message)}

resolveDiscoverySourceCity ::
    ConnectionPool ->
    ValidatedDiscoverySourceWrite ->
    IO (Either ServerError ValidatedDiscoverySourceWrite)
resolveDiscoverySourceCity pool validated@(_, _, _, _, cityKey, _, _) =
    case cityKey of
        Nothing -> pure (Right validated)
        Just key -> do
            city <- runSqlPool (get key) pool
            pure $
                case city of
                    Nothing -> Left err400{errBody = "Configured event city does not exist"}
                    Just _ -> Right validated

discoverySourceEntityToDTO ::
    Entity EventDiscoverySource ->
    SqlPersistT IO DiscoverySourceDTO
discoverySourceEntityToDTO (Entity sourceKey source) = do
    city <- traverse get (eventDiscoverySourceCityId source)
    pure
        DiscoverySourceDTO
            { discoverySourceId = renderKeyText sourceKey
            , discoverySourceKey = eventDiscoverySourceSourceKey source
            , discoverySourceName = eventDiscoverySourceName source
            , discoverySourceType = eventDiscoverySourceSourceType source
            , discoverySourceFeedUrl = eventDiscoverySourceFeedUrl source
            , discoverySourceCityId = renderKeyText <$> eventDiscoverySourceCityId source
            , discoverySourceCityName = SM.eventCityName <$> join city
            , discoverySourceCountryCode = SM.eventCityCountryCode <$> join city
            , discoverySourceEnabled = eventDiscoverySourceEnabled source
            , discoverySourcePriority = eventDiscoverySourcePriority source
            , discoverySourceConsecutiveFailures =
                eventDiscoverySourceConsecutiveFailures source
            , discoverySourceLastSuccessAt = eventDiscoverySourceLastSuccessAt source
            , discoverySourceLastError = eventDiscoverySourceLastError source
            , discoverySourceUpdatedAt = eventDiscoverySourceUpdatedAt source
            }

venueMatchesEventCity :: Venue -> Entity EventCity -> Bool
venueMatchesEventCity venue (Entity _ city) =
    maybe False ((== SM.eventCityNormalizedName city) . normalizeEventCityName) (SM.venueCity venue)
        && countryMatches
  where
    countryMatches =
        case fmap (T.toUpper . T.strip) (SM.venueCountry venue) of
            Nothing -> True
            Just "" -> True
            Just country
                | T.length country == 2 -> country == SM.eventCityCountryCode city
                | otherwise -> True

socialEventsServer :: AuthedUser -> ServerT SocialEventsAPI AppM
socialEventsServer user =
    eventsServer
        :<|> eventCitiesServer
        :<|> eventDiscoverySourcesServer
        :<|> EventResearch.eventResearchServer user
        :<|> venuesServer
        :<|> artistsServer
        :<|> rsvpsServer
        :<|> invitationsServer
        :<|> momentsServer
        :<|> liveBroadcastsServer
        :<|> ticketsServer
        :<|> budgetServer
        :<|> financeServer
        :<|> logisticsServer
  where
    currentPartyId :: T.Text
    currentPartyId = renderPartyId user

    requireFeatureAction :: T.Text -> T.Text -> AppM ()
    requireFeatureAction featureId action =
        either throwError pure (validateSocialEventsFeatureAction featureId action user)

    -- Events
    eventsServer :: ServerT EventsRoutes AppM
    eventsServer =
        listEvents
            :<|> createEvent
            :<|> getEvent
            :<|> updateEvent
            :<|> uploadEventImage
            :<|> deleteEvent

    listEvents :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> AppM [EventDTO]
    listEvents mCity mScope mStartAfter mTypeId mWorkflowStateId mArtistId mVenueId mLimit mOffset = do
        Env{..} <- ask
        limit <- resolveLimit 200 500 mLimit
        offset <- either throwError pure (validateSocialEventsListOffset mOffset)
        scope <- either throwError pure (validateEventListScope mScope)
        eventTypeFilter <- traverse
            (\rawId -> liftIO (runSqlPool (loadKnownEventTypeId rawId) envPool) >>= either throwError pure)
            (cleanMaybeText mTypeId)
        workflowStateFilter <- traverse
            (\rawId -> do
                stateId <- either throwError pure (parseEventWorkflowStateId rawId)
                resolved <- liftIO $ runSqlPool (EventLifecycle.loadActiveSocialEventState stateId) envPool
                maybe (throwError invalidEventWorkflowStateReference) (const (pure stateId)) resolved
            )
            (cleanMaybeText mWorkflowStateId)
        cityFilterText <-
            either throwError pure $
                validateSocialEventsListFilter "city" mCity
        startFilter <- case mStartAfter of
            Nothing -> pure []
            Just raw ->
                case iso8601ParseM (T.unpack raw) of
                    Just t -> pure [SocialEventStartTime >=. t]
                    Nothing -> throwError err400{errBody = "Invalid start_after value (expected ISO-8601 datetime)"}
        cityFilter <- do
            venueRows <- liftIO $ runSqlPool (selectList [] [LimitTo 5000]) envPool
            subscribedCities <-
                if scope == "subscribed"
                    then liftIO (loadSubscribedEventCities envPool currentPartyId)
                    else pure []
            let cityNeedle = T.toCaseFold <$> cityFilterText
                matchesRequestedCity venue =
                    case cityNeedle of
                        Nothing -> True
                        Just needle ->
                            maybe
                                False
                                (T.isInfixOf needle . T.toCaseFold)
                                (SM.venueCity venue)
                requestedVenueIds =
                    [ entityKey venueRow
                    | venueRow@(Entity _ venue) <- venueRows
                    , matchesRequestedCity venue
                    ]
                subscribedVenueIds =
                    [ entityKey venueRow
                    | venueRow@(Entity _ venue) <- venueRows
                    , any (venueMatchesEventCity venue) subscribedCities
                    ]
            subscribedEventIds <-
                if scope /= "subscribed"
                    then pure []
                    else liftIO $ runSqlPool (resolveSubscribedEventIds subscribedCities subscribedVenueIds) envPool
            if (isJust cityFilterText && null requestedVenueIds)
                || (scope == "subscribed" && null subscribedEventIds)
                then pure [SocialEventId ==. toSqlKey 0]
                else
                    pure $
                        (if isJust cityFilterText then [SocialEventVenueId <-. map Just requestedVenueIds] else [])
                            ++ (if scope == "subscribed" then [SocialEventId <-. subscribedEventIds] else [])
        venueFilter <- case fmap T.strip mVenueId of
            Nothing -> pure []
            Just "" -> pure []
            Just raw -> do
                venueKey <- either throwError pure (parseVenueIdEither raw)
                pure [SocialEventVenueId ==. Just venueKey]
        artistFilter <- case fmap T.strip mArtistId of
            Nothing -> pure []
            Just "" -> pure []
            Just raw -> do
                artistKey <- parseArtistId raw
                artistLinks <- liftIO $ runSqlPool (selectList [EventArtistArtistId ==. artistKey] []) envPool
                let eventIds = map (eventArtistEventId . entityVal) artistLinks
                if null eventIds
                    then pure [SocialEventId ==. toSqlKey 0]
                    else pure [SocialEventId <-. eventIds]
        let filters =
                startFilter
                    ++ cityFilter
                    ++ venueFilter
                    ++ artistFilter
                    ++ maybe [] (\eventTypeUuid -> [SocialEventEventTypeId ==. Just eventTypeUuid]) eventTypeFilter
                    ++ maybe [] (\stateUuid -> [SocialEventWorkflowStateId ==. Just stateUuid]) workflowStateFilter
        let dateOrder =
                case mStartAfter of
                    Just _ -> Asc SocialEventStartTime
                    Nothing -> Desc SocialEventStartTime
        rows <- liftIO $ runSqlPool (selectList filters [dateOrder, LimitTo limit, OffsetBy offset]) envPool
        forM rows $ \(Entity eid eventRow) -> do
            artists <- loadEventArtists envPool eid
            sources <- liftIO (loadExternalEventSources envPool eid)
            dto <- liftIO (runSqlPool (eventEntityToDTO (defaultCurrency envConfig) eid eventRow artists) envPool) >>= either throwError pure
            pure dto{eventSources = Just sources}

    resolveSubscribedEventIds ::
        [Entity EventCity] ->
        [VenueId] ->
        SqlPersistT IO [SocialEventId]
    resolveSubscribedEventIds subscribedCities subscribedVenueIds = do
        refs <- selectList [] []
        let referencedEventIds =
                Set.fromList (map (externalEventRefEventId . entityVal) refs)
            importedMatches =
                [ externalEventRefEventId ref
                | Entity _ ref <- refs
                , any (externalRefMatchesCity ref . entityVal) subscribedCities
                ]
        localEvents <-
            if null subscribedVenueIds
                then pure []
                else selectList [SocialEventVenueId <-. map Just subscribedVenueIds] [LimitTo 10000]
        let localMatches =
                [ eventKey
                | Entity eventKey _ <- localEvents
                , Set.notMember eventKey referencedEventIds
                ]
        pure (Set.toList (Set.fromList (importedMatches ++ localMatches)))

    externalRefMatchesCity :: ExternalEventRef -> EventCity -> Bool
    externalRefMatchesCity ref city =
        normalizeEventCityName (externalEventRefCity ref) == SM.eventCityNormalizedName city
            && maybe
                True
                ((== SM.eventCityCountryCode city) . T.toUpper . T.strip)
                (externalEventRefCountryCode ref)

    eventCitiesServer :: ServerT EventCitiesRoutes AppM
    eventCitiesServer =
        listEventCities
            :<|> getCitySubscriptions
            :<|> replaceCitySubscriptions

    eventDiscoverySourcesServer :: ServerT EventDiscoverySourcesRoutes AppM
    eventDiscoverySourcesServer =
        listDiscoverySources
            :<|> createDiscoverySource
            :<|> updateDiscoverySource

    requireDiscoverySourceAdmin :: AppM ()
    requireDiscoverySourceAdmin =
        unless (hasStrictAdminAccess user) $
            throwError err403{errBody = "Strict admin access required"}

    listDiscoverySources :: AppM [DiscoverySourceDTO]
    listDiscoverySources = do
        requireDiscoverySourceAdmin
        Env{..} <- ask
        liftIO $
            runSqlPool
                (do
                    rows <- selectList [] [Desc EventDiscoverySourcePriority, Asc EventDiscoverySourceName]
                    mapM discoverySourceEntityToDTO rows
                )
                envPool

    createDiscoverySource :: DiscoverySourceWriteDTO -> AppM DiscoverySourceDTO
    createDiscoverySource payload = do
        requireDiscoverySourceAdmin
        Env{..} <- ask
        validated <- either throwError pure (validateDiscoverySourceWrite payload)
        resolved <-
            liftIO (resolveDiscoverySourceCity envPool validated)
                >>= either throwError pure
        now <- liftIO getCurrentTime
        result <-
            liftIO $
                runSqlPool
                    (do
                        let (sourceKey, sourceName, sourceType, feedUrl, cityKey, enabled, priority) =
                                resolved
                        existing <- getBy (UniqueEventDiscoverySource sourceKey)
                        case existing of
                            Just _ -> pure (Left err409{errBody = "Event source key already exists"})
                            Nothing -> do
                                let newRow =
                                        EventDiscoverySource
                                            { eventDiscoverySourceSourceKey = sourceKey
                                            , eventDiscoverySourceName = sourceName
                                            , eventDiscoverySourceSourceType = sourceType
                                            , eventDiscoverySourceFeedUrl = feedUrl
                                            , eventDiscoverySourceCityId = cityKey
                                            , eventDiscoverySourceEnabled = enabled
                                            , eventDiscoverySourcePriority = priority
                                            , eventDiscoverySourceConfiguration = Nothing
                                            , eventDiscoverySourceEtag = Nothing
                                            , eventDiscoverySourceLastModified = Nothing
                                            , eventDiscoverySourceConsecutiveFailures = 0
                                            , eventDiscoverySourceLastSuccessAt = Nothing
                                            , eventDiscoverySourceLastError = Nothing
                                            , eventDiscoverySourceCreatedAt = now
                                            , eventDiscoverySourceUpdatedAt = now
                                            }
                                key <- insert newRow
                                Right <$> discoverySourceEntityToDTO (Entity key newRow)
                    )
                    envPool
        either throwError pure result

    updateDiscoverySource :: T.Text -> DiscoverySourceWriteDTO -> AppM DiscoverySourceDTO
    updateDiscoverySource rawSourceId payload = do
        requireDiscoverySourceAdmin
        Env{..} <- ask
        sourceKeyId <-
            either throwError pure $
                fmap toSqlKey (parseInt64Either "event source" rawSourceId)
        validated <- either throwError pure (validateDiscoverySourceWrite payload)
        resolved <-
            liftIO (resolveDiscoverySourceCity envPool validated)
                >>= either throwError pure
        now <- liftIO getCurrentTime
        result <-
            liftIO $
                runSqlPool
                    (do
                        existing <- get sourceKeyId
                        case existing of
                            Nothing -> pure (Left err404)
                            Just existingRow -> do
                                let (sourceKey, sourceName, sourceType, feedUrl, cityKey, enabled, priority) =
                                        resolved
                                if eventDiscoverySourceSourceType existingRow
                                    `elem` ["ticketmaster", "buenplan"]
                                    && ( sourceKey /= eventDiscoverySourceSourceKey existingRow
                                            || sourceType /= eventDiscoverySourceSourceType existingRow
                                       )
                                    then
                                        pure
                                            ( Left
                                                err400
                                                    { errBody =
                                                        "Built-in source identity cannot be changed"
                                                    }
                                            )
                                    else do
                                        conflicting <- getBy (UniqueEventDiscoverySource sourceKey)
                                        case conflicting of
                                            Just (Entity conflictingKey _)
                                                | conflictingKey /= sourceKeyId ->
                                                    pure (Left err409{errBody = "Event source key already exists"})
                                            _ -> do
                                                update
                                                    sourceKeyId
                                                    [ EventDiscoverySourceSourceKey =. sourceKey
                                                    , EventDiscoverySourceName =. sourceName
                                                    , EventDiscoverySourceSourceType =. sourceType
                                                    , EventDiscoverySourceFeedUrl =. feedUrl
                                                    , EventDiscoverySourceCityId =. cityKey
                                                    , EventDiscoverySourceEnabled =. enabled
                                                    , EventDiscoverySourcePriority =. priority
                                                    , EventDiscoverySourceConsecutiveFailures =. 0
                                                    , EventDiscoverySourceLastError =. Nothing
                                                    , EventDiscoverySourceUpdatedAt =. now
                                                    ]
                                                refreshed <- get sourceKeyId
                                                case refreshed of
                                                    Nothing -> pure (Left err404)
                                                    Just row ->
                                                        Right <$> discoverySourceEntityToDTO (Entity sourceKeyId row)
                    )
                    envPool
        either throwError pure result

    listEventCities :: Maybe T.Text -> Maybe T.Text -> AppM [EventCityDTO]
    listEventCities rawQuery rawCountry = do
        Env{..} <- ask
        query <-
            either throwError pure $
                validateSocialEventsListFilter "q" rawQuery
        country <- traverse (either throwError pure . validateEventCountryCode) rawCountry
        liftIO $ do
            cities <- runSqlPool (selectList [] [Asc EventCityName, LimitTo 500]) envPool
            subscribed <- loadSubscribedEventCities envPool currentPartyId
            let subscribedIds = Set.fromList (map entityKey subscribed)
                matchesQuery city =
                    maybe
                        True
                        (\needle -> T.toCaseFold needle `T.isInfixOf` T.toCaseFold (SM.eventCityName city))
                        query
                matchesCountry city =
                    maybe True (== SM.eventCityCountryCode city) country
            pure
                [ eventCityEntityToDTO (Set.member cityKey subscribedIds) cityEntity
                | cityEntity@(Entity cityKey city) <- cities
                , matchesQuery city
                , matchesCountry city
                ]

    getCitySubscriptions :: AppM [EventCityDTO]
    getCitySubscriptions = do
        Env{..} <- ask
        liftIO $ do
            cities <- loadSubscribedEventCities envPool currentPartyId
            pure (map (eventCityEntityToDTO True) cities)

    replaceCitySubscriptions :: EventCitySubscriptionUpdateDTO -> AppM [EventCityDTO]
    replaceCitySubscriptions EventCitySubscriptionUpdateDTO{eventCities = requestedCities} = do
        Env{..} <- ask
        validated <- either throwError pure (validateEventCityInputs requestedCities)
        now <- liftIO getCurrentTime
        liftIO $
            runSqlPool
                (do
                    cityKeys <-
                        forM validated $ \(name, normalizedName, countryCode, timeZone) -> do
                            existing <- getBy (UniqueEventCity normalizedName countryCode)
                            case existing of
                                Just (Entity cityKey _) -> do
                                    update
                                        cityKey
                                        [ EventCityName =. name
                                        , EventCityTimeZone =. timeZone
                                        , EventCityUpdatedAt =. now
                                        ]
                                    pure cityKey
                                Nothing ->
                                    insert
                                        EventCity
                                            { eventCityName = name
                                            , eventCityNormalizedName = normalizedName
                                            , eventCityCountryCode = countryCode
                                            , eventCityTimeZone = timeZone
                                            , eventCityCreatedAt = now
                                            , eventCityUpdatedAt = now
                                            }
                    deleteWhere [EventCitySubscriptionPartyId ==. currentPartyId]
                    forM_ cityKeys $ \cityKey -> do
                        _ <-
                            insertUnique
                                EventCitySubscription
                                    { eventCitySubscriptionPartyId = currentPartyId
                                    , eventCitySubscriptionCityId = cityKey
                                    , eventCitySubscriptionCreatedAt = now
                                    }
                        pure ()
                    rows <- selectList [EventCityId <-. cityKeys] [Asc EventCityName]
                    pure (map (eventCityEntityToDTO True) rows)
                )
                envPool

    loadKnownEventTypeId :: T.Text -> SqlPersistT IO (Either ServerError UUID.UUID)
    loadKnownEventTypeId rawEventTypeId =
        case UUID.fromText (T.strip rawEventTypeId) of
            Nothing -> pure (Left invalidEventTypeReference)
            Just eventTypeUuid ->
                case fromPathPiece (UUID.toText eventTypeUuid) of
                    Nothing -> pure (Left invalidEventTypeReference)
                    Just eventTypeKey -> do
                        mEventType <- getEntity (eventTypeKey :: Catalog.EventTypeId)
                        case mEventType of
                            Nothing -> pure (Left invalidEventTypeReference)
                            Just _ -> pure (Right eventTypeUuid)

    loadSelectableEventTypeId :: UTCTime -> T.Text -> SqlPersistT IO (Either ServerError UUID.UUID)
    loadSelectableEventTypeId now rawEventTypeId = do
        knownResult <- loadKnownEventTypeId rawEventTypeId
        case knownResult of
            Left serverError -> pure (Left serverError)
            Right eventTypeUuid ->
                case fromPathPiece (UUID.toText eventTypeUuid) of
                    Nothing -> pure (Left invalidEventTypeReference)
                    Just eventTypeKey -> do
                        eventType <- getJust (eventTypeKey :: Catalog.EventTypeId)
                        catalog <- getJust (Catalog.eventTypeCatalogId eventType)
                        workflowState <- getJust (Catalog.eventTypeWorkflowStateId eventType)
                        let today = utctDay now
                            effective =
                                maybe True (<= today) (Catalog.eventTypeEffectiveFrom eventType)
                                    && maybe True (>= today) (Catalog.eventTypeEffectiveUntil eventType)
                            selectable =
                                Catalog.eventTypeActive eventType
                                    && Catalog.catalogDefinitionActive catalog
                                    && Catalog.catalogDefinitionCode catalog == "event-types"
                                    && isNothing (Catalog.eventTypeDeprecatedAt eventType)
                                    && Catalog.workflowStateActive workflowState
                                    && Catalog.workflowStateCode workflowState == "published"
                                    && Catalog.workflowStateWorkflowId workflowState
                                        == Catalog.catalogDefinitionWorkflowId catalog
                                    && effective
                        pure (if selectable then Right eventTypeUuid else Left invalidEventTypeReference)

    invalidEventTypeReference :: ServerError
    invalidEventTypeReference =
        err422
            { errBody =
                "eventTypeId must reference an active, effective, published event type"
            }

    invalidEventWorkflowStateReference :: ServerError
    invalidEventWorkflowStateReference =
        err422
            { errBody =
                "eventWorkflowStateId must reference an active state in social-event-lifecycle"
            }

    createEvent :: EventDTO -> AppM EventDTO
    createEvent dto = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        titleVal <- either throwError pure (validateEventTitleInput (eventTitle dto))
        when (eventStart dto >= eventEnd dto) $ throwError err400{errBody = "start time must be before end time"}
        timezoneVal <- traverse (either throwError pure . validateEventTimeZone) (eventTimezone dto)
        either throwError pure $
            validateEventCreateUpdateDimensions
                (eventPriceCents dto)
                (eventCapacity dto)
                (eventBudgetCents dto)
        requestedEventTypeId <-
            maybe (throwError invalidEventTypeReference) pure (cleanMaybeText (eventTypeId dto))
        eventTypeUuid <-
            liftIO (runSqlPool (loadSelectableEventTypeId now requestedEventTypeId) envPool)
                >>= either throwError pure
        validateEventReadOnlyProjectionOmitted dto
        initialWorkflowStateId <- liftIO $ runSqlPool EventLifecycle.resolveInitialSocialEventStateId envPool
        workflowStateId <- case cleanMaybeText (eventWorkflowStateId dto) of
            Nothing -> pure initialWorkflowStateId
            Just rawId -> do
                requestedId <- either throwError pure (parseEventWorkflowStateId rawId)
                activeState <- liftIO $ runSqlPool (EventLifecycle.loadActiveSocialEventState requestedId) envPool
                when (isNothing activeState || requestedId /= initialWorkflowStateId) $
                    throwError invalidEventWorkflowStateReference
                pure requestedId
        currencyVal <- either throwError pure (validateEventCurrencyInput (defaultCurrency envConfig) (eventCurrency dto))
        unless (currencyVal `elem` supportedCurrencies envConfig) $
            throwError err400{errBody = "Currency is not enabled by SUPPORTED_CURRENCIES"}
        ticketUrlVal <-
            either throwError pure $
                validateEventMetadataUrlField "eventTicketUrl" (eventTicketUrl dto)
        imageUrlVal <-
            either throwError pure $
                validateEventMetadataUrlField "eventImageUrl" (eventImageUrl dto)
        artistKeys <- either throwError pure (validateEventArtistIds (eventArtists dto))
        let metadataVal =
                encodeEventMetadata
                    EventMetadataDTO
                        { emTicketUrl = ticketUrlVal
                        , emImageUrl = imageUrlVal
                        , emIsPublic = eventIsPublic dto <|> Just True
                        , emCurrency = Just currencyVal
                        , emBudgetCents = normalizeBudgetCentsMaybe (eventBudgetCents dto)
                        }
        mVenueKey <- case eventVenueId dto of
            Nothing -> pure Nothing
            Just txt -> Just <$> either throwError pure (parseVenueIdEither txt)
        key <-
            liftIO $
                runSqlPool
                    ( insert
                        SocialEvent
                            { socialEventOrganizerPartyId = Just currentPartyId
                            , socialEventTitle = titleVal
                            , socialEventDescription = eventDescription dto
                            , socialEventVenueId = mVenueKey
                            , socialEventTimezone = timezoneVal
                            , socialEventEventTypeId = Just eventTypeUuid
                            , socialEventWorkflowStateId = Just workflowStateId
                            , socialEventStartTime = eventStart dto
                            , socialEventEndTime = eventEnd dto
                            , socialEventPriceCents = eventPriceCents dto
                            , socialEventCurrencyId = Nothing
                            , socialEventCapacity = eventCapacity dto
                            , socialEventMetadata = metadataVal
                            , socialEventCreatedAt = now
                            , socialEventUpdatedAt = now
                            }
                    )
                    envPool
        liftIO $
            runSqlPool
                ( forM_ artistKeys $ \artistKey ->
                    insert_ (EventArtist key artistKey Nothing)
                )
                envPool
        let createdEvent =
                SocialEvent
                    { socialEventOrganizerPartyId = Just currentPartyId
                    , socialEventTitle = titleVal
                    , socialEventDescription = eventDescription dto
                    , socialEventVenueId = mVenueKey
                    , socialEventTimezone = timezoneVal
                    , socialEventEventTypeId = Just eventTypeUuid
                    , socialEventWorkflowStateId = Just workflowStateId
                    , socialEventStartTime = eventStart dto
                    , socialEventEndTime = eventEnd dto
                    , socialEventPriceCents = eventPriceCents dto
                    , socialEventCurrencyId = Nothing
                    , socialEventCapacity = eventCapacity dto
                    , socialEventMetadata = metadataVal
                    , socialEventCreatedAt = now
                    , socialEventUpdatedAt = now
                    }
        liftIO (runSqlPool (eventEntityToDTO (defaultCurrency envConfig) key createdEvent (eventArtists dto)) envPool)
            >>= either throwError pure

    getEvent :: T.Text -> AppM EventDTO
    getEvent rawId = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" rawId
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        case mEvent of
            Nothing -> throwError err404{errBody = "Event not found"}
            Just eventRow -> do
                artists <- loadEventArtists envPool eventKey
                sources <- liftIO (loadExternalEventSources envPool eventKey)
                dto <- liftIO (runSqlPool (eventEntityToDTO (defaultCurrency envConfig) eventKey eventRow artists) envPool) >>= either throwError pure
                pure dto{eventSources = Just sources}

    updateEvent :: T.Text -> EventUpdateDTO -> AppM EventDTO
    updateEvent rawId EventUpdateDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" rawId
        mExisting <- liftIO $ runSqlPool (get eventKey) envPool
        existing <- maybe (throwError err404{errBody = "Event not found"}) pure mExisting
        managedEvent <- claimOrRequireEventManager currentPartyId envPool eventKey existing
        let dto = eudEvent
        validateEventReadOnlyProjectionOmitted dto
        requestedEventTypeId <-
            maybe (throwError invalidEventTypeReference) pure (cleanMaybeText (eventTypeId dto))
        eventTypeUuid <-
            liftIO (runSqlPool (loadSelectableEventTypeId now requestedEventTypeId) envPool)
                >>= either throwError pure
        titleVal <- either throwError pure (validateEventTitleInput (eventTitle dto))
        when (eventStart dto >= eventEnd dto) $ throwError err400{errBody = "start time must be before end time"}
        timezoneVal <- traverse (either throwError pure . validateEventTimeZone) (eventTimezone dto)
        either throwError pure $
            validateEventCreateUpdateDimensions
                (eventPriceCents dto)
                (eventCapacity dto)
                (eventBudgetCents dto)
        validatedMetadataUpdate <- either throwError pure (validateEventMetadataUpdate eudMetadataUpdate)
        artistKeys <- either throwError pure (validateEventArtistIds (eventArtists dto))
        existingMetadata <-
            either (throwError . storedEventMetadataServerError) pure $
                decodeStoredEventMetadata (socialEventMetadata managedEvent)
        let mergedMetadata = applyEventMetadataUpdate validatedMetadataUpdate existingMetadata
        existingWorkflowStateId <-
            maybe (throwError err500{errBody = "Event has no canonical workflow state"}) pure
                (socialEventWorkflowStateId managedEvent)
        workflowStateId <- case eudWorkflowStateIdUpdate of
            FieldMissing -> pure existingWorkflowStateId
            FieldNull -> throwError invalidEventWorkflowStateReference
            FieldValue rawWorkflowStateId -> do
                requestedId <- either throwError pure (parseEventWorkflowStateId rawWorkflowStateId)
                activeState <- liftIO $ runSqlPool (EventLifecycle.loadActiveSocialEventState requestedId) envPool
                when (isNothing activeState) $ throwError invalidEventWorkflowStateReference
                transitionAllowed <- liftIO $ runSqlPool (EventLifecycle.socialEventTransitionAllowed existingWorkflowStateId requestedId) envPool
                unless transitionAllowed $
                    throwError err409{errBody = "The requested social-event workflow transition is not allowed"}
                pure requestedId
        mVenueKey <- case eventVenueId dto of
            Nothing -> pure Nothing
            Just txt -> Just <$> either throwError pure (parseVenueIdEither txt)
        liftIO $
            runSqlPool
                ( update
                    eventKey
                    [ SocialEventTitle =. titleVal
                    , SocialEventDescription =. eventDescription dto
                    , SocialEventVenueId =. mVenueKey
                    , SocialEventTimezone =. timezoneVal
                    , SocialEventStartTime =. eventStart dto
                    , SocialEventEndTime =. eventEnd dto
                    , SocialEventPriceCents =. eventPriceCents dto
                    , SocialEventCapacity =. eventCapacity dto
                    , SocialEventEventTypeId =. Just eventTypeUuid
                    , SocialEventWorkflowStateId =. Just workflowStateId
                    , SocialEventMetadata =. encodeEventMetadata mergedMetadata
                    , SocialEventUpdatedAt =. now
                    ]
                )
                envPool
        liftIO $ runSqlPool (deleteWhere [EventArtistEventId ==. eventKey]) envPool
        liftIO $
            runSqlPool
                ( forM_ artistKeys $ \artistKey ->
                    insert_ (EventArtist eventKey artistKey Nothing)
                )
                envPool
        let updatedEvent =
                managedEvent
                    { socialEventTitle = titleVal
                    , socialEventDescription = eventDescription dto
                    , socialEventVenueId = mVenueKey
                    , socialEventTimezone = timezoneVal
                    , socialEventStartTime = eventStart dto
                    , socialEventEndTime = eventEnd dto
                    , socialEventPriceCents = eventPriceCents dto
                    , socialEventCapacity = eventCapacity dto
                    , socialEventEventTypeId = Just eventTypeUuid
                    , socialEventWorkflowStateId = Just workflowStateId
                    , socialEventMetadata = encodeEventMetadata mergedMetadata
                    , socialEventUpdatedAt = now
                    }
        liftIO (runSqlPool (eventEntityToDTO (defaultCurrency envConfig) eventKey updatedEvent (eventArtists dto)) envPool)
            >>= either throwError pure

    uploadEventImage :: T.Text -> EventImageUploadForm -> AppM EventImageUploadDTO
    uploadEventImage rawId rawUploadForm = do
        EventImageUploadForm{..} <-
            either (throwError . eventImageUploadFormServerError) pure $
                validateEventImageUploadForm rawUploadForm
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, eventRow) <- requireManagedEvent rawId
        let mimeTypeVal = T.toLower (T.strip (fdFileCType eiuFile))
            fallbackName = nonEmptyText (fdFileName eiuFile)
            requestedName = eiuName >>= nonEmptyText
            nameWithExt = applyUploadExtension (requestedName <|> fallbackName) fallbackName
            safeName = sanitizeUploadFileName nameWithExt
        unless (isImageUpload mimeTypeVal safeName) $
            throwError
                err400
                    { errBody =
                        "Only raster image uploads with matching MIME type and extension are allowed"
                    }

        uuid <- liftIO UUIDV4.nextRandom
        existingMeta <-
            either (throwError . storedEventMetadataServerError) pure $
                decodeStoredEventMetadata (socialEventMetadata eventRow)
        let eventIdTxt = renderKeyText eventKey
            storedName = UUID.toText uuid <> "-" <> safeName
            relPath = T.intercalate "/" ["social-events", "events", eventIdTxt, storedName]
            targetDir = assetsRootDir envConfig </> "social-events" </> "events" </> T.unpack eventIdTxt
            targetPath = targetDir </> T.unpack storedName
            assetsBase = resolveConfiguredAssetsBase envConfig
            publicUrl = buildUploadAssetUrl assetsBase relPath
            updatedMeta = existingMeta{emImageUrl = Just publicUrl}
        fileSize <- liftIO (getFileSize (fdPayload eiuFile))
        either throwError pure (validateEventImageUploadSize fileSize)
        liftIO $ createDirectoryIfMissing True targetDir
        liftIO $ copyFile (fdPayload eiuFile) targetPath
        liftIO $
            runSqlPool
                ( update
                    eventKey
                    [ SocialEventMetadata =. encodeEventMetadata updatedMeta
                    , SocialEventUpdatedAt =. now
                    ]
                )
                envPool
        pure
            EventImageUploadDTO
                { eiuEventId = eventIdTxt
                , eiuFileName = storedName
                , eiuPath = relPath
                , eiuPublicUrl = publicUrl
                , eiuImageUrl = publicUrl
                }

    deleteEvent :: T.Text -> AppM NoContent
    deleteEvent rawId = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" rawId
        mExisting <- liftIO $ runSqlPool (get eventKey) envPool
        existing <- maybe (throwError err404{errBody = "Event not found"}) pure mExisting
        _ <- claimOrRequireEventManager currentPartyId envPool eventKey existing
        liftIO $
            runSqlPool
                ( do
                    deleteWhere [EventArtistEventId ==. eventKey]
                    deleteWhere [EventRsvpEventId ==. eventKey]
                    deleteWhere [EventInvitationEventId ==. eventKey]
                    momentKeys <- selectKeysList [EventMomentEventId ==. eventKey] []
                    unless (null momentKeys) $ do
                        deleteWhere [EventMomentReactionMomentId <-. momentKeys]
                        deleteWhere [EventMomentCommentMomentId <-. momentKeys]
                    deleteWhere [EventMomentEventId ==. eventKey]
                    deleteWhere [EventTicketEventId ==. eventKey]
                    deleteWhere [EventTicketOrderEventId ==. eventKey]
                    deleteWhere [EventTicketTierEventId ==. eventKey]
                    deleteWhere [EventFinanceEntryEventId ==. eventKey]
                    deleteWhere [EventBudgetLineEventId ==. eventKey]
                    logisticsActivityKeys <- selectKeysList [EventLogisticsActivityEventId ==. eventKey] []
                    unless (null logisticsActivityKeys) $ do
                        deleteWhere [EventLogisticsAlertDeliveryActivityId <-. logisticsActivityKeys]
                        deleteWhere [EventRouteVerificationActivityId <-. logisticsActivityKeys]
                        deleteWhere [EventLogisticsAssignmentActivityId <-. logisticsActivityKeys]
                        deleteWhere
                            [ FilterOr
                                [ EventLogisticsDependencyActivityId <-. logisticsActivityKeys
                                , EventLogisticsDependencyDependsOnActivityId <-. logisticsActivityKeys
                                ]
                            ]
                    deleteWhere [EventLogisticsActivityEventId ==. eventKey]
                    deleteWhere [EventLogisticsPlaceEventId ==. eventKey]
                    deleteWhere [EventLogisticsMemberEventId ==. eventKey]
                    deleteWhere [EventLogisticsPlanEventId ==. eventKey]
                    deleteWhere [ExternalEventRefEventId ==. eventKey]
                    delete eventKey
                )
                envPool
        pure NoContent

    -- Venues
    venuesServer :: ServerT VenuesRoutes AppM
    venuesServer =
        listVenues
            :<|> createVenue
            :<|> getVenue
            :<|> updateVenue

    listVenues :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> AppM [VenueDTO]
    listVenues mCity mNear mQuery mLimit mOffset = do
        Env{..} <- ask
        limit <- resolveLimit 200 500 mLimit
        offset <- either throwError pure (validateSocialEventsListOffset mOffset)
        cityFilterText <-
            either throwError pure $
                validateSocialEventsListFilter "city" mCity
        searchNeedle <-
            either throwError pure $
                fmap T.toCaseFold <$> validateSocialEventsListFilter "q" mQuery
        nearFilter <- case mNear of
            Nothing -> pure Nothing
            Just raw ->
                case parseNearQueryEither raw of
                    Left e -> throwError e
                    Right parsed -> pure (Just parsed)
        let filters = case cityFilterText of
                Just c -> [VenueCity ==. Just c]
                _ -> []
        let hasTextQuery = maybe False (not . T.null) searchNeedle
            hasNearQuery = isJust nearFilter
            needsInMemoryFilter = hasTextQuery || hasNearQuery
        seeded <-
            if needsInMemoryFilter
                then liftIO $ runSqlPool (selectList filters [Asc VenueName, LimitTo 2000]) envPool
                else liftIO $ runSqlPool (selectList filters [Asc VenueName, LimitTo limit, OffsetBy offset]) envPool
        let matchesText q (Entity _ v) =
                let nameVal = T.toCaseFold (SM.venueName v)
                    cityVal = maybe "" T.toCaseFold (SM.venueCity v)
                    addressVal = maybe "" T.toCaseFold (SM.venueAddress v)
                 in T.isInfixOf q nameVal || T.isInfixOf q cityVal || T.isInfixOf q addressVal
            matchesNear (lat, lng, radiusKm) (Entity _ v) =
                case (venueLatitude v, venueLongitude v) of
                    (Just venueLat, Just venueLng) ->
                        haversineDistanceKm lat lng venueLat venueLng <= radiusKm
                    _ -> False
            rowsFilteredByText = case searchNeedle of
                Just q | not (T.null q) -> filter (matchesText q) seeded
                _ -> seeded
            rowsFiltered = case nearFilter of
                Just nearSpec -> filter (matchesNear nearSpec) rowsFilteredByText
                Nothing -> rowsFilteredByText
            rows =
                if needsInMemoryFilter
                    then take limit (drop offset rowsFiltered)
                    else seeded
        pure $
            map
                ( \(Entity vid v) ->
                    let contactMeta = decodeVenueContactMetadata (SM.venueContact v)
                     in VenueDTO
                            { venueId = Just (renderKeyText vid)
                            , venueName = SM.venueName v
                            , venueAddress = SM.venueAddress v
                            , venueCity = SM.venueCity v
                            , venueCountry = SM.venueCountry v
                            , venueLat = venueLatitude v
                            , venueLng = venueLongitude v
                            , venueCapacity = SM.venueCapacity v
                            , venueContact = vcmPhone contactMeta
                            , venuePhone = vcmPhone contactMeta
                            , venueWebsite = vcmWebsite contactMeta
                            , venueState = vcmState contactMeta
                            , venueZipCode = vcmZipCode contactMeta
                            , venueImageUrl = vcmImageUrl contactMeta
                            , venueCreatedAt = Just (SM.venueCreatedAt v)
                            , venueUpdatedAt = Just (SM.venueUpdatedAt v)
                            }
                )
                rows

    createVenue :: VenueDTO -> AppM VenueDTO
    createVenue dto = do
        requireFeatureAction "social.venue.create" "create"
        Env{..} <- ask
        now <- liftIO getCurrentTime
        either throwError pure $
            validateVenueCreateUpdateFields
                (venueName dto)
                (venueLat dto)
                (venueLng dto)
                (venueCapacity dto)
        let contactMeta = venueContactMetadataFromDTO dto
        key <-
            liftIO $
                runSqlPool
                    ( insert
                        Venue
                            { venueName = venueName dto
                            , venueAddress = venueAddress dto
                            , venueCity = venueCity dto
                            , venueCountry = venueCountry dto
                            , venueCountryCode = Nothing
                            , venueCountryId = Nothing
                            , venueCityId = Nothing
                            , venueTimezone = Nothing
                            , venueLatitude = venueLat dto
                            , venueLongitude = venueLng dto
                            , venueCapacity = venueCapacity dto
                            , venueContact = encodeVenueContactMetadata contactMeta
                            , venueCreatedAt = now
                            , venueUpdatedAt = now
                            }
                    )
                    envPool
        pure
            ( dto
                { venueId = Just (renderKeyText key)
                , venueContact = vcmPhone contactMeta
                , venuePhone = vcmPhone contactMeta
                , venueWebsite = vcmWebsite contactMeta
                , venueState = vcmState contactMeta
                , venueZipCode = vcmZipCode contactMeta
                , venueImageUrl = vcmImageUrl contactMeta
                , venueCreatedAt = Just now
                , venueUpdatedAt = Just now
                }
            )

    getVenue :: T.Text -> AppM VenueDTO
    getVenue rawId = do
        Env{..} <- ask
        venueKey <- parseKeyOr400 "venue" rawId
        mEnt <- liftIO $ runSqlPool (get venueKey) envPool
        case mEnt of
            Nothing -> throwError err404{errBody = "Venue not found"}
            Just v ->
                let contactMeta = decodeVenueContactMetadata (SM.venueContact v)
                 in pure
                        VenueDTO
                            { venueId = Just (T.strip rawId)
                            , venueName = SM.venueName v
                            , venueAddress = SM.venueAddress v
                            , venueCity = SM.venueCity v
                            , venueCountry = SM.venueCountry v
                            , venueLat = venueLatitude v
                            , venueLng = venueLongitude v
                            , venueCapacity = SM.venueCapacity v
                            , venueContact = vcmPhone contactMeta
                            , venuePhone = vcmPhone contactMeta
                            , venueWebsite = vcmWebsite contactMeta
                            , venueState = vcmState contactMeta
                            , venueZipCode = vcmZipCode contactMeta
                            , venueImageUrl = vcmImageUrl contactMeta
                            , venueCreatedAt = Just (SM.venueCreatedAt v)
                            , venueUpdatedAt = Just (SM.venueUpdatedAt v)
                            }

    updateVenue :: T.Text -> VenueUpdateDTO -> AppM VenueDTO
    updateVenue rawId VenueUpdateDTO{..} = do
        requireFeatureAction "social.venues" "edit"
        Env{..} <- ask
        now <- liftIO getCurrentTime
        venueKey <- parseKeyOr400 "venue" rawId
        mExisting <- liftIO $ runSqlPool (get venueKey) envPool
        existing <- maybe (throwError err404{errBody = "Venue not found"}) pure mExisting
        let dto = vudVenue
        either throwError pure $
            validateVenueCreateUpdateFields
                (venueName dto)
                (venueLat dto)
                (venueLng dto)
                (venueCapacity dto)
        let existingContactMeta = decodeVenueContactMetadata (SM.venueContact existing)
            mergedContactMeta = applyVenueContactUpdate vudContactUpdate existingContactMeta
        liftIO $
            runSqlPool
                ( update
                    venueKey
                    [ VenueName =. venueName dto
                    , VenueAddress =. venueAddress dto
                    , VenueCity =. venueCity dto
                    , VenueCountry =. venueCountry dto
                    , VenueLatitude =. venueLat dto
                    , VenueLongitude =. venueLng dto
                    , VenueCapacity =. venueCapacity dto
                    , VenueContact =. encodeVenueContactMetadata mergedContactMeta
                    , VenueUpdatedAt =. now
                    ]
                )
                envPool
        pure
            ( dto
                { venueId = Just rawId
                , venueContact = vcmPhone mergedContactMeta
                , venuePhone = vcmPhone mergedContactMeta
                , venueWebsite = vcmWebsite mergedContactMeta
                , venueState = vcmState mergedContactMeta
                , venueZipCode = vcmZipCode mergedContactMeta
                , venueImageUrl = vcmImageUrl mergedContactMeta
                , venueCreatedAt = Just (SM.venueCreatedAt existing)
                , venueUpdatedAt = Just now
                }
            )

    -- Artists
    artistsServer :: ServerT ArtistsRoutes AppM
    artistsServer =
        listArtists
            :<|> createArtist
            :<|> getArtist
            :<|> updateArtist
            :<|> listArtistFollowers
            :<|> followArtist
            :<|> unfollowArtist

    listArtists :: Maybe T.Text -> Maybe UUID.UUID -> Maybe T.Text -> Maybe Int -> Maybe Int -> AppM [ArtistDTO]
    listArtists mNameFilter mGenreIdFilter mLegacyGenreFilter mLimit mOffset = do
        Env{..} <- ask
        when (isJust mLegacyGenreFilter) $
            throwError err400{errBody = "genre is obsolete; use the canonical genreId UUID parameter"}
        limit <- resolveLimit 500 1000 mLimit
        offset <- either throwError pure (validateSocialEventsListOffset mOffset)
        nameFilter <-
            either throwError pure $
                fmap T.toCaseFold <$> validateSocialEventsListFilter "name" mNameFilter
        let hasFilter = isJust nameFilter || isJust mGenreIdFilter
        rows <-
            liftIO $
                runSqlPool
                    ( selectList
                        []
                        ( [Desc ArtistProfileCreatedAt]
                            ++ if hasFilter
                                then [LimitTo 1000]
                                else [LimitTo limit, OffsetBy offset]
                        )
                    )
                    envPool
        artists <- forM rows $ \(Entity aid a) -> do
            (genreList, genreIds) <- liftIO $ runSqlPool (loadArtistGenreSelections aid a) envPool
            let nameMatches = case nameFilter of
                    Nothing -> True
                    Just name -> T.isInfixOf name (T.toCaseFold (artistProfileName a))
            let genreMatches = case mGenreIdFilter of
                    Nothing -> True
                    Just genreId -> genreId `elem` genreIds
            if nameMatches && genreMatches
                then Just <$> either throwError pure (artistProfileToDTO aid a genreList genreIds)
                else pure Nothing
        let filtered = catMaybes artists
        pure
            ( if hasFilter
                then take limit (drop offset filtered)
                else filtered
            )

    resolveLimit :: Int -> Int -> Maybe Int -> AppM Int
    resolveLimit defaultLimit maxLimit mVal =
        case mVal of
            Nothing -> pure defaultLimit
            Just n
                | n <= 0 -> throwError err400{errBody = "limit must be greater than 0"}
                | n > maxLimit -> throwError err400{errBody = "limit exceeds allowed maximum"}
                | otherwise -> pure n

    listArtistFollowers :: T.Text -> AppM [ArtistFollowerDTO]
    listArtistFollowers artistIdStr = do
        Env{..} <- ask
        artistKey <- parseArtistId artistIdStr
        mArtist <- liftIO $ runSqlPool (get artistKey) envPool
        when (isNothing mArtist) $ throwError err404{errBody = "Artist not found"}
        rows <-
            liftIO $
                runSqlPool
                    (selectList [ArtistFollowArtistId ==. artistKey] [Desc ArtistFollowCreatedAt])
                    envPool
        let artistIdTxt = renderKeyText artistKey
        pure $
            map
                ( \(Entity _ follow) ->
                    ArtistFollowerDTO
                        { afFollowId = Just (renderFollowId artistKey (artistFollowFollowerPartyId follow))
                        , afArtistId = Just artistIdTxt
                        , afFollowerPartyId = artistFollowFollowerPartyId follow
                        , afCreatedAt = Just (artistFollowCreatedAt follow)
                        }
                )
                rows

    createArtist :: ArtistDTO -> AppM ArtistDTO
    createArtist dto = do
        requireFeatureAction "artist.onboarding" "create"
        Env{..} <- ask
        now <- liftIO getCurrentTime
        artistNameVal <- either throwError pure (validateArtistName (artistName dto))
        let requestedPartyId = cleanMaybeText (artistPartyId dto)
        targetPartyId <- either throwError pure (validateArtistProfileCreateParty user requestedPartyId)
        duplicate <- liftIO $ runSqlPool (selectFirst [ArtistProfilePartyId ==. Just targetPartyId] []) envPool
        when (isJust duplicate) $
            throwError err409{errBody = "An artist profile already exists for this party"}
        resolvedGenres <-
            liftIO (runSqlPool (resolvePublishedArtistGenres (artistGenreIds dto)) envPool)
                >>= either (throwError . invalidArtistGenreIdsError) pure
        key <- liftIO $ runSqlPool
            ( do
                artistKey <- insert
                        ArtistProfile
                            { artistProfilePartyId = Just targetPartyId
                            , artistProfileName = artistNameVal
                            , artistProfileBio = artistBio dto
                            , artistProfileAvatarUrl = artistAvatarUrl dto
                            , -- Keep this nullable for compatibility with deployments where the
                              -- legacy column type is TEXT instead of TEXT[].
                              artistProfileGenres = Nothing
                            , artistProfileSocialLinks = encodeSocialLinks (artistSocialLinks dto)
                            , artistProfileCountryCode = Nothing
                            , artistProfileCountryId = Nothing
                            , artistProfileCreatedAt = now
                            , artistProfileUpdatedAt = now
                            }
                forM_ (zip [0 :: Int ..] resolvedGenres) $ \(position, (genreId, _)) ->
                    insert_ ArtistGenreMembership
                        { artistGenreMembershipArtistId = artistKey
                        , artistGenreMembershipGenreId = genreId
                        , artistGenreMembershipSortOrder = position
                        , artistGenreMembershipCreatedAt = now
                        }
                pure artistKey
            ) envPool
        let genreIds = map fst resolvedGenres
            genreList = map snd resolvedGenres
        pure
            ArtistDTO
                { artistId = Just (renderKeyText key)
                , artistPartyId = Just targetPartyId
                , artistName = artistNameVal
                , artistGenres = genreList
                , artistGenreIds = genreIds
                , artistBio = artistBio dto
                , artistAvatarUrl = artistAvatarUrl dto
                , artistSocialLinks = artistSocialLinks dto
                , artistCreatedAt = Just now
                , artistUpdatedAt = Just now
                }

    getArtist :: T.Text -> AppM ArtistDTO
    getArtist idStr = do
        Env{..} <- ask
        artistKey <- parseArtistId idStr
        mArtist <- liftIO $ runSqlPool (get artistKey) envPool
        case mArtist of
            Nothing -> throwError err404{errBody = "Artist not found"}
            Just a -> do
                (genreList, genreIds) <- liftIO $ runSqlPool (loadArtistGenreSelections artistKey a) envPool
                either throwError pure (artistProfileToDTO artistKey a genreList genreIds)

    updateArtist :: T.Text -> ArtistDTO -> AppM ArtistDTO
    updateArtist idStr dto = do
        requireFeatureAction "artist.profile.edit" "edit"
        Env{..} <- ask
        artistKey <- parseArtistId idStr
        now <- liftIO getCurrentTime
        artistNameVal <- either throwError pure (validateArtistName (artistName dto))
        mExisting <- liftIO $ runSqlPool (get artistKey) envPool
        existing <- maybe (throwError err404{errBody = "Artist not found"}) pure mExisting
        either throwError pure (validateArtistProfileWriteAccess user (artistProfilePartyId existing))
        importedRef <-
            liftIO $
                runSqlPool
                    (selectFirst [ExternalArtistRefArtistId ==. artistKey] [])
                    envPool
        when (isJust importedRef) $
            throwError err403{errBody = "Imported artists are managed automatically"}
        resolvedGenres <-
            liftIO (runSqlPool (resolvePublishedArtistGenres (artistGenreIds dto)) envPool)
                >>= either (throwError . invalidArtistGenreIdsError) pure
        let nextPartyId = cleanMaybeText (artistPartyId dto) <|> artistProfilePartyId existing
        liftIO $ runSqlPool
            ( do
                update
                    artistKey
                    [ ArtistProfilePartyId =. nextPartyId
                    , ArtistProfileName =. artistNameVal
                    , ArtistProfileBio =. artistBio dto
                    , ArtistProfileAvatarUrl =. artistAvatarUrl dto
                    , ArtistProfileSocialLinks =. encodeSocialLinks (artistSocialLinks dto)
                    , ArtistProfileUpdatedAt =. now
                    ]
                deleteWhere [ArtistGenreMembershipArtistId ==. artistKey]
                forM_ (zip [0 :: Int ..] resolvedGenres) $ \(position, (genreId, _)) ->
                    insert_ ArtistGenreMembership
                        { artistGenreMembershipArtistId = artistKey
                        , artistGenreMembershipGenreId = genreId
                        , artistGenreMembershipSortOrder = position
                        , artistGenreMembershipCreatedAt = now
                        }
            ) envPool
        let genreIds = map fst resolvedGenres
            genreList = map snd resolvedGenres
        pure
            dto
                { artistId = Just (T.strip idStr)
                , artistName = artistNameVal
                , artistGenres = genreList
                , artistGenreIds = genreIds
                , artistPartyId = nextPartyId
                , artistCreatedAt = Just (artistProfileCreatedAt existing)
                , artistUpdatedAt = Just now
                }

    followArtist :: T.Text -> ArtistFollowRequest -> AppM ArtistFollowerDTO
    followArtist artistIdStr ArtistFollowRequest{..} = do
        Env{..} <- ask
        artistKey <- parseArtistId artistIdStr
        mArtist <- liftIO $ runSqlPool (get artistKey) envPool
        when (isNothing mArtist) $ throwError err404{errBody = "Artist not found"}
        followerParty <-
            liftIO (resolveExistingPartyIdText envPool "followerPartyId" afrFollowerPartyId)
                >>= either throwError pure
        either throwError pure (validateAuthenticatedPartyReference user followerParty)
        liftIO $ followArtistDb envPool artistKey followerParty

    unfollowArtist :: T.Text -> Maybe T.Text -> AppM NoContent
    unfollowArtist artistIdStr mFollower = do
        Env{..} <- ask
        artistKey <- parseArtistId artistIdStr
        mArtist <- liftIO $ runSqlPool (get artistKey) envPool
        when (isNothing mArtist) $ throwError err404{errBody = "Artist not found"}
        followerParty <- either throwError pure (parseFollowerQueryParamEither mFollower)
        either throwError pure (validateAuthenticatedPartyReference user followerParty)
        liftIO $
            runSqlPool
                (deleteWhere [ArtistFollowArtistId ==. artistKey, ArtistFollowFollowerPartyId ==. followerParty])
                envPool
        pure NoContent

    -- RSVPs
    rsvpsServer :: ServerT RsvpRoutes AppM
    rsvpsServer = listRsvps :<|> createRsvp

    listRsvps :: T.Text -> AppM [RsvpDTO]
    listRsvps eventIdStr = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        when (isNothing mEvent) $ throwError err404{errBody = "Event not found"}
        rsvpRows <- liftIO $ runSqlPool (selectList [EventRsvpEventId ==. eventKey] []) envPool
        pure $
            map
                ( \(Entity rid rsvp) ->
                    RsvpDTO
                        { rsvpId = Just (renderKeyText rid)
                        , rsvpEventId = eventIdStr
                        , rsvpPartyId = eventRsvpPartyId rsvp
                        , rsvpStatus = eventRsvpStatus rsvp
                        , rsvpCreatedAt = Just (eventRsvpCreatedAt rsvp)
                        , rsvpUpdatedAt = Just (eventRsvpUpdatedAt rsvp)
                        }
                )
                rsvpRows

    createRsvp :: T.Text -> RsvpCreateDTO -> AppM RsvpDTO
    createRsvp eventIdStr dto = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        let eventIdVal = T.strip eventIdStr
            RsvpCreateDTO partyIdInput statusInput = dto
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        when (isNothing mEvent) $ throwError err404{errBody = "Event not found"}
        statusVal <- either throwError pure (validateRsvpStatus statusInput)
        partyIdVal <-
            liftIO (resolveExistingPartyIdText envPool "rsvpPartyId" partyIdInput)
                >>= either throwError pure

        existingRsvps <-
            liftIO $
                runSqlPool
                    (selectList [EventRsvpEventId ==. eventKey, EventRsvpPartyId ==. partyIdVal] [])
                    envPool

        existingRsvp <- either throwError pure (resolveUniqueRsvpRow existingRsvps)
        case existingRsvp of
            Nothing -> do
                key <-
                    liftIO $
                        runSqlPool
                            ( insert
                                EventRsvp
                                    { eventRsvpEventId = eventKey
                                    , eventRsvpPartyId = partyIdVal
                                    , eventRsvpStatus = statusVal
                                    , eventRsvpMetadata = Nothing
                                    , eventRsvpCreatedAt = now
                                    , eventRsvpUpdatedAt = now
                                    }
                            )
                            envPool
                pure
                    RsvpDTO
                        { rsvpId = Just (renderKeyText key)
                        , rsvpEventId = eventIdVal
                        , rsvpPartyId = partyIdVal
                        , rsvpStatus = statusVal
                        , rsvpCreatedAt = Just now
                        , rsvpUpdatedAt = Just now
                        }
            Just (Entity existingKey existing) -> do
                liftIO $
                    runSqlPool
                        ( update
                            existingKey
                            [ EventRsvpStatus =. statusVal
                            , EventRsvpUpdatedAt =. now
                            ]
                        )
                        envPool
                pure
                    RsvpDTO
                        { rsvpId = Just (renderKeyText existingKey)
                        , rsvpEventId = eventIdVal
                        , rsvpPartyId = partyIdVal
                        , rsvpStatus = statusVal
                        , rsvpCreatedAt = Just (eventRsvpCreatedAt existing)
                        , rsvpUpdatedAt = Just now
                        }

    -- Invitations
    invitationsServer :: ServerT InvitationsRoutes AppM
    invitationsServer eventIdStr =
        listInvitations eventIdStr
            :<|> createInvitation eventIdStr
            :<|> updateInvitation eventIdStr

    momentsServer :: ServerT MomentsRoutes AppM
    momentsServer =
        listMoments
            :<|> createMoment
            :<|> uploadMomentImage
            :<|> reactToMoment
            :<|> commentOnMoment

    liveBroadcastsServer :: ServerT LiveBroadcastsRoutes AppM
    liveBroadcastsServer =
        listLiveBroadcasts
            :<|> createLiveBroadcast
            :<|> heartbeatLiveBroadcast
            :<|> endLiveBroadcast

    listInvitations :: T.Text -> AppM [InvitationDTO]
    listInvitations eventIdStr = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        when (isNothing mEvent) $ throwError err404{errBody = "Event not found"}
        rows <- liftIO $ runSqlPool (selectList [EventInvitationEventId ==. eventKey] [Desc EventInvitationCreatedAt]) envPool
        pure $
            map
                ( \(Entity iid inv) ->
                    InvitationDTO
                        { invitationId = Just (renderKeyText iid)
                        , invitationEventId = Just (T.strip eventIdStr)
                        , invitationFromPartyId = eventInvitationFromPartyId inv
                        , invitationToPartyId = maybe "" id (eventInvitationToPartyId inv)
                        , invitationStatus = eventInvitationStatus inv
                        , invitationMessage = eventInvitationMessage inv
                        , invitationCreatedAt = Just (eventInvitationCreatedAt inv)
                        , invitationUpdatedAt = Just (eventInvitationUpdatedAt inv)
                        }
                )
                rows

    createInvitation :: T.Text -> InvitationDTO -> AppM InvitationDTO
    createInvitation eventIdStr dto = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        when (isNothing mEvent) $ throwError err404{errBody = "Event not found"}
        toParty <- either throwError pure (validateInvitationToPartyId (invitationToPartyId dto))
        fromParty <-
            either
                throwError
                pure
                (validateInvitationFromPartyId currentPartyId (invitationFromPartyId dto))
        statusVal <- either throwError pure (validateInvitationStatusInput (invitationStatus dto))
        key <-
            liftIO $
                runSqlPool
                    ( insert
                        EventInvitation
                            { eventInvitationEventId = eventKey
                            , eventInvitationFromPartyId = Just fromParty
                            , eventInvitationToPartyId = Just toParty
                            , eventInvitationStatus = Just statusVal
                            , eventInvitationMessage = invitationMessage dto
                            , eventInvitationCreatedAt = now
                            , eventInvitationUpdatedAt = now
                            }
                    )
                    envPool
        pure
            InvitationDTO
                { invitationId = Just (renderKeyText key)
                , invitationEventId = Just (T.strip eventIdStr)
                , invitationFromPartyId = Just fromParty
                , invitationToPartyId = toParty
                , invitationStatus = Just statusVal
                , invitationMessage = invitationMessage dto
                , invitationCreatedAt = Just now
                , invitationUpdatedAt = Just now
                }

    updateInvitation :: T.Text -> T.Text -> InvitationUpdateDTO -> AppM InvitationDTO
    updateInvitation eventIdStr invitationIdStr InvitationUpdateDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, invitationKey) <- parseIds eventIdStr invitationIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        when (isNothing mEvent) $ throwError err404{errBody = "Event not found"}
        mExisting <- liftIO $ runSqlPool (get invitationKey) envPool
        case mExisting of
            Nothing -> throwError err404{errBody = "Invitation not found"}
            Just inv -> do
                let dto = iudInvitation
                when (eventInvitationEventId inv /= eventKey) $ throwError err400{errBody = "Invitation does not belong to this event"}
                mStatusVal <- either throwError pure (validateInvitationStatusUpdateInput (invitationStatus dto))
                let messageVal = applyNullableTextUpdate iudMessageUpdate (eventInvitationMessage inv)
                    statusUpdates =
                        maybe [] (\statusVal -> [EventInvitationStatus =. Just statusVal]) mStatusVal
                    responseStatus = mStatusVal <|> eventInvitationStatus inv
                toPartyVal <- either throwError pure (validateInvitationToPartyId (invitationToPartyId dto))
                liftIO $
                    runSqlPool
                        ( update
                            invitationKey
                            ( statusUpdates
                                <> [ EventInvitationMessage =. messageVal
                                   , EventInvitationToPartyId =. Just toPartyVal
                                   , EventInvitationUpdatedAt =. now
                                   ]
                            )
                        )
                        envPool
                pure
                    InvitationDTO
                        { invitationId = Just (renderKeyText invitationKey)
                        , invitationEventId = Just (T.strip eventIdStr)
                        , invitationFromPartyId = eventInvitationFromPartyId inv
                        , invitationToPartyId = toPartyVal
                        , invitationStatus = responseStatus
                        , invitationMessage = messageVal
                        , invitationCreatedAt = Just (eventInvitationCreatedAt inv)
                        , invitationUpdatedAt = Just now
                        }

    -- Moments
    listMoments :: T.Text -> AppM [EventMomentDTO]
    listMoments eventIdStr = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" eventIdStr
        _ <- requireExistingEvent envPool eventKey
        liftIO $ loadEventMoments envPool eventKey

    createMoment :: T.Text -> EventMomentCreateDTO -> AppM EventMomentDTO
    createMoment eventIdStr EventMomentCreateDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        _ <- requireExistingEvent envPool eventKey
        mediaUrl <- maybe (throwError err400{errBody = "Moment media URL is required"}) pure (cleanMaybeText (Just emCreateMediaUrl))
        mediaType <- maybe (throwError err400{errBody = "Moment media type must be image or video"}) pure (normalizeMomentMediaType emCreateMediaType)
        caption <- either throwError pure (normalizeMomentCaption emCreateCaption)
        mediaWidth <-
            either
                throwError
                pure
                (validateMomentMediaDimension "Moment media width" emCreateMediaWidth)
        mediaHeight <-
            either
                throwError
                pure
                (validateMomentMediaDimension "Moment media height" emCreateMediaHeight)
        mediaDurationMs <-
            either
                throwError
                pure
                (validateMomentMediaDuration emCreateMediaDurationMs)
        -- Attribution is derived from the authenticated Party, never from a
        -- client-supplied display name.
        authorName <- liftIO $ loadAuthenticatedPartyDisplayName envPool currentPartyId
        momentKey <-
            liftIO $
                runSqlPool
                    ( insert
                        EventMoment
                            { eventMomentEventId = eventKey
                            , eventMomentAuthorPartyId = Just currentPartyId
                            , eventMomentAuthorName = authorName
                            , eventMomentCaption = caption
                            , eventMomentMediaUrl = mediaUrl
                            , eventMomentMediaType = mediaType
                            , eventMomentMediaWidth = mediaWidth
                            , eventMomentMediaHeight = mediaHeight
                            , eventMomentMediaDurationMs = mediaDurationMs
                            , eventMomentCreatedAt = now
                            , eventMomentUpdatedAt = now
                            }
                    )
                    envPool
        liftIO $ loadMomentDTO envPool momentKey

    uploadMomentImage :: T.Text -> EventImageUploadForm -> AppM EventImageUploadDTO
    uploadMomentImage rawId rawUploadForm = do
        EventImageUploadForm{..} <-
            either (throwError . eventImageUploadFormServerError) pure $
                validateEventImageUploadForm rawUploadForm
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" rawId
        _ <- requireExistingEvent envPool eventKey
        let mimeTypeVal = T.toLower (T.strip (fdFileCType eiuFile))
            fallbackName = nonEmptyText (fdFileName eiuFile)
            requestedName = eiuName >>= nonEmptyText
            nameWithExt = applyUploadExtension (requestedName <|> fallbackName) fallbackName
            safeName = sanitizeUploadFileName nameWithExt
        unless (isImageUpload mimeTypeVal safeName) $
            throwError err400 { errBody = "Only raster image uploads with matching MIME type and extension are allowed" }
        fileSize <- liftIO (getFileSize (fdPayload eiuFile))
        either throwError pure (validateEventImageUploadSize fileSize)
        uuid <- liftIO UUIDV4.nextRandom
        let eventIdTxt = renderKeyText eventKey
            storedName = UUID.toText uuid <> "-" <> safeName
            relPath = T.intercalate "/" ["social-events", "events", eventIdTxt, "moments", storedName]
            targetDir = assetsRootDir envConfig </> "social-events" </> "events" </> T.unpack eventIdTxt </> "moments"
            targetPath = targetDir </> T.unpack storedName
            publicUrl = buildUploadAssetUrl (resolveConfiguredAssetsBase envConfig) relPath
        liftIO $ createDirectoryIfMissing True targetDir
        liftIO $ copyFile (fdPayload eiuFile) targetPath
        pure EventImageUploadDTO
            { eiuEventId = eventIdTxt
            , eiuFileName = storedName
            , eiuPath = relPath
            , eiuPublicUrl = publicUrl
            , eiuImageUrl = publicUrl
            }

    reactToMoment :: T.Text -> T.Text -> EventMomentReactionRequestDTO -> AppM EventMomentDTO
    reactToMoment eventIdStr momentIdStr EventMomentReactionRequestDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        _ <- requireExistingEvent envPool eventKey
        momentKey <- parseKeyOr400 "moment" momentIdStr
        _ <- requireMomentForEvent envPool eventKey momentKey
        reactionTypeId <-
            liftIO (runSqlPool (loadSelectableMomentReactionTypeId emrrReactionTypeId) envPool)
                >>= either throwError pure
        existingSameReaction <-
            liftIO $
                runSqlPool
                    ( selectFirst
                        [ EventMomentReactionMomentId ==. momentKey
                        , EventMomentReactionReactionTypeId ==. Just reactionTypeId
                        , EventMomentReactionReactorPartyId ==. currentPartyId
                        ]
                        []
                    )
                    envPool
        liftIO $
            runSqlPool
                ( do
                    deleteWhere [EventMomentReactionMomentId ==. momentKey, EventMomentReactionReactorPartyId ==. currentPartyId]
                    when (isNothing existingSameReaction) $
                        insert_
                            EventMomentReaction
                                { eventMomentReactionMomentId = momentKey
                                , eventMomentReactionReactionTypeId = Just reactionTypeId
                                , eventMomentReactionReaction = Nothing
                                , eventMomentReactionReactorPartyId = currentPartyId
                                , eventMomentReactionCreatedAt = now
                                }
                )
                envPool
        liftIO $ loadMomentDTO envPool momentKey

    commentOnMoment :: T.Text -> T.Text -> EventMomentCommentCreateDTO -> AppM EventMomentCommentDTO
    commentOnMoment eventIdStr momentIdStr EventMomentCommentCreateDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        _ <- requireExistingEvent envPool eventKey
        momentKey <- parseKeyOr400 "moment" momentIdStr
        _ <- requireMomentForEvent envPool eventKey momentKey
        body <- either throwError pure (normalizeMomentCommentBody emccBody)
        let authorName = resolveMomentAuthorName currentPartyId emccAuthorName
        commentKey <-
            liftIO $
                runSqlPool
                    ( insert
                        EventMomentComment
                            { eventMomentCommentMomentId = momentKey
                            , eventMomentCommentAuthorPartyId = Just currentPartyId
                            , eventMomentCommentAuthorName = authorName
                            , eventMomentCommentBody = body
                            , eventMomentCommentCreatedAt = now
                            , eventMomentCommentUpdatedAt = now
                            }
                    )
                    envPool
        mComment <- liftIO $ runSqlPool (get commentKey) envPool
        case mComment of
            Nothing -> throwError err500{errBody = "Moment comment could not be loaded after insert"}
            Just commentRow -> pure (momentCommentEntityToDTO commentKey commentRow)

    -- Live broadcasts
    listLiveBroadcasts :: T.Text -> AppM [EventLiveBroadcastDTO]
    listLiveBroadcasts eventIdStr = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" eventIdStr
        _ <- requireExistingEvent envPool eventKey
        rows <-
            liftIO $
                runSqlPool
                    (selectList [EventLiveBroadcastEventId ==. eventKey] [Desc EventLiveBroadcastStartedAt])
                    envPool
        accessibleRows <-
            liftIO $
                filterM
                    ( \(Entity _ broadcastRow) ->
                        canAccessLiveBroadcast envPool currentPartyId broadcastRow
                    )
                    rows
        liftIO $
            mapM
                ( \(Entity broadcastKey broadcastRow) ->
                    liveBroadcastEntityToDTO envPool broadcastKey broadcastRow
                )
                accessibleRows

    createLiveBroadcast ::
        T.Text ->
        EventLiveBroadcastCreateDTO ->
        AppM EventLiveBroadcastDTO
    createLiveBroadcast eventIdStr EventLiveBroadcastCreateDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        _ <- requireExistingEvent envPool eventKey
        artistKey <- parseArtistId elbCreateArtistId
        artistRow <- requireEventArtistProfile envPool eventKey artistKey
        requireArtistFollower envPool artistKey currentPartyId
        validateLiveBroadcastBroadcaster currentPartyId elbCreateBroadcasterPartyId
        let fallbackTitle = artistProfileName artistRow <> " en vivo"
        titleVal <-
            either throwError pure $
                normalizeLiveBroadcastTitle (cleanMaybeText elbCreateTitle <|> Just fallbackTitle)
        descriptionVal <-
            either throwError pure $
                normalizeLiveBroadcastDescription elbCreateDescription
        _quality <-
            either throwError pure $
                normalizeLiveBroadcastQuality elbCreateQuality
        mExisting <-
            liftIO $
                runSqlPool
                    ( selectFirst
                        [ EventLiveBroadcastEventId ==. eventKey
                        , EventLiveBroadcastBroadcasterPartyId ==. currentPartyId
                        , EventLiveBroadcastStatus ==. "live"
                        ]
                        []
                    )
                    envPool
        when (isJust mExisting) $
            throwError err409{errBody = "Broadcaster already has an active live session for this event"}
        streamKey <- liftIO (UUID.toText <$> UUIDV4.nextRandom)
        (playbackUrl, ingestUrl, whipUrl) <- resolveLiveBroadcastStreamEndpoints streamKey
        let broadcasterName =
                fromMaybe ("Party " <> currentPartyId) (cleanMaybeText elbCreateBroadcasterName)
        broadcastKey <-
            liftIO $
                runSqlPool
                    ( insert
                        EventLiveBroadcast
                            { eventLiveBroadcastEventId = eventKey
                            , eventLiveBroadcastArtistId = artistKey
                            , eventLiveBroadcastBroadcasterPartyId = currentPartyId
                            , eventLiveBroadcastBroadcasterName = broadcasterName
                            , eventLiveBroadcastTitle = titleVal
                            , eventLiveBroadcastDescription = descriptionVal
                            , eventLiveBroadcastStatus = "live"
                            , eventLiveBroadcastPlaybackUrl = Just playbackUrl
                            , eventLiveBroadcastIngestUrl = Just ingestUrl
                            , eventLiveBroadcastWhipUrl = Just whipUrl
                            , eventLiveBroadcastStreamKey = Just streamKey
                            , eventLiveBroadcastViewerCount = 0
                            , eventLiveBroadcastStartedAt = now
                            , eventLiveBroadcastEndedAt = Nothing
                            , eventLiveBroadcastLastHeartbeatAt = now
                            , eventLiveBroadcastCreatedAt = now
                            , eventLiveBroadcastUpdatedAt = now
                            }
                    )
                    envPool
        liftIO $ loadLiveBroadcastDTO envPool broadcastKey

    heartbeatLiveBroadcast ::
        T.Text ->
        T.Text ->
        EventLiveBroadcastHeartbeatDTO ->
        AppM EventLiveBroadcastDTO
    heartbeatLiveBroadcast eventIdStr broadcastIdStr EventLiveBroadcastHeartbeatDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        _ <- requireExistingEvent envPool eventKey
        broadcastKey <- parseKeyOr400 "live broadcast" broadcastIdStr
        broadcastRow <- requireLiveBroadcastForEvent envPool eventKey broadcastKey
        canAccess <- liftIO $ canAccessLiveBroadcast envPool currentPartyId broadcastRow
        unless canAccess $
            throwError err403{errBody = "Live broadcast is only available to this artist fanclub"}
        let viewerDelta = max (-1000) (min 1000 (fromMaybe 0 elbhViewerDelta))
            nextViewerCount = max 0 (eventLiveBroadcastViewerCount broadcastRow + viewerDelta)
        liftIO $
            runSqlPool
                ( update
                    broadcastKey
                    [ EventLiveBroadcastViewerCount =. nextViewerCount
                    , EventLiveBroadcastLastHeartbeatAt =. now
                    , EventLiveBroadcastUpdatedAt =. now
                    ]
                )
                envPool
        liftIO $ loadLiveBroadcastDTO envPool broadcastKey

    endLiveBroadcast ::
        T.Text ->
        T.Text ->
        EventLiveBroadcastEndDTO ->
        AppM EventLiveBroadcastDTO
    endLiveBroadcast eventIdStr broadcastIdStr EventLiveBroadcastEndDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        _ <- requireExistingEvent envPool eventKey
        broadcastKey <- parseKeyOr400 "live broadcast" broadcastIdStr
        broadcastRow <- requireLiveBroadcastForEvent envPool eventKey broadcastKey
        validateLiveBroadcastBroadcaster currentPartyId elbEndBroadcasterPartyId
        when (eventLiveBroadcastBroadcasterPartyId broadcastRow /= currentPartyId) $
            throwError err403{errBody = "Only the broadcaster can end this live session"}
        liftIO $
            runSqlPool
                ( update
                    broadcastKey
                    [ EventLiveBroadcastStatus =. "ended"
                    , EventLiveBroadcastEndedAt =. Just now
                    , EventLiveBroadcastLastHeartbeatAt =. now
                    , EventLiveBroadcastUpdatedAt =. now
                    ]
                )
                envPool
        liftIO $ loadLiveBroadcastDTO envPool broadcastKey

    -- Tickets
    ticketsServer :: ServerT TicketsRoutes AppM
    ticketsServer =
        listTicketTiers
            :<|> createTicketTier
            :<|> updateTicketTier
            :<|> listCurrentPartyTicketOrders
            :<|> listTicketOrders
            :<|> createTicketOrder
            :<|> updateTicketOrderStatus
            :<|> listTickets
            :<|> checkInTicket
            -- Promo Codes
            :<|> listPromoCodes
            :<|> createPromoCode
            :<|> updatePromoCode
            :<|> validatePromoCode
            -- Stripe Payment
            :<|> createStripePaymentIntent
            -- Refunds
            :<|> createRefundRequest
            :<|> listRefunds
            :<|> approveRefund
            :<|> rejectRefund
            -- Transfers
            :<|> createTransfer
            :<|> listTransfers
            :<|> acceptTransfer
            :<|> cancelTransfer
            -- Waitlist
            :<|> joinWaitlist
            :<|> listWaitlist
            :<|> notifyWaitlist
            :<|> removeFromWaitlist
            -- QR Codes
            :<|> getTicketQR

    listTicketTiers :: T.Text -> AppM [TicketTierDTO]
    listTicketTiers eventIdStr = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        when (isNothing mEvent) $ throwError err404{errBody = "Event not found"}
        rows <-
            liftIO $
                runSqlPool
                    (selectList [EventTicketTierEventId ==. eventKey] [Asc EventTicketTierPosition, Asc EventTicketTierId])
                    envPool
        pure (map (ticketTierEntityToDTO eventKey) rows)

    createTicketTier :: T.Text -> TicketTierDTO -> AppM TicketTierDTO
    createTicketTier eventIdStr dto = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        _ <- claimOrRequireEventManager currentPartyId envPool eventKey eventVal
        let tierName = T.strip (ticketTierName dto)
        when (T.null tierName) $ throwError err400{errBody = "ticket tier name is required"}
        when (ticketTierPriceCents dto < 0) $ throwError err400{errBody = "ticket tier price must be >= 0"}
        when (ticketTierQuantityTotal dto <= 0) $ throwError err400{errBody = "ticket tier quantity must be > 0"}
        (eventCurrencyVal, _) <-
            either
                (throwError . storedEventMetadataServerError)
                pure
                (validateStoredEventFinanceMetadata (defaultCurrency envConfig) eventVal)
        currencyVal <-
            either throwError pure $
                validateTicketTierCurrencyInput eventCurrencyVal (ticketTierCurrency dto)
        tierCode <-
            either throwError pure $
                validateTicketTierCodeInput tierName (ticketTierCode dto)
        let salesStartVal = ticketTierSalesStart dto
            salesEndVal = ticketTierSalesEnd dto
        when (invalidSalesWindow salesStartVal salesEndVal) $ throwError err400{errBody = "invalid sales window"}
        mInserted <-
            liftIO $
                runSqlPool
                    ( insertUnique
                        EventTicketTier
                            { eventTicketTierEventId = eventKey
                            , eventTicketTierCode = tierCode
                            , eventTicketTierName = tierName
                            , eventTicketTierDescription = cleanMaybeText (ticketTierDescription dto)
                            , eventTicketTierPriceCents = ticketTierPriceCents dto
                            , eventTicketTierCurrency = currencyVal
                            , eventTicketTierCurrencyId = Nothing
                            , eventTicketTierQuantityTotal = ticketTierQuantityTotal dto
                            , eventTicketTierQuantitySold = 0
                            , eventTicketTierSalesStart = salesStartVal
                            , eventTicketTierSalesEnd = salesEndVal
                            , eventTicketTierIsActive = ticketTierActive dto
                            , eventTicketTierPosition = ticketTierPosition dto
                            , eventTicketTierEnableWaitlist = False
                            , eventTicketTierAllowTransfers = True
                            , eventTicketTierRefundPolicy = "full"
                            , eventTicketTierRefundDeadline = Nothing
                            , eventTicketTierCreatedAt = now
                            , eventTicketTierUpdatedAt = now
                            }
                    )
                    envPool
        tierKey <- maybe (throwError err409{errBody = "ticket tier code already exists for this event"}) pure mInserted
        mTier <- liftIO $ runSqlPool (getEntity tierKey) envPool
        maybe
            (throwError err500{errBody = "Could not create ticket tier"})
            (pure . ticketTierEntityToDTO eventKey)
            mTier

    updateTicketTier :: T.Text -> T.Text -> TicketTierDTO -> AppM TicketTierDTO
    updateTicketTier eventIdStr tierIdStr dto = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        tierKey <- parseKeyOr400 "ticket tier" tierIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        _ <- claimOrRequireEventManager currentPartyId envPool eventKey eventVal
        mTier <- liftIO $ runSqlPool (get tierKey) envPool
        tier <- maybe (throwError err404{errBody = "Ticket tier not found"}) pure mTier
        when (eventTicketTierEventId tier /= eventKey) $ throwError err400{errBody = "Ticket tier does not belong to this event"}
        let tierName = T.strip (ticketTierName dto)
        when (T.null tierName) $ throwError err400{errBody = "ticket tier name is required"}
        when (ticketTierPriceCents dto < 0) $ throwError err400{errBody = "ticket tier price must be >= 0"}
        when (ticketTierQuantityTotal dto < eventTicketTierQuantitySold tier) $ throwError err400{errBody = "ticket tier quantity cannot be below sold quantity"}
        (eventCurrencyVal, _) <-
            either
                (throwError . storedEventMetadataServerError)
                pure
                (validateStoredEventFinanceMetadata (defaultCurrency envConfig) eventVal)
        currencyVal <-
            either throwError pure $
                validateTicketTierCurrencyInput eventCurrencyVal (ticketTierCurrency dto)
        tierCode <-
            either throwError pure $
                validateTicketTierCodeInput tierName (ticketTierCode dto)
        let salesStartVal = ticketTierSalesStart dto
            salesEndVal = ticketTierSalesEnd dto
        when (invalidSalesWindow salesStartVal salesEndVal) $ throwError err400{errBody = "invalid sales window"}
        mCodeOwner <- liftIO $ runSqlPool (getBy (UniqueEventTicketTierCode eventKey tierCode)) envPool
        case mCodeOwner of
            Just (Entity existingKey _)
                | existingKey /= tierKey ->
                    throwError err409{errBody = "ticket tier code already exists for this event"}
            _ -> pure ()
        liftIO $
            runSqlPool
                ( update
                    tierKey
                    [ EventTicketTierCode =. tierCode
                    , EventTicketTierName =. tierName
                    , EventTicketTierDescription =. cleanMaybeText (ticketTierDescription dto)
                    , EventTicketTierPriceCents =. ticketTierPriceCents dto
                    , EventTicketTierCurrency =. currencyVal
                    , EventTicketTierQuantityTotal =. ticketTierQuantityTotal dto
                    , EventTicketTierSalesStart =. salesStartVal
                    , EventTicketTierSalesEnd =. salesEndVal
                    , EventTicketTierIsActive =. ticketTierActive dto
                    , EventTicketTierPosition =. ticketTierPosition dto
                    , EventTicketTierUpdatedAt =. now
                    ]
                )
                envPool
        mUpdated <- liftIO $ runSqlPool (getEntity tierKey) envPool
        maybe
            (throwError err500{errBody = "Could not update ticket tier"})
            (pure . ticketTierEntityToDTO eventKey)
            mUpdated

    listCurrentPartyTicketOrders :: Maybe T.Text -> AppM [TicketOrderDTO]
    listCurrentPartyTicketOrders mStatus = do
        Env{..} <- ask
        statusFilters <- case cleanMaybeText mStatus of
            Nothing -> pure []
            Just raw -> case parseTicketOrderStatus raw of
                Nothing -> throwError err400{errBody = "Invalid ticket order status"}
                Just statusVal -> pure [EventTicketOrderStatus ==. statusVal]
        let filters =
                [EventTicketOrderBuyerPartyId ==. Just currentPartyId]
                    ++ statusFilters
        rows <-
            liftIO $
                runSqlPool
                    ( selectList
                        filters
                        [ Desc EventTicketOrderPurchasedAt
                        , Desc EventTicketOrderId
                        , LimitTo 500
                        ]
                    )
                    envPool
        forM rows $ \orderEnt@(Entity orderKey orderRow) -> do
            _ <-
                either
                    throwError
                    pure
                    (validateStoredTicketOrderStatus (Just (eventTicketOrderStatus orderRow)))
            tickets <-
                liftIO $
                    runSqlPool
                        (selectList [EventTicketOrderRefId ==. orderKey] [Asc EventTicketId])
                        envPool
            pure (ticketOrderEntityToDTO orderEnt tickets)

    listTicketOrders :: T.Text -> Maybe T.Text -> Maybe T.Text -> AppM [TicketOrderDTO]
    listTicketOrders eventIdStr mBuyerPartyId mStatus = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        let manager = isEventManager currentPartyId eventVal
        requestedBuyer <-
            either
                throwError
                pure
                (validateOptionalTicketBuyerPartyId "buyerPartyId" mBuyerPartyId)
        buyerFilters <-
            if manager
                then pure $ maybe [] (\buyer -> [EventTicketOrderBuyerPartyId ==. Just buyer]) requestedBuyer
                else case requestedBuyer of
                    Nothing -> pure [EventTicketOrderBuyerPartyId ==. Just currentPartyId]
                    Just buyer
                        | buyer == currentPartyId -> pure [EventTicketOrderBuyerPartyId ==. Just currentPartyId]
                        | otherwise -> throwError err403{errBody = "You can only list your own ticket orders"}
        statusFilters <- case cleanMaybeText mStatus of
            Nothing -> pure []
            Just raw -> case parseTicketOrderStatus raw of
                Nothing -> throwError err400{errBody = "Invalid ticket order status"}
                Just statusVal -> pure [EventTicketOrderStatus ==. statusVal]
        let filters = [EventTicketOrderEventId ==. eventKey] ++ buyerFilters ++ statusFilters
        rows <- liftIO $ runSqlPool (selectList filters [Desc EventTicketOrderPurchasedAt, LimitTo 200]) envPool
        forM rows $ \orderEnt@(Entity orderKey orderRow) -> do
            _ <-
                either
                    throwError
                    pure
                    (validateStoredTicketOrderStatus (Just (eventTicketOrderStatus orderRow)))
            tickets <- liftIO $ runSqlPool (selectList [EventTicketOrderRefId ==. orderKey] [Asc EventTicketId]) envPool
            pure (ticketOrderEntityToDTO orderEnt tickets)

    createTicketOrder :: T.Text -> TicketPurchaseRequestDTO -> AppM TicketOrderDTO
    createTicketOrder eventIdStr TicketPurchaseRequestDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        tierKey <- parseKeyOr400 "ticket tier" ticketPurchaseTierId
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        purchaseEnabled <- liftIO $ runSqlPool (eventTicketPurchaseEnabledFor eventVal) envPool
        either throwError pure $
            validateTicketPurchaseEventEligibility (socialEventMetadata eventVal) purchaseEnabled
        mTier <- liftIO $ runSqlPool (get tierKey) envPool
        tier <- maybe (throwError err404{errBody = "Ticket tier not found"}) pure mTier
        when (eventTicketTierEventId tier /= eventKey) $ throwError err400{errBody = "Ticket tier does not belong to this event"}
        when (ticketPurchaseQuantity <= 0) $ throwError err400{errBody = "Quantity must be > 0"}
        when (not (isTicketTierSaleOpen now tier)) $ throwError err400{errBody = "Ticket sales are closed for this tier"}

        let manager = isEventManager currentPartyId eventVal
        either throwError pure $
            validateDirectTicketOrderPricing manager (eventTicketTierPriceCents tier)
        requestedBuyer <-
            either
                throwError
                pure
                (validateOptionalTicketBuyerPartyId "ticketPurchaseBuyerPartyId" ticketPurchaseBuyerPartyId)
        buyerParty <- case requestedBuyer of
            Nothing -> pure (Just currentPartyId)
            Just buyer
                | buyer == currentPartyId -> pure (Just currentPartyId)
                | manager -> pure (Just buyer)
                | otherwise -> throwError err403{errBody = "Cannot assign tickets to another buyer"}

        let availableInTier = ticketTierAvailability tier
        when (ticketPurchaseQuantity > availableInTier) $ throwError err409{errBody = "Not enough tickets available"}

        orderAmountCents <-
            either throwError pure $
                validateTicketCheckoutAmount
                    ticketPurchaseQuantity
                    (eventTicketTierPriceCents tier)
        buyerName <-
            either
                throwError
                pure
                (validateTicketPurchaseBuyerName ticketPurchaseBuyerName)
        buyerEmail <-
            either throwError pure (validateTicketPurchaseBuyerEmail ticketPurchaseBuyerEmail)
        when (orderAmountCents < 0) $ throwError err400{errBody = "Invalid amount"}
        let orderRecord =
                EventTicketOrder
                    { eventTicketOrderEventId = eventKey
                    , eventTicketOrderTierId = tierKey
                    , eventTicketOrderBuyerPartyId = buyerParty
                    , eventTicketOrderBuyerName = buyerName
                    , eventTicketOrderBuyerEmail = buyerEmail
                    , eventTicketOrderQuantity = ticketPurchaseQuantity
                    , eventTicketOrderAmountCents = orderAmountCents
                    , eventTicketOrderCurrency = eventTicketTierCurrency tier
                    , eventTicketOrderStatus = "paid"
                    , eventTicketOrderMetadata = Nothing
                    , eventTicketOrderCheckoutIdempotencyKey = Nothing
                    , eventTicketOrderPurchasedAt = now
                    , eventTicketOrderStripePaymentIntentId = Nothing
                    , eventTicketOrderPromoCodeId = Nothing
                    , eventTicketOrderOriginalAmountCents = Nothing
                    , eventTicketOrderPaymentMethod = Nothing
                    , eventTicketOrderCreatedAt = now
                    , eventTicketOrderUpdatedAt = now
                    }

        createdOrder <-
            liftIO $
                runSqlPool
                    ( do
                        lockedEvents <-
                            rawSql
                                "SELECT ?? FROM social_event WHERE id = ? FOR UPDATE"
                                [toPersistValue eventKey]
                        when (null (lockedEvents :: [Entity SocialEvent])) $
                            fail "Event not found while reserving tickets"
                        soldAcross <- selectList [EventTicketTierEventId ==. eventKey] []
                        let soldCount =
                                sum (map (eventTicketTierQuantitySold . entityVal) soldAcross)
                            capacityAvailable =
                                maybe
                                    True
                                    (\cap -> soldCount + ticketPurchaseQuantity <= cap)
                                    (socialEventCapacity eventVal)
                        if not capacityAvailable
                            then pure Nothing
                            else do
                                reservedCount <-
                                    updateWhereCount
                                        [ EventTicketTierId ==. tierKey
                                        , EventTicketTierIsActive ==. True
                                        , EventTicketTierQuantitySold
                                            <=. eventTicketTierQuantityTotal tier - ticketPurchaseQuantity
                                        ]
                                        [ EventTicketTierQuantitySold +=. ticketPurchaseQuantity
                                        , EventTicketTierUpdatedAt =. now
                                        ]
                                if reservedCount == 0
                                    then pure Nothing
                                    else do
                                        orderKey <- insert orderRecord
                                        codes <-
                                            issueMissingTicketsForOrder now orderKey orderRecord
                                        tickets <-
                                            selectList
                                                [EventTicketOrderRefId ==. orderKey]
                                                [Asc EventTicketId]
                                        pure . Just $
                                            ( ticketOrderEntityToDTO
                                                (Entity orderKey orderRecord)
                                                tickets
                                            , orderRecord
                                            , codes
                                            )
                    )
                    envPool
        (orderDto, orderRecord, ticketCodes) <-
            maybe
                (throwError err409{errBody = "Not enough tickets available"})
                pure
                createdOrder
        sendTicketConfirmationForOrder orderRecord ticketCodes
        pure orderDto

    updateTicketOrderStatus :: T.Text -> T.Text -> TicketOrderStatusUpdateDTO -> AppM TicketOrderDTO
    updateTicketOrderStatus eventIdStr orderIdStr TicketOrderStatusUpdateDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        orderKey <- parseKeyOr400 "ticket order" orderIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        newStatus <- case parseTicketOrderStatus ticketOrderStatus of
            Nothing -> throwError err400{errBody = "Invalid ticket order status"}
            Just "pending" -> throwError err400{errBody = "Use paid, cancelled or refunded"}
            Just s -> pure s

        mOrder <- liftIO $ runSqlPool (get orderKey) envPool
        order <- maybe (throwError err404{errBody = "Ticket order not found"}) pure mOrder
        when (eventTicketOrderEventId order /= eventKey) $ throwError err400{errBody = "Ticket order does not belong to this event"}
        oldStatus <-
            either
                throwError
                pure
                (validateStoredTicketOrderStatus (Just (eventTicketOrderStatus order)))
        when (oldStatus `elem` ["cancelled", "refunded"] && newStatus == "paid") $
            throwError err400{errBody = "Closed orders cannot be moved back to paid"}
        when (oldStatus /= "paid" && newStatus == "paid") $
            throwError
                err409
                    { errBody =
                        "Ticket payment can only be confirmed by verified provider processing"
                    }
        let buyerOwnPendingCancellation =
                eventTicketOrderBuyerPartyId order == Just currentPartyId
                    && oldStatus == "pending"
                    && newStatus == "cancelled"
        when (not buyerOwnPendingCancellation) $ do
            _ <- claimOrRequireEventManager currentPartyId envPool eventKey eventVal
            pure ()

        when (oldStatus == "pending" && newStatus == "cancelled") $
            forM_ (eventTicketOrderStripePaymentIntentId order) $ \paymentIntentId -> do
                stripeCfg <-
                    case (stripeSecretKey envConfig, stripeWebhookSecret envConfig) of
                        (Just secretKey, Just webhookSecret) ->
                            pure
                                Stripe.StripeConfig
                                    { Stripe.stripeSecretKey = secretKey
                                    , Stripe.stripeWebhookSecret = webhookSecret
                                    , Stripe.stripeApiVersion = Stripe.defaultStripeApiVersion
                                    }
                        _ ->
                            throwError
                                err500{errBody = "Stripe is not configured; order remains pending"}
                cancelResult <-
                    liftIO $
                        runStripeRequestSafely $
                            Stripe.cancelPaymentIntent stripeCfg paymentIntentId
                case cancelResult of
                    Left _ ->
                        throwError
                            err502
                                { errBody =
                                    "Could not cancel Stripe payment; order remains pending"
                                }
                    Right _ -> pure ()

        mTier <- liftIO $ runSqlPool (get (eventTicketOrderTierId order)) envPool
        tier <- maybe (throwError err404{errBody = "Ticket tier not found"}) pure mTier
        let qty = eventTicketOrderQuantity order
            tierAvailable = ticketTierAvailability tier
            capacity = socialEventCapacity eventVal
            soldAdjust = ticketOrderInventoryAdjustment qty oldStatus newStatus
        when (soldAdjust > 0 && soldAdjust > tierAvailable) $ throwError err409{errBody = "Not enough ticket inventory to mark as paid"}
        when (soldAdjust > 0) $ do
            soldAcross <-
                liftIO $
                    runSqlPool
                        (selectList [EventTicketTierEventId ==. eventKey] [])
                        envPool
            let soldCount = sum (map (eventTicketTierQuantitySold . entityVal) soldAcross)
            case capacity of
                Nothing -> pure ()
                Just cap ->
                    when (soldCount + soldAdjust > cap) $ throwError err409{errBody = "Event capacity reached"}
        when (eventTicketTierQuantitySold tier + soldAdjust < 0) $ throwError err409{errBody = "Sold quantity underflow"}

        let nextTicketStatus = case newStatus of
                "paid" -> "issued"
                "cancelled" -> "cancelled"
                "refunded" -> "refunded"
                _ -> "issued"
        (statusChanged, orderDto) <-
            liftIO $
                runSqlPool
                    ( do
                        changedCount <-
                            updateWhereCount
                                [ EventTicketOrderId ==. orderKey
                                , EventTicketOrderStatus ==. eventTicketOrderStatus order
                                ]
                                [ EventTicketOrderStatus =. newStatus
                                , EventTicketOrderUpdatedAt =. now
                                ]
                        let changed = changedCount > 0
                        when changed $ do
                            when (soldAdjust /= 0) $
                                update
                                    (eventTicketOrderTierId order)
                                    [ EventTicketTierQuantitySold +=. soldAdjust
                                    , EventTicketTierUpdatedAt =. now
                                    ]
                            when (oldStatus == "pending" && newStatus == "paid") $ do
                                _ <- issueMissingTicketsForOrder now orderKey order
                                pure ()
                            let ticketUpdates =
                                    [ EventTicketStatus =. nextTicketStatus
                                    , EventTicketUpdatedAt =. now
                                    ]
                                        ++ if nextTicketStatus == "issued"
                                            then [EventTicketCheckedInAt =. Nothing]
                                            else []
                            updateWhere [EventTicketOrderRefId ==. orderKey] ticketUpdates
                            when
                                ( oldStatus == "pending"
                                    && newStatus `elem` ["cancelled", "refunded"]
                                )
                                $ forM_ (eventTicketOrderPromoCodeId order)
                                $ \promoKey ->
                                    update promoKey [PromoCodeCurrentRedemptions +=. (-1)]
                        mOrderEnt <- getEntity orderKey
                        case mOrderEnt of
                            Nothing -> pure (changed, Nothing)
                            Just orderEnt -> do
                                tickets <- selectList [EventTicketOrderRefId ==. orderKey] [Asc EventTicketId]
                                pure (changed, Just (ticketOrderEntityToDTO orderEnt tickets))
                    )
                    envPool

        case orderDto of
            Nothing -> throwError err500{errBody = "Could not update ticket order"}
            Just dto
                | statusChanged -> pure dto
                | ticketOrderStatusValue dto == newStatus -> pure dto
                | otherwise -> throwError err409{errBody = "Ticket order status changed; refresh and retry"}

    listTickets :: T.Text -> Maybe T.Text -> Maybe T.Text -> AppM [TicketDTO]
    listTickets eventIdStr mOrderId mStatus = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        let manager = isEventManager currentPartyId eventVal
        orderFilters <- case cleanMaybeText mOrderId of
            Nothing ->
                if manager
                    then pure []
                    else do
                        ownOrders <-
                            liftIO $
                                runSqlPool
                                    (selectList [EventTicketOrderEventId ==. eventKey, EventTicketOrderBuyerPartyId ==. Just currentPartyId] [LimitTo 500])
                                    envPool
                        let orderIds = map entityKey ownOrders
                        if null orderIds
                            then pure [EventTicketId ==. toSqlKey 0]
                            else pure [EventTicketOrderRefId <-. orderIds]
            Just rawOrderId -> do
                orderKey <- parseKeyOr400 "ticket order" rawOrderId
                mOrder <- liftIO $ runSqlPool (get orderKey) envPool
                order <- maybe (throwError err404{errBody = "Ticket order not found"}) pure mOrder
                when (eventTicketOrderEventId order /= eventKey) $ throwError err400{errBody = "Ticket order does not belong to this event"}
                when (not manager && eventTicketOrderBuyerPartyId order /= Just currentPartyId) $
                    throwError err403{errBody = "You can only list your own tickets"}
                pure [EventTicketOrderRefId ==. orderKey]

        statusFilters <- case cleanMaybeText mStatus of
            Nothing -> pure []
            Just raw -> case parseTicketStatus raw of
                Nothing -> throwError err400{errBody = "Invalid ticket status"}
                Just statusVal -> pure [EventTicketStatus ==. statusVal]

        let filters = [EventTicketEventId ==. eventKey] ++ orderFilters ++ statusFilters
        rows <- liftIO $ runSqlPool (selectList filters [Asc EventTicketId, LimitTo 400]) envPool
        pure (map ticketEntityToDTO rows)

    checkInTicket :: T.Text -> TicketCheckInRequestDTO -> AppM TicketDTO
    checkInTicket eventIdStr TicketCheckInRequestDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        _ <- claimOrRequireEventManager currentPartyId envPool eventKey eventVal

        ticketLookup <- either throwError pure (validateTicketCheckInLookup TicketCheckInRequestDTO{..})
        mTicket <- liftIO $ runSqlPool (findTicketForCheckIn eventKey ticketLookup) envPool
        ticketEntity <- maybe (throwError err404{errBody = "Ticket not found"}) pure mTicket

        let ticketKey = entityKey ticketEntity
            ticketVal = entityVal ticketEntity
        orderRef <- liftIO $ runSqlPool (get (eventTicketOrderRefId ticketVal)) envPool
        orderStatus <-
            either
                throwError
                pure
                (validateTicketCheckInOrderStatus (eventTicketOrderStatus <$> orderRef))
        when (orderStatus /= "paid") $ throwError err400{errBody = "Only paid tickets can be checked in"}
        ticketStatus <-
            either
                throwError
                pure
                (validateTicketCheckInTicketStatus (eventTicketStatus ticketVal))
        case ticketStatus of
            "cancelled" -> throwError err400{errBody = "Cancelled tickets cannot be checked in"}
            "refunded" -> throwError err400{errBody = "Refunded tickets cannot be checked in"}
            "checked_in" -> pure (ticketEntityToDTO ticketEntity)
            _ -> do
                liftIO $
                    runSqlPool
                        ( update
                            ticketKey
                            [ EventTicketStatus =. "checked_in"
                            , EventTicketCheckedInAt =. Just now
                            , EventTicketUpdatedAt =. now
                            ]
                        )
                        envPool
                mUpdated <- liftIO $ runSqlPool (getEntity ticketKey) envPool
                maybe
                    (throwError err500{errBody = "Could not check in ticket"})
                    (pure . ticketEntityToDTO)
                    mUpdated

    -- Promo Codes
    listPromoCodes :: T.Text -> AppM [PromoCodeDTO]
    listPromoCodes eventIdStr = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        _ <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        rows <-
            liftIO $
                runSqlPool
                    (selectList [PromoCodeEventId ==. Just eventKey] [Desc PromoCodeCreatedAt])
                    envPool
        pure (map promoCodeEntityToDTO rows)

    createPromoCode :: T.Text -> PromoCodeDTO -> AppM PromoCodeDTO
    createPromoCode eventIdStr PromoCodeDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, _) <- requireManagedEvent eventIdStr
        let code = T.toUpper (T.strip promoCodeCode)
        when (T.null code) $ throwError err400{errBody = "Promo code is required"}
        when (promoCodeDiscountValue < 0) $ throwError err400{errBody = "Discount value must be >= 0"}
        when (promoCodeDiscountType `notElem` ["percentage", "fixed", "fixed_amount"]) $
            throwError err400{errBody = "Discount type must be 'percentage' or 'fixed'"}
        when (promoCodeDiscountType == "percentage" && promoCodeDiscountValue > 10000) $
            throwError err400{errBody = "Percentage discount cannot exceed 100% (10000 basis points)"}
        let tierIdsJson = encodePromoCodeTierIds promoCodeTierIds
        mInserted <-
            liftIO $
                runSqlPool
                    ( insertUnique
                        PromoCode
                            { promoCodeEventId = Just eventKey
                            , promoCodeCode = code
                            , promoCodeDescription = promoCodeDescription
                            , promoCodeDiscountType = promoCodeDiscountType
                            , promoCodeDiscountValue = promoCodeDiscountValue
                            , promoCodeCurrency = promoCodeCurrency
                            , promoCodeMaxRedemptions = promoCodeMaxRedemptions
                            , promoCodeCurrentRedemptions = 0
                            , promoCodeValidFrom = promoCodeValidFrom
                            , promoCodeValidUntil = promoCodeValidUntil
                            , promoCodeTierIds = tierIdsJson
                            , promoCodeMinPurchaseAmountCents = promoCodeMinPurchaseAmountCents
                            , promoCodeIsActive = promoCodeIsActive
                            , promoCodeCreatedByPartyId = Just currentPartyId
                            , promoCodeCreatedAt = now
                            , promoCodeUpdatedAt = now
                            }
                    )
                    envPool
        codeKey <- maybe (throwError err409{errBody = "Promo code already exists"}) pure mInserted
        mCode <- liftIO $ runSqlPool (getEntity codeKey) envPool
        maybe
            (throwError err500{errBody = "Could not create promo code"})
            (pure . promoCodeEntityToDTO)
            mCode

    updatePromoCode :: T.Text -> T.Text -> PromoCodeDTO -> AppM PromoCodeDTO
    updatePromoCode eventIdStr codeIdStr PromoCodeDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, _) <- requireManagedEvent eventIdStr
        codeKey <- parseKeyOr400 "promo code" codeIdStr
        mCode <- liftIO $ runSqlPool (get codeKey) envPool
        codeRow <- maybe (throwError err404{errBody = "Promo code not found"}) pure mCode
        when (SM.promoCodeEventId codeRow /= Just eventKey) $
            throwError err400{errBody = "Promo code does not belong to this event"}
        when (promoCodeDiscountValue < 0) $ throwError err400{errBody = "Discount value must be >= 0"}
        when (promoCodeDiscountType `notElem` ["percentage", "fixed", "fixed_amount"]) $
            throwError err400{errBody = "Discount type must be 'percentage' or 'fixed'"}
        when (promoCodeDiscountType == "percentage" && promoCodeDiscountValue > 10000) $
            throwError err400{errBody = "Percentage discount cannot exceed 100% (10000 basis points)"}
        let tierIdsJson = encodePromoCodeTierIds promoCodeTierIds
        liftIO $
            runSqlPool
                ( update
                    codeKey
                    [ PromoCodeDescription =. promoCodeDescription
                    , PromoCodeDiscountType =. promoCodeDiscountType
                    , PromoCodeDiscountValue =. promoCodeDiscountValue
                    , PromoCodeCurrency =. promoCodeCurrency
                    , PromoCodeMaxRedemptions =. promoCodeMaxRedemptions
                    , PromoCodeValidFrom =. promoCodeValidFrom
                    , PromoCodeValidUntil =. promoCodeValidUntil
                    , PromoCodeTierIds =. tierIdsJson
                    , PromoCodeMinPurchaseAmountCents =. promoCodeMinPurchaseAmountCents
                    , PromoCodeIsActive =. promoCodeIsActive
                    , PromoCodeUpdatedAt =. now
                    ]
                )
                envPool
        mUpdated <- liftIO $ runSqlPool (getEntity codeKey) envPool
        maybe
            (throwError err500{errBody = "Could not update promo code"})
            (pure . promoCodeEntityToDTO)
            mUpdated

    validatePromoCode :: T.Text -> T.Text -> Maybe T.Text -> Maybe T.Text -> AppM PromoCodeDTO
    validatePromoCode eventIdStr codeStr mQueryCode mTierId = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        _ <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        let cleanCode = T.toUpper (T.strip codeStr)
        forM_ (cleanMaybeText mQueryCode) $ \queryCode ->
            when (T.toUpper queryCode /= cleanCode) $
                throwError err400{errBody = "Promo code query must match the promo code path"}
        mCodeEnt <- liftIO $ runSqlPool (getBy (UniquePromoCode cleanCode)) envPool
        codeEnt <- maybe (throwError err404{errBody = "Promo code not found"}) pure mCodeEnt
        let code = entityVal codeEnt
        when (SM.promoCodeEventId code /= Just eventKey) $
            throwError err400{errBody = "Promo code does not belong to this event"}
        when (not (SM.promoCodeIsActive code)) $
            throwError err400{errBody = "Promo code is not active"}
        eitherPromoCodeBadRequest $
            validatePromoCodeDateWindow
                now
                (SM.promoCodeValidFrom code)
                (SM.promoCodeValidUntil code)
        eitherPromoCodeBadRequest $
            validatePromoCodeRedemptionLimit
                (SM.promoCodeCurrentRedemptions code)
                (SM.promoCodeMaxRedemptions code)
        eitherPromoCodeBadRequest $
            validatePromoCodeTierEligibility (SM.promoCodeTierIds code) mTierId
        pure (promoCodeEntityToDTO codeEnt)

    -- Stripe Payment
    createStripePaymentIntent :: TicketPurchaseWithPromoDTO -> AppM StripePaymentIntentDTO
    createStripePaymentIntent TicketPurchaseWithPromoDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        let TicketPurchaseRequestDTO{..} = tpwpPurchase
        tierKey <- parseKeyOr400 "ticket tier" ticketPurchaseTierId
        mTier <- liftIO $ runSqlPool (get tierKey) envPool
        tier <- maybe (throwError err404{errBody = "Ticket tier not found"}) pure mTier
        let eventKey = eventTicketTierEventId tier
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        purchaseEnabled <- liftIO $ runSqlPool (eventTicketPurchaseEnabledFor eventVal) envPool
        either throwError pure $
            validateTicketPurchaseEventEligibility (socialEventMetadata eventVal) purchaseEnabled
        when (ticketPurchaseQuantity <= 0) $ throwError err400{errBody = "Quantity must be > 0"}
        when (not (isTicketTierSaleOpen now tier)) $
            throwError err400{errBody = "Ticket sales are closed for this tier"}
        let manager = isEventManager currentPartyId eventVal
        requestedBuyer <-
            either
                throwError
                pure
                (validateOptionalTicketBuyerPartyId "ticketPurchaseBuyerPartyId" ticketPurchaseBuyerPartyId)
        buyerParty <- case requestedBuyer of
            Nothing -> pure (Just currentPartyId)
            Just buyer
                | buyer == currentPartyId -> pure (Just currentPartyId)
                | manager -> pure (Just buyer)
                | otherwise -> throwError err403{errBody = "Cannot assign tickets to another buyer"}
        buyerName <-
            either
                throwError
                pure
                (validateTicketPurchaseBuyerName ticketPurchaseBuyerName)
        buyerEmail <-
            either
                throwError
                pure
                (validateTicketPurchaseBuyerEmail ticketPurchaseBuyerEmail)
        mExistingCheckout <- case tpwpIdempotencyKey of
            Nothing -> pure Nothing
            Just idempotencyKey ->
                liftIO $
                    runSqlPool
                        ( getBy
                            ( UniqueEventTicketCheckout
                                buyerParty
                                (Just idempotencyKey)
                            )
                        )
                        envPool
        forM_ mExistingCheckout $ \(Entity _ existingOrder) -> do
            let storedPromoCode =
                    decodeTicketCheckoutMetadata (eventTicketOrderMetadata existingOrder)
                        >>= snd
                requestMatches =
                    eventTicketOrderEventId existingOrder == eventKey
                        && eventTicketOrderTierId existingOrder == tierKey
                        && eventTicketOrderQuantity existingOrder == ticketPurchaseQuantity
                        && eventTicketOrderBuyerPartyId existingOrder == buyerParty
                        && eventTicketOrderBuyerName existingOrder == buyerName
                        && eventTicketOrderBuyerEmail existingOrder == buyerEmail
                        && eventTicketOrderCheckoutIdempotencyKey existingOrder
                            == tpwpIdempotencyKey
                        && storedPromoCode == tpwpPromoCode
            unless requestMatches $
                throwError err409{errBody = "ticketPurchaseIdempotencyKey was already used for different checkout details"}
        baseAmountCents <-
            either throwError pure $
                validateTicketCheckoutAmount
                    ticketPurchaseQuantity
                    (eventTicketTierPriceCents tier)
        (faceValueCents, mPromoCodeKey, mPromoCodeEnt) <- case mExistingCheckout of
            Just (Entity _ existingOrder) ->
                pure
                    ( eventTicketOrderAmountCents existingOrder
                    , eventTicketOrderPromoCodeId existingOrder
                    , Nothing
                    )
            Nothing -> case tpwpPromoCode of
                Nothing -> pure (baseAmountCents, Nothing, Nothing)
                Just promoCodeStr -> do
                    let cleanCode = T.toUpper (T.strip promoCodeStr)
                    mCodeEnt <- liftIO $ runSqlPool (getBy (UniquePromoCode cleanCode)) envPool
                    codeEnt <- maybe (throwError err404{errBody = "Promo code not found"}) pure mCodeEnt
                    let code = entityVal codeEnt
                    when (SM.promoCodeEventId code /= Just eventKey) $
                        throwError err400{errBody = "Promo code does not belong to this event"}
                    when (not (SM.promoCodeIsActive code)) $
                        throwError err400{errBody = "Promo code is not active"}
                    eitherPromoCodeBadRequest $
                        validatePromoCodeDateWindow
                            now
                            (SM.promoCodeValidFrom code)
                            (SM.promoCodeValidUntil code)
                    eitherPromoCodeBadRequest $
                        validatePromoCodeRedemptionLimit
                            (SM.promoCodeCurrentRedemptions code)
                            (SM.promoCodeMaxRedemptions code)
                    eitherPromoCodeBadRequest $
                        validatePromoCodeTierEligibility
                            (SM.promoCodeTierIds code)
                            (Just ticketPurchaseTierId)
                    eitherPromoCodeBadRequest $
                        validatePromoCodeMinimumPurchaseCents
                            (SM.promoCodeMinPurchaseAmountCents code)
                            baseAmountCents
                    when
                        ( SM.promoCodeDiscountType code `elem` ["fixed_amount", "fixed"]
                            && normalizeCurrency (SM.promoCodeCurrency code)
                                /= normalizeCurrency (eventTicketTierCurrency tier)
                        )
                        $ throwError err400{errBody = "Fixed promo code currency does not match the ticket tier"}
                    discountAmount <-
                        eitherPromoCodeBadRequest $
                            promoCodeDiscountAmountEither
                                baseAmountCents
                                (SM.promoCodeDiscountType code)
                                (SM.promoCodeDiscountValue code)
                    let discountedAmount = max 0 (baseAmountCents - discountAmount)
                    pure (discountedAmount, Just (entityKey codeEnt), Just codeEnt)
        let feeBreakdown = case mExistingCheckout of
                Just (Entity _ existingOrder) -> decodeTicketPlatformFeeBreakdown existingOrder
                Nothing -> ticketPlatformFeeBreakdown faceValueCents
            finalAmountCents = ticketCheckoutTotalCents feeBreakdown
        createdOrder <- case mExistingCheckout of
            Just (Entity existingOrderKey existingOrder) -> do
                existingTickets <-
                    liftIO $
                        runSqlPool
                            (selectList [EventTicketOrderRefId ==. existingOrderKey] [Asc EventTicketId])
                            envPool
                pure . Just $
                    ( existingOrderKey
                    , existingOrder
                    , map (eventTicketCode . entityVal) existingTickets
                    , True
                    )
            Nothing -> do
                let createReservedOrder = do
                        promoClaimed <- case mPromoCodeEnt of
                            Nothing -> pure True
                            Just (Entity promoKey promo) -> do
                                let redemptionFilters =
                                        [ PromoCodeId ==. promoKey
                                        , PromoCodeIsActive ==. True
                                        ]
                                            ++ maybe
                                                []
                                                (\limit -> [PromoCodeCurrentRedemptions <. limit])
                                                (SM.promoCodeMaxRedemptions promo)
                                (> 0)
                                    <$> updateWhereCount
                                        redemptionFilters
                                        [PromoCodeCurrentRedemptions +=. 1]
                        if not promoClaimed
                            then do
                                update
                                    tierKey
                                    [ EventTicketTierQuantitySold +=. negate ticketPurchaseQuantity
                                    , EventTicketTierUpdatedAt =. now
                                    ]
                                pure Nothing
                            else do
                                let zeroTotal = finalAmountCents == 0
                                    orderRecord =
                                        EventTicketOrder
                                            { eventTicketOrderEventId = eventKey
                                            , eventTicketOrderTierId = tierKey
                                            , eventTicketOrderBuyerPartyId = buyerParty
                                            , eventTicketOrderBuyerName = buyerName
                                            , eventTicketOrderBuyerEmail = buyerEmail
                                            , eventTicketOrderQuantity = ticketPurchaseQuantity
                                            , eventTicketOrderAmountCents = finalAmountCents
                                            , eventTicketOrderCurrency = eventTicketTierCurrency tier
                                            , eventTicketOrderStatus = if zeroTotal then "paid" else "pending"
                                            , eventTicketOrderMetadata =
                                                encodeTicketCheckoutMetadata
                                                    tpwpIdempotencyKey
                                                    tpwpPromoCode
                                                    feeBreakdown
                                            , eventTicketOrderCheckoutIdempotencyKey = tpwpIdempotencyKey
                                            , eventTicketOrderPurchasedAt = now
                                            , eventTicketOrderStripePaymentIntentId = Nothing
                                            , eventTicketOrderPromoCodeId = mPromoCodeKey
                                            , eventTicketOrderOriginalAmountCents = Just baseAmountCents
                                            , eventTicketOrderPaymentMethod =
                                                Just
                                                    ( if zeroTotal
                                                        then if isJust mPromoCodeKey then "promo" else "free"
                                                        else "stripe"
                                                    )
                                            , eventTicketOrderCreatedAt = now
                                            , eventTicketOrderUpdatedAt = now
                                            }
                                orderKey <- insert orderRecord
                                issuedCodes <-
                                    if zeroTotal
                                        then issueMissingTicketsForOrder now orderKey orderRecord
                                        else pure []
                                pure (Just (orderKey, orderRecord, issuedCodes, False))
                    createOrder =
                        runSqlPool
                            ( do
                                lockedEvents <-
                                    rawSql
                                        "SELECT ?? FROM social_event WHERE id = ? FOR UPDATE"
                                        [toPersistValue eventKey]
                                when (null (lockedEvents :: [Entity SocialEvent])) $
                                    fail "Event not found while reserving tickets"
                                soldAcross <-
                                    selectList [EventTicketTierEventId ==. eventKey] []
                                let soldCount =
                                        sum
                                            (map (eventTicketTierQuantitySold . entityVal) soldAcross)
                                    capacityAvailable =
                                        maybe
                                            True
                                            (\cap -> soldCount + ticketPurchaseQuantity <= cap)
                                            (socialEventCapacity eventVal)
                                if not capacityAvailable
                                    then pure Nothing
                                    else do
                                        reservedCount <-
                                            updateWhereCount
                                                [ EventTicketTierId ==. tierKey
                                                , EventTicketTierIsActive ==. True
                                                , EventTicketTierQuantitySold
                                                    <=. eventTicketTierQuantityTotal tier - ticketPurchaseQuantity
                                                ]
                                                [ EventTicketTierQuantitySold +=. ticketPurchaseQuantity
                                                , EventTicketTierUpdatedAt =. now
                                                ]
                                        if reservedCount == 0
                                            then pure Nothing
                                            else createReservedOrder
                            )
                            envPool
                createResult <- liftIO (tryAny createOrder)
                case createResult of
                    Right result -> pure result
                    Left createErr ->
                        case fromException createErr :: Maybe SomeAsyncException of
                            Just _ -> liftIO (throwIO createErr)
                            Nothing
                                | isEventTicketCheckoutConflict createErr ->
                                    case tpwpIdempotencyKey of
                                        Nothing -> liftIO (throwIO createErr)
                                        Just idempotencyKey -> do
                                            concurrentWinner <-
                                                liftIO $
                                                    runSqlPool
                                                        ( getBy
                                                            ( UniqueEventTicketCheckout
                                                                buyerParty
                                                                (Just idempotencyKey)
                                                            )
                                                        )
                                                        envPool
                                            case concurrentWinner of
                                                Nothing -> liftIO (throwIO createErr)
                                                Just (Entity winnerKey winnerOrder) -> do
                                                    winnerTickets <-
                                                        liftIO $
                                                            runSqlPool
                                                                ( selectList
                                                                    [EventTicketOrderRefId ==. winnerKey]
                                                                    [Asc EventTicketId]
                                                                )
                                                                envPool
                                                    pure . Just $
                                                        ( winnerKey
                                                        , winnerOrder
                                                        , map (eventTicketCode . entityVal) winnerTickets
                                                        , True
                                                        )
                                | otherwise -> liftIO (throwIO createErr)
        (orderKey, orderRecord, issuedCodes, reusedCheckout) <-
            maybe
                (throwError err409{errBody = "Tickets or promo code are no longer available"})
                pure
                createdOrder
        let
            currency = eventTicketOrderCurrency orderRecord
            -- Roll the order back to a state where the caller can retry without
            -- double-selling tickets or double-counting promo redemptions.
            rollbackOrder errText = do
                unless reusedCheckout $
                    liftIO $
                        runSqlPool
                            ( do
                                update
                                    orderKey
                                    [ EventTicketOrderStatus =. "cancelled"
                                    , EventTicketOrderUpdatedAt =. now
                                    ]
                                update tierKey [EventTicketTierQuantitySold +=. (negate ticketPurchaseQuantity)]
                                case mPromoCodeKey of
                                    Nothing -> pure ()
                                    Just promoKey -> update promoKey [PromoCodeCurrentRedemptions +=. (-1)]
                            )
                            envPool
                throwError err500{errBody = BL.fromStrict (TE.encodeUtf8 ("Stripe error: " <> errText))}
            storedPromoCode =
                decodeTicketCheckoutMetadata (eventTicketOrderMetadata orderRecord)
                    >>= snd
            canonicalRequestMatches =
                eventTicketOrderEventId orderRecord == eventKey
                    && eventTicketOrderTierId orderRecord == tierKey
                    && eventTicketOrderQuantity orderRecord == ticketPurchaseQuantity
                    && eventTicketOrderBuyerPartyId orderRecord == buyerParty
                    && eventTicketOrderBuyerName orderRecord == buyerName
                    && eventTicketOrderBuyerEmail orderRecord == buyerEmail
                    && eventTicketOrderCheckoutIdempotencyKey orderRecord
                        == tpwpIdempotencyKey
                    && storedPromoCode == tpwpPromoCode
        when (reusedCheckout && not canonicalRequestMatches) $
            throwError err409{errBody = "ticketPurchaseIdempotencyKey was already used for different checkout details"}
        when
            ( reusedCheckout
                && eventTicketOrderStatus orderRecord `elem` ["cancelled", "refunded"]
            )
            $ throwError err409{errBody = "Ticket checkout is already closed; start a new checkout"}
        when
            ( reusedCheckout
                && eventTicketOrderStatus orderRecord == "paid"
                && finalAmountCents > 0
                && isNothing (eventTicketOrderStripePaymentIntentId orderRecord)
            )
            $ throwError err409{errBody = "Ticket checkout is already paid"}
        if finalAmountCents == 0
            then do
                unless reusedCheckout $
                    sendTicketConfirmationForOrder orderRecord issuedCodes
                pure
                    StripePaymentIntentDTO
                        { spiClientSecret = ""
                        , spiOrderId = renderKeyText orderKey
                        , spiAmountCents = 0
                        , spiCurrency = currency
                        , spiPaymentSheet = Nothing
                        , spiLookupToken = Nothing
                        }
            else
                if reusedCheckout && isJust (eventTicketOrderStripePaymentIntentId orderRecord)
                    then reuseStripeTicketCheckout tpwpMobileSdkStripeVersion orderKey orderRecord
                    else do
                        (secretKey, webhookSecret) <-
                            case (stripeSecretKey envConfig, stripeWebhookSecret envConfig) of
                                (Just configuredSecretKey, Just configuredWebhookSecret) ->
                                    pure (configuredSecretKey, configuredWebhookSecret)
                                _ -> rollbackOrder "Stripe is not configured"
                        let stripeCfg =
                                Stripe.StripeConfig
                                    { Stripe.stripeSecretKey = secretKey
                                    , Stripe.stripeWebhookSecret = webhookSecret
                                    , Stripe.stripeApiVersion = Stripe.defaultStripeApiVersion
                                    }
                            description = "Tickets for event " <> renderKeyText eventKey
                            metadata =
                                Aeson.object
                                    [ "order_id" Aeson..= renderKeyText orderKey
                                    , "event_id" Aeson..= renderKeyText eventKey
                                    ]
                            metadataJson = Just (TE.decodeUtf8 (BL.toStrict (Aeson.encode metadata)))
                            stripeIdempotencyKey =
                                (\key -> "ticket-order-" <> currentPartyId <> "-" <> key)
                                    <$> tpwpIdempotencyKey
                            persistPaymentIntentId piId =
                                liftIO $
                                    runSqlPool
                                        (update orderKey [EventTicketOrderStripePaymentIntentId =. Just piId])
                                        envPool
                            createLegacyPaymentIntent = do
                                result <-
                                    liftIO $
                                        runStripeRequestSafely $
                                            case stripeIdempotencyKey of
                                                Nothing ->
                                                    Stripe.createPaymentIntent
                                                        stripeCfg
                                                        finalAmountCents
                                                        currency
                                                        description
                                                        metadataJson
                                                Just idempotencyKey ->
                                                    Stripe.createPaymentIntentWithIdempotencyKey
                                                        stripeCfg
                                                        idempotencyKey
                                                        finalAmountCents
                                                        currency
                                                        description
                                                        metadataJson
                                either rollbackOrder handleLegacyPaymentIntent result
                            handleLegacyPaymentIntent paymentIntent = do
                                (piId, clientSecret) <-
                                    eitherStripeServerError $
                                        parseStripePaymentIntentResponse paymentIntent
                                persistPaymentIntentId piId
                                pure
                                    StripePaymentIntentDTO
                                        { spiClientSecret = clientSecret
                                        , spiOrderId = renderKeyText orderKey
                                        , spiAmountCents = finalAmountCents
                                        , spiCurrency = currency
                                        , spiPaymentSheet = Nothing
                                        , spiLookupToken = Nothing
                                        }
                            createMobilePaymentSheetIntent mobileSdkVer = do
                                publishableKey <-
                                    maybe
                                        (rollbackOrder "Stripe publishable key not configured")
                                        pure
                                        (stripePublishableKey envConfig)
                                -- Order: customer -> ephemeral key -> PI. Failing fast on
                                -- ephemeral key creation avoids orphaning a PI we cannot return
                                -- to the mobile client.
                                buyerKey <- parseKeyOr400 "buyer party" currentPartyId
                                customerId <-
                                    resolveStripeCustomerForBuyer stripeCfg buyerKey buyerEmail buyerName
                                        `catchError` const (rollbackOrder "customer setup failed")
                                ephResult <-
                                    liftIO $
                                        runStripeRequestSafely $
                                            Stripe.createEphemeralKey stripeCfg customerId mobileSdkVer
                                ephemeralKeySecret <-
                                    either
                                        (\err -> rollbackOrder ("ephemeral key: " <> err))
                                        (eitherStripeServerError . parseStripeEphemeralKeySecret)
                                        ephResult
                                piResult <-
                                    liftIO $
                                        runStripeRequestSafely $
                                            case stripeIdempotencyKey of
                                                Nothing ->
                                                    Stripe.createPaymentIntentForCustomer
                                                        stripeCfg
                                                        customerId
                                                        finalAmountCents
                                                        currency
                                                        description
                                                        metadataJson
                                                Just idempotencyKey ->
                                                    Stripe.createPaymentIntentForCustomerWithIdempotencyKey
                                                        stripeCfg
                                                        idempotencyKey
                                                        customerId
                                                        finalAmountCents
                                                        currency
                                                        description
                                                        metadataJson
                                either
                                    rollbackOrder
                                    (handleMobilePaymentIntent customerId ephemeralKeySecret publishableKey)
                                    piResult
                            handleMobilePaymentIntent customerId ephemeralKeySecret publishableKey paymentIntent = do
                                (piId, clientSecret) <-
                                    eitherStripeServerError $
                                        parseStripePaymentIntentResponse paymentIntent
                                persistPaymentIntentId piId
                                pure
                                    StripePaymentIntentDTO
                                        { spiClientSecret = clientSecret
                                        , spiOrderId = renderKeyText orderKey
                                        , spiAmountCents = finalAmountCents
                                        , spiCurrency = currency
                                        , spiPaymentSheet =
                                            Just
                                                PaymentSheetParamsDTO
                                                    { psCustomerId = customerId
                                                    , psEphemeralKeySecret = ephemeralKeySecret
                                                    , psPaymentIntentClientSecret = clientSecret
                                                    , psPublishableKey = publishableKey
                                                    }
                                        , spiLookupToken = Nothing
                                        }
                        maybe createLegacyPaymentIntent createMobilePaymentSheetIntent tpwpMobileSdkStripeVersion

    -- Refunds
    createRefundRequest :: T.Text -> T.Text -> RefundRequestDTO -> AppM RefundDTO
    createRefundRequest eventIdStr orderIdStr RefundRequestDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        orderKey <- parseKeyOr400 "ticket order" orderIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        mOrder <- liftIO $ runSqlPool (get orderKey) envPool
        order <- maybe (throwError err404{errBody = "Ticket order not found"}) pure mOrder
        when (eventTicketOrderEventId order /= eventKey) $
            throwError err400{errBody = "Ticket order does not belong to this event"}
        when (eventTicketOrderStatus order /= "paid") $
            throwError err400{errBody = "Only paid orders can be refunded"}
        let manager = isEventManager currentPartyId eventVal
        when (not manager && eventTicketOrderBuyerPartyId order /= Just currentPartyId) $
            throwError err403{errBody = "You can only request refunds for your own orders"}
        mExisting <-
            liftIO $
                runSqlPool
                    (selectFirst [TicketRefundRequestOrderId ==. orderKey] [])
                    envPool
        when (isJust mExisting) $
            throwError err409{errBody = "Refund request already exists for this order"}
        let amountCents = fromMaybe (eventTicketOrderAmountCents order) refundRequestAmountCents
        when (amountCents > eventTicketOrderAmountCents order) $
            throwError err400{errBody = "Refund amount cannot exceed order amount"}
        when (amountCents <= 0) $ throwError err400{errBody = "Refund amount must be > 0"}
        refundKey <-
            liftIO $
                runSqlPool
                    ( insert
                        TicketRefundRequest
                            { ticketRefundRequestOrderId = orderKey
                            , ticketRefundRequestRequestedByPartyId = Just currentPartyId
                            , ticketRefundRequestReason = refundRequestReason
                            , ticketRefundRequestAmountCents = amountCents
                            , ticketRefundRequestStatus = "pending"
                            , ticketRefundRequestApprovedByPartyId = Nothing
                            , ticketRefundRequestApprovedAt = Nothing
                            , ticketRefundRequestRejectionReason = Nothing
                            , ticketRefundRequestStripeRefundId = Nothing
                            , ticketRefundRequestProcessedAt = Nothing
                            , ticketRefundRequestCreatedAt = now
                            , ticketRefundRequestUpdatedAt = now
                            }
                    )
                    envPool
        mRefund <- liftIO $ runSqlPool (getEntity refundKey) envPool
        maybe
            (throwError err500{errBody = "Could not create refund request"})
            (pure . refundEntityToDTO (eventTicketOrderCurrency order))
            mRefund

    listRefunds :: T.Text -> AppM [RefundDTO]
    listRefunds eventIdStr = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        let manager = isEventManager currentPartyId eventVal
        orders <-
            if manager
                then
                    liftIO $
                        runSqlPool
                            (selectList [EventTicketOrderEventId ==. eventKey] [])
                            envPool
                else
                    liftIO $
                        runSqlPool
                            (selectList [EventTicketOrderEventId ==. eventKey, EventTicketOrderBuyerPartyId ==. Just currentPartyId] [])
                            envPool
        let orderIds = map entityKey orders
            orderCurrencies =
                Map.fromList
                    [ (entityKey orderEntity, eventTicketOrderCurrency (entityVal orderEntity))
                    | orderEntity <- orders
                    ]
        refunds <-
            liftIO $
                runSqlPool
                    (selectList [TicketRefundRequestOrderId <-. orderIds] [Desc TicketRefundRequestCreatedAt])
                    envPool
        forM refunds $ \refundEntity@(Entity _ refundRow) ->
            case Map.lookup (ticketRefundRequestOrderId refundRow) orderCurrencies of
                Just currency -> pure (refundEntityToDTO currency refundEntity)
                Nothing -> throwError err500{errBody = "Refund order currency not found"}

    approveRefund :: T.Text -> T.Text -> AppM RefundDTO
    approveRefund eventIdStr refundIdStr = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, _) <- requireManagedEvent eventIdStr
        refundKey <- parseKeyOr400 "refund" refundIdStr
        mRefund <- liftIO $ runSqlPool (get refundKey) envPool
        refund <- maybe (throwError err404{errBody = "Refund request not found"}) pure mRefund
        let orderKey = ticketRefundRequestOrderId refund
        mOrder <- liftIO $ runSqlPool (get orderKey) envPool
        order <- maybe (throwError err404{errBody = "Ticket order not found"}) pure mOrder
        when (eventTicketOrderEventId order /= eventKey) $
            throwError err400{errBody = "Refund does not belong to this event"}
        when (ticketRefundRequestStatus refund /= "pending") $
            throwError err400{errBody = "Refund request is not pending"}
        case (eventTicketOrderStripePaymentIntentId order, stripeSecretKey envConfig, stripeWebhookSecret envConfig) of
            (Just piId, Just secretKey, Just webhookSecret) -> do
                let stripeCfg =
                        Stripe.StripeConfig
                            { Stripe.stripeSecretKey = secretKey
                            , Stripe.stripeWebhookSecret = webhookSecret
                            , Stripe.stripeApiVersion = Stripe.defaultStripeApiVersion
                            }
                result <- liftIO $ Stripe.createRefund stripeCfg piId (ticketRefundRequestAmountCents refund)
                case result of
                    Left err -> throwError err500{errBody = BL.fromStrict (TE.encodeUtf8 ("Stripe refund error: " <> err))}
                    Right refundResponse -> do
                        refundId <-
                            eitherStripeServerError $
                                parseStripeRefundResponse refundResponse
                        liftIO $
                            runSqlPool
                                ( do
                                    update
                                        refundKey
                                        [ TicketRefundRequestStatus =. "approved"
                                        , TicketRefundRequestApprovedByPartyId =. Just currentPartyId
                                        , TicketRefundRequestApprovedAt =. Just now
                                        , TicketRefundRequestStripeRefundId =. Just refundId
                                        , TicketRefundRequestProcessedAt =. Just now
                                        , TicketRefundRequestUpdatedAt =. now
                                        ]
                                    update orderKey [EventTicketOrderStatus =. "refunded", EventTicketOrderUpdatedAt =. now]
                                    updateWhere
                                        [EventTicketOrderRefId ==. orderKey]
                                        [EventTicketStatus =. "refunded", EventTicketUpdatedAt =. now]
                                    update
                                        (eventTicketOrderTierId order)
                                        [EventTicketTierQuantitySold +=. (negate (eventTicketOrderQuantity order))]
                                )
                                envPool
                        mUpdated <- liftIO $ runSqlPool (getEntity refundKey) envPool
                        maybe
                            (throwError err500{errBody = "Could not approve refund"})
                            (pure . refundEntityToDTO (eventTicketOrderCurrency order))
                            mUpdated
            _ -> throwError err500{errBody = "Cannot process refund: Stripe not configured or order has no payment intent"}

    rejectRefund :: T.Text -> T.Text -> RejectionReasonDTO -> AppM RefundDTO
    rejectRefund eventIdStr refundIdStr RejectionReasonDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, _) <- requireManagedEvent eventIdStr
        refundKey <- parseKeyOr400 "refund" refundIdStr
        mRefund <- liftIO $ runSqlPool (get refundKey) envPool
        refund <- maybe (throwError err404{errBody = "Refund request not found"}) pure mRefund
        let orderKey = ticketRefundRequestOrderId refund
        mOrder <- liftIO $ runSqlPool (get orderKey) envPool
        order <- maybe (throwError err404{errBody = "Ticket order not found"}) pure mOrder
        when (eventTicketOrderEventId order /= eventKey) $
            throwError err400{errBody = "Refund does not belong to this event"}
        when (ticketRefundRequestStatus refund /= "pending") $
            throwError err400{errBody = "Refund request is not pending"}
        when (T.null (T.strip rrReason)) $
            throwError err400{errBody = "Rejection reason is required"}
        liftIO $
            runSqlPool
                ( update
                    refundKey
                    [ TicketRefundRequestStatus =. "rejected"
                    , TicketRefundRequestRejectionReason =. Just rrReason
                    , TicketRefundRequestUpdatedAt =. now
                    ]
                )
                envPool
        mUpdated <- liftIO $ runSqlPool (getEntity refundKey) envPool
        maybe
            (throwError err500{errBody = "Could not reject refund"})
            (pure . refundEntityToDTO (eventTicketOrderCurrency order))
            mUpdated

    -- Transfers
    createTransfer :: T.Text -> T.Text -> TicketTransferCreateDTO -> AppM TicketTransferDTO
    createTransfer eventIdStr ticketIdStr TicketTransferCreateDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        ticketKey <- parseKeyOr400 "ticket" ticketIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        mTicket <- liftIO $ runSqlPool (get ticketKey) envPool
        ticket <- maybe (throwError err404{errBody = "Ticket not found"}) pure mTicket
        when (eventTicketEventId ticket /= eventKey) $
            throwError err400{errBody = "Ticket does not belong to this event"}
        let manager = isEventManager currentPartyId eventVal
        when (not manager && eventTicketCurrentHolderPartyId ticket /= Just currentPartyId) $
            throwError err403{errBody = "You can only transfer your own tickets"}
        when (eventTicketStatus ticket `elem` ["cancelled", "refunded", "checked_in"]) $
            throwError err400{errBody = "Cannot transfer this ticket"}
        mExistingTransfer <-
            liftIO $
                runSqlPool
                    (selectFirst [TicketTransferTicketId ==. ticketKey, TicketTransferStatus ==. "pending"] [])
                    envPool
        when (isJust mExistingTransfer) $
            throwError err409{errBody = "A pending transfer already exists for this ticket"}
        transferCode <- liftIO $ do
            code1 <- Random.randomRIO (100000 :: Int, 999999 :: Int)
            code2 <- Random.randomRIO (100000 :: Int, 999999 :: Int)
            pure (T.pack (show code1 ++ "-" ++ show code2))
        let expiresAt = addUTCTime (48 * 3600) now
        transferKey <-
            liftIO $
                runSqlPool
                    ( insert
                        TicketTransfer
                            { ticketTransferTicketId = ticketKey
                            , ticketTransferFromPartyId = Just currentPartyId
                            , ticketTransferToPartyId = Nothing
                            , ticketTransferToEmail = Just ttcToEmail
                            , ticketTransferToName = ttcToName
                            , ticketTransferStatus = "pending"
                            , ticketTransferTransferCode = transferCode
                            , ticketTransferMessage = ttcMessage
                            , ticketTransferExpiresAt = Just expiresAt
                            , ticketTransferAcceptedAt = Nothing
                            , ticketTransferCreatedAt = now
                            , ticketTransferUpdatedAt = now
                            }
                    )
                    envPool
        mTransfer <- liftIO $ runSqlPool (getEntity transferKey) envPool
        maybe
            (throwError err500{errBody = "Could not create transfer"})
            (pure . transferEntityToDTO)
            mTransfer

    listTransfers :: T.Text -> T.Text -> AppM [TicketTransferDTO]
    listTransfers eventIdStr ticketIdStr = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" eventIdStr
        ticketKey <- parseKeyOr400 "ticket" ticketIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        mTicket <- liftIO $ runSqlPool (get ticketKey) envPool
        ticket <- maybe (throwError err404{errBody = "Ticket not found"}) pure mTicket
        when (eventTicketEventId ticket /= eventKey) $
            throwError err400{errBody = "Ticket does not belong to this event"}
        let manager = isEventManager currentPartyId eventVal
        when (not manager && eventTicketCurrentHolderPartyId ticket /= Just currentPartyId) $
            throwError err403{errBody = "You can only view transfers for your own tickets"}
        transfers <-
            liftIO $
                runSqlPool
                    (selectList [TicketTransferTicketId ==. ticketKey] [Desc TicketTransferCreatedAt])
                    envPool
        pure (map transferEntityToDTO transfers)

    acceptTransfer :: T.Text -> AppM TicketDTO
    acceptTransfer transferCode = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        mTransferEnt <- liftIO $ runSqlPool (getBy (UniqueTicketTransferCode transferCode)) envPool
        transferEnt <- maybe (throwError err404{errBody = "Transfer not found"}) pure mTransferEnt
        let transferKey = entityKey transferEnt
            transfer = entityVal transferEnt
        when (ticketTransferStatus transfer /= "pending") $
            throwError err400{errBody = "Transfer is not pending"}
        case ticketTransferExpiresAt transfer of
            Just expiresAt
                | now > expiresAt ->
                    throwError err400{errBody = "Transfer has expired"}
            _ -> pure ()
        let ticketKey = ticketTransferTicketId transfer
        mTicket <- liftIO $ runSqlPool (get ticketKey) envPool
        ticket <- maybe (throwError err404{errBody = "Ticket not found"}) pure mTicket
        when (eventTicketStatus ticket `elem` ["cancelled", "refunded", "checked_in"]) $
            throwError err400{errBody = "Cannot accept transfer for this ticket"}
        liftIO $
            runSqlPool
                ( do
                    update
                        transferKey
                        [ TicketTransferStatus =. "completed"
                        , TicketTransferToPartyId =. Just currentPartyId
                        , TicketTransferAcceptedAt =. Just now
                        , TicketTransferUpdatedAt =. now
                        ]
                    update
                        ticketKey
                        [ EventTicketCurrentHolderPartyId =. Just currentPartyId
                        , EventTicketCurrentHolderEmail =. (ticketTransferToEmail transfer <|> eventTicketCurrentHolderEmail ticket)
                        , EventTicketCurrentHolderName =. (ticketTransferToName transfer <|> eventTicketCurrentHolderName ticket)
                        , EventTicketUpdatedAt =. now
                        ]
                )
                envPool
        mUpdated <- liftIO $ runSqlPool (getEntity ticketKey) envPool
        maybe
            (throwError err500{errBody = "Could not accept transfer"})
            (pure . ticketEntityToDTO)
            mUpdated

    cancelTransfer :: T.Text -> AppM TicketTransferDTO
    cancelTransfer transferIdStr = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        transferKey <- parseKeyOr400 "transfer" transferIdStr
        mTransfer <- liftIO $ runSqlPool (get transferKey) envPool
        transfer <- maybe (throwError err404{errBody = "Transfer not found"}) pure mTransfer
        when (ticketTransferFromPartyId transfer /= Just currentPartyId) $
            throwError err403{errBody = "You can only cancel your own transfers"}
        when (ticketTransferStatus transfer /= "pending") $
            throwError err400{errBody = "Transfer is not pending"}
        liftIO $
            runSqlPool
                ( update
                    transferKey
                    [ TicketTransferStatus =. "cancelled"
                    , TicketTransferUpdatedAt =. now
                    ]
                )
                envPool
        mUpdated <- liftIO $ runSqlPool (getEntity transferKey) envPool
        maybe
            (throwError err500{errBody = "Could not cancel transfer"})
            (pure . transferEntityToDTO)
            mUpdated

    -- Waitlist
    joinWaitlist :: T.Text -> WaitlistJoinDTO -> AppM WaitlistEntryDTO
    joinWaitlist eventIdStr WaitlistJoinDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        _ <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        mTierKey <- case cleanMaybeText wjTierId of
            Nothing -> pure Nothing
            Just tierIdStr -> do
                tierKey <- parseKeyOr400 "ticket tier" tierIdStr
                mTier <- liftIO $ runSqlPool (get tierKey) envPool
                tier <- maybe (throwError err404{errBody = "Ticket tier not found"}) pure mTier
                when (eventTicketTierEventId tier /= eventKey) $
                    throwError err400{errBody = "Ticket tier does not belong to this event"}
                pure (Just tierKey)
        when (wjQuantity < 1 || wjQuantity > 10) $
            throwError err400{errBody = "Quantity must be between 1 and 10"}
        mExisting <-
            liftIO $
                runSqlPool
                    ( selectFirst
                        [EventWaitlistEventId ==. eventKey, EventWaitlistEmail ==. wjEmail, EventWaitlistStatus ==. "active"]
                        []
                    )
                    envPool
        when (isJust mExisting) $
            throwError err409{errBody = "Already on waitlist for this event"}
        waitlistKey <-
            liftIO $
                runSqlPool
                    ( insert
                        EventWaitlist
                            { eventWaitlistEventId = eventKey
                            , eventWaitlistTierId = mTierKey
                            , eventWaitlistPartyId = Nothing
                            , eventWaitlistEmail = wjEmail
                            , eventWaitlistName = wjName
                            , eventWaitlistQuantity = wjQuantity
                            , eventWaitlistStatus = "active"
                            , eventWaitlistPriority = 0
                            , eventWaitlistNotifiedAt = Nothing
                            , eventWaitlistExpiresAt = Nothing
                            , eventWaitlistConvertedOrderId = Nothing
                            , eventWaitlistCreatedAt = now
                            , eventWaitlistUpdatedAt = now
                            }
                    )
                    envPool
        mWaitlist <- liftIO $ runSqlPool (getEntity waitlistKey) envPool
        maybe
            (throwError err500{errBody = "Could not join waitlist"})
            (pure . waitlistEntityToDTO)
            mWaitlist

    listWaitlist :: T.Text -> Maybe T.Text -> AppM [WaitlistEntryDTO]
    listWaitlist eventIdStr mStatus = do
        Env{..} <- ask
        (eventKey, _) <- requireManagedEvent eventIdStr
        let statusFilters = case cleanMaybeText mStatus of
                Nothing -> []
                Just status -> [EventWaitlistStatus ==. status]
            filters = [EventWaitlistEventId ==. eventKey] ++ statusFilters
        entries <-
            liftIO $
                runSqlPool
                    (selectList filters [Asc EventWaitlistPriority, Asc EventWaitlistCreatedAt])
                    envPool
        pure (map waitlistEntityToDTO entries)

    notifyWaitlist :: T.Text -> T.Text -> AppM WaitlistEntryDTO
    notifyWaitlist eventIdStr waitlistIdStr = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, _) <- requireManagedEvent eventIdStr
        waitlistKey <- parseKeyOr400 "waitlist entry" waitlistIdStr
        mWaitlist <- liftIO $ runSqlPool (get waitlistKey) envPool
        waitlist <- maybe (throwError err404{errBody = "Waitlist entry not found"}) pure mWaitlist
        when (eventWaitlistEventId waitlist /= eventKey) $
            throwError err400{errBody = "Waitlist entry does not belong to this event"}
        when (eventWaitlistStatus waitlist /= "active") $
            throwError err400{errBody = "Waitlist entry is not active"}
        let expiresAt = addUTCTime (24 * 3600) now
        liftIO $
            runSqlPool
                ( update
                    waitlistKey
                    [ EventWaitlistStatus =. "notified"
                    , EventWaitlistNotifiedAt =. Just now
                    , EventWaitlistExpiresAt =. Just expiresAt
                    , EventWaitlistUpdatedAt =. now
                    ]
                )
                envPool
        mUpdated <- liftIO $ runSqlPool (getEntity waitlistKey) envPool
        maybe
            (throwError err500{errBody = "Could not notify waitlist entry"})
            (pure . waitlistEntityToDTO)
            mUpdated

    removeFromWaitlist :: T.Text -> T.Text -> AppM NoContent
    removeFromWaitlist eventIdStr waitlistIdStr = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        waitlistKey <- parseKeyOr400 "waitlist entry" waitlistIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        mWaitlist <- liftIO $ runSqlPool (get waitlistKey) envPool
        waitlist <- maybe (throwError err404{errBody = "Waitlist entry not found"}) pure mWaitlist
        when (eventWaitlistEventId waitlist /= eventKey) $
            throwError err400{errBody = "Waitlist entry does not belong to this event"}
        let manager = isEventManager currentPartyId eventVal
        when (not manager) $
            throwError err403{errBody = "Only event managers can remove waitlist entries"}
        liftIO $
            runSqlPool
                ( update
                    waitlistKey
                    [ EventWaitlistStatus =. "removed"
                    , EventWaitlistUpdatedAt =. now
                    ]
                )
                envPool
        pure NoContent

    -- QR Code
    getTicketQR :: T.Text -> T.Text -> AppM TicketWithQRDTO
    getTicketQR eventIdStr ticketIdStr = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        eventKey <- parseKeyOr400 "event" eventIdStr
        ticketKey <- parseKeyOr400 "ticket" ticketIdStr
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        mTicket <- liftIO $ runSqlPool (get ticketKey) envPool
        ticket <- maybe (throwError err404{errBody = "Ticket not found"}) pure mTicket
        when (eventTicketEventId ticket /= eventKey) $
            throwError err400{errBody = "Ticket does not belong to this event"}
        let manager = isEventManager currentPartyId eventVal
        when (not manager && eventTicketCurrentHolderPartyId ticket /= Just currentPartyId) $
            throwError err403{errBody = "You can only view QR codes for your own tickets"}
        mExistingQR <- liftIO $ runSqlPool (getBy (UniqueTicketQRCode ticketKey)) envPool
        qrData <-
            maybe
                ( do
                    let timestamp = T.pack (show (floor (realToFrac (utcTimeToPOSIXSeconds now) :: Double) :: Int))
                        payload =
                            T.intercalate
                                "|"
                                [ renderKeyText ticketKey
                                , renderKeyText eventKey
                                , fromMaybe "" (eventTicketHolderEmail ticket)
                                , timestamp
                                ]
                        secret = "tdf-qr-secret-key"
                        hmacHex =
                            T.pack $
                                show (hmacGetDigest (hmac (TE.encodeUtf8 secret) (TE.encodeUtf8 payload) :: HMAC SHA256))
                        qrDataValue = payload <> "|" <> hmacHex
                    liftIO $
                        runSqlPool
                            ( insert_
                                TicketQRCode
                                    { ticketQRCodeTicketId = ticketKey
                                    , ticketQRCodeQrData = qrDataValue
                                    , ticketQRCodeQrImageUrl = Nothing
                                    , ticketQRCodeGeneratedAt = now
                                    }
                            )
                            envPool
                    pure qrDataValue
                )
                (pure . ticketQRCodeQrData . entityVal)
                mExistingQR
        mTicketEnt <- liftIO $ runSqlPool (getEntity ticketKey) envPool
        ticketDto <-
            maybe
                (throwError err500{errBody = "Could not load ticket"})
                (pure . ticketEntityToDTO)
                mTicketEnt
        pure
            TicketWithQRDTO
                { twqTicket = ticketDto
                , twqQRData = qrData
                , twqQRImageUrl = Nothing
                }

    -- Budget
    budgetServer :: ServerT BudgetRoutes AppM
    budgetServer =
        listBudgetLines
            :<|> createBudgetLine
            :<|> updateBudgetLine

    listBudgetLines :: T.Text -> AppM [EventBudgetLineDTO]
    listBudgetLines eventIdStr = do
        Env{..} <- ask
        (eventKey, _) <- requireManagedEvent eventIdStr
        budgetRows <-
            liftIO $
                runSqlPool
                    (selectList [EventBudgetLineEventId ==. eventKey] [Asc EventBudgetLineLineType, Asc EventBudgetLineCategory, Asc EventBudgetLineCode])
                    envPool
        postedEntries <-
            liftIO $
                runSqlPool
                    (selectList [EventFinanceEntryEventId ==. eventKey, EventFinanceEntryStatus ==. "posted"] [])
                    envPool
        pure $
            map
                ( \lineEnt@(Entity lineKey lineRec) ->
                    let lineTypeVal = normalizeBudgetLineType (Just (eventBudgetLineLineType lineRec))
                        actualCents =
                            sum
                                [ eventFinanceEntryAmountCents entry
                                | Entity _ entry <- postedEntries
                                , eventFinanceEntryBudgetLineId entry == Just lineKey
                                , normalizeFinanceDirection (Just (eventFinanceEntryDirection entry)) == lineTypeVal
                                ]
                     in budgetLineEntityToDTO eventKey (Just actualCents) lineEnt
                )
                budgetRows

    createBudgetLine :: T.Text -> EventBudgetLineDTO -> AppM EventBudgetLineDTO
    createBudgetLine eventIdStr dto = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, _) <- requireManagedEvent eventIdStr
        let lineName = T.strip (eblName dto)
        when (T.null lineName) $ throwError err400{errBody = "budget line name is required"}
        when (eblPlannedCents dto < 0) $ throwError err400{errBody = "planned cents must be >= 0"}
        lineTypeVal <- either throwError pure (validateBudgetLineTypeInput (eblType dto))
        let codeVal = normalizeBudgetLineCode (fromMaybe lineName (cleanMaybeText (Just (eblCode dto))))
            categoryVal = normalizeCategory (Just (eblCategory dto))
        mInserted <-
            liftIO $
                runSqlPool
                    ( insertUnique
                        EventBudgetLine
                            { eventBudgetLineEventId = eventKey
                            , eventBudgetLineCode = codeVal
                            , eventBudgetLineName = lineName
                            , eventBudgetLineLineType = lineTypeVal
                            , eventBudgetLineCategory = categoryVal
                            , eventBudgetLinePlannedCents = eblPlannedCents dto
                            , eventBudgetLineNotes = cleanMaybeText (eblNotes dto)
                            , eventBudgetLineCreatedAt = now
                            , eventBudgetLineUpdatedAt = now
                            }
                    )
                    envPool
        lineKey <- maybe (throwError err409{errBody = "budget line code already exists for this event"}) pure mInserted
        mLine <- liftIO $ runSqlPool (getEntity lineKey) envPool
        maybe
            (throwError err500{errBody = "Could not create budget line"})
            (pure . budgetLineEntityToDTO eventKey (Just 0))
            mLine

    updateBudgetLine :: T.Text -> T.Text -> EventBudgetLineDTO -> AppM EventBudgetLineDTO
    updateBudgetLine eventIdStr lineIdStr dto = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, _) <- requireManagedEvent eventIdStr
        lineKey <- parseKeyOr400 "budget line" lineIdStr
        mLine <- liftIO $ runSqlPool (get lineKey) envPool
        lineRec <- maybe (throwError err404{errBody = "Budget line not found"}) pure mLine
        when (eventBudgetLineEventId lineRec /= eventKey) $ throwError err400{errBody = "Budget line does not belong to this event"}
        let lineName = T.strip (eblName dto)
        when (T.null lineName) $ throwError err400{errBody = "budget line name is required"}
        when (eblPlannedCents dto < 0) $ throwError err400{errBody = "planned cents must be >= 0"}
        lineTypeVal <- either throwError pure (validateBudgetLineTypeInput (eblType dto))
        let codeVal = normalizeBudgetLineCode (fromMaybe lineName (cleanMaybeText (Just (eblCode dto))))
            categoryVal = normalizeCategory (Just (eblCategory dto))
        mCodeOwner <- liftIO $ runSqlPool (getBy (UniqueEventBudgetLineCode eventKey codeVal)) envPool
        case mCodeOwner of
            Just (Entity existingKey _)
                | existingKey /= lineKey ->
                    throwError err409{errBody = "budget line code already exists for this event"}
            _ -> pure ()
        liftIO $
            runSqlPool
                ( update
                    lineKey
                    [ EventBudgetLineCode =. codeVal
                    , EventBudgetLineName =. lineName
                    , EventBudgetLineLineType =. lineTypeVal
                    , EventBudgetLineCategory =. categoryVal
                    , EventBudgetLinePlannedCents =. eblPlannedCents dto
                    , EventBudgetLineNotes =. cleanMaybeText (eblNotes dto)
                    , EventBudgetLineUpdatedAt =. now
                    ]
                )
                envPool
        postedEntries <-
            liftIO $
                runSqlPool
                    ( selectList
                        [ EventFinanceEntryEventId ==. eventKey
                        , EventFinanceEntryBudgetLineId ==. Just lineKey
                        , EventFinanceEntryStatus ==. "posted"
                        ]
                        []
                    )
                    envPool
        mUpdated <- liftIO $ runSqlPool (getEntity lineKey) envPool
        let actualCents =
                sum
                    [ eventFinanceEntryAmountCents entry
                    | Entity _ entry <- postedEntries
                    , normalizeFinanceDirection (Just (eventFinanceEntryDirection entry)) == lineTypeVal
                    ]
        maybe
            (throwError err500{errBody = "Could not update budget line"})
            (pure . budgetLineEntityToDTO eventKey (Just actualCents))
            mUpdated

    -- Finance
    financeServer :: ServerT FinanceRoutes AppM
    financeServer =
        listFinanceEntries
            :<|> createFinanceEntry
            :<|> updateFinanceEntry
            :<|> getFinanceSummary

    listFinanceEntries :: T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> AppM [EventFinanceEntryDTO]
    listFinanceEntries eventIdStr mDirection mSource mStatus = do
        Env{..} <- ask
        (eventKey, _) <- requireManagedEvent eventIdStr
        directionFilter <- normalizeFinanceDirectionFilter mDirection
        sourceFilter <- normalizeFinanceSourceFilter mSource
        statusFilter <- normalizeFinanceEntryStatusFilter mStatus
        manualRows <-
            liftIO $
                runSqlPool
                    (selectList [EventFinanceEntryEventId ==. eventKey] [Desc EventFinanceEntryOccurredAt, Desc EventFinanceEntryId])
                    envPool
        ticketOrders <-
            liftIO $
                runSqlPool
                    (selectList [EventTicketOrderEventId ==. eventKey] [Desc EventTicketOrderPurchasedAt, Desc EventTicketOrderId])
                    envPool
        manualDtos <-
            either
                (throwError . financeInvariantServerError)
                pure
                (traverse financeEntryEntityToDTOEither manualRows)
        ticketDtos <-
            either
                (throwError . financeInvariantServerError)
                pure
                (fmap concat (traverse (ticketOrderAccountingEntriesEither eventKey) ticketOrders))
        let merged = manualDtos ++ ticketDtos
            filtered = filter (matchesFinanceFilters directionFilter sourceFilter statusFilter) merged
        pure (sortOn (Down . efeOccurredAt) filtered)

    createFinanceEntry :: T.Text -> EventFinanceEntryDTO -> AppM EventFinanceEntryDTO
    createFinanceEntry eventIdStr dto = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, eventRec) <- requireManagedEvent eventIdStr
        directionVal <- normalizeFinanceDirectionInput (efeDirection dto)
        sourceVal <- normalizeFinanceSourceInput (efeSource dto)
        statusVal <- normalizeFinanceEntryStatusInput (efeStatus dto)
        when (sourceVal `elem` ["ticket_sale", "ticket_refund"]) $
            throwError err400{errBody = "ticket_sale and ticket_refund entries are generated from ticket orders"}
        (eventCurrencyVal, _) <-
            either
                (throwError . storedEventMetadataServerError)
                pure
                (validateStoredEventFinanceMetadata (defaultCurrency envConfig) eventRec)
        let categoryVal = normalizeCategory (Just (efeCategory dto))
            conceptVal = T.strip (efeConcept dto)
            amountVal = efeAmountCents dto
        currencyVal <-
            either
                throwError
                pure
                (validateFinanceEntryCurrencyInput eventCurrencyVal (efeCurrency dto))
        when (T.null conceptVal) $ throwError err400{errBody = "concept is required"}
        when (amountVal <= 0) $ throwError err400{errBody = "amountCents must be greater than 0"}
        budgetLineKey <- resolveBudgetLineKey envPool eventKey (efeBudgetLineId dto)
        entryKey <-
            liftIO $
                runSqlPool
                    ( insert
                        EventFinanceEntry
                            { eventFinanceEntryEventId = eventKey
                            , eventFinanceEntryBudgetLineId = budgetLineKey
                            , eventFinanceEntryDirection = directionVal
                            , eventFinanceEntrySource = sourceVal
                            , eventFinanceEntryCategory = categoryVal
                            , eventFinanceEntryConcept = conceptVal
                            , eventFinanceEntryAmountCents = amountVal
                            , eventFinanceEntryCurrency = currencyVal
                            , eventFinanceEntryStatus = statusVal
                            , eventFinanceEntryExternalRef = cleanMaybeText (efeExternalRef dto)
                            , eventFinanceEntryNotes = cleanMaybeText (efeNotes dto)
                            , eventFinanceEntryMetadata = Nothing
                            , eventFinanceEntryOccurredAt = efeOccurredAt dto
                            , eventFinanceEntryRecordedByPartyId = Just currentPartyId
                            , eventFinanceEntryCreatedAt = now
                            , eventFinanceEntryUpdatedAt = now
                            }
                    )
                    envPool
        mCreated <- liftIO $ runSqlPool (getEntity entryKey) envPool
        created <- maybe (throwError err500{errBody = "Could not create finance entry"}) pure mCreated
        either (throwError . financeInvariantServerError) pure (financeEntryEntityToDTOEither created)

    updateFinanceEntry :: T.Text -> T.Text -> EventFinanceEntryDTO -> AppM EventFinanceEntryDTO
    updateFinanceEntry eventIdStr entryIdStr dto = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, eventRec) <- requireManagedEvent eventIdStr
        entryKey <- parseKeyOr400 "finance entry" entryIdStr
        mExisting <- liftIO $ runSqlPool (get entryKey) envPool
        existing <- maybe (throwError err404{errBody = "Finance entry not found"}) pure mExisting
        when (eventFinanceEntryEventId existing /= eventKey) $ throwError err400{errBody = "Finance entry does not belong to this event"}
        directionVal <- normalizeFinanceDirectionInput (efeDirection dto)
        sourceVal <- normalizeFinanceSourceInput (efeSource dto)
        statusVal <- normalizeFinanceEntryStatusInput (efeStatus dto)
        when (sourceVal `elem` ["ticket_sale", "ticket_refund"]) $
            throwError err400{errBody = "ticket_sale and ticket_refund entries are generated from ticket orders"}
        (eventCurrencyVal, _) <-
            either
                (throwError . storedEventMetadataServerError)
                pure
                (validateStoredEventFinanceMetadata (defaultCurrency envConfig) eventRec)
        let categoryVal = normalizeCategory (Just (efeCategory dto))
            conceptVal = T.strip (efeConcept dto)
            amountVal = efeAmountCents dto
        currencyVal <-
            either
                throwError
                pure
                (validateFinanceEntryCurrencyInput eventCurrencyVal (efeCurrency dto))
        when (T.null conceptVal) $ throwError err400{errBody = "concept is required"}
        when (amountVal <= 0) $ throwError err400{errBody = "amountCents must be greater than 0"}
        budgetLineKey <- resolveBudgetLineKey envPool eventKey (efeBudgetLineId dto)
        liftIO $
            runSqlPool
                ( update
                    entryKey
                    [ EventFinanceEntryBudgetLineId =. budgetLineKey
                    , EventFinanceEntryDirection =. directionVal
                    , EventFinanceEntrySource =. sourceVal
                    , EventFinanceEntryCategory =. categoryVal
                    , EventFinanceEntryConcept =. conceptVal
                    , EventFinanceEntryAmountCents =. amountVal
                    , EventFinanceEntryCurrency =. currencyVal
                    , EventFinanceEntryStatus =. statusVal
                    , EventFinanceEntryExternalRef =. cleanMaybeText (efeExternalRef dto)
                    , EventFinanceEntryNotes =. cleanMaybeText (efeNotes dto)
                    , EventFinanceEntryOccurredAt =. efeOccurredAt dto
                    , EventFinanceEntryRecordedByPartyId =. Just currentPartyId
                    , EventFinanceEntryUpdatedAt =. now
                    ]
                )
                envPool
        mUpdated <- liftIO $ runSqlPool (getEntity entryKey) envPool
        updated <- maybe (throwError err500{errBody = "Could not update finance entry"}) pure mUpdated
        either (throwError . financeInvariantServerError) pure (financeEntryEntityToDTOEither updated)

    getFinanceSummary :: T.Text -> AppM EventFinanceSummaryDTO
    getFinanceSummary eventIdStr = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, eventRec) <- requireManagedEvent eventIdStr
        (eventCurrencyVal, budgetOverride) <-
            either
                (throwError . storedEventMetadataServerError)
                pure
                (validateStoredEventFinanceMetadata (defaultCurrency envConfig) eventRec)
        budgetRows <- liftIO $ runSqlPool (selectList [EventBudgetLineEventId ==. eventKey] []) envPool
        allFinanceRows <-
            liftIO $
                runSqlPool
                    (selectList [EventFinanceEntryEventId ==. eventKey] [])
                    envPool
        ticketOrders <- liftIO $ runSqlPool (selectList [EventTicketOrderEventId ==. eventKey] []) envPool
        normalizedBudgetRows <-
            either
                (throwError . financeInvariantServerError)
                pure
                (traverse storedBudgetLineSummaryFields budgetRows)
        normalizedFinanceRows <-
            either
                (throwError . financeInvariantServerError)
                pure
                (traverse storedFinanceEntrySummaryFields allFinanceRows)
        normalizedTicketOrders <-
            either
                (throwError . financeInvariantServerError)
                pure
                (traverse storedTicketOrderSummaryFields ticketOrders)

        let plannedIncomeCents =
                sum
                    [ plannedCents
                    | (plannedCents, lineTypeVal) <- normalizedBudgetRows
                    , lineTypeVal == "income"
                    ]
            plannedExpenseCents =
                sum
                    [ plannedCents
                    | (plannedCents, lineTypeVal) <- normalizedBudgetRows
                    , lineTypeVal == "expense"
                    ]
            entryAmount (amountCents, _, _, _) = amountCents
            entryDirection (_, directionVal, _, _) = directionVal
            entrySource (_, _, sourceVal, _) = sourceVal
            entryStatus (_, _, _, statusVal) = statusVal
            isPosted entry = entryStatus entry == "posted"
            isPendingLike entry =
                let statusVal = entryStatus entry
                 in statusVal == "pending" || statusVal == "draft"
            isNonVoid entry = entryStatus entry /= "void"
            manualIncomeCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isPosted entry
                    , entryDirection entry == "income"
                    ]
            manualExpenseCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isPosted entry
                    , entryDirection entry == "expense"
                    ]
            ticketPaidRevenueCents =
                sum
                    [ amountCents
                    | (amountCents, statusVal) <- normalizedTicketOrders
                    , statusVal == "paid"
                    ]
            ticketRefundedRevenueCents =
                sum
                    [ amountCents
                    | (amountCents, statusVal) <- normalizedTicketOrders
                    , statusVal == "refunded"
                    ]
            ticketPendingRevenueCents =
                sum
                    [ amountCents
                    | (amountCents, statusVal) <- normalizedTicketOrders
                    , statusVal == "pending"
                    ]
            accountsPayableCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isPendingLike entry
                    , entryDirection entry == "expense"
                    ]
            accountsReceivableManualCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isPendingLike entry
                    , entryDirection entry == "income"
                    ]
            accountsReceivableCents = accountsReceivableManualCents + ticketPendingRevenueCents
            contractCommittedCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isNonVoid entry
                    , entrySource entry == "contract_commitment"
                    ]
            contractPaidCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isPosted entry
                    , entrySource entry == "contract_payment"
                    ]
            procurementCommittedCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isNonVoid entry
                    , entrySource entry == "purchase_order"
                    ]
            procurementPaidCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isPosted entry
                    , entrySource entry == "purchase_payment"
                    ]
            assetInvestmentCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isPosted entry
                    , entrySource entry == "asset_purchase"
                    ]
            liabilityIncurredCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isPosted entry
                    , entrySource entry == "liability_loan"
                    , entryDirection entry == "income"
                    ]
            liabilityPaidCents =
                sum
                    [ entryAmount entry
                    | entry <- normalizedFinanceRows
                    , isPosted entry
                    , entrySource entry == "liability_payment"
                    , entryDirection entry == "expense"
                    ]
            liabilityBalanceCents = liabilityIncurredCents - liabilityPaidCents
            actualIncomeCents = manualIncomeCents + ticketPaidRevenueCents
            actualExpenseCents = manualExpenseCents + ticketRefundedRevenueCents
            netCents = actualIncomeCents - actualExpenseCents
            budgetCentsVal = budgetOverride <|> fallbackBudget plannedExpenseCents
            budgetVarianceCents = fmap (\budgetCap -> budgetCap - actualExpenseCents) budgetCentsVal
            budgetUtilizationPct =
                case budgetCentsVal of
                    Just budgetCap
                        | budgetCap > 0 ->
                            Just ((fromIntegral actualExpenseCents / fromIntegral budgetCap) * 100)
                    _ -> Nothing

        pure
            EventFinanceSummaryDTO
                { efsEventId = renderKeyText eventKey
                , efsCurrency = eventCurrencyVal
                , efsBudgetCents = budgetCentsVal
                , efsPlannedIncomeCents = plannedIncomeCents
                , efsPlannedExpenseCents = plannedExpenseCents
                , efsActualIncomeCents = actualIncomeCents
                , efsActualExpenseCents = actualExpenseCents
                , efsNetCents = netCents
                , efsTicketPaidRevenueCents = ticketPaidRevenueCents
                , efsTicketRefundedRevenueCents = ticketRefundedRevenueCents
                , efsTicketPendingRevenueCents = ticketPendingRevenueCents
                , efsAccountsPayableCents = accountsPayableCents
                , efsAccountsReceivableCents = accountsReceivableCents
                , efsContractCommittedCents = contractCommittedCents
                , efsContractPaidCents = contractPaidCents
                , efsProcurementCommittedCents = procurementCommittedCents
                , efsProcurementPaidCents = procurementPaidCents
                , efsAssetInvestmentCents = assetInvestmentCents
                , efsLiabilityBalanceCents = liabilityBalanceCents
                , efsBudgetVarianceCents = budgetVarianceCents
                , efsBudgetUtilizationPct = budgetUtilizationPct
                , efsGeneratedAt = now
                }

    -- Event logistics
    logisticsServer :: ServerT LogisticsRoutes AppM
    logisticsServer eventIdStr =
        getLogisticsPlan eventIdStr
            :<|> updateLogisticsSettings eventIdStr
            :<|> createLogisticsMember eventIdStr
            :<|> updateLogisticsMember eventIdStr
            :<|> deleteLogisticsMember eventIdStr
            :<|> createLogisticsPlace eventIdStr
            :<|> updateLogisticsPlace eventIdStr
            :<|> deleteLogisticsPlace eventIdStr
            :<|> createLogisticsActivity eventIdStr
            :<|> updateLogisticsActivity eventIdStr
            :<|> deleteLogisticsActivity eventIdStr
            :<|> verifyLogisticsRoute eventIdStr
            :<|> verifyAllLogisticsRoutes eventIdStr

    getLogisticsPlan :: T.Text -> AppM EventLogisticsPlanDTO
    getLogisticsPlan eventIdStr = do
        Env{..} <- ask
        (eventKey, _, accessRole) <- requireLogisticsAccess eventIdStr False
        settings <- loadLogisticsSettings (defaultTimezone envConfig) envPool eventKey
        memberRows <- liftIO $ runSqlPool (selectList [EventLogisticsMemberEventId ==. eventKey] [Asc EventLogisticsMemberCreatedAt]) envPool
        members <- mapM (logisticsMemberEntityToDTO envPool) memberRows
        placeRows <- liftIO $ runSqlPool (selectList [EventLogisticsPlaceEventId ==. eventKey] [Asc EventLogisticsPlaceLabel]) envPool
        activityRows <- liftIO $ runSqlPool (selectList [EventLogisticsActivityEventId ==. eventKey] [Asc EventLogisticsActivityStartTime, Asc EventLogisticsActivityId]) envPool
        activities <- mapM (logisticsActivityEntityToDTO envPool) activityRows
        pure
            EventLogisticsPlanDTO
                { elgEventId = renderKeyText eventKey
                , elgAccessRole = accessRole
                , elgSettings = settings
                , elgMembers = members
                , elgPlaces = map logisticsPlaceEntityToDTO placeRows
                , elgActivities = activities
                , elgIssues = buildLogisticsIssues activities
                }

    updateLogisticsSettings :: T.Text -> EventLogisticsSettingsDTO -> AppM EventLogisticsSettingsDTO
    updateLogisticsSettings eventIdStr dto = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsOwner eventIdStr
        now <- liftIO getCurrentTime
        timezoneVal <- validateLogisticsTimezone (elsTimezone dto)
        modeVal <- validateLogisticsTravelMode (elsDefaultTravelMode dto)
        mExisting <- liftIO $ runSqlPool (getBy (UniqueEventLogisticsPlan eventKey)) envPool
        liftIO $ runSqlPool
            (case mExisting of
                Nothing -> insert_ EventLogisticsPlan
                    { eventLogisticsPlanEventId = eventKey
                    , eventLogisticsPlanTimezone = timezoneVal
                    , eventLogisticsPlanDefaultTravelMode = modeVal
                    , eventLogisticsPlanCreatedAt = now
                    , eventLogisticsPlanUpdatedAt = now
                    }
                Just (Entity planKey _) -> update planKey
                    [ EventLogisticsPlanTimezone =. timezoneVal
                    , EventLogisticsPlanDefaultTravelMode =. modeVal
                    , EventLogisticsPlanUpdatedAt =. now
                    ]) envPool
        pure EventLogisticsSettingsDTO{elsTimezone = timezoneVal, elsDefaultTravelMode = modeVal}

    createLogisticsMember :: T.Text -> EventLogisticsMemberDTO -> AppM EventLogisticsMemberDTO
    createLogisticsMember eventIdStr dto = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsOwner eventIdStr
        partyIdVal <- validateLogisticsParty envPool (elmPartyId dto)
        roleVal <- validateLogisticsMemberRole (elmRole dto)
        now <- liftIO getCurrentTime
        inserted <- liftIO $ runSqlPool
            (insertUnique EventLogisticsMember
                { eventLogisticsMemberEventId = eventKey
                , eventLogisticsMemberPartyId = partyIdVal
                , eventLogisticsMemberMemberRole = roleVal
                , eventLogisticsMemberCreatedAt = now
                , eventLogisticsMemberUpdatedAt = now
                }) envPool
        memberKey <- maybe (throwError err409{errBody = "This person is already on the logistics team"}) pure inserted
        mCreated <- liftIO $ runSqlPool (getEntity memberKey) envPool
        maybe (throwError err500{errBody = "Could not create logistics member"}) (logisticsMemberEntityToDTO envPool) mCreated

    updateLogisticsMember :: T.Text -> T.Text -> EventLogisticsMemberDTO -> AppM EventLogisticsMemberDTO
    updateLogisticsMember eventIdStr rawPartyId dto = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsOwner eventIdStr
        partyIdVal <- validateLogisticsParty envPool rawPartyId
        when (T.strip (elmPartyId dto) /= partyIdVal) $ throwError err400{errBody = "member party id does not match URL"}
        roleVal <- validateLogisticsMemberRole (elmRole dto)
        now <- liftIO getCurrentTime
        mMember <- liftIO $ runSqlPool (getBy (UniqueEventLogisticsMember eventKey partyIdVal)) envPool
        (memberKey, memberRow) <- maybe (throwError err404{errBody = "Logistics member not found"}) (pure . (\(Entity k v) -> (k, v))) mMember
        liftIO $ runSqlPool (update memberKey [EventLogisticsMemberMemberRole =. roleVal, EventLogisticsMemberUpdatedAt =. now]) envPool
        logisticsMemberEntityToDTO envPool (Entity memberKey memberRow{eventLogisticsMemberMemberRole = roleVal, eventLogisticsMemberUpdatedAt = now})

    deleteLogisticsMember :: T.Text -> T.Text -> AppM NoContent
    deleteLogisticsMember eventIdStr rawPartyId = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsOwner eventIdStr
        partyIdVal <- pure (T.strip rawPartyId)
        liftIO $ runSqlPool (deleteBy (UniqueEventLogisticsMember eventKey partyIdVal)) envPool
        pure NoContent

    createLogisticsPlace :: T.Text -> EventLogisticsPlaceDTO -> AppM EventLogisticsPlaceDTO
    createLogisticsPlace eventIdStr dto = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsAccess eventIdStr True
        now <- liftIO getCurrentTime
        (labelVal, typeVal, venueKey) <- validateLogisticsPlaceInput envPool eventKey dto
        key <- liftIO $ runSqlPool
            (insert EventLogisticsPlace
                { eventLogisticsPlaceEventId = eventKey
                , eventLogisticsPlaceVenueId = venueKey
                , eventLogisticsPlaceLabel = labelVal
                , eventLogisticsPlacePlaceType = typeVal
                , eventLogisticsPlaceAddress = cleanMaybeText (elpAddress dto)
                , eventLogisticsPlaceGooglePlaceId = cleanMaybeText (elpGooglePlaceId dto)
                , eventLogisticsPlaceLatitude = elpLatitude dto
                , eventLogisticsPlaceLongitude = elpLongitude dto
                , eventLogisticsPlaceInstructions = cleanMaybeText (elpInstructions dto)
                , eventLogisticsPlaceContactName = cleanMaybeText (elpContactName dto)
                , eventLogisticsPlaceContactPhone = cleanMaybeText (elpContactPhone dto)
                , eventLogisticsPlaceCreatedAt = now
                , eventLogisticsPlaceUpdatedAt = now
                }) envPool
        mCreated <- liftIO $ runSqlPool (getEntity key) envPool
        maybe (throwError err500{errBody = "Could not create logistics place"}) (pure . logisticsPlaceEntityToDTO) mCreated

    updateLogisticsPlace :: T.Text -> T.Text -> EventLogisticsPlaceDTO -> AppM EventLogisticsPlaceDTO
    updateLogisticsPlace eventIdStr placeIdStr dto = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsAccess eventIdStr True
        placeKey <- parseKeyOr400 "logistics place" placeIdStr
        existing <- requireLogisticsPlace envPool eventKey placeKey
        now <- liftIO getCurrentTime
        (labelVal, typeVal, venueKey) <- validateLogisticsPlaceInput envPool eventKey dto
        liftIO $ runSqlPool (update placeKey
            [ EventLogisticsPlaceVenueId =. venueKey
            , EventLogisticsPlaceLabel =. labelVal
            , EventLogisticsPlacePlaceType =. typeVal
            , EventLogisticsPlaceAddress =. cleanMaybeText (elpAddress dto)
            , EventLogisticsPlaceGooglePlaceId =. cleanMaybeText (elpGooglePlaceId dto)
            , EventLogisticsPlaceLatitude =. elpLatitude dto
            , EventLogisticsPlaceLongitude =. elpLongitude dto
            , EventLogisticsPlaceInstructions =. cleanMaybeText (elpInstructions dto)
            , EventLogisticsPlaceContactName =. cleanMaybeText (elpContactName dto)
            , EventLogisticsPlaceContactPhone =. cleanMaybeText (elpContactPhone dto)
            , EventLogisticsPlaceUpdatedAt =. now
            ]) envPool
        pure $ logisticsPlaceEntityToDTO (Entity placeKey existing
            { eventLogisticsPlaceVenueId = venueKey
            , eventLogisticsPlaceLabel = labelVal
            , eventLogisticsPlacePlaceType = typeVal
            , eventLogisticsPlaceAddress = cleanMaybeText (elpAddress dto)
            , eventLogisticsPlaceGooglePlaceId = cleanMaybeText (elpGooglePlaceId dto)
            , eventLogisticsPlaceLatitude = elpLatitude dto
            , eventLogisticsPlaceLongitude = elpLongitude dto
            , eventLogisticsPlaceInstructions = cleanMaybeText (elpInstructions dto)
            , eventLogisticsPlaceContactName = cleanMaybeText (elpContactName dto)
            , eventLogisticsPlaceContactPhone = cleanMaybeText (elpContactPhone dto)
            , eventLogisticsPlaceUpdatedAt = now
            })

    deleteLogisticsPlace :: T.Text -> T.Text -> AppM NoContent
    deleteLogisticsPlace eventIdStr placeIdStr = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsAccess eventIdStr True
        placeKey <- parseKeyOr400 "logistics place" placeIdStr
        _ <- requireLogisticsPlace envPool eventKey placeKey
        referenceCount <- liftIO $ runSqlPool (count
            [ FilterOr
                [ EventLogisticsActivityPlaceId ==. Just placeKey
                , EventLogisticsActivityOriginPlaceId ==. Just placeKey
                , EventLogisticsActivityDestinationPlaceId ==. Just placeKey
                ]
            ]) envPool
        when (referenceCount > 0) $ throwError err409{errBody = "Place is used by one or more logistics activities"}
        liftIO $ runSqlPool (delete placeKey) envPool
        pure NoContent

    createLogisticsActivity :: T.Text -> EventLogisticsActivityDTO -> AppM EventLogisticsActivityDTO
    createLogisticsActivity eventIdStr dto = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsAccess eventIdStr True
        now <- liftIO getCurrentTime
        validated <- validateLogisticsActivityInput envPool eventKey Nothing dto
        let (typeVal, titleVal, endVal, placeKey, originKey, destinationKey, modeVal, bufferVal, priorityVal, statusVal, dependencyKeys) = validated
        modeToStore <- if typeVal == "travel" && isNothing modeVal
            then Just . elsDefaultTravelMode <$>
                loadLogisticsSettings (defaultTimezone envConfig) envPool eventKey
            else pure modeVal
        key <- liftIO $ runSqlPool (insert EventLogisticsActivity
            { eventLogisticsActivityEventId = eventKey
            , eventLogisticsActivityActivityType = typeVal
            , eventLogisticsActivityTitle = titleVal
            , eventLogisticsActivityNotes = cleanMaybeText (eacNotes dto)
            , eventLogisticsActivityStartTime = eacStart dto
            , eventLogisticsActivityEndTime = endVal
            , eventLogisticsActivityPlaceId = placeKey
            , eventLogisticsActivityOriginPlaceId = originKey
            , eventLogisticsActivityDestinationPlaceId = destinationKey
            , eventLogisticsActivityTravelMode = modeToStore
            , eventLogisticsActivityBufferMinutes = bufferVal
            , eventLogisticsActivityPriority = priorityVal
            , eventLogisticsActivityStatus = statusVal
            , eventLogisticsActivityVersion = 1
            , eventLogisticsActivityCreatedByPartyId = currentPartyId
            , eventLogisticsActivityCreatedAt = now
            , eventLogisticsActivityUpdatedAt = now
            }) envPool
        replaceLogisticsActivityRelations envPool key (eacAssignments dto) dependencyKeys now
        when (typeVal == "travel") $ void (verifyLogisticsActivityInternal envPool envConfig key Nothing)
        mCreated <- liftIO $ runSqlPool (getEntity key) envPool
        maybe (throwError err500{errBody = "Could not create logistics activity"}) (logisticsActivityEntityToDTO envPool) mCreated

    updateLogisticsActivity :: T.Text -> T.Text -> EventLogisticsActivityDTO -> AppM EventLogisticsActivityDTO
    updateLogisticsActivity eventIdStr activityIdStr dto = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsAccess eventIdStr True
        activityKey <- parseKeyOr400 "logistics activity" activityIdStr
        existing <- requireLogisticsActivity envPool eventKey activityKey
        expectedVersion <- maybe (throwError err400{errBody = "activity version is required"}) pure (eacVersion dto)
        when (expectedVersion /= eventLogisticsActivityVersion existing) $ throwError err409{errBody = "This activity was changed by another collaborator. Reload and try again."}
        validated <- validateLogisticsActivityInput envPool eventKey (Just activityKey) dto
        let (typeVal, titleVal, endVal, placeKey, originKey, destinationKey, modeVal, bufferVal, priorityVal, statusVal, dependencyKeys) = validated
            nextVersion = expectedVersion + 1
        modeToStore <- if typeVal == "travel" && isNothing modeVal
            then Just . elsDefaultTravelMode <$>
                loadLogisticsSettings (defaultTimezone envConfig) envPool eventKey
            else pure modeVal
        now <- liftIO getCurrentTime
        updatedRows <- liftIO $ runSqlPool (updateWhereCount
            [ EventLogisticsActivityId ==. activityKey
            , EventLogisticsActivityVersion ==. expectedVersion
            ]
            [ EventLogisticsActivityActivityType =. typeVal
            , EventLogisticsActivityTitle =. titleVal
            , EventLogisticsActivityNotes =. cleanMaybeText (eacNotes dto)
            , EventLogisticsActivityStartTime =. eacStart dto
            , EventLogisticsActivityEndTime =. endVal
            , EventLogisticsActivityPlaceId =. placeKey
            , EventLogisticsActivityOriginPlaceId =. originKey
            , EventLogisticsActivityDestinationPlaceId =. destinationKey
            , EventLogisticsActivityTravelMode =. modeToStore
            , EventLogisticsActivityBufferMinutes =. bufferVal
            , EventLogisticsActivityPriority =. priorityVal
            , EventLogisticsActivityStatus =. statusVal
            , EventLogisticsActivityVersion =. nextVersion
            , EventLogisticsActivityUpdatedAt =. now
            ]) envPool
        when (updatedRows == 0) $ throwError err409{errBody = "This activity was changed by another collaborator. Reload and try again."}
        replaceLogisticsActivityRelations envPool activityKey (eacAssignments dto) dependencyKeys now
        when (typeVal == "travel") $ void (verifyLogisticsActivityInternal envPool envConfig activityKey Nothing)
        mUpdated <- liftIO $ runSqlPool (getEntity activityKey) envPool
        maybe (throwError err500{errBody = "Could not update logistics activity"}) (logisticsActivityEntityToDTO envPool) mUpdated

    deleteLogisticsActivity :: T.Text -> T.Text -> AppM NoContent
    deleteLogisticsActivity eventIdStr activityIdStr = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsAccess eventIdStr True
        activityKey <- parseKeyOr400 "logistics activity" activityIdStr
        _ <- requireLogisticsActivity envPool eventKey activityKey
        liftIO $ runSqlPool (do
            deleteWhere [EventLogisticsAlertDeliveryActivityId ==. activityKey]
            deleteWhere [EventRouteVerificationActivityId ==. activityKey]
            deleteWhere [EventLogisticsAssignmentActivityId ==. activityKey]
            deleteWhere [FilterOr [EventLogisticsDependencyActivityId ==. activityKey, EventLogisticsDependencyDependsOnActivityId ==. activityKey]]
            delete activityKey) envPool
        pure NoContent

    verifyLogisticsRoute :: T.Text -> T.Text -> AppM EventRouteVerificationDTO
    verifyLogisticsRoute eventIdStr activityIdStr = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsAccess eventIdStr True
        activityKey <- parseKeyOr400 "logistics activity" activityIdStr
        _ <- requireLogisticsActivity envPool eventKey activityKey
        verifyLogisticsActivityInternal envPool envConfig activityKey Nothing

    verifyAllLogisticsRoutes :: T.Text -> AppM [EventRouteVerificationDTO]
    verifyAllLogisticsRoutes eventIdStr = do
        Env{..} <- ask
        (eventKey, _, _) <- requireLogisticsAccess eventIdStr True
        rows <- liftIO $ runSqlPool (selectList
            [ EventLogisticsActivityEventId ==. eventKey
            , EventLogisticsActivityActivityType ==. "travel"
            , EventLogisticsActivityStatus !=. "cancelled"
            ] [Asc EventLogisticsActivityStartTime]) envPool
        mapM (\(Entity key _) -> verifyLogisticsActivityInternal envPool envConfig key Nothing) rows

    requireLogisticsOwner :: T.Text -> AppM (SocialEventId, SocialEvent, T.Text)
    requireLogisticsOwner rawEventId = do
        result@(_, _, role) <- requireLogisticsAccess rawEventId False
        when (role /= "owner") $ throwError err403{errBody = "Only the event organizer can manage the logistics team and settings"}
        pure result

    requireLogisticsAccess :: T.Text -> Bool -> AppM (SocialEventId, SocialEvent, T.Text)
    requireLogisticsAccess rawEventId requireEdit = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" rawEventId
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventRow <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        role <- case cleanMaybeText (socialEventOrganizerPartyId eventRow) of
            Nothing -> do
                managed <- claimOrRequireEventManager currentPartyId envPool eventKey eventRow
                pure (if socialEventOrganizerPartyId managed == Just currentPartyId then "owner" else "viewer")
            Just owner | owner == currentPartyId -> pure "owner"
            Just _ -> do
                mMember <- liftIO $ runSqlPool (getBy (UniqueEventLogisticsMember eventKey currentPartyId)) envPool
                maybe (throwError err403{errBody = "You are not on this event's logistics team"}) (pure . eventLogisticsMemberMemberRole . entityVal) mMember
        when (requireEdit && role == "viewer") $ throwError err403{errBody = "This logistics team role is read-only"}
        pure (eventKey, eventRow, role)

    validateLogisticsPlaceInput :: ConnectionPool -> SocialEventId -> EventLogisticsPlaceDTO -> AppM (T.Text, T.Text, Maybe VenueId)
    validateLogisticsPlaceInput pool _eventKey dto = do
        let labelVal = T.strip (elpLabel dto)
            typeVal = T.toCaseFold (T.strip (elpType dto))
        when (T.null labelVal || T.length labelVal > 160) $ throwError err400{errBody = "place label is required and must be 160 characters or fewer"}
        unless (typeVal `elem` ["venue", "hotel", "airport", "pickup", "custom"]) $ throwError err400{errBody = "invalid logistics place type"}
        validateFiniteCoordinates (elpLatitude dto) (elpLongitude dto)
        venueKey <- case elpVenueId dto >>= cleanMaybeText . Just of
            Nothing -> pure Nothing
            Just raw -> do
                key <- parseKeyOr400 "venue" raw
                mVenue <- liftIO $ runSqlPool (get key) pool
                when (isNothing mVenue) $ throwError err400{errBody = "venue does not exist"}
                pure (Just key)
        pure (labelVal, typeVal, venueKey)

    validateLogisticsActivityInput :: ConnectionPool -> SocialEventId -> Maybe EventLogisticsActivityId -> EventLogisticsActivityDTO -> AppM (T.Text, T.Text, Maybe UTCTime, Maybe EventLogisticsPlaceId, Maybe EventLogisticsPlaceId, Maybe EventLogisticsPlaceId, Maybe T.Text, Maybe Int, T.Text, T.Text, [EventLogisticsActivityId])
    validateLogisticsActivityInput pool eventKey mActivityKey dto = do
        let typeVal = T.toCaseFold (T.strip (eacType dto))
            titleVal = T.strip (eacTitle dto)
            priorityVal = T.toCaseFold (T.strip (eacPriority dto))
            statusVal = T.toCaseFold (T.strip (eacStatus dto))
        unless (typeVal `elem` ["task", "milestone", "wait", "travel"]) $ throwError err400{errBody = "invalid logistics activity type"}
        when (T.null titleVal || T.length titleVal > 200) $ throwError err400{errBody = "activity title is required and must be 200 characters or fewer"}
        unless (priorityVal `elem` ["low", "normal", "high", "critical"]) $ throwError err400{errBody = "invalid logistics priority"}
        unless (statusVal `elem` ["planned", "confirmed", "in_progress", "completed", "cancelled"]) $ throwError err400{errBody = "invalid logistics status"}
        endVal <- if typeVal == "milestone"
            then pure Nothing
            else case eacEnd dto of
                Just value | value > eacStart dto -> pure (Just value)
                _ -> throwError err400{errBody = "non-milestone activities require an end after the start"}
        placeKey <- traverse (requirePlaceReference pool eventKey) (eacPlaceId dto >>= cleanMaybeText . Just)
        originKey <- traverse (requirePlaceReference pool eventKey) (eacOriginPlaceId dto >>= cleanMaybeText . Just)
        destinationKey <- traverse (requirePlaceReference pool eventKey) (eacDestinationPlaceId dto >>= cleanMaybeText . Just)
        modeVal <- traverse validateLogisticsTravelMode (eacTravelMode dto >>= cleanMaybeText . Just)
        when (typeVal == "travel" && (isNothing originKey || isNothing destinationKey)) $ throwError err400{errBody = "travel activities require origin and destination places"}
        when (typeVal == "travel" && originKey == destinationKey) $ throwError err400{errBody = "travel origin and destination must be different"}
        when (typeVal == "travel" && isJust placeKey) $ throwError err400{errBody = "travel activities use origin and destination instead of a single place"}
        when (typeVal /= "travel" && (isJust originKey || isJust destinationKey)) $ throwError err400{errBody = "origin and destination are only valid for travel activities"}
        when (typeVal /= "travel" && isJust modeVal) $ throwError err400{errBody = "travel mode is only valid for travel activities"}
        bufferVal <- case eacBufferMinutes dto of
            Just value | value < 0 || value > 1440 -> throwError err400{errBody = "buffer minutes must be between 0 and 1440"}
            value -> pure value
        dependencyKeys <- mapM (parseKeyOr400 "logistics dependency") (eacDependencyIds dto)
        forM_ dependencyKeys $ \dependencyKey -> do
            when (Just dependencyKey == mActivityKey) $ throwError err400{errBody = "an activity cannot depend on itself"}
            _ <- requireLogisticsActivity pool eventKey dependencyKey
            case mActivityKey of
                Nothing -> pure ()
                Just activityKey -> do
                    cyclic <- liftIO (logisticsDependencyReaches pool dependencyKey activityKey Set.empty)
                    when cyclic $ throwError err400{errBody = "logistics dependencies must not contain cycles"}
        validateLogisticsAssignments pool (eacAssignments dto)
        pure (typeVal, titleVal, endVal, placeKey, originKey, destinationKey, modeVal, bufferVal, priorityVal, statusVal, dependencyKeys)

    replaceLogisticsActivityRelations :: ConnectionPool -> EventLogisticsActivityId -> [EventLogisticsAssignmentDTO] -> [EventLogisticsActivityId] -> UTCTime -> AppM ()
    replaceLogisticsActivityRelations pool activityKey assignments dependencyKeys now =
        liftIO $ runSqlPool (do
            deleteWhere [EventLogisticsAssignmentActivityId ==. activityKey]
            deleteWhere [EventLogisticsDependencyActivityId ==. activityKey]
            forM_ assignments $ \assignment -> insert_ EventLogisticsAssignment
                { eventLogisticsAssignmentActivityId = activityKey
                , eventLogisticsAssignmentPartyId = cleanMaybeText (elaPartyId assignment)
                , eventLogisticsAssignmentExternalName = cleanMaybeText (elaExternalName assignment)
                , eventLogisticsAssignmentExternalPhone = cleanMaybeText (elaExternalPhone assignment)
                , eventLogisticsAssignmentExternalEmail = cleanMaybeText (elaExternalEmail assignment)
                , eventLogisticsAssignmentCreatedAt = now
                }
            forM_ dependencyKeys $ \dependencyKey -> insert_ EventLogisticsDependency
                { eventLogisticsDependencyActivityId = activityKey
                , eventLogisticsDependencyDependsOnActivityId = dependencyKey
                , eventLogisticsDependencyCreatedAt = now
                }) pool

    verifyLogisticsActivityInternal :: ConnectionPool -> AppConfig -> EventLogisticsActivityId -> Maybe T.Text -> AppM EventRouteVerificationDTO
    verifyLogisticsActivityInternal pool config activityKey checkpoint = do
        activity <- maybe (throwError err404{errBody = "Logistics activity not found"}) pure =<< liftIO (runSqlPool (get activityKey) pool)
        when (eventLogisticsActivityActivityType activity /= "travel") $ throwError err400{errBody = "Only travel activities can be route-verified"}
        endTime <- maybe (throwError err400{errBody = "Travel activity requires an end time"}) pure (eventLogisticsActivityEndTime activity)
        originKey <- maybe (throwError err400{errBody = "Travel activity requires an origin"}) pure (eventLogisticsActivityOriginPlaceId activity)
        destinationKey <- maybe (throwError err400{errBody = "Travel activity requires a destination"}) pure (eventLogisticsActivityDestinationPlaceId activity)
        origin <- maybe (throwError err400{errBody = "Travel origin no longer exists"}) pure =<< liftIO (runSqlPool (get originKey) pool)
        destination <- maybe (throwError err400{errBody = "Travel destination no longer exists"}) pure =<< liftIO (runSqlPool (get destinationKey) pool)
        let modeVal = fromMaybe "drive" (eventLogisticsActivityTravelMode activity)
            allocatedSeconds = max 0 (floor (diffUTCTime endTime (eventLogisticsActivityStartTime activity)))
            input = RouteEstimateInput
                { reiOriginLatitude = eventLogisticsPlaceLatitude origin
                , reiOriginLongitude = eventLogisticsPlaceLongitude origin
                , reiDestinationLatitude = eventLogisticsPlaceLatitude destination
                , reiDestinationLongitude = eventLogisticsPlaceLongitude destination
                , reiTravelMode = modeVal
                , reiDepartureTime = eventLogisticsActivityStartTime activity
                }
        result <- case googleRoutesApiKey config of
            Nothing -> pure (Left "GOOGLE_ROUTES_API_KEY no está configurada.")
            Just apiKey -> liftIO (computeGoogleRoute apiKey (googleRoutesApiBase config) (defaultLocale config) input)
        now <- liftIO getCurrentTime
        let computed = routeVerificationValues activity allocatedSeconds result
            (durationVal, staticVal, distanceVal, bufferSeconds, verdictVal, polylineVal, errorVal) = computed
        key <- liftIO $ runSqlPool (insert EventRouteVerification
            { eventRouteVerificationActivityId = activityKey
            , eventRouteVerificationActivityVersion = eventLogisticsActivityVersion activity
            , eventRouteVerificationProvider = "google_routes"
            , eventRouteVerificationTravelMode = modeVal
            , eventRouteVerificationDepartureTime = eventLogisticsActivityStartTime activity
            , eventRouteVerificationDurationSeconds = durationVal
            , eventRouteVerificationStaticDurationSeconds = staticVal
            , eventRouteVerificationDistanceMeters = distanceVal
            , eventRouteVerificationBufferSeconds = bufferSeconds
            , eventRouteVerificationAllocatedSeconds = allocatedSeconds
            , eventRouteVerificationVerdict = verdictVal
            , eventRouteVerificationEncodedPolyline = polylineVal
            , eventRouteVerificationErrorMessage = errorVal
            , eventRouteVerificationCheckpoint = checkpoint
            , eventRouteVerificationVerifiedAt = now
            }) pool
        pure EventRouteVerificationDTO
            { ervId = Just (renderKeyText key)
            , ervActivityVersion = eventLogisticsActivityVersion activity
            , ervProvider = "google_routes"
            , ervTravelMode = modeVal
            , ervDepartureTime = eventLogisticsActivityStartTime activity
            , ervDurationSeconds = durationVal
            , ervStaticDurationSeconds = staticVal
            , ervDistanceMeters = distanceVal
            , ervBufferSeconds = bufferSeconds
            , ervAllocatedSeconds = allocatedSeconds
            , ervVerdict = verdictVal
            , ervEncodedPolyline = polylineVal
            , ervErrorMessage = errorVal
            , ervCheckpoint = checkpoint
            , ervVerifiedAt = now
            }

    requireManagedEvent :: T.Text -> AppM (SocialEventId, SocialEvent)
    requireManagedEvent rawEventId = do
        Env{..} <- ask
        eventKey <- parseKeyOr400 "event" rawEventId
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventVal <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        claimed <- claimOrRequireEventManager currentPartyId envPool eventKey eventVal
        pure (eventKey, claimed)

    requireExistingEvent :: ConnectionPool -> SocialEventId -> AppM SocialEvent
    requireExistingEvent pool eventKey = do
        mEvent <- liftIO $ runSqlPool (get eventKey) pool
        maybe (throwError err404{errBody = "Event not found"}) pure mEvent

    requireMomentForEvent :: ConnectionPool -> SocialEventId -> EventMomentId -> AppM EventMoment
    requireMomentForEvent pool eventKey momentKey = do
        mMoment <- liftIO $ runSqlPool (get momentKey) pool
        momentRow <- maybe (throwError err404{errBody = "Moment not found"}) pure mMoment
        when (eventMomentEventId momentRow /= eventKey) $
            throwError err400{errBody = "Moment does not belong to this event"}
        pure momentRow

    requireLiveBroadcastForEvent ::
        ConnectionPool ->
        SocialEventId ->
        EventLiveBroadcastId ->
        AppM EventLiveBroadcast
    requireLiveBroadcastForEvent pool eventKey broadcastKey = do
        mBroadcast <- liftIO $ runSqlPool (get broadcastKey) pool
        broadcastRow <- maybe (throwError err404{errBody = "Live broadcast not found"}) pure mBroadcast
        when (eventLiveBroadcastEventId broadcastRow /= eventKey) $
            throwError err400{errBody = "Live broadcast does not belong to this event"}
        pure broadcastRow

    requireEventArtistProfile ::
        ConnectionPool ->
        SocialEventId ->
        ArtistProfileId ->
        AppM ArtistProfile
    requireEventArtistProfile pool eventKey artistKey = do
        mArtist <- liftIO $ runSqlPool (get artistKey) pool
        artistRow <- maybe (throwError err404{errBody = "Artist not found"}) pure mArtist
        mLineup <- liftIO $ runSqlPool (get (EventArtistKey eventKey artistKey)) pool
        when (isNothing mLineup) $
            throwError err400{errBody = "Artist is not in this event lineup"}
        pure artistRow

    requireArtistFollower :: ConnectionPool -> ArtistProfileId -> T.Text -> AppM ()
    requireArtistFollower pool artistKey partyId = do
        mFollow <- liftIO $ runSqlPool (get (ArtistFollowKey artistKey partyId)) pool
        when (isNothing mFollow) $
            throwError err403{errBody = "Only followers of this artist can start a fanclub live broadcast"}

    parseIds :: T.Text -> T.Text -> AppM (SocialEventId, EventInvitationId)
    parseIds eventIdStr invitationIdStr =
        case parseInvitationIdsEither eventIdStr invitationIdStr of
            Right ids -> pure ids
            Left e -> throwError e

    parseArtistId :: T.Text -> AppM ArtistProfileId
    parseArtistId = parseKeyOr400 "artist"

validateLogisticsTimezone :: T.Text -> AppM T.Text
validateLogisticsTimezone raw =
    let value = T.strip raw
     in if T.null value || T.length value > 80 || T.any isUnsafeSocialEventsListFilterChar value
            then throwError err400{errBody = "timezone is required and must be a valid IANA name"}
            else
                if value /= "UTC" && not ("/" `T.isInfixOf` value)
                    then throwError err400{errBody = "timezone must be UTC or an IANA area/location name"}
                    else pure value

validateLogisticsTravelMode :: T.Text -> AppM T.Text
validateLogisticsTravelMode raw =
    let value = T.toCaseFold (T.strip raw)
     in if value `elem` ["drive", "walk", "bicycle", "two_wheeler", "transit"]
            then pure value
            else throwError err400{errBody = "invalid logistics travel mode"}

validateLogisticsMemberRole :: T.Text -> AppM T.Text
validateLogisticsMemberRole raw =
    let value = T.toCaseFold (T.strip raw)
     in if value `elem` ["viewer", "editor"]
            then pure value
            else throwError err400{errBody = "logistics member role must be viewer or editor"}

validateLogisticsParty :: ConnectionPool -> T.Text -> AppM T.Text
validateLogisticsParty pool rawPartyId = do
    let normalized = T.strip rawPartyId
    partyKey <- parseKeyOr400 "party" normalized :: AppM PartyId
    mParty <- liftIO $ runSqlPool (get partyKey) pool
    when (isNothing mParty) $ throwError err400{errBody = "party does not exist"}
    pure (renderKeyText partyKey)

validateFiniteCoordinates :: Double -> Double -> AppM ()
validateFiniteCoordinates latitude longitude
    | isNaN latitude || isInfinite latitude = throwError err400{errBody = "latitude must be finite"}
    | isNaN longitude || isInfinite longitude = throwError err400{errBody = "longitude must be finite"}
    | latitude < (-90) || latitude > 90 = throwError err400{errBody = "latitude must be between -90 and 90"}
    | longitude < (-180) || longitude > 180 = throwError err400{errBody = "longitude must be between -180 and 180"}
    | otherwise = pure ()

requirePlaceReference :: ConnectionPool -> SocialEventId -> T.Text -> AppM EventLogisticsPlaceId
requirePlaceReference pool eventKey rawPlaceId = do
    placeKey <- parseKeyOr400 "logistics place" rawPlaceId
    _ <- requireLogisticsPlace pool eventKey placeKey
    pure placeKey

requireLogisticsPlace :: ConnectionPool -> SocialEventId -> EventLogisticsPlaceId -> AppM EventLogisticsPlace
requireLogisticsPlace pool eventKey placeKey = do
    row <- maybe (throwError err404{errBody = "Logistics place not found"}) pure =<< liftIO (runSqlPool (get placeKey) pool)
    when (eventLogisticsPlaceEventId row /= eventKey) $ throwError err400{errBody = "Logistics place does not belong to this event"}
    pure row

requireLogisticsActivity :: ConnectionPool -> SocialEventId -> EventLogisticsActivityId -> AppM EventLogisticsActivity
requireLogisticsActivity pool eventKey activityKey = do
    row <- maybe (throwError err404{errBody = "Logistics activity not found"}) pure =<< liftIO (runSqlPool (get activityKey) pool)
    when (eventLogisticsActivityEventId row /= eventKey) $ throwError err400{errBody = "Logistics activity does not belong to this event"}
    pure row

validateLogisticsAssignments :: ConnectionPool -> [EventLogisticsAssignmentDTO] -> AppM ()
validateLogisticsAssignments pool assignments = forM_ assignments $ \assignment -> do
    let partyIdVal = cleanMaybeText (elaPartyId assignment)
        externalNameVal = cleanMaybeText (elaExternalName assignment)
    when (isJust partyIdVal == isJust externalNameVal) $
        throwError err400{errBody = "each assignment must identify either one TDF user or one external person"}
    forM_ partyIdVal $ \partyId -> void (validateLogisticsParty pool partyId)
    forM_ externalNameVal $ \name ->
        when (T.length name > 160) $ throwError err400{errBody = "external assignee name must be 160 characters or fewer"}
    forM_ (cleanMaybeText (elaExternalEmail assignment)) $ \email ->
        when (T.length email > 320 || not ("@" `T.isInfixOf` email)) $
            throwError err400{errBody = "external assignee email is invalid"}

loadLogisticsSettings :: T.Text -> ConnectionPool -> SocialEventId -> AppM EventLogisticsSettingsDTO
loadLogisticsSettings fallbackTimezone pool eventKey = do
    row <- liftIO $ runSqlPool (getBy (UniqueEventLogisticsPlan eventKey)) pool
    pure $ case row of
        Nothing -> EventLogisticsSettingsDTO fallbackTimezone "drive"
        Just (Entity _ value) -> EventLogisticsSettingsDTO
            { elsTimezone = eventLogisticsPlanTimezone value
            , elsDefaultTravelMode = eventLogisticsPlanDefaultTravelMode value
            }

logisticsMemberEntityToDTO :: ConnectionPool -> Entity EventLogisticsMember -> AppM EventLogisticsMemberDTO
logisticsMemberEntityToDTO pool (Entity _ member) = do
    let partyIdVal = eventLogisticsMemberPartyId member
        mPartyKey = fromPathPiece partyIdVal :: Maybe PartyId
    mParty <- case mPartyKey of
        Nothing -> pure Nothing
        Just partyKey -> liftIO $ runSqlPool (get partyKey) pool
    pure EventLogisticsMemberDTO
        { elmPartyId = partyIdVal
        , elmDisplayName = partyDisplayName <$> mParty
        , elmEmail = mParty >>= partyPrimaryEmail
        , elmRole = eventLogisticsMemberMemberRole member
        , elmCreatedAt = Just (eventLogisticsMemberCreatedAt member)
        }

logisticsPlaceEntityToDTO :: Entity EventLogisticsPlace -> EventLogisticsPlaceDTO
logisticsPlaceEntityToDTO (Entity key place) = EventLogisticsPlaceDTO
    { elpId = Just (renderKeyText key)
    , elpVenueId = renderKeyText <$> eventLogisticsPlaceVenueId place
    , elpLabel = eventLogisticsPlaceLabel place
    , elpType = eventLogisticsPlacePlaceType place
    , elpAddress = eventLogisticsPlaceAddress place
    , elpGooglePlaceId = eventLogisticsPlaceGooglePlaceId place
    , elpLatitude = eventLogisticsPlaceLatitude place
    , elpLongitude = eventLogisticsPlaceLongitude place
    , elpInstructions = eventLogisticsPlaceInstructions place
    , elpContactName = eventLogisticsPlaceContactName place
    , elpContactPhone = eventLogisticsPlaceContactPhone place
    , elpCreatedAt = Just (eventLogisticsPlaceCreatedAt place)
    , elpUpdatedAt = Just (eventLogisticsPlaceUpdatedAt place)
    }

logisticsActivityEntityToDTO :: ConnectionPool -> Entity EventLogisticsActivity -> AppM EventLogisticsActivityDTO
logisticsActivityEntityToDTO pool (Entity key activity) = do
    assignmentRows <- liftIO $ runSqlPool (selectList [EventLogisticsAssignmentActivityId ==. key] [Asc EventLogisticsAssignmentId]) pool
    dependencyRows <- liftIO $ runSqlPool (selectList [EventLogisticsDependencyActivityId ==. key] [Asc EventLogisticsDependencyDependsOnActivityId]) pool
    latestRoute <- liftIO $ runSqlPool (selectFirst [EventRouteVerificationActivityId ==. key] [Desc EventRouteVerificationVerifiedAt, Desc EventRouteVerificationId]) pool
    assignments <- mapM assignmentToDTO assignmentRows
    pure EventLogisticsActivityDTO
        { eacId = Just (renderKeyText key)
        , eacType = eventLogisticsActivityActivityType activity
        , eacTitle = eventLogisticsActivityTitle activity
        , eacNotes = eventLogisticsActivityNotes activity
        , eacStart = eventLogisticsActivityStartTime activity
        , eacEnd = eventLogisticsActivityEndTime activity
        , eacPlaceId = renderKeyText <$> eventLogisticsActivityPlaceId activity
        , eacOriginPlaceId = renderKeyText <$> eventLogisticsActivityOriginPlaceId activity
        , eacDestinationPlaceId = renderKeyText <$> eventLogisticsActivityDestinationPlaceId activity
        , eacTravelMode = eventLogisticsActivityTravelMode activity
        , eacBufferMinutes = eventLogisticsActivityBufferMinutes activity
        , eacPriority = eventLogisticsActivityPriority activity
        , eacStatus = eventLogisticsActivityStatus activity
        , eacVersion = Just (eventLogisticsActivityVersion activity)
        , eacAssignments = assignments
        , eacDependencyIds = map (renderKeyText . eventLogisticsDependencyDependsOnActivityId . entityVal) dependencyRows
        , eacLatestVerification = routeVerificationEntityToDTO <$> latestRoute
        , eacCreatedAt = Just (eventLogisticsActivityCreatedAt activity)
        , eacUpdatedAt = Just (eventLogisticsActivityUpdatedAt activity)
        }
  where
    assignmentToDTO (Entity _ assignment) = do
        let partyIdVal = eventLogisticsAssignmentPartyId assignment
        displayName <- case partyIdVal >>= (fromPathPiece :: T.Text -> Maybe PartyId) of
            Nothing -> pure Nothing
            Just partyKey -> fmap partyDisplayName <$> liftIO (runSqlPool (get partyKey) pool)
        pure EventLogisticsAssignmentDTO
            { elaPartyId = partyIdVal
            , elaDisplayName = displayName
            , elaExternalName = eventLogisticsAssignmentExternalName assignment
            , elaExternalPhone = eventLogisticsAssignmentExternalPhone assignment
            , elaExternalEmail = eventLogisticsAssignmentExternalEmail assignment
            }

routeVerificationEntityToDTO :: Entity EventRouteVerification -> EventRouteVerificationDTO
routeVerificationEntityToDTO (Entity key verification) = EventRouteVerificationDTO
    { ervId = Just (renderKeyText key)
    , ervActivityVersion = eventRouteVerificationActivityVersion verification
    , ervProvider = eventRouteVerificationProvider verification
    , ervTravelMode = eventRouteVerificationTravelMode verification
    , ervDepartureTime = eventRouteVerificationDepartureTime verification
    , ervDurationSeconds = eventRouteVerificationDurationSeconds verification
    , ervStaticDurationSeconds = eventRouteVerificationStaticDurationSeconds verification
    , ervDistanceMeters = eventRouteVerificationDistanceMeters verification
    , ervBufferSeconds = eventRouteVerificationBufferSeconds verification
    , ervAllocatedSeconds = eventRouteVerificationAllocatedSeconds verification
    , ervVerdict = eventRouteVerificationVerdict verification
    , ervEncodedPolyline = eventRouteVerificationEncodedPolyline verification
    , ervErrorMessage = eventRouteVerificationErrorMessage verification
    , ervCheckpoint = eventRouteVerificationCheckpoint verification
    , ervVerifiedAt = eventRouteVerificationVerifiedAt verification
    }

routeVerificationValues :: EventLogisticsActivity -> Int -> Either T.Text RouteEstimateResult -> (Maybe Int, Maybe Int, Maybe Int, Int, T.Text, Maybe T.Text, Maybe T.Text)
routeVerificationValues activity allocatedSeconds result = case result of
    Left message ->
        let bufferSeconds = maybe 900 (* 60) (eventLogisticsActivityBufferMinutes activity)
         in (Nothing, Nothing, Nothing, bufferSeconds, "unavailable", Nothing, Just message)
    Right RouteEstimateResult{..} ->
        let automaticBuffer = max 900 (ceiling (fromIntegral rerDurationSeconds * (0.2 :: Double)))
            bufferSeconds = maybe automaticBuffer (* 60) (eventLogisticsActivityBufferMinutes activity)
            verdict
                | allocatedSeconds < rerDurationSeconds = "infeasible"
                | allocatedSeconds < rerDurationSeconds + bufferSeconds = "tight"
                | otherwise = "feasible"
         in (Just rerDurationSeconds, rerStaticDurationSeconds, Just rerDistanceMeters, bufferSeconds, verdict, rerEncodedPolyline, Nothing)

logisticsDependencyReaches :: ConnectionPool -> EventLogisticsActivityId -> EventLogisticsActivityId -> Set.Set EventLogisticsActivityId -> IO Bool
logisticsDependencyReaches pool current target visited
    | current == target = pure True
    | Set.member current visited = pure False
    | otherwise = do
        rows <- runSqlPool (selectList [EventLogisticsDependencyActivityId ==. current] []) pool
        or <$> mapM (\row -> logisticsDependencyReaches pool (eventLogisticsDependencyDependsOnActivityId (entityVal row)) target (Set.insert current visited)) rows

buildLogisticsIssues :: [EventLogisticsActivityDTO] -> [EventScheduleIssueDTO]
buildLogisticsIssues activities =
    concatMap routeIssue activities
        <> concatMap dependencyIssues activities
        <> overlappingAssignmentIssues
        <> missingTransferIssues
  where
    routeIssue activity = case eacLatestVerification activity of
        Just verification | ervVerdict verification `elem` ["tight", "infeasible", "unavailable"] ->
            [ EventScheduleIssueDTO
                { esiCode = "route_" <> ervVerdict verification
                , esiSeverity = if ervVerdict verification == "tight" then "warning" else "error"
                , esiActivityId = eacId activity
                , esiMessage = fromMaybe (routeMessage (ervVerdict verification)) (ervErrorMessage verification)
                }
            ]
        _ -> []
    dependencyIssues activity =
        [ EventScheduleIssueDTO
            { esiCode = "dependency_timing"
            , esiSeverity = "error"
            , esiActivityId = eacId activity
            , esiMessage = "La actividad comienza antes de que termine una dependencia."
            }
        | dependencyId <- eacDependencyIds activity
        , Just dependency <- [findActivity dependencyId]
        , fromMaybe (eacStart dependency) (eacEnd dependency) > eacStart activity
        ]
    findActivity activityId = listToMaybe [activity | activity <- activities, eacId activity == Just activityId]
    overlappingAssignmentIssues =
        [ EventScheduleIssueDTO
            { esiCode = "assignment_overlap"
            , esiSeverity = "warning"
            , esiActivityId = eacId leftActivity
            , esiMessage =
                "El mismo responsable está asignado a actividades simultáneas: “"
                    <> eacTitle leftActivity
                    <> "” y “"
                    <> eacTitle rightActivity
                    <> "”."
            }
        | (index, leftActivity) <- zip [0 :: Int ..] activities
        , rightActivity <- drop (index + 1) activities
        , eacStatus leftActivity /= "cancelled"
        , eacStatus rightActivity /= "cancelled"
        , activityRangesOverlap leftActivity rightActivity
        , not (Set.null (assignmentIdentities leftActivity `Set.intersection` assignmentIdentities rightActivity))
        ]
    activityRangesOverlap leftActivity rightActivity =
        let leftEnd = fromMaybe (eacStart leftActivity) (eacEnd leftActivity)
            rightEnd = fromMaybe (eacStart rightActivity) (eacEnd rightActivity)
         in eacStart leftActivity < rightEnd && eacStart rightActivity < leftEnd
    assignmentIdentities activity = Set.fromList (map assignmentIdentity (eacAssignments activity))
    assignmentIdentity assignment = case elaPartyId assignment of
        Just partyId -> "party:" <> T.strip partyId
        Nothing ->
            "external:"
                <> T.toCaseFold (T.strip (fromMaybe "" (elaExternalName assignment)))
                <> ":"
                <> T.toCaseFold (T.strip (fromMaybe "" (elaExternalEmail assignment)))
                <> ":"
                <> T.strip (fromMaybe "" (elaExternalPhone assignment))
    missingTransferIssues =
        [ EventScheduleIssueDTO
            { esiCode = "missing_transfer"
            , esiSeverity = "warning"
            , esiActivityId = eacId rightActivity
            , esiMessage =
                "Falta un traslado para "
                    <> assignmentDisplayFor identity leftActivity
                    <> " entre “"
                    <> eacTitle leftActivity
                    <> "” y “"
                    <> eacTitle rightActivity
                    <> "”."
            }
        | identity <- Set.toList allAssignmentIdentities
        , let assignedActivities = sortOn eacStart
                [ activity
                | activity <- activities
                , eacStatus activity /= "cancelled"
                , Set.member identity (assignmentIdentities activity)
                ]
        , (leftActivity, rightActivity) <- zip assignedActivities (drop 1 assignedActivities)
        , fromMaybe (eacStart leftActivity) (eacEnd leftActivity) <= eacStart rightActivity
        , Just leftLocation <- [activityEndLocation leftActivity]
        , Just rightLocation <- [activityStartLocation rightActivity]
        , leftLocation /= rightLocation
        ]
    allAssignmentIdentities = Set.unions (map assignmentIdentities activities)
    activityStartLocation activity
        | eacType activity == "travel" = eacOriginPlaceId activity
        | otherwise = eacPlaceId activity
    activityEndLocation activity
        | eacType activity == "travel" = eacDestinationPlaceId activity
        | otherwise = eacPlaceId activity
    assignmentDisplayFor identity activity =
        fromMaybe "el responsable" $ listToMaybe
            [ fromMaybe (fromMaybe "el responsable" (elaPartyId assignment))
                (elaDisplayName assignment <|> elaExternalName assignment)
            | assignment <- eacAssignments activity
            , assignmentIdentity assignment == identity
            ]
    routeMessage "tight" = "El traslado cabe, pero no respeta la holgura recomendada."
    routeMessage "infeasible" = "El tiempo reservado es menor que la duración estimada del traslado."
    routeMessage _ = "No se pudo verificar la ruta."

-- | Stable, human-friendly identifier for a follow (artistId + follower id).
renderFollowId :: ArtistProfileId -> T.Text -> T.Text
renderFollowId artistId followerPartyId =
    T.intercalate ":" [renderKeyText artistId, followerPartyId]

-- | Insert or fetch an artist follow while keeping the created timestamp stable.
followArtistDb :: ConnectionPool -> ArtistProfileId -> T.Text -> IO ArtistFollowerDTO
followArtistDb pool artistId followerPartyIdRaw = do
    now <- getCurrentTime
    let followerPartyId = fromMaybe (T.strip followerPartyIdRaw) (normalizePositivePartyIdText followerPartyIdRaw)
    let followKey = ArtistFollowKey artistId followerPartyId
    existing <- runSqlPool (get followKey) pool
    _ <- case existing of
        Just _ -> pure followKey
        Nothing -> do
            mInserted <- runSqlPool (insertUnique (ArtistFollow artistId followerPartyId now)) pool
            pure (fromMaybe followKey mInserted)
    let createdAtVal = maybe now artistFollowCreatedAt existing
    pure
        ArtistFollowerDTO
            { afFollowId = Just (renderFollowId artistId followerPartyId)
            , afArtistId = Just (renderKeyText artistId)
            , afFollowerPartyId = followerPartyId
            , afCreatedAt = Just createdAtVal
            }

normalizePositivePartyIdText :: T.Text -> Maybe T.Text
normalizePositivePartyIdText rawPartyId =
    normalizePositiveIdentifierText rawPartyId

resolveExistingPartyIdText :: ConnectionPool -> T.Text -> T.Text -> IO (Either ServerError T.Text)
resolveExistingPartyIdText pool fieldName rawPartyId =
    case normalizePositivePartyIdText rawPartyId of
        Nothing ->
            pure (Left err400{errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be a positive integer"))})
        Just normalized ->
            case readMaybe (T.unpack normalized) :: Maybe Int64 of
                Nothing ->
                    pure (Left err400{errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be a positive integer"))})
                Just partyIdValue -> do
                    mParty <- runSqlPool (get (toSqlKey partyIdValue :: Key Party)) pool
                    pure $
                        case mParty of
                            Just _ -> Right normalized
                            Nothing ->
                                Left
                                    err422
                                        { errBody =
                                            BL.fromStrict
                                                (TE.encodeUtf8 (fieldName <> " references an unknown party"))
                                        }

resolveUniqueRsvpRow :: [Entity EventRsvp] -> Either ServerError (Maybe (Entity EventRsvp))
resolveUniqueRsvpRow [] = Right Nothing
resolveUniqueRsvpRow [row] = Right (Just row)
resolveUniqueRsvpRow _ =
    Left
        err409
            { errBody =
                "Multiple RSVP rows exist for this event and party; resolve duplicate rows before updating RSVP"
            }

normalizePositiveIdentifierText :: T.Text -> Maybe T.Text
normalizePositiveIdentifierText rawIdentifier =
    T.pack . show <$> normalizePositiveIdentifier rawIdentifier

normalizePositiveIdentifier :: T.Text -> Maybe Int64
normalizePositiveIdentifier rawIdentifier =
    let trimmed = T.strip rawIdentifier
     in if T.null trimmed || not (T.all isAsciiDecimalDigit trimmed)
            then Nothing
            else case readMaybe (T.unpack trimmed) of
                Just identifier | identifier > 0 -> Just identifier
                _ -> Nothing

isAsciiDecimalDigit :: Char -> Bool
isAsciiDecimalDigit ch =
    ch >= '0' && ch <= '9'

parseFollowerQueryParamEither :: Maybe T.Text -> Either ServerError T.Text
parseFollowerQueryParamEither mFollower =
    case cleanMaybeText mFollower of
        Nothing -> Left err400{errBody = "follower query param is required"}
        Just rawFollower ->
            case normalizePositivePartyIdText rawFollower of
                Nothing -> Left err400{errBody = "follower query param must be a positive integer"}
                Just normalized -> Right normalized

validateInvitationToPartyId :: T.Text -> Either ServerError T.Text
validateInvitationToPartyId rawInvitationPartyId =
    case cleanMaybeText (Just rawInvitationPartyId) of
        Nothing -> Left err400{errBody = "invitationToPartyId is required"}
        Just trimmed ->
            case normalizePositivePartyIdText trimmed of
                Nothing -> Left err400{errBody = "invitationToPartyId must be a positive integer"}
                Just normalized -> Right normalized

validateInvitationFromPartyId :: T.Text -> Maybe T.Text -> Either ServerError T.Text
validateInvitationFromPartyId currentPartyId rawInvitationPartyId =
    case cleanMaybeText rawInvitationPartyId of
        Nothing -> Right currentPartyId
        Just trimmed ->
            case normalizePositivePartyIdText trimmed of
                Nothing -> Left err400{errBody = "invitationFromPartyId must be a positive integer"}
                Just normalized
                    | normalized == currentPartyId -> Right currentPartyId
                    | otherwise ->
                        Left err403{errBody = "invitationFromPartyId must match the authenticated party"}

validateRsvpStatus :: T.Text -> Either ServerError T.Text
validateRsvpStatus raw =
    case T.toLower (T.strip raw) of
        "accepted" -> Right "accepted"
        "declined" -> Right "declined"
        "maybe" -> Right "maybe"
        _ -> Left err400{errBody = "rsvpStatus must be one of: accepted, declined, maybe"}

parseInvitationStatus :: T.Text -> Maybe T.Text
parseInvitationStatus raw =
    case T.toLower (T.strip raw) of
        "pending" -> Just "pending"
        "accepted" -> Just "accepted"
        "declined" -> Just "declined"
        _ -> Nothing

validateInvitationStatusInput :: Maybe T.Text -> Either ServerError T.Text
validateInvitationStatusInput Nothing = Right "pending"
validateInvitationStatusInput (Just rawStatus) =
    case T.strip rawStatus of
        "" -> Right "pending"
        _ ->
            case parseInvitationStatus rawStatus of
                Just statusVal -> Right statusVal
                Nothing ->
                    Left
                        err400
                            { errBody = "invitationStatus must be one of: pending, accepted, declined"
                            }

validateInvitationStatusUpdateInput :: Maybe T.Text -> Either ServerError (Maybe T.Text)
validateInvitationStatusUpdateInput Nothing = Right Nothing
validateInvitationStatusUpdateInput (Just rawStatus) =
    case cleanMaybeText (Just rawStatus) of
        Nothing ->
            Left
                err400
                    { errBody = "invitationStatus must be one of: pending, accepted, declined"
                    }
        Just _ ->
            Just <$> validateInvitationStatusInput (Just rawStatus)

validateEventArtistIds :: [ArtistDTO] -> Either ServerError [ArtistProfileId]
validateEventArtistIds artists
    | length artists > maxEventArtistsPerEvent =
        Left err400{errBody = "eventArtists supports at most 50 artists"}
    | otherwise = do
        artistKeys <- traverse validateArtistId artists
        if Set.size (Set.fromList artistKeys) == length artistKeys
            then Right artistKeys
            else Left err400{errBody = "eventArtists[].artistId must be unique"}
  where
    validateArtistId artist =
        case artistId artist of
            Nothing -> Left err400{errBody = "eventArtists[].artistId is required"}
            Just rawArtistId ->
                case normalizePositiveIdentifierText rawArtistId of
                    Nothing -> Left err400{errBody = "eventArtists[].artistId must be a positive integer"}
                    Just normalizedArtistId ->
                        case readMaybe (T.unpack normalizedArtistId) :: Maybe Int64 of
                            Just artistIdValue -> Right (toSqlKey artistIdValue)
                            Nothing -> Left err400{errBody = "eventArtists[].artistId must be a positive integer"}

maxEventArtistsPerEvent :: Int
maxEventArtistsPerEvent = 50

validateArtistName :: T.Text -> Either ServerError T.Text
validateArtistName rawName
    | T.null normalized =
        Left err400{errBody = "artist name is required"}
    | T.any isControl rawName =
        Left err400{errBody = "artist name must not contain control characters"}
    | otherwise =
        Right normalized
  where
    normalized = T.strip rawName

-- | Normalize invitation status to a lowercase, non-empty value.
normalizeInvitationStatus :: Maybe T.Text -> T.Text
normalizeInvitationStatus mStatus =
    case mStatus >>= parseInvitationStatus of
        Nothing -> "pending"
        Just s -> s

normalizeTicketOrderStatus :: Maybe T.Text -> T.Text
normalizeTicketOrderStatus mStatus =
    case cleanMaybeText mStatus of
        Nothing -> "pending"
        Just rawStatus ->
            fromMaybe (T.toLower rawStatus) (parseTicketOrderStatus rawStatus)

{- | Both pending Stripe orders and paid orders reserve inventory. Closed
orders consume none, so status retries naturally produce a zero delta.
-}
ticketOrderInventoryAdjustment :: Int -> T.Text -> T.Text -> Int
ticketOrderInventoryAdjustment quantity oldStatus newStatus =
    reservedQuantity newStatus - reservedQuantity oldStatus
  where
    reservedQuantity rawStatus =
        case parseTicketOrderStatus rawStatus of
            Just "pending" -> quantity
            Just "paid" -> quantity
            _ -> 0

{- | Direct issuance is limited to tiers whose authoritative price is exactly
zero. A manager is not payment evidence: priced tickets must use a verified
provider checkout or a separately implemented dual-controlled manual-payment
workflow.
-}
validateDirectTicketOrderPricing :: Bool -> Int -> Either ServerError ()
validateDirectTicketOrderPricing _ authoritativeTierPriceCents
    | authoritativeTierPriceCents < 0 =
        Left err500{errBody = "Stored ticket tier price is invalid"}
    | authoritativeTierPriceCents > 0 =
        Left
            err409
                { errBody =
                    "Priced ticket orders require a server-verified checkout"
                }
    | otherwise = Right ()

validateTicketPurchaseEventEligibility :: Maybe T.Text -> Bool -> Either ServerError ()
validateTicketPurchaseEventEligibility rawMetadata purchaseEnabled = do
    metadata <-
        either (Left . storedEventMetadataServerError) Right $
            decodeStoredEventMetadata rawMetadata
    when (emIsPublic metadata == Just False) $
        Left err403{errBody = "Tickets are not available for private events"}
    unless purchaseEnabled $
        Left err409{errBody = "Tickets are not on sale for this event"}

eventTicketPurchaseEnabledFor :: SocialEvent -> SqlPersistT IO Bool
eventTicketPurchaseEnabledFor eventRow =
    case socialEventWorkflowStateId eventRow of
        Nothing -> pure False
        Just stateId -> EventLifecycle.socialEventStateHasCapability stateId "ticket-purchase"

normalizeTicketStatus :: Maybe T.Text -> T.Text
normalizeTicketStatus mStatus =
    case mStatus >>= parseTicketStatus of
        Nothing -> "issued"
        Just s -> s

validateStoredTicketOrderStatus :: Maybe T.Text -> Either ServerError T.Text
validateStoredTicketOrderStatus Nothing =
    Left err500{errBody = "Ticket order could not be loaded"}
validateStoredTicketOrderStatus (Just rawStatus) =
    case parseTicketOrderStatus rawStatus of
        Just statusVal -> Right statusVal
        Nothing -> Left err500{errBody = "Stored ticket order status is invalid"}

validateTicketCheckInOrderStatus :: Maybe T.Text -> Either ServerError T.Text
validateTicketCheckInOrderStatus = validateStoredTicketOrderStatus

validateTicketCheckInTicketStatus :: T.Text -> Either ServerError T.Text
validateTicketCheckInTicketStatus rawStatus =
    case parseTicketStatus rawStatus of
        Just statusVal -> Right statusVal
        Nothing -> Left err500{errBody = "Stored ticket status is invalid"}

findTicketForCheckIn :: SocialEventId -> TicketCheckInLookup -> SqlPersistT IO (Maybe (Entity EventTicket))
findTicketForCheckIn eventKey ticketLookup =
    case ticketLookup of
        TicketCheckInLookupById ticketId ->
            selectFirst [EventTicketId ==. toSqlKey ticketId, EventTicketEventId ==. eventKey] []
        TicketCheckInLookupByCode codeVal ->
            selectFirst [EventTicketEventId ==. eventKey, EventTicketCode ==. codeVal] []

validateTicketCheckInLookup :: TicketCheckInRequestDTO -> Either ServerError TicketCheckInLookup
validateTicketCheckInLookup TicketCheckInRequestDTO{..} =
    case (ticketCheckInTicketId, ticketCheckInTicketCode) of
        (Just _, Just _) ->
            Left err400{errBody = "Provide exactly one of ticketCheckInTicketId or ticketCheckInTicketCode"}
        (Just rawTicketId, Nothing) ->
            case normalizePositiveIdentifier rawTicketId of
                Just ticketId -> Right (TicketCheckInLookupById ticketId)
                Nothing ->
                    Left err400{errBody = "ticketCheckInTicketId must be a positive integer"}
        (Nothing, Just rawCode) ->
            case normalizeTicketCheckInCode rawCode of
                Just codeVal -> Right (TicketCheckInLookupByCode codeVal)
                Nothing ->
                    Left err400{errBody = "ticketCheckInTicketCode must be a generated ticket code"}
        (Nothing, Nothing) ->
            Left err400{errBody = "Provide ticketCheckInTicketId or ticketCheckInTicketCode"}

validateOptionalTicketBuyerPartyId :: T.Text -> Maybe T.Text -> Either ServerError (Maybe T.Text)
validateOptionalTicketBuyerPartyId fieldName mPartyId =
    case cleanMaybeText mPartyId of
        Nothing -> Right Nothing
        Just rawPartyId ->
            case normalizePositivePartyIdText rawPartyId of
                Nothing ->
                    Left
                        err400
                            { errBody =
                                BL.fromStrict
                                    (TE.encodeUtf8 (fieldName <> " must be a positive integer"))
                            }
                Just normalized -> Right (Just normalized)

validateTicketPurchaseBuyerName :: Maybe T.Text -> Either ServerError (Maybe T.Text)
validateTicketPurchaseBuyerName rawName =
    case cleanMaybeText rawName of
        Nothing -> Right Nothing
        Just buyerName
            | T.length buyerName > 160 ->
                Left
                    err400
                        { errBody = "ticketPurchaseBuyerName must be 160 characters or fewer"
                        }
            | T.any isUnsafeTicketPurchaseBuyerNameChar buyerName ->
                Left
                    err400
                        { errBody =
                            "ticketPurchaseBuyerName must not contain control characters "
                                <> "or hidden formatting characters"
                        }
            | not (T.any isAlphaNum buyerName) ->
                Left
                    err400
                        { errBody = "ticketPurchaseBuyerName must include letters or numbers"
                        }
            | otherwise ->
                Right (Just buyerName)

isUnsafeTicketPurchaseBuyerNameChar :: Char -> Bool
isUnsafeTicketPurchaseBuyerNameChar ch =
    isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateTicketPurchaseBuyerEmail :: Maybe T.Text -> Either ServerError (Maybe T.Text)
validateTicketPurchaseBuyerEmail rawEmail =
    case cleanMaybeText rawEmail of
        Nothing -> Right Nothing
        Just email
            | T.length normalized > 254 ->
                Left
                    err400
                        { errBody = "ticketPurchaseBuyerEmail must be 254 characters or fewer"
                        }
            | isValidSocialEventEmail normalized ->
                Right (Just normalized)
            | otherwise ->
                Left
                    err400
                        { errBody = "ticketPurchaseBuyerEmail must be a valid email address"
                        }
          where
            normalized = T.toLower email

isValidSocialEventEmail :: T.Text -> Bool
isValidSocialEventEmail candidate =
    case T.splitOn "@" candidate of
        [localPart, domain] ->
            T.length candidate <= 254
                && isValidSocialEventEmailLocalPart localPart
                && not (T.null domain)
                && not (T.any (`elem` (" \t\n\r" :: String)) candidate)
                && T.isInfixOf "." domain
                && hasValidSocialEventEmailFinalDomainLabel domain
                && all isValidSocialEventEmailDomainLabel (T.splitOn "." domain)
        _ -> False

hasValidSocialEventEmailFinalDomainLabel :: T.Text -> Bool
hasValidSocialEventEmailFinalDomainLabel domain =
    case reverse (T.splitOn "." domain) of
        finalLabel : _ ->
            T.length finalLabel >= 2 && T.any isAsciiLower finalLabel
        [] -> False

isValidSocialEventEmailLocalPart :: T.Text -> Bool
isValidSocialEventEmailLocalPart localPart =
    not (T.null localPart)
        && T.length localPart <= 64
        && not (T.isPrefixOf "." localPart)
        && not (T.isSuffixOf "." localPart)
        && not (".." `T.isInfixOf` localPart)
        && T.all isValidSocialEventEmailLocalChar localPart

isValidSocialEventEmailLocalChar :: Char -> Bool
isValidSocialEventEmailLocalChar c =
    isAscii c
        && (isAlphaNum c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String))

isValidSocialEventEmailDomainLabel :: T.Text -> Bool
isValidSocialEventEmailDomainLabel label =
    not (T.null label)
        && T.length label <= 63
        && not (T.isPrefixOf "-" label)
        && not (T.isSuffixOf "-" label)
        && T.all isValidSocialEventEmailDomainChar label

isValidSocialEventEmailDomainChar :: Char -> Bool
isValidSocialEventEmailDomainChar c =
    isAscii c && (isAlphaNum c || c == '-')

normalizeTicketCheckInCode :: T.Text -> Maybe T.Text
normalizeTicketCheckInCode rawCode = do
    suffix <- T.stripPrefix "TDF-" normalized
    if T.length suffix == 12 && T.all isAsciiHexDigit suffix
        then Just normalized
        else Nothing
  where
    normalized = T.toUpper (T.strip rawCode)
    isAsciiHexDigit ch = isAscii ch && isHexDigit ch

normalizeMomentMediaType :: T.Text -> Maybe T.Text
normalizeMomentMediaType raw =
    case T.toLower (T.strip raw) of
        "image" -> Just "image"
        "photo" -> Just "image"
        "picture" -> Just "image"
        "video" -> Just "video"
        "clip" -> Just "video"
        _ -> Nothing

loadSelectableMomentReactionTypeId :: T.Text -> SqlPersistT IO (Either ServerError UUID.UUID)
loadSelectableMomentReactionTypeId rawId =
    case UUID.fromText (T.strip rawId) of
        Nothing -> pure (Left invalidReactionTypeReference)
        Just reactionTypeUuid -> do
            let reactionTypeKey = Catalog.ReactionTypeKey reactionTypeUuid
            mReactionType <- get reactionTypeKey
            case mReactionType of
                Nothing -> pure (Left invalidReactionTypeReference)
                Just reactionType -> do
                    catalog <- getJust (Catalog.reactionTypeCatalogId reactionType)
                    workflowState <- getJust (Catalog.reactionTypeWorkflowStateId reactionType)
                    let selectable =
                            Catalog.reactionTypeActive reactionType
                                && Catalog.catalogDefinitionActive catalog
                                && Catalog.catalogDefinitionCode catalog == "reaction-types"
                                && Catalog.workflowStateActive workflowState
                                && Catalog.workflowStateCode workflowState == "published"
                                && Catalog.workflowStateWorkflowId workflowState == Catalog.catalogDefinitionWorkflowId catalog
                    pure (if selectable then Right reactionTypeUuid else Left invalidReactionTypeReference)
  where
    invalidReactionTypeReference =
        err422{errBody = "reactionTypeId must identify an active published reaction type"}

normalizeMomentCaption :: Maybe T.Text -> Either ServerError (Maybe T.Text)
normalizeMomentCaption mCaption =
    case cleanMaybeText mCaption of
        Nothing -> Right Nothing
        Just captionVal
            | T.length captionVal > 280 ->
                Left err400{errBody = "Moment caption must be 280 characters or less"}
            | maybe False (T.any isUnsafeMomentTextChar) mCaption ->
                Left
                    err400
                        { errBody =
                            "Moment caption must not contain control characters or hidden formatting characters"
                        }
            | otherwise -> Right (Just captionVal)

normalizeMomentCommentBody :: T.Text -> Either ServerError T.Text
normalizeMomentCommentBody rawBody =
    case nonEmptyText rawBody of
        Nothing ->
            Left err400{errBody = "Moment comment body is required"}
        Just bodyVal
            | T.length bodyVal > 500 ->
                Left err400{errBody = "Moment comment body must be 500 characters or less"}
            | T.any isUnsafeMomentTextChar rawBody ->
                Left
                    err400
                        { errBody =
                            "Moment comment body must not contain control characters or hidden formatting characters"
                        }
            | otherwise -> Right bodyVal

isUnsafeMomentTextChar :: Char -> Bool
isUnsafeMomentTextChar ch =
    isControl ch
        || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateMomentMediaDimension :: T.Text -> Maybe Int -> Either ServerError (Maybe Int)
validateMomentMediaDimension _ Nothing = Right Nothing
validateMomentMediaDimension fieldName (Just value)
    | value > 0 = Right (Just value)
    | otherwise =
        Left
            err400
                { errBody =
                    BL.fromStrict
                        (TE.encodeUtf8 (fieldName <> " must be greater than 0"))
                }

validateMomentMediaDuration :: Maybe Int -> Either ServerError (Maybe Int)
validateMomentMediaDuration Nothing = Right Nothing
validateMomentMediaDuration (Just value)
    | value >= 0 = Right (Just value)
    | otherwise =
        Left err400{errBody = "Moment media duration must be 0 or greater"}

resolveMomentAuthorName :: T.Text -> Maybe T.Text -> T.Text
resolveMomentAuthorName currentParty mAuthorName =
    fromMaybe ("Party " <> currentParty) (cleanMaybeText mAuthorName)

loadAuthenticatedPartyDisplayName :: ConnectionPool -> T.Text -> IO T.Text
loadAuthenticatedPartyDisplayName pool currentParty =
    case readMaybe (T.unpack currentParty) :: Maybe Int64 of
        Nothing -> pure ("Party " <> currentParty)
        Just partyId -> do
            mParty <- runSqlPool (get (toSqlKey partyId)) pool
            pure $ maybe ("Party " <> currentParty) partyDisplayName mParty

validateLiveBroadcastBroadcaster :: T.Text -> Maybe T.Text -> AppM ()
validateLiveBroadcastBroadcaster currentPartyId mRawBroadcaster =
    maybe (pure ()) validateBroadcaster (cleanMaybeText mRawBroadcaster)
  where
    validateBroadcaster rawBroadcaster =
        maybe
            (throwError err400{errBody = "broadcasterPartyId must be a positive integer"})
            requireMatchingParty
            (normalizePositivePartyIdText rawBroadcaster)

    requireMatchingParty normalized
        | normalized == currentPartyId = pure ()
        | otherwise =
            throwError err403{errBody = "broadcasterPartyId must match authenticated party"}

normalizeLiveBroadcastTitle :: Maybe T.Text -> Either ServerError T.Text
normalizeLiveBroadcastTitle mTitle =
    maybe
        (Left err400{errBody = "Live broadcast title is required"})
        validateTitle
        (cleanMaybeText mTitle)
  where
    validateTitle titleVal
        | T.length titleVal > 120 =
            Left err400{errBody = "Live broadcast title must be 120 characters or less"}
        | maybe False (T.any isUnsafeMomentTextChar) mTitle =
            Left
                err400
                    { errBody =
                        "Live broadcast title must not contain control characters or hidden formatting characters"
                    }
        | otherwise = Right titleVal

normalizeLiveBroadcastDescription :: Maybe T.Text -> Either ServerError (Maybe T.Text)
normalizeLiveBroadcastDescription mDescription =
    maybe (Right Nothing) validateDescription (cleanMaybeText mDescription)
  where
    validateDescription descriptionVal
        | T.length descriptionVal > 280 =
            Left err400{errBody = "Live broadcast description must be 280 characters or less"}
        | maybe False (T.any isUnsafeMomentTextChar) mDescription =
            Left
                err400
                    { errBody =
                        "Live broadcast description must not contain control characters or hidden formatting characters"
                    }
        | otherwise = Right (Just descriptionVal)

normalizeLiveBroadcastQuality :: Maybe T.Text -> Either ServerError T.Text
normalizeLiveBroadcastQuality mQuality =
    maybe (Right "auto") validateQuality (fmap (T.toLower . T.strip) mQuality)
  where
    validateQuality quality
        | T.null quality = Right "auto"
        | quality `elem` ["auto", "720p", "480p"] = Right quality
        | otherwise =
            Left err400{errBody = "Live broadcast quality must be one of: auto, 720p, 480p"}

resolveLiveBroadcastStreamEndpoints :: T.Text -> AppM (T.Text, T.Text, T.Text)
resolveLiveBroadcastStreamEndpoints streamKey = do
    mListenBaseRaw <- liftIO (lookupEnv "RADIO_PUBLIC_BASE")
    listenBaseRaw <-
        either throwError pure $
            resolveRadioTransmissionEnvBase
                "RADIO_PUBLIC_BASE"
                "https://tdf-hq.fly.dev/live"
                mListenBaseRaw
    listenBase <- either throwError pure (validateRadioTransmissionPublicBase listenBaseRaw)
    let fallbackIngest = deriveLiveBroadcastBase listenBase "rtmp" "/live"
        fallbackWhip = deriveLiveBroadcastBase listenBase "https" "/whip"
    mIngestBaseRaw <- liftIO (lookupEnv "RADIO_INGEST_BASE")
    mWhipBaseRaw <- liftIO (lookupEnv "RADIO_WHIP_BASE")
    ingestBaseRaw <-
        either throwError pure $
            resolveRadioTransmissionEnvBase
                "RADIO_INGEST_BASE"
                fallbackIngest
                mIngestBaseRaw
    whipBaseRaw <-
        either throwError pure $
            resolveRadioTransmissionEnvBase
                "RADIO_WHIP_BASE"
                fallbackWhip
                mWhipBaseRaw
    ingestBase <- either throwError pure (validateRadioTransmissionIngestBase ingestBaseRaw)
    whipBase <- either throwError pure (validateRadioTransmissionWhipBase whipBaseRaw)
    pure
        ( appendLiveBroadcastPath listenBase streamKey
        , appendLiveBroadcastPath ingestBase streamKey
        , appendLiveBroadcastPath whipBase streamKey
        )

appendLiveBroadcastPath :: T.Text -> T.Text -> T.Text
appendLiveBroadcastPath base path =
    T.dropWhileEnd (== '/') base <> "/" <> path

deriveLiveBroadcastBase :: T.Text -> T.Text -> T.Text -> T.Text
deriveLiveBroadcastBase baseUrl newScheme newPath =
    let noScheme =
            fromMaybe
                baseUrl
                (T.stripPrefix "https://" baseUrl <|> T.stripPrefix "http://" baseUrl)
        host = T.takeWhile (/= '/') noScheme
        cleanHost = if T.null host then "localhost" else host
        normalizedPath = if T.isPrefixOf "/" newPath then newPath else "/" <> newPath
     in newScheme <> "://" <> cleanHost <> normalizedPath

normalizeBudgetCentsMaybe :: Maybe Int -> Maybe Int
normalizeBudgetCentsMaybe mBudget =
    case mBudget of
        Just n | n >= 0 -> Just n
        _ -> Nothing

validateEventTitleInput :: T.Text -> Either ServerError T.Text
validateEventTitleInput rawTitle
    | T.null titleVal =
        Left err400{errBody = "title is required"}
    | T.length titleVal > maxEventTitleChars =
        Left err400{errBody = "title must be 160 characters or fewer"}
    | T.any isUnsafeEventTitleChar titleVal =
        Left
            err400
                { errBody =
                    "title must not contain control characters or hidden formatting characters"
                }
    | otherwise =
        Right titleVal
  where
    titleVal = T.strip rawTitle

maxEventTitleChars :: Int
maxEventTitleChars = 160

isUnsafeEventTitleChar :: Char -> Bool
isUnsafeEventTitleChar ch =
    isControl ch
        || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateEventCreateUpdateDimensions :: Maybe Int -> Maybe Int -> Maybe Int -> Either ServerError ()
validateEventCreateUpdateDimensions mPriceCents mCapacity mBudgetCents
    | maybe False (< 0) mPriceCents =
        Left err400{errBody = "event price must be >= 0"}
    | maybe False (< 0) mCapacity =
        Left err400{errBody = "event capacity must be >= 0"}
    | maybe False (< 0) mBudgetCents =
        Left err400{errBody = "event budget must be >= 0"}
    | otherwise =
        Right ()

validateVenueCreateUpdateFields ::
    T.Text ->
    Maybe Double ->
    Maybe Double ->
    Maybe Int ->
    Either ServerError ()
validateVenueCreateUpdateFields rawName mLat mLng mCapacity
    | T.null (T.strip rawName) =
        Left err400{errBody = "venue name is required"}
    | T.any isUnsafeVenueNameChar rawName =
        Left
            err400
                { errBody =
                    "venue name must not contain control characters or hidden formatting characters"
                }
    | maybe False (< 0) mCapacity =
        Left err400{errBody = "venue capacity must be >= 0"}
    | otherwise =
        validateVenueCoordinatePair mLat mLng

isUnsafeVenueNameChar :: Char -> Bool
isUnsafeVenueNameChar ch =
    isControl ch
        || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateVenueCoordinatePair :: Maybe Double -> Maybe Double -> Either ServerError ()
validateVenueCoordinatePair Nothing Nothing = Right ()
validateVenueCoordinatePair (Just lat) (Just lng)
    | isNaN lat || isInfinite lat =
        Left err400{errBody = "Invalid venue latitude"}
    | isNaN lng || isInfinite lng =
        Left err400{errBody = "Invalid venue longitude"}
    | lat < (-90) || lat > 90 =
        Left err400{errBody = "venue latitude must be between -90 and 90"}
    | lng < (-180) || lng > 180 =
        Left err400{errBody = "venue longitude must be between -180 and 180"}
    | otherwise =
        Right ()
validateVenueCoordinatePair _ _ =
    Left err400{errBody = "venue latitude and longitude must be provided together"}

validateEventCurrencyInput :: T.Text -> Maybe T.Text -> Either ServerError T.Text
validateEventCurrencyInput configuredDefault mCurrency =
    case cleanMaybeText mCurrency of
        Nothing ->
            maybe
                (Left err500{errBody = "Configured default currency is invalid"})
                Right
                (normalizeCurrencyCode configuredDefault)
        Just rawCurrency ->
            case normalizeCurrencyCode rawCurrency of
                Just currency -> Right currency
                Nothing -> Left err400{errBody = "eventCurrency must be a valid ISO 4217 code"}

normalizeEventCurrencyMaybe :: Maybe T.Text -> Maybe T.Text
normalizeEventCurrencyMaybe = (>>= normalizeEventCurrencyCode) . cleanMaybeText

normalizeEventCurrencyCode :: T.Text -> Maybe T.Text
normalizeEventCurrencyCode = normalizeCurrencyCode

normalizeCurrencyMaybe :: Maybe T.Text -> Maybe T.Text
normalizeCurrencyMaybe mCurrency = normalizeCurrency <$> cleanMaybeText mCurrency

validateFinanceEntryCurrencyInput :: T.Text -> T.Text -> Either ServerError T.Text
validateFinanceEntryCurrencyInput defaultCurrency rawCurrency =
    case cleanMaybeText (Just rawCurrency) of
        Nothing ->
            case normalizeEventCurrencyCode defaultCurrency of
                Just fallbackCurrency -> Right fallbackCurrency
                Nothing ->
                    Left
                        err409
                            { errBody =
                                "event default currency must be a 3-letter ISO code before finance entries can inherit it"
                            }
        Just providedCurrency ->
            case normalizeEventCurrencyCode providedCurrency of
                Just currency -> Right currency
                Nothing ->
                    Left err400{errBody = "finance entry currency must be a 3-letter ISO code"}

validateTicketTierCodeInput :: T.Text -> T.Text -> Either ServerError T.Text
validateTicketTierCodeInput rawTierName rawCode =
    let source = fromMaybe (T.strip rawTierName) (cleanMaybeText (Just rawCode))
        normalized = normalizeTicketTierCode source
     in if not (T.any isAlphaNum source)
            then Left err400{errBody = "ticket tier code must include at least one letter or digit"}
            else
                if T.length normalized > 64
                    then Left err400{errBody = "ticket tier code must be 64 characters or fewer"}
                    else Right normalized

validateTicketTierCurrencyInput :: T.Text -> T.Text -> Either ServerError T.Text
validateTicketTierCurrencyInput defaultCurrency rawCurrency =
    case cleanMaybeText (Just rawCurrency) of
        Nothing ->
            case normalizeEventCurrencyCode defaultCurrency of
                Just fallbackCurrency -> Right fallbackCurrency
                Nothing ->
                    Left
                        err409
                            { errBody =
                                "event default currency must be a 3-letter ISO code before ticket tiers can inherit it"
                            }
        Just providedCurrency ->
            case normalizeEventCurrencyCode providedCurrency of
                Just currency -> Right currency
                Nothing ->
                    Left err400{errBody = "ticket tier currency must be a 3-letter ISO code"}

normalizeBudgetLineType :: Maybe T.Text -> T.Text
normalizeBudgetLineType mType =
    case mType >>= parseBudgetLineType of
        Just lineTypeVal -> lineTypeVal
        Nothing -> "expense"

parseBudgetLineType :: T.Text -> Maybe T.Text
parseBudgetLineType raw =
    case T.toLower (T.strip raw) of
        "income" -> Just "income"
        "expense" -> Just "expense"
        _ -> Nothing

validateBudgetLineTypeInput :: T.Text -> Either ServerError T.Text
validateBudgetLineTypeInput raw =
    case parseBudgetLineType raw of
        Just lineTypeVal -> Right lineTypeVal
        Nothing -> Left err400{errBody = "budget line type must be income or expense"}

validateStoredBudgetLineDimensions :: EventBudgetLine -> Either T.Text (Int, T.Text)
validateStoredBudgetLineDimensions line = do
    lineTypeVal <-
        maybe
            (Left "Stored budget line type is invalid")
            Right
            (parseBudgetLineType (eventBudgetLineLineType line))
    if eventBudgetLinePlannedCents line < 0
        then Left "Stored budget line planned cents is invalid"
        else Right (eventBudgetLinePlannedCents line, lineTypeVal)

normalizeBudgetLineCode :: T.Text -> T.Text
normalizeBudgetLineCode raw =
    let upper = T.toUpper (T.strip raw)
        withDash = T.map (\c -> if c == ' ' then '-' else c) upper
        cleaned = T.filter (\c -> isAlphaNum c || c == '-' || c == '_') withDash
        chunks = filter (not . T.null) (T.splitOn "-" cleaned)
        normalized = T.intercalate "-" chunks
     in if T.null normalized then "LINE" else normalized

normalizeCategory :: Maybe T.Text -> T.Text
normalizeCategory mCategory =
    case fmap (T.toLower . T.strip) mCategory of
        Nothing -> "general"
        Just "" -> "general"
        Just v -> v

parseFinanceDirection :: T.Text -> Maybe T.Text
parseFinanceDirection raw =
    case T.toLower (T.strip raw) of
        "income" -> Just "income"
        "expense" -> Just "expense"
        _ -> Nothing

normalizeFinanceDirection :: Maybe T.Text -> T.Text
normalizeFinanceDirection mDirection =
    case mDirection >>= parseFinanceDirection of
        Just directionVal -> directionVal
        Nothing -> "expense"

normalizeFinanceSource :: Maybe T.Text -> T.Text
normalizeFinanceSource mSource =
    case mSource >>= parseFinanceSource of
        Just src -> src
        Nothing -> "manual"

parseFinanceSource :: T.Text -> Maybe T.Text
parseFinanceSource raw =
    case T.toLower (T.strip raw) of
        "ticket_sale" -> Just "ticket_sale"
        "ticket_refund" -> Just "ticket_refund"
        "sponsorship" -> Just "sponsorship"
        "vendor_payment" -> Just "vendor_payment"
        "merchandise" -> Just "merchandise"
        "operations" -> Just "operations"
        "manual" -> Just "manual"
        "other" -> Just "other"
        "contract_commitment" -> Just "contract_commitment"
        "contract_payment" -> Just "contract_payment"
        "purchase_order" -> Just "purchase_order"
        "purchase_payment" -> Just "purchase_payment"
        "asset_purchase" -> Just "asset_purchase"
        "liability_loan" -> Just "liability_loan"
        "liability_payment" -> Just "liability_payment"
        "accounts_receivable" -> Just "accounts_receivable"
        "accounts_receivable_collection" -> Just "accounts_receivable_collection"
        "accounts_receivable_settlement" -> Just "accounts_receivable_collection"
        _ -> Nothing

parseFinanceEntryStatus :: T.Text -> Maybe T.Text
parseFinanceEntryStatus raw =
    case T.toLower (T.strip raw) of
        "draft" -> Just "draft"
        "posted" -> Just "posted"
        "void" -> Just "void"
        "pending" -> Just "pending"
        _ -> Nothing

normalizeFinanceEntryStatus :: Maybe T.Text -> T.Text
normalizeFinanceEntryStatus mStatus =
    case mStatus >>= parseFinanceEntryStatus of
        Just statusVal -> statusVal
        Nothing -> "posted"

validateStoredFinanceEntryDimensions :: EventFinanceEntry -> Either T.Text (T.Text, T.Text, T.Text)
validateStoredFinanceEntryDimensions entry = do
    directionVal <-
        maybe
            (Left "Stored finance entry direction is invalid")
            Right
            (parseFinanceDirection (eventFinanceEntryDirection entry))
    sourceVal <-
        maybe
            (Left "Stored finance entry source is invalid")
            Right
            (parseFinanceSource (eventFinanceEntrySource entry))
    statusVal <-
        maybe
            (Left "Stored finance entry status is invalid")
            Right
            (parseFinanceEntryStatus (eventFinanceEntryStatus entry))
    _currencyVal <-
        maybe
            (Left "Stored finance entry currency is invalid")
            Right
            (normalizeEventCurrencyCode (eventFinanceEntryCurrency entry))
    if eventFinanceEntryAmountCents entry <= 0
        then Left "Stored finance entry amount is invalid"
        else pure ()
    pure (directionVal, sourceVal, statusVal)

normalizeFinanceDirectionInput :: T.Text -> AppM T.Text
normalizeFinanceDirectionInput raw =
    case parseFinanceDirection raw of
        Just directionVal -> pure directionVal
        Nothing -> throwError err400{errBody = "direction must be income or expense"}

normalizeFinanceSourceInput :: T.Text -> AppM T.Text
normalizeFinanceSourceInput raw =
    case parseFinanceSource raw of
        Just sourceVal -> pure sourceVal
        Nothing -> throwError err400{errBody = "Invalid finance source"}

normalizeFinanceEntryStatusInput :: T.Text -> AppM T.Text
normalizeFinanceEntryStatusInput raw =
    case parseFinanceEntryStatus raw of
        Just statusVal -> pure statusVal
        Nothing -> throwError err400{errBody = "Invalid finance status"}

normalizeFinanceDirectionFilter :: Maybe T.Text -> AppM (Maybe T.Text)
normalizeFinanceDirectionFilter Nothing = pure Nothing
normalizeFinanceDirectionFilter (Just raw) =
    case T.toLower (T.strip raw) of
        "" -> pure Nothing
        _ ->
            case parseFinanceDirection raw of
                Just directionVal -> pure (Just directionVal)
                Nothing -> throwError err400{errBody = "Invalid direction filter"}

normalizeFinanceSourceFilter :: Maybe T.Text -> AppM (Maybe T.Text)
normalizeFinanceSourceFilter Nothing = pure Nothing
normalizeFinanceSourceFilter (Just raw) =
    case T.strip raw of
        "" -> pure Nothing
        nonEmpty ->
            case parseFinanceSource nonEmpty of
                Just sourceVal -> pure (Just sourceVal)
                Nothing -> throwError err400{errBody = "Invalid source filter"}

normalizeFinanceEntryStatusFilter :: Maybe T.Text -> AppM (Maybe T.Text)
normalizeFinanceEntryStatusFilter Nothing = pure Nothing
normalizeFinanceEntryStatusFilter (Just raw) =
    case T.toLower (T.strip raw) of
        "" -> pure Nothing
        _ ->
            case parseFinanceEntryStatus raw of
                Just statusVal -> pure (Just statusVal)
                Nothing -> throwError err400{errBody = "Invalid status filter"}

parseTicketOrderStatus :: T.Text -> Maybe T.Text
parseTicketOrderStatus raw =
    case T.toLower (T.strip raw) of
        "pending" -> Just "pending"
        "paid" -> Just "paid"
        "cancelled" -> Just "cancelled"
        "canceled" -> Just "cancelled"
        "refunded" -> Just "refunded"
        _ -> Nothing

parseTicketStatus :: T.Text -> Maybe T.Text
parseTicketStatus raw =
    case T.toLower (T.strip raw) of
        "issued" -> Just "issued"
        "checked_in" -> Just "checked_in"
        "checkedin" -> Just "checked_in"
        "cancelled" -> Just "cancelled"
        "canceled" -> Just "cancelled"
        "refunded" -> Just "refunded"
        _ -> Nothing

-- | Parse event and invitation ids, returning a typed pair or an HTTP 400 error.
parseInvitationIdsEither :: T.Text -> T.Text -> Either ServerError (SocialEventId, EventInvitationId)
parseInvitationIdsEither eventIdStr invitationIdStr =
    case (parseInt64Either "event" eventIdStr, parseInt64Either "invitation" invitationIdStr) of
        (Right eventNum, Right invitationNum) -> Right (toSqlKey eventNum, toSqlKey invitationNum)
        _ -> Left err400{errBody = "Invalid event or invitation id"}

parseVenueIdEither :: T.Text -> Either ServerError VenueId
parseVenueIdEither =
    fmap toSqlKey . parseInt64Either "venue"

parseInt64Either :: T.Text -> T.Text -> Either ServerError Int64
parseInt64Either label raw =
    case normalizePositiveIdentifier raw of
        Just n -> Right n
        Nothing ->
            Left
                err400
                    { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid " <> label <> " id"))
                    }

parseDoubleEither :: T.Text -> T.Text -> Either ServerError Double
parseDoubleEither label raw =
    case readMaybe (T.unpack (T.strip raw)) :: Maybe Double of
        Just n
            | isNaN n || isInfinite n ->
                Left
                    err400
                        { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid " <> label))
                        }
            | otherwise -> Right n
        Nothing ->
            Left
                err400
                    { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid " <> label))
                    }

parseNearQueryEither :: T.Text -> Either ServerError (Double, Double, Double)
parseNearQueryEither raw =
    case map T.strip (T.splitOn "," raw) of
        [latRaw, lngRaw] -> do
            lat <- parseDoubleEither "near latitude" latRaw
            lng <- parseDoubleEither "near longitude" lngRaw
            validateCoordinates lat lng
            pure (lat, lng, 25)
        [latRaw, lngRaw, radiusRaw] -> do
            lat <- parseDoubleEither "near latitude" latRaw
            lng <- parseDoubleEither "near longitude" lngRaw
            radiusKm <- parseDoubleEither "near radiusKm" radiusRaw
            validateCoordinates lat lng
            validateRadius radiusKm
            pure (lat, lng, radiusKm)
        _ ->
            Left
                err400
                    { errBody = "near must use format lat,lng or lat,lng,radiusKm"
                    }

validateCoordinates :: Double -> Double -> Either ServerError ()
validateCoordinates lat lng
    | lat < (-90) || lat > 90 = Left err400{errBody = "near latitude must be between -90 and 90"}
    | lng < (-180) || lng > 180 = Left err400{errBody = "near longitude must be between -180 and 180"}
    | otherwise = Right ()

validateRadius :: Double -> Either ServerError ()
validateRadius radiusKm
    | radiusKm <= 0 = Left err400{errBody = "near radiusKm must be greater than 0"}
    | radiusKm > 1000 = Left err400{errBody = "near radiusKm exceeds allowed maximum"}
    | otherwise = Right ()

haversineDistanceKm :: Double -> Double -> Double -> Double -> Double
haversineDistanceKm lat1 lng1 lat2 lng2 =
    let earthRadiusKm = 6371
        dLat = degToRad (lat2 - lat1)
        dLng = degToRad (lng2 - lng1)
        lat1Rad = degToRad lat1
        lat2Rad = degToRad lat2
        a =
            (sin (dLat / 2) * sin (dLat / 2))
                + (cos lat1Rad * cos lat2Rad * sin (dLng / 2) * sin (dLng / 2))
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
     in earthRadiusKm * c

degToRad :: Double -> Double
degToRad deg = deg * pi / 180

parseKeyOr400 :: (ToBackendKey SqlBackend record) => T.Text -> T.Text -> AppM (Key record)
parseKeyOr400 label raw =
    case parseInt64Either label raw of
        Left e -> throwError e
        Right n -> pure (toSqlKey n)

renderPartyId :: AuthedUser -> T.Text
renderPartyId = renderKeyText . auPartyId

renderKeyText :: (ToBackendKey SqlBackend record) => Key record -> T.Text
renderKeyText = T.pack . show . fromSqlKey

cleanMaybeText :: Maybe T.Text -> Maybe T.Text
cleanMaybeText mVal =
    case fmap T.strip mVal of
        Just txt | not (T.null txt) -> Just txt
        _ -> Nothing

normalizeArtistGenres :: [T.Text] -> [T.Text]
normalizeArtistGenres rawGenres =
    reverse normalizedRev
  where
    (_, normalizedRev) = foldl' step (Set.empty, []) rawGenres

    step :: (Set.Set T.Text, [T.Text]) -> T.Text -> (Set.Set T.Text, [T.Text])
    step (seen, acc) rawGenre =
        case nonEmptyText rawGenre of
            Nothing -> (seen, acc)
            Just genreVal ->
                let dedupeKey = T.toCaseFold genreVal
                 in if Set.member dedupeKey seen
                        then (seen, acc)
                        else (Set.insert dedupeKey seen, genreVal : acc)

resolvePublishedArtistGenres
    :: [UUID.UUID]
    -> SqlPersistT IO (Either T.Text [(UUID.UUID, T.Text)])
resolvePublishedArtistGenres rawGenreIds
    | Set.size uniqueGenreIds /= length rawGenreIds =
        pure (Left "artistGenreIds must not contain duplicates")
    | otherwise = do
        resolved <- forM rawGenreIds $ \genreId -> do
            mGenre <- get (Catalog.GenreKey genreId)
            case mGenre of
                Nothing -> pure (Left ("Unknown genre id: " <> UUID.toText genreId))
                Just genre -> do
                    mCatalog <- get (Catalog.genreCatalogId genre)
                    mState <- get (Catalog.genreWorkflowStateId genre)
                    pure $
                        if Catalog.genreActive genre
                            && maybe False ((== "genres") . Catalog.catalogDefinitionCode) mCatalog
                            && maybe False
                                (\state -> Catalog.workflowStateActive state && Catalog.workflowStateCode state == "published")
                                mState
                            then Right (genreId, Catalog.genreNameEs genre)
                            else Left ("Genre is not active and published: " <> UUID.toText genreId)
        pure (sequence resolved)
  where
    uniqueGenreIds = Set.fromList rawGenreIds

loadArtistGenreSelections
    :: ArtistProfileId
    -> ArtistProfile
    -> SqlPersistT IO ([T.Text], [UUID.UUID])
loadArtistGenreSelections artistKey artist = do
    memberships <-
        selectList
            [ArtistGenreMembershipArtistId ==. artistKey]
            [Asc ArtistGenreMembershipSortOrder]
    if null memberships
        then do
            legacyRows <- selectList [ArtistGenreArtistId ==. artistKey] []
            pure (artistGenresFromRowsAndFallback legacyRows (artistProfileGenres artist), [])
        else do
            resolved <- forM memberships $ \(Entity _ membership) -> do
                let genreId = artistGenreMembershipGenreId membership
                mGenre <- get (Catalog.GenreKey genreId)
                pure (fmap (\genre -> (genreId, Catalog.genreNameEs genre)) mGenre)
            let available = catMaybes resolved
            pure (map snd available, map fst available)

invalidArtistGenreIdsError :: T.Text -> ServerError
invalidArtistGenreIdsError message =
    err400{errBody = BL.fromStrict (TE.encodeUtf8 message)}

artistGenresFromRowsAndFallback :: [Entity ArtistGenre] -> Maybe [T.Text] -> [T.Text]
artistGenresFromRowsAndFallback genreRows fallbackGenres =
    let normalizedFromRows = normalizeArtistGenres (map (artistGenreGenre . entityVal) genreRows)
        normalizedFallback = normalizeArtistGenres (fromMaybe [] fallbackGenres)
     in if null normalizedFromRows then normalizedFallback else normalizedFromRows

artistProfileToDTO ::
    ArtistProfileId ->
    ArtistProfile ->
    [T.Text] ->
    [UUID.UUID] ->
    Either ServerError ArtistDTO
artistProfileToDTO artistKey artist genreList genreIds = do
    socialLinks <-
        either (Left . storedArtistSocialLinksServerError) Right $
            decodeStoredArtistSocialLinks (artistProfileSocialLinks artist)
    Right
        ArtistDTO
            { artistId = Just (renderKeyText artistKey)
            , artistPartyId = artistProfilePartyId artist
            , artistName = artistProfileName artist
            , artistGenres = genreList
            , artistGenreIds = genreIds
            , artistBio = artistProfileBio artist
            , artistAvatarUrl = artistProfileAvatarUrl artist
            , artistSocialLinks = socialLinks
            , artistCreatedAt = Just (artistProfileCreatedAt artist)
            , artistUpdatedAt = Just (artistProfileUpdatedAt artist)
            }

storedArtistSocialLinksServerError :: T.Text -> ServerError
storedArtistSocialLinksServerError message =
    err500{errBody = BL.fromStrict (TE.encodeUtf8 message)}

resolveBudgetLineKey :: ConnectionPool -> SocialEventId -> Maybe T.Text -> AppM (Maybe EventBudgetLineId)
resolveBudgetLineKey _ _ Nothing = pure Nothing
resolveBudgetLineKey pool eventKey rawInput = do
    normalizedBudgetLineId <- either throwError pure (validateOptionalBudgetLineIdInput rawInput)
    case normalizedBudgetLineId of
        Nothing -> pure Nothing
        Just raw -> do
            lineKey <- parseKeyOr400 "budget line" raw
            mLine <- liftIO $ runSqlPool (get lineKey) pool
            lineRec <- maybe (throwError err404{errBody = "Budget line not found"}) pure mLine
            when (eventBudgetLineEventId lineRec /= eventKey) $
                throwError err400{errBody = "Budget line does not belong to this event"}
            pure (Just lineKey)

validateOptionalBudgetLineIdInput :: Maybe T.Text -> Either ServerError (Maybe T.Text)
validateOptionalBudgetLineIdInput Nothing = Right Nothing
validateOptionalBudgetLineIdInput (Just raw)
    | T.null stripped =
        Left
            err400
                { errBody = "budgetLineId must be omitted or null when no budget line should be linked"
                }
    | otherwise = Right (Just stripped)
  where
    stripped = T.strip raw

decodeStoredEventMetadata :: Maybe T.Text -> Either T.Text EventMetadataDTO
decodeStoredEventMetadata Nothing = Right emptyEventMetadata
decodeStoredEventMetadata (Just raw)
    | T.null (T.strip raw) = Right emptyEventMetadata
    | otherwise =
        case Aeson.eitherDecodeStrict' (TE.encodeUtf8 raw) of
            Right metadata -> Right metadata
            Left err -> Left (storedEventMetadataDecodeError err)

storedEventMetadataDecodeError :: String -> T.Text
storedEventMetadataDecodeError rawError =
    case T.breakOn unknownFieldsPrefix (T.pack rawError) of
        (_, message) | not (T.null message) -> message
        _ -> "Stored event metadata is invalid JSON"
  where
    unknownFieldsPrefix = "Stored event metadata contains unknown fields:"

parseEventWorkflowStateId :: T.Text -> Either ServerError UUID.UUID
parseEventWorkflowStateId rawId =
    maybe
        (Left err400{errBody = "workflow_state_id must be a UUID"})
        Right
        (UUID.fromText (T.strip rawId))

validateEventReadOnlyProjectionOmitted :: EventDTO -> AppM ()
validateEventReadOnlyProjectionOmitted dto =
    when
        ( any
            isJust
            [ eventWorkflowStateCode dto
            , eventWorkflowStateNameEs dto
            , eventWorkflowStateNameEn dto
            ]
            || isJust (eventPublicListable dto)
            || isJust (eventTicketPurchaseEnabled dto)
        ) $
        throwError err400{errBody = "Workflow labels and capabilities are read-only"}

validateStoredEventFinanceMetadata :: T.Text -> SocialEvent -> Either T.Text (T.Text, Maybe Int)
validateStoredEventFinanceMetadata configuredDefault eventRec = do
    metadata <- decodeStoredEventMetadata (socialEventMetadata eventRec)
    currencyVal <-
        case emCurrency metadata of
            Nothing -> maybe (Left "Configured default currency is invalid") Right (normalizeCurrencyCode configuredDefault)
            Just rawCurrency ->
                maybe
                    (Left "Stored event currency is invalid")
                    Right
                    (normalizeEventCurrencyCode rawCurrency)
    budgetVal <-
        case emBudgetCents metadata of
            Just budgetCents
                | budgetCents < 0 ->
                    Left "Stored event budget is invalid"
            value -> Right value
    Right (currencyVal, budgetVal)

decodeStoredPromoCodeTierIds :: Maybe T.Text -> Maybe [T.Text]
decodeStoredPromoCodeTierIds rawTierIds = do
    tierIdsText <- rawTierIds
    Aeson.decodeStrict' (TE.encodeUtf8 tierIdsText)

encodePromoCodeTierIds :: Maybe [T.Text] -> Maybe T.Text
encodePromoCodeTierIds =
    fmap (TE.decodeUtf8 . BL.toStrict . Aeson.encode)

validatePromoCodeDateWindow :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> Either T.Text ()
validatePromoCodeDateWindow now mValidFrom mValidUntil =
    validateNotBefore *> validateNotExpired
  where
    validateNotBefore =
        maybe
            (Right ())
            ( \validFrom ->
                if now < validFrom
                    then Left "Promo code is not yet valid"
                    else Right ()
            )
            mValidFrom
    validateNotExpired =
        maybe
            (Right ())
            ( \validUntil ->
                if now > validUntil
                    then Left "Promo code has expired"
                    else Right ()
            )
            mValidUntil

validatePromoCodeRedemptionLimit :: Int -> Maybe Int -> Either T.Text ()
validatePromoCodeRedemptionLimit currentRedemptions =
    maybe
        (Right ())
        ( \maxRedemptions ->
            if currentRedemptions >= maxRedemptions
                then Left "Promo code redemption limit reached"
                else Right ()
        )

validatePromoCodeTierEligibility :: Maybe T.Text -> Maybe T.Text -> Either T.Text ()
validatePromoCodeTierEligibility mStoredTierIds mRequestedTierId =
    maybe (Right ()) validateStoredTierIds mStoredTierIds
  where
    validateStoredTierIds storedTierIds =
        decodePromoCodeTierIdsText storedTierIds >>= validateRequestedTier
    validateRequestedTier tierIds =
        maybe
            (Right ())
            ( \requestedTierId ->
                if requestedTierId `elem` tierIds
                    then Right ()
                    else Left "Promo code is not valid for this ticket tier"
            )
            mRequestedTierId

validatePromoCodeMinimumPurchaseParam :: Maybe Int -> Maybe T.Text -> Either T.Text ()
validatePromoCodeMinimumPurchaseParam mMinimumAmount mAmountText =
    maybe (Right ()) validateMinimum mMinimumAmount
  where
    validateMinimum minimumAmount =
        maybe
            (Right ())
            ( \amountText ->
                parsePromoCodePurchaseAmount amountText >>= validatePromoCodePurchaseAmountAtLeast minimumAmount
            )
            mAmountText

validatePromoCodeMinimumPurchaseCents :: Maybe Int -> Int -> Either T.Text ()
validatePromoCodeMinimumPurchaseCents mMinimumAmount amountCents =
    maybe
        (Right ())
        ( \minimumAmount ->
            if amountCents < minimumAmount
                then Left "Purchase amount does not meet minimum requirement"
                else Right ()
        )
        mMinimumAmount

validateTicketCheckoutAmount :: Int -> Int -> Either ServerError Int
validateTicketCheckoutAmount quantity unitPriceCents
    | quantity <= 0 = Left err400{errBody = "Quantity must be > 0"}
    | unitPriceCents < 0 =
        Left err500{errBody = "Stored ticket tier price is invalid"}
    | total > 99999999 =
        Left err400{errBody = "Ticket checkout amount exceeds Stripe's supported limit"}
    | otherwise = Right (fromInteger total)
  where
    total = toInteger quantity * toInteger unitPriceCents

promoCodeDiscountAmountEither :: Int -> T.Text -> Int -> Either T.Text Int
promoCodeDiscountAmountEither baseAmountCents discountType discountValue
    | discountValue < 0 =
        Left "Promo code discount value is invalid"
    | discountType == "percentage" =
        if discountValue > 10000
            then Left "Promo code percentage discount is invalid"
            else
                Right . fromInteger $
                    (toInteger baseAmountCents * toInteger discountValue) `div` 10000
    | discountType `elem` ["fixed_amount", "fixed"] =
        Right (min discountValue baseAmountCents)
    | otherwise =
        Left "Promo code discount type is invalid"

decodePromoCodeTierIdsText :: T.Text -> Either T.Text [T.Text]
decodePromoCodeTierIdsText storedTierIds =
    maybe
        (Left "Promo code tier restrictions are invalid")
        Right
        (Aeson.decodeStrict' (TE.encodeUtf8 storedTierIds))

parsePromoCodePurchaseAmount :: T.Text -> Either T.Text Int
parsePromoCodePurchaseAmount amountText =
    maybe
        (Left "Invalid amount")
        Right
        (readMaybe (T.unpack amountText))

validatePromoCodePurchaseAmountAtLeast :: Int -> Int -> Either T.Text ()
validatePromoCodePurchaseAmountAtLeast minimumAmount amount =
    if amount < minimumAmount
        then Left "Purchase amount does not meet minimum requirement"
        else Right ()

storedEventMetadataServerError :: T.Text -> ServerError
storedEventMetadataServerError message =
    err500{errBody = BL.fromStrict (TE.encodeUtf8 message)}

eventImageUploadFormServerError :: T.Text -> ServerError
eventImageUploadFormServerError message =
    err400{errBody = BL.fromStrict (TE.encodeUtf8 message)}

fallbackBudget :: Int -> Maybe Int
fallbackBudget plannedExpenseCents
    | plannedExpenseCents > 0 = Just plannedExpenseCents
    | otherwise = Nothing

normalizeCurrency :: T.Text -> T.Text
normalizeCurrency raw =
    T.toUpper (T.strip raw)

nonEmptyText :: T.Text -> Maybe T.Text
nonEmptyText txt =
    let trimmed = T.strip txt
     in if T.null trimmed then Nothing else Just trimmed

applyUploadExtension :: Maybe T.Text -> Maybe T.Text -> T.Text
applyUploadExtension name fallback =
    let resolved = fromMaybe "upload" name
        extFromFallback =
            case fallback of
                Nothing -> ""
                Just raw -> T.pack (takeExtension (T.unpack raw))
        extFromName = T.pack (takeExtension (T.unpack resolved))
     in if T.null extFromName && not (T.null extFromFallback)
            then resolved <> extFromFallback
            else resolved

sanitizeUploadFileName :: T.Text -> T.Text
sanitizeUploadFileName raw =
    let trimmed = T.strip raw
        baseName = T.pack (takeFileName (T.unpack trimmed))
        cleaned = T.map normalizeUploadChar baseName
        stripped = T.dropWhile (== '-') (T.dropWhileEnd (== '-') cleaned)
     in if T.null stripped || stripped == "." || stripped == ".."
            then "upload"
            else stripped

normalizeUploadChar :: Char -> Char
normalizeUploadChar ch
    | isAscii ch && isAlphaNum ch = ch
    | ch == '.' || ch == '-' || ch == '_' = ch
    | ch == ' ' = '-'
    | otherwise = '-'

buildUploadAssetUrl :: T.Text -> T.Text -> T.Text
buildUploadAssetUrl assetsBase relPath =
    let base = T.dropWhileEnd (== '/') assetsBase
        path = T.dropWhile (== '/') relPath
     in base <> "/" <> path

isImageUpload :: T.Text -> T.Text -> Bool
isImageUpload mimeType fileName
    | T.any isUnsafeUploadMetadataChar mimeType = False
    | otherwise =
        let normalizedMime = normalizeUploadMimeType mimeType
            ext = T.toLower (T.pack (takeExtension (T.unpack fileName)))
         in case normalizedMime of
                "image/jpeg" -> ext `elem` [".jpg", ".jpeg"]
                "image/png" -> ext == ".png"
                "image/webp" -> ext == ".webp"
                "image/gif" -> ext == ".gif"
                "image/bmp" -> ext == ".bmp"
                _ -> False

isUnsafeUploadMetadataChar :: Char -> Bool
isUnsafeUploadMetadataChar ch =
    isControl ch
        || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

maxEventImageUploadBytes :: Integer
maxEventImageUploadBytes = 10 * 1024 * 1024

validateEventImageUploadSize :: Integer -> Either ServerError ()
validateEventImageUploadSize size
    | size < 0 =
        Left err400{errBody = "event image upload size is invalid"}
    | size == 0 =
        Left err400{errBody = "event image upload must not be empty"}
    | size > maxEventImageUploadBytes =
        Left err400{errBody = "event image upload must be 10 MB or smaller"}
    | otherwise =
        Right ()

normalizeUploadMimeType :: T.Text -> T.Text
normalizeUploadMimeType raw =
    T.toLower (T.strip (fst (T.breakOn ";" raw)))

normalizeTicketTierCode :: T.Text -> T.Text
normalizeTicketTierCode raw =
    let upper = T.toUpper (T.strip raw)
        withDash = T.map (\c -> if c == ' ' then '-' else c) upper
        cleaned = T.filter (\c -> isAlphaNum c || c == '-' || c == '_') withDash
        chunks = filter (not . T.null) (T.splitOn "-" cleaned)
        normalized = T.intercalate "-" chunks
     in if T.null normalized then "GENERAL" else normalized

invalidSalesWindow :: Maybe UTCTime -> Maybe UTCTime -> Bool
invalidSalesWindow (Just startAt) (Just endAt) = startAt >= endAt
invalidSalesWindow _ _ = False

ticketTierAvailability :: EventTicketTier -> Int
ticketTierAvailability tier =
    max 0 (eventTicketTierQuantityTotal tier - eventTicketTierQuantitySold tier)

isTicketTierSaleOpen :: UTCTime -> EventTicketTier -> Bool
isTicketTierSaleOpen now tier =
    eventTicketTierIsActive tier
        && maybe True (<= now) (eventTicketTierSalesStart tier)
        && maybe True (>= now) (eventTicketTierSalesEnd tier)

isEventManager :: T.Text -> SocialEvent -> Bool
isEventManager currentParty eventRow =
    case cleanMaybeText (socialEventOrganizerPartyId eventRow) of
        Nothing -> False
        Just owner -> owner == currentParty

claimOrRequireEventManager :: T.Text -> ConnectionPool -> SocialEventId -> SocialEvent -> AppM SocialEvent
claimOrRequireEventManager currentParty pool eventKey eventRow =
    case cleanMaybeText (socialEventOrganizerPartyId eventRow) of
        Just owner | owner == currentParty -> pure eventRow
        Just _ -> throwError err403{errBody = "Only the event organizer can manage this event"}
        Nothing -> do
            now <- liftIO getCurrentTime
            liftIO $
                runSqlPool
                    (update eventKey [SocialEventOrganizerPartyId =. Just currentParty, SocialEventUpdatedAt =. now])
                    pool
            pure eventRow{socialEventOrganizerPartyId = Just currentParty, socialEventUpdatedAt = now}

loadEventArtists :: ConnectionPool -> SocialEventId -> AppM [ArtistDTO]
loadEventArtists pool eventKey = do
    loaded <-
        liftIO $
            runSqlPool
                ( do
                    artistLinks <- selectList [EventArtistEventId ==. eventKey] []
                    forM artistLinks $ \(Entity _ link) -> do
                        mArtist <- get (eventArtistArtistId link)
                        case mArtist of
                            Nothing ->
                                pure $
                                    Right
                                        ArtistDTO
                                            { artistId = Nothing
                                            , artistPartyId = Nothing
                                            , artistName = "(unknown)"
                                            , artistGenres = []
                                            , artistGenreIds = []
                                            , artistBio = Nothing
                                            , artistAvatarUrl = Nothing
                                            , artistSocialLinks = Nothing
                                            , artistCreatedAt = Nothing
                                            , artistUpdatedAt = Nothing
                                            }
                            Just a -> do
                                (genreList, genreIds) <- loadArtistGenreSelections (eventArtistArtistId link) a
                                pure (artistProfileToDTO (eventArtistArtistId link) a genreList genreIds)
                )
                pool
    either throwError pure (sequence loaded)

momentReactionEntityToDTO :: Map.Map UUID.UUID Catalog.ReactionType -> Entity EventMomentReaction -> Maybe EventMomentReactionDTO
momentReactionEntityToDTO reactionTypes (Entity _ reactionRow) = do
    reactionTypeId <- eventMomentReactionReactionTypeId reactionRow
    reactionType <- Map.lookup reactionTypeId reactionTypes
    pure
        EventMomentReactionDTO
            { emrReactionTypeId = UUID.toText reactionTypeId
            , emrReactionCode = Catalog.reactionTypeCode reactionType
            , emrReactionNameEs = Catalog.reactionTypeNameEs reactionType
            , emrReactionNameEn = Catalog.reactionTypeNameEn reactionType
            , emrReactionEmoji = Catalog.reactionTypeEmoji reactionType
            , emrPartyId = eventMomentReactionReactorPartyId reactionRow
            , emrCreatedAt = Just (eventMomentReactionCreatedAt reactionRow)
            }

momentCommentEntityToDTO :: EventMomentCommentId -> EventMomentComment -> EventMomentCommentDTO
momentCommentEntityToDTO commentKey commentRow =
    EventMomentCommentDTO
        { emcId = Just (renderKeyText commentKey)
        , emcMomentId = Just (renderKeyText (eventMomentCommentMomentId commentRow))
        , emcAuthorPartyId = eventMomentCommentAuthorPartyId commentRow
        , emcAuthorName = eventMomentCommentAuthorName commentRow
        , emcBody = eventMomentCommentBody commentRow
        , emcCreatedAt = Just (eventMomentCommentCreatedAt commentRow)
        , emcUpdatedAt = Just (eventMomentCommentUpdatedAt commentRow)
        }

momentEntityToDTO ::
    EventMomentId ->
    EventMoment ->
    [EventMomentReactionDTO] ->
    [EventMomentCommentDTO] ->
    EventMomentDTO
momentEntityToDTO momentKey momentRow reactions comments =
    EventMomentDTO
        { emId = Just (renderKeyText momentKey)
        , emEventId = Just (renderKeyText (eventMomentEventId momentRow))
        , emAuthorPartyId = eventMomentAuthorPartyId momentRow
        , emAuthorName = eventMomentAuthorName momentRow
        , emCaption = eventMomentCaption momentRow
        , emMediaUrl = eventMomentMediaUrl momentRow
        , emMediaType =
            fromMaybe
                (eventMomentMediaType momentRow)
                (normalizeMomentMediaType (eventMomentMediaType momentRow))
        , emMediaWidth = eventMomentMediaWidth momentRow
        , emMediaHeight = eventMomentMediaHeight momentRow
        , emMediaDurationMs = eventMomentMediaDurationMs momentRow
        , emCreatedAt = Just (eventMomentCreatedAt momentRow)
        , emUpdatedAt = Just (eventMomentUpdatedAt momentRow)
        , emReactions = reactions
        , emComments = comments
        }

loadMomentDTO :: ConnectionPool -> EventMomentId -> IO EventMomentDTO
loadMomentDTO pool momentKey =
    runSqlPool
        ( do
            mMoment <- get momentKey
            case mMoment of
                Nothing -> liftIO (ioError (userError "Moment not found"))
                Just momentRow -> do
                    reactionRows <- selectList [EventMomentReactionMomentId ==. momentKey] [Asc EventMomentReactionCreatedAt]
                    reactionTypes <- loadMomentReactionTypes reactionRows
                    commentRows <- selectList [EventMomentCommentMomentId ==. momentKey] [Asc EventMomentCommentCreatedAt]
                    let reactions = mapMaybe (momentReactionEntityToDTO reactionTypes) reactionRows
                        comments = map (\(Entity commentKey commentRow) -> momentCommentEntityToDTO commentKey commentRow) commentRows
                    when (length reactions /= length reactionRows) $
                        liftIO (ioError (userError "Moment reaction is missing its canonical reaction type"))
                    pure (momentEntityToDTO momentKey momentRow reactions comments)
        )
        pool

loadEventMoments :: ConnectionPool -> SocialEventId -> IO [EventMomentDTO]
loadEventMoments pool eventKey =
    runSqlPool
        ( do
            momentRows <- selectList [EventMomentEventId ==. eventKey] [Desc EventMomentCreatedAt]
            let momentKeys = map entityKey momentRows
            reactionRows <- selectList [EventMomentReactionMomentId <-. momentKeys] [Asc EventMomentReactionCreatedAt]
            reactionTypes <- loadMomentReactionTypes reactionRows
            commentRows <- selectList [EventMomentCommentMomentId <-. momentKeys] [Asc EventMomentCommentCreatedAt]
            let reactionsByMoment = Map.fromListWith (<>)
                    [ ( eventMomentReactionMomentId reactionRow
                      , maybe [] pure (momentReactionEntityToDTO reactionTypes reactionEntity)
                      )
                    | reactionEntity@(Entity _ reactionRow) <- reactionRows
                    ]
                commentsByMoment = Map.fromListWith (<>)
                    [ ( eventMomentCommentMomentId commentRow
                      , [momentCommentEntityToDTO commentKey commentRow]
                      )
                    | Entity commentKey commentRow <- commentRows
                    ]
                canonicalReactionCount = sum (map length (Map.elems reactionsByMoment))
            when (canonicalReactionCount /= length reactionRows) $
                liftIO (ioError (userError "Moment reaction is missing its canonical reaction type"))
            pure
                [ momentEntityToDTO
                    momentKey
                    momentRow
                    (Map.findWithDefault [] momentKey reactionsByMoment)
                    (Map.findWithDefault [] momentKey commentsByMoment)
                | Entity momentKey momentRow <- momentRows
                ]
        )
        pool

loadMomentReactionTypes :: [Entity EventMomentReaction] -> SqlPersistT IO (Map.Map UUID.UUID Catalog.ReactionType)
loadMomentReactionTypes reactionRows = do
    let reactionTypeIds = nub (catMaybes (map (eventMomentReactionReactionTypeId . entityVal) reactionRows))
    rows <- selectList [Catalog.ReactionTypeId <-. map Catalog.ReactionTypeKey reactionTypeIds] []
    pure (Map.fromList [(reactionTypeId, reactionType) | Entity (Catalog.ReactionTypeKey reactionTypeId) reactionType <- rows])

liveBroadcastEntityToDTO ::
    ConnectionPool ->
    EventLiveBroadcastId ->
    EventLiveBroadcast ->
    IO EventLiveBroadcastDTO
liveBroadcastEntityToDTO pool broadcastKey broadcastRow =
    runSqlPool
        ( do
            mArtist <- get (eventLiveBroadcastArtistId broadcastRow)
            let artistName = maybe "Artista" artistProfileName mArtist
            pure
                EventLiveBroadcastDTO
                    { elbId = Just (renderKeyText broadcastKey)
                    , elbEventId = Just (renderKeyText (eventLiveBroadcastEventId broadcastRow))
                    , elbArtistId = renderKeyText (eventLiveBroadcastArtistId broadcastRow)
                    , elbArtistName = artistName
                    , elbBroadcasterName = eventLiveBroadcastBroadcasterName broadcastRow
                    , elbBroadcasterPartyId = Just (eventLiveBroadcastBroadcasterPartyId broadcastRow)
                    , elbTitle = eventLiveBroadcastTitle broadcastRow
                    , elbDescription = eventLiveBroadcastDescription broadcastRow
                    , elbStatus = eventLiveBroadcastStatus broadcastRow
                    , elbPlaybackUrl = eventLiveBroadcastPlaybackUrl broadcastRow
                    , elbIngestUrl = eventLiveBroadcastIngestUrl broadcastRow
                    , elbWhipUrl = eventLiveBroadcastWhipUrl broadcastRow
                    , elbStreamKey = eventLiveBroadcastStreamKey broadcastRow
                    , elbViewerCount = eventLiveBroadcastViewerCount broadcastRow
                    , elbStartedAt = Just (eventLiveBroadcastStartedAt broadcastRow)
                    , elbEndedAt = eventLiveBroadcastEndedAt broadcastRow
                    , elbLastHeartbeatAt = Just (eventLiveBroadcastLastHeartbeatAt broadcastRow)
                    }
        )
        pool

loadLiveBroadcastDTO :: ConnectionPool -> EventLiveBroadcastId -> IO EventLiveBroadcastDTO
loadLiveBroadcastDTO pool broadcastKey =
    runSqlPool
        ( do
            mBroadcast <- get broadcastKey
            maybe
                (liftIO (ioError (userError "Live broadcast not found")))
                (\broadcastRow -> liftIO (liveBroadcastEntityToDTO pool broadcastKey broadcastRow))
                mBroadcast
        )
        pool

canAccessLiveBroadcast :: ConnectionPool -> T.Text -> EventLiveBroadcast -> IO Bool
canAccessLiveBroadcast pool partyId broadcastRow =
    if eventLiveBroadcastBroadcasterPartyId broadcastRow == partyId
        then pure True
        else do
            mFollow <-
                runSqlPool
                    (get (ArtistFollowKey (eventLiveBroadcastArtistId broadcastRow) partyId))
                    pool
            pure (isJust mFollow)

loadExternalEventSources ::
    ConnectionPool ->
    SocialEventId ->
    IO [EventSourceDTO]
loadExternalEventSources pool eventKey =
    runSqlPool
        (do
            refs <- selectList [ExternalEventRefEventId ==. eventKey] []
            ranked <-
                forM refs $ \(Entity _ ref) -> do
                    source <- getBy (UniqueEventDiscoverySource (externalEventRefProvider ref))
                    let (priority, label) =
                            case source of
                                Just (Entity _ sourceRow) ->
                                    ( eventDiscoverySourcePriority sourceRow
                                    , eventDiscoverySourceName sourceRow
                                    )
                                Nothing -> (1000, externalEventRefProvider ref)
                    pure
                        ( priority
                        , EventSourceDTO
                            { eventSourceProvider = externalEventRefProvider ref
                            , eventSourceLabel = label
                            , eventSourceUrl = externalEventRefSourceUrl ref
                            , eventSourcePriceCents = externalEventRefPriceCents ref
                            , eventSourceCurrency = externalEventRefCurrency ref
                            , eventSourceStatus = externalEventRefSourceStatus ref
                            }
                        )
            pure (map snd (sortOn (negate . fst) ranked))
        )
        pool

eventEntityToDTO :: T.Text -> SocialEventId -> SocialEvent -> [ArtistDTO] -> SqlPersistT IO (Either ServerError EventDTO)
eventEntityToDTO configuredDefault eid eventRow artists = do
    case decodeStoredEventMetadata (socialEventMetadata eventRow) of
      Left message -> pure (Left (storedEventMetadataServerError message))
      Right metadata -> case socialEventWorkflowStateId eventRow of
        Nothing -> pure (Left err500{errBody = "Event has no canonical workflow state"})
        Just workflowStateId -> do
          workflowState <- EventLifecycle.loadActiveSocialEventState workflowStateId
          case workflowState of
            Nothing -> pure (Left err500{errBody = "Event references an invalid workflow state"})
            Just (stateCode, nameEs, nameEn) -> do
              publicListable <- EventLifecycle.socialEventStateHasCapability workflowStateId "public-listable"
              ticketPurchaseEnabled <- EventLifecycle.socialEventStateHasCapability workflowStateId "ticket-purchase"
              pure . Right $
                EventDTO
                  { eventId = Just (renderKeyText eid)
                  , eventOrganizerPartyId = socialEventOrganizerPartyId eventRow
                  , eventTitle = socialEventTitle eventRow
                  , eventDescription = socialEventDescription eventRow
                  , eventStart = socialEventStartTime eventRow
                  , eventEnd = socialEventEndTime eventRow
                  , eventTimezone = socialEventTimezone eventRow
                  , eventVenueId = fmap renderKeyText (socialEventVenueId eventRow)
                  , eventPriceCents = socialEventPriceCents eventRow
                  , eventCapacity = socialEventCapacity eventRow
                  , eventTicketUrl = emTicketUrl metadata
                  , eventImageUrl = emImageUrl metadata
                  , eventIsPublic = emIsPublic metadata <|> Just True
                  , eventTypeId = UUID.toText <$> socialEventEventTypeId eventRow
                  , eventWorkflowStateId = Just (UUID.toText workflowStateId)
                  , eventWorkflowStateCode = Just stateCode
                  , eventWorkflowStateNameEs = Just nameEs
                  , eventWorkflowStateNameEn = Just nameEn
                  , eventPublicListable = Just publicListable
                  , eventTicketPurchaseEnabled = Just ticketPurchaseEnabled
                  , eventCurrency = emCurrency metadata <|> Just configuredDefault
                  , eventBudgetCents = emBudgetCents metadata
                  , eventSources = Nothing
                  , eventCreatedAt = Just (socialEventCreatedAt eventRow)
                  , eventUpdatedAt = Just (socialEventUpdatedAt eventRow)
                  , eventArtists = artists
                  }

ticketTierEntityToDTO :: SocialEventId -> Entity EventTicketTier -> TicketTierDTO
ticketTierEntityToDTO eventKey (Entity tierKey tier) =
    TicketTierDTO
        { ticketTierId = Just (renderKeyText tierKey)
        , ticketTierEventId = Just (renderKeyText eventKey)
        , ticketTierCode = eventTicketTierCode tier
        , ticketTierName = eventTicketTierName tier
        , ticketTierDescription = eventTicketTierDescription tier
        , ticketTierPriceCents = eventTicketTierPriceCents tier
        , ticketTierCurrency = eventTicketTierCurrency tier
        , ticketTierQuantityTotal = eventTicketTierQuantityTotal tier
        , ticketTierQuantitySold = eventTicketTierQuantitySold tier
        , ticketTierSalesStart = eventTicketTierSalesStart tier
        , ticketTierSalesEnd = eventTicketTierSalesEnd tier
        , ticketTierActive = eventTicketTierIsActive tier
        , ticketTierPosition = eventTicketTierPosition tier
        }

ticketEntityToDTO :: Entity EventTicket -> TicketDTO
ticketEntityToDTO (Entity ticketKey ticketRow) =
    TicketDTO
        { ticketId = Just (renderKeyText ticketKey)
        , ticketEventId = Just (renderKeyText (eventTicketEventId ticketRow))
        , ticketTierId = Just (renderKeyText (eventTicketTierRefId ticketRow))
        , ticketOrderId = Just (renderKeyText (eventTicketOrderRefId ticketRow))
        , ticketCode = eventTicketCode ticketRow
        , ticketStatus = normalizeTicketStatus (Just (eventTicketStatus ticketRow))
        , ticketHolderName = eventTicketHolderName ticketRow
        , ticketHolderEmail = eventTicketHolderEmail ticketRow
        , ticketCheckedInAt = eventTicketCheckedInAt ticketRow
        , ticketCreatedAt = Just (eventTicketCreatedAt ticketRow)
        , ticketUpdatedAt = Just (eventTicketUpdatedAt ticketRow)
        }

ticketOrderEntityToDTO :: Entity EventTicketOrder -> [Entity EventTicket] -> TicketOrderDTO
ticketOrderEntityToDTO (Entity orderKey orderRow) tickets =
    let feeBreakdown = decodeTicketPlatformFeeBreakdown orderRow
     in TicketOrderDTO
        { ticketOrderId = Just (renderKeyText orderKey)
        , ticketOrderEventId = Just (renderKeyText (eventTicketOrderEventId orderRow))
        , ticketOrderTierId = Just (renderKeyText (eventTicketOrderTierId orderRow))
        , ticketOrderBuyerPartyId = eventTicketOrderBuyerPartyId orderRow
        , ticketOrderBuyerName = eventTicketOrderBuyerName orderRow
        , ticketOrderBuyerEmail = eventTicketOrderBuyerEmail orderRow
        , ticketOrderQuantity = eventTicketOrderQuantity orderRow
        , ticketOrderFaceValueCents = ticketFaceValueCents feeBreakdown
        , ticketOrderBuyerPlatformFeeCents = ticketBuyerPlatformFeeCents feeBreakdown
        , ticketOrderOrganizerPlatformFeeCents = ticketOrganizerPlatformFeeCents feeBreakdown
        , ticketOrderAmountCents = eventTicketOrderAmountCents orderRow
        , ticketOrderCurrency = eventTicketOrderCurrency orderRow
        , ticketOrderStatusValue = normalizeTicketOrderStatus (Just (eventTicketOrderStatus orderRow))
        , ticketOrderPurchasedAt = Just (eventTicketOrderPurchasedAt orderRow)
        , ticketOrderCreatedAt = Just (eventTicketOrderCreatedAt orderRow)
        , ticketOrderUpdatedAt = Just (eventTicketOrderUpdatedAt orderRow)
        , ticketOrderTickets = map ticketEntityToDTO tickets
        }

promoCodeEntityToDTO :: Entity SM.PromoCode -> PromoCodeDTO
promoCodeEntityToDTO (Entity codeKey codeRow) =
    PromoCodeDTO
        { promoCodeId = Just (renderKeyText codeKey)
        , promoCodeEventId = fmap renderKeyText (SM.promoCodeEventId codeRow)
        , promoCodeCode = SM.promoCodeCode codeRow
        , promoCodeDescription = SM.promoCodeDescription codeRow
        , promoCodeDiscountType = normalizeStoredPromoCodeDiscountType (SM.promoCodeDiscountType codeRow)
        , promoCodeDiscountValue = SM.promoCodeDiscountValue codeRow
        , promoCodeCurrency = SM.promoCodeCurrency codeRow
        , promoCodeMaxRedemptions = SM.promoCodeMaxRedemptions codeRow
        , promoCodeCurrentRedemptions = SM.promoCodeCurrentRedemptions codeRow
        , promoCodeValidFrom = SM.promoCodeValidFrom codeRow
        , promoCodeValidUntil = SM.promoCodeValidUntil codeRow
        , promoCodeTierIds = decodeStoredPromoCodeTierIds (SM.promoCodeTierIds codeRow)
        , promoCodeMinPurchaseAmountCents = SM.promoCodeMinPurchaseAmountCents codeRow
        , promoCodeIsActive = SM.promoCodeIsActive codeRow
        , promoCodeCreatedAt = Just (SM.promoCodeCreatedAt codeRow)
        , promoCodeUpdatedAt = Just (SM.promoCodeUpdatedAt codeRow)
        }

normalizeStoredPromoCodeDiscountType :: T.Text -> T.Text
normalizeStoredPromoCodeDiscountType "fixed_amount" = "fixed"
normalizeStoredPromoCodeDiscountType discountType = discountType

refundEntityToDTO :: T.Text -> Entity TicketRefundRequest -> RefundDTO
refundEntityToDTO currency (Entity refundKey refundRow) =
    RefundDTO
        { refundId = Just (renderKeyText refundKey)
        , refundOrderId = renderKeyText (ticketRefundRequestOrderId refundRow)
        , refundRequestedByPartyId = ticketRefundRequestRequestedByPartyId refundRow
        , refundReason = ticketRefundRequestReason refundRow
        , refundAmountCents = ticketRefundRequestAmountCents refundRow
        , refundCurrency = currency
        , refundStatus = ticketRefundRequestStatus refundRow
        , refundApprovedByPartyId = ticketRefundRequestApprovedByPartyId refundRow
        , refundApprovedAt = ticketRefundRequestApprovedAt refundRow
        , refundRejectionReason = ticketRefundRequestRejectionReason refundRow
        , refundStripeRefundId = ticketRefundRequestStripeRefundId refundRow
        , refundProcessedAt = ticketRefundRequestProcessedAt refundRow
        , refundCreatedAt = Just (ticketRefundRequestCreatedAt refundRow)
        , refundUpdatedAt = Just (ticketRefundRequestUpdatedAt refundRow)
        }

transferEntityToDTO :: Entity TicketTransfer -> TicketTransferDTO
transferEntityToDTO (Entity transferKey transferRow) =
    TicketTransferDTO
        { ttId = Just (renderKeyText transferKey)
        , ttTicketId = renderKeyText (ticketTransferTicketId transferRow)
        , ttFromPartyId = ticketTransferFromPartyId transferRow
        , ttToEmail = fromMaybe "" (ticketTransferToEmail transferRow)
        , ttToName = ticketTransferToName transferRow
        , ttStatus = ticketTransferStatus transferRow
        , ttTransferCode = ticketTransferTransferCode transferRow
        , ttMessage = ticketTransferMessage transferRow
        , ttExpiresAt = ticketTransferExpiresAt transferRow
        , ttAcceptedAt = ticketTransferAcceptedAt transferRow
        , ttCreatedAt = Just (ticketTransferCreatedAt transferRow)
        , ttUpdatedAt = Just (ticketTransferUpdatedAt transferRow)
        }

waitlistEntityToDTO :: Entity EventWaitlist -> WaitlistEntryDTO
waitlistEntityToDTO (Entity waitlistKey waitlistRow) =
    WaitlistEntryDTO
        { weId = Just (renderKeyText waitlistKey)
        , weEventId = renderKeyText (eventWaitlistEventId waitlistRow)
        , weTierId = fmap renderKeyText (eventWaitlistTierId waitlistRow)
        , weEmail = eventWaitlistEmail waitlistRow
        , weName = eventWaitlistName waitlistRow
        , weQuantity = eventWaitlistQuantity waitlistRow
        , weStatus = eventWaitlistStatus waitlistRow
        , wePriority = eventWaitlistPriority waitlistRow
        , weNotifiedAt = eventWaitlistNotifiedAt waitlistRow
        , weExpiresAt = eventWaitlistExpiresAt waitlistRow
        , weConvertedOrderId = fmap renderKeyText (eventWaitlistConvertedOrderId waitlistRow)
        , weCreatedAt = Just (eventWaitlistCreatedAt waitlistRow)
        , weUpdatedAt = Just (eventWaitlistUpdatedAt waitlistRow)
        }

budgetLineEntityToDTO :: SocialEventId -> Maybe Int -> Entity EventBudgetLine -> EventBudgetLineDTO
budgetLineEntityToDTO eventKey mActualCents (Entity lineKey lineRec) =
    EventBudgetLineDTO
        { eblId = Just (renderKeyText lineKey)
        , eblEventId = Just (renderKeyText eventKey)
        , eblCode = eventBudgetLineCode lineRec
        , eblName = eventBudgetLineName lineRec
        , eblType = normalizeBudgetLineType (Just (eventBudgetLineLineType lineRec))
        , eblCategory = normalizeCategory (Just (eventBudgetLineCategory lineRec))
        , eblPlannedCents = eventBudgetLinePlannedCents lineRec
        , eblActualCents = mActualCents
        , eblNotes = eventBudgetLineNotes lineRec
        , eblCreatedAt = Just (eventBudgetLineCreatedAt lineRec)
        , eblUpdatedAt = Just (eventBudgetLineUpdatedAt lineRec)
        }

financeInvariantServerError :: T.Text -> ServerError
financeInvariantServerError message =
    err500{errBody = BL.fromStrict (TE.encodeUtf8 message)}

financeEntryEntityToDTOEither :: Entity EventFinanceEntry -> Either T.Text EventFinanceEntryDTO
financeEntryEntityToDTOEither (Entity entryKey entryRec) = do
    (directionVal, sourceVal, statusVal) <- validateStoredFinanceEntryDimensions entryRec
    pure
        EventFinanceEntryDTO
            { efeId = Just (renderKeyText entryKey)
            , efeEventId = Just (renderKeyText (eventFinanceEntryEventId entryRec))
            , efeBudgetLineId = fmap renderKeyText (eventFinanceEntryBudgetLineId entryRec)
            , efeDirection = directionVal
            , efeSource = sourceVal
            , efeCategory = normalizeCategory (Just (eventFinanceEntryCategory entryRec))
            , efeConcept = eventFinanceEntryConcept entryRec
            , efeAmountCents = eventFinanceEntryAmountCents entryRec
            , efeCurrency = normalizeCurrency (eventFinanceEntryCurrency entryRec)
            , efeStatus = statusVal
            , efeExternalRef = eventFinanceEntryExternalRef entryRec
            , efeNotes = eventFinanceEntryNotes entryRec
            , efeOccurredAt = eventFinanceEntryOccurredAt entryRec
            , efeRecordedByPartyId = eventFinanceEntryRecordedByPartyId entryRec
            , efeCreatedAt = Just (eventFinanceEntryCreatedAt entryRec)
            , efeUpdatedAt = Just (eventFinanceEntryUpdatedAt entryRec)
            }

storedFinanceEntrySummaryFields :: Entity EventFinanceEntry -> Either T.Text (Int, T.Text, T.Text, T.Text)
storedFinanceEntrySummaryFields (Entity _ entryRec) = do
    (directionVal, sourceVal, statusVal) <- validateStoredFinanceEntryDimensions entryRec
    pure (eventFinanceEntryAmountCents entryRec, directionVal, sourceVal, statusVal)

storedBudgetLineSummaryFields :: Entity EventBudgetLine -> Either T.Text (Int, T.Text)
storedBudgetLineSummaryFields (Entity _ lineRec) =
    validateStoredBudgetLineDimensions lineRec

storedTicketOrderSummaryFields :: Entity EventTicketOrder -> Either T.Text (Int, T.Text)
storedTicketOrderSummaryFields (Entity _ orderRec) =
    case parseTicketOrderStatus (eventTicketOrderStatus orderRec) of
        Just statusVal -> Right (eventTicketOrderAmountCents orderRec, statusVal)
        Nothing -> Left "Stored ticket order status is invalid"

ticketOrderAccountingEntriesEither ::
    SocialEventId ->
    Entity EventTicketOrder ->
    Either T.Text [EventFinanceEntryDTO]
ticketOrderAccountingEntriesEither eventKey orderEnt@(Entity _ orderRec) =
    case parseTicketOrderStatus (eventTicketOrderStatus orderRec) of
        Just statusVal -> Right (ticketOrderAccountingEntriesWithStatus eventKey orderEnt statusVal)
        Nothing -> Left "Stored ticket order status is invalid"

ticketOrderAccountingEntriesWithStatus ::
    SocialEventId ->
    Entity EventTicketOrder ->
    T.Text ->
    [EventFinanceEntryDTO]
ticketOrderAccountingEntriesWithStatus eventKey (Entity orderKey orderRec) statusVal =
    case statusVal of
        "paid" ->
            [ mkEntry "paid" "income" "ticket_sale" "posted" "Ticket sale"
            ]
        "refunded" ->
            [ mkEntry "refunded" "expense" "ticket_refund" "posted" "Ticket refund"
            ]
        "pending" ->
            [ mkEntry "pending" "income" "ticket_sale" "pending" "Ticket sale pending"
            ]
        _ -> []
  where
    orderIdTxt = renderKeyText orderKey
    feeBreakdown = decodeTicketPlatformFeeBreakdown orderRec
    organizerNetCents = max 0 (ticketFaceValueCents feeBreakdown - ticketOrganizerPlatformFeeCents feeBreakdown)
    mkEntry suffix direction source statusLabel conceptPrefix =
        EventFinanceEntryDTO
            { efeId = Just ("ticket-order-" <> orderIdTxt <> "-" <> suffix)
            , efeEventId = Just (renderKeyText eventKey)
            , efeBudgetLineId = Nothing
            , efeDirection = direction
            , efeSource = source
            , efeCategory = "tickets"
            , efeConcept = conceptPrefix <> " #" <> orderIdTxt
            , efeAmountCents =
                if source == "ticket_sale"
                    then organizerNetCents
                    else eventTicketOrderAmountCents orderRec
            , efeCurrency = normalizeCurrency (eventTicketOrderCurrency orderRec)
            , efeStatus = statusLabel
            , efeExternalRef = Just orderIdTxt
            , efeNotes = Nothing
            , efeOccurredAt = eventTicketOrderPurchasedAt orderRec
            , efeRecordedByPartyId = eventTicketOrderBuyerPartyId orderRec
            , efeCreatedAt = Just (eventTicketOrderCreatedAt orderRec)
            , efeUpdatedAt = Just (eventTicketOrderUpdatedAt orderRec)
            }

matchesFinanceFilters :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> EventFinanceEntryDTO -> Bool
matchesFinanceFilters mDirection mSource mStatus entry =
    directionOk && sourceOk && statusOk
  where
    directionOk = maybe True (== efeDirection entry) mDirection
    sourceOk = maybe True (== efeSource entry) mSource
    statusOk = maybe True (== efeStatus entry) mStatus

generateUniqueTicketCode :: (MonadIO m) => ReaderT SqlBackend m T.Text
generateUniqueTicketCode = do
    uuidVal <- liftIO UUIDV4.nextRandom
    let baseCode =
            T.toUpper
                (T.take 12 (T.replace "-" "" (UUID.toText uuidVal)))
        code = "TDF-" <> baseCode
    mExisting <- getBy (UniqueEventTicketCode code)
    case mExisting of
        Nothing -> pure code
        Just _ -> generateUniqueTicketCode
