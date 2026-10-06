{-# LANGUAGE OverloadedStrings #-}

-- | PayPal execution behind the existing organizer refund endpoints. A durable
-- canonical claim precedes HTTP; retries can only query the bound provider refund.
module TDF.Server.TicketRefunds
  (approvePaypalTicketRefund, approvePaypalTicketRefundWithManager) where

import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql
import Network.HTTP.Client (Manager)
import Servant
import System.Environment (lookupEnv)
import TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import TDF.DB (Env(..))
import TDF.Internationalization (formatMinorUnitsDecimal)
import qualified TDF.Models.SocialEventsModels as M
import qualified TDF.Commerce.CheckoutStore as C
import qualified TDF.Commerce.RefundStore as R
import qualified TDF.Commerce.RefundReconciliation as Recovery
import qualified TDF.Commerce.ProviderAdapter.PayPalRefund as Q
import qualified TDF.Commerce.ProviderAdapter.Http as Http
import qualified TDF.Ticketing.Refund as Ticket
import TDF.Server.ProviderTransport

type AppM = ReaderT Env Handler

approvePaypalTicketRefund :: AuthedUser -> M.SocialEventId -> M.TicketRefundRequestId
  -> AppM (Entity M.TicketRefundRequest)
approvePaypalTicketRefund = approvePaypalTicketRefundWithManager Http.sharedProviderManager

-- Injection keeps the same bounded HTTP executor for deterministic transport tests.
approvePaypalTicketRefundWithManager :: Manager -> AuthedUser -> M.SocialEventId
  -> M.TicketRefundRequestId -> AppM (Entity M.TicketRefundRequest)
approvePaypalTicketRefundWithManager manager user eventKey requestKey = do
  env <- ask
  now <- liftIO getCurrentTime
  -- Authorize against stored event/order relationships before reading credentials.
  (orderKey, ref, existing, capture) <- transaction $ do
    request <- get requestKey >>= maybe (fail "Refund not found") pure
    let orderKey = M.ticketRefundRequestOrderId request
    Ticket.withOrder orderKey $ \event order _ -> do
      unless (M.eventTicketOrderEventId order == eventKey &&
        (hasStrictAdminAccess user || M.socialEventOrganizerPartyId event == Just actor)) $
        fail "Refund approval is not authorized"
      ref <- Ticket.loadTicketRefundReference requestKey >>= maybe (fail "Canonical refund missing") pure
      record <- R.loadRefund ref >>= maybe (fail "Canonical refund missing") pure
      unless (R.rrStatus record == "succeeded" || R.rrRequestedBy record /= actorId) $
        fail "Refund approval requires a different authenticated party"
      capture <- loadCapture record
      pure (orderKey,ref,record,capture)
  if R.rrStatus existing == "succeeded" then reload else do
    (cid, secret, baseUrl, environment, merchant) <- loadPaypalEnvForService
    unless (R.rrProvider existing == "paypal" &&
      C.checkoutEnvironmentText environment == R.rrEnvironment existing &&
      merchant == R.rrMerchantRef existing) $
      throwError err503 { errBody="Refund provider configuration is unavailable" }
    enabled <- liftIO (lookupEnv "PAYPAL_REFUND_RECONCILIATION_ENABLED")
    unless (enabled == Just "true") $
      throwError err503 { errBody="Refund reconciliation is unavailable" }
    let readiness = Q.RefundQueryBinding environment (R.refundReferenceId ref) capture merchant
          (R.rrAmountMinor existing) (R.rrCurrency existing)
    -- A failed token request has not submitted a refund: keep the request
    -- pending so it can be safely retried. Never hold database locks during HTTP.
    token <- paypalAccessTokenForService manager cid secret baseUrl
    (record, shouldIssue) <- transaction $ Ticket.withOrder orderKey $ \event order _ -> do
      unless (M.eventTicketOrderEventId order == eventKey &&
        (hasStrictAdminAccess user || M.socialEventOrganizerPartyId event == Just actor)) $
        fail "Refund approval authority changed"
      enabled <- C.capabilityEnabledForEnvironment environment "checkout.paypal.refunds"
      ready <- Recovery.queryReady readiness
      unless (enabled && ready) $ fail "Verified refund capabilities are unavailable"
      result <- either (fail . T.unpack) pure =<< R.approveRefundForProcessing ref actorId now
      when (snd result) $ update requestKey
        [M.TicketRefundRequestStatus =. "processing",M.TicketRefundRequestApprovedByPartyId =. Just actor,
         M.TicketRefundRequestApprovedAt =. Just now,M.TicketRefundRequestUpdatedAt =. now]
      pure result
    when shouldIssue $ do
      outcome <- issuePaypalRefundWithTokenRemote manager token baseUrl capture record
        `catchError` (\failure -> do
          transaction (R.recordRefundFailure ref "ticket_refund_transport_unknown" now)
          throwError failure)
      unless (proCurrency outcome == R.rrCurrency record &&
        proAmount outcome == formatMinorUnitsDecimal (R.rrCurrency record) (fromIntegral (R.rrAmountMinor record))) $ do
        transaction (R.recordRefundFailure ref "ticket_refund_response_mismatch" now)
        throwError err502 { errBody="Refund outcome requires reconciliation; funds remain reserved" }
      transaction $ either (fail . T.unpack) pure =<< R.recordRefundPending ref (proRefundId outcome) now
    current <- transaction $ R.loadRefund ref >>= maybe (fail "Refund missing") pure
    when (R.rrStatus current == "processing") $ case R.rrProviderRefundId current of
      Nothing -> pure () -- Unknown POST outcome is never permission for another POST.
      Just _ -> do
        result <- liftIO $ Recovery.reconcileKnownRefund (envPool env) ref actorId
          (\binding -> pure (Q.rqbEnvironment binding == environment && Q.rqbMerchantId binding == merchant))
          (\binding -> do
            queried <- runHandler (runReaderT (queryRefund manager token binding) env)
            pure (either (const (Left Recovery.RefundRecoveryQueryFailed)) Right queried))
        case result of
          Right _ -> pure ()
          Left Recovery.RefundRecoveryRateLimited -> throwError err429 { errBody="Refund query limit reached" }
          Left _ -> throwError err503 { errBody="Refund remains reserved pending provider reconciliation" }
    reload
  where
    actorId = fromSqlKey (auPartyId user)
    actor = T.pack (show actorId)
    reload = transaction $ getEntity requestKey >>= maybe (fail "Refund missing") pure

-- IO user errors carry domain denials, not provider payloads or database details.
transaction :: SqlPersistT IO a -> AppM a
transaction action = do
  env <- ask
  result <- liftIO $ tryIO (runSqlPool action (envPool env))
  either (const (throwError err409 {errBody="Refund state or authorization does not permit this operation"})) pure result
  where tryIO :: IO a -> IO (Either IOException a)
        tryIO = try

loadCapture :: R.RefundRecord -> SqlPersistT IO Text
loadCapture record = do
  rows <- rawSql
    "SELECT binding.provider_resource_id FROM commerce_provider_binding binding\
    \ JOIN commerce_payment_attempt attempt ON attempt.id=binding.payment_attempt_id\
    \ JOIN commerce_checkout_session checkout ON checkout.id=attempt.checkout_id\
    \ WHERE checkout.id=?::uuid AND attempt.id=?::uuid AND checkout.domain_type='event_ticket_order'\
    \ AND attempt.status='succeeded' AND binding.resource_type='capture'\
    \ AND binding.provider='paypal' AND binding.provider=attempt.provider\
    \ AND binding.environment=attempt.environment AND checkout.environment=attempt.environment\
    \ AND binding.merchant_account_ref=attempt.merchant_account_ref\
    \ AND binding.amount_minor=attempt.amount_minor AND checkout.total_minor=attempt.amount_minor\
    \ AND binding.currency=attempt.currency AND checkout.currency=attempt.currency\
    \ AND attempt.environment=? AND attempt.merchant_account_ref=? AND attempt.currency=?"
    [PersistText (C.checkoutReferenceId (R.rrCheckout record)),PersistText (C.paymentAttemptReferenceId (R.rrPaymentAttempt record)),
     PersistText (R.rrEnvironment record),PersistText (R.rrMerchantRef record),PersistText (R.rrCurrency record)]
  case rows of
    [Single capture] | isProviderReference capture -> pure capture
    _ -> fail "Refund requires one exact original capture"

queryRefund :: Manager -> Text -> Q.RefundQueryBinding -> AppM Q.RefundQueryOutcome
queryRefund manager token binding = do
  void $ either (const (throwError err409)) pure (Q.validateRefundQueryBinding binding)
  request <- either (const (throwError err503)) pure (Q.buildRefundQuery token binding)
  response <- liftIO (Http.executeAdapterRequest manager request)
  value <- either (const (throwError err502)) pure response
  either (const (throwError err502)) pure (Q.parseRefundQuery binding value)
