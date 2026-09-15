{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.CommerceOperations
  ( commerceOperationsServer
  , validateProviderEventReplayReason
  , validateProviderQueryFilters
  , providerQueryOutcome
  , validateReconciliationFilters
  ) where

import           Control.Monad (unless)
import           Control.Exception.Safe (tryAny)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Data.Char (isControl)
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql
  ( Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool )
import           Servant

import           TDF.API.CommerceOperations
import           TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import qualified TDF.Commerce.ProviderEventStore as ProviderEvent
import qualified TDF.Commerce.ProviderExecutionStore as ProviderExecution
import           TDF.DB (Env(..))

type AppM = ReaderT Env Handler

commerceOperationsServer
  :: AuthedUser
  -> ServerT CommerceOperationsAPI AppM
commerceOperationsServer user =
       (requireAccess *> paymentOverviewHandler)
  :<|> (\status limit offset -> requireAccess *> listProviderEventsHandler status limit offset)
  :<|> (\eventId request -> requireAccess *> replayProviderEventHandler user eventId request)
  :<|> (\environment status limit offset -> requireAccess *>
          (addHeader ("no-store" :: Text) <$>
            listProviderQueriesHandler environment status limit offset))
  :<|> (\environment status checkout limit offset -> requireAccess *>
          (addHeader ("no-store" :: Text) <$>
            listReconciliationHandler environment status checkout limit offset))
  where
    requireAccess = unless (hasStrictAdminAccess user) $
      throwError err403 { errBody = "Strict Admin access required" }

validateReconciliationFilters
  :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int
  -> Either Text (Text, Maybe Text, Maybe Text, Int, Int)
validateReconciliationFilters rawEnvironment rawStatus rawCheckout rawLimit rawOffset = do
  (environment, _, limit, offset) <-
    validateProviderQueryFilters rawEnvironment Nothing rawLimit rawOffset
  status <- case T.toLower . T.strip <$> rawStatus of
    Nothing -> Right Nothing
    Just "open" -> Right (Just "open")
    Just "assigned" -> Right (Just "assigned")
    Just "resolved" -> Right (Just "resolved")
    Just "ignored" -> Right (Just "ignored")
    _ -> Left "Unsupported reconciliation status"
  checkout <- case rawCheckout of
    Nothing -> Right Nothing
    Just value -> maybe (Left "Invalid reconciliation checkout identifier")
      (Right . Just . UUID.toText) (UUID.fromText (T.strip value))
  pure (environment, status, checkout, limit, offset)

listReconciliationHandler
  :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int
  -> AppM CommerceReconciliationReportDTO
listReconciliationHandler rawEnvironment rawStatus rawCheckout rawLimit rawOffset = do
  (environment, status, checkout, limit, offset) <- either (throwError . badRequest) pure $
    validateReconciliationFilters rawEnvironment rawStatus rawCheckout rawLimit rawOffset
  Env{envPool = pool} <- ask
  observed <- liftIO getCurrentTime
  result <- liftIO $ tryAny $ flip runSqlPool pool $ do
    rawExecute "SET TRANSACTION READ ONLY" []
    rawExecute "SET LOCAL statement_timeout = '3s'" []
    installed <- rawSql
      "SELECT to_regclass('commerce_reconciliation_exception') IS NOT NULL\
      \ AND to_regclass('commerce_provider_binding') IS NOT NULL\
      \ AND to_regclass('commerce_payment_attempt') IS NOT NULL\
      \ AND to_regclass('commerce_checkout_session') IS NOT NULL" []
    if installed /= [Single True] then pure (False, []) else do
      entries <- loadReconciliationEntries environment status checkout (limit + 1) offset
      pure (True, entries)
  case result of
    Left _ -> throwError err503
      { errBody = "Payment reconciliation report is temporarily unavailable"
      , errHeaders = [("Cache-Control", "no-store")]
      }
    Right (ready, entries) -> pure CommerceReconciliationReportDTO
      { crrGeneratedAt = observed, crrEnvironment = environment, crrStatus = status
      , crrCheckoutId = checkout, crrSchemaReady = ready, crrEntries = take limit entries
      , crrLimit = limit, crrOffset = offset, crrHasMore = length entries > limit
      }

-- Link only a unique stored attempt whose resource/account/environment and
-- checkout agree. Never cast or return arbitrary legacy internal references.
-- The link identifies a record; it is not proof of a capture or settlement.
loadReconciliationEntries
  :: Text -> Maybe Text -> Maybe Text -> Int -> Int
  -> SqlPersistT IO [CommerceReconciliationEntryDTO]
loadReconciliationEntries environment status checkout limit offset = do
  rows <- rawSql
    ("SELECT review.id::text,review.provider,review.status,review.exception_type,\
     \ linked.checkout_id,linked.attempt_id,review.expected_amount_minor::text,\
     \ review.actual_amount_minor::text,\
     \ CASE WHEN review.currency ~ '^[A-Z]{3}$' THEN review.currency ELSE NULL END,\
     \ review.detected_at,review.resolved_at\
     \ FROM (SELECT id,provider,status,exception_type,internal_reference,provider_reference,\
     \ merchant_account_ref,environment,expected_amount_minor,actual_amount_minor,currency,\
     \ detected_at,resolved_at FROM commerce_reconciliation_exception WHERE environment=?"
     <> maybe "" (const " AND status=?") status
     <> maybe "" (const " AND internal_reference=?") checkout <>
     " ORDER BY detected_at DESC,id DESC LIMIT ? OFFSET ?) review\
     \ LEFT JOIN LATERAL (SELECT\
     \ CASE WHEN COUNT(DISTINCT attempt.id)=1 THEN MIN(checkout.id::text) END AS checkout_id,\
     \ CASE WHEN COUNT(DISTINCT attempt.id)=1 THEN MIN(attempt.id::text) END AS attempt_id\
     \ FROM commerce_provider_binding binding\
     \ JOIN commerce_payment_attempt attempt ON attempt.id=binding.payment_attempt_id\
     \ JOIN commerce_checkout_session checkout ON checkout.id=attempt.checkout_id\
     \ WHERE binding.provider=review.provider AND binding.environment=review.environment\
     \ AND binding.merchant_account_ref=review.merchant_account_ref\
     \ AND binding.provider_resource_id=review.provider_reference\
     \ AND checkout.id::text=review.internal_reference AND checkout.environment=binding.environment\
     \ AND attempt.provider=binding.provider AND attempt.environment=binding.environment\
     \ AND attempt.merchant_account_ref=binding.merchant_account_ref\
     \ AND attempt.amount_minor=binding.amount_minor AND attempt.currency=binding.currency\
     \ AND checkout.total_minor=binding.amount_minor AND checkout.currency=binding.currency) linked ON TRUE\
     \ ORDER BY review.detected_at DESC,review.id DESC")
    ([PersistText environment] <> maybe [] (pure . PersistText) status
      <> maybe [] (pure . PersistText) checkout <>
      [PersistInt64 (fromIntegral limit), PersistInt64 (fromIntegral offset)])
    :: SqlPersistT IO
      [(Single Text,Single Text,Single Text,Single Text,Single (Maybe Text),
        Single (Maybe Text),Single (Maybe Text),Single (Maybe Text),Single (Maybe Text),
        Single UTCTime,Single (Maybe UTCTime))]
  pure [ CommerceReconciliationEntryDTO ident (safeProvider provider) (safeStatus state)
           (safeReason reason) checkoutId attempt expected actual currency detected resolved
       | (Single ident,Single provider,Single state,Single reason,Single checkoutId,
          Single attempt,Single expected,Single actual,Single currency,Single detected,
          Single resolved) <- rows ]
  where
    safeProvider provider = case provider of
      "datafast" -> "datafast"
      "paypal" -> "paypal"
      "placetopay" -> "placetopay"
      "payphone" -> "payphone"
      "bank_transfer" -> "bank_transfer"
      "stripe" -> "stripe"
      _ -> "unrecognized"
    safeStatus state = case state of
      "open" -> "open"
      "assigned" -> "assigned"
      "resolved" -> "resolved"
      "ignored" -> "ignored"
      _ -> "unrecognized"
    safeReason reason = case reason of
      "verified_payment_on_closed_checkout" -> "closed_checkout_approval"
      "scheduled_query_requires_review" -> "scheduled_query_review"
      "provider_query_binding_mismatch" -> "binding_mismatch"
      "provider_status_unknown" -> "unknown_provider_state"
      _ -> "unrecognized"

-- The gate above runs before filter validation and before any database access.
-- Reading this view never reserves a provider budget, claims a lease or sends HTTP.
listProviderQueriesHandler
  :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int
  -> AppM CommerceProviderQueriesDTO
listProviderQueriesHandler rawEnvironment rawStatus rawLimit rawOffset = do
  (environment, status, limit, offset) <- either (throwError . badRequest) pure $
    validateProviderQueryFilters rawEnvironment rawStatus rawLimit rawOffset
  Env{envPool = pool} <- ask
  now <- liftIO getCurrentTime
  result <- liftIO $ tryAny $ flip runSqlPool pool $ do
    rawExecute "SET TRANSACTION READ ONLY" []
    -- Bound the reporting read independently of worker locks and provider calls.
    rawExecute "SET LOCAL statement_timeout = '3s'" []
    installed <- ProviderExecution.providerQueryRecoveryInstalled
    if not installed then pure (False, False, [], []) else do
      flags <- rawSql
        "SELECT enabled FROM revenue_feature_flag\
        \ WHERE flag_key='checkout.provider_query_recovery' AND environment=?"
        [PersistText environment] :: SqlPersistT IO [Single Bool]
      budgetRows <- rawSql
        "SELECT provider,next_query_at FROM commerce_provider_query_budget\
        \ WHERE environment=? ORDER BY provider LIMIT 100"
        [PersistText environment] :: SqlPersistT IO [(Single Text,Single UTCTime)]
      jobs <- loadProviderQueries environment status (limit + 1) offset
      pure (True, flags == [Single True], jobs,
        [CommerceProviderQueryBudgetDTO provider next | (Single provider,Single next) <- budgetRows])
  case result of
    Left _ -> throwError err503
      { errBody = "Payment query report is temporarily unavailable"
      , errHeaders = [("Cache-Control", "no-store")]
      }
    Right (installed, flagEnabled, jobs, budgets) -> pure CommerceProviderQueriesDTO
      { cpqsGeneratedAt = now, cpqsEnvironment = environment
      , cpqsSchemaReady = installed, cpqsRecoveryFlagEnabled = flagEnabled
      , cpqsJobs = take limit jobs, cpqsBudgets = budgets
      , cpqsLimit = limit, cpqsOffset = offset, cpqsHasMore = length jobs > limit
      }

validateProviderQueryFilters
  :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int
  -> Either Text (Text, Maybe Text, Int, Int)
validateProviderQueryFilters rawEnvironment rawStatus rawLimit rawOffset = do
  environment <- case T.toLower . T.strip <$> rawEnvironment of
    Nothing -> Right "sandbox"
    Just "sandbox" -> Right "sandbox"
    Just "production" -> Right "production"
    _ -> Left "Unsupported payment query environment"
  status <- case T.toLower . T.strip <$> rawStatus of
    Nothing -> Right Nothing
    Just "pending" -> Right (Just "pending")
    Just "processing" -> Right (Just "processing")
    Just "retry" -> Right (Just "retry")
    Just "completed" -> Right (Just "completed")
    Just "dead_letter" -> Right (Just "dead_letter")
    _ -> Left "Unsupported payment query status"
  let limit = maybe 25 id rawLimit
      offset = maybe 0 id rawOffset
  if limit < 1 || limit > 100 || offset < 0 || offset > 10000
    then Left "Payment query limit must be 1-100 and offset 0-10000"
    else Right (environment, status, limit, offset)

-- Project only known server-authored outcomes. A regex-safe database value can
-- still contain an accidental credential or other private diagnostic.
providerQueryOutcome :: Text -> Text
providerQueryOutcome code = case code of
  "retry_exhausted" -> "retry_exhausted"
  "process_switch_disabled" -> "process_switch_disabled"
  "binding_changed" -> "binding_changed"
  "query_unavailable" -> "query_unavailable"
  "query_unsupported" -> "query_unsupported"
  "query_binding_mismatch" -> "query_binding_mismatch"
  "query_application_rejected" -> "query_application_rejected"
  "query_applied" -> "query_applied"
  "provider_nonterminal" -> "provider_nonterminal"
  "provider_requires_review" -> "provider_requires_review"
  "configuration_revoked" -> "configuration_revoked"
  "operation_already_terminal" -> "operation_already_terminal"
  "immutable_binding_unavailable" -> "immutable_binding_unavailable"
  _ -> "unrecognized"

loadProviderQueries :: Text -> Maybe Text -> Int -> Int -> SqlPersistT IO [CommerceProviderQueryDTO]
loadProviderQueries environment status limit offset = do
  rows <- rawSql
    ("SELECT job.operation_id::text,attempt.checkout_id::text,attempt.id::text,\
     \ operation.provider,job.status,job.attempt_count,operation.status,operation.outcome_certainty,\
     \ job.created_at,job.last_attempt_at,job.next_attempt_at,job.lease_expires_at,\
     \ job.completed_at,job.last_error_code\
     \ FROM commerce_provider_query_job job\
     \ JOIN commerce_provider_operation operation ON operation.id=job.operation_id\
     \ JOIN commerce_payment_attempt attempt ON attempt.id=operation.payment_attempt_id\
     \ WHERE operation.environment=?"
     <> maybe "" (const " AND job.status=?") status <>
     " ORDER BY job.created_at DESC,job.operation_id DESC LIMIT ? OFFSET ?")
    ([PersistText environment] <> maybe [] (pure . PersistText) status <>
      [PersistInt64 (fromIntegral limit),PersistInt64 (fromIntegral offset)])
    :: SqlPersistT IO
      [(Single Text,Single Text,Single Text,Single Text,Single Text,Single Int,
        Single Text,Single Text,Single UTCTime,Single (Maybe UTCTime),Single UTCTime,
        Single (Maybe UTCTime),Single (Maybe UTCTime),Single (Maybe Text))]
  pure [ CommerceProviderQueryDTO operation checkout attempt provider jobStatus attempts
           operationStatus certainty created lastAttempt next lease completed
           (providerQueryOutcome <$> outcome)
       | (Single operation,Single checkout,Single attempt,Single provider,Single jobStatus,
          Single attempts,Single operationStatus,Single certainty,Single created,
          Single lastAttempt,Single next,Single lease,Single completed,Single outcome) <- rows ]

paymentOverviewHandler :: AppM CommercePaymentOverviewDTO
paymentOverviewHandler = do
  Env{..} <- ask
  generatedAt <- liftIO getCurrentTime
  ( accounts, intents, amountComponents, commissions, refunds, disputes
    , exceptions, settlements, sellerBalances, payouts ) <-
    liftIO $ flip runSqlPool envPool $ do
      accounts <- loadProviderAccounts
      intents <- loadPaymentIntentSummaries
      amountComponents <- loadAmountComponentSummaries
      commissions <- loadCommissionSummaries
      refunds <- loadRefundSummaries
      disputes <- loadDisputeSummaries
      exceptions <- loadReconciliationSummaries
      settlements <- loadSettlementSummaries
      sellerBalances <- loadSellerBalanceSummaries generatedAt
      payouts <- loadPayoutSummaries
      pure
        ( accounts, intents, amountComponents, commissions, refunds, disputes
        , exceptions, settlements, sellerBalances, payouts )
  pure CommercePaymentOverviewDTO
    { cpoGeneratedAt = generatedAt
    , cpoProviderAccounts = accounts
    , cpoPaymentIntents = intents
    , cpoAmountComponents = amountComponents
    , cpoCommissions = commissions
    , cpoRefunds = refunds
    , cpoDisputes = disputes
    , cpoReconciliationExceptions = exceptions
    , cpoSettlements = settlements
    , cpoSellerBalances = sellerBalances
    , cpoPayouts = payouts
    }

loadProviderAccounts :: SqlPersistT IO [CommerceProviderAccountDTO]
loadProviderAccounts = do
  accountRows <- rawSql
    "SELECT account.id::text, account.provider, account.environment, account.status,\
    \ account.contract_status, account.credential_status, account.settlement_currency,\
    \ account.enabled, COALESCE(flag.enabled, account.environment = 'sandbox'), account.verified_at,\
    \ account.disabled_reason\
    \ FROM commerce_provider_account account\
    \ LEFT JOIN revenue_feature_flag flag\
    \   ON flag.flag_key = account.feature_flag_key\
    \  AND flag.environment = account.environment\
    \ ORDER BY account.environment, account.provider"
    [] :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Text, Single Text
       , Single Text, Single Text, Single Bool, Single Bool, Single (Maybe UTCTime)
       , Single (Maybe Text)
       )]
  capabilityRows <- rawSql
    "SELECT capability.provider_account_id::text, capability.payment_method,\
    \ capability.capability, capability.verification_status, capability.verified_at\
    \ FROM commerce_provider_capability capability\
    \ ORDER BY capability.provider_account_id, capability.payment_method, capability.capability"
    [] :: SqlPersistT IO
      [(Single Text, Single Text, Single Text, Single Text, Single (Maybe UTCTime))]
  pure
    [ CommerceProviderAccountDTO
        { cpaProvider = provider
        , cpaEnvironment = environment
        , cpaStatus = status
        , cpaContractStatus = contractStatus
        , cpaCredentialStatus = credentialStatus
        , cpaSettlementCurrency = settlementCurrency
        , cpaEnabled = enabled
        , cpaFeatureEnabled = featureEnabled
        , cpaVerifiedAt = verifiedAt
        , cpaDisabledReason = disabledReason
        , cpaCapabilities =
            [ CommerceProviderCapabilityDTO method capability verification capabilityVerifiedAt
            | ( Single capabilityAccountId, Single method, Single capability
              , Single verification, Single capabilityVerifiedAt
              ) <- capabilityRows
            , capabilityAccountId == accountId
            ]
        }
    | ( Single accountId, Single provider, Single environment, Single status
      , Single contractStatus, Single credentialStatus, Single settlementCurrency
      , Single enabled, Single featureEnabled, Single verifiedAt, Single disabledReason
      ) <- accountRows
    ]

loadPaymentIntentSummaries
  :: SqlPersistT IO [CommercePaymentIntentSummaryDTO]
loadPaymentIntentSummaries = do
  rows <- rawSql
    "SELECT status, currency, COUNT(*)::bigint, COALESCE(SUM(amount_minor), 0)::bigint,\
    \ COALESCE(SUM(authorized_minor), 0)::bigint, COALESCE(SUM(captured_minor), 0)::bigint,\
    \ COALESCE(SUM(refunded_minor), 0)::bigint\
    \ FROM commerce_payment_intent GROUP BY status, currency ORDER BY currency, status"
    [] :: SqlPersistT IO
      [(Single Text, Single Text, Single Int64, Single Int64, Single Int64,
        Single Int64, Single Int64)]
  pure
    [ CommercePaymentIntentSummaryDTO status currency count amount authorized captured refunded
    | ( Single status, Single currency, Single count, Single amount
      , Single authorized, Single captured, Single refunded
      ) <- rows
    ]

loadAmountComponentSummaries
  :: SqlPersistT IO [CommerceAmountComponentSummaryDTO]
loadAmountComponentSummaries = do
  rows <- rawSql
    "SELECT component_type, source, currency, COUNT(*)::bigint,\
    \ COALESCE(SUM(amount_minor), 0)::bigint\
    \ FROM commerce_payment_amount_component\
    \ GROUP BY component_type, source, currency\
    \ ORDER BY currency, component_type, source"
    [] :: SqlPersistT IO
      [(Single Text, Single Text, Single Text, Single Int64, Single Int64)]
  pure
    [ CommerceAmountComponentSummaryDTO componentType source currency count amount
    | ( Single componentType, Single source, Single currency, Single count
      , Single amount ) <- rows
    ]

loadCommissionSummaries :: SqlPersistT IO [CommerceCommissionSummaryDTO]
loadCommissionSummaries = do
  rows <- rawSql
    "SELECT account.provider, account.environment, commission.currency,\
    \ COUNT(*)::bigint, COALESCE(SUM(commission.basis_amount_minor), 0)::bigint,\
    \ COALESCE(SUM(commission.commission_minor), 0)::bigint,\
    \ COALESCE(SUM(commission.provider_fee_minor), 0)::bigint,\
    \ COALESCE(SUM(commission.tax_minor), 0)::bigint,\
    \ COALESCE(SUM(commission.seller_net_minor), 0)::bigint\
    \ FROM commerce_commission commission\
    \ JOIN commerce_connected_account account\
    \   ON account.id = commission.connected_account_id\
    \ GROUP BY account.provider, account.environment, commission.currency\
    \ ORDER BY account.environment, account.provider, commission.currency"
    [] :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Int64, Single Int64
       , Single Int64, Single Int64, Single Int64, Single Int64 )]
  pure
    [ CommerceCommissionSummaryDTO provider environment currency count basis
        commissionAmount providerFee tax sellerNet
    | ( Single provider, Single environment, Single currency, Single count
      , Single basis, Single commissionAmount, Single providerFee, Single tax
      , Single sellerNet ) <- rows
    ]

loadRefundSummaries :: SqlPersistT IO [CommerceRefundSummaryDTO]
loadRefundSummaries = do
  rows <- rawSql
    "SELECT COALESCE(refund.provider, attempt.provider),\
    \ COALESCE(refund.environment, attempt.environment), refund.status,\
    \ refund.currency, COUNT(*)::bigint,\
    \ COALESCE(SUM(refund.amount_minor), 0)::bigint\
    \ FROM commerce_refund refund\
    \ JOIN commerce_payment_attempt attempt ON attempt.id = refund.payment_attempt_id\
    \ GROUP BY COALESCE(refund.provider, attempt.provider),\
    \ COALESCE(refund.environment, attempt.environment), refund.status, refund.currency\
    \ ORDER BY 2, 1, refund.currency, refund.status"
    [] :: SqlPersistT IO
      [(Single Text, Single Text, Single Text, Single Text, Single Int64, Single Int64)]
  pure
    [ CommerceRefundSummaryDTO provider environment status currency count amount
    | ( Single provider, Single environment, Single status, Single currency
      , Single count, Single amount ) <- rows
    ]

loadDisputeSummaries :: SqlPersistT IO [CommerceDisputeSummaryDTO]
loadDisputeSummaries = do
  rows <- rawSql
    "SELECT attempt.provider, attempt.environment, dispute.kind, dispute.status,\
    \ dispute.currency, COUNT(*)::bigint,\
    \ COALESCE(SUM(dispute.amount_minor), 0)::bigint\
    \ FROM commerce_dispute dispute\
    \ JOIN commerce_payment_attempt attempt ON attempt.id = dispute.payment_attempt_id\
    \ GROUP BY attempt.provider, attempt.environment, dispute.kind, dispute.status,\
    \ dispute.currency\
    \ ORDER BY attempt.environment, attempt.provider, dispute.currency,\
    \ dispute.kind, dispute.status"
    [] :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Text, Single Text
       , Single Int64, Single Int64 )]
  pure
    [ CommerceDisputeSummaryDTO provider environment kind status currency count amount
    | ( Single provider, Single environment, Single kind, Single status
      , Single currency, Single count, Single amount ) <- rows
    ]

loadReconciliationSummaries
  :: SqlPersistT IO [CommerceReconciliationSummaryDTO]
loadReconciliationSummaries = do
  rows <- rawSql
    "SELECT provider, environment, status, currency, COUNT(*)::bigint,\
    \ COALESCE(SUM(expected_amount_minor), 0)::bigint,\
    \ COALESCE(SUM(actual_amount_minor), 0)::bigint\
    \ FROM commerce_reconciliation_exception\
    \ GROUP BY provider, environment, status, currency\
    \ ORDER BY environment, provider, status, currency"
    [] :: SqlPersistT IO
      [(Single Text, Single Text, Single Text, Single (Maybe Text), Single Int64,
        Single Int64, Single Int64)]
  pure
    [ CommerceReconciliationSummaryDTO provider environment status currency
        count expected actual
    | ( Single provider, Single environment, Single status, Single currency
      , Single count, Single expected, Single actual
      ) <- rows
    ]

loadSettlementSummaries :: SqlPersistT IO [CommerceSettlementSummaryDTO]
loadSettlementSummaries = do
  rows <- rawSql
    "SELECT provider, environment, status, currency, COUNT(*)::bigint,\
    \ COALESCE(SUM(gross_minor), 0)::bigint, COALESCE(SUM(fee_minor), 0)::bigint,\
    \ COALESCE(SUM(withholding_minor), 0)::bigint,\
    \ COALESCE(SUM(refund_minor), 0)::bigint,\
    \ COALESCE(SUM(chargeback_minor), 0)::bigint, COALESCE(SUM(net_minor), 0)::bigint\
    \ FROM commerce_settlement GROUP BY provider, environment, status, currency\
    \ ORDER BY environment, provider, currency, status"
    [] :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Text, Single Int64
       , Single Int64, Single Int64, Single Int64, Single Int64, Single Int64
       , Single Int64
       )]
  pure
    [ CommerceSettlementSummaryDTO provider environment status currency count
        gross fee withholding refund chargeback net
    | ( Single provider, Single environment, Single status, Single currency
      , Single count, Single gross, Single fee, Single withholding, Single refund
      , Single chargeback, Single net
      ) <- rows
    ]

loadSellerBalanceSummaries
  :: UTCTime
  -> SqlPersistT IO [CommerceSellerBalanceSummaryDTO]
loadSellerBalanceSummaries generatedAt = do
  rows <- rawSql
    "SELECT account.provider, account.environment,\
    \ CASE WHEN entry.available_at IS NOT NULL AND entry.available_at <= ?\
    \      THEN 'available' ELSE 'pending' END,\
    \ entry.currency, COUNT(*)::bigint, COALESCE(SUM(entry.amount_minor), 0)::bigint\
    \ FROM commerce_seller_balance_entry entry\
    \ JOIN commerce_connected_account account ON account.id = entry.connected_account_id\
    \ GROUP BY account.provider, account.environment,\
    \ CASE WHEN entry.available_at IS NOT NULL AND entry.available_at <= ?\
    \      THEN 'available' ELSE 'pending' END, entry.currency\
    \ ORDER BY account.environment, account.provider, entry.currency, 3"
    [PersistUTCTime generatedAt, PersistUTCTime generatedAt] :: SqlPersistT IO
      [(Single Text, Single Text, Single Text, Single Text, Single Int64, Single Int64)]
  pure
    [ CommerceSellerBalanceSummaryDTO provider environment availability currency count amount
    | ( Single provider, Single environment, Single availability, Single currency
      , Single count, Single amount
      ) <- rows
    ]

loadPayoutSummaries :: SqlPersistT IO [CommercePayoutSummaryDTO]
loadPayoutSummaries = do
  rows <- rawSql
    "SELECT account.provider, account.environment, payout.status, payout.currency,\
    \ COUNT(*)::bigint, COALESCE(SUM(payout.amount_minor), 0)::bigint\
    \ FROM commerce_payout payout\
    \ JOIN commerce_connected_account account ON account.id = payout.connected_account_id\
    \ GROUP BY account.provider, account.environment, payout.status, payout.currency\
    \ ORDER BY account.environment, account.provider, payout.currency, payout.status"
    [] :: SqlPersistT IO
      [(Single Text, Single Text, Single Text, Single Text, Single Int64, Single Int64)]
  pure
    [ CommercePayoutSummaryDTO provider environment status currency count amount
    | ( Single provider, Single environment, Single status, Single currency
      , Single count, Single amount
      ) <- rows
    ]

listProviderEventsHandler
  :: Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> AppM [CommerceProviderEventDTO]
listProviderEventsHandler rawStatus rawLimit rawOffset = do
  Env{..} <- ask
  status <- either (throwError . badRequest) pure (validateProviderEventStatus rawStatus)
  let limit = min 100 (max 1 (maybe 50 id rawLimit))
      offset = min 10000 (max 0 (maybe 0 id rawOffset))
  records <- liftIO $ flip runSqlPool envPool $
    ProviderEvent.listProviderEvents status limit offset
  pure (map providerEventToDTO records)

replayProviderEventHandler
  :: AuthedUser
  -> Text
  -> CommerceProviderEventReplayCreate
  -> AppM CommerceProviderEventDTO
replayProviderEventHandler user rawEventId CommerceProviderEventReplayCreate{..} = do
  Env{..} <- ask
  eventRef <- either (throwError . badRequest) pure $
    ProviderEvent.parseProviderEventReference rawEventId
  reason <- either (throwError . badRequest) pure $
    validateProviderEventReplayReason cperReason
  now <- liftIO getCurrentTime
  result <- liftIO $ flip runSqlPool envPool $
    ProviderEvent.requeueDeadLetterProviderEvent
      eventRef (fromSqlKey (auPartyId user) :: Int64) reason now
  case result of
    Left ProviderEvent.ProviderEventNotFound ->
      throwError err404 { errBody = "Provider event not found" }
    Left (ProviderEvent.ProviderEventReplayConflict status) ->
      throwError err409
        { errBody = BL.fromStrict (TE.encodeUtf8
            ("Only dead-letter provider events can be replayed; current status is " <> status)) }
    Right record -> pure (providerEventToDTO record)

validateProviderEventReplayReason :: Text -> Either Text Text
validateProviderEventReplayReason rawReason =
  let reason = T.strip rawReason
  in if T.length reason < 8 || T.length reason > 500
       then Left "Replay reason must contain 8 to 500 characters"
       else if T.any isControl reason
         then Left "Replay reason contains unsupported control characters"
         else Right reason

validateProviderEventStatus :: Maybe Text -> Either Text (Maybe Text)
validateProviderEventStatus Nothing = Right Nothing
validateProviderEventStatus (Just rawStatus)
  | status `elem` allowedStatuses = Right (Just status)
  | otherwise = Left "Unsupported provider event status filter"
  where
    status = T.toLower (T.strip rawStatus)
    allowedStatuses =
      [ "pending", "processing", "processed", "retry", "dead_letter", "ignored" ]

providerEventToDTO :: ProviderEvent.ProviderEventRecord -> CommerceProviderEventDTO
providerEventToDTO ProviderEvent.ProviderEventRecord{..} = CommerceProviderEventDTO
  { cpeId = perId
  , cpeProvider = perProvider
  , cpeEnvironment = perEnvironment
  , cpeProviderEventId = perProviderEventId
  , cpeEventType = perEventType
  , cpeEvidenceType = perEvidenceType
  , cpeProviderResourceId = perProviderResourceId
  , cpeStatus = perStatus
  , cpeAttemptCount = perAttemptCount
  , cpeCheckoutId = perCheckoutId
  , cpePaymentAttemptId = perPaymentAttemptId
  , cpeRefundId = perRefundId
  , cpeReceivedAt = perReceivedAt
  , cpeProviderCreatedAt = perProviderCreatedAt
  , cpeProcessingStartedAt = perProcessingStartedAt
  , cpeLastAttemptAt = perLastAttemptAt
  , cpeNextAttemptAt = perNextAttemptAt
  , cpeProcessedAt = perProcessedAt
  , cpeErrorSummary = perErrorSummary
  }

badRequest :: Text -> ServerError
badRequest message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }
