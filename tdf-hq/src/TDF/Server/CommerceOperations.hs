{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.CommerceOperations
  ( commerceOperationsServer
  , validateProviderEventReplayReason
  ) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Data.Char (isControl)
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, getCurrentTime)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql
  ( Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool )
import           Servant

import           TDF.API.CommerceOperations
import           TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import qualified TDF.Commerce.ProviderEventStore as ProviderEvent
import           TDF.DB (Env(..))

type AppM = ReaderT Env Handler

commerceOperationsServer
  :: AuthedUser
  -> ServerT CommerceOperationsAPI AppM
commerceOperationsServer user =
       (requireAccess *> paymentOverviewHandler)
  :<|> (\status limit offset -> requireAccess *> listProviderEventsHandler status limit offset)
  :<|> (\eventId request -> requireAccess *> replayProviderEventHandler user eventId request)
  where
    requireAccess = unless (hasStrictAdminAccess user) $
      throwError err403 { errBody = "Strict Admin access required" }

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
