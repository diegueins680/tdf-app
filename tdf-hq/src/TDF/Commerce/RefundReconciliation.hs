{-# LANGUAGE OverloadedStrings #-}

-- | An operator-triggered query is not another refund execution permit.
-- Persist admission before HTTP, share the provider query quota, recheck authority
-- after HTTP, and apply only exact positive completion through RefundStore.
module TDF.Commerce.RefundReconciliation
  ( RefundRecoveryError(..)
  , RefundRecoveryView(..)
  , readRefundRecovery
  , reconcileKnownRefund
  ) where

import Control.Exception.Safe (tryAny)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.ProviderExecutionStore as Execution
import TDF.Commerce.ProviderAdapter.PayPalRefund
import qualified TDF.Commerce.RefundStore as Refund

data RefundRecoveryError
  = RefundRecoveryNotFound | RefundRecoveryUnavailable | RefundRecoveryConflict
  | RefundRecoveryRateLimited | RefundRecoveryQueryFailed
  deriving (Eq, Show)

data RefundRecoveryView = RefundRecoveryView
  { rrvRefund :: Refund.RefundRecord
  , rrvCanQuery :: Bool
  , rrvOutcome :: Text
  , rrvCheckedAt :: Maybe UTCTime
  }

instance Show RefundRecoveryView where
  show _ = "RefundRecoveryView(<redacted>)"

type Target = (Refund.RefundRecord, RefundQueryBinding)

-- Configuration returns only readiness, never credentials. The HTTP callback
-- must use the shared bounded transport and validate the complete binding.
readRefundRecovery
  :: ConnectionPool -> Refund.RefundReference -> (RefundQueryBinding -> IO Bool)
  -> IO (Either RefundRecoveryError RefundRecoveryView)
readRefundRecovery pool ref configured = safely $ do
  target <- runSqlPool (loadTarget ref) pool
  case target of
    Left problem -> pure (Left problem)
    Right (record, binding) -> do
      configReady <- configured binding
      ready <- runSqlPool (queryReady binding) pool
      pure (Right (RefundRecoveryView record
        (Refund.rrStatus record == "processing" && configReady && ready) "not_queried" Nothing))

reconcileKnownRefund
  :: ConnectionPool -> Refund.RefundReference -> Int64
  -> (RefundQueryBinding -> IO Bool)
  -> (RefundQueryBinding -> IO (Either RefundRecoveryError RefundQueryOutcome))
  -> IO (Either RefundRecoveryError RefundRecoveryView)
reconcileKnownRefund pool ref actor configured query
  | actor <= 0 = pure (Left RefundRecoveryUnavailable)
  | otherwise = safely $ do
      initial <- runSqlPool (loadTarget ref) pool
      case initial of
        Left problem -> pure (Left problem)
        Right (_, binding) -> do
          ready <- configured binding
          if not ready then pure (Left RefundRecoveryUnavailable) else do
            correlation <- ("refund-query:" <>) . toText <$> nextRandom
            prepared <- runSqlPool (prepare binding correlation) pool
            case prepared of
              Left problem -> pure (Left problem)
              Right target -> do
                response <- tryAny (query binding)
                stillConfigured <- configured binding
                now <- getCurrentTime
                runSqlPool (apply target correlation stillConfigured now
                  (either (const (Left RefundRecoveryQueryFailed)) id response)) pool
  where
    prepare binding correlation = do
      lockRefund ref
      target <- loadTarget ref
      case target of
        Left problem -> pure (Left problem)
        Right current@(record, currentBinding)
          | binding /= currentBinding || Refund.rrStatus record /= "processing" ->
              pure (Left RefundRecoveryConflict)
          | otherwise -> do
              ready <- queryReady binding
              if not ready then pure (Left RefundRecoveryUnavailable) else do
                quota <- Execution.reserveProviderQueryBudget
                  Checkout.ProviderPayPal (rqbEnvironment binding)
                if not quota then pure (Left RefundRecoveryRateLimited) else do
                  audit record actor correlation "requested"
                  pure (Right current)
    apply (_, binding) correlation configReady now response = do
      lockRefund ref
      current <- loadTarget ref
      case current of
        Left problem -> pure (Left problem)
        Right (record, currentBinding) -> do
          ready <- queryReady binding
          if not configReady || not ready || currentBinding /= binding
            then audit record actor correlation "authority_changed"
              >> pure (Left RefundRecoveryUnavailable)
            else case response of
              Left _ -> audit record actor correlation "query_failed"
                >> pure (Left RefundRecoveryQueryFailed)
              Right outcome
                | Refund.rrStatus record == "succeeded" -> do
                    audit record actor correlation "already_completed"
                    pure (Right (RefundRecoveryView record False "already_completed" (Just now)))
                | Refund.rrStatus record /= "processing" ->
                    audit record actor correlation "state_conflict"
                      >> pure (Left RefundRecoveryConflict)
                | outcome == RefundQueryHeld -> do
                    audit record actor correlation "held"
                    pure (Right (RefundRecoveryView record True "held" (Just now)))
                | otherwise -> do
                    completed <- Refund.recordVerifiedRefund Refund.VerifiedRefund
                      { Refund.vrRefund = ref, Refund.vrProviderRefund = rqbRefundId binding
                      , Refund.vrAmountMinor = rqbAmountMinor binding
                      , Refund.vrCurrency = rqbCurrency binding, Refund.vrOccurredAt = now
                      , Refund.vrCorrelationId = correlation
                      }
                    case completed of
                      Left _ -> audit record actor correlation "accounting_conflict"
                        >> pure (Left RefundRecoveryConflict)
                      Right _ -> do
                        audit record actor correlation "completed"
                        updated <- Refund.loadRefund ref
                        -- The locked row cannot vanish. Do not acknowledge a
                        -- completion without a fresh read of committed-to-be state.
                        case updated of
                          Just value -> pure (Right
                            (RefundRecoveryView value False "completed" (Just now)))
                          Nothing -> fail "Refund completion could not be reloaded"

loadTarget :: Refund.RefundReference -> SqlPersistT IO (Either RefundRecoveryError Target)
loadTarget ref = do
  current <- Refund.loadRefund ref
  case current of
    Nothing -> pure (Left RefundRecoveryNotFound)
    Just record -> do
      captures <- rawSql
        "SELECT binding.provider_resource_id FROM commerce_provider_binding binding\
        \ JOIN commerce_payment_attempt attempt ON attempt.id=binding.payment_attempt_id\
        \ JOIN commerce_checkout_session checkout ON checkout.id=attempt.checkout_id\
        \ WHERE checkout.id=?::uuid AND attempt.id=?::uuid\
        \ AND checkout.domain_type='mixing_mastering' AND attempt.status='succeeded'\
        \ AND binding.resource_type='capture' AND binding.provider='paypal'\
        \ AND binding.provider=attempt.provider AND binding.environment=attempt.environment\
        \ AND binding.merchant_account_ref=attempt.merchant_account_ref\
        \ AND binding.checkout_id=checkout.id AND binding.amount_minor=attempt.amount_minor\
        \ AND binding.currency=attempt.currency AND checkout.currency=attempt.currency\
        \ AND checkout.environment=attempt.environment\
        \ AND attempt.provider=? AND attempt.environment=? AND attempt.merchant_account_ref=?\
        \ AND attempt.currency=?"
        [ PersistText (Checkout.checkoutReferenceId (Refund.rrCheckout record))
        , PersistText (Checkout.paymentAttemptReferenceId (Refund.rrPaymentAttempt record))
        , PersistText (Refund.rrProvider record), PersistText (Refund.rrEnvironment record)
        , PersistText (Refund.rrMerchantRef record), PersistText (Refund.rrCurrency record)
        ] :: SqlPersistT IO [Single Text]
      pure $ case (captures, Refund.rrProviderRefundId record, Refund.rrEnvironment record) of
        ([Single capture], Just refundId, environment)
          | Refund.rrProvider record == "paypal"
          , Just approved <- Refund.rrApprovedBy record
          , approved > 0 && approved /= Refund.rrRequestedBy record
          , Just parsed <- parseEnvironment environment
          , let binding = RefundQueryBinding parsed refundId capture
                  (Refund.rrMerchantRef record) (Refund.rrAmountMinor record) (Refund.rrCurrency record)
          , Right () <- validateRefundQueryBinding binding -> Right (record, binding)
        _ -> Left RefundRecoveryConflict

queryReady :: RefundQueryBinding -> SqlPersistT IO Bool
queryReady binding = do
  rows <- rawSql
    "SELECT account.id::text FROM commerce_provider_account account\
    \ JOIN revenue_feature_flag flag ON flag.environment=account.environment\
    \ AND flag.flag_key='checkout.paypal.refund_reconciliation'\
    \ WHERE account.provider='paypal' AND account.environment=?\
    \ AND account.merchant_account_ref=? AND flag.enabled AND account.enabled\
    \ AND account.status='ready' AND account.contract_status='approved'\
    \ AND account.credential_status='validated' AND account.settlement_currency='USD'\
    \ FOR SHARE OF account,flag"
    [ PersistText (Checkout.checkoutEnvironmentText (rqbEnvironment binding))
    , PersistText (rqbMerchantId binding)
    ] :: SqlPersistT IO [Single Text]
  case rows of
    [Single accountId] -> do
      -- Lock the actual evidence rows, not an aggregate subquery: an operator
      -- cannot revoke one capability between this check and financial commit.
      capabilities <- rawSql
        "SELECT capability FROM commerce_provider_capability\
        \ WHERE provider_account_id=?::uuid AND payment_method='paypal_wallet'\
        \ AND capability IN ('server_verification','full_refund','partial_refund')\
        \ AND verification_status=? ORDER BY capability FOR SHARE"
        [ PersistText accountId
        , PersistText (case rqbEnvironment binding of
            Checkout.CheckoutSandbox -> "sandbox_verified"
            Checkout.CheckoutProduction -> "production_verified")
        ] :: SqlPersistT IO [Single Text]
      pure (capabilities == map Single ["full_refund", "partial_refund", "server_verification"])
    _ -> pure False

lockRefund :: Refund.RefundReference -> SqlPersistT IO ()
lockRefund ref = do
  _ <- rawSql "SELECT id::text FROM commerce_refund WHERE id=?::uuid FOR UPDATE"
    [PersistText (Refund.refundReferenceId ref)] :: SqlPersistT IO [Single Text]
  pure ()

audit :: Refund.RefundRecord -> Int64 -> Text -> Text -> SqlPersistT IO ()
audit record actor correlation outcome = rawExecute
  "INSERT INTO commerce_checkout_audit_event\
  \ (checkout_id,event_type,actor_type,actor_id,correlation_id,metadata)\
  \ VALUES (?::uuid,'refund_query','admin',?,?,\
  \ jsonb_build_object('refund_id',?::text,'outcome',?::text))"
  [ PersistText (Checkout.checkoutReferenceId (Refund.rrCheckout record))
  , PersistText (T.pack (show actor)), PersistText correlation
  , PersistText (Refund.refundReferenceId (Refund.rrReference record)), PersistText outcome
  ]

parseEnvironment :: Text -> Maybe Checkout.CheckoutEnvironment
parseEnvironment "sandbox" = Just Checkout.CheckoutSandbox
parseEnvironment "production" = Just Checkout.CheckoutProduction
parseEnvironment _ = Nothing

safely :: IO (Either RefundRecoveryError a) -> IO (Either RefundRecoveryError a)
safely action = either (const (Left RefundRecoveryUnavailable)) id <$> tryAny action
