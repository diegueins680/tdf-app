{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- All endpoints inherit bearer authentication and additionally require strict
-- admin scope. Provider POST, provider readback and manual review are separate.
module TDF.Server.DomoRefunds (domoRefundAdminServer, domoRefundAdminServerWithManager) where

import Control.Exception.Safe (tryAny)
import Control.Monad (unless, when)
import Control.Monad.Except (MonadError, catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool)
import Network.HTTP.Client (Manager)
import Servant
import System.Environment (lookupEnv)

import TDF.Auth (AuthedUser(..), ModuleAccess(..), hasStrictAdminAccess, validateModuleAccess, withCurrentAuthorization)
import TDF.DB (Env(..))
import TDF.Routes.DomoRefunds
import qualified TDF.Commerce.CheckoutStore as C
import qualified TDF.Commerce.DomoRefundStore as D
import qualified TDF.Commerce.RefundStore as R
import qualified TDF.Commerce.RefundReconciliation as Recovery
import qualified TDF.Commerce.ProviderAdapter.PayPalRefund as Q
import qualified TDF.Commerce.ProviderAdapter.Http as Http
import TDF.Server.ProviderTransport

domoRefundAdminServer
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => AuthedUser -> ServerT DomoRefundAdminAPI m
domoRefundAdminServer = domoRefundAdminServerWithManager Http.sharedProviderManager

domoRefundAdminServerWithManager
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => Manager -> AuthedUser -> ServerT DomoRefundAdminAPI m
domoRefundAdminServerWithManager manager user rawQuoteId =
       (authorize >>= readStatus)
  :<|> (authorize >>= approve)
  :<|> (authorize >>= execute)
  :<|> (authorize >>= reconcile)
  :<|> (\evidence -> authorize >>= \quoteId -> manualEvidence quoteId evidence)
  :<|> (authorize >>= confirmManual)
  where
    actor = fromSqlKey (auPartyId user)
    authorize = do
      unless (hasStrictAdminAccess user) $ throwError err403
      either throwError pure (validateModuleAccess ModuleAdmin user)
      maybe (throwError err400 { errBody="quoteId must be a UUID" }) (pure . UUID.toText)
        (UUID.fromText (T.strip rawQuoteId))
    readStatus quoteId = dto <$> authorizedTransaction user (load quoteId)
    approve quoteId = do
      now <- liftIO getCurrentTime
      dto <$> authorizedTransaction user (D.approveDomoRefund quoteId actor now)
    execute quoteId = do
      initial <- authorizedTransaction user (load quoteId)
      -- Replays never get another POST permit and work while gates are off.
      if R.rrStatus initial `elem` ["processing","succeeded"] then pure (dto initial)
      else if R.rrProvider initial/="paypal" then do
        now <- liftIO getCurrentTime
        dto . fst <$> authorizedTransaction user (manualReady initial >> D.claimDomoRefund quoteId actor now)
      else do
        (cid, secret, baseUrl, environment, merchant) <- provider loadPaypalEnvForService
        ready <- queryConfigured initial
        unless (ready && C.checkoutEnvironmentText environment==R.rrEnvironment initial
          && merchant==R.rrMerchantRef initial) $ throwError err503
        capture <- authorizedTransaction user (D.loadDomoRefundCapture initial)
        let binding = Q.RefundQueryBinding environment (R.refundReferenceId (R.rrReference initial))
              capture merchant (R.rrAmountMinor initial) (R.rrCurrency initial)
        _ <- either (const (throwError err409)) pure (Q.validateRefundQueryBinding binding)
        -- OAuth is not a refund. Failure here keeps the approved request retryable.
        token <- provider (paypalAccessTokenForService manager cid secret baseUrl)
        now <- liftIO getCurrentTime
        (record, permitted) <- authorizedTransaction user $ do
          gate <- explicitFlag initial "checkout.paypal.refunds"
          capabilities <- Recovery.queryReady binding
          unless (gate && capabilities) $ fail "Verified refund execution capabilities unavailable"
          D.claimDomoRefund quoteId actor now
        when permitted $ do
          outcome <- provider (issuePaypalRefundWithTokenRemote manager token baseUrl capture record)
            `catchError` (\failure -> do
              authorizedTransaction user $ do
                R.recordRefundFailure (R.rrReference record) "domo_refund_transport_unknown" now
                D.refundAudit record actor "provider_outcome_unknown"
              throwError failure)
          -- Even COMPLETED in the POST response is only an identity to query.
          -- Exact capture, environment, merchant, amount and currency are checked
          -- by the shared authenticated GET reconciliation path.
          authorizedTransaction user $ do
            _ <- either (fail . T.unpack) pure =<<
              R.recordRefundPending (R.rrReference record) (proRefundId outcome) now
            R.recordRefundFailure (R.rrReference record) "domo_refund_awaiting_readback" now
            D.refundAudit record actor "provider_response_received"
        readStatus quoteId
    reconcile quoteId = do
      env <- ask
      record <- authorizedTransaction user (load quoteId)
      unless (R.rrProvider record=="paypal") $ throwError err409
        {errBody="Manual refunds require receipt evidence and independent confirmation"}
      let configured binding = do
            result <- runHandler (runReaderT (queryConfigured record) env)
            pure (result==Right True && R.rrEnvironment record==C.checkoutEnvironmentText (Q.rqbEnvironment binding)
              && R.rrMerchantRef record==Q.rqbMerchantId binding)
          query binding = do
            result <- runHandler (runReaderT (queryRemote manager binding) env)
            pure (either (const (Left Recovery.RefundRecoveryQueryFailed)) Right result)
      result <- liftIO $ Recovery.reconcileKnownRefundGuarded
        (fmap (either (const False) (const True)) (withCurrentAuthorization strictAdmin user (pure ())))
        (envPool env) (R.rrReference record)
        actor configured query
      case result of
        Right view -> pure (dto (Recovery.rrvRefund view))
        Left Recovery.RefundRecoveryRateLimited -> throwError err429
          {errBody="Refund query limit reached",errHeaders=[("Retry-After","10")]}
        Left _ -> throwError err409 {errBody="Refund remains reserved pending verified reconciliation"}
    manualEvidence quoteId ManualRefundEvidence{..} = do
      now <- liftIO getCurrentTime
      authorizedTransaction user $ do
        record <- load quoteId
        manualReady record
        D.recordManualEvidence quoteId actor now D.ManualEvidence
          { D.meExternalReference=externalReference,D.meEvidenceReference=evidenceReference
          , D.meAmountMinor=amountMinor,D.meCurrency=currency,D.meOccurredAt=occurredAt }
      readStatus quoteId
    confirmManual quoteId = do
      now <- liftIO getCurrentTime
      dto <$> authorizedTransaction user (do
        record <- load quoteId
        manualReady record
        D.confirmManualEvidence quoteId actor now)

load :: Text -> SqlPersistT IO R.RefundRecord
load quoteId = either (fail . T.unpack) pure =<< D.loadDomoRefund quoteId

explicitFlag :: R.RefundRecord -> Text -> SqlPersistT IO Bool
explicitFlag record key = do
  rows <- rawSql
    "SELECT enabled FROM revenue_feature_flag WHERE flag_key=? AND environment=? FOR SHARE"
    [PersistText key,PersistText (R.rrEnvironment record)] :: SqlPersistT IO [Single Bool]
  pure (rows==[Single True])

manualReady :: R.RefundRecord -> SqlPersistT IO ()
manualReady record = do
  enabled <- explicitFlag record "domo.refund_manual"
  unless (enabled && R.rrProvider record/="paypal") $ fail "Manual refund workflow is unavailable"

queryConfigured
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => R.RefundRecord -> m Bool
queryConfigured record = do
  enabled <- liftIO (lookupEnv "PAYPAL_REFUND_RECONCILIATION_ENABLED")
  if enabled/=Just "true" then pure False else
    (do
      (_,_,_,environment,merchant) <- provider loadPaypalEnvForService
      pure (C.checkoutEnvironmentText environment==R.rrEnvironment record && merchant==R.rrMerchantRef record))
    `catchError` const (pure False)

queryRemote :: Manager -> Q.RefundQueryBinding -> ReaderT Env Handler Q.RefundQueryOutcome
queryRemote manager binding = do
  (cid, secret, baseUrl, environment, merchant) <- loadPaypalEnvForService
  enabled <- liftIO (lookupEnv "PAYPAL_REFUND_RECONCILIATION_ENABLED")
  unless (enabled==Just "true" && environment==Q.rqbEnvironment binding && merchant==Q.rqbMerchantId binding) $
    throwError err503
  token <- paypalAccessTokenForService manager cid secret baseUrl
  request <- either (const (throwError err409)) pure (Q.buildRefundQuery token binding)
  response <- liftIO (Http.executeAdapterRequest manager request)
  value <- either (const (throwError err502)) pure response
  either (const (throwError err502)) pure (Q.parseRefundQuery binding value)

provider :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => ReaderT Env Handler a -> m a
provider action = do
  env <- ask
  result <- liftIO (runHandler (runReaderT action env))
  either throwError pure result

strictAdmin :: AuthedUser -> Either ServerError ()
strictAdmin current = do
  unless (hasStrictAdminAccess current) (Left err403)
  validateModuleAccess ModuleAdmin current

authorizedTransaction
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => AuthedUser -> SqlPersistT IO a -> m a
authorizedTransaction user action = do
  result <- transaction (withCurrentAuthorization strictAdmin user action)
  either throwError pure result

transaction :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => SqlPersistT IO a -> m a
transaction action = do
  env <- ask
  result <- liftIO $ tryAny $ runSqlPool action (envPool env)
  either (const (throwError err409 {errBody="Refund state or evidence does not permit this operation"})) pure result

dto :: R.RefundRecord -> Value
dto record = object
  [ "refundId" .= R.refundReferenceId (R.rrReference record),"status" .= R.rrStatus record
  , "provider" .= R.rrProvider record,"environment" .= R.rrEnvironment record
  , "amountMinor" .= T.pack (show (R.rrAmountMinor record)),"currency" .= R.rrCurrency record
  , "requestedBy" .= R.rrRequestedBy record,"approvedBy" .= R.rrApprovedBy record
  , "completedAt" .= R.rrCompletedAt record
  , "executionMode" .= (if R.rrProvider record=="paypal" then "automatic" else "manual" :: Text)
  ]
