{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Recover hosted debit sales from saved checkouts after a process or database
-- restore. Only the authenticated status GET crosses the provider boundary.
module TDF.Commerce.DatafastRecovery (recoverOne) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.URI (urlEncode)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool, toSqlKey)
import System.Environment (lookupEnv)

import qualified TDF.Commerce.CheckoutStore as C
import TDF.Commerce.ProviderAdapter
import TDF.Commerce.ProviderAdapter.Http (AdapterTransportError)
import qualified TDF.Commerce.ProviderExecutionStore as Store
import TDF.DB (Env(..))
import qualified TDF.Commerce.DatafastStatus as DF
import qualified TDF.Ticketing.Issuance as Tickets

data Binding = Binding
  { bindingId, attemptId, checkoutId, domain, orderRef, resource :: Text
  , storedPath :: Maybe Text
  , amount :: Int64
  , currency :: Text
  } deriving (Eq)

type Fetch = AdapterRequest -> IO (Either AdapterTransportError A.Value)

enabled :: IO Bool
enabled = do
  worker <- lookupEnv "COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED"
  datafast <- lookupEnv "DATAFAST_RESTORE_RECOVERY_ENABLED"
  pure (worker == Just "true" && datafast == Just "true")

recoverOne :: Fetch -> Env -> C.CheckoutEnvironment -> IO Int
recoverOne fetch Env{envPool} environment = do
  active <- enabled
  if not active then pure 0 else do
    configured <- runExceptT DF.readDatafastEnvironment
    case configured of
      Right config | DF.sdfEnvironment config == environment -> do
        let merchant = DF.sdfEntityId config
        claimed <- runSqlPool (claim environment merchant) envPool
        case claimed of
          Nothing -> pure 0
          Just (binding, generation) -> do
            let basePath = "/v1/checkouts/" <> resource binding
                path = basePath <> "/payment"
                expectedPath = storedPath binding
                pathOk = expectedPath == Nothing || expectedPath == Just basePath || expectedPath == Just path
            answer <- if not pathOk || DF.validateDatafastOrderResourcePath (Just (resource binding)) path /= Right path
              then pure (Review "invalid_stored_path")
              else query fetch config binding generation
            stillEnabled <- enabled
            if not stillEnabled then pure 1 else do
              runSqlPool (finish environment merchant binding generation path answer) envPool
              pure 1
      _ -> pure 0

-- Historical Dataweb creates have a checkout binding but no generic
-- commerce_provider_operation. Match the binding, attempt and checkout again
-- after the network call, including the original merchant and domain identity.
source :: Text
source =
  " FROM commerce_provider_binding b JOIN commerce_payment_attempt a ON a.id=b.payment_attempt_id\
  \ JOIN commerce_checkout_session c ON c.id=a.checkout_id\
  \ LEFT JOIN commerce_datafast_recovery_query q ON q.binding_id=b.id\
  \ WHERE b.provider='datafast' AND b.resource_type='checkout' AND b.environment=?\
  \ AND b.merchant_account_ref=? AND a.operation='create' AND a.provider=b.provider\
  \ AND a.environment=b.environment AND c.environment=a.environment\
  \ AND a.merchant_account_ref=b.merchant_account_ref AND b.merchant_reference=c.domain_order_id\
  \ AND b.amount_minor=a.amount_minor AND a.amount_minor=c.total_minor\
  \ AND b.currency=a.currency AND a.currency=c.currency"

bindings :: C.CheckoutEnvironment -> Text -> Text -> [PersistValue] -> SqlPersistT IO [Binding]
bindings environment merchant predicate params = do
  rows <- rawSql ("SELECT b.id::text,a.id::text,c.id::text,c.domain_type,c.domain_order_id,\
    \ b.provider_resource_id,b.provider_resource_path,c.total_minor,c.currency" <> source <> predicate)
    ([PersistText (C.checkoutEnvironmentText environment), PersistText merchant] <> params)
  pure [Binding b a c d o r p n u | (Single b,Single a,Single c,Single d,Single o,
                                      Single r,Single p,Single n,Single u) <- rows]

claim :: C.CheckoutEnvironment -> Text -> SqlPersistT IO (Maybe (Binding, Int))
claim environment merchant = do
  installed <- rawSql "SELECT to_regclass('commerce_datafast_recovery_query') IS NOT NULL" []
  ready <- Store.queryRecoveryReady C.ProviderDatafast environment merchant
  if installed /= [Single True] || not ready then pure Nothing else do
    candidates <- bindings environment merchant
      " AND a.status IN ('requires_customer_action','processing','failed')\
      \ AND (q.binding_id IS NULL OR (q.state='pending' AND q.next_query_at<=clock_timestamp()))\
      \ ORDER BY COALESCE(q.next_query_at,b.created_at),b.id LIMIT 1 FOR UPDATE OF b SKIP LOCKED" []
    case candidates of
      [binding] -> do
        budget <- Store.reserveProviderQueryBudget C.ProviderDatafast environment
        if not budget then pure Nothing else do
          rawExecute "UPDATE commerce_provider_query_budget SET next_query_at=clock_timestamp()+interval '31 seconds' WHERE provider='datafast' AND environment=?" [PersistText (C.checkoutEnvironmentText environment)]
          let key = [PersistText (bindingId binding)]
          generations <- rawSql
            "INSERT INTO commerce_datafast_recovery_query(binding_id,attempts,next_query_at,state,outcome)\
            \ VALUES (?::uuid,1,clock_timestamp()+interval '2 minutes','pending','claimed')\
            \ ON CONFLICT(binding_id) DO UPDATE SET attempts=commerce_datafast_recovery_query.attempts+1,\
            \ next_query_at=excluded.next_query_at,outcome='claimed' WHERE commerce_datafast_recovery_query.state='pending'\
            \ AND commerce_datafast_recovery_query.attempts<24 AND commerce_datafast_recovery_query.next_query_at<=clock_timestamp()\
            \ RETURNING attempts" key
          case generations of
            [Single generation] -> pure (Just (binding,generation))
            _ -> do
              rawExecute "UPDATE commerce_datafast_recovery_query SET state='review',outcome='exhausted'\
                \ WHERE binding_id=?::uuid AND state='pending' AND attempts=24 AND next_query_at<=clock_timestamp()" key
              pure Nothing
      _ -> pure Nothing

data Answer = Retry | Review Text | Verified Text

-- Transaction reporting remains available after a checkout-status GET has
-- consumed the short-lived identifier. Never take the first of multiple rows:
-- reversals/refunds can be listed alongside the original debit.
query :: Fetch -> DF.ServiceDatafastEnv -> Binding -> Int -> IO Answer
query fetch config binding generation = do
  let reference = if odd generation then attemptId binding else orderRef binding
      url = T.dropWhileEnd (== '/') (T.pack (DF.sdfBaseUrl config)) <> "/v1/query?entityId="
        <> DF.sdfEntityId config <> "&merchantTransactionId=" <> TE.decodeUtf8 (urlEncode True (TE.encodeUtf8 reference))
      request = AdapterRequest C.ProviderDatafast AdapterQuery AdapterGet url
        [("Authorization",sensitiveText ("Bearer " <> DF.sdfBearerToken config))] Nothing NeverRetryAutomatically
      safeRef ch = ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z'
        || ch >= '0' && ch <= '9' || ch `elem` ("-_.:" :: String)
  if T.null reference || T.length reference > 255 || not (T.all safeRef reference)
    then pure (Review "invalid_merchant_reference")
    else do
      result <- fetch request
      pure $ case result of
        Left _ -> Retry
        Right (A.Array records) -> assess reference (toList records)
        Right (A.Object envelope)
          | Just (A.Array records) <- KM.lookup "payments" envelope
          , Just (A.Object outcome) <- KM.lookup "result" envelope
          , KM.lookup "code" outcome == Just (A.String "000.000.100") -> assess reference (toList records)
        _ -> Retry
  where
    assess _ [] = Retry
    assess reference [value@(A.Object fields)] = case A.fromJSON value of
      A.Success status
        | DF.sdfpsMerchantTransactionId status /= Just reference -> Review "reference_mismatch"
        | KM.lookup "paymentType" fields /= Just (A.String "DB") -> Review "unexpected_payment_type"
        | not (DF.isDatafastPaymentSuccess (DF.sdfEnvironment config) (DF.sdfpsResultCode status)) -> Retry
        | Just checkout <- KM.lookup "checkoutId" fields, checkout /= A.String (resource binding) -> Review "checkout_mismatch"
        | reference /= attemptId binding && KM.lookup "checkoutId" fields /= Just (A.String (resource binding)) ->
            Review "legacy_checkout_unproven"
        | otherwise -> case (DF.validateDatafastSuccessfulPayment reference (fromIntegral (amount binding))
                                (currency binding) status, DF.sdfpsPaymentId status) of
            (Right (), Just paymentId) | not (T.null paymentId), T.strip paymentId == paymentId ->
              Verified paymentId
            _ -> Review "status_mismatch"
      _ -> Review "invalid_status"
    assess _ _ = Review "ambiguous_report"

finish :: C.CheckoutEnvironment -> Text -> Binding -> Int -> Text -> Answer -> SqlPersistT IO ()
finish environment merchant binding generation path answer = do
  let key = [PersistText (bindingId binding),PersistInt64 (fromIntegral generation)]
      update :: Text -> Text -> SqlPersistT IO ()
      update state outcome = rawExecute
        "UPDATE commerce_datafast_recovery_query SET state=?,outcome=?,\
        \ next_query_at=clock_timestamp()+least(3600,30*(2^least(attempts,7))) * interval '1 second'\
        \ WHERE binding_id=?::uuid AND attempts=?" ([PersistText state,PersistText outcome] <> key)
  current <- bindings environment merchant " AND b.id=?::uuid FOR UPDATE OF b,a,c" [PersistText (bindingId binding)]
  locks <- rawSql "SELECT binding_id::text FROM commerce_datafast_recovery_query WHERE binding_id=?::uuid\
    \ AND attempts=? AND state='pending' AND next_query_at>clock_timestamp() FOR UPDATE" key
  ready <- Store.queryRecoveryReady C.ProviderDatafast environment merchant
  if locks /= [Single (bindingId binding)] || not ready then pure ()
  else if current /= [binding] then update "review" "binding_changed"
  else case answer of
    Retry | generation < 24 -> update "pending" "unconfirmed"
          | otherwise -> update "review" "exhausted"
    Review reason -> update "review" reason
    Verified paymentId -> do
      rawExecute "SAVEPOINT datafast_restored_payment" []
      result <- settle environment merchant binding path paymentId
      case result of
        Left reason -> do
          rawExecute "ROLLBACK TO SAVEPOINT datafast_restored_payment" []
          now <- liftIO getCurrentTime
          C.recordReconciliationException C.ProviderDatafast environment merchant reason
            (orderRef binding) paymentId (amount binding) (Just (amount binding)) (currency binding) now
          update "review" reason
        Right () -> update "completed" "verified"
      rawExecute "RELEASE SAVEPOINT datafast_restored_payment" []

-- Lock the actual domain hold. Booking, course and Domo sync through their
-- installed checkout triggers. Ticket issuance also needs the canonical issuer.
settle :: C.CheckoutEnvironment -> Text -> Binding -> Text -> Text -> SqlPersistT IO (Either Text ())
settle environment merchant binding@Binding{..} path paymentId = do
  admitted <- lockFulfillment binding
  case admitted of
    Nothing -> pure (Left "verified_datafast_domain_review")
    Just issue -> do
      now <- liftIO getCurrentTime
      let attempt = C.PaymentAttemptReference attemptId
          checkout = C.CheckoutReference checkoutId
          correlation = "datafast-recovery:" <> bindingId
      bound <- C.bindProviderResource C.ProviderBindingCreation
        { C.pbcAttempt=attempt, C.pbcCheckout=checkout, C.pbcProvider=C.ProviderDatafast
        , C.pbcEnvironment=environment, C.pbcMerchantRef=merchant, C.pbcResourceType="payment"
        , C.pbcProviderResource=paymentId, C.pbcResourcePath=Just path, C.pbcOrderReference=orderRef
        , C.pbcAmountMinor=amount, C.pbcCurrency=currency, C.pbcStage=C.AttemptProcessing
        , C.pbcOccurredAt=now, C.pbcCorrelationId=correlation }
      case bound of
        Left _ -> pure (Left "datafast_payment_binding_conflict")
        Right () -> do
          verified <- C.recordVerifiedPaymentOrReconcile C.VerifiedPayment
            { C.vpAttempt=attempt, C.vpCheckout=checkout, C.vpProvider=C.ProviderDatafast
            , C.vpEnvironment=environment, C.vpMerchantRef=merchant, C.vpResourceType="checkout"
            , C.vpProviderResource=resource, C.vpProviderResourcePath=storedPath
            , C.vpOrderReference=orderRef, C.vpProviderReference=orderRef, C.vpAmountMinor=amount
            , C.vpCurrency=currency, C.vpEvidence="server_to_server", C.vpOccurredAt=now
            , C.vpCorrelationId=correlation }
          case verified of
            Left _ -> pure (Left "datafast_payment_requires_review")
            Right _ -> do
              case issue of
                Just orderKey -> void (Tickets.finalizePaidTicketOrder now (toSqlKey orderKey))
                Nothing -> pure ()
              pure (Right ())

-- The SQL fragments are local constants. The lock and the DB clock prevent
-- a late provider confirmation from granting an expired reservation.
lockFulfillment :: Binding -> SqlPersistT IO (Maybe (Maybe Int64))
lockFulfillment Binding{..} = case domain of
  "event_ticket_order" -> do
    orders <- eligible "event_ticket_checkout_runtime" "r.order_id::text" "checkout_total_minor"
      "r.fulfillment_status='seat_held' AND greatest(r.hold_expires_at,r.manual_hold_expires_at)>clock_timestamp()" "r.order_id"
    pure $ case orders of [Single ticket] -> Just (Just ticket); _ -> Nothing
  "service_booking" -> admit "service_booking_checkout_runtime" "r.booking_id::text" "deposit_minor" "r.fulfillment_status='on_hold'"
  "course_registration" -> admit "course_registration_checkout_runtime" "r.registration_id::text" "due_now_minor" "r.enrollment_status='seat_held'"
  "domo_event_quote" -> admit "domo_event_quote_runtime" "('domo-quote:'||r.id::text)" "deposit_minor" "r.quote_status='deposit_due' AND r.fulfillment_status='date_held'"
  _ -> pure Nothing
  where
    admit table identity total predicate = do
      found <- eligible table identity total (predicate <> " AND r.hold_expires_at>clock_timestamp()") "1::bigint"
      pure $ if found == [Single 1] then Just Nothing else Nothing
    eligible :: Text -> Text -> Text -> Text -> Text -> SqlPersistT IO [Single Int64]
    eligible table identity total predicate projection = rawSql
      ("SELECT " <> projection <> " FROM " <> table <> " r JOIN commerce_checkout_session c ON c.id=r.checkout_id\
       \ WHERE c.id=?::uuid AND c.domain_type=? AND " <> identity <> "=c.domain_order_id AND r."
       <> total <> "=c.total_minor AND r.currency=c.currency AND c.paid_minor=0 AND c.refunded_minor=0\
       \ AND c.status IN ('awaiting_payment','processing','failed') AND " <> predicate <> " FOR UPDATE OF r,c")
      [PersistText checkoutId,PersistText domain]
