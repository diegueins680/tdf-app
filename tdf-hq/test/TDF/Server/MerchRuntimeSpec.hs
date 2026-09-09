{-# LANGUAGE OverloadedStrings #-}

module TDF.Server.MerchRuntimeSpec (spec) where

import           Control.Monad (unless)
import           Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS8
import           Data.Int (Int64)
import           Data.Set (empty)
import           Data.Text (Text)
import qualified Data.UUID as UUID
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool, toSqlKey)
import           Servant (ServerError(..), (:<|>)(..))
import           Servant.Server (runHandler)
import           System.Environment (lookupEnv)
import           Test.Hspec (Spec, describe, it, runIO)

import           TDF.API.Merch (MerchCancellationRequest(..), MerchIssueTriageRequest(..))
import           TDF.Auth (AuthedUser(..), modulesForRoles)
import           TDF.DB (Env(..), makePool)
import           TDF.Models (RoleEnum(..))
import           TDF.Server.Merch (merchProtectedServer, merchPublicServer)

assert :: Bool -> String -> IO ()
assert condition message = unless condition (fail message)

requiredUuid :: Text -> UUID.UUID
requiredUuid value = maybe (error "Invalid synthetic UUID") id (UUID.fromText value)

spec :: Spec
spec = do
  databaseUrl <- runIO (lookupEnv "TDF_MERCH_RUNTIME_DATABASE_URL")
  case databaseUrl of
    Nothing -> pure ()
    Just value -> describe "artist-merch-runtime PostgreSQL handlers" $
      it "enforces private capabilities, cancellation idempotency, finance redaction, seller isolation, and issue triage" $
        runChecks value

runChecks :: String -> IO ()
runChecks databaseUrl = do
  pool <- makePool (BS8.pack databaseUrl)
  let env = Env { envPool = pool, envConfig = error "Merch runtime test does not read AppConfig" }
      storeId = requiredUuid "92000000-0000-4000-8000-000000000001"
      orderId = requiredUuid "98000000-0000-4000-8000-000000000004"
      shippingIssueId = requiredUuid "94000000-0000-4000-8000-000000000001"
      refundIssueId = requiredUuid "94000000-0000-4000-8000-000000000002"
      owner = AuthedUser (toSqlKey 900002) [] empty
      collaborator = AuthedUser (toSqlKey 900003) [] empty
      otherSeller = AuthedUser (toSqlKey 900004) [] empty
      strictAdmin = AuthedUser (toSqlKey 900005) [Admin] (modulesForRoles [Admin])
      _capabilities
        :<|> _storefronts
        :<|> _storefront
        :<|> _product
        :<|> _createCart
        :<|> _getCart
        :<|> _putCartItem
        :<|> _deleteCartItem
        :<|> _checkout
        :<|> getOrder
        :<|> _createIssue
        :<|> cancelOrder = merchPublicServer
      cancellation = MerchCancellationRequest "Synthetic buyer no longer wants this unpaid order"

  hidden <- runHandler (runReaderT (getOrder orderId (Just "wrong-private-token")) env)
  case hidden of
    Left err -> assert (errHTTPCode err == 404) "Wrong order capability did not produce a non-enumerating 404"
    Right _ -> fail "Wrong order capability exposed a private order"

  cancelled <- runHandler (runReaderT (cancelOrder orderId (Just "runtime-order-token") (Just "runtime-cancel-key-001") cancellation) env)
  case cancelled of
    Left err -> fail ("Valid unpaid cancellation failed with HTTP " <> show (errHTTPCode err))
    Right _ -> pure ()

  retried <- runHandler (runReaderT (cancelOrder orderId (Just "runtime-order-token") (Just "runtime-cancel-key-001") cancellation) env)
  case retried of
    Left err -> fail ("Idempotent cancellation retry failed with HTTP " <> show (errHTTPCode err))
    Right _ -> pure ()

  conflict <- runHandler (runReaderT (cancelOrder orderId (Just "runtime-order-token") (Just "runtime-cancel-key-001") (MerchCancellationRequest "A different cancellation reason for the same key")) env)
  case conflict of
    Left err -> assert (errHTTPCode err == 409) "Conflicting cancellation retry did not return 409"
    Right _ -> fail "Conflicting cancellation retry was accepted"

  let runOrders user =
        let _a :<|> _b :<|> _c :<|> _d :<|> _e :<|> _f :<|> _g :<|> _h
              :<|> _i :<|> _j :<|> _k :<|> _l :<|> _m :<|> _n :<|> _o
              :<|> _p :<|> scopedOrders :<|> _rest = merchProtectedServer user
        in runHandler (runReaderT (scopedOrders storeId Nothing) env)
  ownerResult <- runOrders owner
  collaboratorResult <- runOrders collaborator
  outsiderResult <- runOrders otherSeller
  ownerRows <- either (fail . show) pure ownerResult
  collaboratorRows <- either (fail . show) pure collaboratorResult
  assert (not (null ownerRows) && not (null collaboratorRows)) "Authorized seller order handlers returned no scoped rows"
  case outsiderResult of
    Left err -> assert (errHTTPCode err == 403) "Cross-seller order access did not return 403"
    Right _ -> fail "A seller read another seller's orders"
  let collaboratorLeaksFinance (Aeson.Object value) =
        KeyMap.member (AesonKey.fromText "sellerNetMinor") value
          || KeyMap.member (AesonKey.fromText "tdfCommissionMinor") value
      collaboratorLeaksFinance _ = True
  assert (not (any collaboratorLeaksFinance collaboratorRows)) "Orders permission leaked finance-only totals"

  let runIssueHandlers user =
        let _a :<|> _b :<|> _c :<|> _d :<|> _e :<|> _f :<|> _g :<|> _h
              :<|> _i :<|> _j :<|> _k :<|> _l :<|> _m :<|> _n :<|> _o
              :<|> _p :<|> _orders :<|> scopedIssues :<|> updateScopedIssue :<|> _rest = merchProtectedServer user
        in (scopedIssues, updateScopedIssue)
      (collaboratorIssues, collaboratorUpdateIssue) = runIssueHandlers collaborator
      (outsiderIssues, _) = runIssueHandlers otherSeller
  issueRows <- runHandler (runReaderT (collaboratorIssues storeId Nothing) env) >>= either (fail . show) pure
  assert (length issueRows == 3) "Orders collaborator could not read the store-scoped issue queue"
  outsiderIssueResult <- runHandler (runReaderT (outsiderIssues storeId Nothing) env)
  case outsiderIssueResult of
    Left err -> assert (errHTTPCode err == 403) "Cross-seller issue access did not return 403"
    Right _ -> fail "A seller read another seller's support cases"
  resolvedIssue <- runHandler (runReaderT (collaboratorUpdateIssue storeId shippingIssueId (MerchIssueTriageRequest "resolved" (Just "The synthetic shipping question was resolved safely.") Nothing)) env)
  either (fail . show) (const (pure ())) resolvedIssue
  forbiddenFinancialClose <- runHandler (runReaderT (collaboratorUpdateIssue storeId refundIssueId (MerchIssueTriageRequest "resolved" (Just "This seller must not decide the financial outcome.") Nothing)) env)
  case forbiddenFinancialClose of
    Left err -> assert (errHTTPCode err == 409) "Seller financial closure did not return 409"
    Right _ -> fail "A seller resolved a refund case without staff review"
  escalatedIssue <- runHandler (runReaderT (collaboratorUpdateIssue storeId refundIssueId (MerchIssueTriageRequest "staff_review" Nothing (Just "Synthetic escalation without payment mutation"))) env)
  either (fail . show) (const (pure ())) escalatedIssue
  sellerTakeback <- runHandler (runReaderT (collaboratorUpdateIssue storeId refundIssueId (MerchIssueTriageRequest "seller_review" Nothing Nothing)) env)
  case sellerTakeback of
    Left err -> assert (errHTTPCode err == 409) "Seller took a case back after staff escalation"
    Right _ -> fail "A seller took control of a staff-review case"
  let runAdminIssueHandlers user =
        let _a :<|> _b :<|> _c :<|> _d :<|> _e :<|> _f :<|> _g :<|> _h
              :<|> _i :<|> _j :<|> _k :<|> _l :<|> _m :<|> _n :<|> _o :<|> _p
              :<|> _q :<|> _r :<|> _s :<|> _t :<|> _u :<|> _v :<|> _w :<|> _x
              :<|> scopedAdminIssues :<|> scopedAdminUpdateIssue :<|> _rest = merchProtectedServer user
        in (scopedAdminIssues, scopedAdminUpdateIssue)
      (adminIssues, adminUpdateIssue) = runAdminIssueHandlers strictAdmin
  adminIssueRows <- runHandler (runReaderT (adminIssues Nothing) env) >>= either (fail . show) pure
  assert (length adminIssueRows == 3) "Strict administrator could not read the cross-store issue queue"
  adminResolution <- runHandler (runReaderT (adminUpdateIssue refundIssueId (MerchIssueTriageRequest "resolved" (Just "Refund review completed using the independent synthetic workflow.") (Just "No provider or real funds were used"))) env)
  either (fail . show) (const (pure ())) adminResolution

  issueStates <- runSqlPool (rawSql
    "SELECT id::text,status FROM merch_order_issue WHERE id=ANY(?::uuid[]) ORDER BY id"
    [PersistArray [PersistText (UUID.toText shippingIssueId),PersistText (UUID.toText refundIssueId)]]
    :: SqlPersistT IO [(Single Text,Single Text)]) pool
  assert (map (\(Single _issueId,Single status) -> status) issueStates == ["resolved","resolved"]) "Seller/staff issue transitions were not persisted independently"

  states <- runSqlPool (rawSql
    "SELECT order_record.commercial_status,order_record.payment_status,order_record.fulfillment_status,checkout.status,reservation.status,variant.stock_reserved,(SELECT count(*) FROM merch_order_issue issue WHERE issue.order_id=order_record.id),(SELECT count(*) FROM merch_audit_event audit WHERE audit.entity_type='order' AND audit.entity_id=order_record.id::text AND audit.action='order.cancelled_before_payment'),(SELECT count(*) FROM merch_audit_event audit WHERE audit.entity_type='order_issue' AND audit.action='order_issue.status_changed') FROM merch_order order_record JOIN commerce_checkout_session checkout ON checkout.id=order_record.checkout_id JOIN merch_inventory_reservation reservation ON reservation.order_id=order_record.id JOIN merch_product_variant variant ON variant.id=reservation.variant_id WHERE order_record.id=?::uuid"
    [PersistText (UUID.toText orderId)]
    :: SqlPersistT IO [(Single Text,Single Text,Single Text,Single Text,Single Text,Single Int,Single Int64,Single Int64,Single Int64)]) pool
  case states of
    [(Single commercial,Single payment,Single fulfillment,Single checkoutStatus,Single reservationStatus,Single reserved,Single issueCount,Single cancellationAuditCount,Single issueAuditCount)] -> do
      assert (commercial == "cancelled") "Commercial status was not cancelled"
      assert (payment == "cancelled") "Checkout trigger did not keep payment status separate and cancelled"
      assert (fulfillment == "cancelled") "Fulfillment status was not independently cancelled"
      assert (checkoutStatus == "cancelled") "Canonical checkout was not cancelled"
      assert (reservationStatus == "released" && reserved == 0) "Reserved stock was not released exactly once"
      assert (issueCount == 3 && cancellationAuditCount == 1) "Cancellation retry duplicated issue or audit evidence"
      assert (issueAuditCount == 3) "Issue triage did not record exactly one audit row per accepted transition"
    _ -> fail "Cancellation runtime state was missing or ambiguous"
