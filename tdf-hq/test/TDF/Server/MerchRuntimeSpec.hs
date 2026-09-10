{-# LANGUAGE OverloadedStrings #-}

module TDF.Server.MerchRuntimeSpec (spec) where

import           Control.Monad (unless)
import           Control.Monad.Reader (runReaderT)
import           Codec.Picture (PixelRGB8(..), encodePng, generateImage)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (toList)
import           Data.Int (Int64)
import           Data.Set (empty)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as UUID
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool, toSqlKey)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai.Handler.Warp as Warp
import           Servant (ServerError(..), (:<|>)(..))
import           Servant.Server (runHandler)
import           System.Environment (lookupEnv, setEnv)
import           System.FilePath ((</>))
import           System.Directory (doesFileExist)
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Hspec (Spec, describe, it, runIO)

import           TDF.API.Merch (MerchCancellationRequest(..), MerchIssueTriageRequest(..))
import           TDF.Auth (AuthedUser(..), modulesForRoles)
import           TDF.Config (loadConfig)
import           TDF.DB (Env(..), makePool)
import           TDF.Models (RoleEnum(..))
import           TDF.Server (mkApp)
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
    Just value -> describe "artist-merch-runtime PostgreSQL handlers and HTTP API" $
      it "enforces buyer, seller, administrator, idempotency, payment, and tenant boundaries" $ do
        runChecks value
        runHttpChecks value

type HttpResponse = (Int, Aeson.Value, BL.ByteString)

httpJson
  :: HTTP.Manager
  -> Int
  -> BS8.ByteString
  -> String
  -> [HTTPTypes.Header]
  -> Maybe Aeson.Value
  -> IO HttpResponse
httpJson manager port requestMethod path headers body = do
  base <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> path)
  let request = base
        { HTTP.method = requestMethod
        , HTTP.requestHeaders =
            [("Accept","application/json"),("Content-Type","application/json")] <> headers
        , HTTP.requestBody = HTTP.RequestBodyLBS (maybe "" Aeson.encode body)
        , HTTP.checkResponse = \_ _ -> pure ()
        }
  response <- HTTP.httpLbs request manager
  let raw = HTTP.responseBody response
      decoded = either (const Aeson.Null) id (Aeson.eitherDecode raw)
  pure (HTTPTypes.statusCode (HTTP.responseStatus response), decoded, raw)

httpMultipart
  :: HTTP.Manager
  -> Int
  -> String
  -> [HTTPTypes.Header]
  -> [(BS8.ByteString, BS8.ByteString)]
  -> BL.ByteString
  -> IO HttpResponse
httpMultipart manager port path headers fields fileBytes = do
  base <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> path)
  let boundary = "----tdf-merch-settlement-runtime"
      line value = BL.fromStrict value <> "\r\n"
      fieldPart (name,value) =
        line ("--" <> boundary)
          <> line ("Content-Disposition: form-data; name=\"" <> name <> "\"")
          <> "\r\n" <> line value
      filePart =
        line ("--" <> boundary)
          <> line "Content-Disposition: form-data; name=\"file\"; filename=\"synthetic-receipt.png\""
          <> line "Content-Type: image/png"
          <> "\r\n" <> fileBytes <> "\r\n"
      body = mconcat (map fieldPart fields) <> filePart <> line ("--" <> boundary <> "--")
      request = base
        { HTTP.method = "POST"
        , HTTP.requestHeaders =
            [("Accept","application/json"),("Content-Type","multipart/form-data; boundary=" <> boundary)] <> headers
        , HTTP.requestBody = HTTP.RequestBodyLBS body
        , HTTP.checkResponse = \_ _ -> pure ()
        }
  response <- HTTP.httpLbs request manager
  let raw = HTTP.responseBody response
      decoded = either (const Aeson.Null) id (Aeson.eitherDecode raw)
  pure (HTTPTypes.statusCode (HTTP.responseStatus response), decoded, raw)

expectStatus :: Int -> String -> HttpResponse -> IO Aeson.Value
expectStatus expected label (actual, value, raw) = do
  assert (actual == expected)
    (label <> " returned HTTP " <> show actual <> " instead of " <> show expected
      <> "; response=" <> show raw)
  pure value

field :: Text -> Aeson.Value -> Maybe Aeson.Value
field key (Aeson.Object value) = KeyMap.lookup (AesonKey.fromText key) value
field _ _ = Nothing

textField :: Text -> Aeson.Value -> Maybe Text
textField key value = case field key value of
  Just (Aeson.String result) -> Just result
  _ -> Nothing

arrayField :: Text -> Aeson.Value -> [Aeson.Value]
arrayField key value = case field key value of
  Just (Aeson.Array result) -> toList result
  _ -> []

auth :: BS8.ByteString -> [HTTPTypes.Header]
auth token = [("Authorization", "Bearer " <> token)]

runHttpChecks :: String -> IO ()
runHttpChecks databaseUrl =
  withSystemTempDirectory "tdf-merch-settlement-evidence" (runHttpChecksWithEvidenceRoot databaseUrl)

runHttpChecksWithEvidenceRoot :: String -> FilePath -> IO ()
runHttpChecksWithEvidenceRoot databaseUrl evidenceRoot = do
  -- loadConfig supplies the real session-cookie/auth parser used by mkApp.
  -- The database and feature flags remain isolated to the disposable runtime.
  setEnv "DATABASE_URL" databaseUrl
  setEnv "APP_ENV" "sandbox"
  setEnv "MERCH_SETTLEMENT_EVIDENCE_DIR" evidenceRoot
  cfg <- loadConfig
  pool <- makePool (BS8.pack databaseUrl)
  let app = mkApp Env { envPool = pool, envConfig = cfg }
      storeId = "92000000-0000-4000-8000-000000000001"
      variantId = "96000000-0000-4000-8000-000000000001" :: Text
      shippingZoneId = "93000000-0000-4000-8000-000000000002" :: Text
      applicantProfileId = "91000000-0000-4000-8000-000000000003" :: Text
      settlementOrderId = "98000000-0000-4000-8000-000000000005" :: Text
      ownerHeaders = auth "runtime-owner-token"
      collaboratorHeaders = auth "runtime-collaborator-token"
      outsiderHeaders = auth "runtime-other-seller-token"
      adminHeaders = auth "runtime-admin-token"
      independentAdminHeaders = auth "runtime-independent-admin-token"
      applicantHeaders = auth "runtime-applicant-token"
  Warp.testWithApplication (pure app) $ \port -> do
    manager <- HTTP.newManager HTTP.defaultManagerSettings

    capabilities <- httpJson manager port "GET" "/merch/capabilities" [] Nothing
      >>= expectStatus 200 "Merch capabilities"
    assert (field "environment" capabilities == Just (Aeson.String "sandbox"))
      "HTTP capabilities did not remain in sandbox"
    assert ((field "checkout" =<< field "features" capabilities) == Just (Aeson.Bool True))
      "Synthetic manual checkout was not available inside the isolated runtime"
    assert ((field "datafast" =<< field "paymentMethods" capabilities) == Just (Aeson.Bool False))
      "Datafast became available without credentials"
    assert ((field "paypal" =<< field "paymentMethods" capabilities) == Just (Aeson.Bool False))
      "PayPal became available without credentials"
    assert ((field "bankTransfer" =<< field "paymentMethods" capabilities) == Just (Aeson.Bool True))
      "Synthetic manual checkout capability was not gated as expected"

    _ <- httpJson manager port "GET" "/merch/seller/stores" [] Nothing
      >>= expectStatus 401 "Unauthenticated seller request"
    _ <- httpJson manager port "GET" "/merch/seller/stores" (auth "invalid-runtime-token") Nothing
      >>= expectStatus 401 "Invalid seller token"
    _ <- httpJson manager port "GET" "/merch/storefronts/runtime-band" [] Nothing
      >>= expectStatus 200 "Public storefront"
    productResponse <- httpJson manager port "GET" "/merch/storefronts/runtime-band/products/runtime-shirt" [] Nothing
      >>= expectStatus 200 "Public product"
    assert (textField "name" productResponse == Just "Runtime Shirt")
      "Public product response was not serialized through the HTTP API"

    ownerStores <- httpJson manager port "GET" "/merch/seller/stores" ownerHeaders Nothing
      >>= expectStatus 200 "Owner stores"
    assert (not (null (case ownerStores of Aeson.Array rows -> toList rows; _ -> [])))
      "Authenticated primary owner did not receive a managed store"
    _ <- httpJson manager port "GET" ("/merch/seller/stores/" <> storeId <> "/orders") outsiderHeaders Nothing
      >>= expectStatus 403 "Cross-seller order request"
    collaboratorOrders <- httpJson manager port "GET" ("/merch/seller/stores/" <> storeId <> "/orders") collaboratorHeaders Nothing
      >>= expectStatus 200 "Collaborator order queue"
    _ <- httpJson manager port "GET" ("/merch/seller/stores/" <> storeId <> "/orders?status=not-a-real-state") collaboratorHeaders Nothing
      >>= expectStatus 400 "Invalid seller order filter"
    let leaksFinance (Aeson.Object row) =
          KeyMap.member (AesonKey.fromText "sellerNetMinor") row
            || KeyMap.member (AesonKey.fromText "tdfCommissionMinor") row
        leaksFinance _ = True
    assert (not (any leaksFinance (case collaboratorOrders of Aeson.Array rows -> toList rows; _ -> [Aeson.Null])))
      "Orders-only collaborator received finance-only fields over HTTP"

    let applicationBody = Aeson.object
          [ "profileId" Aeson..= applicantProfileId
          , "slug" Aeson..= ("runtime-applicant-band-store" :: Text)
          , "displayName" Aeson..= ("Runtime Applicant Band" :: Text)
          , "description" Aeson..= ("Synthetic pilot applicant used only by the isolated HTTP test." :: Text)
          , "applicationNote" Aeson..= ("We want to validate the pilot workflow with synthetic catalog data." :: Text)
          ]
        applicationRequestHeaders = ("Idempotency-Key","runtime-http-application-001") : applicantHeaders
    application <- httpJson manager port "POST" "/merch/seller/applications" applicationRequestHeaders (Just applicationBody)
      >>= expectStatus 201 "Seller application"
    applicationRetry <- httpJson manager port "POST" "/merch/seller/applications" applicationRequestHeaders (Just applicationBody)
      >>= expectStatus 201 "Seller application retry"
    let applicationId = textField "id" application
    assert (applicationId /= Nothing && applicationId == textField "id" applicationRetry)
      "Seller application retry did not return the same store"
    let reviewPath = "/merch/admin/stores/" <> maybe "missing" T.unpack applicationId <> "/review"
        reviewBody = Aeson.object
          [ "decision" Aeson..= ("approve" :: Text)
          , "reviewerNotes" Aeson..= ("Approved only for the synthetic isolated pilot runtime." :: Text)
          , "commissionBps" Aeson..= (0 :: Int)
          , "commissionReason" Aeson..= ("Synthetic pilot override; no commercial activity." :: Text)
          ]
    approved <- httpJson manager port "POST" reviewPath adminHeaders (Just reviewBody)
      >>= expectStatus 200 "Administrator store approval"
    assert (textField "applicationStatus" approved == Just "approved"
      && textField "operationalStatus" approved == Just "active")
      "Administrator approval did not activate the synthetic applicant store"

    cart <- httpJson manager port "POST" "/merch/carts" []
      (Just (Aeson.object ["storeSlug" Aeson..= ("runtime-band" :: Text)]))
      >>= expectStatus 201 "Guest cart creation"
    cartId <- maybe (fail "Cart HTTP response omitted id") pure (textField "id" cart)
    cartToken <- maybe (fail "Cart HTTP response omitted lookupToken") pure (textField "lookupToken" cart)
    let cartHeaders = [("X-Cart-Lookup-Token",BS8.pack (T.unpack cartToken))]
        cartPath suffix = "/merch/carts/" <> T.unpack cartId <> suffix
    updatedCart <- httpJson manager port "PUT" (cartPath "/items") cartHeaders
      (Just (Aeson.object ["variantId" Aeson..= variantId,"quantity" Aeson..= (1 :: Int)]))
      >>= expectStatus 200 "Guest cart item"
    assert (length (arrayField "items" updatedCart) == 1)
      "Guest cart did not retain its selected variant"

    let checkoutBody = Aeson.object
          [ "recipient" Aeson..= Aeson.object
              [ "name" Aeson..= ("Synthetic HTTP Buyer" :: Text)
              , "email" Aeson..= ("synthetic.http.buyer@example.test" :: Text)
              , "phone" Aeson..= Aeson.Null
              , "countryCode" Aeson..= ("EC" :: Text)
              , "subdivision" Aeson..= ("Pichincha" :: Text)
              , "city" Aeson..= ("Quito" :: Text)
              , "addressLine1" Aeson..= ("Synthetic address 100" :: Text)
              , "addressLine2" Aeson..= Aeson.Null
              , "postalCode" Aeson..= Aeson.Null
              , "deliveryNote" Aeson..= ("Synthetic data; do not dispatch." :: Text)
              ]
          , "shippingZoneId" Aeson..= shippingZoneId
          , "createAccount" Aeson..= False
          , "locale" Aeson..= ("es" :: Text)
          ]
        checkoutHeaders = ("Idempotency-Key","runtime-http-checkout-001") : cartHeaders
    order <- httpJson manager port "POST" (cartPath "/checkout") checkoutHeaders (Just checkoutBody)
      >>= expectStatus 200 "Guest checkout"
    orderRetry <- httpJson manager port "POST" (cartPath "/checkout") checkoutHeaders (Just checkoutBody)
      >>= expectStatus 200 "Guest checkout retry"
    orderId <- maybe (fail "Checkout HTTP response omitted order id") pure (textField "id" order)
    orderToken <- maybe (fail "Checkout HTTP response omitted order lookupToken") pure (textField "lookupToken" order)
    assert (textField "id" orderRetry == Just orderId)
      "Checkout retry created or returned a different order"
    assert (textField "paymentStatus" order == Just "pending"
      && textField "fulfillmentStatus" order == Just "pending")
      "Checkout conflated pending payment with fulfillment"
    assert (field "productSubtotalMinor" order == Just (Aeson.Number 5000)
      && field "shippingMinor" order == Just (Aeson.Number 500)
      && field "totalMinor" order == Just (Aeson.Number 5500))
      "Server-calculated HTTP checkout totals were incorrect"

    let conflictingCheckout = case checkoutBody of
          Aeson.Object value -> Aeson.Object (KeyMap.insert (AesonKey.fromText "locale") (Aeson.String "en") value)
          value -> value
    _ <- httpJson manager port "POST" (cartPath "/checkout") checkoutHeaders (Just conflictingCheckout)
      >>= expectStatus 409 "Conflicting checkout retry"
    let orderHeaders = [("X-Order-Lookup-Token",BS8.pack (T.unpack orderToken))]
        orderPath suffix = "/merch/orders/" <> T.unpack orderId <> suffix
    _ <- httpJson manager port "GET" (orderPath "") [("X-Order-Lookup-Token","wrong-runtime-token")] Nothing
      >>= expectStatus 404 "Wrong private order capability"
    browserReturn <- httpJson manager port "GET" (orderPath "?payment=success&status=paid") orderHeaders Nothing
      >>= expectStatus 200 "Forged browser return"
    assert (textField "paymentStatus" browserReturn == Just "pending")
      "A forged browser return changed the payment status"

    issue <- httpJson manager port "POST" (orderPath "/issues")
      (("Idempotency-Key","runtime-http-issue-001") : orderHeaders)
      (Just (Aeson.object
        [ "issueType" Aeson..= ("shipping" :: Text)
        , "message" Aeson..= ("Synthetic buyer asks for a harmless shipping clarification." :: Text)
        ]))
      >>= expectStatus 201 "Buyer issue creation"
    issueId <- maybe (fail "Issue HTTP response omitted id") pure (textField "id" issue)
    let issueTriagePath = "/merch/seller/stores/" <> storeId <> "/issues/" <> T.unpack issueId
    resolved <- httpJson manager port "PATCH" issueTriagePath collaboratorHeaders
      (Just (Aeson.object
        [ "status" Aeson..= ("resolved" :: Text)
        , "publicResponse" Aeson..= ("Synthetic shipping question resolved without exposing private notes." :: Text)
        , "internalNotes" Aeson..= Aeson.Null
        ]))
      >>= expectStatus 200 "Seller issue triage"
    assert (textField "status" resolved == Just "resolved")
      "Orders collaborator did not resolve the operational issue over HTTP"
    _ <- httpJson manager port "GET" "/merch/admin/issues" adminHeaders Nothing
      >>= expectStatus 200 "Administrator issue queue"

    _ <- httpJson manager port "GET" ("/merch/admin/stores/" <> storeId <> "/settlement-orders") outsiderHeaders Nothing
      >>= expectStatus 403 "Non-admin settlement order queue"
    eligibleSettlementOrders <- httpJson manager port "GET" ("/merch/admin/stores/" <> storeId <> "/settlement-orders") adminHeaders Nothing
      >>= expectStatus 200 "Settlement eligible order queue"
    let eligibleRows = case eligibleSettlementOrders of Aeson.Array rows -> toList rows; _ -> []
        isSettlementOrder value = textField "id" value == Just settlementOrderId
        leaksBuyerData (Aeson.Object row) = any (`KeyMap.member` row) (map AesonKey.fromText ["customerName","customerEmail","customerPhone","recipient"])
        leaksBuyerData _ = True
    assert (any isSettlementOrder eligibleRows) "Delivered paid order was not eligible for settlement"
    assert (not (any leaksBuyerData eligibleRows)) "Settlement preparation queue exposed buyer personal data"

    let settlementBody = Aeson.object
          [ "storeId" Aeson..= (T.pack storeId)
          , "periodStart" Aeson..= ("2026-01-01" :: Text)
          , "periodEnd" Aeson..= ("2027-01-01" :: Text)
          , "orderIds" Aeson..= [settlementOrderId]
          , "reviewNotes" Aeson..= ("Synthetic preparation; no funds are moved by this request." :: Text)
          ]
        outsidePeriodBody = Aeson.object
          [ "storeId" Aeson..= (T.pack storeId)
          , "periodStart" Aeson..= ("2025-01-01" :: Text)
          , "periodEnd" Aeson..= ("2025-02-01" :: Text)
          , "orderIds" Aeson..= [settlementOrderId]
          ]
    _ <- httpJson manager port "POST" "/merch/admin/settlements" adminHeaders (Just outsidePeriodBody)
      >>= expectStatus 409 "Settlement order outside accounting period"
    settlement <- httpJson manager port "POST" "/merch/admin/settlements" adminHeaders (Just settlementBody)
      >>= expectStatus 201 "Settlement preparation"
    settlementId <- maybe (fail "Settlement response omitted id") pure (textField "id" settlement)
    assert (textField "status" settlement == Just "under_review")
      "Settlement preparation implied approval or payment"
    _ <- httpJson manager port "GET" "/merch/admin/settlements?status=under_review" independentAdminHeaders Nothing
      >>= expectStatus 200 "Settlement review queue"
    let settlementStatusPath = "/merch/admin/settlements/" <> T.unpack settlementId <> "/status"
        approvalBody = Aeson.object
          [ "status" Aeson..= ("approved" :: Text)
          , "reason" Aeson..= ("Independently checked against the synthetic reconciliation data." :: Text)
          ]
        holdBody = Aeson.object
          [ "status" Aeson..= ("held" :: Text)
          , "reason" Aeson..= ("Synthetic discrepancy requires independent review." :: Text)
          ]
    _ <- httpJson manager port "PATCH" settlementStatusPath adminHeaders (Just approvalBody)
      >>= expectStatus 409 "Settlement self-approval"
    heldSettlement <- httpJson manager port "PATCH" settlementStatusPath independentAdminHeaders (Just holdBody)
      >>= expectStatus 200 "Independent settlement hold"
    assert (textField "status" heldSettlement == Just "held")
      "Independent review did not place the settlement on hold"
    approvedSettlement <- httpJson manager port "PATCH" settlementStatusPath independentAdminHeaders (Just approvalBody)
      >>= expectStatus 200 "Independent approval after settlement hold"
    assert (textField "status" approvedSettlement == Just "approved")
      "Independent review did not approve the settlement"

    paidAt <- BS8.pack . iso8601Show <$> getCurrentTime
    let evidenceBytes = encodePng (generateImage (\_ _ -> PixelRGB8 32 64 96) 4 4)
        evidenceHeaders = ("Idempotency-Key","runtime-settlement-payment-001") : independentAdminHeaders
        evidenceFields reference =
          [ ("paidAt",paidAt)
          , ("externalReference",reference)
          , ("notes","Synthetic receipt; no real bank transfer or provider was used.")
          ]
        evidencePath = "/merch/admin/settlements/" <> T.unpack settlementId <> "/payment-evidence"
    paidSettlement <- httpMultipart manager port evidencePath evidenceHeaders (evidenceFields "SYNTHETIC-BANK-REFERENCE-001") evidenceBytes
      >>= expectStatus 200 "Settlement payment evidence"
    paidSettlementRetry <- httpMultipart manager port evidencePath evidenceHeaders (evidenceFields "SYNTHETIC-BANK-REFERENCE-001") evidenceBytes
      >>= expectStatus 200 "Settlement payment evidence retry"
    assert (textField "status" paidSettlement == Just "paid" && textField "status" paidSettlementRetry == Just "paid")
      "Private payment evidence did not idempotently record the paid settlement"
    _ <- httpMultipart manager port evidencePath evidenceHeaders (evidenceFields "DIFFERENT-REFERENCE") evidenceBytes
      >>= expectStatus 409 "Conflicting settlement payment evidence retry"
    evidenceObjectKey <- maybe (fail "Paid settlement omitted evidence object key") pure (textField "evidenceObjectKey" paidSettlement)
    evidenceFileName <- maybe (fail "Settlement evidence object key was outside its private namespace") pure
      (T.stripPrefix ("merch-settlements/" <> settlementId <> "/") evidenceObjectKey)
    evidenceExists <- doesFileExist (evidenceRoot </> T.unpack settlementId </> T.unpack evidenceFileName)
    assert evidenceExists "Re-encoded private settlement evidence was not persisted"

    persistedSettlement <- runSqlPool (rawSql
      "SELECT settlement.status,order_record.settlement_status,(SELECT count(*) FROM merch_settlement_payment_evidence evidence WHERE evidence.settlement_id=settlement.id),(SELECT count(*) FROM merch_audit_event audit WHERE audit.entity_type='settlement' AND audit.entity_id=settlement.id::text AND audit.action='settlement.payment_recorded') FROM merch_settlement settlement JOIN merch_settlement_order linked ON linked.settlement_id=settlement.id JOIN merch_order order_record ON order_record.id=linked.order_id WHERE settlement.id=?::uuid"
      [PersistText settlementId]
      :: SqlPersistT IO [(Single Text,Single Text,Single Int64,Single Int64)]) pool
    assert (persistedSettlement == [(Single "paid",Single "paid",Single 1,Single 1)])
      "Settlement evidence did not preserve one paid ledger state and one audit event"

    let cancellationBody = Aeson.object
          ["reason" Aeson..= ("Synthetic buyer cancels before any payment attempt." :: Text)]
        cancellationHeaders = ("Idempotency-Key","runtime-http-cancel-001") : orderHeaders
    cancelled <- httpJson manager port "POST" (orderPath "/cancel") cancellationHeaders (Just cancellationBody)
      >>= expectStatus 200 "Guest cancellation"
    cancellationRetry <- httpJson manager port "POST" (orderPath "/cancel") cancellationHeaders (Just cancellationBody)
      >>= expectStatus 200 "Guest cancellation retry"
    assert (textField "commercialStatus" cancelled == Just "cancelled"
      && textField "paymentStatus" cancellationRetry == Just "cancelled"
      && textField "fulfillmentStatus" cancellationRetry == Just "cancelled")
      "HTTP cancellation did not preserve independent terminal states"

    persisted <- runSqlPool (rawSql
      "SELECT order_record.payment_status,order_record.fulfillment_status,checkout.status,reservation.status,variant.stock_reserved,(SELECT count(*) FROM merch_order duplicate WHERE duplicate.store_id=order_record.store_id AND duplicate.create_idempotency_key='runtime-http-checkout-001'),(SELECT count(*) FROM commerce_payment_attempt attempt WHERE attempt.checkout_id=checkout.id) FROM merch_order order_record JOIN commerce_checkout_session checkout ON checkout.id=order_record.checkout_id JOIN merch_inventory_reservation reservation ON reservation.order_id=order_record.id JOIN merch_product_variant variant ON variant.id=reservation.variant_id WHERE order_record.id=?::uuid"
      [PersistText orderId]
      :: SqlPersistT IO [(Single Text,Single Text,Single Text,Single Text,Single Int,Single Int64,Single Int64)]) pool
    case persisted of
      [(Single payment,Single fulfillment,Single checkoutStatus,Single reservationStatus,Single reserved,Single duplicateCount,Single paymentAttemptCount)] -> do
        assert (payment == "cancelled" && fulfillment == "cancelled" && checkoutStatus == "cancelled")
          "Persisted HTTP order states were not independently cancelled"
        assert (reservationStatus == "released" && reserved == 0)
          "HTTP cancellation did not release its stock reservation exactly once"
        assert (duplicateCount == 1) "HTTP checkout idempotency created a duplicate order"
        assert (paymentAttemptCount == 0) "Synthetic HTTP checkout unexpectedly created a payment attempt"
      _ -> fail "HTTP checkout persistence evidence was missing or ambiguous"
    pilotCommission <- runSqlPool (rawSql
      "SELECT commission_bps FROM merch_commission_policy WHERE store_id=?::uuid AND effective_until IS NULL"
      [PersistText (maybe "" id applicationId)] :: SqlPersistT IO [Single Int]) pool
    assert (pilotCommission == [Single 0])
      "Administrator pilot approval did not persist the audited 0% commission override"

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
