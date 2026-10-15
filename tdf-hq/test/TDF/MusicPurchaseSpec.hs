{-# LANGUAGE OverloadedStrings #-}
module TDF.MusicPurchaseSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, withAsync, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, bracket_, finally, try)
import Control.Monad (forM, forM_, replicateM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist (toPersistValue)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool, toSqlKey)
import Servant (Handler, ServerError(errHTTPCode), (:<|>)(..))
import Servant.Server.Internal.Handler (runHandler)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Test.Hspec
import TDF.API.MusicRelease (MusicFreeDownloadRequest(..), MusicPurchaseCreateRequest(..))
import TDF.Auth (AuthedUser(..), modulesForRoles)
import qualified TDF.Commerce.CheckoutStore as Checkout
import TDF.DB (Env(..))
import TDF.Models (RoleEnum(Fan))
import TDF.Server.MusicRelease (musicReleasePublicServer)
import TDF.Server.MusicReleaseAssets
  (authorizeDownloadDB, authorizeFreeMusicDownload, downloadUrlLifetimeSeconds)
import TDF.Server.MusicReleaseCommerce
  (PurchaseContext(..), createMusicPurchase, loadPurchaseContext, recordMusicVerifiedPayment)

-- PAY-MUSIC-DOWNLOAD-001 on PostgreSQL with the migrated triggers: the real
-- purchase handler, the real verified-payment record and the real download
-- authorization. No payment provider or object store is contacted; the
-- provider's answer is represented by the capture identifier handed to the
-- recording step. Fixture releases are inserted with triggers bypassed and
-- are withdrawn again when the group ends.
spec :: Spec
spec = describe "music-purchase-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_MUSIC_PURCHASE_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-music-purchase.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (open connection) $ afterAll close $ beforeWith (pure . fst) $ do
      it "creates one order per buyer and idempotency key" $ \pool -> do
        offer <- purchaseOffer pool
        first <- order pool offer "music-fixture-key-1"
        again <- order pool offer "music-fixture-key-1"
        again `shouldBe` first
        other <- order pool offer "music-fixture-key-2"
        other `shouldNotBe` first
        ordersOf pool offer `shouldReturn` 2

      it "preserves a published price outside the JavaScript integer range" $ \pool -> do
        offer <- offerWith pool "'purchase',TRUE,9007199254740993,'USD'"
        slug <- runSqlPool (rawSql "SELECT canonical_slug FROM music_release WHERE id=?::uuid"
          [toPersistValue (offerRelease offer)]) pool
        let _ :<|> getOne :<|> _ = musicReleasePublicServer
        case slug of
          [Single value] -> do
            response <- run pool (getOne value Nothing)
            case response of
              Right (Object fields) -> case KeyMap.lookup "availability" fields of
                Just (Array rules) ->
                  [KeyMap.lookup "priceMinor" rule | Object rule <- toList rules]
                    `shouldBe` [Just (String "9007199254740993")]
                _ -> expectationFailure "published offer has no availability"
              other -> expectationFailure ("published price read failed: " <> show other)
          _ -> expectationFailure "fixture release slug missing"

      it "requires the exact confirmed offer before creating a purchase" $ \pool -> do
        offer <- purchaseOffer pool
        forM_ [("251", "USD", 409), ("250", "EUR", 409),
               ("250.0", "USD", 400), ("0", "USD", 400),
               ("9223372036854775808", "USD", 400), ("250", "US", 400)] $
          \(amount, currency, expected) -> do
            response <- run pool (createMusicPurchase (buyerOf offer)
              (Just "music-fixture-exact-quote") Nothing
              (MusicPurchaseCreateRequest (offerRule offer) "EC" "fixture-terms-v1" amount currency))
            either errHTTPCode (const 201) response `shouldBe` expected
        ordersOf pool offer `shouldReturn` 0
        _ <- order pool offer "music-fixture-exact-quote"
        ordersOf pool offer `shouldReturn` 1

      it "preserves a large confirmed purchase amount through its JSON response" $ \pool -> do
        offer <- offerWith pool "'purchase',TRUE,9007199254740993,'USD'"
        response <- run pool (createMusicPurchase (buyerOf offer)
          (Just "music-fixture-exact-large") Nothing
          (MusicPurchaseCreateRequest (offerRule offer) "EC" "fixture-terms-v1"
            "9007199254740993" "USD"))
        case response of
          Right (Object fields) -> KeyMap.lookup "grossMinor" fields
            `shouldBe` Just (String "9007199254740993")
          other -> expectationFailure ("large confirmed purchase failed: " <> show other)
        ordersOf pool offer `shouldReturn` 1

      it "does not admit a sale in a currency unsupported by music content" $ \pool -> do
        offer <- offerWith pool "'purchase',TRUE,250,'EUR'"
        response <- run pool (createMusicPurchase (buyerOf offer)
          (Just "music-fixture-exact-currency") Nothing
          (MusicPurchaseCreateRequest (offerRule offer) "EC" "fixture-terms-v1" "250" "EUR"))
        either errHTTPCode (const 201) response `shouldBe` 404
        ordersOf pool offer `shouldReturn` 0

      it "refuses a purchase key replay with different accepted terms" $ \pool -> do
        offer <- purchaseOffer pool
        original <- order pool offer "music-fixture-replay-terms"
        conflict <- run pool (createMusicPurchase (buyerOf offer)
          (Just "music-fixture-replay-terms") Nothing
          (MusicPurchaseCreateRequest (offerRule offer) "EC" "fixture-terms-v2" "250" "USD"))
        either errHTTPCode (const 201) conflict `shouldBe` 409
        order pool offer "music-fixture-replay-terms" `shouldReturn` original
        ordersOf pool offer `shouldReturn` 1

      it "binds a purchase key to the offer and the declared territory" $ \pool -> do
        offer <- purchaseOffer pool
        other <- purchaseOffer pool
        let key = "music-fixture-replay-payload"
        original <- order pool offer key
        forM_ [(offerRule other, "EC"), (offerRule offer, "US")] $ \(rule, territory) -> do
          conflict <- run pool (createMusicPurchase (buyerOf offer) (Just key) Nothing
            (MusicPurchaseCreateRequest rule territory "fixture-terms-v1" "250" "USD"))
          either errHTTPCode (const 201) conflict `shouldBe` 409
        order pool offer key `shouldReturn` original
        ordersOf pool offer `shouldReturn` 1

      it "normalizes replay inputs and preserves the original order" $ \pool -> do
        offer <- purchaseOffer pool
        let key = "music-fixture-replay-normalized"
        original <- run pool (createMusicPurchase (buyerOf offer) (Just key) Nothing
          (MusicPurchaseCreateRequest (offerRule offer) " ec " " fixture-terms-v1 " "250" "USD"))
        replay <- run pool (createMusicPurchase (buyerOf offer) (Just key) Nothing
          (MusicPurchaseCreateRequest (offerRule offer) "EC" "fixture-terms-v1" "250" "USD"))
        replay `shouldBe` original
        case replay of
          Right (Object fields) -> KeyMap.lookup "grossMinor" fields `shouldBe` Just (Number 250)
          _ -> expectationFailure "normalized replay did not return its original order"
        ordersOf pool offer `shouldReturn` 1

      it "scopes a purchase key to its buyer" $ \pool -> do
        offer <- purchaseOffer pool
        let otherBuyer = offer {offerBuyer = offerBuyer offer + 1}
            key = "music-fixture-replay-buyer"
        first <- order pool offer key
        second <- order pool otherBuyer key
        first `shouldNotBe` second
        ordersOf pool offer `shouldReturn` 1
        ordersOf pool otherBuyer `shouldReturn` 1

      it "binds the effective edge territory and refuses missing replay evidence" $ \pool -> do
        offer <- purchaseOffer pool
        previous <- lookupEnv "MUSIC_TRUST_CF_IPCOUNTRY"
        let restore = maybe (unsetEnv "MUSIC_TRUST_CF_IPCOUNTRY")
              (setEnv "MUSIC_TRUST_CF_IPCOUNTRY") previous
            key = "music-fixture-replay-edge"
            request = MusicPurchaseCreateRequest (offerRule offer) "EC" "fixture-terms-v1" "250" "USD"
        bracket_ (setEnv "MUSIC_TRUST_CF_IPCOUNTRY" "true") restore $ do
          original <- run pool (createMusicPurchase (buyerOf offer) (Just key) (Just "EC") request)
          either errHTTPCode (const 201) original `shouldBe` 201
          changed <- run pool (createMusicPurchase (buyerOf offer) (Just key) (Just "US") request)
          either errHTTPCode (const 201) changed `shouldBe` 409
          -- Deliberately corrupt only this synthetic receipt to represent an
          -- older/incomplete binding. Normal line-item writes are immutable.
          runSqlPool (do
            rawExecute "SET LOCAL session_replication_role=replica" []
            rawExecute
              "UPDATE commerce_checkout_line_item item SET snapshot=snapshot-'termsVersion' FROM music_purchase_order purchase WHERE item.checkout_id=purchase.checkout_id AND purchase.buyer_party_id=?"
              [toPersistValue (offerBuyer offer)]) pool
          missing <- run pool (createMusicPurchase (buyerOf offer) (Just key) (Just "EC") request)
          either errHTTPCode (const 201) missing `shouldBe` 409
        ordersOf pool offer `shouldReturn` 1

      it "gives concurrent conflicting requests one order and one conflict" $ \pool -> do
        offer <- purchaseOffer pool
        forM_ [1 .. 12 :: Int] $ \attempt -> do
          let key = "music-fixture-replay-race-" <> T.pack (show attempt)
          outcomes <- within (mapConcurrently (\terms -> run pool
            (createMusicPurchase (buyerOf offer) (Just key) Nothing
              (MusicPurchaseCreateRequest (offerRule offer) "EC" terms "250" "USD")))
            ["fixture-terms-v1", "fixture-terms-v2"])
          length (filter isRight outcomes) `shouldBe` 1
          [errHTTPCode err | Left err <- outcomes] `shouldBe` [409]
        ordersOf pool offer `shouldReturn` 12

      it "checks the winning snapshot after both requests have observed an empty key" $ \pool -> do
        offer <- purchaseOffer pool
        nonce <- T.filter (/= '-') . UUID.toText <$> nextRandom
        let name = "music_replay_barrier_" <> nonce
            key = "music-fixture-replay-locked-" <> nonce
        lockKey <- randomRIO (4000000000, 8000000000 :: Int64)
        ready <- newEmptyMVar
        release <- newEmptyMVar
        let install = runSqlPool (do
              rawExecute ("CREATE FUNCTION " <> name <> "() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.idempotency_key='" <> key <> "' THEN PERFORM pg_advisory_xact_lock(" <> T.pack (show lockKey) <> "); END IF; RETURN NEW; END $$") []
              rawExecute ("CREATE TRIGGER " <> name <> " BEFORE INSERT ON music_purchase_order FOR EACH ROW EXECUTE FUNCTION " <> name <> "()") []) pool
            cleanup = runSqlPool (do
              rawExecute ("DROP TRIGGER " <> name <> " ON music_purchase_order") []
              rawExecute ("DROP FUNCTION " <> name <> "()") []) pool
            hold = runSqlPool (do
              _ <- rawSql "SELECT pg_advisory_xact_lock(?)::text" [toPersistValue lockKey]
                :: SqlPersistT IO [Single Text]
              [Single pid] <- rawSql "SELECT pg_backend_pid()" []
              liftIO (putMVar ready (pid :: Int64))
              liftIO (takeMVar release)) pool
            awaitBoth pid = do
              blocked <- runSqlPool (rawSql
                "SELECT count(*) FROM pg_stat_activity WHERE ?=ANY(pg_blocking_pids(pid)) AND query LIKE 'INSERT INTO music_purchase_order%'"
                [toPersistValue pid]) pool
              if blocked == [Single (2 :: Int64)] then pure ()
                else threadDelay 10000 >> awaitBoth pid
            requests = mapConcurrently (\terms -> run pool
              (createMusicPurchase (buyerOf offer) (Just key) Nothing
                (MusicPurchaseCreateRequest (offerRule offer) "EC" terms "250" "USD")))
              ["fixture-terms-v1", "fixture-terms-v2"]
        bracket_ install cleanup $ within $ withAsync hold $ \holder -> do
          pid <- takeMVar ready
          withAsync requests $ \pending -> flip finally (void (tryPutMVar release ())) $ do
            within (awaitBoth pid)
            putMVar release ()
            outcomes <- wait pending
            length (filter isRight outcomes) `shouldBe` 1
            [errHTTPCode err | Left err <- outcomes] `shouldBe` [409]
          wait holder
        ordersOf pool offer `shouldReturn` 1

      it "refuses to mark a checkout paid without bound verified evidence" $ \pool -> do
        offer <- purchaseOffer pool
        orderId <- order pool offer "music-fixture-unverified"
        context <- purchaseContext pool offer orderId
        forged <- try (runSqlPool (rawExecute
          "UPDATE commerce_checkout_session SET status='paid', paid_minor=total_minor, paid_at=NOW() WHERE id=?::uuid"
          [toPersistValue (purchaseCheckoutId context)]) pool) :: IO (Either SomeException ())
        either show (const "accepted") forged
          `shouldContain` "music checkout cannot become paid without bound verified payment evidence"
        orderState pool orderId `shouldReturn` "awaiting_payment"
        entitlementsOf pool orderId `shouldReturn` []

      it "records a verified payment and grants exactly one entitlement, however often it is repeated" $ \pool -> do
        offer <- purchaseOffer pool
        orderId <- order pool offer "music-fixture-paid"
        pay pool offer orderId `shouldReturn` Right True
        orderState pool orderId `shouldReturn` "paid"
        entitlementsOf pool orderId `shouldReturn` [("active", Just 10)]
        -- The checkout store reports the repeat as already settled.
        pay pool offer orderId `shouldReturn` Right False
        entitlementsOf pool orderId `shouldReturn` [("active", Just 10)]

      it "counts each download request once and stops at the entitlement limit" $ \pool -> do
        offer <- purchaseOffer pool
        entitlement <- paidEntitlement pool offer "music-fixture-limit"
        now <- getCurrentTime
        requests <- replicateM 10 nextRandom
        forM_ requests $ \request ->
          download pool (offerBuyer offer) entitlement request now `shouldReturn` Right ()
        extra <- nextRandom
        download pool (offerBuyer offer) entitlement extra now `shouldReturn` Left "limit_reached"
        downloadsOf pool entitlement `shouldReturn` 10

      it "honours a repeated request only while its signed URL is still valid" $ \pool -> do
        offer <- purchaseOffer pool
        entitlement <- paidEntitlement pool offer "music-fixture-replay"
        now <- getCurrentTime
        request <- nextRandom
        let lifetime = fromIntegral downloadUrlLifetimeSeconds
        download pool (offerBuyer offer) entitlement request now `shouldReturn` Right ()
        download pool (offerBuyer offer) entitlement request (addUTCTime (lifetime - 1) now) `shouldReturn` Right ()
        download pool (offerBuyer offer) entitlement request (addUTCTime (lifetime + 1) now)
          `shouldReturn` Left "request_expired"
        downloadsOf pool entitlement `shouldReturn` 1

      it "answers a repeated request with its original authorization time" $ \pool -> do
        offer <- purchaseOffer pool
        entitlement <- paidEntitlement pool offer "music-fixture-replay-expiry"
        now <- getCurrentTime
        request <- nextRandom
        let lifetime = fromIntegral downloadUrlLifetimeSeconds
            authorizedAt moment = fmap snd <$>
              runSqlPool (authorizeDownloadDB (offerBuyer offer) entitlement request moment) pool
            near expected =
              either (const False) (\actual -> abs (diffUTCTime actual expected) < 0.001)
        first <- authorizedAt now
        first `shouldSatisfy` near now
        -- Just before the window closes the replay still signs from the first
        -- authorization, so the URL it yields expires with the original one.
        replay <- authorizedAt (addUTCTime (lifetime - 1) now)
        replay `shouldBe` first

      it "admits exactly the limit when requests arrive together" $ \pool -> do
        offer <- purchaseOffer pool
        entitlement <- paidEntitlement pool offer "music-fixture-race"
        now <- getCurrentTime
        requests <- replicateM 16 nextRandom
        outcomes <- within (mapConcurrently (\request -> download pool (offerBuyer offer) entitlement request now) requests)
        length (filter isRight outcomes) `shouldBe` 10
        filter (not . isRight) outcomes `shouldBe` replicate 6 (Left "limit_reached")
        downloadsOf pool entitlement `shouldReturn` 10

      it "keeps an entitlement to its buyer and a request identifier to its entitlement" $ \pool -> do
        offer <- purchaseOffer pool
        entitlement <- paidEntitlement pool offer "music-fixture-owner"
        other <- paidEntitlement pool offer "music-fixture-owner-2"
        now <- getCurrentTime
        request <- nextRandom
        download pool (offerBuyer offer + 1) entitlement request now `shouldReturn` Left "not_found"
        download pool (offerBuyer offer) entitlement request now `shouldReturn` Right ()
        download pool (offerBuyer offer) other request now `shouldReturn` Left "request_conflict"

      it "gives a request identifier to one entitlement when two claim it together" $ \pool -> do
        offer <- purchaseOffer pool
        one <- paidEntitlement pool offer "music-fixture-shared-1"
        two <- paidEntitlement pool offer "music-fixture-shared-2"
        now <- getCurrentTime
        forM_ [1 .. 10 :: Int] $ \_ -> do
          request <- nextRandom
          outcomes <- within (mapConcurrently (\entitlement -> download pool (offerBuyer offer) entitlement request now) [one, two])
          length (filter isRight outcomes) `shouldBe` 1
          filter (not . isRight) outcomes `shouldBe` [Left "request_conflict"]

      it "stays closed when the checkout environment has no enabled flag" $ \pool -> do
        offer <- purchaseOffer pool
        refused <- bracket_ (setEnv "COMMERCE_CHECKOUT_ENV" "production") (unsetEnv "COMMERCE_CHECKOUT_ENV") $
          run pool (createMusicPurchase (buyerOf offer) (Just "music-fixture-production") Nothing
            (MusicPurchaseCreateRequest (offerRule offer) "EC" "fixture-terms-v1" "250" "USD"))
        either errHTTPCode (const 200) refused `shouldBe` 503
        ordersOf pool offer `shouldReturn` 0

      it "ends the entitlement when the checkout is refunded" $ \pool -> do
        offer <- purchaseOffer pool
        orderId <- order pool offer "music-fixture-refund"
        _ <- pay pool offer orderId
        context <- purchaseContext pool offer orderId
        runSqlPool (rawExecute
          "UPDATE commerce_checkout_session SET status='refunded', refunded_minor=total_minor, updated_at=NOW() WHERE id=?::uuid"
          [toPersistValue (purchaseCheckoutId context)]) pool
        orderState pool orderId `shouldReturn` "refunded"
        entitlementsOf pool orderId `shouldReturn` [("refunded", Just 10)]
        entitlement <- entitlementId pool orderId
        now <- getCurrentTime
        request <- nextRandom
        download pool (offerBuyer offer) entitlement request now `shouldReturn` Left "not_found"

      it "does not revive a revoked free grant when it is requested again" $ \pool -> do
        offer <- freeOffer pool
        first <- nextRandom
        granted <- free pool offer first
        either errHTTPCode (const 200) granted `shouldBe` 200
        runSqlPool (rawExecute
          "UPDATE music_entitlement SET status='revoked', revoked_at=NOW() WHERE buyer_party_id=? AND source_kind='free_grant'"
          [toPersistValue (offerBuyer offer)]) pool
        second <- nextRandom
        refused <- free pool offer second
        either errHTTPCode (const 200) refused `shouldBe` 404
        rows <- runSqlPool (rawSql
          "SELECT status FROM music_entitlement WHERE buyer_party_id=? AND source_kind='free_grant'"
          [toPersistValue (offerBuyer offer)]) pool
        rows `shouldBe` [Single ("revoked" :: Text)]

data Offer = Offer
  { offerBuyer :: Int64
  , offerRule :: UUID
  , offerRelease :: UUID
  }

open :: String -> IO (ConnectionPool, [Text])
open connection = do
  pool <- runNoLoggingT (createPostgresqlPool (BS.pack connection) 20)
  -- The signer needs configuration but contacts nothing.
  setEnv "MUSIC_S3_ENDPOINT" "https://objects.fixture.invalid"
  setEnv "MUSIC_S3_ACCESS_KEY_ID" "fixture-access-key"
  setEnv "MUSIC_S3_SECRET_ACCESS_KEY" "fixture-secret-key"
  enabled <- runSqlPool (rawSql
    "UPDATE revenue_feature_flag SET enabled=TRUE WHERE flag_key IN ('music_releases.commerce','music_releases.public') \
    \AND environment='sandbox' AND NOT enabled RETURNING flag_key" []) pool
  pure (pool, [flag | Single flag <- enabled])

-- Puts back the flags this group switched on and takes its releases out of
-- the public catalogue. Orders, checkouts and download events stay: they are
-- append-only or restricted from deletion by design.
close :: (ConnectionPool, [Text]) -> IO ()
close (pool, enabled) = do
  runSqlPool (do
    forM_ enabled $ \flag -> rawExecute
      "UPDATE revenue_feature_flag SET enabled=FALSE WHERE flag_key=? AND environment='sandbox'" [toPersistValue flag]
    rawExecute "SET LOCAL session_replication_role = replica" []
    rawExecute
      "UPDATE music_release SET withdrawn_at=NOW() WHERE canonical_slug LIKE 'music-purchase-fixture-%' AND withdrawn_at IS NULL"
      []) pool
  destroyAllResources pool

purchaseOffer, freeOffer :: ConnectionPool -> IO Offer
purchaseOffer pool = offerWith pool "'purchase',TRUE,250,'USD'"
freeOffer pool = offerWith pool "'free',FALSE,NULL,NULL"

-- One published release with one downloadable asset and one availability
-- rule, plus two parties (the buyer and buyer + 1). Triggers are bypassed:
-- the release authoring workflow is not under test.
offerWith :: ConnectionPool -> Text -> IO Offer
offerWith pool terms = do
  buyer <- (* 2) <$> randomRIO (3650000000, 3699999999 :: Int64)
  [release, version, asset, rule] <- replicateM 4 nextRandom
  runSqlPool (do
    rawExecute "SET LOCAL session_replication_role = replica" []
    forM_ [buyer, buyer + 1] $ \party -> rawExecute
      "INSERT INTO party(id, display_name, is_org, primary_email, created_at) \
      \VALUES (?, 'Music purchase fixture', FALSE, 'music-fixture-' || ?::text || '@example.test', NOW())"
      [toPersistValue party, toPersistValue party]
    rawExecute
      "INSERT INTO music_release(id, artist_party_id, canonical_slug, release_kind, created_by) \
      \VALUES (?::uuid, ?, 'music-purchase-fixture-' || ?::text, 'single', ?)"
      [toPersistValue release, toPersistValue buyer, toPersistValue buyer, toPersistValue buyer]
    rawExecute
      "INSERT INTO music_release_version(id, release_id, version_number, state, title, display_artist, \
      \published_at, created_by, approved_by, approved_at, release_at_utc, release_timezone, \
      \immutable_snapshot, snapshot_sha256) VALUES (?::uuid, ?::uuid, 1, 'published', 'Music purchase fixture', \
      \'Fixture artist', NOW() - INTERVAL '1 hour', ?, ?, NOW() - INTERVAL '2 hours', NOW() - INTERVAL '1 hour', \
      \'UTC', '{}'::jsonb, repeat('b', 64))"
      [toPersistValue version, toPersistValue release, toPersistValue buyer, toPersistValue buyer]
    rawExecute "UPDATE music_release SET published_version_id=?::uuid WHERE id=?::uuid"
      [toPersistValue version, toPersistValue release]
    rawExecute
      "INSERT INTO music_asset(id, release_version_id, asset_role, storage_provider, bucket_name, object_key, \
      \media_type, byte_size, sha256, processing_state, immutable, created_by, ready_at) VALUES (?::uuid, ?::uuid, \
      \'cover_original', 's3_compatible', 'fixture', 'music/fixture/' || ?::text || '.bin', \
      \'application/octet-stream', 4, repeat('a', 64), 'ready', TRUE, ?, NOW())"
      [toPersistValue asset, toPersistValue version, toPersistValue buyer, toPersistValue buyer]
    rawExecute
      ("INSERT INTO music_availability_rule(id, release_version_id, listening_policy, download_policy, \
       \purchasable, price_minor, currency, downloadable_asset_id) VALUES (?::uuid, ?::uuid, 'none', "
        <> terms <> ", ?::uuid)")
      [toPersistValue rule, toPersistValue version, toPersistValue asset]) pool
  pure (Offer buyer rule release)

buyerOf :: Offer -> AuthedUser
buyerOf offer = AuthedUser
  { auPartyId = toSqlKey (offerBuyer offer)
  , auRoles = [Fan]
  , auModules = modulesForRoles [Fan]
  , auApiTokenId = Nothing
  , auSessionWitness = Nothing
  }

run :: ConnectionPool -> ReaderT Env Handler a -> IO (Either ServerError a)
run pool action = runHandler (runReaderT action
  Env {envPool = pool, envConfig = error "music purchase cases need no application config"})

order :: ConnectionPool -> Offer -> Text -> IO UUID
order pool offer key = do
  created <- run pool (createMusicPurchase (buyerOf offer) (Just key) Nothing
    (MusicPurchaseCreateRequest (offerRule offer) "EC" "fixture-terms-v1" "250" "USD"))
  case created of
    Right (Object fields) | Just (String raw) <- KeyMap.lookup "id" fields, Just ident <- UUID.fromText raw -> pure ident
    Right other -> fail ("unexpected purchase response: " <> show other)
    Left err -> fail ("purchase was refused: " <> show err)

purchaseContext :: ConnectionPool -> Offer -> UUID -> IO PurchaseContext
purchaseContext pool offer orderId =
  run pool (loadPurchaseContext (buyerOf offer) orderId) >>= either (fail . show) pure

-- What the capture handler does after the provider has answered COMPLETED.
pay :: ConnectionPool -> Offer -> UUID -> IO (Either Text Bool)
pay pool offer orderId = do
  context <- purchaseContext pool offer orderId
  now <- getCurrentTime
  let checkout = Checkout.CheckoutReference (UUID.toText (purchaseCheckoutId context))
      capture = "FIXTURE-" <> UUID.toText orderId
  attempt <- runSqlPool (Checkout.beginPaymentAttempt Checkout.PaymentAttemptCreation
    { Checkout.pacCheckout = checkout, Checkout.pacProvider = Checkout.ProviderPayPal
    , Checkout.pacEnvironment = Checkout.CheckoutSandbox, Checkout.pacOperation = Checkout.OperationCapture
    , Checkout.pacAmountMinor = purchaseAmountMinor context, Checkout.pacCurrency = purchaseCurrency context
    , Checkout.pacMerchantRef = "fixture-merchant", Checkout.pacIdempotencyKey = "capture-" <> UUID.toText orderId
    , Checkout.pacCreatedAt = now, Checkout.pacCorrelationId = "music-purchase-fixture"
    }) pool >>= either (fail . T.unpack) pure
  runSqlPool (recordMusicVerifiedPayment context checkout attempt Checkout.ProviderPayPal Checkout.CheckoutSandbox
    "fixture-merchant" "capture" capture (Just ("/v2/payments/captures/" <> capture)) now) pool

paidEntitlement :: ConnectionPool -> Offer -> Text -> IO UUID
paidEntitlement pool offer key = do
  orderId <- order pool offer key
  paid <- pay pool offer orderId
  paid `shouldBe` Right True
  entitlementId pool orderId

entitlementId :: ConnectionPool -> UUID -> IO UUID
entitlementId pool orderId = do
  rows <- runSqlPool (rawSql "SELECT id FROM music_entitlement WHERE purchase_order_id=?::uuid"
    [toPersistValue orderId]) pool
  case rows of
    [Single ident] -> pure ident
    _ -> fail "expected exactly one entitlement for the order"

download :: ConnectionPool -> Int64 -> UUID -> UUID -> UTCTime -> IO (Either Text ())
download pool buyer entitlement request now =
  fmap (const ()) <$> runSqlPool (authorizeDownloadDB buyer entitlement request now) pool

free :: ConnectionPool -> Offer -> UUID -> IO (Either ServerError Value)
free pool offer request =
  run pool (authorizeFreeMusicDownload (buyerOf offer) Nothing (MusicFreeDownloadRequest (offerRule offer) request))

ordersOf :: ConnectionPool -> Offer -> IO Int64
ordersOf pool offer = do
  rows <- runSqlPool (rawSql "SELECT count(*) FROM music_purchase_order WHERE buyer_party_id=?"
    [toPersistValue (offerBuyer offer)]) pool
  pure (sum [n | Single n <- rows])

orderState :: ConnectionPool -> UUID -> IO Text
orderState pool orderId = do
  rows <- runSqlPool (rawSql "SELECT state FROM music_purchase_order WHERE id=?::uuid" [toPersistValue orderId]) pool
  case rows of
    [Single state] -> pure state
    _ -> fail "purchase order disappeared"

entitlementsOf :: ConnectionPool -> UUID -> IO [(Text, Maybe Int)]
entitlementsOf pool orderId = do
  rows <- runSqlPool (rawSql
    "SELECT status, max_downloads FROM music_entitlement WHERE purchase_order_id=?::uuid" [toPersistValue orderId]) pool
  forM rows $ \(Single status, Single limit) -> pure (status, limit)

downloadsOf :: ConnectionPool -> UUID -> IO Int64
downloadsOf pool entitlement = do
  rows <- runSqlPool (rawSql "SELECT count(*) FROM music_download_event WHERE entitlement_id=?::uuid"
    [toPersistValue entitlement]) pool
  pure (sum [n | Single n <- rows])

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres:postgres@postgres:5432/"]

within :: IO a -> IO a
within action = timeout 30000000 action >>= maybe (fail "Music purchase case timed out") pure
