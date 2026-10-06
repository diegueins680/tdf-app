{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Run only against a dedicated, disposable PostgreSQL database.
module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft, isRight)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..), getCurrentTime, addUTCTime)
import Database.Persist
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql
import System.Environment (getEnv, lookupEnv)
import TDF.DisposableTicketDatabase (safeTicketDatabase)
import Test.Hspec
import qualified TDF.Models.SocialEventsModels as M
import TDF.Ticketing.Admission
import TDF.Ticketing.Inventory
import qualified TDF.Ticketing.Transfer as Transfer

main :: IO ()
main = do
  dsn <- getEnv "TICKET_ADMISSION_TEST_DSN"
  ci <- (== Just "true") <$> lookupEnv "CI"
  overrides <- traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"]
  unless (safeTicketDatabase ci overrides dsn) $
    fail "Refusing non-local ticket fixture routing or libpq overrides"
  runNoLoggingT $ withPostgresqlPool (BS.pack dsn) 8 $ \pool -> liftIO $ do
    names <- runSqlPool (rawSql "SELECT current_database()" []) pool
    unless (names == [Single ("tdf_ticket_admission_test" :: Text)]) $
      fail "Refusing fixtures outside tdf_ticket_admission_test"
    hspec $ do
      describe "opaque admission credentials" $ do
        it "accepts legacy codes and generates full random credentials without PII" $ do
          normalizeTicketCode " tdf-ab12cd34ef56 " `shouldBe` Just "TDF-AB12CD34EF56"
          codes <- sequence (replicate 100 newTicketCode)
          length (nub codes) `shouldBe` 100
          forM_ codes $ \code -> do
            T.length code `shouldBe` 36
            normalizeTicketCode code `shouldBe` Just code
          forM_ ["TDF-123", "TDF-ABCDEFGHIJKL", "TDF-１２３４５６７８９０１２", "1|1|email@example.com|signature"] $ \code ->
            normalizeTicketCode code `shouldBe` Nothing
      before_ (seed pool) $ describe "PostgreSQL final-ticket inventory" $ do
        forM_ [1,2] $ \remaining -> it ("sells only " <> show remaining <> " remaining places to eight concurrent buyers") $ do
          runSqlPool (update (toSqlKey 1) [M.EventTicketTierQuantitySold =. (20 - remaining)]) pool
          now <- getCurrentTime
          results <- mapConcurrently (const (runSqlPool
            (reserveTicketInventory (toSqlKey 1) (toSqlKey 1) 1 now) pool)) [1..8 :: Int]
          length (filter id results) `shouldBe` remaining
          runSqlPool (fmap (fmap M.eventTicketTierQuantitySold) (get (toSqlKey 1))) pool
            `shouldReturn` Just 20
        it "shares the event capacity between different tiers" $ do
          runSqlPool (do
            update (toSqlKey 1) [M.SocialEventCapacity =. Just 2]
            update (toSqlKey 1) [M.EventTicketTierQuantitySold =. 0]
            rawExecute "INSERT INTO event_ticket_tier(id,event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active,created_at,updated_at) VALUES (2,1,'SECOND','Second',2000,'USD',20,0,true,now(),now())" []
            ) pool
          now <- getCurrentTime
          results <- mapConcurrently (\tier -> runSqlPool
            (reserveTicketInventory (toSqlKey 1) (toSqlKey tier) 1 now) pool) [1,2,1,2,1,2,1,2]
          length (filter id results) `shouldBe` 2
        it "rolls inventory back when order creation fails" $ do
          now <- getCurrentTime
          result <- try $ runSqlPool (do
            reserved <- reserveTicketInventory (toSqlKey 1) (toSqlKey 1) 1 now
            unless reserved (fail "Unexpected unavailable inventory")
            rawExecute "INSERT INTO event_ticket(id) VALUES (1)" []
            ) pool
          (result :: Either SomeException ()) `shouldSatisfy` isLeft
          runSqlPool (fmap (fmap M.eventTicketTierQuantitySold) (get (toSqlKey 1))) pool
            `shouldReturn` Just 1
      before_ (seed pool) $ describe "PostgreSQL admission authority" $ do
        let admit actor event ticket = do
              now <- getCurrentTime
              runSqlPool (admitTicket actor (toSqlKey event) ticket now) pool
            scan = admit "10" 1 (AdmissionByCode "TDF-AB12CD34EF56")
            status = fmap (M.eventTicketStatus . entityVal)
            deny action expected = fmap status action `shouldReturn` Left expected
        it "admits once under eight simultaneous scans and records exactly one audit" $ do
          results <- mapConcurrently (const scan) [1..8 :: Int]
          length (filter isRight results) `shouldBe` 1
          map status (filter isLeft results) `shouldBe` replicate 7 (Left AdmissionAlreadyUsed)
          runSqlPool (rawSql "SELECT count(*) FROM event_ticket_admission_audit" []) pool
            `shouldReturn` [Single (1 :: Int)]
        it "rejects outsider, unowned event, wrong event, invalid code and missing ticket" $ do
          deny (admit "11" 1 (AdmissionById (toSqlKey 1))) AdmissionForbidden
          deny (admit "10" 2 (AdmissionById (toSqlKey 1))) AdmissionForbidden
          deny (admit "10" 3 (AdmissionById (toSqlKey 1))) AdmissionNotFound
          deny (admit "10" 1 (AdmissionByCode "TDF-FFFFFFFFFFFF")) AdmissionNotFound
          deny (admit "10" 1 (AdmissionById (toSqlKey 999))) AdmissionNotFound
          owners <- runSqlPool (rawSql "SELECT organizer_party_id FROM social_event WHERE id=2" []) pool
          owners `shouldBe` [Single (Nothing :: Maybe Text)]
        it "rejects unpaid, refunded, cancelled and unknown states without admission" $ do
          forM_ [("pending", AdmissionUnpaid), ("refunded", AdmissionRefunded), ("cancelled", AdmissionCancelled)] $ \(value, expected) -> do
            runSqlPool (update (toSqlKey 1) [M.EventTicketOrderStatus =. value]) pool
            deny scan expected
          runSqlPool (update (toSqlKey 1) [M.EventTicketOrderStatus =. "paid"]) pool
          forM_ [("refunded", AdmissionRefunded), ("cancelled", AdmissionCancelled), ("expired", AdmissionInvalidState)] $ \(value, expected) -> do
            runSqlPool (update (toSqlKey 1) [M.EventTicketStatus =. value]) pool
            deny scan expected
          runSqlPool (rawSql "SELECT count(*) FROM event_ticket_admission_audit" []) pool
            `shouldReturn` [Single (0 :: Int)]
        it "denies disputed and reversed canonical payments despite a stale paid order" $ do
          runSqlPool (do
            rawExecute "INSERT INTO event_ticket_checkout_policy(id,event_id) VALUES ('11111111-1111-4111-a111-111111111111',1)" []
            rawExecute "INSERT INTO event_ticket_checkout_runtime VALUES (1,'11111111-1111-4111-a111-111111111111',1,'disputed')" []
            ) pool
          forM_ (["disputed", "chargeback", "refunded", "processing"] :: [Text]) $ \payment -> do
            runSqlPool (rawExecute "UPDATE event_ticket_checkout_runtime SET payment_status=?" [PersistText payment]) pool
            deny scan AdmissionUnpaid
          runSqlPool (rawSql "SELECT count(*) FROM event_ticket_admission_audit" []) pool
            `shouldReturn` [Single (0 :: Int)]
        forM_ ["1", "unmatched-capture:capture"] $ \internalReference ->
          it ("quarantines bound or late-bound external refunds: " <> T.unpack internalReference) $ do
            runSqlPool (do
              rawExecute "INSERT INTO event_ticket_checkout_policy(id,event_id) VALUES ('11111111-1111-4111-a111-111111111111',1)" []
              rawExecute "INSERT INTO event_ticket_checkout_runtime VALUES (1,'11111111-1111-4111-a111-111111111111',1,'paid','22222222-2222-4222-a222-222222222222')" []
              rawExecute "INSERT INTO commerce_payment_attempt VALUES ('33333333-3333-4333-a333-333333333333','22222222-2222-4222-a222-222222222222')" []
              rawExecute "INSERT INTO commerce_provider_binding VALUES ('33333333-3333-4333-a333-333333333333','paypal','sandbox','merchant','capture','capture')" []
              rawExecute "INSERT INTO commerce_reconciliation_exception VALUES ('paypal','sandbox','merchant','capture',?,'external_refund_detected','open')" [PersistText internalReference]
              ) pool
            deny scan AdmissionPaymentReview
            forM_ (["assigned", "resolved", "ignored"] :: [Text]) $ \reviewStatus -> do
              runSqlPool (rawExecute "UPDATE commerce_reconciliation_exception SET status=?" [PersistText reviewStatus]) pool
              deny scan AdmissionPaymentReview
            runSqlPool (rawSql "SELECT count(*) FROM event_ticket_admission_audit" []) pool
              `shouldReturn` [Single (0 :: Int)]
            runSqlPool (rawExecute "UPDATE commerce_reconciliation_exception SET exception_type='external_reversal_detected'" []) pool
            deny scan AdmissionPaymentReview
            runSqlPool (rawExecute "UPDATE commerce_reconciliation_exception SET merchant_account_ref='other'" []) pool
            fmap status scan `shouldReturn` Right "checked_in"

        it "rolls back admission when durable audit fails" $ do
          runSqlPool (rawExecute "INSERT INTO event_ticket_admission_audit VALUES (1,1,1,'10',now())" []) pool
          result <- try scan
          (result :: Either SomeException (Either AdmissionError (Entity M.EventTicket))) `shouldSatisfy` isLeft
          runSqlPool (fmap (fmap M.eventTicketStatus) (get (toSqlKey 1))) pool `shouldReturn` Just "issued"
        it "rejects a rotated old code and admits the replacement" $ do
          replacement <- newTicketCode
          runSqlPool (update (toSqlKey 1) [M.EventTicketCode =. replacement]) pool
          deny scan AdmissionNotFound
          fmap status (admit "10" 1 (AdmissionByCode replacement)) `shouldReturn` Right "checked_in"

      before_ (seed pool >> prepareTransfer pool) $ describe "PostgreSQL transfer authority" $ do
        let invite = do
              now <- getCurrentTime
              code <- newTicketCode
              let proposal = M.TicketTransfer (toSqlKey 1) (Just "20") Nothing
                    (Just "recipient@example.invalid") (Just "Recipient") "pending" code Nothing
                    (Just (addUTCTime 3600 now)) Nothing now now
              runSqlPool (Transfer.createTransfer "20" (toSqlKey 1) proposal now) pool
            accept actor code = do
              now <- getCurrentTime
              replacement <- newTicketCode
              runSqlPool (Transfer.acceptTransfer actor code replacement now) pool
            invitationCode result = case result of
              Right entity -> pure (M.ticketTransferTransferCode (entityVal entity))
              Left err -> fail (T.unpack err)
        it "allows one recipient under eight concurrent acceptances, revokes old QR and keeps the buyer out" $ do
          code <- invite >>= invitationCode
          results <- mapConcurrently (\actor -> accept (T.pack (show actor)) code) [30..37 :: Int]
          length (filter isRight results) `shouldBe` 1
          ticket <- runSqlPool (getJust (toSqlKey 1)) pool
          Transfer.retainedByBuyer ticket `shouldBe` False
          M.eventTicketCode ticket `shouldNotBe` "TDF-AB12CD34EF56"
          now <- getCurrentTime
          old <- runSqlPool (admitTicket "10" (toSqlKey 1) (AdmissionByCode "TDF-AB12CD34EF56") now) pool
          fmap (M.eventTicketStatus . entityVal) old `shouldBe` Left AdmissionNotFound
          new <- runSqlPool (admitTicket "10" (toSqlKey 1) (AdmissionByCode (M.eventTicketCode ticket)) now) pool
          new `shouldSatisfy` isRight
          runSqlPool (rawSql "SELECT count(*) FROM ticket_transfer WHERE status='completed' AND accepted_at IS NOT NULL AND to_party_id IS NOT NULL" []) pool
            `shouldReturn` [Single (1 :: Int)]
        it "creates only one pending invitation under contention" $ do
          results <- mapConcurrently (const invite) [1..8 :: Int]
          length (filter isRight results) `shouldBe` 1
        it "never overwrites a completed transfer with cancellation" $ do
          result <- invite
          code <- invitationCode result
          accepted <- accept "30" code
          accepted `shouldSatisfy` isRight
          now <- getCurrentTime
          case result of
            Right entity -> runSqlPool (Transfer.cancelTransfer "20" (entityKey entity) now) pool
              >>= (`shouldSatisfy` isLeft)
            Left err -> fail (T.unpack err)
        it "rejects cancelled, expired, used, unpaid and disabled transfers without changing the code" $ do
          forM_ (["cancelled", "expired", "used", "unpaid", "disabled"] :: [Text]) $ \scenario -> do
            seed pool
            prepareTransfer pool
            code <- invite >>= invitationCode
            runSqlPool (case scenario of
              "cancelled" -> updateWhere [] [M.TicketTransferStatus =. "cancelled"]
              "expired" -> rawExecute "UPDATE ticket_transfer SET expires_at=now()-interval '1 second'" []
              "used" -> update (toSqlKey 1) [M.EventTicketStatus =. "checked_in"]
              "unpaid" -> update (toSqlKey 1) [M.EventTicketOrderStatus =. "pending"]
              _ -> update (toSqlKey 1) [M.EventTicketTierAllowTransfers =. False]) pool
            accept "30" code >>= (`shouldSatisfy` isLeft)
            runSqlPool (fmap M.eventTicketCode (getJust (toSqlKey 1))) pool
              `shouldReturn` "TDF-AB12CD34EF56"
        it "rechecks the current holder and does not authorize a former holder's invitation" $ do
          code <- invite >>= invitationCode
          runSqlPool (update (toSqlKey 1) [M.EventTicketCurrentHolderPartyId =. Just "40"]) pool
          accept "30" code >>= (`shouldSatisfy` isLeft)
        it "enforces the versioned public checkout transfer policy and exact cutoff" $ do
          observed <- getCurrentTime
          let now = observed { utctDayTime = fromRational
                (fromInteger (floor (utctDayTime observed)) + 1234567 / 10000000) }
          runSqlPool (do
            rawExecute "INSERT INTO event_ticket_checkout_policy(id,event_id,transfer_allowed,transfer_deadline) VALUES ('11111111-1111-4111-a111-111111111111',1,false,?)" [PersistUTCTime (addUTCTime 1800 now)]
            rawExecute "INSERT INTO event_ticket_checkout_runtime VALUES (1,'11111111-1111-4111-a111-111111111111',1,'paid')" []
            ) pool
          invite >>= (`shouldSatisfy` isLeft)
          runSqlPool (rawExecute "UPDATE event_ticket_checkout_policy SET transfer_allowed=true" []) pool
          code <- invite >>= invitationCode
          runSqlPool (rawExecute "UPDATE event_ticket_checkout_policy SET transfer_deadline=?,approval_status='approved'" [PersistUTCTime now]) pool
          -- PostgreSQL rounds the deliberately finer-than-microsecond fixture above.
          -- Exercise the actual stored deadline, including both adjacent boundaries.
          [Single cutoff] <- runSqlPool (rawSql
            "SELECT transfer_deadline FROM event_ticket_checkout_policy WHERE id='11111111-1111-4111-a111-111111111111'" []) pool
          replacement <- newTicketCode
          let acceptAt instant = runSqlPool (Transfer.acceptTransfer "30" code replacement instant) pool
          acceptAt cutoff >>= (`shouldSatisfy` isLeft)
          acceptAt (addUTCTime 0.000001 cutoff) >>= (`shouldSatisfy` isLeft)
          acceptAt (addUTCTime (-0.000001) cutoff) >>= (`shouldSatisfy` isRight)
          result <- try $ runSqlPool (rawExecute "UPDATE event_ticket_checkout_policy SET transfer_deadline=NULL" []) pool
          (result :: Either SomeException ()) `shouldSatisfy` isLeft
        it "transfers retained tickets after partial refund but never revoked allocations" $ do
          runSqlPool (do
            rawExecute "INSERT INTO event_ticket_checkout_policy(id,event_id) VALUES ('11111111-1111-4111-a111-111111111111',1)" []
            rawExecute "INSERT INTO event_ticket_checkout_runtime VALUES (1,'11111111-1111-4111-a111-111111111111',1,'partially_refunded')" []
            ) pool
          code <- invite >>= invitationCode
          forM_ (["refund_pending", "refunded", "cancelled"] :: [Text]) $ \ticketState -> do
            runSqlPool (update (toSqlKey 1) [M.EventTicketStatus =. ticketState]) pool
            accept "30" code >>= (`shouldSatisfy` isLeft)
            runSqlPool (fmap M.eventTicketCode (getJust (toSqlKey 1))) pool
              `shouldReturn` "TDF-AB12CD34EF56"
          runSqlPool (update (toSqlKey 1) [M.EventTicketStatus =. "issued"]) pool
          accept "30" code >>= (`shouldSatisfy` isRight)
          runSqlPool (fmap M.eventTicketCurrentHolderPartyId (getJust (toSqlKey 1))) pool
            `shouldReturn` Just "30"
        it "rejects a disputed public payment even if the legacy order still says paid" $ do
          runSqlPool (do
            rawExecute "INSERT INTO event_ticket_checkout_policy(id,event_id) VALUES ('11111111-1111-4111-a111-111111111111',1)" []
            rawExecute "INSERT INTO event_ticket_checkout_runtime VALUES (1,'11111111-1111-4111-a111-111111111111',1,'disputed')" []
            ) pool
          invite >>= (`shouldSatisfy` isLeft)
        it "rolls back holder and invitation changes if code rotation violates uniqueness" $ do
          code <- invite >>= invitationCode
          runSqlPool (rawExecute "INSERT INTO event_ticket(id,event_id,tier_ref_id,order_ref_id,code,status,created_at,updated_at) VALUES(2,1,1,1,'TDF-FFFFFFFFFFFF','issued',now(),now())" []) pool
          now <- getCurrentTime
          result <- try $ runSqlPool (Transfer.acceptTransfer "30" code "TDF-FFFFFFFFFFFF" now) pool
          (result :: Either SomeException (Either Text (Entity M.EventTicket))) `shouldSatisfy` isLeft
          ticket <- runSqlPool (getJust (toSqlKey 1)) pool
          Transfer.retainedByBuyer ticket `shouldBe` True
          runSqlPool (rawSql "SELECT status FROM ticket_transfer" []) pool
            `shouldReturn` [Single ("pending" :: Text)]

prepareTransfer :: ConnectionPool -> IO ()
prepareTransfer pool = runSqlPool (do
  rawExecute "UPDATE social_event SET start_time=now()+interval '1 day' WHERE id=1" []
  update (toSqlKey 1) [M.EventTicketTierAllowTransfers =. True]
  update (toSqlKey 1) [M.EventTicketCurrentHolderPartyId =. Just "20", M.EventTicketOriginalHolderPartyId =. Just "20"]
  ) pool

seed :: ConnectionPool -> IO ()
seed pool = runSqlPool (do
  rawExecute "TRUNCATE commerce_reconciliation_exception,commerce_provider_binding,commerce_payment_attempt,event_ticket_checkout_runtime,event_ticket_checkout_policy,ticket_transfer,event_ticket_admission_audit,event_ticket,event_ticket_order,event_ticket_tier,social_event RESTART IDENTITY CASCADE" []
  rawExecute "INSERT INTO social_event(id,organizer_party_id,title,start_time,created_at,updated_at) VALUES (1,'10','Admission fixture',now(),now(),now()),(2,NULL,'Unowned',now(),now(),now()),(3,'10','Other event',now(),now(),now())" []
  rawExecute "INSERT INTO event_ticket_tier(id,event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active,created_at,updated_at) VALUES (1,1,'GA','General',2000,'USD',20,1,true,now(),now())" []
  rawExecute "INSERT INTO event_ticket_order(id,event_id,tier_id,quantity,amount_cents,currency,status,purchased_at,created_at,updated_at) VALUES (1,1,1,1,2000,'USD','paid',now(),now(),now())" []
  rawExecute "INSERT INTO event_ticket(id,event_id,tier_ref_id,order_ref_id,code,status,created_at,updated_at) VALUES (1,1,1,1,'TDF-AB12CD34EF56','issued',now(),now())" []
  ) pool
