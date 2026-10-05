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
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql
import System.Environment (getEnv)
import Test.Hspec
import qualified TDF.Models.SocialEventsModels as M
import TDF.Ticketing.Admission
import TDF.Ticketing.Inventory

main :: IO ()
main = do
  dsn <- getEnv "TICKET_ADMISSION_TEST_DSN"
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

seed :: ConnectionPool -> IO ()
seed pool = runSqlPool (do
  rawExecute "TRUNCATE event_ticket_admission_audit,event_ticket,event_ticket_order,event_ticket_tier,social_event RESTART IDENTITY CASCADE" []
  rawExecute "INSERT INTO social_event(id,organizer_party_id,title,start_time,created_at,updated_at) VALUES (1,'10','Admission fixture',now(),now(),now()),(2,NULL,'Unowned',now(),now(),now()),(3,'10','Other event',now(),now(),now())" []
  rawExecute "INSERT INTO event_ticket_tier(id,event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active,created_at,updated_at) VALUES (1,1,'GA','General',2000,'USD',20,1,true,now(),now())" []
  rawExecute "INSERT INTO event_ticket_order(id,event_id,tier_id,quantity,amount_cents,currency,status,purchased_at,created_at,updated_at) VALUES (1,1,1,1,2000,'USD','paid',now(),now(),now())" []
  rawExecute "INSERT INTO event_ticket(id,event_id,tier_ref_id,order_ref_id,code,status,created_at,updated_at) VALUES (1,1,1,1,'TDF-AB12CD34EF56','issued',now(),now())" []
  ) pool
