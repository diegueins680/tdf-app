{-# LANGUAGE OverloadedStrings #-}
module TDF.WhatsAppRedeliverySpec (spec) where

import Control.Concurrent.Async (forConcurrently)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist (Entity(..), Key, PersistValue(..), toPersistValue)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import Test.Hspec
import TDF.Server (WhatsAppReceipt(..), claimWhatsAppAutoReply, receiveWhatsAppInbound)
import qualified TDF.ModelsExtra as ME
import TDF.WhatsApp.History (IncomingWhatsAppRecord(..))

-- Deliveries are given to the function the webhook calls after it has checked
-- the signature and decoded the body. No message is sent anywhere.
spec :: Spec
spec = describe "whatsapp-redelivery-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_WHATSAPP_REDELIVERY_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  used <- runIO (newIORef [])
  let newPhone = newPhoneIn used
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-whatsapp-redelivery.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 10)) $
      afterAll (\pool -> forget used pool >> destroyAllResources pool) $ do
        it "withdraws consent on the first delivery of STOP" $ \pool -> do
          phone <- newPhone
          subscribe pool phone
          receipt <- deliver pool phone "m1" "STOP"
          warFirst receipt `shouldBe` True
          consentOf pool phone `shouldReturn` [False]

        it "leaves a later subscription alone when the same STOP is delivered again" $ \pool -> do
          phone <- newPhone
          subscribe pool phone
          _ <- deliver pool phone "m1" "STOP"
          subscribe pool phone
          again <- deliver pool phone "m1" "STOP"
          warFirst again `shouldBe` False
          consentOf pool phone `shouldReturn` [True]

        it "still withdraws consent for a new STOP after a redelivered one" $ \pool -> do
          phone <- newPhone
          subscribe pool phone
          _ <- deliver pool phone "m1" "STOP"
          subscribe pool phone
          _ <- deliver pool phone "m1" "STOP"
          fresh <- deliver pool phone "m2" "stop"
          warFirst fresh `shouldBe` True
          consentOf pool phone `shouldReturn` [False]

        it "stores a redelivered message and its audit entry once" $ \pool -> do
          phone <- newPhone
          first <- deliver pool phone "m1" "Hola, inscribirme"
          again <- deliver pool phone "m1" "Hola, inscribirme"
          (warFirst first, warFirst again) `shouldBe` (True, False)
          stored pool phone "m1" `shouldReturn` (1, 1)

        it "admits exactly one of several simultaneous deliveries of one message" $ \pool -> do
          phone <- newPhone
          subscribe pool phone
          receipts <- forConcurrently [1 .. 8 :: Int] $ \_ -> deliver pool phone "m1" "STOP"
          length (filter warFirst receipts) `shouldBe` 1
          stored pool phone "m1" `shouldReturn` (1, 1)
          consentOf pool phone `shouldReturn` [False]

        it "confirms a pending request once and reports it only on the first delivery" $ \pool -> do
          phone <- newPhone
          request pool phone
          first <- deliver pool phone "m1" "Sí"
          again <- deliver pool phone "m1" "Sí"
          (warFirst first, warConfirmed first) `shouldBe` (True, True)
          (warFirst again, warConfirmed again) `shouldBe` (False, False)
          consentOf pool phone `shouldReturn` [True]

        it "does not confirm again when an earlier yes is redelivered after a withdrawal and a new request" $ \pool -> do
          phone <- newPhone
          request pool phone
          _ <- deliver pool phone "m1" "Sí"
          _ <- deliver pool phone "m2" "STOP"
          request pool phone
          again <- deliver pool phone "m1" "Sí"
          warConfirmed again `shouldBe` False
          consentOf pool phone `shouldReturn` [False]

        it "gives the automatic reply of a message to one caller only" $ \pool -> do
          phone <- newPhone
          receipt <- deliver pool phone "m1" "inscribirme"
          let key = entityKey (warMessage receipt)
          claim pool key `shouldReturn` True
          claim pool key `shouldReturn` False
          other <- deliver pool phone "m2" "inscribirme"
          claim pool (entityKey (warMessage other)) `shouldReturn` True

        it "gives the automatic reply to exactly one of several simultaneous callers" $ \pool -> do
          phone <- newPhone
          receipt <- deliver pool phone "m1" "inscribirme"
          claims <- forConcurrently [1 .. 8 :: Int] $ \_ -> claim pool (entityKey (warMessage receipt))
          length (filter id claims) `shouldBe` 1

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres:postgres@postgres:5432/"]

-- Synthetic numbers in one block, remembered for removal.
newPhoneIn :: IORef [Text] -> IO Text
newPhoneIn used = do
  suffix <- randomRIO (0, 999999 :: Int)
  let digits = T.pack (show suffix)
      phone = "+593973" <> T.replicate (6 - T.length digits) "0" <> digits
  modifyIORef' used (phone :)
  pure phone

externalIdFor :: Text -> Text -> Text
externalIdFor phone label = "wamid.redelivery-spec." <> T.drop 1 phone <> "." <> label

-- A delivery carries the time the message was sent, which a redelivery
-- repeats. Each label stands for one message sent a fixed time ago.
deliver :: ConnectionPool -> Text -> Text -> Text -> IO WhatsAppReceipt
deliver pool phone label body = do
  now <- getCurrentTime
  sentAt <- sentTime phone label now
  runSqlPool (receiveWhatsAppInbound now (Just sentAt) IncomingWhatsAppRecord
    { iwrExternalId = externalIdFor phone label
    , iwrSenderId = phone
    , iwrSenderName = Nothing
    , iwrText = body
    , iwrAdExternalId = Nothing
    , iwrAdName = Nothing
    , iwrCampaignExternalId = Nothing
    , iwrCampaignName = Nothing
    , iwrMetadata = Nothing
    , iwrTransportPayload = Nothing
    , iwrSource = Just "whatsapp_webhook"
    }) pool

-- The sent time of a message is the time of its first delivery.
sentTime :: Text -> Text -> UTCTime -> IO UTCTime
sentTime phone label now = do
  known <- lookup (phone, label) <$> readIORef sentTimes
  case known of
    Just sentAt -> pure sentAt
    Nothing -> modifyIORef' sentTimes (((phone, label), now) :) >> pure now

{-# NOINLINE sentTimes #-}
sentTimes :: IORef [((Text, Text), UTCTime)]
sentTimes = unsafePerformIO (newIORef [])

claim :: ConnectionPool -> Key ME.WhatsAppMessage -> IO Bool
claim pool key = do
  now <- getCurrentTime
  runSqlPool (claimWhatsAppAutoReply now key) pool

-- Consent as it stands after a completed subscription.
subscribe :: ConnectionPool -> Text -> IO ()
subscribe pool phone = do
  now <- getCurrentTime
  setConsent pool phone True (Just now) Nothing

-- A pending request made a minute ago.
request :: ConnectionPool -> Text -> IO ()
request pool phone = do
  now <- getCurrentTime
  setConsent pool phone False Nothing (Just (addUTCTime (-60) now))

setConsent :: ConnectionPool -> Text -> Bool -> Maybe UTCTime -> Maybe UTCTime -> IO ()
setConsent pool phone consent consentedAt requestedAt = runSqlPool (rawExecute
  "INSERT INTO whats_app_consent(phone_e164,consent,consented_at,revoked_at,confirmation_requested_at,created_at,updated_at) \
  \VALUES(?,?,?,NULL,?,now(),now()) \
  \ON CONFLICT(phone_e164) DO UPDATE SET consent=excluded.consent,consented_at=excluded.consented_at, \
  \revoked_at=NULL,confirmation_requested_at=excluded.confirmation_requested_at,updated_at=now()"
  [PersistText phone, PersistBool consent, toPersistValue consentedAt, toPersistValue requestedAt]) pool

consentOf :: ConnectionPool -> Text -> IO [Bool]
consentOf pool phone = do
  rows <- runSqlPool (rawSql "SELECT consent FROM whats_app_consent WHERE phone_e164=?" [PersistText phone]) pool
  pure [value | Single value <- rows]

-- Stored copies of one message and "received" audit entries for it.
stored :: ConnectionPool -> Text -> Text -> IO (Int64, Int64)
stored pool phone label = do
  rows <- runSqlPool (rawSql
    "SELECT (SELECT count(*) FROM whats_app_message WHERE external_id=?), \
    \(SELECT count(*) FROM audit_log a JOIN whats_app_message m ON a.entity_id=m.id::text \
    \ WHERE a.entity='whatsapp_message' AND a.action='received' AND m.external_id=?)"
    [PersistText (externalIdFor phone label), PersistText (externalIdFor phone label)]) pool
  case rows of
    [(Single messages, Single audits)] -> pure (messages, audits)
    _ -> fail "Stored message counts were not a single row"

-- Removes only what the synthetic numbers of this run created.
forget :: IORef [Text] -> ConnectionPool -> IO ()
forget used pool = do
  phones <- readIORef used
  runSqlPool (mapM_ (\phone -> do
    let like = PersistText ("wamid.redelivery-spec." <> T.drop 1 phone <> ".%")
    rawExecute
      "DELETE FROM audit_log WHERE entity='whatsapp_message' AND entity_id IN \
      \(SELECT id::text FROM whats_app_message WHERE external_id LIKE ?)" [like]
    rawExecute "DELETE FROM whats_app_message WHERE external_id LIKE ?" [like]
    rawExecute "DELETE FROM whats_app_consent WHERE phone_e164=?" [PersistText phone]) phones) pool
