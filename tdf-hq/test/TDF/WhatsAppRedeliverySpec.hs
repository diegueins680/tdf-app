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
import TDF.Server
  ( WhatsAppReceipt(..), claimWhatsAppAutoReply, receiveWhatsAppInbound
  , whatsAppCourseRegistrationAdmitted, whatsAppCourseRegistrationKey, whatsAppSentRequestNote )
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

        it "lets a redelivery finish the message whose registration filled the hourly cap" $
          \pool -> do
          phone <- newPhone
          third <- deliver pool phone "m3" "inscribirme"
          later <- deliver pool phone "m4" "inscribirme"
          let key = entityKey (warMessage third)
              admitted message = do
                now <- getCurrentTime
                runSqlPool (whatsAppCourseRegistrationAdmitted now phone message) pool
          admitted key `shouldReturn` True
          -- Three registrations in the hour, the third made by this message
          -- and committed; the process stopped before the reply was claimed.
          registrations <- runSqlPool (rawSql
            "INSERT INTO course_registration \
            \(course_slug,phone_e164,source,status,created_at,updated_at) \
            \SELECT 'redelivery-spec-course',?,'whatsapp','pending_payment',now(),now() \
            \FROM generate_series(1,3) RETURNING id" [PersistText phone]) pool
          let lastRegistration = maximum [ident | Single ident <- registrations] :: Int64
          runSqlPool (rawExecute
            "INSERT INTO identity_course_registration_request \
            \(request_scope,request_key,request_payload,registration_id) \
            \VALUES('whatsapp-course:redelivery-spec-course',?,'{}'::jsonb,?)"
            [PersistText (whatsAppCourseRegistrationKey key), PersistInt64 lastRegistration]) pool
          -- The redelivery is admitted and can claim the reply; a new message
          -- from the same sender stays refused by the cap.
          admitted key `shouldReturn` True
          admitted (entityKey (warMessage later)) `shouldReturn` False
          claim pool key `shouldReturn` True
          runSqlPool (do
            rawExecute "DELETE FROM identity_course_registration_request WHERE registration_id = ?"
              [PersistInt64 lastRegistration]
            rawExecute
              "DELETE FROM course_registration WHERE course_slug = 'redelivery-spec-course' \
              \AND phone_e164 = ?" [PersistText phone]) pool

        it "gives the automatic reply to exactly one of several simultaneous callers" $ \pool -> do
          phone <- newPhone
          receipt <- deliver pool phone "m1" "inscribirme"
          claims <- forConcurrently [1 .. 8 :: Int] $ \_ -> claim pool (entityKey (warMessage receipt))
          length (filter id claims) `shouldBe` 1

        it "applies the STOP of a message stored before this version when it is delivered again, once" $ \pool -> do
          phone <- newPhone
          subscribe pool phone
          _ <- storedBefore pool phone "m1" "STOP" Nothing
          again <- deliver pool phone "m1" "STOP"
          (warFirst again, warConfirmed again) `shouldBe` (False, False)
          consentOf pool phone `shouldReturn` [False]
          subscribe pool phone
          _ <- deliver pool phone "m1" "STOP"
          consentOf pool phone `shouldReturn` [True]

        it "confirms a pending request from a yes stored before this version, once" $ \pool -> do
          phone <- newPhone
          request pool phone
          _ <- storedBefore pool phone "m1" "Sí" Nothing
          again <- deliver pool phone "m1" "Sí"
          (warFirst again, warConfirmed again) `shouldBe` (False, True)
          third <- deliver pool phone "m1" "Sí"
          warConfirmed third `shouldBe` False
          consentOf pool phone `shouldReturn` [True]

        it "does not answer again a message answered before this version" $ \pool -> do
          phone <- newPhone
          now <- getCurrentTime
          answered <- storedBefore pool phone "m1" "inscribirme" (Just now)
          claim pool answered `shouldReturn` False
          unanswered <- storedBefore pool phone "m2" "inscribirme" Nothing
          claim pool unanswered `shouldReturn` True

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

-- A message as an earlier version stored it: no effects entry, no claim entry,
-- answered or not.
storedBefore :: ConnectionPool -> Text -> Text -> Text -> Maybe UTCTime -> IO (Key ME.WhatsAppMessage)
storedBefore pool phone label body repliedAt = do
  now <- getCurrentTime
  rows <- runSqlPool (rawSql
    "INSERT INTO whats_app_message(external_id,sender_id,phone_e164,text,direction,reply_status, \
    \attempt_count,replied_at,delivery_status,created_at) \
    \VALUES(?,?,?,?,'incoming',?,0,?,'received',?) RETURNING id"
    [ PersistText (externalIdFor phone label), PersistText phone, PersistText phone, PersistText body
    , PersistText (maybe "pending" (const "sent") repliedAt), toPersistValue repliedAt
    , toPersistValue (addUTCTime (-3600) now) ]) pool
  case rows of
    [Single key] -> pure key
    _ -> fail "The earlier-version message was not stored"

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
  setConsent pool phone True (Just now) Nothing Nothing

-- A delivered pending request made a minute ago: confirmation requires the
-- request note as well as the request instant (PRIV-WHATSAPP-001).
request :: ConnectionPool -> Text -> IO ()
request pool phone = do
  now <- getCurrentTime
  setConsent pool phone False Nothing (Just (addUTCTime (-60) now)) (Just whatsAppSentRequestNote)

setConsent :: ConnectionPool -> Text -> Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> IO ()
setConsent pool phone consent consentedAt requestedAt note = runSqlPool (rawExecute
  "INSERT INTO whats_app_consent(phone_e164,consent,consented_at,revoked_at,confirmation_requested_at,note,created_at,updated_at) \
  \VALUES(?,?,?,NULL,?,?,now(),now()) \
  \ON CONFLICT(phone_e164) DO UPDATE SET consent=excluded.consent,consented_at=excluded.consented_at, \
  \revoked_at=NULL,confirmation_requested_at=excluded.confirmation_requested_at,note=excluded.note,updated_at=now()"
  [PersistText phone, PersistBool consent, toPersistValue consentedAt, toPersistValue requestedAt, toPersistValue note]) pool

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
