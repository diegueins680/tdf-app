{-# LANGUAGE OverloadedStrings #-}
module TDF.OperationsInboundCaptureSpec (spec) where

import Control.Exception (bracket, finally)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist (insert_)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, PersistValue(..), Single(..), rawExecute, rawSql, runSqlPool)
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Test.Hspec
import qualified TDF.Models as M
import TDF.ServerExtra
  ( IGInbound(..), IGInboundDeleted(..), MetaChannel(..), MetaInboundEvent(..), persistMetaInbound )
import TDF.WhatsApp.History (IncomingWhatsAppRecord(..), recordIncomingWhatsAppMessage)

-- Inbound messages are stored through the functions the webhooks call, so
-- the direction is the one the application writes, and the installed triggers
-- decide what reaches Operations. The outgoing and history rows are inserted
-- directly.
spec :: Spec
spec = describe "operations-inbound-capture-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_OPERATIONS_CAPTURE_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  used <- runIO (newIORef [])
  let newId = newIdIn used
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-operations-inbound-capture.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> aroundAll (withEnabledFixture connection used) $ do
        it "stores all three channels without capture while Operations is disabled" $ \pool -> do
          wa <- newId
          ig <- newId
          fb <- newId
          let mids = [wa, ig, fb]
          withEnabled pool False $ do
            whatsapp pool wa
            social pool MetaInstagram ig
            social pool MetaFacebook fb
          mapM_ (\mid -> events pool mid `shouldReturn` []) mids
          rows <- runSqlPool (rawSql
            "SELECT (SELECT count(*) FROM whats_app_message WHERE external_id=?) + \
            \(SELECT count(*) FROM instagram_message WHERE external_id=?) + \
            \(SELECT count(*) FROM facebook_message WHERE external_id=?)"
            (map PersistText mids)) pool
          rows `shouldBe` [Single (3 :: Int)]

        it "does not capture a disabled-period message on redelivery after enablement" $ \pool -> do
          mid <- newId
          withEnabled pool False (whatsapp pool mid)
          whatsapp pool mid
          events pool mid `shouldReturn` []

        it "records one event for an inbound WhatsApp message stored by the application" $ \pool -> do
          mid <- newId
          whatsapp pool mid
          events pool mid `shouldReturn` [("communication.whatsapp.received", "uncorrelated_inbound", "whatsapp")]

        it "records nothing more when the same WhatsApp message is delivered again" $ \pool -> do
          mid <- newId
          whatsapp pool mid
          whatsapp pool mid
          length <$> events pool mid `shouldReturn` 1

        it "records one event for an inbound Instagram message and one for a Facebook message" $ \pool -> do
          instagramId <- newId
          facebookId <- newId
          social pool MetaInstagram instagramId
          social pool MetaFacebook facebookId
          events pool instagramId `shouldReturn` [("communication.instagram.received", "uncorrelated_inbound", "instagram")]
          events pool facebookId `shouldReturn` [("communication.facebook.received", "uncorrelated_inbound", "facebook")]

        it "records nothing for an outgoing message" $ \pool -> do
          mid <- newId
          now <- getCurrentTime
          runSqlPool (insert_ (instagramRow mid "outgoing" now)) pool
          events pool mid `shouldReturn` []

        it "records nothing for a deletion that arrives before its message" $ \pool -> do
          mid <- newId
          now <- getCurrentTime
          runSqlPool (persistMetaInbound MetaInstagram now
            [MetaInboundDeleted (IGInboundDeleted mid sender Nothing Nothing)]) pool
          events pool mid `shouldReturn` []

        it "records nothing for a message imported from history" $ \pool -> do
          mid <- newId
          now <- getCurrentTime
          runSqlPool (insert_ (instagramRow mid "incoming" (addUTCTime (-3 * 86400) now))) pool
          events pool mid `shouldReturn` []

        it "records nothing for a recent message the backfill imported" $ \pool -> do
          mid <- newId
          now <- getCurrentTime
          -- The metadata the backfill writes, on a message sent a minute ago.
          let backfilled = (instagramRow mid "incoming" (addUTCTime (-60) now))
                { M.instagramMessageMetadata = Just
                    "{\"backfilled\":true,\"conversationId\":\"t_1\",\"source\":\"meta-graph\"}" }
          runSqlPool (insert_ backfilled) pool
          events pool mid `shouldReturn` []

        it "still records a recent message whose metadata is not a backfill marker" $ \pool -> do
          mid <- newId
          now <- getCurrentTime
          let webhook = (instagramRow mid "incoming" now)
                { M.instagramMessageMetadata =
                    Just "{\"backfilled\":false,\"recipient_id\":\"biz-1\"}" }
          runSqlPool (insert_ webhook) pool
          events pool mid `shouldReturn`
            [("communication.instagram.received", "uncorrelated_inbound", "instagram")]

        it "stores a message whose capture fails, without an event" $ \pool -> do
          mid <- newId
          now <- getCurrentTime
          -- An Operations key holds at most 320 characters; this sender exceeds it.
          let longSender = T.replicate 400 "9"
          runSqlPool (persistMetaInbound MetaInstagram now
            [MetaInboundMessage (IGInbound mid longSender Nothing "Hola" Nothing Nothing Nothing Nothing Nothing)]) pool
          rows <- runSqlPool (rawSql "SELECT count(*) FROM instagram_message WHERE external_id=?" [PersistText mid]) pool
          rows `shouldBe` [Single (1 :: Int)]
          events pool mid `shouldReturn` []

withEnabledFixture :: String -> IORef [Text] -> ActionWith ConnectionPool -> IO ()
withEnabledFixture connection used action = bracket
  (runNoLoggingT (createPostgresqlPool (BS.pack connection) 4))
  destroyAllResources $ \pool ->
    withEnabled pool True (action pool `finally` forget used pool)

withEnabled :: ConnectionPool -> Bool -> IO a -> IO a
withEnabled pool enabled action = bracket acquire restore (const action)
  where
    acquire = do
      rows <- runSqlPool (rawSql
        "SELECT operations_enabled FROM operations_organization \
        \WHERE id='00000000-0000-4000-8000-000000000001'::uuid" []) pool
      case rows of
        [Single previous] -> setEnabled enabled >> pure previous
        _ -> fail "The isolated fixture requires the default Operations organization"
    restore = setEnabled
    setEnabled value = runSqlPool (rawExecute
      "UPDATE operations_organization SET operations_enabled=? \
      \WHERE id='00000000-0000-4000-8000-000000000001'::uuid" [PersistBool value]) pool

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any local
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres:postgres@postgres:5432/"]
  where
    -- The accepted prefix must name the host: nothing after it may introduce
    -- another authority ("postgresql://localhost:secret@db.example/x_test"
    -- has localhost as the user name and db.example as the host).
    local prefix = prefix `isPrefixOf` value && '@' `notElem` drop (length prefix) value

newIdIn :: IORef [Text] -> IO Text
newIdIn used = do
  suffix <- randomRIO (100000000, 999999999 :: Int)
  let mid = "capture-spec." <> T.pack (show suffix)
  modifyIORef' used (mid :)
  pure mid

sender :: Text
sender = "+593973990001"

whatsapp :: ConnectionPool -> Text -> IO ()
whatsapp pool mid = do
  now <- getCurrentTime
  _ <- runSqlPool (recordIncomingWhatsAppMessage now IncomingWhatsAppRecord
    { iwrExternalId = mid
    , iwrSenderId = sender
    , iwrSenderName = Nothing
    , iwrText = "Hola"
    , iwrAdExternalId = Nothing
    , iwrAdName = Nothing
    , iwrCampaignExternalId = Nothing
    , iwrCampaignName = Nothing
    , iwrMetadata = Nothing
    , iwrTransportPayload = Nothing
    , iwrSource = Just "whatsapp_webhook"
    }) pool
  pure ()

social :: ConnectionPool -> MetaChannel -> Text -> IO ()
social pool channel mid = do
  now <- getCurrentTime
  runSqlPool (persistMetaInbound channel now
    [MetaInboundMessage (IGInbound mid sender Nothing "Hola" Nothing Nothing Nothing Nothing Nothing)]) pool

instagramRow :: Text -> Text -> UTCTime -> M.InstagramMessage
instagramRow mid direction createdAt =
  M.InstagramMessage mid sender Nothing (Just "Hola") direction Nothing Nothing Nothing Nothing Nothing
    "pending" Nothing Nothing Nothing 0 Nothing Nothing Nothing Nothing createdAt

-- Event type, aggregate type and source system of the events of one message.
events :: ConnectionPool -> Text -> IO [(Text, Text, Text)]
events pool mid = do
  rows <- runSqlPool (rawSql
    "SELECT event_type, aggregate_type, source_system FROM operations_domain_event WHERE provider_event_id=?"
    [PersistText mid]) pool
  pure [(kind, aggregate, source) | (Single kind, Single aggregate, Single source) <- rows]

-- Removes only what this run created. Events are immutable by trigger, so the
-- removal runs with triggers off, in its own transaction.
forget :: IORef [Text] -> ConnectionPool -> IO ()
forget used pool = do
  mids <- readIORef used
  runSqlPool (do
    rawExecute "SET LOCAL session_replication_role = replica" []
    mapM_ (\mid -> do
      let key = [PersistText mid]
      rawExecute "DELETE FROM operations_work_item_event WHERE domain_event_id IN (SELECT id FROM operations_domain_event WHERE provider_event_id=?)" key
      rawExecute "DELETE FROM operations_outbox WHERE event_id IN (SELECT id FROM operations_domain_event WHERE provider_event_id=?)" key
      rawExecute "DELETE FROM operations_domain_event WHERE provider_event_id=?" key
      rawExecute "DELETE FROM audit_log WHERE entity='whatsapp_message' AND entity_id IN (SELECT id::text FROM whats_app_message WHERE external_id=?)" key
      rawExecute "DELETE FROM whats_app_message WHERE external_id=?" key
      rawExecute "DELETE FROM instagram_message WHERE external_id=?" key
      rawExecute "DELETE FROM facebook_message WHERE external_id=?" key) mids) pool
