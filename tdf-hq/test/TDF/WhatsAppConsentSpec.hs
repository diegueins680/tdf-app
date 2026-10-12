{-# LANGUAGE OverloadedStrings #-}
module TDF.WhatsAppConsentSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (mapConcurrently, withAsync, wait)
import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist (Entity(..), getBy, toPersistValue)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Test.Hspec
import TDF.CampaignAutomation (applyWhatsAppCampaignOptOut)
import qualified TDF.ModelsExtra as ME
import TDF.Server
  ( applyWhatsAppConsentConfirmation
  , claimWhatsAppConfirmationRequest
  , markWhatsAppConfirmationRequestUnsent
  )

-- PRIV-WHATSAPP-001 against PostgreSQL row locking and timestamp precision.
-- No provider request is made; the database is the fully migrated synthetic
-- HTTP fixture and every case uses its own random number.
spec :: Spec
spec = describe "whatsapp-consent-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_WHATSAPP_CONSENT_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-whatsapp-consent.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 8)) $
      afterAll destroyAllResources $ do
        it "lets exactly one of several simultaneous requests claim the confirmation message" $ \pool -> do
          phone <- freshPhone
          now <- getCurrentTime
          claims <- within $ mapConcurrently
            (\offset -> runSqlPool (claim (addUTCTime offset now) phone) pool)
            [0, 1, 2, 3, 4, 5]
          length (filter id claims) `shouldBe` 1

        it "activates consent once when the same reply is delivered twice at once" $ \pool -> do
          phone <- freshPhone
          now <- getCurrentTime
          runSqlPool (claim now phone) pool `shouldReturn` True
          let replyAt = addUTCTime 5 now
          confirmations <- within $ mapConcurrently
            (const (runSqlPool (applyWhatsAppConsentConfirmation replyAt (Just replyAt) phone) pool))
            [(), ()]
          length (filter id confirmations) `shouldBe` 1
          consentOf pool phone `shouldReturn` Just True

        it "lets a withdrawal that holds the row win over a waiting confirmation" $ \pool -> do
          phone <- freshPhone
          now <- getCurrentTime
          runSqlPool (claim now phone) pool `shouldReturn` True
          ready <- newEmptyMVar
          release <- newEmptyMVar
          label <- observerLabel
          let replyAt = addUTCTime 5 now
          withAsync (runSqlPool (do
            applyWhatsAppCampaignOptOut replyAt phone Nothing
            -- A caller-supplied opt-out reason may equal the pending marker;
            -- only the withdrawal instant may decide.
            forgeNote "pending_confirmation" phone
            liftIO (putMVar ready () >> takeMVar release)) pool) $ \withdrawing ->
            (do
              within (takeMVar ready)
              withAsync (runSqlPool
                (observeAs label >> applyWhatsAppConsentConfirmation replyAt (Just replyAt) phone) pool) $ \confirming ->
                (do
                  waitBlocked pool label
                  putMVar release ()
                  within (wait withdrawing)
                  within (wait confirming) `shouldReturn` False
                  consentOf pool phone `shouldReturn` Just False
                ) `finally` void (tryPutMVar release ())
              ) `finally` void (tryPutMVar release ())

        it "treats a withdrawal whose clock was read before the request as later than it" $ \pool -> do
          phone <- freshPhone
          now <- getCurrentTime
          let replyAt = addUTCTime 60 now
          runSqlPool (claim now phone) pool `shouldReturn` True
          runSqlPool (applyWhatsAppCampaignOptOut (addUTCTime (-5) now) phone Nothing) pool
          runSqlPool (forgeNote "pending_confirmation" phone) pool
          runSqlPool (applyWhatsAppConsentConfirmation replyAt (Just replyAt) phone) pool `shouldReturn` False
          runSqlPool (forgeNote "pending_unsent" phone) pool
          runSqlPool (claim (addUTCTime 30 now) phone) pool `shouldReturn` False
          runSqlPool (applyWhatsAppConsentConfirmation replyAt (Just replyAt) phone) pool `shouldReturn` False

        it "keeps an undelivered request confirmable by the number and briefly open to a retry" $ \pool -> do
          phone <- freshPhone
          now <- getCurrentTime
          Just requestedAt <- runSqlPool (claimWhatsAppConfirmationRequest now phone Nothing (Just "public")) pool
          -- The stored instant is rounded to microseconds; the failure is bound to it.
          runSqlPool (markWhatsAppConfirmationRequestUnsent requestedAt phone) pool
          noteOf pool phone `shouldReturn` Just (Just "pending_unsent")
          -- A retry keeps the request instant, so it cannot extend the window.
          runSqlPool (claimWhatsAppConfirmationRequest (addUTCTime 60 now) phone Nothing (Just "public")) pool
            `shouldReturn` Just requestedAt
          noteOf pool phone `shouldReturn` Just (Just "pending_confirmation")
          runSqlPool (claim (addUTCTime 90 now) phone) pool `shouldReturn` False
          runSqlPool (markWhatsAppConfirmationRequestUnsent requestedAt phone) pool
          let replyAt = addUTCTime 120 now
          -- A reply older than the request cannot confirm it.
          runSqlPool (applyWhatsAppConsentConfirmation replyAt (Just (addUTCTime (-10) now)) phone) pool
            `shouldReturn` False
          runSqlPool (applyWhatsAppConsentConfirmation replyAt (Just replyAt) phone) pool `shouldReturn` True
          consentOf pool phone `shouldReturn` Just True

        it "closes an undelivered request to replies and retries once its short window has passed" $ \pool -> do
          phone <- freshPhone
          now <- getCurrentTime
          runSqlPool (claim now phone) pool `shouldReturn` True
          runSqlPool (markWhatsAppConfirmationRequestUnsent now phone) pool
          let late = addUTCTime 3700 now
          runSqlPool (applyWhatsAppConsentConfirmation late (Just late) phone) pool `shouldReturn` False
          runSqlPool (claim late phone) pool `shouldReturn` False
          consentOf pool phone `shouldReturn` Just False
          runSqlPool (claim (addUTCTime (24 * 3600 + 60) now) phone) pool `shouldReturn` True

        it "does not let a superseded attempt's failure change a newer request, a consent or a withdrawal" $ \pool -> do
          phone <- freshPhone
          now <- getCurrentTime
          let second = addUTCTime (24 * 3600 + 60) now
              replyAt = addUTCTime 120 second
          -- The first attempt's outcome is still unknown when the interval ends.
          runSqlPool (claim now phone) pool `shouldReturn` True
          runSqlPool (claim second phone) pool `shouldReturn` True
          runSqlPool (markWhatsAppConfirmationRequestUnsent now phone) pool
          noteOf pool phone `shouldReturn` Just (Just "pending_confirmation")
          runSqlPool (claim (addUTCTime 30 second) phone) pool `shouldReturn` False
          runSqlPool (applyWhatsAppConsentConfirmation replyAt (Just replyAt) phone) pool `shouldReturn` True
          runSqlPool (markWhatsAppConfirmationRequestUnsent second phone) pool
          consentOf pool phone `shouldReturn` Just True
          runSqlPool (applyWhatsAppCampaignOptOut (addUTCTime 180 second) phone Nothing) pool
          runSqlPool (markWhatsAppConfirmationRequestUnsent second phone) pool
          runSqlPool (applyWhatsAppConsentConfirmation (addUTCTime 200 second) (Just (addUTCTime 200 second)) phone) pool
            `shouldReturn` False
          consentOf pool phone `shouldReturn` Just False

        it "does not let a withdrawal reopen the request interval or leave the old request confirmable" $ \pool -> do
          phone <- freshPhone
          now <- getCurrentTime
          let replyAt = addUTCTime 120 now
              nextDay = addUTCTime (24 * 3600 + 60) now
          runSqlPool (claim now phone) pool `shouldReturn` True
          runSqlPool (applyWhatsAppCampaignOptOut (addUTCTime 30 now) phone Nothing) pool
          runSqlPool (claim (addUTCTime 60 now) phone) pool `shouldReturn` False
          runSqlPool (applyWhatsAppConsentConfirmation replyAt (Just replyAt) phone) pool `shouldReturn` False
          -- Marking the withdrawn request undelivered must not reopen it either.
          runSqlPool (forgeNote "pending_unsent" phone) pool
          runSqlPool (claim (addUTCTime 90 now) phone) pool `shouldReturn` False
          runSqlPool (applyWhatsAppConsentConfirmation replyAt (Just replyAt) phone) pool `shouldReturn` False
          -- After the interval a new request supersedes the withdrawal.
          runSqlPool (claim nextDay phone) pool `shouldReturn` True
          runSqlPool (applyWhatsAppConsentConfirmation (addUTCTime 60 nextDay) (Just (addUTCTime 60 nextDay)) phone) pool
            `shouldReturn` True
          consentOf pool phone `shouldReturn` Just True
          -- A confirmed number that withdraws cannot be asked again inside the interval.
          runSqlPool (applyWhatsAppCampaignOptOut (addUTCTime 120 nextDay) phone Nothing) pool
          runSqlPool (claim (addUTCTime 180 nextDay) phone) pool `shouldReturn` False

claim :: UTCTime -> Text -> SqlPersistT IO Bool
claim at phone = (/= Nothing) <$> claimWhatsAppConfirmationRequest at phone Nothing (Just "public")

forgeNote :: Text -> Text -> SqlPersistT IO ()
forgeNote note phone =
  rawExecute "UPDATE whats_app_consent SET note=? WHERE phone_e164=?" [toPersistValue note, toPersistValue phone]

consentOf :: ConnectionPool -> Text -> IO (Maybe Bool)
consentOf pool phone =
  fmap (ME.whatsAppConsentConsent . entityVal) <$> runSqlPool (getBy (ME.UniqueWhatsAppConsent phone)) pool

noteOf :: ConnectionPool -> Text -> IO (Maybe (Maybe Text))
noteOf pool phone =
  fmap (ME.whatsAppConsentNote . entityVal) <$> runSqlPool (getBy (ME.UniqueWhatsAppConsent phone)) pool

-- +999 is not an assigned country code, so no case can touch a real number.
freshPhone :: IO Text
freshPhone = do
  suffix <- randomRIO (100000000000, 999999999999 :: Integer)
  pure ("+999" <> T.pack (show suffix))

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres:postgres@postgres:5432/"]

within :: IO a -> IO a
within action = timeout 20000000 action >>= maybe (fail "WhatsApp consent barrier timed out") pure

-- PostgreSQL truncates application_name at 63 bytes. Keep the entire unique
-- UUID inside that bound so overlapping test runs cannot satisfy our barrier.
observerLabel :: IO Text
observerLabel = ("tdf_waconsent_" <>) . UUID.toText <$> nextRandom

observeAs :: Text -> SqlPersistT IO ()
observeAs label = do
  _ <- rawSql "SELECT set_config('application_name', ?, true)"
    [toPersistValue label] :: SqlPersistT IO [Single Text]
  pure ()

waitBlocked :: ConnectionPool -> Text -> IO ()
waitBlocked pool label = within loop
  where
    loop = do
      rows <- runSqlPool (rawSql
        "SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND application_name=? AND wait_event_type='Lock'"
        [toPersistValue label] :: SqlPersistT IO [Single Int64]) pool
      if rows == [Single 1] then pure () else threadDelay 10000 >> loop
