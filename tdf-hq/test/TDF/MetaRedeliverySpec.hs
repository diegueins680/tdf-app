{-# LANGUAGE OverloadedStrings #-}
module TDF.MetaRedeliverySpec (spec) where

import Control.Concurrent.Async (forConcurrently)
import Control.Exception (try)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist (Entity(..), getBy, insert_, update, (=.))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, PersistValue(..), rawExecute, runSqlPool)
import Database.PostgreSQL.Simple (SqlError)
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Test.Hspec
import qualified TDF.Models as M
import qualified TDF.ModelsExtra as ME
import TDF.ServerExtra
  ( IGInbound(..), IGInboundDeleted(..), MetaChannel(..), MetaInboundEvent(..), persistMetaInbound )

-- Deliveries are given to the function both webhooks call after they have
-- checked the signature and decoded the body. Nothing is sent anywhere.
spec :: Spec
spec = describe "meta-redelivery-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_META_REDELIVERY_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  used <- runIO (newIORef [])
  let newId = newIdIn used
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-meta-redelivery.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 10)) $
      afterAll (\pool -> forget used pool >> destroyAllResources pool) $ do
        it "leaves a message on hold on hold when it is delivered again" $ \pool -> do
          mid <- newId
          deliver pool MetaInstagram mid "Necesito hablar con una persona"
          runSqlPool (setInstagram mid
            [ M.InstagramMessageReplyStatus =. "hold"
            , M.InstagramMessageHoldReason =. Just "needs_human" ]) pool
          deliver pool MetaInstagram mid "Necesito hablar con una persona"
          row <- instagram pool mid
          (M.instagramMessageReplyStatus row, M.instagramMessageHoldReason row)
            `shouldBe` ("hold", Just "needs_human")

        it "leaves an answered message answered when it is delivered again" $ \pool -> do
          mid <- newId
          now <- getCurrentTime
          deliver pool MetaInstagram mid "hola"
          runSqlPool (setInstagram mid
            [ M.InstagramMessageReplyStatus =. "sent"
            , M.InstagramMessageRepliedAt =. Just now ]) pool
          deliver pool MetaInstagram mid "hola"
          row <- instagram pool mid
          M.instagramMessageReplyStatus row `shouldBe` "sent"
          M.instagramMessageRepliedAt row `shouldSatisfy` (/= Nothing)

        it "leaves a failed reply failed when the message is delivered again" $ \pool -> do
          mid <- newId
          deliver pool MetaInstagram mid "hola"
          runSqlPool (setInstagram mid [M.InstagramMessageReplyStatus =. "error"]) pool
          deliver pool MetaInstagram mid "hola"
          M.instagramMessageReplyStatus <$> instagram pool mid `shouldReturn` "error"

        it "keeps the stored text when the same message arrives with other text" $ \pool -> do
          mid <- newId
          deliver pool MetaInstagram mid "hola"
          deliver pool MetaInstagram mid "hola de nuevo"
          M.instagramMessageText <$> instagram pool mid `shouldReturn` Just "hola"

        it "does not turn a stored outgoing message into an incoming one" $ \pool -> do
          mid <- newId
          now <- getCurrentTime
          runSqlPool (insert_ (instagramRow mid "Gracias por escribir" "outgoing" "sent" now)) pool
          deliver pool MetaInstagram mid "Gracias por escribir"
          row <- instagram pool mid
          (M.instagramMessageDirection row, M.instagramMessageReplyStatus row) `shouldBe` ("outgoing", "sent")

        it "keeps a deleted message deleted, with its text and first deletion time, when it is delivered again" $ \pool -> do
          mid <- newId
          deliver pool MetaInstagram mid "hola"
          delete pool MetaInstagram mid
          first <- instagram pool mid
          deliver pool MetaInstagram mid "hola de nuevo"
          delete pool MetaInstagram mid
          again <- instagram pool mid
          M.instagramMessageDeletedAt first `shouldSatisfy` (/= Nothing)
          M.instagramMessageDeletedAt again `shouldBe` M.instagramMessageDeletedAt first
          M.instagramMessageText again `shouldBe` Just "hola"

        it "records a deletion that arrives before its message and stores no text afterwards" $ \pool -> do
          mid <- newId
          delete pool MetaInstagram mid
          deliver pool MetaInstagram mid "hola"
          row <- instagram pool mid
          M.instagramMessageDeletedAt row `shouldSatisfy` (/= Nothing)
          M.instagramMessageText row `shouldBe` Nothing

        it "fills a missing sender name and keeps a stored one" $ \pool -> do
          mid <- newId
          deliverAs pool MetaInstagram mid "hola" Nothing
          deliverAs pool MetaInstagram mid "hola" (Just "Fan")
          deliverAs pool MetaInstagram mid "hola" (Just "Otro nombre")
          M.instagramMessageSenderName <$> instagram pool mid `shouldReturn` Just "Fan"

        it "applies the same rule to Facebook messages" $ \pool -> do
          mid <- newId
          deliver pool MetaFacebook mid "hola"
          runSqlPool (do
            Just (Entity key _) <- getBy (ME.UniqueFacebookMessage mid)
            update key [ME.FacebookMessageReplyStatus =. "hold"]) pool
          deliver pool MetaFacebook mid "hola de nuevo"
          Just (Entity _ row) <- runSqlPool (getBy (ME.UniqueFacebookMessage mid)) pool
          (ME.facebookMessageReplyStatus row, ME.facebookMessageText row) `shouldBe` ("hold", Just "hola")

        it "lets metadata gain the receiving account once and then keeps it" $ \pool -> do
          mid <- newId
          deliverWith pool MetaInstagram mid "hola" Nothing (Just "{\"backfilled\":true}")
          deliverWith pool MetaInstagram mid "hola" Nothing (Just "{\"recipient_id\":\"biz-1\"}")
          deliverWith pool MetaInstagram mid "hola" Nothing (Just "{\"recipient_id\":\"biz-2\"}")
          deliverWith pool MetaInstagram mid "hola" Nothing (Just "{\"backfilled\":true}")
          M.instagramMessageMetadata <$> instagram pool mid `shouldReturn` Just "{\"recipient_id\":\"biz-1\"}"

        it "stores one message, and fails no delivery, when several first deliveries arrive at once" $ \pool -> do
          mid <- newId
          outcomes <- forConcurrently [1 .. 8 :: Int] $ \_ ->
            try (deliver pool MetaInstagram mid "hola") :: IO (Either SqlError ())
          length [() | Right () <- outcomes] `shouldBe` 8
          row <- instagram pool mid
          (M.instagramMessageText row, M.instagramMessageReplyStatus row) `shouldBe` (Just "hola", "pending")

        it "keeps the first deletion time, and fails no deletion, when several deletions arrive at once" $ \pool -> do
          mid <- newId
          deliver pool MetaInstagram mid "hola"
          first <- forConcurrently [1 .. 8 :: Int] $ \_ ->
            try (delete pool MetaInstagram mid) :: IO (Either SqlError ())
          length [() | Right () <- first] `shouldBe` 8
          deletedAt <- M.instagramMessageDeletedAt <$> instagram pool mid
          deletedAt `shouldSatisfy` (/= Nothing)
          later <- forConcurrently [1 .. 8 :: Int] $ \_ ->
            try (delete pool MetaInstagram mid) :: IO (Either SqlError ())
          length [() | Right () <- later] `shouldBe` 8
          row <- instagram pool mid
          (M.instagramMessageDeletedAt row, M.instagramMessageText row) `shouldBe` (deletedAt, Just "hola")

        it "stores one tombstone when several deletions of an unknown message arrive at once" $ \pool -> do
          mid <- newId
          outcomes <- forConcurrently [1 .. 8 :: Int] $ \_ ->
            try (delete pool MetaFacebook mid) :: IO (Either SqlError ())
          length [() | Right () <- outcomes] `shouldBe` 8
          Just (Entity _ row) <- runSqlPool (getBy (ME.UniqueFacebookMessage mid)) pool
          (ME.facebookMessageText row, ME.facebookMessageDeletedAt row /= Nothing) `shouldBe` (Nothing, True)

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres:postgres@postgres:5432/"]

-- Synthetic message ids, remembered for removal.
newIdIn :: IORef [Text] -> IO Text
newIdIn used = do
  suffix <- randomRIO (100000000, 999999999 :: Int)
  let mid = "mid.redelivery-spec." <> T.pack (show suffix)
  modifyIORef' used (mid :)
  pure mid

sender :: Text
sender = "redelivery-spec-sender"

deliver :: ConnectionPool -> MetaChannel -> Text -> Text -> IO ()
deliver pool channel mid body = deliverAs pool channel mid body Nothing

deliverAs :: ConnectionPool -> MetaChannel -> Text -> Text -> Maybe Text -> IO ()
deliverAs pool channel mid body name = deliverWith pool channel mid body name Nothing

deliverWith :: ConnectionPool -> MetaChannel -> Text -> Text -> Maybe Text -> Maybe Text -> IO ()
deliverWith pool channel mid body name metadata = do
  now <- getCurrentTime
  runSqlPool (persistMetaInbound channel now
    [ MetaInboundMessage IGInbound
        { igInboundExternalId = mid
        , igInboundSenderId = sender
        , igInboundSenderName = name
        , igInboundText = body
        , igInboundAdExternalId = Nothing
        , igInboundAdName = Nothing
        , igInboundCampaignExternalId = Nothing
        , igInboundCampaignName = Nothing
        , igInboundMetadata = metadata
        }
    ]) pool

delete :: ConnectionPool -> MetaChannel -> Text -> IO ()
delete pool channel mid = do
  now <- getCurrentTime
  runSqlPool (persistMetaInbound channel (addUTCTime 1 now)
    [ MetaInboundDeleted IGInboundDeleted
        { igInboundDeletedExternalId = mid
        , igInboundDeletedSenderId = sender
        , igInboundDeletedSenderName = Nothing
        , igInboundDeletedMetadata = Nothing
        }
    ]) pool

instagram :: ConnectionPool -> Text -> IO M.InstagramMessage
instagram pool mid = do
  found <- runSqlPool (getBy (M.UniqueInstagramMessage mid)) pool
  maybe (fail "Instagram message was not stored") (pure . entityVal) found

setInstagram mid changes = do
  found <- getBy (M.UniqueInstagramMessage mid)
  case found of
    Just (Entity key _) -> update key changes
    Nothing -> fail "Instagram message was not stored"

instagramRow :: Text -> Text -> Text -> Text -> UTCTime -> M.InstagramMessage
instagramRow mid body direction status now =
  M.InstagramMessage mid sender Nothing (Just body) direction Nothing Nothing Nothing Nothing Nothing
    status Nothing Nothing Nothing 0 Nothing Nothing Nothing Nothing now

-- Removes only the messages this run created.
forget :: IORef [Text] -> ConnectionPool -> IO ()
forget used pool = do
  mids <- readIORef used
  runSqlPool (mapM_ (\mid -> do
    rawExecute "DELETE FROM instagram_message WHERE external_id=?" [PersistText mid]
    rawExecute "DELETE FROM facebook_message WHERE external_id=?" [PersistText mid]) mids) pool
