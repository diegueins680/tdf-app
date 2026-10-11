{-# LANGUAGE OverloadedStrings #-}
module TDF.IndexDeclarationSpec (spec) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (Value(Null))
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist (Entity(..), insert, insert_)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql
  ( ConnectionPool, PersistValue(..), Single(..), SqlPersistT, rawSql, runSqlPool, toSqlKey, transactionUndo )
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Test.Hspec
import qualified TDF.Models as M
import TDF.Server (insertCourseRegistrationFollowUp)
import TDF.WhatsApp.Client (SendTextResult(..))
import TDF.WhatsApp.History
  ( IncomingWhatsAppRecord(..), OutgoingWhatsAppRecord(..)
  , recordIncomingWhatsAppMessage, recordOutgoingWhatsAppMessage )

-- Each case writes through the installed schema inside one transaction that
-- is undone at its end, so nothing it creates outlives it.
spec :: Spec
spec = describe "index-declaration-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_INDEX_DECLARATION_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-index-declarations.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 2)) $
      afterAll destroyAllResources $ do
        it "keeps every message of a conversation" $ \pool -> do
          stored <- undone pool $ do
            now <- currentTime
            a <- party
            b <- party
            thread <- insert (M.ChatThread (toSqlKey a) (toSqlKey b) now now)
            insert_ (M.ChatMessage thread (toSqlKey a) "uno" now)
            insert_ (M.ChatMessage thread (toSqlKey b) "dos" now)
            insert_ (M.ChatMessage thread (toSqlKey a) "tres" (addUTCTime 1 now))
            countOf "SELECT count(*) FROM chat_message WHERE thread_id IN (SELECT id FROM chat_thread WHERE dm_party_a=?)" [PersistInt64 a]
          stored `shouldBe` 3

        it "stores a WhatsApp reply recorded at the instant of the message it answers" $ \pool -> do
          phone <- newPhone
          stored <- undone pool $ do
            now <- currentTime
            Entity incoming _ <- recordIncomingWhatsAppMessage now (inbound phone "index-spec.in")
            _ <- recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
              { owrRecipientPhone = phone
              , owrRecipientPartyId = Nothing
              , owrRecipientName = Nothing
              , owrRecipientEmail = Nothing
              , owrActorPartyId = Nothing
              , owrBody = "Gracias"
              , owrSource = Just "whatsapp_webhook"
              , owrReplyToMessageId = Just incoming
              , owrReplyToExternalId = Nothing
              , owrResendOfMessageId = Nothing
              , owrMetadata = Nothing
              } (Right (SendTextResult Null (Just (phone <> ".index-spec.out"))))
            directions phone
          stored `shouldBe` ["incoming", "outgoing"]

        it "stores two WhatsApp messages one sender wrote that arrive together" $ \pool -> do
          phone <- newPhone
          stored <- undone pool $ do
            now <- currentTime
            _ <- recordIncomingWhatsAppMessage now (inbound phone "index-spec.first")
            _ <- recordIncomingWhatsAppMessage now (inbound phone "index-spec.second")
            directions phone
          stored `shouldBe` ["incoming", "incoming"]

        it "still stores a WhatsApp message delivered twice once" $ \pool -> do
          phone <- newPhone
          stored <- undone pool $ do
            now <- currentTime
            _ <- recordIncomingWhatsAppMessage now (inbound phone "index-spec.same")
            _ <- recordIncomingWhatsAppMessage (addUTCTime 5 now) (inbound phone "index-spec.same")
            directions phone
          stored `shouldBe` ["incoming"]

        it "keeps course follow-ups that share a reminder time or are written together" $ \pool -> do
          stored <- undone pool $ do
            now <- currentTime
            who <- party
            staff <- party
            first <- registration who "index-spec-a" now
            second <- registration who "index-spec-b" now
            let reminder = Just (addUTCTime 86400 now)
                followUp reg = insertCourseRegistrationFollowUp (toSqlKey reg) (Just (toSqlKey who))
                  (Just (toSqlKey staff)) "note" Nothing "Llamar" Nothing Nothing reminder now
            _ <- followUp first
            _ <- followUp first
            _ <- followUp second
            countOf "SELECT count(*) FROM course_registration_follow_up WHERE party_id=?" [PersistInt64 who]
          stored `shouldBe` 3

        it "keeps two receipts uploaded together for one registration" $ \pool -> do
          stored <- undone pool $ do
            now <- currentTime
            who <- party
            reg <- registration who "index-spec-a" now
            let receipt name = rawSql
                  "INSERT INTO course_registration_receipt (registration_id, party_id, file_url, created_at, updated_at) VALUES (?,?,?,?,?) RETURNING id"
                  [PersistInt64 reg, PersistInt64 who, PersistText name, PersistUTCTime now, PersistUTCTime now]
                  :: SqlPersistT IO [Single Int64]
            _ <- receipt "https://example.invalid/uno.pdf"
            _ <- receipt "https://example.invalid/dos.pdf"
            countOf "SELECT count(*) FROM course_registration_receipt WHERE registration_id=?" [PersistInt64 reg]
          stored `shouldBe` 2

        it "keeps two registrations one person makes at the same instant" $ \pool -> do
          stored <- undone pool $ do
            now <- currentTime
            who <- party
            _ <- registration who "index-spec-a" now
            _ <- registration who "index-spec-b" now
            countOf "SELECT count(*) FROM course_registration WHERE party_id=?" [PersistInt64 who]
          stored `shouldBe` 2

        it "leaves the nine as plain indexes and the message identity unique" $ \pool -> do
          constraints <- runSqlPool (countOf
            "SELECT count(*) FROM pg_constraint WHERE contype='u' AND conname LIKE 'index\\_%'" []) pool
          plain <- runSqlPool (countOf
            "SELECT count(*) FROM pg_indexes WHERE schemaname='public' AND indexdef NOT LIKE 'CREATE UNIQUE%' AND indexname IN \
            \('index_chat_message_thread','index_course_registration_party','index_course_registration_follow_up_next',\
            \'index_course_registration_follow_up_party','index_course_registration_follow_up_registration',\
            \'index_course_registration_receipt_party','index_course_registration_receipt_registration',\
            \'index_whats_app_message_party','index_whats_app_message_phone')" []) pool
          identity <- runSqlPool (countOf
            "SELECT count(*) FROM pg_constraint WHERE contype='u' AND conname='unique_whats_app_message'" []) pool
          (constraints, plain, identity) `shouldBe` (0, 9, 1)

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres:postgres@postgres:5432/"]

-- Runs the action and undoes everything it wrote.
undone :: ConnectionPool -> SqlPersistT IO a -> IO a
undone pool action = runSqlPool (action <* transactionUndo) pool

-- The transaction's own clock reading, at the precision the database stores.
currentTime :: SqlPersistT IO UTCTime
currentTime = do
  rows <- rawSql "SELECT date_trunc('milliseconds', now())" []
  case rows of
    [Single now] -> pure now
    _ -> error "the database returned no current time"

newPhone :: IO Text
newPhone = do
  suffix <- randomRIO (10000000, 99999999 :: Int)
  pure ("+5939" <> T.pack (show suffix))

party :: SqlPersistT IO Int64
party = do
  rows <- rawSql "INSERT INTO party (display_name, is_org, created_at) VALUES ('index-spec', false, now()) RETURNING id" []
  case rows of
    [Single key] -> pure key
    _ -> error "the party was not created"

registration :: Int64 -> Text -> UTCTime -> SqlPersistT IO Int64
registration who slug now = do
  rows <- rawSql
    "INSERT INTO course_registration (course_slug, party_id, source, status, created_at, updated_at) VALUES (?,?,'landing','pending_payment',?,?) RETURNING id"
    [PersistText slug, PersistInt64 who, PersistUTCTime now, PersistUTCTime now]
  case rows of
    [Single key] -> pure key
    _ -> error "the registration was not created"

inbound :: Text -> Text -> IncomingWhatsAppRecord
inbound phone suffix = IncomingWhatsAppRecord
  { iwrExternalId = phone <> "." <> suffix
  , iwrSenderId = phone
  , iwrSenderName = Nothing
  , iwrText = "Hola"
  , iwrAdExternalId = Nothing
  , iwrAdName = Nothing
  , iwrCampaignExternalId = Nothing
  , iwrCampaignName = Nothing
  , iwrMetadata = Nothing
  , iwrTransportPayload = Nothing
  , iwrSource = Just "whatsapp_webhook"
  }

directions :: Text -> SqlPersistT IO [Text]
directions phone = do
  rows <- rawSql "SELECT direction FROM whats_app_message WHERE phone_e164=? ORDER BY direction" [PersistText phone]
  pure [direction | Single direction <- rows]

countOf :: Text -> [PersistValue] -> SqlPersistT IO Int
countOf sql args = do
  rows <- rawSql sql args
  case rows of
    [Single n] -> pure n
    _ -> error "the count returned no row"
