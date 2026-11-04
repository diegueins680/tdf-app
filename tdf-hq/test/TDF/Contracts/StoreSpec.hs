{-# LANGUAGE OverloadedStrings #-}

-- | CONTRACT-STORE-001 and CONTRACT-SEND-001 against a real (SQLite) database:
-- contracts are stored in the contract table, and "sent" means the mail
-- transport accepted a message carrying the contract PDF.
module TDF.Contracts.StoreSpec (spec) where

import           Control.Exception (throwIO)
import           Control.Monad.Logger (runNoLoggingT)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import           Data.IORef (modifyIORef', newIORef, readIORef)
import           Data.Text (Text)
import           Database.Persist (Entity (..), selectList)
import           Database.Persist.Sql (ConnectionPool, rawExecute, runSqlPool)
import           Database.Persist.Sqlite (createSqlitePool)
import           Servant (ServerError (..), (:<|>) (..), runHandler)
import           Test.Hspec

import           TDF.Contracts.Server
import qualified TDF.ModelsExtra as ME

-- Same columns as 2026-11-04_contract_store.sql.
withPool :: (ConnectionPool -> IO a) -> IO a
withPool action = do
  pool <- runNoLoggingT (createSqlitePool ":memory:" 1)
  flip runSqlPool pool $ do
    rawExecute
      "CREATE TABLE \"contract\" (\"id\" VARCHAR PRIMARY KEY, \"kind\" VARCHAR NOT NULL,\
      \ \"payload\" VARCHAR NOT NULL, \"created_by_party_id\" INTEGER NULL, \"created_at\" TIMESTAMP NOT NULL)" []
    rawExecute
      "CREATE TABLE \"contract_delivery\" (\"id\" INTEGER PRIMARY KEY, \"contract_id\" VARCHAR NOT NULL,\
      \ \"recipient_email\" VARCHAR NOT NULL, \"requested_by_party_id\" INTEGER NULL,\
      \ \"requested_at\" TIMESTAMP NOT NULL, \"accepted_at\" TIMESTAMP NULL, \"failed_at\" TIMESTAMP NULL,\
      \ \"failure\" VARCHAR NULL)" []
  action pool

fakePdf :: Text -> IO (Either Text BL.ByteString)
fakePdf _ = pure (Right "%PDF-1.4 fake")

deps :: ConnectionPool -> Maybe (Text -> Text -> Text -> BL.ByteString -> IO ()) -> ContractsDeps
deps pool email = ContractsDeps
  { cdPool = pool
  , cdEmail = email
  , cdActor = Nothing
  , cdRenderPdf = fakePdf
  }

create :: ContractsDeps -> IO Text
create d = do
  let createH :<|> _ :<|> _ = server d
  result <- runHandler (createH (A.object ["kind" A..= ("generic" :: Text)]))
  case result of
    Right (A.Object o) | Just (A.String cid) <- KM.lookup "id" o -> pure cid
    other -> expectationFailure ("contract was not created: " <> show other) >> fail "stop"

send :: ContractsDeps -> Text -> IO (Either ServerError A.Value)
send d cid =
  let _ :<|> _ :<|> sendH = server d
  in runHandler (sendH cid (A.object ["email" A..= ("cliente@example.com" :: Text)]))

deliveries :: ConnectionPool -> IO [ME.ContractDelivery]
deliveries pool = map entityVal <$> runSqlPool (selectList [] []) pool

spec :: Spec
spec = describe "Contract store and send (CONTRACT-STORE-001, CONTRACT-SEND-001)" $ do
  it "stores a created contract in the database and reads it back for the PDF" $ withPool $ \pool -> do
    let d = deps pool Nothing
    cid <- create d
    rows <- runSqlPool (selectList [] []) pool :: IO [Entity ME.Contract]
    map (ME.contractKind . entityVal) rows `shouldBe` ["generic"]
    let _ :<|> pdfH :<|> _ = server d
    runHandler (pdfH cid) `shouldReturn` Right "%PDF-1.4 fake"

  it "refuses to claim a send when email is not configured, and records nothing" $ withPool $ \pool -> do
    let d = deps pool Nothing
    cid <- create d
    result <- send d cid
    fmap errHTTPCode (either Just (const Nothing) result) `shouldBe` Just 503
    length <$> deliveries pool `shouldReturn` 0

  it "emails the contract PDF and records the accepted delivery" $ withPool $ \pool -> do
    sent <- newIORef []
    let d = deps pool (Just (\to kind cid pdf -> modifyIORef' sent ((to, kind, cid, pdf) :)))
    cid <- create d
    result <- send d cid
    case result of
      Right (A.Object o) -> KM.lookup "status" o `shouldBe` Just (A.String "sent")
      other -> expectationFailure ("expected sent, got " <> show other)
    readIORef sent `shouldReturn` [("cliente@example.com", "generic", cid, "%PDF-1.4 fake")]
    rows <- deliveries pool
    map ME.contractDeliveryRecipientEmail rows `shouldBe` ["cliente@example.com"]
    map (fmap (const ()) . ME.contractDeliveryAcceptedAt) rows `shouldBe` [Just ()]
    map ME.contractDeliveryFailure rows `shouldBe` [Nothing]

  it "answers 502 and records the failure when the mail transport refuses" $ withPool $ \pool -> do
    let d = deps pool (Just (\_ _ _ _ -> throwIO (userError "smtp refused")))
    cid <- create d
    result <- send d cid
    fmap errHTTPCode (either Just (const Nothing) result) `shouldBe` Just 502
    rows <- deliveries pool
    map ME.contractDeliveryAcceptedAt rows `shouldBe` [Nothing]
    map (fmap (const ()) . ME.contractDeliveryFailedAt) rows `shouldBe` [Just ()]

  it "answers 404 for an unknown contract without sending" $ withPool $ \pool -> do
    let d = deps pool (Just (\_ _ _ _ -> expectationFailure "must not send"))
    result <- send d "550e8400-e29b-41d4-a716-446655440000"
    fmap errHTTPCode (either Just (const Nothing) result) `shouldBe` Just 404
    length <$> deliveries pool `shouldReturn` 0
