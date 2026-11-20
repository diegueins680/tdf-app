{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module TDF.Contracts.DeliverySpec (spec) where

import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity(..), selectList)
import Database.Persist.Sql (rawExecute, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Servant ((:<|>)(..), ServerError(..))
import Servant.Server.Internal.Handler (runHandler)
import System.Directory (createDirectoryIfMissing, listDirectory, withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import qualified TDF.Contracts.Server as Contracts
import qualified TDF.ModelsExtra as ME

-- Isolated database/files and an injected renderer; never opens SMTP or reads
-- real contract documents. StoreSpec covers accepted/refused fake transports.
spec :: Spec
spec = sequential $ describe "contract delivery reporting after durable-store integration" $ around sandbox $ do
  it "reports unconfigured SMTP as 503 and preserves the stored contract" $ \d -> do
    cid <- createFixture d
    before <- map show <$> contracts d
    send d cid (email "recipient@example.test") >>= assertStatus 503
    (map show <$> contracts d) `shouldReturn` before
    deliveryCount d `shouldReturn` 0
    listDirectory "." `shouldReturn` []

  it "preserves creation in canonical database storage and independent PDF download" $ \d -> do
    cid <- createFixture d
    pdf d cid `shouldReturn` Right "%PDF synthetic"
    rows <- contracts d
    length rows `shouldBe` 1
    case rows of
      [Entity _ row] -> case Contracts.storedFromRecord cid row of
        Left err -> expectationFailure (show err)
        Right _ -> pure ()
      _ -> expectationFailure "expected one persisted contract"

  it "rejects malformed ids before considering delivery" $ \d ->
    send d "../outside" (email "recipient@example.test") >>= assertStatus 400
  it "rejects invalid recipient addresses before lookup" $ \d ->
    send d missingId (email "bad-email") >>= assertStatus 400
  it "rejects overposted send intent before lookup" $ \d ->
    send d missingId (A.object ["email" .= ("recipient@example.test" :: Text), "status" .= ("sent" :: Text)]) >>= assertStatus 400
  it "preserves missing-contract send errors" $ \d ->
    send d missingId (email "recipient@example.test") >>= assertStatus 404
  it "preserves unreadable legacy-contract errors without delivery" $ \d -> do
    createDirectoryIfMissing True "contracts/store"
    BL.writeFile (contractPath missingId) "not-json"
    send d missingId (email "recipient@example.test") >>= assertStatus 500
    deliveryCount d `shouldReturn` 0
  it "preserves legacy stored-id mismatch errors" $ \d -> do
    createDirectoryIfMissing True "contracts/store"
    BL.writeFile (contractPath missingId)
      "{\"id\":\"550e8400-e29b-41d4-a716-446655440001\",\"kind\":\"generic\",\"payload\":{\"kind\":\"generic\"},\"created_at\":\"2026-01-01T00:00:00Z\"}"
    send d missingId (email "recipient@example.test") >>= assertStatus 500
  it "preserves missing-contract PDF errors" $ \d -> pdf d missingId >>= assertStatus 404
  it "preserves malformed legacy-contract PDF errors" $ \d -> do
    createDirectoryIfMissing True "contracts/store"
    BL.writeFile (contractPath missingId) "not-json"
    pdf d missingId >>= assertStatus 500
  it "does not attempt SMTP or claim delivery after PDF generation fails" $ \d -> do
    cid <- createFixture d
    let failing = d { Contracts.cdEmail = Just (\_ _ _ _ -> expectationFailure "must not contact SMTP")
                    , Contracts.cdRenderPdf = \_ -> pure (Left "synthetic renderer failure") }
    send failing cid (email "recipient@example.test") >>= assertStatus 500
    deliveryCount d `shouldReturn` 0
    pdf d cid `shouldReturn` Right "%PDF synthetic"

sandbox :: ActionWith Contracts.ContractsDeps -> IO ()
sandbox action = withSystemTempDirectory "tdf-contract-delivery-" $ \directory ->
  withCurrentDirectory directory $ do
    pool <- runNoLoggingT (createSqlitePool ":memory:" 1)
    flip runSqlPool pool $ do
      rawExecute "CREATE TABLE contract(id VARCHAR PRIMARY KEY,kind VARCHAR NOT NULL,payload VARCHAR NOT NULL,created_by_party_id INTEGER NULL,created_at TIMESTAMP NOT NULL)" []
      rawExecute "CREATE TABLE contract_delivery(id INTEGER PRIMARY KEY,contract_id VARCHAR NOT NULL,recipient_email VARCHAR NOT NULL,requested_by_party_id INTEGER NULL,requested_at TIMESTAMP NOT NULL,accepted_at TIMESTAMP NULL,failed_at TIMESTAMP NULL,failure VARCHAR NULL)" []
    action (Contracts.ContractsDeps pool Nothing Nothing (\_ -> pure (Right "%PDF synthetic")))

contracts :: Contracts.ContractsDeps -> IO [Entity ME.Contract]
contracts d = runSqlPool (selectList [] []) (Contracts.cdPool d)
deliveryCount :: Contracts.ContractsDeps -> IO Int
deliveryCount d = length <$> (runSqlPool (selectList [] []) (Contracts.cdPool d) :: IO [Entity ME.ContractDelivery])
createFixture :: Contracts.ContractsDeps -> IO Text
createFixture d = do
  let handler :<|> _ :<|> _ = Contracts.server d
  response <- runHandler (handler (A.object ["kind" .= ("generic" :: Text), "concept" .= ("Synthetic contract" :: Text)]))
  case response of
    Right (A.Object object) | Just (A.String cid) <- KM.lookup "id" object -> pure cid
    _ -> expectationFailure ("Create-handler fixture failed: " <> show response) >> fail "fixture failed"
send :: Contracts.ContractsDeps -> Text -> A.Value -> IO (Either ServerError A.Value)
send d cid payload = case Contracts.server d of
  _ :<|> _ :<|> handler -> runHandler (handler cid payload)
pdf :: Contracts.ContractsDeps -> Text -> IO (Either ServerError BL.ByteString)
pdf d cid = case Contracts.server d of
  _ :<|> handler :<|> _ -> runHandler (handler cid)
email :: Text -> A.Value
email address = A.object ["email" .= address]
missingId :: Text
missingId = "550e8400-e29b-41d4-a716-446655440000"
contractPath :: Text -> FilePath
contractPath cid = "contracts/store/" <> T.unpack cid <> ".json"
assertStatus :: Show a => Int -> Either ServerError a -> Expectation
assertStatus expected (Left err) = errHTTPCode err `shouldBe` expected
assertStatus _ (Right value) = expectationFailure ("Expected refused operation, got successful response: " <> show value)
