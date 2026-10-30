{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module TDF.Contracts.DeliverySpec (spec) where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Servant ((:<|>)(..), ServerError(..) )
import Servant.Server.Internal.Handler (runHandler)
import System.Directory (createDirectoryIfMissing, listDirectory, withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import qualified TDF.Contracts.Server as Contracts

spec :: Spec
spec = sequential $ describe "contract delivery reporting" $ around sandbox $ do
  it "refuses unsupported delivery instead of claiming an existing contract was sent" $ \() -> do
    cid <- createFixture
    before <- BS.readFile (contractPath cid)
    files <- listDirectory "contracts/store"
    result <- send cid (email "recipient@example.test")
    assertStatus 501 result
    BS.readFile (contractPath cid) `shouldReturn` before
    listDirectory "contracts/store" `shouldReturn` files

  it "preserves successful contract creation and canonical storage" $ \() -> do
    cid <- createFixture
    bytes <- BL.readFile (contractPath cid)
    case Contracts.decodeStoredContractFor cid bytes of
      Left err -> expectationFailure (show err)
      Right _ -> pure ()

  it "rejects malformed ids before considering delivery" $ \() ->
    send "../outside" (email "recipient@example.test") >>= assertStatus 400

  it "rejects invalid recipient addresses before file lookup" $ \() ->
    send missingId (email "bad-email") >>= assertStatus 400

  it "rejects overposted send intent before file lookup" $ \() ->
    send missingId (A.object ["email" .= ("recipient@example.test" :: Text), "status" .= ("sent" :: Text)]) >>= assertStatus 400

  it "preserves missing-contract send errors" $ \() ->
    send missingId (email "recipient@example.test") >>= assertStatus 404

  it "preserves unreadable-contract send errors" $ \() -> do
    createDirectoryIfMissing True "contracts/store"
    BL.writeFile (contractPath missingId) "not-json"
    send missingId (email "recipient@example.test") >>= assertStatus 500

  it "preserves stored-id mismatch errors" $ \() -> do
    cid <- createFixture
    bytes <- BL.readFile (contractPath cid)
    BL.writeFile (contractPath missingId) bytes
    send missingId (email "recipient@example.test") >>= assertStatus 500

  it "preserves missing-contract PDF errors without invoking a renderer" $ \() ->
    pdf missingId >>= assertStatus 404

  it "preserves malformed-contract PDF errors without invoking a renderer" $ \() -> do
    createDirectoryIfMissing True "contracts/store"
    BL.writeFile (contractPath missingId) "not-json"
    pdf missingId >>= assertStatus 500

sandbox :: ActionWith () -> IO ()
sandbox action = withSystemTempDirectory "tdf-contract-delivery-" $ \directory ->
  withCurrentDirectory directory (action ())

createFixture :: IO Text
createFixture = do
  response <- create (A.object ["kind" .= ("generic" :: Text), "concept" .= ("Synthetic contract" :: Text)])
  case response of
    Right (A.Object object) -> case (KM.lookup "status" object, KM.lookup "id" object) of
      (Just (A.String "created"), Just (A.String cid)) -> pure cid
      _ -> expectationFailure "Invalid create-handler fixture response" >> fail "fixture failed"
    _ -> expectationFailure ("Create-handler fixture failed: " <> show response) >> fail "fixture failed"

create :: A.Value -> IO (Either ServerError A.Value)
create payload = case Contracts.server of
  handler :<|> _ :<|> _ -> runHandler (handler payload)

send :: Text -> A.Value -> IO (Either ServerError A.Value)
send cid payload = case Contracts.server of
  _ :<|> _ :<|> handler -> runHandler (handler cid payload)

pdf :: Text -> IO (Either ServerError BL.ByteString)
pdf cid = case Contracts.server of
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
