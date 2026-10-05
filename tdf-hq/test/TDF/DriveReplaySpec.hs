{-# LANGUAGE OverloadedStrings #-}

-- Synthetic in-memory HTTP wire only: both connection constructors below are
-- replaced, so neither DNS, sockets, TLS nor provider credentials are used.
module TDF.DriveReplaySpec (spec) where

import Control.Exception (AsyncException(ThreadKilled), IOException, SomeAsyncException, SomeException, bracket, fromException, throwIO, try, tryJust)
import Control.Monad (unless)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Maybe (isJust)
import Database.Persist.Sql (toSqlKey)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.URI (parseQuery)
import Servant (ServerError, errHTTPCode)
import Servant.Multipart (FileData(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec
import TDF.API.Types (DriveUploadDTO(..))
import TDF.Server (uploadToDrive)

type Hop = (BS.ByteString, BS.ByteString)
type Attempt = (BS.ByteString, Int, Bool, BS.ByteString, BS.ByteString)

fixtureKey :: Text
fixtureKey = T.replicate 64 "a"

fixtureToken :: Text
fixtureToken = "synthetic-drive-replay-token"

fileId :: Text
fileId = "syntheticDriveFileA"

listHop, createHop, shareHop, metaHop :: Hop
listHop = ("GET", "/drive/v3/files")
createHop = ("POST", "/upload/drive/v3/files")
shareHop = ("POST", "/drive/v3/files/syntheticDriveFileA/permissions")
metaHop = ("GET", "/drive/v3/files/syntheticDriveFileA")

firstUploadTrace, replayTrace :: [Hop]
firstUploadTrace = [listHop, createHop, shareHop, metaHop]
replayTrace = [listHop, shareHop, metaHop]

spec :: Spec
spec = describe "Drive replay payload binding (no provider network)" $ do
  it "reuses an identical upload without a second multipart create" $
    withFixture False False $ \manager trace wire directory _ -> do
      first <- send manager directory "first.tmp" "same.webp" "image/webp" "SYNTHETIC_PAYLOAD_A"
      second <- send manager directory "retry.tmp" "same.webp" "image/webp" "SYNTHETIC_PAYLOAD_A"
      duFileId first `shouldBe` fileId
      duFileId second `shouldBe` fileId
      trace `shouldReturn` (firstUploadTrace <> replayTrace)
      wire >>= (\bytes -> bytes `shouldSatisfy` BS.isInfixOf "SYNTHETIC_PAYLOAD_A")

  mapM_ (\(label, name, mime, bytes) ->
    it ("rejects a reused key with different " <> label <> " before provider mutation") $
      withFixture False False $ \manager trace wire directory _ -> do
        first <- send manager directory "first.tmp" "same.webp" "image/webp" "SYNTHETIC_PAYLOAD_A"
        duFileId first `shouldBe` fileId
        outcome <- trySynchronous (send manager directory "retry.tmp" name mime bytes)
          :: IO (Either SomeException DriveUploadDTO)
        -- Check effects before checking the response. A failure after sharing is
        -- not a passing conflict. These three examples fail on the old helper.
        trace `shouldReturn` (firstUploadTrace <> [listHop])
        case outcome of
          Left exception -> case fromException exception :: Maybe ServerError of
            Just conflict -> errHTTPCode conflict `shouldBe` 409
            Nothing -> expectationFailure "Expected a typed conflict, not arbitrary IO/JSON failure"
          Right result -> expectationFailure ("Conflicting upload returned " <> show result)
        sent <- wire
        sent `shouldSatisfy` (not . BS.isInfixOf "SYNTHETIC_PAYLOAD_B")
    )
    [ ("content", "same.webp", "image/webp", "SYNTHETIC_PAYLOAD_B")
    , ("name", "different.webp", "image/webp", "SYNTHETIC_PAYLOAD_A")
    , ("MIME", "same.webp", "image/avif", "SYNTHETIC_PAYLOAD_A")
    ]

  it "rejects another Party reusing the same key and folder" $
    withFixture False False $ \manager trace _ directory _ -> do
      _ <- send manager directory "first.tmp" "same.webp" "image/webp" "SYNTHETIC_PAYLOAD_A"
      outcome <- trySynchronous (sendAs 202 manager directory "foreign.tmp" "same.webp" "image/webp" "SYNTHETIC_PAYLOAD_A")
        :: IO (Either SomeException DriveUploadDTO)
      trace `shouldReturn` (firstUploadTrace <> [listHop])
      expectConflict outcome

  it "does not adopt or share a legacy entry without a stored fingerprint" $
    withFixture False True $ \manager trace _ directory _ -> do
      _ <- send manager directory "first.tmp" "same.webp" "image/webp" "SYNTHETIC_PAYLOAD_A"
      outcome <- trySynchronous (send manager directory "legacy.tmp" "same.webp" "image/webp" "SYNTHETIC_PAYLOAD_A")
        :: IO (Either SomeException DriveUploadDTO)
      trace `shouldReturn` (firstUploadTrace <> [listHop])
      expectConflict outcome

  it "does not follow a provider redirect" $
    withFixture True False $ \manager trace _ directory transport -> do
      outcome <- trySynchronous (send manager directory "redirect.tmp" "same.webp" "image/webp" "SYNTHETIC_PAYLOAD_A")
        :: IO (Either SomeException DriveUploadDTO)
      trace `shouldReturn` [listHop]
      transport `shouldReturn` ([("www.googleapis.com", 443, True, "GET", "/drive/v3/files")], 1)
      case outcome of
        Left _ -> pure ()
        Right _ -> expectationFailure "Redirect must not produce an upload result"

  it "rejects an unexpected destination before creating a connection" $
    withFixture False False $ \manager trace wire _ transport -> do
      request <- HC.parseRequest "https://unexpected.invalid/drive/v3/files"
      outcome <- trySynchronous (HC.httpLbs request manager)
        :: IO (Either SomeException (HC.Response BL.ByteString))
      trace `shouldReturn` []
      wire `shouldReturn` BS.empty
      transport `shouldReturn` ([("unexpected.invalid", 443, True, "GET", "/drive/v3/files")], 0)
      case outcome of
        Left _ -> pure ()
        Right _ -> expectationFailure "Unexpected destination was admitted"

  it "does not turn asynchronous cancellation into an expected provider failure" $ do
    outcome <- try (trySynchronous (throwIO ThreadKilled :: IO ()))
      :: IO (Either AsyncException (Either SomeException ()))
    case outcome of
      Left ThreadKilled -> pure ()
      _ -> expectationFailure "Asynchronous cancellation was swallowed"

-- Accept only the synchronous failure classes this adapter can deliberately
-- report. Unknown exceptions (including the outer timeout's exception) escape;
-- cancellation must never make a negative assertion pass.
trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous = tryJust $ \exception ->
  if isJust (fromException exception :: Maybe SomeAsyncException)
    then Nothing
    else if isJust (fromException exception :: Maybe IOException)
      || isJust (fromException exception :: Maybe HC.HttpException)
      || isJust (fromException exception :: Maybe ServerError)
      then Just exception
      else Nothing

send :: HC.Manager -> FilePath -> FilePath -> Text -> Text -> BS.ByteString -> IO DriveUploadDTO
send = sendAs 101

sendAs :: Int64 -> HC.Manager -> FilePath -> FilePath -> Text -> Text -> BS.ByteString -> IO DriveUploadDTO
sendAs actor manager directory localName remoteName mime bytes = do
  let localPath = directory </> localName
  BS.writeFile localPath bytes
  uploadToDrive manager (toSqlKey actor) fixtureToken
    FileData { fdInputName = "file", fdFileName = remoteName
             , fdFileCType = mime, fdPayload = localPath }
    mime (Just remoteName) (Just "syntheticFolder") (Just fixtureKey)

expectConflict :: Either SomeException DriveUploadDTO -> Expectation
expectConflict outcome = case outcome of
  Left exception -> case fromException exception :: Maybe ServerError of
    Just conflict -> errHTTPCode conflict `shouldBe` 409
    Nothing -> expectationFailure "Expected typed409, not arbitrary IO/JSON failure"
  Right result -> expectationFailure ("Conflicting upload returned " <> show result)

-- The fake echoes appProperties from the ACTUAL multipart metadata, without
-- recomputing the proposed fingerprint. A/A proves matching entries still work.
-- legacyReplay suppresses the property on replay to represent an old object.
withFixture
  :: Bool -> Bool
  -> (HC.Manager -> IO [Hop] -> IO BS.ByteString -> FilePath -> IO ([Attempt], Int) -> IO a)
  -> IO a
withFixture redirectList legacyReplay action = withSystemTempDirectory "tdf-drive-replay-review-" $ \directory -> do
  created <- newIORef False
  attempts <- newIORef []
  connections <- newIORef (0 :: Int)
  hops <- newIORef []
  writes <- newIORef []
  nextResponse <- newIORef BS.empty
  let modifyRequest request = do
        -- Record before admission: a followed redirect rejected by the host
        -- guard must still fail the redirect-control assertion.
        modifyIORef' attempts (<> [(HC.host request, HC.port request, HC.secure request, HC.method request, HC.path request)])
        let hop = (HC.method request, HC.path request)
        unless (HC.host request == "www.googleapis.com" && HC.port request == 443
          && HC.secure request && hop `elem` [listHop, createHop, shareHop, metaHop]) $
            throwIO (userError "Unexpected synthetic provider destination or operation")
        unless (lookup "Authorization" (HC.requestHeaders request) == Just ("Bearer " <> TE.encodeUtf8 fixtureToken)) $
          throwIO (userError "Only the explicit synthetic credential is admitted")
        seen <- readIORef hops
        unless (length seen < 16) $ throwIO (userError "Unexpected provider request loop")
        modifyIORef' hops (<> [hop])
        body <- if hop == listHop then do
          let expectedQuery = "appProperties has { key='tdfIdempotencyKey' and value='"
                <> TE.encodeUtf8 fixtureKey <> "' } and trashed=false and 'syntheticFolder' in parents"
          unless (lookup "q" (parseQuery (HC.queryString request)) == Just (Just expectedQuery)) $
            throwIO (userError "Unexpected key/folder lookup")
          exists <- readIORef created
          if not exists then pure "{\"files\":[]}" else do
            uploadedWire <- BS.concat <$> readIORef writes
            properties <- capturedProperties uploadedWire
            let fields = ["id" A..= fileId]
                  <> if legacyReplay then [] else ["appProperties" A..= properties]
            pure (BL.toStrict (A.encode (A.object ["files" A..= [A.object fields]])))
        else if hop == createHop then do
          writeIORef created True
          pure "{\"id\":\"syntheticDriveFileA\"}"
        else pure "{}"
        writeIORef nextResponse $ if redirectList && hop == listHop
          then "HTTP/1.1 302 Found\r\nLocation: https://unexpected.invalid/not-allowed\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
          else "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: "
            <> BS.pack (show (BS.length body)) <> "\r\nConnection: close\r\n\r\n" <> body
        pure request { HC.redirectCount = 0 }
      connect _ _ _ = do
        modifyIORef' connections (+1)
        response <- readIORef nextResponse
        remaining <- newIORef response
        HC.makeConnection
          (atomicModifyIORef' remaining (\bytes -> (BS.empty, bytes)))
          (\bytes -> modifyIORef' writes (<> [bytes]))
          (pure ())
      settings = HC.managerSetProxy HC.noProxy HC.defaultManagerSettings
        { HC.managerModifyRequest = modifyRequest
        , HC.managerTlsConnection = pure connect
        , HC.managerRawConnection = pure connect
        , HC.managerResponseTimeout = HC.responseTimeoutMicro 5000000
        }
  result <- timeout 15000000 $ bracket (HC.newManager settings) HC.closeManager $ \manager ->
    action manager (readIORef hops) (BS.concat <$> readIORef writes) directory
      ((,) <$> readIORef attempts <*> readIORef connections)
  maybe (fail "Synthetic Drive replay fixture timed out") pure result

capturedProperties :: BS.ByteString -> IO A.Value
capturedProperties wire = do
  let marker = "Content-Type: application/json; charset=UTF-8\r\n\r\n"
      (_, metadataStart) = BS.breakSubstring marker wire
      metadata = fst (BS.breakSubstring "\r\n--" (BS.drop (BS.length marker) metadataStart))
  unless (not (BS.null metadataStart)) $ fail "No actual upload metadata was captured"
  case A.eitherDecodeStrict' metadata of
    Right (A.Object object) -> case KM.lookup "appProperties" object of
      Just value@(A.Object properties) -> do
        unless (KM.lookup "tdfIdempotencyKey" properties == Just (A.String fixtureKey)) $
          fail "Actual upload omitted the original retry key"
        pure value
      _ -> fail "Actual upload omitted provider appProperties"
    _ -> fail "Actual multipart metadata did not decode"
