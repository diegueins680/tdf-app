{-# LANGUAGE OverloadedStrings #-}
module TDF.FailureBoundarySpec (spec, main) where

import Control.Exception (AsyncException(ThreadKilled), Exception, SomeException,
  bracket, evaluate, fromException, throwIO, toException, try)
import Control.Monad (forM_)
import Data.ByteString.Builder (toLazyByteString)
import Data.IORef
import Data.Text (Text)
import Network.HTTP.Types (status200, status400, status413, status431, status500)
import Network.Wai
import Network.Wai.Internal (ResponseReceived(..))
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec
import TDF.App.FailureBoundary
import TDF.Cors (corsPolicy)

data Unrenderable = Unrenderable
instance Show Unrenderable where show _ = error "EXCEPTION_RENDERED_PRIVATE_SECRET"
instance Exception Unrenderable

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "request and activity failure authority" $ do
  it "returns a fixed pre-response error and never logs the exception payload" $ do
    logs <- newIORef ([] :: [Text])
    count <- newIORef (0 :: Int)
    _ <- requestExceptionBoundary (\entry -> modifyIORef' logs (entry:))
      (\_ _ -> throwIO (userError "private@example.invalid Bearer=SYNTHETIC_SECRET"))
      defaultRequest $ \response -> do
        modifyIORef' count (+1)
        responseStatus response `shouldBe` status500
        responseHeaders response `shouldBe` [("Content-Type", "text/plain; charset=utf-8")]
        chunks <- newIORef mempty
        let (_, _, body) = responseToStream response
        body $ \stream -> stream (\chunk -> modifyIORef' chunks (<> chunk)) (pure ())
        (toLazyByteString <$> readIORef chunks) `shouldReturn` "Internal server error"
        pure ResponseReceived
    readIORef count `shouldReturn` 1
    readIORef logs `shouldReturn` ["[HTTP] Unhandled request failure"]
  it "does not even render an exception with an unsafe Show instance" $ do
    logs <- newIORef ([] :: [Text])
    _ <- requestExceptionBoundary (\entry -> modifyIORef' logs (entry:))
      (\_ _ -> throwIO Unrenderable) defaultRequest (const (pure ResponseReceived))
    readIORef logs `shouldReturn` ["[HTTP] Unhandled request failure"]
  it "propagates cancellation without logging or sending an ordinary response" $ do
    result <- try (requestExceptionBoundary (const (expectationFailure "cancellation logged"))
      (\_ _ -> throwIO ThreadKilled) defaultRequest
      (\_ -> expectationFailure "cancellation responded" >> pure ResponseReceived))
      :: IO (Either SomeException ResponseReceived)
    case result of
      Left ex -> fromException ex `shouldBe` Just ThreadKilled
      Right _ -> expectationFailure "cancellation swallowed"
  it "never invokes the response callback again after an application responds then fails" $ do
    count <- newIORef (0 :: Int)
    result <- try (requestExceptionBoundary (const (pure ()))
      (\_ respond -> respond (responseLBS status200 [] "ok") >> throwIO Unrenderable)
      defaultRequest (\_ -> modifyIORef' count (+1) >> pure ResponseReceived))
      :: IO (Either SomeException ResponseReceived)
    readIORef count `shouldReturn` 1
    case result of Left _ -> pure (); Right _ -> expectationFailure "post-response failure swallowed"
  it "never retries a throwing response callback" $ do
    count <- newIORef (0 :: Int)
    result <- try (requestExceptionBoundary (const (pure ()))
      (\_ respond -> respond (responseLBS status200 [] "ok"))
      defaultRequest (\_ -> modifyIORef' count (+1) >> throwIO Unrenderable))
      :: IO (Either SomeException ResponseReceived)
    readIORef count `shouldReturn` 1
    case result of Left _ -> pure (); Right _ -> expectationFailure "callback failure swallowed"
  it "does not replace a fixed response when diagnostic persistence fails" $ do
    count <- newIORef (0 :: Int)
    _ <- requestExceptionBoundary (const (throwIO Unrenderable))
      (\_ _ -> throwIO Unrenderable) defaultRequest
      (\response -> (responseStatus response `shouldBe` status500) >> modifyIORef' count (+1) >> pure ResponseReceived)
    readIORef count `shouldReturn` 1
  it "Warp logging never renders arbitrary exceptions and excludes cancellation" $ do
    logs <- newIORef ([] :: [Text])
    let logger entry = modifyIORef' logs (entry:)
    reportUnhandledException logger (toException Unrenderable)
    reportUnhandledException logger (toException ThreadKilled)
    readIORef logs `shouldReturn` ["[HTTP] Unhandled request failure"]
    lookup "Access-Control-Allow-Origin" (responseHeaders internalErrorResponse) `shouldBe` Nothing
  it "Warp retains safe protocol error statuses and asynchronous propagation" $ do
    forM_ [(Warp.BadFirstLine "PRIVATE_HEADER",status400),
           (Warp.PayloadTooLarge,status413), (Warp.RequestHeaderFieldsTooLarge,status431)] $ \(err,status) -> do
      let response = Warp.defaultOnExceptionResponse (toException err)
      responseStatus response `shouldBe` status
      lookup "Access-Control-Allow-Origin" (responseHeaders response) `shouldBe` Nothing
    evaluate (responseStatus (Warp.defaultOnExceptionResponse (toException ThreadKilled)))
      `shouldThrow` (== ThreadKilled)
  it "activity failures emit fixed diagnostics and preserve cancellation" $ do
    logs <- newIORef ([] :: [Text])
    let logger entry = modifyIORef' logs (entry:)
    bestEffortActivity logger (throwIO Unrenderable)
    readIORef logs `shouldReturn` ["[Auth][Activity] Audit persistence failed"]
    bestEffortActivity logger (throwIO ThreadKilled) `shouldThrow` (== ThreadKilled)
    readIORef logs `shouldReturn` ["[Auth][Activity] Audit persistence failed"]
  forM_ ["source_fetch_failed", "radio_fetch_failed"] $ \label ->
    it ("radio failure retains only its fixed diagnostic: " <> show label) $ do
      fixedFailure label (throwIO Unrenderable :: IO ()) `shouldReturn` Left label
      fixedFailure label (throwIO ThreadKilled :: IO ()) `shouldThrow` (== ThreadKilled)
  it "normal activity and fetch success remain unchanged" $ do
    bestEffortActivity (const (expectationFailure "success logged as failure")) (pure ())
    fixedFailure "radio_fetch_failed" (pure (7 :: Int)) `shouldReturn` Right 7
  it "the configured CORS layer controls pre-response failures" $
    withProductionCors $ do
      cors <- corsPolicy
      logs <- newIORef ([] :: [Text])
      calls <- newIORef (0 :: Int)
      let app = cors $ requestExceptionBoundary (\entry -> modifyIORef' logs (entry:))
            (\_ _ -> modifyIORef' calls (+1) >> throwIO Unrenderable)
      _ <- app defaultRequest{requestHeaders=[("Origin","https://www.tdfrecords.net")]} $ \response -> do
        responseStatus response `shouldBe` status500
        lookup "Access-Control-Allow-Origin" (responseHeaders response) `shouldBe` Just "https://www.tdfrecords.net"
        pure ResponseReceived
      _ <- app defaultRequest{requestHeaders=[("Origin","https://untrusted.invalid")]} $ \response -> do
        lookup "Access-Control-Allow-Origin" (responseHeaders response) `shouldNotBe` Just "*"
        lookup "Access-Control-Allow-Credentials" (responseHeaders response) `shouldNotBe` Just "true"
        pure ResponseReceived
      readIORef calls `shouldReturn` 1

withProductionCors :: IO a -> IO a
withProductionCors action = bracket
  (mapM (\(name,_) -> (,) name <$> lookupEnv name) values)
  (mapM_ (\(name,value) -> maybe (unsetEnv name) (setEnv name) value))
  (\_ -> mapM_ (uncurry setEnv) values >> action)
  where values = [("APP_ENV","production"),("ALLOW_ALL_ORIGINS","false"),
                  ("CORS_DISABLE_DEFAULTS","true"),("ALLOWED_ORIGINS","https://www.tdfrecords.net")]
