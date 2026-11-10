{-# LANGUAGE OverloadedStrings #-}
module TDF.SocialTransportSpec (spec) where

import Control.Concurrent (newEmptyMVar, readMVar, tryPutMVar)
import Control.Concurrent.Async (AsyncCancelled, cancel, waitCatch, withAsync)
import Control.Exception (SomeException, bracket, finally, fromException)
import Control.Monad (forM_, void)
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (status200, status307, status500)
import Network.Wai (Application, responseLBS, responseRaw, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Timeout (timeout)
import Test.Hspec
import TDF.Config (AppConfig(..))
import TDF.EventOperations.HttpTestConfig (httpTestConfig)
import TDF.Services.MessagingManager (messagingManagerSettings)
import TDF.Services.FacebookMessaging (sendFacebookTextUsing)
import TDF.Services.InstagramMessaging (sendInstagramTextWithContextAndTagUsing)
import qualified TDF.WhatsApp.Client as WhatsApp

-- Every endpoint is an owned loopback server; no host credentials are loaded.
withSender :: String -> Int -> (IO (Either Text ()) -> IO a) -> IO a
withSender channel port action =
  bracket
    (HTTP.newManager (HTTP.managerSetProxy HTTP.noProxy messagingManagerSettings)
      { HTTP.managerModifyRequest = \request -> pure request
          { HTTP.host = "127.0.0.1", HTTP.port = port, HTTP.secure = False }
      })
    HTTP.closeManager $ \manager ->
      let cfg = httpTestConfig
            { instagramMessagingApiBase = "https://fixture.example.invalid"
            , instagramMessagingToken = Just "fixture-configured-token"
            , instagramMessagingAccountId = Just "configured-account"
            , facebookMessagingApiBase = "https://fixture.example.invalid"
            , facebookMessagingToken = Just "fixture-facebook-token"
            , facebookMessagingPageId = Just "fixture-page"
            }
      in case channel of
        "instagram" -> action (fmap (fmap (const ())) $
          sendInstagramTextWithContextAndTagUsing manager cfg (Just "fixture-connected-token")
            (Just "connected-account") "fixture-recipient" "synthetic body" Nothing)
        "facebook" -> action (fmap (fmap (const ())) $
          sendFacebookTextUsing manager cfg "fixture-recipient" "synthetic body")
        "whatsapp" -> action $ do
          result <- WhatsApp.sendText manager "v20.0" "fixture-whatsapp-token"
            "123456789" "+593999999999" "synthetic body"
          pure (either (Left . T.pack) (const (Right ())) result)
        _ -> fail "Unknown synthetic channel"

counted :: IO () -> Application -> Application
counted accepted app request respond = do
  _ <- strictRequestBody request
  accepted
  app request respond

spec :: Spec
spec = describe "social transport dispatch boundary" $
  forM_ ["instagram", "facebook", "whatsapp"] $ \channel -> describe channel $ do
    it "does not dispatch a fallback after acceptance followed by a lost response" $ do
      calls <- newIORef (0 :: Int)
      let accepted = atomicModifyIORef' calls (\n -> (n + 1, ()))
          closeWithoutReply _ respond = respond $
            responseRaw (\_ _ -> pure ()) (responseLBS status500 [] "synthetic fallback")
      testWithApplication (pure (counted accepted closeWithoutReply)) $ \port ->
        withSender channel port $ \send -> do
          result <- send
          result `shouldSatisfy` isLeft
          result `shouldSatisfy` either (T.isInfixOf "outcome unknown") (const False)
          show result `shouldNotContain` "fixture-connected-token"
          show result `shouldNotContain` "fixture-facebook-token"
          show result `shouldNotContain` "fixture-whatsapp-token"
          readIORef calls `shouldReturn` 1
    it "does not follow a redirect with a second message request" $ do
      calls <- newIORef (0 :: Int)
      let accepted = atomicModifyIORef' calls (\n -> (n + 1, ()))
          redirect _ respond = respond (responseLBS status307 [("Location", "/other-account/messages")] "{}")
      testWithApplication (pure (counted accepted redirect)) $ \port ->
        withSender channel port $ \send -> do
          send >>= (`shouldSatisfy` isLeft)
          readIORef calls `shouldReturn` 1
    it "does not replay a lost reply on a previously warmed pooled connection" $ do
      calls <- newIORef (0 :: Int)
      let pooled _ respond = do
            number <- atomicModifyIORef' calls (\n -> (n + 1, n + 1))
            if number == 1
              then respond (responseLBS status200 [] "{\"messages\":[{\"id\":\"synthetic\"}]}")
              else respond $ responseRaw (\_ _ -> pure ()) (responseLBS status500 [] "synthetic fallback")
      testWithApplication (pure (counted (pure ()) pooled)) $ \port ->
        withSender channel port $ \send -> do
          send `shouldReturn` Right ()
          send >>= (`shouldSatisfy` isLeft)
          readIORef calls `shouldReturn` 2
    it "propagates cancellation after dispatch without a fallback" $ do
      accepted <- newEmptyMVar
      release <- newEmptyMVar
      calls <- newIORef (0 :: Int)
      let record = atomicModifyIORef' calls (\n -> (n + 1, ())) >> void (tryPutMVar accepted ())
          hold _ respond = readMVar release >> respond (responseLBS status200 [] "{\"messages\":[{\"id\":\"synthetic\"}]}")
      testWithApplication (pure (counted record hold)) $ \port ->
        withSender channel port $ \send -> withAsync send $ \worker ->
          (do
            timeout 2000000 (readMVar accepted) `shouldReturn` Just ()
            timeout 2000000 (cancel worker) `shouldReturn` Just ()
            result <- waitCatch worker
            result `shouldSatisfy` either isCancellation (const False)
            readIORef calls `shouldReturn` 1
          ) `finally` void (tryPutMVar release ())
  where
    isCancellation :: SomeException -> Bool
    isCancellation err = isJust (fromException err :: Maybe AsyncCancelled)
