{-# LANGUAGE OverloadedStrings #-}

module TDF.StartupResponseSpec (spec) where

import Data.Aeson (Value, eitherDecode, object, (.=))
import Data.ByteString.Builder (toLazyByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Network.HTTP.Types (status503)
import Network.Wai (defaultRequest, pathInfo, responseHeaders, responseStatus, responseToStream)
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec
import TDF.App.StartupResponse (startupApp)

spec :: Spec
spec = describe "startup readiness" $ do
  it "rejects load-balancer readiness until initialization finishes" $ do
    captured <- newIORef Nothing
    _ <- startupApp (defaultRequest { pathInfo = ["health"] }) $ \response -> do
      writeIORef captured (Just response)
      pure ResponseReceived
    Just response <- readIORef captured
    responseStatus response `shouldBe` status503
    lookup "Retry-After" (responseHeaders response) `shouldBe` Just "5"
    lookup "Cache-Control" (responseHeaders response) `shouldBe` Just "no-store"
    body <- newIORef mempty
    let (_, _, withBody) = responseToStream response
    withBody $ \stream -> stream (\chunk -> modifyBody body chunk) (pure ())
    bytes <- toLazyByteString <$> readIORef body
    (eitherDecode bytes :: Either String Value) `shouldBe` Right (object
      [ "status" .= ("starting" :: Text)
      , "db" .= ("starting" :: Text)
      , "message" .= ("El servicio está arrancando. Intenta de nuevo en unos segundos." :: Text)
      ])
  it "keeps authentication and other requests unavailable during startup" $ do
    statuses <- newIORef []
    mapM_ (\path -> startupApp (defaultRequest { pathInfo = path }) $ \response -> do
      previous <- readIORef statuses
      writeIORef statuses (responseStatus response : previous)
      pure ResponseReceived) [["auth", "login"], ["version"], ["session"]]
    readIORef statuses `shouldReturn` replicate 3 status503
  where
    modifyBody ref chunk = do
      previous <- readIORef ref
      writeIORef ref (previous <> chunk)
