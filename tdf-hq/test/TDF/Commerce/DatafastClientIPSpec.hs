{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Commerce.DatafastClientIPSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Either (isLeft)
import Data.IORef (newIORef, modifyIORef', readIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.IP as IP
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Header, status200, status503)
import Network.HTTP.Types.URI (parseQuery)
import qualified Network.Socket as Socket
import Network.Wai (defaultRequest, remoteHost, requestHeaders, requestMethod)
import Network.Wai.Test (SRequest(..), SResponse, runSession, srequest, simpleStatus, simpleBody)
import Servant hiding (Header)
import Test.Hspec
import Test.QuickCheck (choose, forAll, ioProperty)
import Text.Read (readMaybe)

import TDF.Commerce.DatafastClientIP
import TDF.Commerce.DatafastConfig (encodeDatafastForm)

type ProbeAPI = DatafastClientRequest :> ReqBody '[JSON] Value :> Post '[PlainText] T.Text

probe :: Maybe String -> Socket.SockAddr -> [Header] -> IO (SResponse, Int)
probe configured peer headers = do
  policy <- either (fail . T.unpack) pure (parseTrustedDatafastProxies configured)
  calls <- newIORef (0 :: Int)
  let handler :: DatafastClientRequest -> Value -> Handler T.Text
      handler request _body = case resolveDatafastClientIP policy request of
        Left message -> throwError err503 { errBody = BL.fromStrict (TE.encodeUtf8 message) }
        Right client -> do
          liftIO (modifyIORef' calls (+1))
          pure (TE.decodeUtf8 (encodeDatafastForm (datafastClientIPParameters client)))
      app = serve (Proxy :: Proxy ProbeAPI) handler
      request = defaultRequest
        { remoteHost = peer, requestMethod = "POST"
        , requestHeaders = ("Content-Type", "application/json") : headers
        }
  response <- runSession (srequest (SRequest request
    "{\"customer.ip\":\"6.6.6.6\",\"remoteHost\":\"6.6.6.6\"}")) app
  count <- readIORef calls
  pure (response, count)

address :: String -> Socket.SockAddr
address raw = case readMaybe raw :: Maybe IP.IP of
  Just ip -> IP.toSockAddr (ip, 12345)
  Nothing -> error "Invalid numeric test fixture"

expectIP :: String -> (SResponse, Int) -> Expectation
expectIP expected (response, count) = do
  simpleStatus response `shouldBe` status200
  parseQuery (BL.toStrict (simpleBody response))
    `shouldBe` [("customer.ip", Just (B8.pack expected))]
  count `shouldBe` 1

expectRefused :: (SResponse, Int) -> Expectation
expectRefused (response, count) = do
  simpleStatus response `shouldBe` status503
  count `shouldBe` 0

spec :: Spec
spec = describe "Dataweb transport-owned customer IP" $ do
  it "uses the real socket peer and ignores browser JSON and all forwarded headers by default" $ do
    result <- probe Nothing (address "8.8.8.8")
      [ ("X-TDF-Client-IP", "1.1.1.1"), ("X-Forwarded-For", "1.1.1.1")
      , ("CF-Connecting-IP", "1.1.1.1"), ("Forwarded", "for=1.1.1.1") ]
    expectIP "8.8.8.8" result

  it "accepts one numeric address from an explicitly trusted proxy" $ do
    probe (Just "172.18.0.3") (address "172.18.0.3")
      [("X-TDF-Client-IP", "1.1.1.1")] >>= expectIP "1.1.1.1"

  it "does not trust another private peer on the same network" $ do
    forM_ [Nothing, Just "172.18.0.4"] $ \trusted ->
      probe trusted (address "172.18.0.3") [("X-TDF-Client-IP", "1.1.1.1")]
        >>= expectRefused

  it "refuses absent, duplicate, chained or malformed trusted-proxy assertions without fallback" $ do
    let assertions =
          [ [], [("X-TDF-Client-IP", "")]
          , [("X-TDF-Client-IP", "1.1.1.1"), ("x-tdf-client-ip", "8.8.8.8")]
          , [("X-TDF-Client-IP", "1.1.1.1, 8.8.8.8")]
          , [("X-TDF-Client-IP", "customer.example")]
          , [("X-TDF-Client-IP", "https://1.1.1.1")]
          , [("X-TDF-Client-IP", "1.1.1.1:443")]
          , [("X-TDF-Client-IP", "[2001:4860::1]:443")]
          , [("X-TDF-Client-IP", "2001:4860::1%en0")]
          , [("X-TDF-Client-IP", "1.1.1.1\r\n")]
          , [("X-TDF-Client-IP", BS.pack [255])]
          , [("X-TDF-Client-IP", B8.replicate 65 '1')]
          , [("X-TDF-Client-IP", "001.1.1.1")]
          , [("X-TDF-Client-IP", "2130706433")]
          ]
    forM_ assertions $ \headers ->
      probe (Just "172.18.0.3") (address "172.18.0.3") headers >>= expectRefused

  it "never substitutes X-Forwarded-For or another provider's edge header" $
    forM_ [[("X-Forwarded-For", "1.1.1.1")], [("CF-Connecting-IP", "1.1.1.1")],
           [("Forwarded", "for=1.1.1.1")]] $ \headers ->
      probe (Just "172.18.0.3") (address "172.18.0.3") headers >>= expectRefused

  it "does not let malformed spoofed headers deny service to an untrusted direct customer" $
    forM_ [BS.pack [255], "1.1.1.1, 2.2.2.2", "\r\n"] $ \header ->
      probe Nothing (address "8.8.8.8") [("X-TDF-Client-IP", header)] >>= expectIP "8.8.8.8"

  it "refuses local, documentation, reserved and proxy addresses before provider work" $ do
    let refused = ["0.0.0.0", "0.1.2.3", "10.1.2.3", "100.64.0.1", "127.0.0.1",
          "169.254.1.2", "172.16.0.1", "172.31.255.255", "192.168.1.1", "192.0.0.1",
          "192.0.2.1", "192.88.99.1", "198.18.0.1", "198.19.255.255", "198.51.100.1",
          "203.0.113.1", "224.0.0.1", "240.0.0.1", "255.255.255.255", "::", "::1",
          "fc00::1", "fe80::1", "fec0::1", "ff02::1", "2001:db8::1", "3fff::1",
          "2001:2::1", "::ffff:127.0.0.1", "::ffff:192.168.1.1"]
    forM_ refused $ \ip -> do
      probe Nothing (address ip) [] >>= expectRefused
      probe (Just "172.18.0.3") (address "172.18.0.3")
        [("X-TDF-Client-IP", B8.pack ip)] >>= expectRefused
    probe (Just "1.1.1.1") (address "1.1.1.1")
      [("X-TDF-Client-IP", "1.1.1.1")] >>= expectRefused

  it "normalizes native IPv6 and IPv4-mapped peers and proxy configuration" $ do
    probe Nothing (address "2001:4860:0:0:0:0:0:1") [] >>= expectIP "2001:4860::1"
    probe Nothing (address "::ffff:8.8.8.8") [] >>= expectIP "8.8.8.8"
    probe (Just "::ffff:172.18.0.3") (address "172.18.0.3")
      [("X-TDF-Client-IP", "2001:4860:0:0:0:0:0:1")] >>= expectIP "2001:4860::1"
    probe (Just "172.18.0.3") (address "::ffff:172.18.0.3")
      [("X-TDF-Client-IP", "::ffff:8.8.8.8")] >>= expectIP "8.8.8.8"

  it "refuses Unix and scoped IPv6 peers rather than inventing a server address" $ do
    probe Nothing (Socket.SockAddrUnix "synthetic") [] >>= expectRefused
    probe Nothing (Socket.SockAddrInet6 12345 0
      (IP.toHostAddress6 (IP.toIPv6w (0x20014860,0,0,1))) 1) [] >>= expectRefused

  it "validates bounded exact proxy addresses without echoing configured values" $ do
    forM_ ["not-an-ip-secret", "172.18.0.0/16", "*", "0.0.0.0", "::",
           "1.1.1.1,", "proxy.example", "1.1.1.1:443", replicate 1025 '1',
           T.unpack (T.intercalate "," (replicate 17 "172.18.0.3"))] $ \raw -> do
      let parsed = parseTrustedDatafastProxies (Just raw)
      parsed `shouldSatisfy` isLeft
      either T.unpack (const "unexpected admission") parsed `shouldNotContain` raw
    parseTrustedDatafastProxies Nothing `shouldBe` parseTrustedDatafastProxies (Just "")
    parseTrustedDatafastProxies (Just " 172.18.0.3,::ffff:172.18.0.3 ")
      `shouldBe` parseTrustedDatafastProxies (Just "172.18.0.3")

  it "keeps a direct customer's address independent of spoofed browser values" $
    forAll ((,,) <$> choose (0 :: Int,255) <*> choose (0 :: Int,255)
      <*> choose (0 :: Int,255)) $ \(a,b,c) ->
      ioProperty $ do
        let ip = "8." <> show (a :: Int) <> "." <> show b <> "." <> show c
        (response, count) <- probe Nothing (address ip)
          [("X-TDF-Client-IP", "1.1.1.1"), ("X-Forwarded-For", "6.6.6.6")]
        pure (count == 1 && simpleStatus response == status200
          && parseQuery (BL.toStrict (simpleBody response)) == [("customer.ip", Just (B8.pack ip))])
