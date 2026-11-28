{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | PAY-HOSTED-SESSION-001: the buyer address sent to PlaceToPay follows the
-- transport-verified Datafast rule when it can be established, and is absent
-- (the handler then sends the socket peer, as before) when it cannot.
module TDF.Commerce.BuyerNetworkAddressSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.IP as IP
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Header)
import qualified Network.Socket as Socket
import Network.Wai (defaultRequest, remoteHost, requestHeaders)
import Network.Wai.Test (SRequest(..), runSession, simpleBody, srequest)
import Servant hiding (Header)
import Test.Hspec
import Text.Read (readMaybe)

import TDF.Commerce.DatafastClientIP (DatafastClientRequest)
import TDF.Server.ProviderExecution (verifiedBuyerAddress)

type ProbeAPI = DatafastClientRequest :> Get '[PlainText] T.Text

-- | What the route hands the handler for this peer and these headers.
resolved :: Maybe String -> String -> [Header] -> IO T.Text
resolved configured peer headers = do
  let handler :: DatafastClientRequest -> Handler T.Text
      handler request = pure (maybe "unverified" id (verifiedBuyerAddress configured request))
      request = defaultRequest { remoteHost = address peer, requestHeaders = headers }
  response <- runSession (srequest (SRequest request "")) (serve (Proxy :: Proxy ProbeAPI) handler)
  pure (TE.decodeUtf8 (BL.toStrict (simpleBody response)))

address :: String -> Socket.SockAddr
address raw = case readMaybe raw :: Maybe IP.IP of
  Just ip -> IP.toSockAddr (ip, 12345)
  Nothing -> error "Invalid numeric test fixture"

spec :: Spec
spec = describe "PlaceToPay buyer network address (PAY-HOSTED-SESSION-001)" $ do
  let proxy = "172.18.0.3"
      header value = [("X-TDF-Client-IP", value)]
  it "uses the dedicated header from a listed proxy" $ do
    resolved (Just proxy) proxy (header "8.8.8.8") `shouldReturn` "8.8.8.8"
    resolved (Just proxy) proxy (header "2606:4700:4700::1111") `shouldReturn` "2606:4700:4700::1111"
  it "uses a public peer itself and ignores what it writes in the header" $ do
    resolved Nothing "8.8.4.4" [] `shouldReturn` "8.8.4.4"
    resolved Nothing "8.8.4.4" (header "1.1.1.1") `shouldReturn` "8.8.4.4"
    resolved (Just proxy) "8.8.4.4" (header "1.1.1.1") `shouldReturn` "8.8.4.4"
  it "is unverified behind a proxy that is not listed, whatever the headers say" $ do
    resolved Nothing proxy (header "8.8.8.8") `shouldReturn` "unverified"
    resolved (Just "172.18.0.9") proxy (header "8.8.8.8") `shouldReturn` "unverified"
    resolved Nothing proxy [("X-Forwarded-For", "8.8.8.8")] `shouldReturn` "unverified"
  it "is unverified for a missing, duplicate, chained or reserved header value" $ do
    resolved (Just proxy) proxy [] `shouldReturn` "unverified"
    resolved (Just proxy) proxy (header "8.8.8.8" <> header "1.1.1.1") `shouldReturn` "unverified"
    resolved (Just proxy) proxy (header "8.8.8.8, 1.1.1.1") `shouldReturn` "unverified"
    resolved (Just proxy) proxy (header "10.0.0.7") `shouldReturn` "unverified"
    resolved (Just proxy) proxy [("X-Forwarded-For", "8.8.8.8")] `shouldReturn` "unverified"
  it "is unverified, not an error, when the proxy setting is invalid" $
    resolved (Just "not-an-address") "8.8.4.4" [] `shouldReturn` "unverified"
