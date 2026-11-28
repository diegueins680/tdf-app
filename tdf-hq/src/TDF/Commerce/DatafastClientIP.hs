{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Transport-owned request metadata. Neither JSON nor a browser-supplied
-- header can construct the socket peer. A trusted proxy must overwrite the
-- one forwarded header; proxy trust is explicit and empty by default.
module TDF.Commerce.DatafastClientIP
  ( DatafastClientRequest
  , DatafastClientIP
  , TrustedDatafastProxies
  , parseTrustedDatafastProxies
  , resolveDatafastClientIP
  , datafastClientIPParameters
  , datafastClientIPText
  ) where

import Control.Monad (unless)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.IP as IP
import Data.List (nub)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import Servant (HasServer(..), (:>))
import Servant.Server.Internal (passToServer)
import Text.Read (readMaybe)

data DatafastClientRequest = DatafastClientRequest (Maybe IP.IP) [ByteString]

-- This combinator adds only an internal handler argument, not an HTTP input.
instance HasServer api context => HasServer (DatafastClientRequest :> api) context where
  type ServerT (DatafastClientRequest :> api) m = DatafastClientRequest -> ServerT api m
  route _ context server = route (Proxy :: Proxy api) context $
    passToServer server $ \request -> DatafastClientRequest
      (socketIP (Wai.remoteHost request))
      [value | (name, value) <- Wai.requestHeaders request, name == "X-TDF-Client-IP"]
  hoistServerWithContext _ context transform server request =
    hoistServerWithContext (Proxy :: Proxy api) context transform (server request)

newtype DatafastClientIP = DatafastClientIP IP.IP deriving (Eq, Show)
newtype TrustedDatafastProxies = TrustedDatafastProxies [IP.IP] deriving (Eq, Show)

parseTrustedDatafastProxies :: Maybe String -> Either Text TrustedDatafastProxies
parseTrustedDatafastProxies raw = do
  let value = T.strip (maybe "" T.pack raw)
      entries = if T.null value then [] else T.splitOn "," value
  unless (T.length value <= 1024 && length entries <= 16) (Left invalidProxyConfiguration)
  addresses <- traverse (maybe (Left invalidProxyConfiguration) Right . numericIP) entries
  unless (all usableProxyAddress addresses) (Left invalidProxyConfiguration)
  pure (TrustedDatafastProxies (nub addresses))
  where
    usableProxyAddress (IP.IPv4 address) = IP.fromIPv4w address /= 0
    usableProxyAddress (IP.IPv6 address) = IP.fromIPv6w address /= (0,0,0,0)

invalidProxyConfiguration :: Text
invalidProxyConfiguration =
  "DATAFAST_TRUSTED_PROXY_IPS must contain at most 16 exact numeric IP addresses"

resolveDatafastClientIP
  :: TrustedDatafastProxies -> DatafastClientRequest -> Either Text DatafastClientIP
resolveDatafastClientIP (TrustedDatafastProxies trusted) (DatafastClientRequest peer headers) = do
  observed <- maybe unavailable Right peer
  customer <- if observed `elem` trusted
    then case headers of
      [value] | BS.length value <= 64 && not (BS.any isControlByte value) ->
        either (const unavailable) (maybe unavailable Right . numericIP) (TE.decodeUtf8' value)
      _ -> unavailable
    else Right observed
  unless (customerAddress customer && customer `notElem` trusted) unavailable
  pure (DatafastClientIP customer)
  where
    unavailable = Left "Datafast customer network address could not be verified"
    isControlByte value = value < 32 || value == 127

datafastClientIPParameters :: DatafastClientIP -> [(ByteString, ByteString)]
datafastClientIPParameters (DatafastClientIP address) =
  [("customer.ip", TE.encodeUtf8 (T.pack (show address)))]

-- | The verified customer address in canonical text form, for providers
-- that take it as a JSON field (PlaceToPay).
datafastClientIPText :: DatafastClientIP -> Text
datafastClientIPText (DatafastClientIP address) = T.pack (show address)

socketIP :: Socket.SockAddr -> Maybe IP.IP
socketIP (Socket.SockAddrInet6 _ _ _ scope) | scope /= 0 = Nothing
socketIP address = normalizeIP . fst <$> IP.fromSockAddr address

numericIP :: Text -> Maybe IP.IP
numericIP raw = do
  let value = T.strip raw
  unless (not (T.null value) && T.length value <= 64 && T.all numericCharacter value) Nothing
  -- Dotted addresses must be the unambiguous four-octet decimal form,
  -- including the tail of IPv4-mapped IPv6 addresses.
  if T.any (== '.') value
    then unless (decimalIPv4 (lastComponent value)) Nothing
    else pure ()
  normalizeIP <$> readMaybe (T.unpack value)
  where
    numericCharacter c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
      || (c >= 'A' && c <= 'F') || c == ':' || c == '.'
    lastComponent value = case reverse (T.splitOn ":" value) of
      component:_ -> component
      [] -> ""
    decimalIPv4 value = case T.splitOn "." value of
      [a,b,c,d] -> all octet [a,b,c,d]
      _ -> False
    octet value = not (T.null value) && T.length value <= 3
      && T.all (\c -> c >= '0' && c <= '9') value
      && (T.length value == 1 || not ("0" `T.isPrefixOf` value))
      && maybe False (\number -> number >= (0 :: Int) && number <= 255)
        (readMaybe (T.unpack value))

normalizeIP :: IP.IP -> IP.IP
normalizeIP (IP.IPv6 address)
  | (0,0,0xffff,word) <- IP.fromIPv6w address = IP.IPv4 (IP.toIPv4w word)
normalizeIP address = address

-- Conservative customer unicast policy. Special-purpose/transition IPv6
-- ranges are not inferred to be customer addresses. See the PAY-DATAFAST-001
-- contract for scope and the IANA sources; this is not a routability proof.
customerAddress :: IP.IP -> Bool
customerAddress (IP.IPv4 address) = not $ any blocked
  [ (0x00000000,8), (0x0a000000,8), (0x64400000,10), (0x7f000000,8)
  , (0xa9fe0000,16), (0xac100000,12), (0xc0000000,24), (0xc0000200,24)
  , (0xc0586300,24), (0xc0a80000,16), (0xc6120000,15), (0xc6336400,24)
  , (0xcb007100,24), (0xe0000000,3)
  ]
  where blocked (network, prefix) = IP.isMatchedTo address
          (IP.makeAddrRange (IP.toIPv4w network) prefix)
customerAddress (IP.IPv6 address) =
  let (first,_,_,_) = IP.fromIPv6w address
  in first .&. 0xe0000000 == 0x20000000
    && first .&. 0xfffffe00 /= 0x20010000
    && first /= 0x20010db8
    && first .&. 0xfffff000 /= 0x3fff0000
