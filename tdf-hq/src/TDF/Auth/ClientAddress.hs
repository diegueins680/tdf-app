{-# LANGUAGE OverloadedStrings #-}

{- | The client network address used by the per-address login limit
(AUTH-CREDENTIAL-INTAKE-001).

The backend is reached through one reverse proxy: Caddy in production
(@ops/hetzner/Caddyfile.production@; the API container publishes no port) and
the Fly proxy in staging. Both put the address of the client they accepted the
connection from at the end of @X-Forwarded-For@. So:

  * when the TCP peer is a loopback or private address (the proxy), the last
    @X-Forwarded-For@ entry is the client, if it is a numeric address;
  * otherwise the peer itself is the client, and @X-Forwarded-For@ is
    ignored: anyone reaching the backend directly could write it.

Entries before the last one are whatever the client sent and are never used.
A deployment that puts a second proxy in front (a CDN) would make every
client look like that proxy; this module would then need its address range.

IPv6 clients are keyed by their /64 prefix, since one subscriber normally
holds a whole /64 and could rotate through it.
-}
module TDF.Auth.ClientAddress
  ( clientAddressKey
  , sockAddrKey
  , trustedPeer
  , lastForwardedEntry
  ) where

import           Control.Exception (SomeException, try)
import           Data.Bits         (shiftR, (.&.))
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Word         (Word16, Word8)
import           Numeric           (showHex)
import           Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(..)
  , SockAddr(..)
  , defaultHints
  , getAddrInfo
  , hostAddress6ToTuple
  , hostAddressToTuple
  )

-- | The key of the client behind this request.
clientAddressKey :: SockAddr -> Maybe Text -> IO Text
clientAddressKey peer forwarded
  | trustedPeer peer, Just entry <- forwarded >>= lastForwardedEntry = do
      parsed <- parseNumeric entry
      pure (maybe (sockAddrKey peer) sockAddrKey parsed)
  | otherwise = pure (sockAddrKey peer)

-- | The last comma-separated entry, if it can only be a numeric address.
lastForwardedEntry :: Text -> Maybe Text
lastForwardedEntry header =
  case reverse (T.splitOn "," header) of
    raw : _ ->
      let entry = T.strip raw
      in if not (T.null entry) && T.length entry <= 45 && T.all numericChar entry
           then Just entry
           else Nothing
    [] -> Nothing
  where
    numericChar c = c `elem` ("0123456789abcdefABCDEF:." :: String)

-- | No name lookup happens: only a numeric host is accepted.
parseNumeric :: Text -> IO (Maybe SockAddr)
parseNumeric entry = do
  result <- try (getAddrInfo (Just defaultHints { addrFlags = [AI_NUMERICHOST] }) (Just (T.unpack entry)) Nothing)
    :: IO (Either SomeException [AddrInfo])
  pure $ case result of
    Right (info : _) -> Just (addrAddress info)
    _ -> Nothing

-- | A loopback or private peer: the reverse proxy in front of the backend.
trustedPeer :: SockAddr -> Bool
trustedPeer address = case address of
  SockAddrInet _ host -> privateV4 (hostAddressToTuple host)
  SockAddrInet6 _ _ host _ -> case mappedV4 (hostAddress6ToTuple host) of
    Just v4 -> privateV4 v4
    Nothing ->
      let (a, b, c, d, e, f, g, h) = hostAddress6ToTuple host
      in (a, b, c, d, e, f, g, h) == (0, 0, 0, 0, 0, 0, 0, 1)
           || a .&. 0xfe00 == 0xfc00   -- unique local fc00::/7
           || a .&. 0xffc0 == 0xfe80   -- link local fe80::/10
  SockAddrUnix _ -> True
  where
    privateV4 (a, b, _, _) =
      a == 127 || a == 10 || (a == 172 && b >= 16 && b <= 31) || (a == 192 && b == 168)

sockAddrKey :: SockAddr -> Text
sockAddrKey address = case address of
  SockAddrInet _ host -> v4Text (hostAddressToTuple host)
  SockAddrInet6 _ _ host _ -> case mappedV4 (hostAddress6ToTuple host) of
    Just v4 -> v4Text v4
    Nothing ->
      let (a, b, c, d, _, _, _, _) = hostAddress6ToTuple host
      in T.intercalate ":" (map hex [a, b, c, d]) <> "::/64"
  SockAddrUnix _ -> "unix"
  where
    hex :: Word16 -> Text
    hex value = T.pack (showHex value "")

type V4 = (Word8, Word8, Word8, Word8)

v4Text :: V4 -> Text
v4Text (a, b, c, d) = T.intercalate "." (map (T.pack . show) [a, b, c, d])

-- | ::ffff:a.b.c.d, as a dual-stack listener reports an IPv4 peer.
mappedV4 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> Maybe V4
mappedV4 (0, 0, 0, 0, 0, 0xffff, hi, lo) =
  Just (byte (hi `shiftR` 8), byte hi, byte (lo `shiftR` 8), byte lo)
  where byte = fromIntegral . (.&. 0xff)
mappedV4 _ = Nothing
