{-# LANGUAGE OverloadedStrings #-}

-- | Guest ticket order access keys. The key is derived, never stored: the
-- checkout keeps only its SHA-256, so the purchase response and the
-- confirmation email can both hand it to the buyer without a second copy.
module TDF.Ticketing.LookupToken
  ( deriveTicketLookupToken
  , ticketLookupTokenMatches
  , ticketLookupTokenSecretFromEnv
  , ticketOrderAccessUrl
  ) where

import           Crypto.Hash (Digest, SHA256, hash)
import           Crypto.MAC.HMAC (HMAC, hmac)
import           Data.ByteArray (constEq)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.Environment (lookupEnv)

deriveTicketLookupToken :: BS.ByteString -> Text -> Int64 -> Either Text Text
deriveTicketLookupToken secret idempotencyKey eventId
  | BS.length secret < 32 = Left "Ticket lookup secret must contain at least 32 bytes"
  | otherwise = Right $ TE.decodeUtf8 $ BAE.convertToBase BAE.Base16
      (hmac secret (TE.encodeUtf8
        ("event-ticket-order-lookup:" <> idempotencyKey <> ":" <> T.pack (show eventId)))
        :: HMAC SHA256)

-- | Whether a key opens the order whose stored hash is given. A rotated
-- secret derives a different key; callers must not hand that one out.
ticketLookupTokenMatches :: Text -> Text -> Bool
ticketLookupTokenMatches storedHash token =
  constEq (TE.encodeUtf8 storedHash) (TE.encodeUtf8 (sha256Hex token))
  where
    sha256Hex value = TE.decodeUtf8 $ BAE.convertToBase BAE.Base16
      (hash (TE.encodeUtf8 value) :: Digest SHA256)

-- | The configured secret, or Nothing when it is absent or too short.
ticketLookupTokenSecretFromEnv :: IO (Maybe BS.ByteString)
ticketLookupTokenSecretFromEnv = do
  configured <- lookupEnv "COMMERCE_LOOKUP_TOKEN_SECRET"
  pure $ case TE.encodeUtf8 . T.pack <$> configured of
    Just secret | BS.length secret >= 32 -> Just secret
    _ -> Nothing

-- | The buyer's order page with its key after "#": a fragment is not sent to
-- the server, so it stays out of request logs and referrers; the page stores
-- the key on that device and removes it from the address bar.
ticketOrderAccessUrl :: Text -> Int64 -> Int64 -> Text -> Text
ticketOrderAccessUrl base eventId orderId token =
  T.dropWhileEnd (== '/') base <> "/eventos/" <> T.pack (show eventId)
    <> "/orden/" <> T.pack (show orderId) <> "#acceso=" <> token
