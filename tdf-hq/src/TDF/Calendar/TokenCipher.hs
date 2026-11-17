{-# LANGUAGE OverloadedStrings #-}

{- | Encryption at rest for the Google Calendar OAuth tokens kept in
@google_calendar_config@ (INTEG-GOOGLE-CALENDAR-001).

A sealed value is @enc:v1:@ followed by base64 of nonce (12 bytes), GCM tag
(16 bytes) and ciphertext, under AES-256-GCM with the SHA-256 of the
configured key text. A stored value without the prefix is a token written
before encryption existed: it is still readable, and is sealed the next time
it is written.
-}
module TDF.Calendar.TokenCipher
  ( CalendarTokenKey
  , CalendarTokenError(..)
  , calendarTokenKeyEnvVar
  , calendarTokenKeyFromText
  , loadCalendarTokenKey
  , calendarTokenIsSealed
  , sealedCalendarTokenLength
  , calendarTokenStoredLimit
  , sealCalendarTokenWithNonce
  , sealCalendarToken
  , openCalendarToken
  ) where

import           Crypto.Cipher.AES      (AES256)
import           Crypto.Cipher.Types    (AEADMode(AEAD_GCM), AuthTag(..), aeadInit,
                                         aeadSimpleDecrypt, aeadSimpleEncrypt, cipherInit)
import           Crypto.Error           (CryptoFailable(..))
import           Crypto.Hash            (Digest, SHA256, hash)
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Char              (isAscii, isPrint, isSpace)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           System.Entropy         (getEntropy)
import           System.Environment     (lookupEnv)

newtype CalendarTokenKey = CalendarTokenKey BS.ByteString

instance Show CalendarTokenKey where
  show _ = "CalendarTokenKey <redacted>"

data CalendarTokenError
  = CalendarTokenKeyMissing
  | CalendarTokenUnreadable
  deriving (Eq, Show)

calendarTokenKeyEnvVar :: String
calendarTokenKeyEnvVar = "GOOGLE_CALENDAR_TOKEN_KEY"

sealedPrefix :: Text
sealedPrefix = "enc:v1:"

-- | The key text must be 32 to 256 visible ASCII characters, like the other
-- at-rest keys of the backend.
calendarTokenKeyFromText :: Text -> Maybe CalendarTokenKey
calendarTokenKeyFromText raw
  | T.length raw >= 32 && T.length raw <= 256 && T.all visible raw =
      Just (CalendarTokenKey (BA.convert (hash (TE.encodeUtf8 raw) :: Digest SHA256)))
  | otherwise = Nothing
  where
    visible ch = isAscii ch && isPrint ch && not (isSpace ch)

loadCalendarTokenKey :: IO (Maybe CalendarTokenKey)
loadCalendarTokenKey =
  (>>= calendarTokenKeyFromText . T.pack) <$> lookupEnv calendarTokenKeyEnvVar

calendarTokenIsSealed :: Text -> Bool
calendarTokenIsSealed = T.isPrefixOf sealedPrefix

-- | Characters of the sealed form of a token of that many UTF-8 bytes: the
-- prefix, then base64 of nonce (12), tag (16) and ciphertext.
sealedCalendarTokenLength :: Int -> Int
sealedCalendarTokenLength plainBytes =
  T.length sealedPrefix + 4 * ((12 + 16 + plainBytes + 2) `div` 3)

-- | Longest stored value to accept for a token of at most @plainLimit@
-- characters: a sealed value is longer than the token it holds, so the limit
-- of the plain token must not be applied to it.
calendarTokenStoredLimit :: Int -> Text -> Int
calendarTokenStoredLimit plainLimit stored
  | calendarTokenIsSealed stored = sealedCalendarTokenLength plainLimit
  | otherwise = plainLimit

-- | Deterministic sealing for a caller-supplied 12-byte nonce.
sealCalendarTokenWithNonce :: CalendarTokenKey -> BS.ByteString -> Text -> Maybe Text
sealCalendarTokenWithNonce (CalendarTokenKey key) nonce plain
  | BS.length nonce /= 12 = Nothing
  | otherwise =
      case cipherInit key :: CryptoFailable AES256 of
        CryptoFailed _ -> Nothing
        CryptoPassed cipher ->
          case aeadInit AEAD_GCM cipher nonce of
            CryptoFailed _ -> Nothing
            CryptoPassed aead ->
              let (AuthTag tag, sealed) =
                    aeadSimpleEncrypt aead BS.empty (TE.encodeUtf8 plain) 16
              in Just (sealedPrefix <> TE.decodeUtf8 (B64.encode (nonce <> BA.convert tag <> sealed)))

sealCalendarToken :: CalendarTokenKey -> Text -> IO Text
sealCalendarToken key plain = do
  nonce <- getEntropy 12
  maybe (ioError (userError "Google Calendar token could not be sealed")) pure
    (sealCalendarTokenWithNonce key nonce plain)

-- | Read a stored token. A legacy plaintext value needs no key.
openCalendarToken :: Maybe CalendarTokenKey -> Text -> Either CalendarTokenError Text
openCalendarToken mKey stored =
  case T.stripPrefix sealedPrefix stored of
    Nothing -> Right stored
    Just encoded ->
      case mKey of
        Nothing -> Left CalendarTokenKeyMissing
        Just (CalendarTokenKey key) -> maybe (Left CalendarTokenUnreadable) Right $ do
          raw <- either (const Nothing) Just (B64.decode (TE.encodeUtf8 encoded))
          let (nonce, rest) = BS.splitAt 12 raw
              (tag, sealed) = BS.splitAt 16 rest
          if BS.length nonce /= 12 || BS.length tag /= 16 then Nothing else Just ()
          cipher <- passed (cipherInit key :: CryptoFailable AES256)
          aead <- passed (aeadInit AEAD_GCM cipher nonce)
          plain <- aeadSimpleDecrypt aead BS.empty sealed (AuthTag (BA.convert tag))
          either (const Nothing) Just (TE.decodeUtf8' plain)
  where
    passed (CryptoPassed value) = Just value
    passed (CryptoFailed _) = Nothing
