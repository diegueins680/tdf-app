{-# LANGUAGE OverloadedStrings #-}

-- | AUTH-WEBHOOK-001: Meta webhook bodies are trusted only with a configured
-- secret and a valid HMAC-SHA256 over the exact raw bytes; a missing secret
-- disables receipt (503) and never authentication.
module TDF.MetaWebhookSignatureSpec (spec) where

import           Crypto.Hash (SHA256)
import           Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Servant (ServerError (..))
import           Test.Hspec
import           Test.QuickCheck

import           TDF.API.Types (verifyMetaWebhookSignature)

newtype Secret = Secret T.Text deriving (Show)

instance Arbitrary Secret where
  arbitrary = Secret . T.pack <$> (chooseInt (1, 40) >>= \n -> vectorOf n (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_")))

newtype Body = Body BL.ByteString deriving (Show)

instance Arbitrary Body where
  arbitrary = Body . BL.pack <$> listOf arbitrary

digestFor :: T.Text -> BL.ByteString -> T.Text
digestFor secret body =
  T.pack (show (hmacGetDigest (hmac (TE.encodeUtf8 secret) (BL.toStrict body) :: HMAC SHA256)))

status :: Either ServerError () -> Maybe Int
status = either (Just . errHTTPCode) (const Nothing)

flipHex :: T.Text -> T.Text
flipHex digest = case T.uncons digest of
  Just (c, rest) -> T.cons (if c == '0' then '1' else '0') rest
  Nothing -> digest

spec :: Spec
spec = describe "verifyMetaWebhookSignature (AUTH-WEBHOOK-001)" $ do
  it "accepts exactly the HMAC of the exact body under the configured secret, in either hex case" $
    property $ \(Secret secret) (Body body) upper ->
      let digest = digestFor secret body
          header = "sha256=" <> (if upper then T.toUpper digest else digest)
      in verifyMetaWebhookSignature (Just secret) (Just header) body `shouldSatisfy` isRight
  it "answers 503 without a configured or blank secret, whatever the header says" $
    property $ \(Body body) mHeader blank ->
      let secret = if blank then Just "   " else Nothing
      in status (verifyMetaWebhookSignature secret (fmap T.pack mHeader) body) === Just 503
  it "answers 401 for a missing header" $
    property $ \(Secret secret) (Body body) ->
      status (verifyMetaWebhookSignature (Just secret) Nothing body) === Just 401
  it "answers 401 when any byte of the body, the secret or the digest differs" $
    property $ \(Secret secret) (Body body) ->
      let digest = digestFor secret body
          header = "sha256=" <> digest
          otherBody = BL.snoc body 0
          otherSecret = secret <> "x"
      in conjoin
           [ status (verifyMetaWebhookSignature (Just secret) (Just header) otherBody) === Just 401
           , status (verifyMetaWebhookSignature (Just otherSecret) (Just header) body) === Just 401
           , status (verifyMetaWebhookSignature (Just secret) (Just ("sha256=" <> flipHex digest)) body) === Just 401
           ]
  it "answers 401 for malformed headers" $
    property $ \(Secret secret) (Body body) ->
      let digest = digestFor secret body
          malformed =
            [ digest                                  -- no prefix
            , "sha1=" <> digest                       -- wrong algorithm
            , " sha256=" <> digest                    -- leading space
            , "sha256=" <> digest <> " "              -- trailing space
            , "sha256=" <> T.take 63 digest           -- short
            , "sha256=" <> digest <> "0"              -- long
            , "sha256=" <> T.replace "a" "g" (T.replace "0" "g" digest) -- non-hex
            , "SHA256=" <> digest                     -- prefix case
            ]
      in conjoin [status (verifyMetaWebhookSignature (Just secret) (Just h) body) === Just 401 | h <- malformed]
  it "never answers anything but success, 401 or 503" $
    property $ \(Secret secret) (Body body) mHeader configured ->
      status (verifyMetaWebhookSignature (if configured then Just secret else Nothing) (fmap T.pack mHeader) body)
        `elem` [Nothing, Just 401, Just 503]
  it "the body bytes are the exact raw payload: a one-byte difference is detected for every length" $
    property $ forAll (chooseInt (0, 64)) $ \n -> forAll (vectorOf n arbitrary) $ \bytes ->
      let body = BL.pack bytes
          secret = "secret"
          header = "sha256=" <> digestFor secret body
      in isRight (verifyMetaWebhookSignature (Just secret) (Just header) body)
           && all (\i -> status (verifyMetaWebhookSignature (Just secret) (Just header)
                                   (BL.fromStrict (flipByte i (BL.toStrict body)))) == Just 401)
                  [0 .. n - 1]
  where
    flipByte i bs = BS.concat [BS.take i bs, BS.singleton (BS.index bs i + 1), BS.drop (i + 1) bs]
