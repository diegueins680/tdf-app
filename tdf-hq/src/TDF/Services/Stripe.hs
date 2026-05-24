{-# LANGUAGE OverloadedStrings #-}

module TDF.Services.Stripe
  ( StripeConfig(..)
  , createPaymentIntent
  , createRefund
  , decodeStripeResponse
  , verifyWebhookSignature
  ) where

import           Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           Crypto.Hash.Algorithms (SHA256)
import           Crypto.MAC.HMAC (HMAC, hmac)
import           Data.ByteArray (convert)

-- | Configuration for Stripe API
data StripeConfig = StripeConfig
  { stripeSecretKey     :: Text
  , stripeWebhookSecret :: Text
  , stripeApiVersion    :: Text
  } deriving (Show, Eq)

-- | Create a Stripe PaymentIntent
-- Returns the full PaymentIntent JSON response
createPaymentIntent
  :: StripeConfig
  -> Int         -- ^ Amount in cents
  -> Text        -- ^ Currency (e.g., "usd")
  -> Text        -- ^ Description
  -> Maybe Text  -- ^ Optional metadata JSON string
  -> IO (Either Text Value)
createPaymentIntent cfg amountCents currency description mMetadata = do
  manager <- newManager tlsManagerSettings

  let url = "https://api.stripe.com/v1/payment_intents"
      body = BS8.intercalate "&"
        [ "amount=" <> BS8.pack (show amountCents)
        , "currency=" <> TE.encodeUtf8 (T.toLower currency)
        , "description=" <> urlEncode (TE.encodeUtf8 description)
        , "automatic_payment_methods[enabled]=true"
        ] <> maybe "" (\meta -> "&metadata=" <> urlEncode (TE.encodeUtf8 meta)) mMetadata

  initialRequest <- parseRequest url
  let request = initialRequest
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", "Bearer " <> TE.encodeUtf8 (stripeSecretKey cfg))
            , ("Content-Type", "application/x-www-form-urlencoded")
            , ("Stripe-Version", TE.encodeUtf8 (stripeApiVersion cfg))
            ]
        , requestBody = RequestBodyBS body
        }

  response <- httpLbs request manager
  let status = statusCode (responseStatus response)
      bodyLBS = responseBody response

  pure $
    decodeStripeResponse
      status
      bodyLBS
      "Failed to parse Stripe PaymentIntent response"
      "Stripe API error"

-- | Create a refund for a PaymentIntent
createRefund
  :: StripeConfig
  -> Text  -- ^ PaymentIntent ID
  -> Int   -- ^ Amount in cents to refund
  -> IO (Either Text Value)
createRefund cfg paymentIntentId amountCents = do
  manager <- newManager tlsManagerSettings

  let url = "https://api.stripe.com/v1/refunds"
      body = BS8.intercalate "&"
        [ "payment_intent=" <> TE.encodeUtf8 paymentIntentId
        , "amount=" <> BS8.pack (show amountCents)
        ]

  initialRequest <- parseRequest url
  let request = initialRequest
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", "Bearer " <> TE.encodeUtf8 (stripeSecretKey cfg))
            , ("Content-Type", "application/x-www-form-urlencoded")
            , ("Stripe-Version", TE.encodeUtf8 (stripeApiVersion cfg))
            ]
        , requestBody = RequestBodyBS body
        }

  response <- httpLbs request manager
  let status = statusCode (responseStatus response)
      bodyLBS = responseBody response

  pure $
    decodeStripeResponse
      status
      bodyLBS
      "Failed to parse Stripe refund response"
      "Stripe refund error"

-- | Decode a Stripe response while preserving deterministic errors for
-- malformed upstream bodies.
decodeStripeResponse
  :: Int
  -> BL.ByteString
  -> Text
  -> Text
  -> Either Text Value
decodeStripeResponse status bodyLBS parseFailureMessage errorPrefix
  | status == 200 =
      maybe (Left parseFailureMessage) Right decodedBody
  | otherwise =
      Left $
        maybe
          (errorPrefix <> " with status: " <> statusText)
          (\val -> errorPrefix <> ": " <> T.pack (show val))
          decodedBody
  where
    decodedBody = Aeson.decode bodyLBS :: Maybe Value
    statusText = T.pack (show status)

-- | Verify Stripe webhook signature
-- This prevents replay attacks and ensures the webhook is from Stripe
verifyWebhookSignature
  :: StripeConfig
  -> Text       -- ^ Stripe-Signature header value
  -> ByteString -- ^ Raw request body
  -> Bool
verifyWebhookSignature cfg signatureHeader rawBody =
  case parseSignatureHeader signatureHeader of
    Nothing -> False
    Just (timestamp, signature) ->
      let signedPayload = TE.encodeUtf8 timestamp <> "." <> rawBody
          secret = TE.encodeUtf8 (stripeWebhookSecret cfg)
          expectedSig = convert (hmac secret signedPayload :: HMAC SHA256)
          expectedSigHex = Base16.encode expectedSig
      in expectedSigHex == TE.encodeUtf8 signature

-- | Parse the Stripe-Signature header
-- Format: "t=1492774577,v1=5257a869e7ecebeda32affa62cdca3fa51cad7e77a0e56ff536d0ce8e108d8bd"
parseSignatureHeader :: Text -> Maybe (Text, Text)
parseSignatureHeader header =
  let parts = T.splitOn "," header
      pairs = map (T.breakOn "=") parts
      findValue key = lookup key [(T.strip k, T.drop 1 v) | (k, v) <- pairs, not (T.null v)]
  in case (findValue "t", findValue "v1") of
    (Just timestamp, Just sig) -> Just (timestamp, sig)
    _ -> Nothing

-- | URL-encode a ByteString (simple implementation for form data)
urlEncode :: ByteString -> ByteString
urlEncode = BS.concatMap encode1
  where
    encode1 c
      | c >= 65 && c <= 90 = BS.singleton c  -- A-Z
      | c >= 97 && c <= 122 = BS.singleton c  -- a-z
      | c >= 48 && c <= 57 = BS.singleton c  -- 0-9
      | c == 45 || c == 46 || c == 95 || c == 126 = BS.singleton c  -- - . _ ~
      | otherwise = BS8.pack $ '%' : toHex c

    toHex w = [hexDigit (w `div` 16), hexDigit (w `mod` 16)]

    hexDigit n
      | n < 10 = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')
