{-# LANGUAGE OverloadedStrings #-}

module TDF.Services.Stripe
  ( StripeConfig(..)
  , defaultStripeApiVersion
  , createCustomer
  , createEphemeralKey
  , createPaymentIntent
  , createPaymentIntentForCustomer
  , createRefund
  , decodeStripeResponse
  , verifyWebhookSignature
  ) where

import           Control.Exception (bracket)
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

-- | Configuration for Stripe API.
--
-- 'stripeSecretKey' accepts either a secret key (sk_) or a restricted key (rk_);
-- both authenticate as bearer tokens at the same endpoints. Production deployments
-- should prefer restricted keys scoped to only the resources this service touches
-- (PaymentIntents, Refunds, Customers, and Webhook Endpoints).
data StripeConfig = StripeConfig
  { stripeSecretKey     :: Text
  , stripeWebhookSecret :: Text
  , stripeApiVersion    :: Text
  } deriving (Show, Eq)

-- | The Stripe API version this service is written against. Centralized so
-- the value moves in one place when upgrading. See <https://docs.stripe.com/changelog>.
defaultStripeApiVersion :: Text
defaultStripeApiVersion = "2026-04-22.dahlia"

-- | Resource invariant: every manager allocated for a single Stripe request is
-- closed exactly once, even if request construction, I/O, or response decoding
-- throws. The callback must not retain the manager after it returns.
withStripeManager :: (Manager -> IO a) -> IO a
withStripeManager =
  bracket (newManager tlsManagerSettings) closeManager

-- | Create a Stripe PaymentIntent
--
-- Preconditions:
-- * amount is expressed in the smallest currency unit accepted by Stripe.
-- * currency is a Stripe currency code; it is normalized to lowercase.
-- * metadata, when present, is already serialized into the expected form field.
--
-- Postcondition: the returned Either is produced by 'decodeStripeResponse', so
-- successful HTTP responses must contain valid JSON and error responses never
-- parse as success.
-- Returns the full PaymentIntent JSON response
createPaymentIntent
  :: StripeConfig
  -> Int         -- ^ Amount in cents
  -> Text        -- ^ Currency (e.g., "usd")
  -> Text        -- ^ Description
  -> Maybe Text  -- ^ Optional metadata JSON string
  -> IO (Either Text Value)
createPaymentIntent cfg amountCents currency description mMetadata =
  withStripeManager $ \manager -> do
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

-- | Create a Stripe Customer.
--
-- Returns the full Customer JSON response; caller is expected to extract @id@
-- (a @cus_*@ identifier) and persist it. Customers are the attachment point for
-- saved payment methods, PaymentSheet ephemeral keys, and Subscriptions.
--
-- Preconditions:
-- * email, when present, is a syntactically valid email; Stripe does not validate
--   deliverability.
-- * metadata, when present, is already serialized into the form-field shape
--   Stripe expects (same convention as 'createPaymentIntent').
createCustomer
  :: StripeConfig
  -> Maybe Text  -- ^ Optional email
  -> Maybe Text  -- ^ Optional display name
  -> Maybe Text  -- ^ Optional metadata JSON string
  -> IO (Either Text Value)
createCustomer cfg mEmail mName mMetadata =
  withStripeManager $ \manager -> do
    let url = "https://api.stripe.com/v1/customers"
        body = BS.intercalate "&" $ filter (not . BS.null)
          [ maybe "" (\e -> "email=" <> urlEncode (TE.encodeUtf8 e)) mEmail
          , maybe "" (\n -> "name=" <> urlEncode (TE.encodeUtf8 n)) mName
          , maybe "" (\m -> "metadata=" <> urlEncode (TE.encodeUtf8 m)) mMetadata
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
        "Failed to parse Stripe Customer response"
        "Stripe customer error"

-- | Create a PaymentSheet-style PaymentIntent attached to a Customer.
--
-- Identical to 'createPaymentIntent' except it sets @customer=cus_*@, which is
-- required for PaymentSheet to display saved payment methods and to persist
-- methods chosen during checkout.
--
-- Preconditions:
-- * customerId is a non-empty Stripe Customer id.
-- * amount is expressed in the smallest currency unit accepted by Stripe.
-- * metadata, when present, is already serialized into the expected form field.
--
-- Postcondition: the outgoing request pins the service Stripe API version from
-- 'StripeConfig', includes the customer exactly once, and returns the same
-- JSON/error contract as 'createPaymentIntent'.
createPaymentIntentForCustomer
  :: StripeConfig
  -> Text        -- ^ Stripe Customer ID (cus_*)
  -> Int         -- ^ Amount in cents
  -> Text        -- ^ Currency (e.g., "usd")
  -> Text        -- ^ Description
  -> Maybe Text  -- ^ Optional metadata JSON string
  -> IO (Either Text Value)
createPaymentIntentForCustomer cfg customerId amountCents currency description mMetadata =
  withStripeManager $ \manager -> do
    let url = "https://api.stripe.com/v1/payment_intents"
        body = BS8.intercalate "&"
          [ "amount=" <> BS8.pack (show amountCents)
          , "currency=" <> TE.encodeUtf8 (T.toLower currency)
          , "customer=" <> urlEncode (TE.encodeUtf8 customerId)
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

-- | Create a Stripe Ephemeral Key for a Customer for mobile PaymentSheet.
--
-- Resource invariant: HTTP managers are acquired only through
-- 'withStripeManager', which brackets 'newManager' and 'closeManager'. The
-- manager does not escape the callback.
--
-- Preconditions:
-- * customerId is a non-empty Stripe Customer id.
-- * clientStripeVersion is the exact @Stripe-Version@ required by the mobile
--   SDK, not the merchant's pinned 'defaultStripeApiVersion'. The mobile client
--   provides its expected version. See
--   <https://docs.stripe.com/payments/accept-a-payment?platform=react-native>.
--
-- Postcondition: the outgoing request sends @clientStripeVersion@ in the
-- @Stripe-Version@ header and returns the same JSON/error contract as
-- 'decodeStripeResponse'.
--
-- Returns the full ephemeral key JSON; the mobile SDK only needs the @secret@
-- field, but the whole object is forwarded so we don't need to update this
-- function when the SDK starts reading new fields.
createEphemeralKey
  :: StripeConfig
  -> Text  -- ^ Stripe Customer ID (cus_*)
  -> Text  -- ^ Stripe-Version expected by the calling mobile SDK
  -> IO (Either Text Value)
createEphemeralKey cfg customerId clientStripeVersion =
  withStripeManager $ \manager -> do
    let url = "https://api.stripe.com/v1/ephemeral_keys"
        body = "customer=" <> urlEncode (TE.encodeUtf8 customerId)

    initialRequest <- parseRequest url
    let request = initialRequest
          { method = "POST"
          , requestHeaders =
              [ ("Authorization", "Bearer " <> TE.encodeUtf8 (stripeSecretKey cfg))
              , ("Content-Type", "application/x-www-form-urlencoded")
              , ("Stripe-Version", TE.encodeUtf8 clientStripeVersion)
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
        "Failed to parse Stripe Ephemeral Key response"
        "Stripe ephemeral key error"

-- | Create a refund for a PaymentIntent
--
-- Preconditions:
-- * paymentIntentId is an existing Stripe PaymentIntent id.
-- * amount is expressed in the smallest currency unit accepted by Stripe.
--
-- Postcondition: the returned Either follows the same JSON/error contract as
-- 'createPaymentIntent'.
createRefund
  :: StripeConfig
  -> Text  -- ^ PaymentIntent ID
  -> Int   -- ^ Amount in cents to refund
  -> IO (Either Text Value)
createRefund cfg paymentIntentId amountCents =
  withStripeManager $ \manager -> do
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
--
-- Contract:
-- * status 200 succeeds only when the body decodes as JSON.
-- * status 200 with malformed JSON returns parseFailureMessage.
-- * non-200 statuses always return Left, preserving decoded Stripe error JSON
--   when present and otherwise preserving the HTTP status in the error text.
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

-- | Verify Stripe webhook signature.
-- This checks authenticity of the raw body; timestamp freshness is a separate
-- replay-protection concern.
--
-- Contract: returns True exactly when the header contains both "t" and "v1",
-- and v1 is the lowercase hex HMAC-SHA256 of @t <> "." <> rawBody@ using the
-- configured webhook secret.
verifyWebhookSignature
  :: StripeConfig
  -> Text       -- ^ Stripe-Signature header value
  -> ByteString -- ^ Raw request body
  -> Bool
verifyWebhookSignature cfg signatureHeader rawBody =
  maybe False matchesSignature (parseSignatureHeader signatureHeader)
  where
    matchesSignature (timestamp, signature) =
      let signedPayload = TE.encodeUtf8 timestamp <> "." <> rawBody
          secret = TE.encodeUtf8 (stripeWebhookSecret cfg)
          expectedSig = convert (hmac secret signedPayload :: HMAC SHA256)
          expectedSigHex = Base16.encode expectedSig
      in expectedSigHex == TE.encodeUtf8 signature

-- | Parse the Stripe-Signature header
-- Format: "t=1492774577,v1=5257a869e7ecebeda32affa62cdca3fa51cad7e77a0e56ff536d0ce8e108d8bd"
parseSignatureHeader :: Text -> Maybe (Text, Text)
parseSignatureHeader header =
  (,) <$> findValue "t" <*> findValue "v1"
  where
    parts = T.splitOn "," header
    pairs = map (T.breakOn "=") parts
    findValue key =
      lookup key [(T.strip k, T.drop 1 v) | (k, v) <- pairs, not (T.null v)]

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
