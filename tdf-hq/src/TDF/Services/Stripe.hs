{-# LANGUAGE OverloadedStrings #-}

module TDF.Services.Stripe
  ( StripeConfig(..)
  , defaultStripeApiVersion
  , createAccountLink
  , createCheckoutSessionForSubscription
  , createConnectExpressAccount
  , createCustomer
  , createEphemeralKey
  , createPaymentIntent
  , createPaymentIntentForCustomer
  , createPaymentIntentForTip
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
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           Crypto.Hash.Algorithms (SHA256)
import           Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)

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

type StripeAccountId = Text
type StripeAmountCents = Int
type StripeCountryCode = Text
type StripeCurrencyCode = Text
type StripeMetadataForm = Text
type StripeRedirectUrl = Text

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

-- | Create a Stripe Checkout Session in @subscription@ mode.
--
-- Returns the full Session JSON; the caller usually only needs the @url@ field
-- (hosted Checkout page to redirect the buyer to).
--
-- Preconditions:
-- * priceId is an existing recurring Stripe Price (@price_*@).
-- * customerId is an existing Stripe Customer (@cus_*@). PaymentSheet-style
--   anonymous sessions would use @customer_email@ instead — for the course
--   subscription flow we always have a Party-backed customer.
-- * successUrl and cancelUrl are absolute https URLs the browser can reach.
-- * metadata, when present, is a serialized form-field metadata block.
createCheckoutSessionForSubscription
  :: StripeConfig
  -> Text        -- ^ Stripe Price ID (price_*)
  -> Text        -- ^ Stripe Customer ID (cus_*)
  -> Text        -- ^ success URL
  -> Text        -- ^ cancel URL
  -> Maybe Text  -- ^ optional metadata JSON string
  -> IO (Either Text Value)
createCheckoutSessionForSubscription cfg priceId customerId successUrl cancelUrl mMetadata =
  withStripeManager $ \manager -> do
    let url = "https://api.stripe.com/v1/checkout/sessions"
        body = BS8.intercalate "&"
          [ "mode=subscription"
          , "customer=" <> urlEncode (TE.encodeUtf8 customerId)
          , "line_items[0][price]=" <> urlEncode (TE.encodeUtf8 priceId)
          , "line_items[0][quantity]=1"
          , "success_url=" <> urlEncode (TE.encodeUtf8 successUrl)
          , "cancel_url=" <> urlEncode (TE.encodeUtf8 cancelUrl)
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
        "Failed to parse Stripe Checkout Session response"
        "Stripe checkout session error"

-- | Create a Stripe Express payout account for an artist receiving tips.
--
-- Resource invariant:
-- * validates local preconditions before allocating an HTTP manager;
-- * allocates the manager only through 'withStripeManager', which brackets
--   'newManager' with 'closeManager';
-- * the manager cannot escape the callback.
--
-- Preconditions:
-- * country is an ISO 3166-1 alpha-2 country code; it is normalized to
--   uppercase before the request is sent.
-- * email, when present, is already accepted by the calling signup/profile
--   flow.
-- * metadata, when present, is already serialized into Stripe form fields.
--
-- Returns the full account JSON; caller persists @id@ (@acct_*@) and immediately
-- requests an 'createAccountLink' for onboarding.
createConnectExpressAccount
  :: StripeConfig
  -> StripeCountryCode     -- ^ ISO 3166-1 alpha-2 country code (e.g. "EC", "US")
  -> Maybe Text            -- ^ Optional email to prefill on the onboarding form
  -> Maybe StripeMetadataForm
  -> IO (Either Text Value)
createConnectExpressAccount cfg country mEmail mMetadata =
  case normalizeStripeCountryCode country of
    Left err -> pure (Left err)
    Right countryCode ->
      withStripeManager $ \manager -> do
        let url = "https://api.stripe.com/v1/accounts"
            body = BS8.intercalate "&" $ filter (not . BS.null)
              [ "type=express"
              , "country=" <> urlEncode (TE.encodeUtf8 countryCode)
              , "capabilities[card_payments][requested]=true"
              , "capabilities[transfers][requested]=true"
              , maybe "" (\e -> "email=" <> urlEncode (TE.encodeUtf8 e)) mEmail
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
            "Failed to parse Stripe Account response"
            "Stripe account error"

-- | Create a Stripe Account Link for hosted payout onboarding. The returned
-- URL is single-use and short-lived (~minutes); always generate one fresh when
-- the artist clicks the onboarding CTA.
--
-- Resource invariant: validates local fields before allocation and then uses
-- only 'withStripeManager' for the HTTP manager lifetime.
--
-- Preconditions:
-- * accountId is a normalized @acct_*@ identifier.
-- * refreshUrl and returnUrl are non-blank URLs accepted by the configured
--   Stripe environment.
--
-- Postcondition: all outcomes use 'decodeStripeResponse'; malformed success
-- bodies never parse as successful JSON.
createAccountLink
  :: StripeConfig
  -> StripeAccountId   -- ^ Stripe account id (acct_*)
  -> StripeRedirectUrl -- ^ refresh URL; Stripe sends the user here if the link expires
  -> StripeRedirectUrl -- ^ return URL; Stripe sends the user here after they finish
  -> IO (Either Text Value)
createAccountLink cfg accountId refreshUrl returnUrl =
  case (,,)
    <$> normalizeStripeAccountId accountId
    <*> normalizeRequiredStripeText "refreshUrl" refreshUrl
    <*> normalizeRequiredStripeText "returnUrl" returnUrl of
      Left err -> pure (Left err)
      Right (accountIdValue, refreshUrlValue, returnUrlValue) ->
        withStripeManager $ \manager -> do
          let url = "https://api.stripe.com/v1/account_links"
              body = BS8.intercalate "&"
                [ "account=" <> urlEncode (TE.encodeUtf8 accountIdValue)
                , "refresh_url=" <> urlEncode (TE.encodeUtf8 refreshUrlValue)
                , "return_url=" <> urlEncode (TE.encodeUtf8 returnUrlValue)
                , "type=account_onboarding"
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
              "Failed to parse Stripe Account Link response"
              "Stripe account link error"

-- | Create a PaymentIntent for a destination charge tip. The platform collects
-- @applicationFeeCents@, and the rest is routed to the artist payout account
-- via @transfer_data[destination]@.
--
-- Resource invariant: local preconditions are checked before manager
-- allocation; the network phase uses 'withStripeManager' so cleanup is
-- guaranteed on success, failure, and exceptions.
--
-- Preconditions:
-- * destinationAccountId is an @acct_*@ id that has finished onboarding.
-- * amountCents is positive and within Stripe's integer amount envelope.
-- * applicationFeeCents >= 0 and <= amountCents.
-- * currency is a three-letter code; it is normalized to lowercase.
--
-- Postcondition: all outcomes use 'decodeStripeResponse'; the returned success
-- value is valid JSON from Stripe and non-success statuses return @Left@.
createPaymentIntentForTip
  :: StripeConfig
  -> StripeAccountId       -- ^ destination Stripe account id (acct_*)
  -> StripeAmountCents     -- ^ Amount in cents
  -> StripeAmountCents     -- ^ Application fee in cents (platform cut)
  -> StripeCurrencyCode    -- ^ Currency (e.g., "usd")
  -> Text                  -- ^ Description
  -> Maybe StripeMetadataForm
  -> IO (Either Text Value)
createPaymentIntentForTip cfg destinationAccountId amountCents applicationFeeCents currency description mMetadata =
  case (,,,)
    <$> normalizeStripeAccountId destinationAccountId
    <*> validateStripeAmountCents "amountCents" amountCents
    <*> validateStripeApplicationFee amountCents applicationFeeCents
    <*> normalizeStripeCurrencyCode currency of
      Left err -> pure (Left err)
      Right (destinationAccountIdValue, amountCentsValue, applicationFeeCentsValue, currencyValue) ->
        withStripeManager $ \manager -> do
          let url = "https://api.stripe.com/v1/payment_intents"
              body = BS8.intercalate "&"
                [ "amount=" <> BS8.pack (show amountCentsValue)
                , "currency=" <> TE.encodeUtf8 currencyValue
                , "application_fee_amount=" <> BS8.pack (show applicationFeeCentsValue)
                , "transfer_data[destination]=" <> urlEncode (TE.encodeUtf8 destinationAccountIdValue)
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

normalizeStripeCountryCode :: Text -> Either Text StripeCountryCode
normalizeStripeCountryCode rawCountry =
  let country = T.toUpper (T.strip rawCountry)
  in if T.length country == 2 && T.all isAsciiUpperChar country
       then Right country
       else Left "country must be a 2-letter ISO code"

normalizeStripeAccountId :: Text -> Either Text StripeAccountId
normalizeStripeAccountId rawAccountId =
  let accountId = T.strip rawAccountId
  in if T.isPrefixOf "acct_" accountId
        && T.length accountId > T.length ("acct_" :: Text)
        && T.length accountId <= 128
        && T.all isStripeIdChar accountId
       then Right accountId
       else Left "Stripe account id must be an acct_* identifier"

normalizeRequiredStripeText :: Text -> Text -> Either Text Text
normalizeRequiredStripeText fieldName rawValue =
  let value = T.strip rawValue
  in if T.null value
       then Left (fieldName <> " must not be blank")
       else Right value

validateStripeAmountCents :: Text -> Int -> Either Text StripeAmountCents
validateStripeAmountCents fieldName amount
  | amount <= 0 =
      Left (fieldName <> " must be greater than zero")
  | amount > maxStripeAmountCents =
      Left (fieldName <> " exceeds Stripe's maximum integer amount")
  | otherwise =
      Right amount

validateStripeApplicationFee :: Int -> Int -> Either Text StripeAmountCents
validateStripeApplicationFee amount fee
  | fee < 0 =
      Left "applicationFeeCents must not be negative"
  | fee > amount =
      Left "applicationFeeCents must not exceed amountCents"
  | otherwise =
      Right fee

normalizeStripeCurrencyCode :: Text -> Either Text StripeCurrencyCode
normalizeStripeCurrencyCode rawCurrency =
  let currency = T.toLower (T.strip rawCurrency)
  in if T.length currency == 3 && T.all isAsciiLowerChar currency
       then Right currency
       else Left "currency must be a 3-letter ISO code"

maxStripeAmountCents :: Int
maxStripeAmountCents = 99999999

isStripeIdChar :: Char -> Bool
isStripeIdChar ch =
  isAsciiLowerChar ch || isAsciiUpperChar ch || isAsciiDigitChar ch || ch == '_'

isAsciiLowerChar :: Char -> Bool
isAsciiLowerChar ch =
  ch >= 'a' && ch <= 'z'

isAsciiUpperChar :: Char -> Bool
isAsciiUpperChar ch =
  ch >= 'A' && ch <= 'Z'

isAsciiDigitChar :: Char -> Bool
isAsciiDigitChar ch =
  ch >= '0' && ch <= '9'

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
          expectedSigHex =
            TE.encodeUtf8 $
              T.pack $
                show (hmacGetDigest (hmac secret signedPayload :: HMAC SHA256))
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
