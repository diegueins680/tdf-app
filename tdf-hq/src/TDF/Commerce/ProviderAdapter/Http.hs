{-# LANGUAGE OverloadedStrings #-}

-- | The only transport allowed to reveal adapter credentials. It deliberately
-- disables redirects and implicit retries, bounds streamed responses and total
-- time, validates provider HTTPS destinations, and redacts transport errors.
module TDF.Commerce.ProviderAdapter.Http
  ( AdapterTransportError(..)
  , executeAdapterRequest
  , adapterRequestDestinationAllowed
  , parseProviderRequest
  , prepareProviderRequest
  , executeProviderRequest
  , readProviderResponse
  , providerManagerSettings
  , sharedProviderManager
  ) where

import           Control.Exception (try, evaluate)
import           Control.Exception.Safe (tryAny)
import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Client
  ( BodyReader, HttpException, Manager, ManagerSettings(..), Request(..), RequestBody(..)
  , newManager, withResponse, parseRequest, responseBody, responseStatus
  , responseTimeoutMicro, brRead
  )
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Timeout (timeout)

import           TDF.Commerce.CheckoutStore (PaymentProvider(..))
import           TDF.Commerce.ProviderAdapter

newtype AdapterTransportError = AdapterTransportError
  { adapterTransportPublicMessage :: Text
  } deriving (Eq, Show)

executeAdapterRequest
  :: Manager
  -> AdapterRequest
  -> IO (Either AdapterTransportError A.Value)
executeAdapterRequest manager adapterRequest
  | not (adapterRequestDestinationAllowed adapterRequest) =
      pure (Left (AdapterTransportError
        "Payment provider destination is not allowlisted."))
  | otherwise = do
      parsed <- parseProviderRequest (arProvider adapterRequest)
        (T.unpack (arUrl adapterRequest))
      case parsed of
        Left failure -> pure (Left failure)
        Right baseRequest -> do
          let request = baseRequest
                { method = httpMethodBytes (arMethod adapterRequest)
                , requestHeaders =
                    [ (CI.mk (TE.encodeUtf8 name), TE.encodeUtf8 (revealSensitiveText value))
                    | (name, value) <- arHeaders adapterRequest
                    ]
                , requestBody = maybe (RequestBodyLBS "")
                    (RequestBodyLBS . A.encode) (arBody adapterRequest)
                }
          executeProviderRequest manager (arProvider adapterRequest) request

adapterRequestDestinationAllowed :: AdapterRequest -> Bool
adapterRequestDestinationAllowed request =
  providerUrlAllowed (arProvider request) (arUrl request)

-- Keep the existing Datafast oppwa.com origin family for configured regional
-- endpoints. Its environment-specific validator still runs before construction.
providerHostAllowed :: PaymentProvider -> Text -> Bool
providerHostAllowed provider hostname = case provider of
  ProviderPlaceToPay -> hostname == "checkout-test.placetopay.ec"
    || hostname == "checkout.placetopay.ec"
  ProviderPayPhone -> hostname == "pay.payphonetodoesposible.com"
  ProviderPayPal -> hostname == "api-m.sandbox.paypal.com"
    || hostname == "api-m.paypal.com"
  ProviderDatafast -> hostname == "oppwa.com"
    || (".oppwa.com" `T.isSuffixOf` hostname && validDnsName hostname)
  _ -> False
  where
    validDnsName = all (\label -> not (T.null label) && T.length label <= 63
      && T.head label /= '-' && T.last label /= '-'
      && T.all (\c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-') label)
      . T.splitOn "."

providerUrlAllowed :: PaymentProvider -> Text -> Bool
providerUrlAllowed provider url = case T.stripPrefix "https://" url of
  Nothing -> False
  Just rest ->
    let (authority, suffix) = T.breakOn "/" rest
    in T.length url <= 4096 && T.all (\c -> c >= '!' && c <= '~') url
      && not (T.any (\c -> c == '#' || c == '\\') url)
      && not (T.null suffix)
      && providerHostAllowed provider (T.toLower authority)

-- Parse without leaking InvalidUrlException's embedded URL. Userinfo, explicit
-- ports, fragments and non-provider authorities are rejected before parsing.
parseProviderRequest :: PaymentProvider -> String -> IO (Either AdapterTransportError Request)
parseProviderRequest provider url
  | not (providerUrlAllowed provider (T.pack url)) = pure (Left destinationError)
  | otherwise = do
      parsed <- try (parseRequest url) :: IO (Either HttpException Request)
      pure $ either (const (Left destinationError)) (prepareProviderRequest provider) parsed

-- Revalidate after credentials/headers are attached; a Host header cannot bypass
-- the destination check. Do not print Request values or use untrusted managers.
prepareProviderRequest :: PaymentProvider -> Request -> Either AdapterTransportError Request
prepareProviderRequest provider request
  | not (secure request && port request == 443
      && either (const False) (providerHostAllowed provider . T.toLower)
          (TE.decodeUtf8' (host request)))
      || any ((== "Host") . fst) (requestHeaders request) = Left destinationError
  | otherwise = Right request
      { redirectCount = 0
      , responseTimeout = responseTimeoutMicro providerTimeoutMicroseconds
      , checkResponse = \_ _ -> pure ()
      , cookieJar = Nothing
      }

-- A total deadline also covers DNS/connect, upload and a stalled body after
-- headers. http-client's responseTimeout alone does not bound body consumption.
-- No financial state is inferred from ANY transport error (including HTTP 4xx).
executeProviderRequest
  :: A.FromJSON a
  => Manager -> PaymentProvider -> Request -> IO (Either AdapterTransportError a)
executeProviderRequest manager provider original = case prepareProviderRequest provider original of
  Left failure -> pure (Left failure)
  Right request -> do
    result <- tryAny $ timeout providerTimeoutMicroseconds $
      withResponse request manager $ \response ->
        readProviderResponse (statusCode (responseStatus response)) (responseBody response)
    pure $ case result of
      Left _ -> Left unavailableError
      Right Nothing -> Left unavailableError
      Right (Just value) -> value

-- Stop at the first overflowing chunk, without consuming the rest or retaining
-- it. The reader has already decompressed HTTP content, so the bound applies to
-- expanded bytes as well. Non-2xx bodies are not read or exposed at all.
readProviderResponse
  :: A.FromJSON a => Int -> BodyReader -> IO (Either AdapterTransportError a)
readProviderResponse code reader
  | code < 200 || code >= 300 = pure $ Left $ AdapterTransportError
      ("Payment provider rejected the request (HTTP " <> T.pack (show code) <> ").")
  | otherwise = collect 0 []
  where
    collect total chunks = do
      chunk <- brRead reader
      if BS.null chunk
        then evaluate $ case A.eitherDecode (BL.fromChunks (reverse chunks)) of
          Left _ -> Left (AdapterTransportError "Payment provider returned an invalid response.")
          Right value -> Right value
        else if BS.length chunk > maximumResponseBytes - total
          then pure (Left (AdapterTransportError "Payment provider response is too large."))
          else collect (total + BS.length chunk) (chunk : chunks)

-- Do not inherit http-client's retry-on-stale-pooled-connection policy for
-- payment mutations. Reconciliation/idempotency is owned by the payment domain.
providerManagerSettings :: ManagerSettings
providerManagerSettings = tlsManagerSettings { managerRetryableException = const False }

sharedProviderManager :: Manager
sharedProviderManager = unsafePerformIO (newManager providerManagerSettings)
{-# NOINLINE sharedProviderManager #-}

destinationError :: AdapterTransportError
destinationError = AdapterTransportError "Payment provider destination is not allowlisted."

unavailableError :: AdapterTransportError
unavailableError = AdapterTransportError "Payment provider is temporarily unavailable; reconcile before retrying."

httpMethodBytes :: AdapterHttpMethod -> ByteString
httpMethodBytes AdapterGet = "GET"
httpMethodBytes AdapterPost = "POST"

maximumResponseBytes :: Int
maximumResponseBytes = 1024 * 1024

providerTimeoutMicroseconds :: Int
providerTimeoutMicroseconds = 15 * 1000 * 1000
