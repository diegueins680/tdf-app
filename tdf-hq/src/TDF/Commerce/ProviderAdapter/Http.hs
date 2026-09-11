{-# LANGUAGE OverloadedStrings #-}

-- | The only transport allowed to reveal adapter credentials. It deliberately
-- disables redirects, limits time/body size, accepts only fixed provider HTTPS
-- hosts, and never exposes provider response bodies in errors.
module TDF.Commerce.ProviderAdapter.Http
  ( AdapterTransportError(..)
  , executeAdapterRequest
  , adapterRequestDestinationAllowed
  ) where

import           Control.Exception (try)
import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Client
  ( HttpException, Manager, Request(..), RequestBody(..), Response
  , httpLbs, parseRequest, responseBody, responseStatus
  , responseTimeoutMicro
  )
import           Network.HTTP.Types.Status (statusCode)

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
      parsed <- try (parseRequest (T.unpack (arUrl adapterRequest)))
        :: IO (Either HttpException Request)
      case parsed of
        Left _ -> pure (Left (AdapterTransportError
          "Payment provider destination is invalid."))
        Right baseRequest -> do
          let request = baseRequest
                { method = httpMethodBytes (arMethod adapterRequest)
                , requestHeaders =
                    [ (CI.mk (TE.encodeUtf8 name), TE.encodeUtf8 (revealSensitiveText value))
                    | (name, value) <- arHeaders adapterRequest
                    ]
                , requestBody = maybe (RequestBodyLBS "")
                    (RequestBodyLBS . A.encode) (arBody adapterRequest)
                , responseTimeout = responseTimeoutMicro (15 * 1000 * 1000)
                , redirectCount = 0
                , checkResponse = \_ _ -> pure ()
                }
          result <- try (httpLbs request manager)
            :: IO (Either HttpException (Response BL.ByteString))
          pure $ case result of
            Left _ -> Left (AdapterTransportError
              "Payment provider is temporarily unavailable.")
            Right response -> decodeResponse response

adapterRequestDestinationAllowed :: AdapterRequest -> Bool
adapterRequestDestinationAllowed request = case arProvider request of
  ProviderPlaceToPay -> any (`fixedBase` arUrl request)
    [ "https://checkout-test.placetopay.ec"
    , "https://checkout.placetopay.ec"
    ]
  ProviderPayPhone ->
    fixedBase "https://pay.payphonetodoesposible.com" (arUrl request)
  _ -> False
  where
    fixedBase base url = (base <> "/") `T.isPrefixOf` url

decodeResponse
  :: Response BL.ByteString
  -> Either AdapterTransportError A.Value
decodeResponse response
  | code < 200 || code >= 300 =
      Left (AdapterTransportError
        ("Payment provider rejected the request (HTTP "
          <> T.pack (show code) <> ")."))
  | BL.length body > maximumResponseBytes =
      Left (AdapterTransportError "Payment provider response is too large.")
  | otherwise = case A.eitherDecode body of
      Left _ -> Left (AdapterTransportError
        "Payment provider returned an invalid response.")
      Right value -> Right value
  where
    code = statusCode (responseStatus response)
    body = responseBody response

httpMethodBytes :: AdapterHttpMethod -> ByteString
httpMethodBytes AdapterGet = "GET"
httpMethodBytes AdapterPost = "POST"

maximumResponseBytes :: Int64
maximumResponseBytes = 1024 * 1024
