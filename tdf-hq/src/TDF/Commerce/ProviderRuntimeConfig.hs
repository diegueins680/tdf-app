{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Secret-backed provider configuration. Values are validated and closed
-- over by an adapter; this module never returns raw credentials or derives
-- runtime readiness merely from one environment variable being present.
module TDF.Commerce.ProviderRuntimeConfig
  ( RuntimeProviderAdapter(..)
  , loadRuntimeProviderAdapter
  , runtimeProviderConfigured
  , runtimeProviderMethodConfigured
  , loadProviderOperationEncryptionKey
  , loadConfiguredCheckoutEnvironment
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe (catMaybes)
import           System.Environment (lookupEnv)

import           TDF.Commerce.CheckoutStore
  ( CheckoutEnvironment, PaymentProvider(..), resolveCheckoutEnvironment )
import           TDF.Commerce.ProviderAdapter
  ( AdapterError(..), ProviderAdapter )
import           TDF.Commerce.ProviderAdapter.PayPhone
  ( PayPhoneConfig(..), payPhoneAdapter )
import           TDF.Commerce.ProviderAdapter.PlaceToPay
  ( PlaceToPayConfig(..), placeToPayAdapter )
import           TDF.Commerce.ProviderCapabilities (PaymentMethod(..))

data RuntimeProviderAdapter = RuntimeProviderAdapter
  { rpaAdapter         :: ProviderAdapter
  , rpaReturnUrl       :: Text
  , rpaNotificationUrl :: Maybe Text
  , rpaConfiguredMethods :: [PaymentMethod]
  }

loadRuntimeProviderAdapter
  :: CheckoutEnvironment
  -> PaymentProvider
  -> IO (Either Text RuntimeProviderAdapter)
loadRuntimeProviderAdapter environment provider = case provider of
  ProviderPlaceToPay -> do
    login <- required "PLACETOPAY_LOGIN"
    secret <- required "PLACETOPAY_SECRET_KEY"
    returnUrl <- requiredHttps "PLACETOPAY_RETURN_URL"
    notificationUrl <- requiredHttps "PLACETOPAY_NOTIFICATION_URL"
    cardMethods <- optional "PLACETOPAY_CARD_PAYMENT_METHODS"
    bankMethods <- optional "PLACETOPAY_BANK_PAYMENT_METHODS"
    deunaMethods <- optional "PLACETOPAY_DEUNA_PAYMENT_METHODS"
    pure $ do
      validatedLogin <- login
      validatedSecret <- secret
      validatedReturn <- returnUrl
      validatedNotification <- notificationUrl
      validatedCardMethods <- cardMethods
      validatedBankMethods <- bankMethods
      validatedDeunaMethods <- deunaMethods
      let methodMappings = catMaybes
            [ (MethodCard,) <$> validatedCardMethods
            , (MethodBankRedirect,) <$> validatedBankMethods
            , (MethodDeunaQr,) <$> validatedDeunaMethods
            ]
      adapter <- firstAdapterError (placeToPayAdapter PlaceToPayConfig
        { ptpEnvironment = environment
        , ptpLogin = validatedLogin
        , ptpSecretKey = validatedSecret
        , ptpPaymentMethods = methodMappings
        })
      Right RuntimeProviderAdapter
        { rpaAdapter = adapter
        , rpaReturnUrl = validatedReturn
        , rpaNotificationUrl = Just validatedNotification
        , rpaConfiguredMethods = map fst methodMappings
        }
  ProviderPayPhone -> do
    token <- required "PAYPHONE_TOKEN"
    storeId <- required "PAYPHONE_STORE_ID"
    returnUrl <- requiredHttps "PAYPHONE_RESPONSE_URL"
    pure $ do
      validatedToken <- token
      validatedStoreId <- storeId
      validatedReturn <- returnUrl
      adapter <- firstAdapterError (payPhoneAdapter PayPhoneConfig
        { payPhoneToken = validatedToken
        , payPhoneStoreId = validatedStoreId
        })
      Right RuntimeProviderAdapter
        { rpaAdapter = adapter
        , rpaReturnUrl = validatedReturn
        , rpaNotificationUrl = Nothing
        , rpaConfiguredMethods = [MethodPayPhoneWallet]
        }
  _ -> pure (Left "Shared provider executor does not support this provider")

runtimeProviderConfigured
  :: CheckoutEnvironment
  -> PaymentProvider
  -> IO Bool
runtimeProviderConfigured environment provider =
  either (const False) (const True) <$> loadRuntimeProviderAdapter environment provider

runtimeProviderMethodConfigured
  :: CheckoutEnvironment
  -> PaymentProvider
  -> PaymentMethod
  -> IO Bool
runtimeProviderMethodConfigured environment provider method = do
  runtime <- loadRuntimeProviderAdapter environment provider
  pure $ either (const False) (elem method . rpaConfiguredMethods) runtime

loadProviderOperationEncryptionKey :: IO (Either Text Text)
loadProviderOperationEncryptionKey = do
  value <- required "COMMERCE_EVENT_ENCRYPTION_KEY"
  pure $ do
    key <- value
    if T.length key >= 32 && T.length key <= 256
        && T.all (\character -> character >= '!' && character <= '~') key
      then Right key
      else Left "COMMERCE_EVENT_ENCRYPTION_KEY must contain 32 to 256 visible ASCII characters"

loadConfiguredCheckoutEnvironment :: IO (Either Text CheckoutEnvironment)
loadConfiguredCheckoutEnvironment = do
  rawEnvironment <- lookupEnv "COMMERCE_CHECKOUT_ENV"
  pure (resolveCheckoutEnvironment rawEnvironment)

required :: String -> IO (Either Text Text)
required name = do
  raw <- lookupEnv name
  pure $ case T.strip . T.pack <$> raw of
    Just value
      | not (T.null value)
      , T.all (\character -> character >= '!' && character <= '~') value ->
          Right value
    _ -> Left (T.pack name <> " is not configured")

requiredHttps :: String -> IO (Either Text Text)
requiredHttps name = do
  value <- required name
  pure $ do
    url <- value
    if "https://" `T.isPrefixOf` T.toLower url
        && T.length url <= 2048
        && not ("#" `T.isInfixOf` url)
      then Right url
      else Left (T.pack name <> " must be an HTTPS URL without a fragment")

optional :: String -> IO (Either Text (Maybe Text))
optional name = do
  raw <- lookupEnv name
  pure $ case raw of
    Nothing -> Right Nothing
    Just value
      | T.null (T.strip (T.pack value)) -> Right Nothing
      | otherwise -> Just <$> validateVisible name value

validateVisible :: String -> String -> Either Text Text
validateVisible name raw =
  let value = T.strip (T.pack raw)
  in if T.length value <= 256
      && T.all (\character -> character >= '!' && character <= '~') value
    then Right value
    else Left (T.pack name <> " is invalid")

firstAdapterError
  :: Either AdapterError value
  -> Either Text value
firstAdapterError = either (Left . adapterErrorPublicMessage) Right
