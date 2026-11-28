{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.DatafastStatus
  ( ServiceDatafastEnv(..), ServiceDatafastPaymentStatus(..)
  , readDatafastEnvironment, readRequiredProviderSetting
  , validateDatafastEnvironmentBase, validateDatafastOrderResourcePath
  , isDatafastPaymentSuccess, validateDatafastSuccessfulPayment
  ) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import qualified TDF.Commerce.CheckoutStore as Checkout
import TDF.Commerce.PaypalWebhook (isProviderReference, parseDatafastCents)

readRequiredProviderSetting :: String -> Int -> ExceptT Text IO Text
readRequiredProviderSetting variableName maxLength = do
  rawValue <- liftIO (lookupEnv variableName)
  case T.strip . T.pack <$> rawValue of
    Just value
      | not (T.null value)
      , T.length value <= maxLength
      , T.all (\character -> character >= '!' && character <= '~') value -> pure value
    _ -> throwError (T.pack variableName <> " must be configured with safe visible ASCII")

-- | Datafast environment configuration.
data ServiceDatafastEnv = ServiceDatafastEnv
  { sdfEntityId    :: Text
  , sdfBearerToken :: Text
  , sdfBaseUrl     :: String
  , sdfTestMode    :: Maybe Text
  , sdfEnvironment :: Checkout.CheckoutEnvironment
  }

instance Show ServiceDatafastEnv where
  show environment =
    "ServiceDatafastEnv {sdfEntityId = <redacted>, sdfBearerToken = <redacted>, "
      <> "sdfBaseUrl = " <> show (sdfBaseUrl environment)
      <> ", sdfTestMode = " <> show (sdfTestMode environment)
      <> ", sdfEnvironment = " <> show (sdfEnvironment environment) <> "}"

validateDatafastEnvironmentBase
  :: Checkout.CheckoutEnvironment
  -> String
  -> Either Text ()
validateDatafastEnvironmentBase environment rawBaseUrl = do
  let baseUrl = T.toLower (T.strip (T.pack rawBaseUrl))
      mAfterScheme = T.stripPrefix "https://" baseUrl
      authority = maybe "" (T.takeWhile (`notElem` ("/?#" :: String))) mAfterScheme
      suffix = maybe "" (T.dropWhile (`notElem` ("/?#" :: String))) mAfterScheme
      validSuffix = T.null suffix || suffix == "/"
      validAuthority = not (T.null authority)
        && not ("@" `T.isInfixOf` authority)
        && not (":" `T.isInfixOf` authority)
      isOppwa = authority == "oppwa.com" || ".oppwa.com" `T.isSuffixOf` authority
      isTest = "test.oppwa.com" `T.isSuffixOf` authority
  unless (mAfterScheme /= Nothing && validAuthority && validSuffix && isOppwa) $
    Left "DATAFAST_BASE_URL must be an HTTPS oppwa.com endpoint"
  case environment of
    Checkout.CheckoutSandbox ->
      unless isTest (Left "DATAFAST_ENV=sandbox requires a test.oppwa.com endpoint")
    Checkout.CheckoutProduction ->
      when isTest (Left "DATAFAST_ENV=production cannot use a test.oppwa.com endpoint")

-- | Load Datafast environment from env vars.
readDatafastEnvironment :: ExceptT Text IO ServiceDatafastEnv
readDatafastEnvironment = do
  mTest   <- liftIO $ lookupEnv "DATAFAST_TEST_MODE"
  mEnvironment <- liftIO $ lookupEnv "DATAFAST_ENV"
  entityId <- readRequiredProviderSetting "DATAFAST_ENTITY_ID" 256
  unless (isProviderReference entityId) $
    throwError "DATAFAST_ENTITY_ID must contain only safe provider-reference characters"
  bearer <- readRequiredProviderSetting "DATAFAST_BEARER_TOKEN" 4096
  baseUrlText <- readRequiredProviderSetting "DATAFAST_BASE_URL" 512
  testMode <- case mTest of
    Nothing -> pure Nothing
    Just _ -> Just <$> readRequiredProviderSetting "DATAFAST_TEST_MODE" 128
  let baseUrl = T.unpack (T.toLower baseUrlText)
  environment <- either throwError pure
    (Checkout.resolveCheckoutEnvironment mEnvironment)
  either throwError pure
    (validateDatafastEnvironmentBase environment baseUrl)
  when (environment == Checkout.CheckoutProduction && testMode /= Nothing) $
    throwError "DATAFAST_TEST_MODE must be unset when DATAFAST_ENV=production"
  pure ServiceDatafastEnv
    { sdfEntityId = entityId
    , sdfBearerToken = bearer
    , sdfBaseUrl = baseUrl
    , sdfTestMode = testMode
    , sdfEnvironment = environment
    }

validateDatafastOrderResourcePath :: Maybe Text -> Text -> Either Text Text
validateDatafastOrderResourcePath mCheckoutId rawResourcePath = do
  checkoutId <- maybe (Left "Order does not have a Datafast checkout to confirm") Right mCheckoutId
  let resourcePath = T.strip rawResourcePath
      expected = "/v1/checkouts/" <> checkoutId <> "/payment"
      validCheckoutId =
        not (T.null checkoutId)
          && T.length checkoutId <= 256
          && T.all isDatafastIdChar checkoutId
  unless validCheckoutId (Left "Stored Datafast checkout ID is invalid")
  unless (resourcePath == expected) (Left "resourcePath does not match this order's Datafast checkout")
  pure resourcePath
  where
    isDatafastIdChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-_." :: String)

data ServiceDatafastPaymentStatus = ServiceDatafastPaymentStatus
  { sdfpsPaymentId             :: Maybe Text
  , sdfpsAmount                :: Maybe Text
  , sdfpsCurrency              :: Maybe Text
  , sdfpsMerchantTransactionId :: Maybe Text
  , sdfpsResultCode            :: Text
  } deriving (Show)

instance FromJSON ServiceDatafastPaymentStatus where
  parseJSON = withObject "ServiceDatafastPaymentStatus" $ \o -> do
    result <- o .: "result"
    ServiceDatafastPaymentStatus
      <$> o .:? "id"
      <*> o .:? "amount"
      <*> o .:? "currency"
      <*> o .:? "merchantTransactionId"
      <*> result .: "code"

isDatafastPaymentSuccess :: Checkout.CheckoutEnvironment -> Text -> Bool
isDatafastPaymentSuccess environment rawCode =
  case environment of
    Checkout.CheckoutProduction -> code == "000.000.000"
    Checkout.CheckoutSandbox -> code `elem` ["000.100.110", "000.100.112"]
  where
    code = T.strip rawCode

validateDatafastSuccessfulPayment
  :: Text
  -> Int
  -> Text
  -> ServiceDatafastPaymentStatus
  -> Either Text ()
validateDatafastSuccessfulPayment expectedOrderId expectedCents expectedCurrency status = do
  paidCents <- maybe (Left "Datafast response did not include an amount") parseDatafastCents (sdfpsAmount status)
  paidCurrency <- maybe (Left "Datafast response did not include a currency") (Right . T.toUpper . T.strip) (sdfpsCurrency status)
  merchantReference <- maybe (Left "Datafast response did not include the merchant transaction ID") (Right . T.strip) (sdfpsMerchantTransactionId status)
  paymentId <- maybe (Left "Datafast response did not include a payment ID") (Right . T.strip) (sdfpsPaymentId status)
  unless (paidCents == expectedCents) (Left "Datafast amount does not match the immutable order total")
  unless (paidCurrency == T.toUpper (T.strip expectedCurrency)) (Left "Datafast currency does not match the immutable order currency")
  unless (merchantReference == expectedOrderId) (Left "Datafast merchant reference does not match the internal order")
  unless (not (T.null paymentId) && T.length paymentId <= 256) (Left "Datafast payment ID is invalid")
