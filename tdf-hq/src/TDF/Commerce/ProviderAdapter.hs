{-# LANGUAGE OverloadedStrings #-}

-- | Stable, provider-neutral boundary for remote payment integrations.
--
-- The types in this module deliberately contain no PAN, CVV, magnetic-stripe
-- data, or provider access tokens.  Provider SDKs/hosted checkout components
-- collect card data; the backend exchanges only provider references and
-- canonical integer-minor-unit amounts.
module TDF.Commerce.ProviderAdapter
  ( AdapterOperation(..)
  , AdapterHttpMethod(..)
  , AdapterRetryPolicy(..)
  , AdapterResultState(..)
  , AdapterError(..)
  , SensitiveText
  , sensitiveText
  , revealSensitiveText
  , AdapterRequest(..)
  , AdapterRequestSummary(..)
  , safeRequestSummary
  , AdapterContext(..)
  , MoneyBreakdown(..)
  , CreatePayment(..)
  , ExpectedPayment(..)
  , PaymentLocator(..)
  , PaymentMutation(..)
  , AdapterResult(..)
  , NotificationAssessment(..)
  , ProviderAdapter(..)
  , minorToDecimal
  , decimalToMinor
  , normalizedCurrency
  , validateUsdMoney
  , validVisibleCredential
  , validProviderIdentifier
  , unsupportedOperation
  ) where

import           Data.Aeson (Value)
import           Data.ByteString (ByteString)
import           Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit,
                            isPrint)
import           Data.Int (Int64)
import           Data.Scientific (Scientific, scientific, toBoundedInteger)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)

import           TDF.Commerce.CheckoutStore (PaymentProvider)
import           TDF.Commerce.ProviderCapabilities
  ( ProviderOutcomeCertainty )

data AdapterOperation
  = AdapterCreate
  | AdapterQuery
  | AdapterCancel
  | AdapterCapture
  | AdapterVoid
  | AdapterRefund
  | AdapterSameDayReverse
  deriving (Eq, Ord, Show, Enum, Bounded)

data AdapterHttpMethod = AdapterGet | AdapterPost
  deriving (Eq, Ord, Show)

-- | Network retry policy.  A caller must reconcile before changing provider
-- whenever the outcome of a create request is ambiguous.
data AdapterRetryPolicy
  = ReuseStableReference
  | QueryBeforeRetry
  | SafeReadRetry
  | NeverRetryAutomatically
  deriving (Eq, Ord, Show)

data AdapterResultState
  = AdapterPending
  | AdapterRequiresCustomerAction
  | AdapterAuthorized
  | AdapterSucceeded
  | AdapterCancelled
  | AdapterDeclined
  | AdapterReversed
  | AdapterUnknown
  deriving (Eq, Ord, Show)

newtype AdapterError = AdapterError
  { adapterErrorPublicMessage :: Text
  } deriving (Eq, Show)

-- | Intentionally redacted when formatted.  Revealing is an explicit action
-- reserved for the HTTP executor.
newtype SensitiveText = SensitiveText Text
  deriving (Eq)

instance Show SensitiveText where
  show _ = "<redacted>"

sensitiveText :: Text -> SensitiveText
sensitiveText = SensitiveText

revealSensitiveText :: SensitiveText -> Text
revealSensitiveText (SensitiveText value) = value

-- | The request has no 'Show' instance because its JSON body can contain PII
-- and derived authentication material. Use 'safeRequestSummary' for logs.
data AdapterRequest = AdapterRequest
  { arOperation   :: AdapterOperation
  , arMethod      :: AdapterHttpMethod
  , arUrl         :: Text
  , arHeaders     :: [(Text, SensitiveText)]
  , arBody        :: Maybe Value
  , arRetryPolicy :: AdapterRetryPolicy
  }

data AdapterRequestSummary = AdapterRequestSummary
  { arsOperation   :: AdapterOperation
  , arsMethod      :: AdapterHttpMethod
  , arsUrl         :: Text
  , arsHeaderNames :: [Text]
  , arsHasBody     :: Bool
  , arsRetryPolicy :: AdapterRetryPolicy
  } deriving (Eq, Show)

safeRequestSummary :: AdapterRequest -> AdapterRequestSummary
safeRequestSummary request = AdapterRequestSummary
  { arsOperation = arOperation request
  , arsMethod = arMethod request
  , arsUrl = arUrl request
  , arsHeaderNames = map fst (arHeaders request)
  , arsHasBody = maybe False (const True) (arBody request)
  , arsRetryPolicy = arRetryPolicy request
  }

data AdapterContext = AdapterContext
  { acNow      :: UTCTime
  , acRawNonce :: ByteString
  } deriving (Eq, Show)

-- | All fields are integer minor units. The total must equal the component
-- sum, even for providers that accept the components independently.
data MoneyBreakdown = MoneyBreakdown
  { mbCurrency              :: Text
  , mbTotalMinor            :: Int64
  , mbWithoutTaxMinor       :: Int64
  , mbTaxableBaseMinor      :: Int64
  , mbTaxMinor              :: Int64
  , mbServiceMinor          :: Int64
  , mbTipMinor              :: Int64
  } deriving (Eq, Show)

data CreatePayment = CreatePayment
  { cpReference        :: Text
  , cpDescription      :: Text
  , cpMoney            :: MoneyBreakdown
  , cpReturnUrl        :: Text
  , cpNotificationUrl  :: Maybe Text
  , cpBuyerPhone       :: Maybe Text
  , cpBuyerCountryCode :: Maybe Text
  , cpIpAddress        :: Text
  , cpUserAgent        :: Text
  } deriving (Eq, Show)

data ExpectedPayment = ExpectedPayment
  { epReference   :: Text
  , epAmountMinor :: Int64
  , epCurrency    :: Text
  } deriving (Eq, Show)

data PaymentLocator = PaymentLocator
  { plExternalId :: Text
  , plExpected   :: ExpectedPayment
  } deriving (Eq, Show)

data PaymentMutation = PaymentMutation
  { pmLocator          :: PaymentLocator
  , pmAmountMinor      :: Int64
  , pmProviderActionAt :: UTCTime
  } deriving (Eq, Show)

data AdapterResult = AdapterResult
  { adapterResultState       :: AdapterResultState
  , adapterResultExternalId  :: Text
  , adapterResultRedirectUrl :: Maybe Text
  , adapterResultAmountMinor :: Maybe Int64
  , adapterResultCurrency    :: Maybe Text
  , adapterResultCertainty   :: ProviderOutcomeCertainty
  } deriving (Eq, Show)

data NotificationAssessment = NotificationAssessment
  { notificationExternalId     :: Text
  , notificationMerchantReference :: Maybe Text
  , notificationProviderStatus :: Maybe Text
  , notificationAuthenticated  :: Bool
  , notificationRequiresQuery  :: Bool
  } deriving (Eq, Show)

-- | A closed adapter value captures validated provider configuration. The
-- application can route and execute it without branching in product modules.
data ProviderAdapter = ProviderAdapter
  { adapterProvider           :: PaymentProvider
  , adapterBuildCreate        :: AdapterContext -> CreatePayment -> Either AdapterError AdapterRequest
  , adapterBuildQuery         :: AdapterContext -> PaymentLocator -> Either AdapterError AdapterRequest
  , adapterBuildCancel        :: AdapterContext -> PaymentLocator -> Either AdapterError AdapterRequest
  , adapterBuildCapture       :: AdapterContext -> PaymentMutation -> Either AdapterError AdapterRequest
  , adapterBuildVoid          :: AdapterContext -> PaymentLocator -> Either AdapterError AdapterRequest
  , adapterBuildRefund        :: AdapterContext -> PaymentMutation -> Either AdapterError AdapterRequest
  , adapterBuildSameDayReverse :: AdapterContext -> PaymentMutation -> Either AdapterError AdapterRequest
  , adapterParseResponse      :: AdapterOperation -> PaymentLocator -> Value -> Either AdapterError AdapterResult
  , adapterAssessNotification :: Value -> Either AdapterError NotificationAssessment
  }

minorToDecimal :: Int64 -> Scientific
minorToDecimal amountMinor = scientific (fromIntegral amountMinor) (-2)

decimalToMinor :: Scientific -> Maybe Int64
decimalToMinor amount = toBoundedInteger (amount * 100)

normalizedCurrency :: Text -> Text
normalizedCurrency = T.toUpper . T.strip

validateUsdMoney :: MoneyBreakdown -> Either AdapterError ()
validateUsdMoney money
  | normalizedCurrency (mbCurrency money) /= "USD" =
      Left (AdapterError "This provider account supports USD only.")
  | any (< 0) components =
      Left (AdapterError "Payment amounts cannot be negative.")
  | mbTotalMinor money <= 0 =
      Left (AdapterError "Payment total must be greater than zero.")
  | mbTotalMinor money /= sum components =
      Left (AdapterError "Payment total does not match its amount components.")
  | otherwise = Right ()
  where
    components =
      [ mbWithoutTaxMinor money
      , mbTaxableBaseMinor money
      , mbTaxMinor money
      , mbServiceMinor money
      , mbTipMinor money
      ]

validVisibleCredential :: Text -> Bool
validVisibleCredential value =
  not (T.null value)
    && value == T.strip value
    && T.all (\character -> isAscii character && isPrint character) value

validProviderIdentifier :: Text -> Bool
validProviderIdentifier value =
  not (T.null value)
    && T.length value <= 128
    && T.all allowed value
  where
    allowed character =
      isAsciiLower character
        || isAsciiUpper character
        || isDigit character
        || character `elem` ("._-" :: String)

unsupportedOperation :: Either AdapterError value
unsupportedOperation = Left (AdapterError "Operation is not enabled for this provider adapter.")
