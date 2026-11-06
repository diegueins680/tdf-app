{-# LANGUAGE OverloadedStrings #-}

-- | PAY-ADAPTER-BINDING-001: a hosted-provider adapter admits a payment result
-- only when the provider's reference, amount and currency equal the stored
-- expectation, and money conversion is exact.
module TDF.Commerce.ProviderAdapterBindingSpec (spec) where

import           Data.Aeson (Value, object, toJSON, (.=))
import           Data.Either (isLeft, isRight)
import           Data.Int (Int64)
import           Data.Scientific (scientific)
import qualified Data.Text as T
import           Test.Hspec
import           Test.QuickCheck

import           TDF.Commerce.CheckoutStore (CheckoutEnvironment (..))
import           TDF.Commerce.ProviderAdapter
import           TDF.Commerce.ProviderAdapter.PayPhone
import           TDF.Commerce.ProviderAdapter.PlaceToPay
import           TDF.Commerce.ProviderCapabilities (ProviderOutcomeCertainty (..))

newtype Ref = Ref T.Text deriving (Show)

instance Arbitrary Ref where
  arbitrary = Ref . T.pack <$> (chooseInt (1, 12) >>= \n -> vectorOf n (elements (['a'..'z'] ++ ['0'..'9'])))

newtype Currency = Currency T.Text deriving (Show)

instance Arbitrary Currency where
  arbitrary = Currency <$> elements ["USD", "usd", "Usd", "EUR", "COP"]

positiveMinor :: Gen Int64
positiveMinor = frequency [(8, fromIntegral <$> chooseInt (1, 5000000)), (1, choose (1, maxBound `div` 1000))]

-- | A binding the provider reports, and whether it equals the expectation.
data Reported = Reported
  { repReference :: T.Text
  , repAmount    :: Int64
  , repCurrency  :: T.Text
  , repMatches   :: Bool
  } deriving (Show)

genReported :: ExpectedPayment -> Gen Reported
genReported expected = do
  Ref otherRef <- arbitrary
  otherAmount <- positiveMinor
  Currency otherCurrency <- arbitrary
  caseLabel <- chooseInt (0 :: Int, 4)
  let sameCurrencyDifferentCase = T.toLower (epCurrency expected)
      reported = case caseLabel of
        0 -> (epReference expected, epAmountMinor expected, epCurrency expected)
        1 -> (epReference expected, epAmountMinor expected, sameCurrencyDifferentCase)
        2 -> (otherRef, epAmountMinor expected, epCurrency expected)
        3 -> (epReference expected, otherAmount, epCurrency expected)
        _ -> (epReference expected, epAmountMinor expected, otherCurrency)
      (r, a, c) = reported
  pure (Reported r a c
    (r == epReference expected && a == epAmountMinor expected
      && normalizedCurrency c == normalizedCurrency (epCurrency expected)))

genExpected :: Gen ExpectedPayment
genExpected = do
  Ref r <- arbitrary
  a <- positiveMinor
  Currency c <- arbitrary
  pure (ExpectedPayment r a c)

placeToPay :: ProviderAdapter
placeToPay = either (error . show) id $ placeToPayAdapter PlaceToPayConfig
  { ptpEnvironment = CheckoutSandbox
  , ptpLogin = "login"
  , ptpSecretKey = "secret-key-value"
  , ptpPaymentMethods = []
  }

payPhone :: ProviderAdapter
payPhone = either (error . show) id $ payPhoneAdapter PayPhoneConfig
  { payPhoneToken = "token-value"
  , payPhoneStoreId = "store-1"
  }

amountJson :: T.Text -> Int64 -> Value
amountJson currency minor = object ["currency" .= currency, "total" .= scientific (fromIntegral minor) (-2)]

placeToPayQuery :: T.Text -> Reported -> [T.Text] -> Value
placeToPayQuery status reported transactionStatuses = object
  [ "requestId" .= (4711 :: Int64)
  , "status" .= object ["status" .= status]
  , "request" .= object ["payment" .= object
      [ "reference" .= repReference reported
      , "amount" .= amountJson (repCurrency reported) (repAmount reported) ]]
  , "payment" .= map transaction transactionStatuses
  ]
  where
    transaction s = object
      [ "status" .= object ["status" .= s]
      , "refunded" .= False
      , "internalReference" .= (99 :: Int64)
      , "reference" .= repReference reported
      , "amount" .= object
          [ "from" .= amountJson (repCurrency reported) (repAmount reported)
          , "to" .= amountJson (repCurrency reported) (repAmount reported) ]
      ]

payPhoneQuery :: Reported -> Int -> Value
payPhoneQuery reported code = object
  [ "clientTransactionId" .= repReference reported
  , "transactionId" .= (7 :: Int64)
  , "amount" .= repAmount reported
  , "currency" .= repCurrency reported
  , "statusCode" .= code
  ]

locatorFor :: T.Text -> ExpectedPayment -> PaymentLocator
locatorFor externalId expected = PaymentLocator externalId expected

spec :: Spec
spec = describe "Provider adapter binding admission (PAY-ADAPTER-BINDING-001)" $ do
  describe "PlaceToPay query" $ do
    it "admits an approved session exactly when reference, amount and currency match" $
      property $ forAll genExpected $ \expected -> forAll (genReported expected) $ \reported ->
        let result = adapterParseResponse placeToPay AdapterQuery (locatorFor "4711" expected)
              (placeToPayQuery "APPROVED" reported ["APPROVED"])
        in if repMatches reported
             then case result of
               Right r -> adapterResultState r === AdapterSucceeded
                 .&&. adapterResultCertainty r === ProviderSucceeded
                 .&&. adapterResultAmountMinor r === Just (epAmountMinor expected)
               Left err -> counterexample (show err) False
             else property (isLeft result)
    it "never reports success without exactly one approved complete transaction" $
      property $ forAll genExpected $ \expected ->
        forAll (listOf (elements ["APPROVED", "REJECTED", "PENDING"])) $ \statuses ->
          let reported = Reported (epReference expected) (epAmountMinor expected) (epCurrency expected) True
              result = adapterParseResponse placeToPay AdapterQuery (locatorFor "4711" expected)
                (placeToPayQuery "APPROVED" reported statuses)
              approved = length (filter (== "APPROVED") statuses)
          in if approved == 1 && all (`elem` ["APPROVED", "REJECTED"]) statuses
               then property (isRight result)
               else property (isLeft result)
    it "rejects a session whose request id differs from the stored one" $
      property $ forAll genExpected $ \expected ->
        let reported = Reported (epReference expected) (epAmountMinor expected) (epCurrency expected) True
        in isLeft (adapterParseResponse placeToPay AdapterQuery (locatorFor "4712" expected)
             (placeToPayQuery "APPROVED" reported ["APPROVED"]))
    it "classifies rejected sessions as confirmed no-charge only when every transaction is rejected" $
      property $ forAll genExpected $ \expected ->
        let reported = Reported (epReference expected) (epAmountMinor expected) (epCurrency expected) True
            rejected = adapterParseResponse placeToPay AdapterQuery (locatorFor "4711" expected)
              (placeToPayQuery "REJECTED" reported ["REJECTED", "REJECTED"])
            mixed = adapterParseResponse placeToPay AdapterQuery (locatorFor "4711" expected)
              (placeToPayQuery "REJECTED" reported ["REJECTED", "PENDING"])
        in (fmap adapterResultCertainty rejected === Right ProviderConfirmedNoCharge)
             .&&. (fmap adapterResultCertainty mixed === Right ProviderAmbiguous)

  describe "PayPhone query" $ do
    it "admits a sale exactly when reference, amount and currency match" $
      property $ forAll genExpected $ \expected -> forAll (genReported expected) $ \reported ->
        forAll (chooseInt (0, 5)) $ \code ->
          let result = adapterParseResponse payPhone AdapterQuery (locatorFor "7" expected)
                (payPhoneQuery reported code)
          in if repMatches reported
               then case result of
                 Right r -> (adapterResultState r, adapterResultCertainty r) ===
                   (case code of
                      1 -> (AdapterPending, ProviderAmbiguous)
                      2 -> (AdapterCancelled, ProviderConfirmedNoCharge)
                      3 -> (AdapterSucceeded, ProviderSucceeded)
                      _ -> (AdapterUnknown, ProviderAmbiguous))
                 Left err -> counterexample (show err) False
               else property (isLeft result)
    it "never authenticates a callback" $
      property $ forAll genExpected $ \expected ->
        let callback = object ["TransactionId" .= (7 :: Int64), "ClientTransactionId" .= epReference expected, "StatusCode" .= (3 :: Int)]
        in case adapterAssessNotification payPhone callback of
             Right assessment -> not (notificationAuthenticated assessment) && notificationRequiresQuery assessment
             Left _ -> True

  describe "money conversion" $ do
    it "round-trips every minor amount through the provider decimal" $
      property $ forAll (choose (0, maxBound `div` 100)) $ \minor ->
        decimalToMinor (minorToDecimal minor) === Just minor
    it "rejects sub-cent precision" $ do
      decimalToMinor (scientific 10005 (-3)) `shouldBe` Nothing
      decimalToMinor (scientific 1 (-3)) `shouldBe` Nothing
    it "accepts a USD breakdown exactly when the components sum to a positive total" $
      property $ \(NonNegative a) (NonNegative b) (NonNegative c) (NonNegative d) (NonNegative e) extra ->
        let components = map fromIntegral [a, b, c, d, e :: Int] :: [Int64]
            total = sum components + fromIntegral (extra :: Int)
            money = MoneyBreakdown "USD" total (components !! 0) (components !! 1) (components !! 2) (components !! 3) (components !! 4)
        in isRight (validateUsdMoney money) === (extra == 0 && total > 0)
    it "rejects non-USD currencies and negative components" $ do
      validateUsdMoney (MoneyBreakdown "EUR" 100 100 0 0 0 0) `shouldSatisfy` isLeft
      validateUsdMoney (MoneyBreakdown "USD" 100 150 0 0 0 (-50)) `shouldSatisfy` isLeft
      validateUsdMoney (MoneyBreakdown "usd" 100 100 0 0 0 0) `shouldSatisfy` isRight
      toJSON (minorToDecimal 12345) `shouldBe` toJSON (scientific 12345 (-2))
