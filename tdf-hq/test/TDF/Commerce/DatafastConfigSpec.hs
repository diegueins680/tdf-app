{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.DatafastConfigSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.URI (parseQuery)
import Test.Hspec
import Test.QuickCheck (choose, forAll, property)
import Text.Read (readMaybe)

import TDF.Commerce.DatafastConfig
  (validateDatafastMerchantParameters, encodeDatafastForm, splitDatafastBuyerName,
   DatafastCartLine(..), datafastCartParameters)
import TDF.Commerce.TicketBuyerContact (ticketPaymentBuyerPhone, normalizeTicketBuyerPhone)

spec :: Spec
spec = describe "Dataweb merchant metadata" $ do
  it "does not fabricate a buyer name when the stored value is empty" $
    forM_ ["", " ", "\t\n"] $ \name ->
      splitDatafastBuyerName name `shouldBe` ("", "")

  it "does not duplicate a mononym into a surname" $
    forM_ ["Prince", "  José  ", "李"] $ \name ->
      splitDatafastBuyerName name `shouldBe` (T.strip name, "")

  it "retains supplied compound name components and UTF-8 form values" $ do
    let (givenName, surname) = splitDatafastBuyerName "  José  María  Muñoz & López  "
        fields = [("customer.givenName", TE.encodeUtf8 givenName)
                 ,("customer.surname", TE.encodeUtf8 surname)]
    parseQuery (encodeDatafastForm fields) `shouldBe`
      [("customer.givenName", Just (TE.encodeUtf8 "José"))
      ,("customer.surname", Just (TE.encodeUtf8 "María Muñoz & López"))]

  it "never adds or removes a supplied name token" $ property $ \raw ->
    let name = T.pack raw
        (givenName, surname) = splitDatafastBuyerName name
    in T.words givenName <> T.words surname == T.words name

  it "preserves the phone saved when the ticket order was created" $ do
    let saved = Just "{\"runtime\":\"canonical_public_checkout\",\"buyer_phone\":\"+593 99 123 4567\",\"buyer_fee_minor\":20}"
    ticketPaymentBuyerPhone saved `shouldBe` Just "+593 99 123 4567"
    let fields = maybe [] (\phone -> [("customer.phone", TE.encodeUtf8 phone)])
          (ticketPaymentBuyerPhone saved)
    lookup "customer.phone" (parseQuery (encodeDatafastForm fields))
      `shouldBe` Just (Just "+593 99 123 4567")

  it "keeps absent or malformed stored phones absent without using another field" $
    forM_ [Nothing, Just "", Just "invalid json", Just "[]", Just "null"
          , Just "{}", Just "{\"buyer_phone\":null}"
          , Just "{\"buyer_email\":\"buyer@example.invalid\"}"
          , Just "{\"buyer_phone\":1234567}", Just "{\"buyer_phone\":[]}"
          , Just "{\"buyer_phone\":\"   \"}"
          , Just "{\"buyer_phone\":\"+593 99\\n1234567\"}"
          , Just "{\"buyer_phone\":\"1234567890123456789012345\"}"
          ] $ \saved -> ticketPaymentBuyerPhone saved `shouldBe` Nothing

  it "uses the initial order's existing normalization and length boundary" $ do
    normalizeTicketBuyerPhone (Just "  +593 (99) 123-4567  ")
      `shouldBe` Right (Just "+593 (99) 123-4567")
    ticketPaymentBuyerPhone (Just "{\"buyer_phone\":\"  +593 (99) 123-4567  \"}")
      `shouldBe` Just "+593 (99) 123-4567"
    normalizeTicketBuyerPhone (Just (T.replicate 25 "1")) `shouldSatisfy` isLeft
    normalizeTicketBuyerPhone (Just "1234\n5678") `shouldSatisfy` isLeft

  it "preserves accented buyer names and separators in the actual form encoder" $ do
    let fields =
          [ ("customer.givenName", TE.encodeUtf8 "José")
          , ("customer.surname", TE.encodeUtf8 "Muñoz")
          , ("customer.email", "buyer+tag@example.invalid")
          , ("risk.parameters[USER_DATA2]", TE.encodeUtf8 "Música & Arte=Quito")
          ]
    parseQuery (encodeDatafastForm fields) `shouldBe` map (\(key, value) -> (key, Just value)) fields

  it "requires all merchant fields independently of API credentials" $
    forM_ ["DATAFAST_MID", "DATAFAST_TID", "DATAFAST_PSERV", "DATAFAST_USER_DATA2"] $ \field ->
      forM_ [Map.delete field fixture, Map.insert field " \t " fixture] $ \input ->
        validateDatafastMerchantParameters input `shouldSatisfy` isLeft

  it "emits the documented Dataweb field names once with the configured identity" $
    validateDatafastMerchantParameters fixture `shouldBe` Right
      [ ("customParameters[SHOPPER_MID]", "1234567890")
      , ("customParameters[SHOPPER_TID]", "PD123456")
      , ("customParameters[SHOPPER_PSERV]", "12345678")
      , ("customParameters[SHOPPER_ECI]", "0103910")
      , ("customParameters[SHOPPER_VERSIONDF]", "2")
      , ("risk.parameters[USER_DATA2]", "Synthetic Merchant")
      ]

  it "retains explicitly configured protocol versions and the documented default" $ do
    validateDatafastMerchantParameters (Map.delete "DATAFAST_VERSIONDF" fixture)
      `shouldBe` validateDatafastMerchantParameters fixture
    forM_ ["1", "3", "9999"] $ \version ->
      fmap (lookup "customParameters[SHOPPER_VERSIONDF]")
        (validateDatafastMerchantParameters (Map.insert "DATAFAST_VERSIONDF" version fixture))
        `shouldBe` Right (Just (BS.pack version))
    forM_ ["0", "02", "10000", "2\nsecret", "٢", "1.0"] $ \version ->
      validateDatafastMerchantParameters (Map.insert "DATAFAST_VERSIONDF" version fixture)
        `shouldSatisfy` isLeft

  it "rejects unsafe and oversized metadata without echoing its content" $
    forM_ [ ("DATAFAST_MID", "private value")
          , ("DATAFAST_TID", "private&other=value")
          , ("DATAFAST_PSERV", replicate 257 'x')
          , ("DATAFAST_USER_DATA2", replicate 31 'x')
          , ("DATAFAST_USER_DATA2", "private\r\nheader")
          , ("DATAFAST_USER_DATA2", "private\NULvalue")
          ] $ \(field, value) -> case
            validateDatafastMerchantParameters (Map.insert field value fixture) of
      Left message -> message `shouldSatisfy` (not . T.isInfixOf "private")
      Right _ -> expectationFailure ("Accepted invalid " <> field)
  describe "immutable cart projection" $ do
    it "sends every stored item with its own exact price and quantity" $
      datafastCartParameters 3000 "USD"
        [DatafastCartLine "Entrada" 2 1250, DatafastCartLine "Reserva" 1 500]
        `shouldBe` Right
          [ ("cart.items[0].name", "Entrada")
          , ("cart.items[0].description", "Entrada")
          , ("cart.items[0].price", "12.50")
          , ("cart.items[0].quantity", "2")
          , ("cart.items[1].name", "Reserva")
          , ("cart.items[1].description", "Reserva")
          , ("cart.items[1].price", "5.00")
          , ("cart.items[1].quantity", "1")
          ]

    it "never rounds a mismatched total into an artificial per-item price" $ do
      datafastCartParameters 1000 "USD" [DatafastCartLine "Entrada" 3 333]
        `shouldSatisfy` isLeft
      fmap (lookup "cart.items[0].price")
        (datafastCartParameters 999 "USD" [DatafastCartLine "Entrada" 3 333])
        `shouldBe` Right (Just "3.33")

    it "rejects missing, zero and negative canonical amounts or quantities" $ do
      datafastCartParameters 100 "USD" [] `shouldSatisfy` isLeft
      forM_ [0, -1] $ \amount ->
        datafastCartParameters amount "USD" [DatafastCartLine "Entrada" 1 100]
          `shouldSatisfy` isLeft
      forM_ [DatafastCartLine "Entrada" 0 100, DatafastCartLine "Entrada" (-1) 100,
             DatafastCartLine "Entrada" 1 0, DatafastCartLine "Entrada" 1 (-100)] $ \line ->
        datafastCartParameters 100 "USD" [line] `shouldSatisfy` isLeft

    it "refuses unsupported currencies instead of treating their minor units as USD" $ do
      forM_ ["EUR", "JPY", "", "US1"] $ \currency ->
        datafastCartParameters 100 currency [DatafastCartLine "Entrada" 1 100]
          `shouldSatisfy` isLeft
      fmap (lookup "cart.items[0].price")
        (datafastCartParameters 1 " usd " [DatafastCartLine "Entrada" 1 1])
        `shouldBe` Right (Just "0.01")

    it "retains UTF-8 product descriptions and literal encoded separators" $ do
      let description = "Sesión José + mezcla=1"
      case datafastCartParameters 100 "USD" [DatafastCartLine description 1 100] of
        Left message -> expectationFailure (T.unpack message)
        Right fields -> do
          parseQuery (encodeDatafastForm fields)
            `shouldBe` map (\(key, value) -> (key, Just value)) fields
          lookup "cart.items[0].name" fields `shouldBe` Just (TE.encodeUtf8 description)

    it "rejects provider-invalid product names without exposing private text" $
      forM_ ["", "  ", "private&product", "private\nproduct", "private\NULproduct",
             T.replicate 256 "x"] $ \description ->
        case datafastCartParameters 100 "USD" [DatafastCartLine description 1 100] of
          Left message -> message `shouldSatisfy` (not . T.isInfixOf "private")
          Right _ -> expectationFailure "Provider-invalid cart name was accepted"

    it "respects exact decimal and quantity boundaries before provider I/O" $ do
      datafastCartParameters 999999999999 "USD"
        [DatafastCartLine "Entrada" 1 999999999999] `shouldSatisfy` either (const False) (const True)
      datafastCartParameters 1000000000000 "USD"
        [DatafastCartLine "Entrada" 1 1000000000000] `shouldSatisfy` isLeft
      datafastCartParameters 100000 "USD"
        [DatafastCartLine "Entrada" 100000 1] `shouldSatisfy` isLeft
      datafastCartParameters 99999 "USD"
        [DatafastCartLine "Entrada" 99999 1] `shouldSatisfy` either (const False) (const True)

    it "cannot wrap an overflowing product into a matching positive total" $
      datafastCartParameters 1 "USD" [DatafastCartLine "Entrada" 3 (maxBound :: Int64)]
        `shouldSatisfy` isLeft

    it "conserves generated cent totals and unique indexed fields" $
      forAll (choose (1, 999 :: Int)) $ \quantity ->
      forAll (choose (1, 100000 :: Int64)) $ \unit ->
        let amount = fromIntegral quantity * unit
        in case datafastCartParameters amount "USD"
            [DatafastCartLine "Entrada" quantity unit] of
          Left _ -> False
          Right fields -> length fields == 4 && Map.size (Map.fromList fields) == 4
            && lookup "cart.items[0].quantity" fields == Just (BS.pack (show quantity))
            && (do
                 price <- lookup "cart.items[0].price" fields
                 decimal <- readMaybe (BS.unpack price) :: Maybe Scientific
                 pure (toRational decimal * 100 * fromIntegral quantity)
               ) == Just (fromIntegral amount)
            && parseQuery (encodeDatafastForm fields)
              == map (\(key, value) -> (key, Just value)) fields

fixture :: Map.Map String String
fixture = Map.fromList
  [ ("DATAFAST_MID", "1234567890")
  , ("DATAFAST_TID", "PD123456")
  , ("DATAFAST_PSERV", "12345678")
  , ("DATAFAST_USER_DATA2", "Synthetic Merchant")
  , ("DATAFAST_VERSIONDF", "2")
  ]
