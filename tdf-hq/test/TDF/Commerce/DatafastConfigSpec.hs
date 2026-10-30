{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.DatafastConfigSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.URI (parseQuery)
import Test.Hspec

import TDF.Commerce.DatafastConfig (validateDatafastMerchantParameters, encodeDatafastForm)
import TDF.Commerce.TicketBuyerContact (ticketPaymentBuyerPhone, normalizeTicketBuyerPhone)

spec :: Spec
spec = describe "Dataweb merchant metadata" $ do
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
fixture :: Map.Map String String
fixture = Map.fromList
  [ ("DATAFAST_MID", "1234567890")
  , ("DATAFAST_TID", "PD123456")
  , ("DATAFAST_PSERV", "12345678")
  , ("DATAFAST_USER_DATA2", "Synthetic Merchant")
  , ("DATAFAST_VERSIONDF", "2")
  ]
