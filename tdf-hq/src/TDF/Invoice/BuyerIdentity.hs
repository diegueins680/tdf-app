{-# LANGUAGE OverloadedStrings #-}

-- | Buyer identification for Ecuadorian invoices. SRI accepts "consumidor
-- final" only up to USD 50 per invoice; above that the buyer must be identified.
module TDF.Invoice.BuyerIdentity
  ( BillingIdentity(..)
  , consumidorFinalLimitMinor
  , validateBillingIdentity
  , validCedula
  , validRuc
  , billingIdentityType
  ) where

import           Data.Char (digitToInt, isAlphaNum, isDigit)
import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T

data BillingIdentity
  = BillingConsumidorFinal
  | BillingCedula Text Text
  | BillingRuc Text Text
  | BillingPasaporte Text Text
  deriving (Eq, Show)

consumidorFinalLimitMinor :: Int64
consumidorFinalLimitMinor = 5000

billingIdentityType :: BillingIdentity -> Text
billingIdentityType BillingConsumidorFinal = "consumidor_final"
billingIdentityType (BillingCedula _ _) = "cedula"
billingIdentityType (BillingRuc _ _) = "ruc"
billingIdentityType (BillingPasaporte _ _) = "pasaporte"

validateBillingIdentity
  :: Int64        -- ^ checkout total in minor units
  -> Maybe Text   -- ^ type: consumidor_final | cedula | ruc | pasaporte
  -> Maybe Text   -- ^ identification number
  -> Maybe Text   -- ^ legal name
  -> Either Text BillingIdentity
validateBillingIdentity totalMinor rawType rawNumber rawName =
  case T.toLower . T.strip <$> rawType of
    Nothing -> consumidorFinal
    Just "consumidor_final" -> consumidorFinal
    Just "cedula" -> identified validCedula "La cédula no es válida" BillingCedula
    Just "ruc" -> identified validRuc "El RUC no es válido" BillingRuc
    Just "pasaporte" -> identified validPassport "El pasaporte no es válido" BillingPasaporte
    Just _ -> Left "Tipo de identificación no soportado"
  where
    number = T.strip <$> rawNumber
    name = T.strip <$> rawName
    consumidorFinal
      | totalMinor > consumidorFinalLimitMinor =
          Left "Para compras sobre USD 50 ingresa cédula, RUC o pasaporte para la factura"
      | otherwise = Right BillingConsumidorFinal
    identified valid message constructor = case (number, name) of
      (Just value, Just legal)
        | not (valid value) -> Left message
        | T.length legal < 2 || T.length legal > 300 -> Left "Ingresa el nombre o razón social para la factura"
        | otherwise -> Right (constructor value legal)
      _ -> Left "Ingresa el número de identificación y el nombre para la factura"
    validPassport value = T.length value >= 3 && T.length value <= 20 && T.all isAlphaNum value

-- | Ten digits, province 01-24 or 30, third digit below 6, modulo-10 check.
validCedula :: Text -> Bool
validCedula value
  | T.length value /= 10 || not (T.all isDigit value) = False
  | otherwise = case map digitToInt (T.unpack value) of
      ds@(p1 : p2 : third : _) ->
        provinceValid (p1 * 10 + p2) && third < 6
          && checkDigit (take 9 ds) == listToMaybe (drop 9 ds)
      _ -> False
  where
    checkDigit nine =
      let products = zipWith (\coefficient digit -> let p = coefficient * digit in if p >= 10 then p - 9 else p)
            (cycle [2, 1]) nine
      in Just ((10 - sum products `mod` 10) `mod` 10)

-- | Thirteen digits ending in an establishment number. Natural-person RUCs embed
-- a valid cédula; company and public-sector RUCs use a valid province code.
validRuc :: Text -> Bool
validRuc value
  | T.length value /= 13 || not (T.all isDigit value) || T.takeEnd 3 value == "000" = False
  | otherwise = case map digitToInt (T.unpack value) of
      p1 : p2 : third : _ ->
        provinceValid (p1 * 10 + p2)
          && ((third < 6 && validCedula (T.take 10 value)) || third `elem` [6, 9])
      _ -> False

provinceValid :: Int -> Bool
provinceValid province = (province >= 1 && province <= 24) || province == 30
