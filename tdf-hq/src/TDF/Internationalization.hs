{-# LANGUAGE OverloadedStrings #-}

module TDF.Internationalization
  ( CurrencyDefinition (..)
  , currencyDefinition
  , formatMinorUnitsDecimal
  , formatMoney
  , isIso4217CurrencyCode
  , normalizeCountryCode
  , normalizeCurrencyCode
  , normalizeLocaleCode
  , normalizeTimeZone
  , supportedLocaleCodes
  ) where

import Data.Char (isAscii, isControl, isDigit, isLetter)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

data CurrencyDefinition = CurrencyDefinition
  { currencyCode :: Text
  , currencySymbol :: Text
  , currencyDecimalPlaces :: Int
  } deriving (Eq, Show)

-- ISO 4217 active tender and commonly used fund/metal codes. Keeping this list
-- in one module makes API validation deterministic and independent of process
-- locale data.
iso4217Codes :: Set.Set Text
iso4217Codes = Set.fromList
  [ "AED", "AFN", "ALL", "AMD", "ANG", "AOA", "ARS", "AUD", "AWG", "AZN"
  , "BAM", "BBD", "BDT", "BGN", "BHD", "BIF", "BMD", "BND", "BOB", "BOV"
  , "BRL", "BSD", "BTN", "BWP", "BYN", "BZD", "CAD", "CDF", "CHE", "CHF"
  , "CHW", "CLF", "CLP", "CNY", "COP", "COU", "CRC", "CUP", "CVE", "CZK"
  , "DJF", "DKK", "DOP", "DZD", "EGP", "ERN", "ETB", "EUR", "FJD", "FKP"
  , "GBP", "GEL", "GHS", "GIP", "GMD", "GNF", "GTQ", "GYD", "HKD", "HNL"
  , "HTG", "HUF", "IDR", "ILS", "INR", "IQD", "IRR", "ISK", "JMD", "JOD"
  , "JPY", "KES", "KGS", "KHR", "KMF", "KPW", "KRW", "KWD", "KYD", "KZT"
  , "LAK", "LBP", "LKR", "LRD", "LSL", "LYD", "MAD", "MDL", "MGA", "MKD"
  , "MMK", "MNT", "MOP", "MRU", "MUR", "MVR", "MWK", "MXN", "MXV", "MYR"
  , "MZN", "NAD", "NGN", "NIO", "NOK", "NPR", "NZD", "OMR", "PAB", "PEN"
  , "PGK", "PHP", "PKR", "PLN", "PYG", "QAR", "RON", "RSD", "RUB", "RWF"
  , "SAR", "SBD", "SCR", "SDG", "SEK", "SGD", "SHP", "SLE", "SOS", "SRD"
  , "SSP", "STN", "SVC", "SYP", "SZL", "THB", "TJS", "TMT", "TND", "TOP"
  , "TRY", "TTD", "TWD", "TZS", "UAH", "UGX", "USD", "USN", "UYI", "UYU"
  , "UYW", "UZS", "VED", "VES", "VND", "VUV", "WST", "XAF", "XAG", "XAU"
  , "XBA", "XBB", "XBC", "XBD", "XCD", "XDR", "XOF", "XPD", "XPF", "XPT"
  , "XSU", "XTS", "XUA", "XXX", "YER", "ZAR", "ZMW", "ZWG"
  ]

normalizeCurrencyCode :: Text -> Maybe Text
normalizeCurrencyCode raw =
  let normalized = T.toUpper (T.strip raw)
  in if isIso4217CurrencyCode normalized then Just normalized else Nothing

isIso4217CurrencyCode :: Text -> Bool
isIso4217CurrencyCode code = Set.member code iso4217Codes

currencyDefinition :: Text -> Maybe CurrencyDefinition
currencyDefinition raw = do
  code <- normalizeCurrencyCode raw
  pure CurrencyDefinition
    { currencyCode = code
    , currencySymbol = symbolFor code
    , currencyDecimalPlaces = decimalPlacesFor code
    }
  where
    symbolFor "AUD" = "A$"
    symbolFor "BRL" = "R$"
    symbolFor "CAD" = "C$"
    symbolFor "EUR" = "€"
    symbolFor "GBP" = "£"
    symbolFor "JPY" = "¥"
    symbolFor "USD" = "$"
    symbolFor code = code

    decimalPlacesFor code
      | code `elem` zeroDecimalCurrencies = 0
      | code `elem` threeDecimalCurrencies = 3
      | otherwise = 2

    zeroDecimalCurrencies =
      [ "BIF", "CLP", "DJF", "GNF", "ISK", "JPY", "KMF", "KRW", "PYG", "RWF"
      , "UGX", "UYI", "VND", "VUV", "XAF", "XOF", "XPF"
      ]
    threeDecimalCurrencies = ["BHD", "IQD", "JOD", "KWD", "LYD", "OMR", "TND"]

supportedLocaleCodes :: [Text]
supportedLocaleCodes = ["en", "es", "fr", "de", "pt"]

normalizeLocaleCode :: Text -> Maybe Text
normalizeLocaleCode raw =
  let base = T.toLower (T.takeWhile (\ch -> ch /= '-' && ch /= '_') (T.strip raw))
  in if base `elem` supportedLocaleCodes then Just base else Nothing

normalizeCountryCode :: Text -> Maybe Text
normalizeCountryCode raw =
  let value = T.toUpper (T.strip raw)
  in if T.length value == 2 && T.all isAsciiLetter value && Set.member value iso3166CountryCodes
       then Just value
       else Nothing
  where
    isAsciiLetter ch = isAscii ch && isLetter ch

iso3166CountryCodes :: Set.Set Text
iso3166CountryCodes = Set.fromList . T.words $
  "AD AE AF AG AI AL AM AO AQ AR AS AT AU AW AX AZ BA BB BD BE BF BG BH BI BJ BL BM BN BO BQ BR BS BT BV BW BY BZ "
  <> "CA CC CD CF CG CH CI CK CL CM CN CO CR CU CV CW CX CY CZ DE DJ DK DM DO DZ EC EE EG EH ER ES ET FI FJ FK FM FO FR "
  <> "GA GB GD GE GF GG GH GI GL GM GN GP GQ GR GS GT GU GW GY HK HM HN HR HT HU ID IE IL IM IN IO IQ IR IS IT JE JM JO JP "
  <> "KE KG KH KI KM KN KP KR KW KY KZ LA LB LC LI LK LR LS LT LU LV LY MA MC MD ME MF MG MH MK ML MM MN MO MP MQ MR MS MT MU "
  <> "MV MW MX MY MZ NA NC NE NF NG NI NL NO NP NR NU NZ OM PA PE PF PG PH PK PL PM PN PR PS PT PW PY QA RE RO RS RU RW SA SB "
  <> "SC SD SE SG SH SI SJ SK SL SM SN SO SR SS ST SV SX SY SZ TC TD TF TG TH TJ TK TL TM TN TO TR TT TV TW TZ UA UG UM US UY "
  <> "UZ VA VC VE VG VI VN VU WF WS YE YT ZA ZM ZW"

normalizeTimeZone :: Text -> Maybe Text
normalizeTimeZone raw =
  let value = T.strip raw
      validChar ch = isAscii ch && (isLetter ch || isDigit ch || ch `elem` ("/_+-" :: String))
      components = T.splitOn "/" value
      validComponent component =
        not (T.null component)
          && component /= "."
          && component /= ".."
          && T.all validChar component
  in if value == "UTC"
       then Just value
       else if T.length value <= 80
            && not (T.any isControl value)
            && length components >= 2
            && all validComponent components
         then Just value
         else Nothing

-- A deterministic server-side formatter for emails, exports, and legacy DTO
-- display fields. Browser and mobile clients should still use Intl so they get
-- the complete Unicode CLDR behavior available on the device.
formatMoney :: Text -> Text -> Integer -> Text
formatMoney rawLocale rawCurrency minorUnits =
  let locale = fromMaybe "en" (normalizeLocaleCode rawLocale)
      definition = fromMaybe fallbackDefinition (currencyDefinition rawCurrency)
      decimals = currencyDecimalPlaces definition
      factor = 10 ^ decimals :: Integer
      absoluteUnits = abs minorUnits
      whole = if factor == 0 then absoluteUnits else absoluteUnits `div` factor
      fraction = if factor == 0 then 0 else absoluteUnits `mod` factor
      (thousandsSeparator, decimalSeparator, symbolAfter) = localePunctuation locale
      groupedWhole = groupThousands thousandsSeparator (show whole)
      fractionText =
        if decimals == 0
          then ""
          else T.singleton decimalSeparator
            <> T.justifyRight decimals '0' (T.pack (show fraction))
      sign = if minorUnits < 0 then "-" else ""
      number = T.pack groupedWhole <> fractionText
      code = currencyCode definition
      symbol = currencySymbol definition
      rendered
        | symbolAfter = number <> " " <> symbol
        | otherwise = symbol <> number
  in sign <> rendered <> " " <> code
  where
    fallbackDefinition = CurrencyDefinition (T.toUpper (T.strip rawCurrency)) rawCurrency 2

    localePunctuation "de" = ('.', ',', True)
    localePunctuation "es" = ('.', ',', True)
    localePunctuation "fr" = ('\8239', ',', True)
    localePunctuation "pt" = ('.', ',', True)
    localePunctuation _ = (',', '.', False)

    groupThousands separator digits =
      let reversedGroups = chunksOfThree (reverse digits)
      in intercalate [separator] (map reverse (reverse reversedGroups))

    chunksOfThree [] = []
    chunksOfThree value = take 3 value : chunksOfThree (drop 3 value)

-- Provider-facing decimal amount without a localized symbol or grouping.
formatMinorUnitsDecimal :: Text -> Integer -> Text
formatMinorUnitsDecimal rawCurrency minorUnits =
  let decimals = maybe 2 currencyDecimalPlaces (currencyDefinition rawCurrency)
      factor = 10 ^ decimals :: Integer
      absoluteUnits = abs minorUnits
      whole = if factor == 0 then absoluteUnits else absoluteUnits `div` factor
      fraction = if factor == 0 then 0 else absoluteUnits `mod` factor
      sign = if minorUnits < 0 then "-" else ""
      fractionText =
        if decimals == 0
          then ""
          else "." <> T.justifyRight decimals '0' (T.pack (show fraction))
  in sign <> T.pack (show whole) <> fractionText
