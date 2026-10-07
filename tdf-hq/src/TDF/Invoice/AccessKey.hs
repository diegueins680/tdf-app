{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | SRI offline-scheme access key (clave de acceso): 48 digits plus a modulo-11
-- check digit. Generating it ourselves binds every retry of one invoice to the
-- same SRI identity, so a resend cannot become a second invoice.
module TDF.Invoice.AccessKey
  ( AccessKeyInput(..)
  , accessKey
  , modulo11CheckDigit
  , numericCodeFromSeed
  ) where

import           Data.Char (digitToInt, isDigit, ord)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, toGregorian)
import           Text.Printf (printf)

data AccessKeyInput = AccessKeyInput
  { akiIssuedOn      :: Day
  , akiDocumentType  :: Text   -- ^ "01" invoice, "04" credit note
  , akiRuc           :: Text
  , akiEnvironment   :: Int    -- ^ 1 test, 2 production
  , akiEstablishment :: Text
  , akiEmissionPoint :: Text
  , akiSequential    :: Int64
  , akiNumericCode   :: Text   -- ^ eight digits
  } deriving (Eq, Show)

accessKey :: AccessKeyInput -> Either Text Text
accessKey AccessKeyInput{..}
  | not (digits 2 akiDocumentType) = Left "Document type must have two digits"
  | not (digits 13 akiRuc) = Left "RUC must have 13 digits"
  | akiEnvironment `notElem` [1, 2] = Left "Environment must be 1 or 2"
  | not (digits 3 akiEstablishment && digits 3 akiEmissionPoint) =
      Left "Establishment and emission point must have three digits"
  | akiSequential < 1 || akiSequential > 999999999 = Left "Sequential is out of range"
  | not (digits 8 akiNumericCode) = Left "Numeric code must have eight digits"
  | otherwise = Right (body <> T.singleton (modulo11CheckDigit body))
  where
    (year, month, day) = toGregorian akiIssuedOn
    body = T.concat
      [ T.pack (printf "%02d%02d%04d" day month year)
      , akiDocumentType, akiRuc, T.pack (show akiEnvironment)
      , akiEstablishment, akiEmissionPoint
      , T.pack (printf "%09d" akiSequential)
      , akiNumericCode
      , "1"
      ]
    digits n value = T.length value == n && T.all isDigit value

-- | Weights 2..7 repeat from the rightmost digit; 11 maps to 0 and 10 to 1.
modulo11CheckDigit :: Text -> Char
modulo11CheckDigit value =
  case 11 - (total `mod` 11) of
    11 -> '0'
    10 -> '1'
    check -> toEnum (fromEnum '0' + check)
  where
    total = sum (zipWith (*) (cycle [2 .. 7]) (map digitToInt (reverse (T.unpack value))))

-- | Deterministic eight-digit code derived from a stable document identifier.
numericCodeFromSeed :: Text -> Text
numericCodeFromSeed seed =
  T.pack (printf "%08d" (foldl step (7 :: Integer) (T.unpack seed) `mod` 100000000))
  where
    step acc char = (acc * 131 + toInteger (ord char)) `mod` 1000000007
