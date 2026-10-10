{-# LANGUAGE OverloadedStrings #-}

-- | Phone normalization for public course registration.
--
-- Both registration paths (legacy lead and checkout) store phones as E.164.
-- Visitors in Ecuador commonly type their local format (e.g. @0988384849@),
-- which used to be rejected with "phoneE164 inválido" (2026-10-07 user test).
module TDF.CoursePhone
  ( normalizeInternationalPhoneInput
  , normalizeEcuadorLocalPhone
  , normalizeCoursePhone
  ) where

import           Control.Applicative ((<|>))
import           Data.Text (Text)
import qualified Data.Text as T

isAsciiDigit :: Char -> Bool
isAsciiDigit ch = ch >= '0' && ch <= '9'

-- | International numbers written with a leading @+@ and a non-zero country
-- code; 8-15 digits; only digits, spaces and @+-().@ separators.
normalizeInternationalPhoneInput :: Text -> Maybe Text
normalizeInternationalPhoneInput raw =
  let trimmed = T.strip raw
      onlyDigits = T.filter isAsciiDigit trimmed
      digitCount = T.length onlyDigits
      plusCount = T.count "+" trimmed
      plusIndex = T.findIndex (== '+') trimmed
      firstDigitIndex = T.findIndex isAsciiDigit trimmed
      allowedPhoneChar ch =
        isAsciiDigit ch || ch == ' ' || ch `elem` ("+-()." :: String)
      hasInvalidChars = T.any (not . allowedPhoneChar) trimmed
      plusIsValid =
        case plusIndex of
          Nothing -> True
          Just idx ->
            case firstDigitIndex of
              Nothing -> False
              Just digitIdx -> plusCount == 1 && idx < digitIdx
      hasInternationalPrefix =
        T.isPrefixOf "+" trimmed
          && maybe False (/= '0') (T.find isAsciiDigit trimmed)
  in
    if T.null onlyDigits
         || digitCount < 8
         || digitCount > 15
         || hasInvalidChars
         || not plusIsValid
         || not hasInternationalPrefix
      then Nothing
      else Just ("+" <> onlyDigits)

-- | Ecuador national formats: mobile @09XXXXXXXX@ and landline
-- @0[2-7]XXXXXXX@, with optional spaces and @-().@ separators.
normalizeEcuadorLocalPhone :: Text -> Maybe Text
normalizeEcuadorLocalPhone raw
  | T.null trimmed || T.any (not . allowed) trimmed = Nothing
  | otherwise =
      case T.unpack (T.filter isAsciiDigit trimmed) of
        ('0' : '9' : rest)
          | length rest == 8 -> Just (T.pack ("+5939" <> rest))
        ('0' : area : rest)
          | area `elem` ("234567" :: String), length rest == 7 -> Just (T.pack ("+593" <> (area : rest)))
        _ -> Nothing
  where
    trimmed = T.strip raw
    allowed ch = isAsciiDigit ch || ch `elem` (" -()." :: String)

-- | Course registration phone: international E.164 input or an Ecuador
-- national number, normalized to E.164.
normalizeCoursePhone :: Text -> Maybe Text
normalizeCoursePhone raw =
  stripEcuadorTrunkZero <$> (normalizeInternationalPhoneInput raw <|> normalizeEcuadorLocalPhone raw)

-- | People often keep Ecuador's national trunk zero after the country code
-- (@+593 098 838 4849@, @+593 02 234 5678@). E.164 has no trunk prefix, so
-- drop it, matching the web normalizer in @tdf-hq-ui/src/utils/phone.ts@.
stripEcuadorTrunkZero :: Text -> Text
stripEcuadorTrunkZero e164 =
  case T.stripPrefix "+5930" e164 of
    Just rest | T.length rest `elem` [8, 9] -> "+593" <> rest
    _ -> e164
