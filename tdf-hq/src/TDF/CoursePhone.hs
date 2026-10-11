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

import           Data.List (isPrefixOf, stripPrefix)
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

-- | Course registration phone, normalized to E.164. A port of
-- @normalizePhoneToE164@ in @tdf-hq-ui/src/utils/phone.ts@, so the web form and
-- every server intake path store the same value; ServerSpec checks the web
-- test table. Two deliberate differences, both stricter: only the ASCII space
-- separates digit groups (the web's @\\s@ also admits newlines and Unicode
-- separators), and after a @00@ international prefix at least 9 digits are
-- required, because @00@ plus 8 digits is indistinguishable from a mistyped
-- Ecuador national number.
normalizeCoursePhone :: Text -> Maybe Text
normalizeCoursePhone raw
  | T.null trimmed || T.any (not . allowedChar) trimmed = Nothing
  | plusCount > 1 || (plusCount == 1 && not ("+" `T.isPrefixOf` trimmed)) = Nothing
  | null digits = Nothing
  | "+" `T.isPrefixOf` trimmed = fromInternationalDigits digits
  | Just rest <- stripPrefix "00" digits =
      if length rest >= 9 then fromInternationalDigits rest else Nothing
  | ('0' : '9' : rest) <- digits, length rest == 8 = ecuador ('9' : rest)
  | ('0' : area : rest) <- digits, area `elem` ("234567" :: String), length rest == 7 =
      ecuador (area : rest)
  -- Mobile typed without the trunk 0 ("98 838 4849").
  | ('9' : rest) <- digits, length rest == 8 = ecuador digits
  -- Country code typed without "+" ("593 98 838 4849").
  | "593" `isPrefixOf` digits, length digits `elem` [11, 12] = fromInternationalDigits digits
  | otherwise = Nothing
  where
    trimmed = T.strip raw
    -- Only the ASCII space separates groups: newlines and Unicode separators
    -- (U+00A0, U+2028, …) are rejected as forged-contact vectors.
    allowedChar ch = isAsciiDigit ch || ch `elem` (" +-()." :: String)
    plusCount = T.count "+" trimmed
    digits = T.unpack (T.filter isAsciiDigit trimmed)
    ecuador national = Just (T.pack ("+593" <> national))

-- | Digits after an international prefix. People often keep Ecuador's national
-- trunk zero ("+593 09…", "00593 0 2…"); E.164 has none, so it is dropped.
-- Ecuador numbers must then be a valid mobile (9 + 8 digits) or landline
-- ([2-7] + 7 digits); other countries need 8-15 digits without a leading 0.
fromInternationalDigits :: String -> Maybe Text
fromInternationalDigits raw =
  let normalized = case raw of
        ('5' : '9' : '3' : '0' : rest) | length raw `elem` [12, 13] -> "593" <> rest
        _ -> raw
  in case stripPrefix "593" normalized of
       Just national
         | validMobile national || validLandline national -> Just (T.pack ('+' : normalized))
         | otherwise -> Nothing
       Nothing
         | validE164 normalized -> Just (T.pack ('+' : normalized))
         | otherwise -> Nothing
  where
    validMobile ('9' : rest) = length rest == 8
    validMobile _ = False
    validLandline (area : rest) = area `elem` ("234567" :: String) && length rest == 7
    validLandline _ = False
    validE164 (first : rest) = first `elem` ("123456789" :: String) && length rest >= 7 && length rest <= 14
    validE164 [] = False
