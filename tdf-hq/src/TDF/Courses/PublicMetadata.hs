{-# LANGUAGE OverloadedStrings #-}

-- | Pure public course identity and stored-URL policy. A stored identity must
-- already be canonical: normalizing it could disclose a different course row.
module TDF.Courses.PublicMetadata
  ( isValidNormalizedCourseSlug
  , isCanonicalCourseSlug
  , isProductionCoursePublicSlug
  , hasRequiredProductionLead
  , isCurrentPublicCourseIdentity
  , ambiguousProductionCourseSlugs
  , validateCoursePublicUrlField
  , sanitizeStoredCoursePublicUrl
  , isInternationalWhatsappCtaNumber
  ) where

import Control.Applicative ((<|>))
import Data.Char (digitToInt, isAsciiLower, isDigit, isHexDigit)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified TDF.Courses.Production as Production
import qualified TDF.PublicUrl as PublicUrl

isValidNormalizedCourseSlug :: Text -> Bool
isValidNormalizedCourseSlug slug =
  not (T.null slug) && T.length slug <= 96
    && T.all isSlugChar slug && T.any isSlugAtom slug
  where
    isSlugAtom ch = isAsciiLower ch || isDigit ch
    isSlugChar ch = isSlugAtom ch || ch == '-'

isCanonicalCourseSlug :: Text -> Bool
isCanonicalCourseSlug slug =
  slug == T.toLower (T.strip slug) && isValidNormalizedCourseSlug slug

isProductionCoursePublicSlug :: Text -> Text -> Bool
isProductionCoursePublicSlug configuredSlug rawSlug =
  let slug = T.toLower (T.strip rawSlug)
  in slug == "produccion-musical" || slug == configuredSlug
      || "produccion-musical-" `T.isPrefixOf` slug

hasRequiredProductionLead :: Day -> Maybe Day -> Bool
hasRequiredProductionLead today = maybe False (>= Production.minimumProductionStartDate today)

-- Aliases resolve to another cohort, never to their own stored metadata. A
-- cohort is self-resolving only while its earliest session has the public lead.
isCurrentPublicCourseIdentity :: Text -> Day -> Text -> Maybe Day -> Bool
isCurrentPublicCourseIdentity configuredSlug today slug firstSession =
  isCanonicalCourseSlug slug
    && if "produccion-musical-" `T.isPrefixOf` slug
         then hasRequiredProductionLead today firstSession
         else not (isProductionCoursePublicSlug configuredSlug slug)

-- Public production lookup normalizes all stored identities, including legacy
-- rows. Suppress every ambiguous identity instead of choosing a competing row.
-- Text's whitespace and case semantics deliberately match the public resolver.
ambiguousProductionCourseSlugs :: [Text] -> [Text]
ambiguousProductionCourseSlugs slugs =
  Map.keys $ Map.filter (> (1 :: Int)) $ Map.fromListWith (+)
    [ (normalized, 1)
    | slug <- slugs
    , let normalized = T.toLower (T.strip slug)
    , "produccion-musical-" `T.isPrefixOf` normalized
    ]

isAsciiPhoneDigit :: Char -> Bool
isAsciiPhoneDigit ch = ch >= '0' && ch <= '9'

validateCoursePublicUrlField :: Text -> Maybe Text -> Either Text (Maybe Text)
validateCoursePublicUrlField _ Nothing = Right Nothing
validateCoursePublicUrlField fieldName (Just rawUrl) =
  case cleanOptional (Just rawUrl) of
    Nothing -> Right Nothing
    Just urlVal
      | T.length urlVal > maxCoursePublicUrlChars ->
          Left (fieldName <> " must be " <> T.pack (show maxCoursePublicUrlChars)
            <> " characters or fewer")
      | not ("https://" `T.isPrefixOf` T.toLower urlVal && PublicUrl.isValidHttpUrl urlVal) ->
          Left (fieldName <> " must be an absolute https URL")
      | "#" `T.isInfixOf` urlVal ->
          Left (fieldName <> " must not include a URL fragment")
      | hasPercentEncodedControlByte urlVal ->
          Left (fieldName <> " must not include percent-encoded control bytes")
      | PublicUrl.hasAmbiguousPublicUrlPath urlVal ->
          Left (fieldName <> " path must not contain empty, dot, or dot-dot segments")
      | fieldName == "whatsappCtaUrl" && not (isAllowedWhatsAppCtaUrl urlVal) ->
          Left "whatsappCtaUrl must use wa.me, api.whatsapp.com, or web.whatsapp.com \
            \on the default HTTPS port with a valid WhatsApp send path"
      | otherwise -> Right (Just urlVal)

maxCoursePublicUrlChars :: Int
maxCoursePublicUrlChars = 2048

hasPercentEncodedControlByte :: Text -> Bool
hasPercentEncodedControlByte rawUrl =
  case T.breakOn "%" rawUrl of
    (_, rest)
      | T.null rest -> False
      | otherwise ->
          case T.unpack (T.take 3 rest) of
            ['%', high, low]
              | isHexDigit high && isHexDigit low ->
                  let byteValue = digitToInt high * 16 + digitToInt low
                  in byteValue < 32
                       || byteValue == 127
                       || hasPercentEncodedControlByte (T.drop 3 rest)
            _ -> hasPercentEncodedControlByte (T.drop 1 rest)

isAllowedWhatsAppCtaUrl :: Text -> Bool
isAllowedWhatsAppCtaUrl rawUrl =
  case extractHttpUrlParts rawUrl of
    Just (host, portSuffix, pathAndQuery) ->
      host `elem` allowedHosts
        && (T.null portSuffix || portSuffix == ":443")
        && isAllowedWhatsAppCtaPath host pathAndQuery
    Nothing ->
      False
  where
    allowedHosts =
      [ "wa.me"
      , "api.whatsapp.com"
      , "web.whatsapp.com"
      ]

isAllowedWhatsAppCtaPath :: Text -> Text -> Bool
isAllowedWhatsAppCtaPath host pathAndQuery
  | host == "wa.me" =
      isAllowedWaMePath pathAndQuery
  | host `elem` ["api.whatsapp.com", "web.whatsapp.com"] =
      isAllowedWhatsAppSendPath pathAndQuery
  | otherwise =
      False

isAllowedWaMePath :: Text -> Bool
isAllowedWaMePath pathAndQuery =
  case T.breakOn "?" pathAndQuery of
    ("/", query) ->
      isAllowedWhatsAppTextQuery query
    (path, query) ->
      case T.stripPrefix "/" path of
        Just digitsOnly ->
          isInternationalWhatsappCtaNumber digitsOnly
            && isAllowedOptionalWhatsAppTextQuery query
        Nothing ->
          False

isAllowedWhatsAppSendPath :: Text -> Bool
isAllowedWhatsAppSendPath pathAndQuery =
  case T.breakOn "?" pathAndQuery of
    ("/send", query) ->
      isAllowedWhatsAppSendQuery query
    _ ->
      False

isAllowedOptionalWhatsAppTextQuery :: Text -> Bool
isAllowedOptionalWhatsAppTextQuery query =
  T.null query || isAllowedWhatsAppTextQuery query

isAllowedWhatsAppTextQuery :: Text -> Bool
isAllowedWhatsAppTextQuery query =
  case parseWhatsAppQuery query of
    Just [("text", textValue)] -> not (T.null textValue)
    _ -> False

isAllowedWhatsAppSendQuery :: Text -> Bool
isAllowedWhatsAppSendQuery query =
  case parseWhatsAppQuery query of
    Nothing -> False
    Just params ->
      let phoneValues = [value | ("phone", value) <- params]
          textValues = [value | ("text", value) <- params]
      in not (null params)
           && length phoneValues <= 1
           && length textValues <= 1
           && all isAllowedWhatsAppSendParam params
           && (not (null phoneValues) || not (null textValues))

isAllowedWhatsAppSendParam :: (Text, Text) -> Bool
isAllowedWhatsAppSendParam ("phone", value) =
  isInternationalWhatsappCtaNumber value
isAllowedWhatsAppSendParam ("text", value) =
  not (T.null value)
isAllowedWhatsAppSendParam _ =
  False

parseWhatsAppQuery :: Text -> Maybe [(Text, Text)]
parseWhatsAppQuery query = do
  rawQuery <- T.stripPrefix "?" query
  let rawParams = T.splitOn "&" rawQuery
  if null rawParams || any T.null rawParams
    then Nothing
    else traverse parseParam rawParams
  where
    parseParam rawParam =
      let (name, rawValue) = T.breakOn "=" rawParam
      in if T.null name || not ("=" `T.isPrefixOf` rawValue)
           then Nothing
           else Just (name, T.drop 1 rawValue)

extractHttpUrlParts :: Text -> Maybe (Text, Text, Text)
extractHttpUrlParts rawUrl = do
  withoutScheme <-
    T.stripPrefix "https://" lowerUrl <|> T.stripPrefix "http://" lowerUrl
  let authority = T.takeWhile (`notElem` ("/?#" :: String)) withoutScheme
      pathAndQuery = T.drop (T.length authority) withoutScheme
      (host, portSuffix) = T.breakOn ":" authority
  if T.null host then Nothing else Just (host, portSuffix, pathAndQuery)
  where
    lowerUrl = T.toLower (T.strip rawUrl)

sanitizeStoredCoursePublicUrl :: Text -> Maybe Text -> Maybe Text
sanitizeStoredCoursePublicUrl fieldName =
  either (const Nothing) (\urlVal -> urlVal) . validateCoursePublicUrlField fieldName

isInternationalWhatsappCtaNumber :: Text -> Bool
isInternationalWhatsappCtaNumber digitsOnly =
  -- wa.me paths require country-code digits, not local trunk-prefix numbers.
  case T.uncons digitsOnly of
    Just (firstDigit, _) ->
      T.all isAsciiPhoneDigit digitsOnly
        && firstDigit /= '0'
        && T.length digitsOnly >= 8
        && T.length digitsOnly <= 15
    Nothing -> False

cleanOptional :: Maybe Text -> Maybe Text
cleanOptional Nothing = Nothing
cleanOptional (Just raw) =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed
