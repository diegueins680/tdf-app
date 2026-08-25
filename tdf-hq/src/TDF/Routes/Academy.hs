{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Routes.Academy
  ( AcademyAPI
  , EnrollReq(..)
  , ProgressReq(..)
  , ReferralClaimReq(..)
  , MicrocourseDTO(..)
  , LessonDTO(..)
  , NextCohortDTO(..)
  , validateAcademyRole
  , validateAcademyPlatform
  , validateAcademyReferralCode
  , validateAcademySlug
  ) where

import           Data.Aeson (FromJSON(parseJSON), Options(..), ToJSON, defaultOptions, genericParseJSON)
import           Data.Aeson.Types (Parser)
import           Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator, Space)
  , generalCategory
  , isAsciiLower
  , isControl
  , isDigit
  , isSpace
  )
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Servant

import           TDF.API.Types (rejectNullOptionalFields)

data EnrollReq = EnrollReq
  { email        :: Text
  , role         :: Text
  , platform     :: Maybe Text
  , referralCode :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON EnrollReq
instance FromJSON EnrollReq where
  parseJSON value = do
    rejectNullOptionalFields "EnrollReq" ["platform", "referralCode"] value
    EnrollReq rawEmail rawRole rawPlatform rawReferralCode <-
      genericParseJSON strictObjectOptions value
    EnrollReq
      <$> requiredEmail "email" rawEmail
      <*> requiredAcademyRole rawRole
      <*> optionalAcademyPlatform rawPlatform
      <*> optionalAcademyReferralCode rawReferralCode

data ProgressReq = ProgressReq
  { email :: Text
  , slug  :: Text
  , day   :: Int
  } deriving (Show, Generic)

instance FromJSON ProgressReq where
  parseJSON value = do
    ProgressReq rawEmail rawSlug rawDay <-
      genericParseJSON strictObjectOptions value
    ProgressReq
      <$> requiredEmail "email" rawEmail
      <*> requiredAcademySlug rawSlug
      <*> requiredPositiveDay rawDay
instance ToJSON ProgressReq

data ReferralClaimReq = ReferralClaimReq
  { email :: Text
  , code  :: Text
  } deriving (Show, Generic)

instance FromJSON ReferralClaimReq where
  parseJSON value = do
    ReferralClaimReq rawEmail rawCode <-
      genericParseJSON strictObjectOptions value
    ReferralClaimReq
      <$> requiredEmail "email" rawEmail
      <*> requiredAcademyReferralCode rawCode
instance ToJSON ReferralClaimReq

data LessonDTO = LessonDTO
  { lessonDay   :: Int
  , lessonTitle :: Text
  , lessonBody  :: Text
  } deriving (Show, Generic)

instance ToJSON LessonDTO

data MicrocourseDTO = MicrocourseDTO
  { mcSlug    :: Text
  , mcTitle   :: Text
  , mcSummary :: Maybe Text
  , lessons   :: [LessonDTO]
  } deriving (Show, Generic)

instance ToJSON MicrocourseDTO

data NextCohortDTO = NextCohortDTO
  { nextSlug     :: Text
  , nextTitle    :: Text
  , nextStartsAt :: UTCTime
  , nextEndsAt   :: UTCTime
  , nextSeatCap  :: Int
  , nextSeatsLeft :: Int
  } deriving (Show, Generic)

instance ToJSON NextCohortDTO

type AcademyAPI =
       "academy" :> "enroll" :> ReqBody '[JSON] EnrollReq :> Post '[JSON] NoContent
  :<|> "academy" :> "microcourse" :> Capture "slug" Text :> Get '[JSON] MicrocourseDTO
  :<|> "academy" :> "progress" :> ReqBody '[JSON] ProgressReq :> Post '[JSON] NoContent
  :<|> "referrals" :> "claim" :> ReqBody '[JSON] ReferralClaimReq :> Post '[JSON] NoContent
  :<|> "cohorts" :> "next" :> Get '[JSON] NextCohortDTO

strictObjectOptions :: Options
strictObjectOptions = defaultOptions { rejectUnknownFields = True }

requiredEmail :: String -> Text -> Parser Text
requiredEmail fieldName raw =
  let normalized = T.toLower (T.strip raw)
  in if T.null normalized
       then fail (fieldName <> " must not be blank")
       else
         if isValidAcademyEmail normalized
           then pure normalized
           else fail (fieldName <> " must be a valid email address")

requiredAcademyRole :: Text -> Parser Text
requiredAcademyRole raw =
  either (fail . T.unpack) pure (validateAcademyRole raw)

validateAcademyRole :: Text -> Either Text Text
validateAcademyRole raw =
  let normalized = T.toLower (T.strip raw)
  in if T.null normalized
       then Left "role must not be blank"
       else if normalized `elem` allowedAcademyRoles
         then Right normalized
         else Left "role must be one of: artist, manager"

allowedAcademyRoles :: [Text]
allowedAcademyRoles = ["artist", "manager"]

optionalAcademyPlatform :: Maybe Text -> Parser (Maybe Text)
optionalAcademyPlatform Nothing = pure Nothing
optionalAcademyPlatform (Just raw)
  | T.null (T.strip raw) && not (T.any isUnsafeAcademyPlatformChar raw) =
      pure Nothing
  | otherwise =
      Just <$> requiredAcademyPlatform raw

requiredAcademyPlatform :: Text -> Parser Text
requiredAcademyPlatform raw =
  either (fail . T.unpack) pure (validateAcademyPlatform raw)

validateAcademyPlatform :: Text -> Either Text Text
validateAcademyPlatform raw =
  let platformValue = T.strip raw
  in if T.any isUnsafeAcademyPlatformChar raw
       then Left "platform must not contain control characters, hidden Unicode formatting marks, or non-ASCII spaces"
       else if T.null platformValue
         then Left "platform must not be blank"
       else if T.length platformValue > maxAcademyPlatformChars
         then Left "platform must be 80 characters or fewer"
       else Right platformValue

maxAcademyPlatformChars :: Int
maxAcademyPlatformChars = 80

isUnsafeAcademyPlatformChar :: Char -> Bool
isUnsafeAcademyPlatformChar ch =
  isControl ch
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]
    || (generalCategory ch == Space && ch /= ' ')

optionalAcademyReferralCode :: Maybe Text -> Parser (Maybe Text)
optionalAcademyReferralCode Nothing = pure Nothing
optionalAcademyReferralCode (Just raw)
  | T.null (T.strip raw)
      && not (T.any isControl raw)
      && not (T.any isUnsafeAcademyReferralCodeBlankChar raw) =
      pure Nothing
  | otherwise = Just <$> requiredAcademyReferralCode raw

requiredAcademyReferralCode :: Text -> Parser Text
requiredAcademyReferralCode raw =
  either (fail . T.unpack) pure (validateAcademyReferralCode raw)

validateAcademyReferralCode :: Text -> Either Text Text
validateAcademyReferralCode raw =
  let codeValue = T.toUpper (T.strip raw)
  in if T.any isControl raw
       then Left "referral code must not contain control characters"
       else if T.any isUnsafeAcademyReferralCodeBlankChar raw
         then Left "referral code must not contain hidden Unicode formatting marks or non-ASCII spaces"
       else if T.null codeValue
       then Left "referral code must not be blank"
       else if T.length codeValue > maxAcademyReferralCodeChars
         then Left "referral code must be 128 characters or fewer"
       else if T.any isSpace codeValue
         then Left "referral code must not contain whitespace"
       else if T.any isControl codeValue
         then Left "referral code must not contain control characters"
       else if T.any isNonVisibleAscii codeValue
         then Left "referral code must contain visible ASCII characters only"
       else Right codeValue

maxAcademyReferralCodeChars :: Int
maxAcademyReferralCodeChars = 128

isUnsafeAcademyReferralCodeBlankChar :: Char -> Bool
isUnsafeAcademyReferralCodeBlankChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]
    || (generalCategory ch == Space && ch /= ' ')

isNonVisibleAscii :: Char -> Bool
isNonVisibleAscii ch = ch < '!' || ch > '~'

requiredAcademySlug :: Text -> Parser Text
requiredAcademySlug raw =
  either (fail . T.unpack) pure (validateAcademySlug raw)

validateAcademySlug :: Text -> Either Text Text
validateAcademySlug raw =
  let slugValue = T.toLower (T.strip raw)
  in if T.null slugValue
       then Left "slug must not be blank"
       else if T.length slugValue <= maxAcademySlugChars
            && T.any isAcademySlugAtom slugValue
            && T.all isAcademySlugChar slugValue
         then Right slugValue
         else Left
           ( "slug must contain only ASCII letters, numbers, and hyphens, "
               <> "include at least one letter or number, and be 96 characters or fewer"
           )

maxAcademySlugChars :: Int
maxAcademySlugChars = 96

isAcademySlugAtom :: Char -> Bool
isAcademySlugAtom ch = isAsciiLower ch || isDigit ch

isAcademySlugChar :: Char -> Bool
isAcademySlugChar ch = isAcademySlugAtom ch || ch == '-'

requiredPositiveDay :: Int -> Parser Int
requiredPositiveDay dayNumber =
  if dayNumber <= 0
    then fail "day must be positive"
    else pure dayNumber

isValidAcademyEmail :: Text -> Bool
isValidAcademyEmail candidate =
  case T.splitOn "@" candidate of
    [local, domain] ->
      let domainLabels = T.splitOn "." domain
      in T.length candidate <= maxAcademyEmailChars
          && isValidAcademyEmailLocalPart local
          && not (T.null domain)
          && not (T.any (\ch -> isSpace ch || isControl ch) candidate)
          && T.isInfixOf "." domain
          && all isValidAcademyEmailDomainLabel domainLabels
          && isValidAcademyEmailTopLevelLabel domainLabels
    _ -> False

maxAcademyEmailChars :: Int
maxAcademyEmailChars = 254

isValidAcademyEmailLocalPart :: Text -> Bool
isValidAcademyEmailLocalPart local =
  not (T.null local)
    && T.length local <= maxAcademyEmailLocalPartChars
    && not (T.isPrefixOf "." local)
    && not (T.isSuffixOf "." local)
    && not (".." `T.isInfixOf` local)
    && T.all isValidAcademyEmailLocalChar local

maxAcademyEmailLocalPartChars :: Int
maxAcademyEmailLocalPartChars = 64

isValidAcademyEmailLocalChar :: Char -> Bool
isValidAcademyEmailLocalChar ch =
  isAsciiLower ch || isDigit ch || ch `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidAcademyEmailDomainLabel :: Text -> Bool
isValidAcademyEmailDomainLabel label =
  not (T.null label)
    && T.length label <= maxAcademyEmailDomainLabelChars
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidAcademyEmailDomainChar label

maxAcademyEmailDomainLabelChars :: Int
maxAcademyEmailDomainLabelChars = 63

isValidAcademyEmailDomainChar :: Char -> Bool
isValidAcademyEmailDomainChar ch =
  isAsciiLower ch || isDigit ch || ch == '-'

isValidAcademyEmailTopLevelLabel :: [Text] -> Bool
isValidAcademyEmailTopLevelLabel labels =
  case reverse labels of
    topLevelLabel : _ ->
      T.length topLevelLabel >= 2 && T.any isAsciiLower topLevelLabel
    [] -> False
