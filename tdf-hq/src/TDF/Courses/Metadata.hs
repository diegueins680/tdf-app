{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | Public course metadata: the built-in production course, the projection of
-- stored courses, and the landing and WhatsApp links derived from them. Shared
-- by HTTP handlers and background workers; it must not depend on either.
module TDF.Courses.Metadata
  ( productionCourseSlug
  , productionCoursePrice
  , productionCourseCapacity
  , buildLandingUrl
  , buildLandingUrlFor
  , buildWhatsappCtaFor
  , resolveWhatsappCtaNumber
  , isInternationalWhatsappCtaNumber
  , courseMetadataFor
  , productionCourseMetadataForStartDate
  , firstSaturdayOfConfiguredProductionSlug
  , monthSlugToNumber
  , nextSaturdayOnOrAfterDay
  , toCourseMetadata
  , normalizeSlug
  , isAsciiPhoneDigit
  , courseMetaSlug
  , courseMetaTitle
  , courseMetaSubtitle
  , courseMetaFormat
  , courseMetaDuration
  , courseMetaPrice
  , courseMetaCurrency
  , courseMetaCapacity
  , courseMetaSessionStartHour
  , courseMetaSessionDurationHours
  , courseMetaLocationLabel
  , courseMetaLocationMapUrl
  , courseMetaDaws
  , courseMetaIncludes
  , courseMetaInstructorName
  , courseMetaInstructorBio
  , courseMetaInstructorAvatarUrl
  , courseMetaSessions
  , courseMetaSyllabus
  , courseMetaWhatsappCtaUrl
  , courseMetaLandingUrl
  , courseSessionLabel
  , courseSessionDate
  , syllabusItemTitle
  , syllabusItemTopics
  ) where

import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (Day, addDays, defaultTimeLocale, formatTime, fromGregorian)
import           Database.Persist (Entity(..))
import           Network.HTTP.Types.URI (urlEncode)
import           Text.Read (readMaybe)

import           TDF.Config (AppConfig(..), courseInstructorAvatarFallback, courseMapFallback, courseSlugFallback, resolveConfiguredAppBase)
import qualified TDF.CoursePhone as CoursePhone
import qualified TDF.Courses.Production as ProductionCourse
import qualified TDF.Courses.PublicMetadata as CoursePublic
import           TDF.Courses.PublicMetadata (cleanOptional, sanitizeStoredCoursePublicUrl)
import           TDF.Routes.Courses (CourseMetadata(..), CourseSession(..), SyllabusItem(..))
import qualified TDF.Routes.Courses as Courses
import qualified TDF.Trials.Models as Trials
import           TDF.WhatsApp.Transport (WhatsAppEnv(..))

productionCourseSlug :: AppConfig -> Text
productionCourseSlug = courseSlugFallback
productionCoursePrice :: Double
productionCoursePrice = fromIntegral ProductionCourse.productionCoursePriceCents / 100
productionCourseCapacity :: Int
productionCourseCapacity = ProductionCourse.productionCourseCapacity

-- Backward-compatible helpers used by cron jobs
buildLandingUrl :: AppConfig -> Text
buildLandingUrl cfg = buildLandingUrlFor cfg (productionCourseSlug cfg)

courseMetadataFor :: AppConfig -> Maybe Text -> Text -> Maybe CourseMetadata
courseMetadataFor cfg mWaContact slugVal =
  let fallbackSlug = productionCourseSlug cfg
      startDate = firstSaturdayOfConfiguredProductionSlug fallbackSlug
  in if normalizeSlug slugVal == fallbackSlug
       then Just (productionCourseMetadataForStartDate cfg mWaContact fallbackSlug startDate Nothing)
       else Nothing

courseMetaSlug :: CourseMetadata -> Text
courseMetaSlug Courses.CourseMetadata{Courses.slug = value} = value

courseMetaTitle :: CourseMetadata -> Text
courseMetaTitle Courses.CourseMetadata{Courses.title = value} = value

courseMetaSubtitle :: CourseMetadata -> Text
courseMetaSubtitle Courses.CourseMetadata{Courses.subtitle = value} = value

courseMetaFormat :: CourseMetadata -> Text
courseMetaFormat Courses.CourseMetadata{Courses.format = value} = value

courseMetaDuration :: CourseMetadata -> Text
courseMetaDuration Courses.CourseMetadata{Courses.duration = value} = value

courseMetaPrice :: CourseMetadata -> Double
courseMetaPrice Courses.CourseMetadata{Courses.price = value} = value

courseMetaCurrency :: CourseMetadata -> Text
courseMetaCurrency Courses.CourseMetadata{Courses.currency = value} = value

courseMetaCapacity :: CourseMetadata -> Int
courseMetaCapacity Courses.CourseMetadata{Courses.capacity = value} = value

courseMetaSessionStartHour :: CourseMetadata -> Int
courseMetaSessionStartHour Courses.CourseMetadata{Courses.sessionStartHour = value} = value

courseMetaSessionDurationHours :: CourseMetadata -> Int
courseMetaSessionDurationHours Courses.CourseMetadata{Courses.sessionDurationHours = value} = value

courseMetaLocationLabel :: CourseMetadata -> Text
courseMetaLocationLabel Courses.CourseMetadata{Courses.locationLabel = value} = value

courseMetaLocationMapUrl :: CourseMetadata -> Text
courseMetaLocationMapUrl Courses.CourseMetadata{Courses.locationMapUrl = value} = value

courseMetaDaws :: CourseMetadata -> [Text]
courseMetaDaws Courses.CourseMetadata{Courses.daws = value} = value

courseMetaIncludes :: CourseMetadata -> [Text]
courseMetaIncludes Courses.CourseMetadata{Courses.includes = value} = value

courseMetaInstructorName :: CourseMetadata -> Maybe Text
courseMetaInstructorName Courses.CourseMetadata{Courses.instructorName = value} = value

courseMetaInstructorBio :: CourseMetadata -> Maybe Text
courseMetaInstructorBio Courses.CourseMetadata{Courses.instructorBio = value} = value

courseMetaInstructorAvatarUrl :: CourseMetadata -> Maybe Text
courseMetaInstructorAvatarUrl Courses.CourseMetadata{Courses.instructorAvatarUrl = value} = value

courseMetaSessions :: CourseMetadata -> [CourseSession]
courseMetaSessions Courses.CourseMetadata{Courses.sessions = value} = value

courseMetaSyllabus :: CourseMetadata -> [SyllabusItem]
courseMetaSyllabus Courses.CourseMetadata{Courses.syllabus = value} = value

courseMetaWhatsappCtaUrl :: CourseMetadata -> Text
courseMetaWhatsappCtaUrl Courses.CourseMetadata{Courses.whatsappCtaUrl = value} = value

courseMetaLandingUrl :: CourseMetadata -> Text
courseMetaLandingUrl Courses.CourseMetadata{Courses.landingUrl = value} = value

courseSessionLabel :: CourseSession -> Text
courseSessionLabel Courses.CourseSession{Courses.label = value} = value

courseSessionDate :: CourseSession -> Day
courseSessionDate Courses.CourseSession{Courses.date = value} = value

syllabusItemTitle :: SyllabusItem -> Text
syllabusItemTitle Courses.SyllabusItem{Courses.title = value} = value

syllabusItemTopics :: SyllabusItem -> [Text]
syllabusItemTopics Courses.SyllabusItem{Courses.topics = value} = value

productionCourseMetadataForStartDate
  :: AppConfig
  -> Maybe Text
  -> Text
  -> Day
  -> Maybe CourseMetadata
  -> CourseMetadata
productionCourseMetadataForStartDate cfg mWaContact slugVal startDate mTemplate =
  let landing = buildLandingUrlFor cfg slugVal
      templateText field defaultValue =
        case mTemplate of
          Just meta ->
            let value = field meta
            in if T.null (T.strip value) then defaultValue else value
          Nothing -> defaultValue
      templateList field defaultValue =
        case mTemplate of
          Just meta ->
            let values = filter (not . T.null . T.strip) (field meta)
            in if null values then defaultValue else values
          Nothing -> defaultValue
      templateMaybe field defaultValue =
        case cleanOptional (mTemplate >>= field) of
          Just value -> Just value
          Nothing -> defaultValue
      templateInt field defaultValue =
        case mTemplate of
          Just meta | field meta > 0 -> field meta
          _ -> defaultValue
      templatePrice =
        case mTemplate of
          Just meta | courseMetaPrice meta >= 0 -> courseMetaPrice meta
          _ -> productionCoursePrice
      sessions =
        [ CourseSession labelTxt dayVal
        | (labelTxt, dayVal) <- ProductionCourse.productionCourseSessions startDate
        ]
      syllabus =
        [ SyllabusItem titleTxt topics
        | (titleTxt, topics) <- ProductionCourse.productionCourseSyllabus
        ]
      titleVal = templateText courseMetaTitle ProductionCourse.productionCourseTitle
  in CourseMetadata
      { slug = slugVal
      , title = titleVal
      , subtitle = ProductionCourse.productionCourseSubtitleForStartDate startDate
      , format = templateText courseMetaFormat ProductionCourse.productionCourseFormat
      , duration = templateText courseMetaDuration ProductionCourse.productionCourseDuration
      , price = templatePrice
      , currency = templateText courseMetaCurrency (defaultCurrency cfg)
      , capacity = templateInt courseMetaCapacity ProductionCourse.productionCourseCapacity
      , remaining = templateInt courseMetaCapacity ProductionCourse.productionCourseCapacity
      , sessionStartHour = templateInt courseMetaSessionStartHour ProductionCourse.productionCourseSessionStartHour
      , sessionDurationHours = templateInt courseMetaSessionDurationHours ProductionCourse.productionCourseSessionDurationHours
      , locationLabel = templateText courseMetaLocationLabel ProductionCourse.productionCourseLocationLabel
      , locationMapUrl = templateText courseMetaLocationMapUrl (courseMapFallback cfg)
      , daws = templateList courseMetaDaws ProductionCourse.productionCourseDaws
      , includes = templateList courseMetaIncludes ProductionCourse.productionCourseIncludes
      , instructorName = templateMaybe courseMetaInstructorName (Just ProductionCourse.productionCourseInstructorName)
      , instructorBio = templateMaybe courseMetaInstructorBio (Just ProductionCourse.productionCourseInstructorBio)
      , instructorAvatarUrl = templateMaybe courseMetaInstructorAvatarUrl (Just (courseInstructorAvatarFallback cfg))
      , sessions = sessions
      , syllabus = syllabus
      , whatsappCtaUrl = buildWhatsappCtaFor mWaContact titleVal landing
      , landingUrl = landing
      , checkoutTerms = Nothing
      }

firstSaturdayOfConfiguredProductionSlug :: Text -> Day
firstSaturdayOfConfiguredProductionSlug slugVal =
  let fallback = fromGregorian 2026 6 6
      parts = T.splitOn "-" slugVal
      mYear = readMaybe . T.unpack =<< listToMaybe (reverse parts)
      mMonth = listToMaybe (mapMaybe monthSlugToNumber parts)
  in case (mYear, mMonth) of
       (Just y, Just m) -> nextSaturdayOnOrAfterDay (fromGregorian y m 1)
       _ -> fallback

monthSlugToNumber :: Text -> Maybe Int
monthSlugToNumber "ene" = Just 1
monthSlugToNumber "feb" = Just 2
monthSlugToNumber "mar" = Just 3
monthSlugToNumber "abr" = Just 4
monthSlugToNumber "may" = Just 5
monthSlugToNumber "jun" = Just 6
monthSlugToNumber "jul" = Just 7
monthSlugToNumber "ago" = Just 8
monthSlugToNumber "sep" = Just 9
monthSlugToNumber "oct" = Just 10
monthSlugToNumber "nov" = Just 11
monthSlugToNumber "dic" = Just 12
monthSlugToNumber _ = Nothing

nextSaturdayOnOrAfterDay :: Day -> Day
nextSaturdayOnOrAfterDay day =
  fromMaybe day . listToMaybe $
    [ candidate
    | offset <- [0..6]
    , let candidate = addDays offset day
    , formatTime defaultTimeLocale "%u" candidate == "6"
    ]

toCourseMetadata
  :: AppConfig
  -> WhatsAppEnv
  -> Trials.Course
  -> [Entity Trials.CourseSessionModel]
  -> [Entity Trials.CourseSyllabusItem]
  -> CourseMetadata
toCourseMetadata cfg waEnv course sessions syllabus =
  let slugVal = Trials.courseSlug course
      landingUrlVal =
        fromMaybe
          (buildLandingUrlFor cfg slugVal)
          (sanitizeStoredCoursePublicUrl "landingUrl" (Trials.courseLandingUrl course))
      whatsappUrl =
        fromMaybe
          (buildWhatsappCtaFor (waContactNumber waEnv) (Trials.courseTitle course) landingUrlVal)
          (sanitizeStoredCoursePublicUrl "whatsappCtaUrl" (Trials.courseWhatsappCtaUrl course))
      dawsList = filter (not . T.null) (maybe [] (map T.strip) (Trials.courseDaws course))
      includesList = filter (not . T.null) (maybe [] (map T.strip) (Trials.courseIncludes course))
      instructorNameVal = cleanOptional (Trials.courseInstructorName course)
      instructorBioVal = cleanOptional (Trials.courseInstructorBio course)
      instructorAvatarVal =
        sanitizeStoredCoursePublicUrl "instructorAvatarUrl" (Trials.courseInstructorAvatarUrl course)
      toSession (Entity _ s) =
        CourseSession
          { label = Trials.courseSessionModelLabel s
          , date = Trials.courseSessionModelDate s
          }
      toSyllabus (Entity _ s) =
        SyllabusItem
          { title = Trials.courseSyllabusItemTitle s
          , topics = filter (not . T.null . T.strip) (Trials.courseSyllabusItemTopics s)
          }
  in CourseMetadata
      { slug = slugVal
      , title = Trials.courseTitle course
      , subtitle = fromMaybe "" (Trials.courseSubtitle course)
      , format = fromMaybe "" (Trials.courseFormat course)
      , duration = fromMaybe "" (Trials.courseDuration course)
      , price = fromIntegral (Trials.coursePriceCents course) / 100
      , currency = Trials.courseCurrency course
      , capacity = Trials.courseCapacity course
      , remaining = Trials.courseCapacity course
      , sessionStartHour = fromMaybe 0 (Trials.courseSessionStartHour course)
      , sessionDurationHours = fromMaybe 0 (Trials.courseSessionDurationHours course)
      , locationLabel = fromMaybe "" (Trials.courseLocationLabel course)
      , locationMapUrl =
          fromMaybe
            (courseMapFallback cfg)
            (sanitizeStoredCoursePublicUrl "locationMapUrl" (Trials.courseLocationMapUrl course))
      , daws = dawsList
      , includes = includesList
      , instructorName = instructorNameVal
      , instructorBio = instructorBioVal
      , instructorAvatarUrl = instructorAvatarVal
      , sessions = map toSession sessions
      , syllabus = map toSyllabus syllabus
      , whatsappCtaUrl = whatsappUrl
      , landingUrl = landingUrlVal
      , checkoutTerms = Nothing
      }

buildLandingUrlFor :: AppConfig -> Text -> Text
buildLandingUrlFor cfg slugVal =
  let base = resolveConfiguredAppBase cfg
  in base <> "/curso/" <> slugVal

buildWhatsappCtaFor :: Maybe Text -> Text -> Text -> Text
buildWhatsappCtaFor mNumber courseTitle landingUrl =
  let msg = "INSCRIBIRME " <> courseTitle <> " " <> landingUrl
      encoded = TE.decodeUtf8 (urlEncode True (TE.encodeUtf8 msg))
  in case resolveWhatsappCtaNumber mNumber of
       Just num -> "https://wa.me/" <> num <> "?text=" <> encoded
       Nothing  -> "https://wa.me/?text=" <> encoded

resolveWhatsappCtaNumber :: Maybe Text -> Maybe Text
resolveWhatsappCtaNumber mNumber =
  case mNumber >>= CoursePhone.normalizeInternationalPhoneInput of
    Just phone ->
      let digitsOnly = T.filter isAsciiPhoneDigit phone
      in if isInternationalWhatsappCtaNumber digitsOnly
           then Just digitsOnly
           else Nothing
    Nothing -> Nothing

isInternationalWhatsappCtaNumber :: Text -> Bool
isInternationalWhatsappCtaNumber = CoursePublic.isInternationalWhatsappCtaNumber

isAsciiPhoneDigit :: Char -> Bool
isAsciiPhoneDigit ch = ch >= '0' && ch <= '9'

normalizeSlug :: Text -> Text
normalizeSlug = T.toLower . T.strip
