{-# LANGUAGE OverloadedStrings #-}

module TDF.Courses.Production (
    productionCourseMinimumLeadDays,
    productionCourseTitle,
    productionCourseFormat,
    productionCourseDuration,
    productionCoursePriceCents,
    productionCourseCapacity,
    productionCourseSessionStartHour,
    productionCourseSessionDurationHours,
    productionCourseLocationLabel,
    productionCourseInstructorName,
    productionCourseInstructorBio,
    productionCourseDaws,
    productionCourseIncludes,
    productionCourseSyllabus,
    minimumProductionStartDate,
    nextProductionCourseStartDate,
    productionCourseSessionDates,
    productionCourseSessions,
    productionCourseSlugForStartDate,
    productionCourseDaySlugForStartDate,
    productionCourseSubtitleForStartDate,
    dayLabelForLocale,
    monthNameForLocale,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, addDays, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

productionCourseMinimumLeadDays :: Integer
productionCourseMinimumLeadDays = 28

productionCourseTitle :: Text
productionCourseTitle = "Curso de Producción Musical"

productionCourseFormat :: Text
productionCourseFormat = "Presencial"

productionCourseDuration :: Text
productionCourseDuration = "Cuatro sábados (16 horas en total)"

productionCoursePriceCents :: Int
productionCoursePriceCents = 15000

productionCourseCapacity :: Int
productionCourseCapacity = 16

productionCourseSessionStartHour :: Int
productionCourseSessionStartHour = 15

productionCourseSessionDurationHours :: Int
productionCourseSessionDurationHours = 4

productionCourseLocationLabel :: Text
productionCourseLocationLabel = "TDF Records"

productionCourseInstructorName :: Text
productionCourseInstructorName = "Esteban Muñoz"

productionCourseInstructorBio :: Text
productionCourseInstructorBio = "Productor en TDF Records. 10+ años grabando bandas, rap y electrónica."

productionCourseDaws :: [Text]
productionCourseDaws = ["Logic", "Luna"]

productionCourseIncludes :: [Text]
productionCourseIncludes =
    [ "Acceso a grabaciones"
    , "Certificado de participación"
    , "Mentorías"
    , "Grupo de WhatsApp"
    , "Acceso a la plataforma de TDF Records"
    ]

productionCourseSyllabus :: [(Text, [Text])]
productionCourseSyllabus =
    [ ("Introducción a la producción musical", ["Conceptos básicos", "Herramientas esenciales"])
    , ("Grabación y captura de audio", ["Técnicas de grabación", "Configuración de micrófonos"])
    , ("Mezcla y edición", ["Ecualización y compresión", "Balance y panoramización"])
    , ("Masterización y publicación", ["Mastering", "Distribución digital"])
    ]

minimumProductionStartDate :: Day -> Day
minimumProductionStartDate today = addDays productionCourseMinimumLeadDays today

nextProductionCourseStartDate :: Day -> Day
nextProductionCourseStartDate today = nextSaturdayOnOrAfter (minimumProductionStartDate today)

productionCourseSessionDates :: Day -> [Day]
productionCourseSessionDates startDate =
    [ addDays 0 startDate
    , addDays 7 startDate
    , addDays 14 startDate
    , addDays 21 startDate
    ]

productionCourseSessions :: Day -> [(Text, Day)]
productionCourseSessions startDate =
    zip
        [ "Sábado 1 · Introducción"
        , "Sábado 2 · Grabación"
        , "Sábado 3 · Mezcla"
        , "Sábado 4 · Masterización"
        ]
        (productionCourseSessionDates startDate)

productionCourseSlugForStartDate :: Day -> Text
productionCourseSlugForStartDate startDate =
    let (year, month, _) = toGregorian startDate
     in "produccion-musical-" <> monthSlug month <> "-" <> T.pack (show year)

productionCourseDaySlugForStartDate :: Day -> Text
productionCourseDaySlugForStartDate startDate =
    let (year, month, day) = toGregorian startDate
     in "produccion-musical-" <> monthSlug month <> "-" <> twoDigits day <> "-" <> T.pack (show year)

productionCourseSubtitleForStartDate :: Day -> Text
productionCourseSubtitleForStartDate startDate =
    "Presencial · Cuatro sábados · 16 horas en total · Próximo inicio: "
        <> dayLabelForLocale "es" startDate

nextSaturdayOnOrAfter :: Day -> Day
nextSaturdayOnOrAfter day =
    let (_, _, weekDay) = toWeekDate day
        offset = (6 - weekDay) `mod` 7
     in addDays (fromIntegral offset) day

dayLabelForLocale :: Text -> Day -> Text
dayLabelForLocale rawLocale day =
    let locale = normalizeLocale rawLocale
        (_, month, dayOfMonth) = toGregorian day
        (_, _, weekDay) = toWeekDate day
        weekday = weekdayNameForLocale locale weekDay
        monthName = monthNameForLocale locale month
        dayNumber = T.pack (show dayOfMonth)
     in case locale of
          "es" -> weekday <> " " <> dayNumber <> " de " <> monthName
          "fr" -> weekday <> " " <> dayNumber <> " " <> monthName
          "de" -> weekday <> ", " <> dayNumber <> ". " <> monthName
          "pt" -> weekday <> ", " <> dayNumber <> " de " <> monthName
          _ -> weekday <> ", " <> monthName <> " " <> dayNumber

monthSlug :: Int -> Text
monthSlug 1 = "ene"
monthSlug 2 = "feb"
monthSlug 3 = "mar"
monthSlug 4 = "abr"
monthSlug 5 = "may"
monthSlug 6 = "jun"
monthSlug 7 = "jul"
monthSlug 8 = "ago"
monthSlug 9 = "sep"
monthSlug 10 = "oct"
monthSlug 11 = "nov"
monthSlug 12 = "dic"
monthSlug n = twoDigits n

monthNameForLocale :: Text -> Int -> Text
monthNameForLocale rawLocale month =
    let locale = normalizeLocale rawLocale
        monthNames = case locale of
          "es" -> ["enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"]
          "fr" -> ["janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre", "octobre", "novembre", "décembre"]
          "de" -> ["Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"]
          "pt" -> ["janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"]
          _ -> ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
     in if month >= 1 && month <= 12
          then monthNames !! (month - 1)
          else twoDigits month

weekdayNameForLocale :: Text -> Int -> Text
weekdayNameForLocale locale weekDay =
    let names = case locale of
          "es" -> ["lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"]
          "fr" -> ["lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"]
          "de" -> ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"]
          "pt" -> ["segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado", "domingo"]
          _ -> ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
     in if weekDay >= 1 && weekDay <= 7 then names !! (weekDay - 1) else ""

normalizeLocale :: Text -> Text
normalizeLocale = T.toLower . T.takeWhile (\ch -> ch /= '-' && ch /= '_') . T.strip

twoDigits :: Show a => a -> Text
twoDigits value =
    let raw = show value
     in T.pack (if length raw == 1 then '0' : raw else raw)
