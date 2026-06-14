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
productionCourseLocationLabel = "TDF Records – Quito"

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
        <> spanishDayLabel startDate

nextSaturdayOnOrAfter :: Day -> Day
nextSaturdayOnOrAfter day =
    let (_, _, weekDay) = toWeekDate day
        offset = (6 - weekDay) `mod` 7
     in addDays (fromIntegral offset) day

spanishDayLabel :: Day -> Text
spanishDayLabel day =
    let (_, month, dayOfMonth) = toGregorian day
     in "sábado " <> T.pack (show dayOfMonth) <> " de " <> spanishMonthName month

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

spanishMonthName :: Int -> Text
spanishMonthName 1 = "enero"
spanishMonthName 2 = "febrero"
spanishMonthName 3 = "marzo"
spanishMonthName 4 = "abril"
spanishMonthName 5 = "mayo"
spanishMonthName 6 = "junio"
spanishMonthName 7 = "julio"
spanishMonthName 8 = "agosto"
spanishMonthName 9 = "septiembre"
spanishMonthName 10 = "octubre"
spanishMonthName 11 = "noviembre"
spanishMonthName 12 = "diciembre"
spanishMonthName n = twoDigits n

twoDigits :: Show a => a -> Text
twoDigits value =
    let raw = show value
     in T.pack (if length raw == 1 then '0' : raw else raw)
