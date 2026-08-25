{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Artists.Promotion
  ( reportTimezoneLabel
  , listArtistPromoSlotsForDay
  , createArtistPromoSlotRecord
  , updateArtistPromoSlotRecord
  , deleteArtistPromoSlotRecord
  , loadArtistPromoDayReport
  , validateArtistPromoSlotUpsert
  , renderArtistPromoDayReportLatex
  , generateArtistPromoDayReportPdf
  ) where

import           Control.Monad.IO.Class      (MonadIO)
import           Data.List                   (find)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   ( Day
                                             , TimeOfDay
                                             , UTCTime
                                             , defaultTimeLocale
                                             , formatTime
                                             , parseTimeM
                                             , toGregorian
                                             )
import           Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.ByteString.Lazy        as BL
import           Database.Persist            ( Entity(..)
                                             , SelectOpt(..)
                                             , delete
                                             , get
                                             , getEntity
                                             , getJustEntity
                                             , insert
                                             , selectList
                                             , update
                                             , (==.)
                                             , (=.)
                                             )
import           Database.Persist.Sql        (SqlPersistT, fromSqlKey)

import           TDF.DTO                     ( ArtistPromoDayReportDTO(..)
                                             , ArtistPromoSlotDTO(..)
                                             , ArtistPromoSlotUpsert(..)
                                             )
import           TDF.Handlers.InputList      (generateInputListPdf)
import           TDF.Models

data ArtistPromoSlotWrite = ArtistPromoSlotWrite
  { apswDay             :: Day
  , apswStartTime       :: TimeOfDay
  , apswMedium          :: Text
  , apswProgram         :: Text
  , apswInterviewerHost :: Text
  , apswBandMembers     :: Text
  , apswStatus          :: Maybe Text
  , apswNotes           :: Maybe Text
  }

reportTimezoneLabel :: Text
reportTimezoneLabel = "Hora de Ecuador (America/Guayaquil)"

listArtistPromoSlotsForDay
  :: MonadIO m
  => PartyId
  -> Day
  -> SqlPersistT m [ArtistPromoSlotDTO]
listArtistPromoSlotsForDay artistKey dayVal = do
  rows <- selectList
    [ ArtistPromoSlotArtistPartyId ==. artistKey
    , ArtistPromoSlotDay ==. dayVal
    ]
    [ Asc ArtistPromoSlotStartTime
    , Asc ArtistPromoSlotMedium
    , Asc ArtistPromoSlotProgram
    , Asc ArtistPromoSlotId
    ]
  pure (map artistPromoSlotEntityToDTO rows)

createArtistPromoSlotRecord
  :: MonadIO m
  => PartyId
  -> ArtistPromoSlotUpsert
  -> UTCTime
  -> Either Text (SqlPersistT m ArtistPromoSlotDTO)
createArtistPromoSlotRecord artistKey payload now = do
  ArtistPromoSlotWrite{..} <- validateArtistPromoSlotUpsert payload
  pure $ do
    promotionId <- insert ArtistPromoSlot
      { artistPromoSlotArtistPartyId = artistKey
      , artistPromoSlotDay = apswDay
      , artistPromoSlotStartTime = apswStartTime
      , artistPromoSlotMedium = apswMedium
      , artistPromoSlotProgram = apswProgram
      , artistPromoSlotInterviewerHost = apswInterviewerHost
      , artistPromoSlotBandMembers = apswBandMembers
      , artistPromoSlotStatus = apswStatus
      , artistPromoSlotNotes = apswNotes
      , artistPromoSlotCreatedAt = now
      , artistPromoSlotUpdatedAt = now
      }
    entity <- getJustEntity promotionId
    pure (artistPromoSlotEntityToDTO entity)

updateArtistPromoSlotRecord
  :: MonadIO m
  => PartyId
  -> ArtistPromoSlotId
  -> ArtistPromoSlotUpsert
  -> UTCTime
  -> Either Text (SqlPersistT m (Maybe ArtistPromoSlotDTO))
updateArtistPromoSlotRecord artistKey promotionKey payload now = do
  ArtistPromoSlotWrite{..} <- validateArtistPromoSlotUpsert payload
  pure $ do
    mExisting <- getEntity promotionKey
    case mExisting of
      Nothing -> pure Nothing
      Just (Entity _ existing)
        | artistPromoSlotArtistPartyId existing /= artistKey -> pure Nothing
        | otherwise -> do
            update promotionKey
              [ ArtistPromoSlotDay =. apswDay
              , ArtistPromoSlotStartTime =. apswStartTime
              , ArtistPromoSlotMedium =. apswMedium
              , ArtistPromoSlotProgram =. apswProgram
              , ArtistPromoSlotInterviewerHost =. apswInterviewerHost
              , ArtistPromoSlotBandMembers =. apswBandMembers
              , ArtistPromoSlotStatus =. apswStatus
              , ArtistPromoSlotNotes =. apswNotes
              , ArtistPromoSlotUpdatedAt =. now
              ]
            entity <- getJustEntity promotionKey
            pure (Just (artistPromoSlotEntityToDTO entity))

deleteArtistPromoSlotRecord
  :: MonadIO m
  => PartyId
  -> ArtistPromoSlotId
  -> SqlPersistT m Bool
deleteArtistPromoSlotRecord artistKey promotionKey = do
  mExisting <- getEntity promotionKey
  case mExisting of
    Nothing -> pure False
    Just (Entity _ existing)
      | artistPromoSlotArtistPartyId existing /= artistKey -> pure False
      | otherwise -> do
          delete promotionKey
          pure True

loadArtistPromoDayReport
  :: MonadIO m
  => PartyId
  -> Day
  -> SqlPersistT m (Maybe ArtistPromoDayReportDTO)
loadArtistPromoDayReport artistKey dayVal = do
  mParty <- get artistKey
  case mParty of
    Nothing -> pure Nothing
    Just party -> do
      entries <- listArtistPromoSlotsForDay artistKey dayVal
      pure $
        Just ArtistPromoDayReportDTO
          { apdArtistId = fromSqlKey artistKey
          , apdArtistName = partyDisplayName party
          , apdDay = dayVal
          , apdTimezone = reportTimezoneLabel
          , apdDayHeader = formatArtistPromoDayHeader dayVal
          , apdEntries = entries
          }

validateArtistPromoSlotUpsert :: ArtistPromoSlotUpsert -> Either Text ArtistPromoSlotWrite
validateArtistPromoSlotUpsert ArtistPromoSlotUpsert{..} = do
  startTimeVal <- validateStartTime apsuStartTime
  mediumVal <- requireField "medium" apsuMedium
  programVal <- requireField "program" apsuProgram
  hostVal <- requireField "interviewerHost" apsuInterviewerHost
  bandMembersVal <- requireField "bandMembers" apsuBandMembers
  pure ArtistPromoSlotWrite
    { apswDay = apsuDay
    , apswStartTime = startTimeVal
    , apswMedium = mediumVal
    , apswProgram = programVal
    , apswInterviewerHost = hostVal
    , apswBandMembers = bandMembersVal
    , apswStatus = cleanOptionalText apsuStatus
    , apswNotes = cleanOptionalText apsuNotes
    }

renderArtistPromoDayReportLatex :: ArtistPromoDayReportDTO -> Text
renderArtistPromoDayReportLatex ArtistPromoDayReportDTO{..} =
  let bodyLines =
        if null apdEntries
          then ["\\multicolumn{7}{@{}l@{}}{Sin actividades registradas para este día.} \\\\"]
          else map renderRow apdEntries
  in T.unlines $
       [ "\\documentclass[a4paper,landscape,10pt]{article}"
       , "\\usepackage[margin=12mm]{geometry}"
       , "\\usepackage{array,booktabs,longtable,xcolor}"
       , "\\definecolor{rowalt}{RGB}{246,246,246}"
       , "\\rowcolors{2}{rowalt}{white}"
       , "\\pagestyle{empty}"
       , "\\begin{document}"
       , "\\section*{Reporte diario de promoción}"
       , "\\noindent\\textbf{Artista:} " <> latexEscape apdArtistName <> "\\\\"
       , "\\textbf{Día:} " <> latexEscape apdDayHeader <> "\\\\"
       , "\\textbf{Horario:} " <> latexEscape apdTimezone
       , "\\vspace{0.6em}"
       , "\\small"
       , "\\renewcommand{\\arraystretch}{1.18}"
       , "\\begin{longtable}{@{}>{\\raggedright\\arraybackslash}p{1.5cm}>{\\raggedright\\arraybackslash}p{2.5cm}>{\\raggedright\\arraybackslash}p{3.2cm}>{\\raggedright\\arraybackslash}p{3.7cm}>{\\raggedright\\arraybackslash}p{4.0cm}>{\\raggedright\\arraybackslash}p{2.2cm}>{\\raggedright\\arraybackslash}p{5.0cm}@{}}"
       , "\\toprule"
       , "Hora & Medio & Programa & Entrevistador / host & Miembros participantes & Estado & Notas \\\\"
       , "\\midrule"
       ]
       ++ bodyLines ++
       [ "\\bottomrule"
       , "\\end{longtable}"
       , "\\end{document}"
       ]
  where
    renderRow ArtistPromoSlotDTO{..} =
      T.intercalate
        " & "
        (map latexEscape
          [ apsStartTime
          , apsMedium
          , apsProgram
          , apsInterviewerHost
          , apsBandMembers
          , fromMaybe "—" apsStatus
          , fromMaybe "—" apsNotes
          ]) <> " \\\\"

generateArtistPromoDayReportPdf :: ArtistPromoDayReportDTO -> IO (Either Text BL.ByteString)
generateArtistPromoDayReportPdf = generateInputListPdf . renderArtistPromoDayReportLatex

artistPromoSlotEntityToDTO :: Entity ArtistPromoSlot -> ArtistPromoSlotDTO
artistPromoSlotEntityToDTO (Entity promotionId slot) =
  ArtistPromoSlotDTO
    { apsPromotionId = fromSqlKey promotionId
    , apsArtistId = fromSqlKey (artistPromoSlotArtistPartyId slot)
    , apsDay = artistPromoSlotDay slot
    , apsStartTime = formatArtistPromoTime (artistPromoSlotStartTime slot)
    , apsMedium = artistPromoSlotMedium slot
    , apsProgram = artistPromoSlotProgram slot
    , apsInterviewerHost = artistPromoSlotInterviewerHost slot
    , apsBandMembers = artistPromoSlotBandMembers slot
    , apsStatus = artistPromoSlotStatus slot
    , apsNotes = artistPromoSlotNotes slot
    , apsCreatedAt = artistPromoSlotCreatedAt slot
    , apsUpdatedAt = artistPromoSlotUpdatedAt slot
    }

cleanOptionalText :: Maybe Text -> Maybe Text
cleanOptionalText = (>>= nonBlank)
  where
    nonBlank raw =
      let trimmed = normalizeCellText raw
      in if T.null trimmed then Nothing else Just trimmed

requireField :: Text -> Text -> Either Text Text
requireField fieldName raw =
  let trimmed = normalizeCellText raw
  in if T.null trimmed
       then Left (fieldName <> " es obligatorio")
       else Right trimmed

validateStartTime :: Text -> Either Text TimeOfDay
validateStartTime raw =
  let trimmed = T.strip raw
      parsed =
        find
          (const True)
          [ value
          | formatter <- ["%H:%M", "%H:%M:%S"]
          , value <- maybeToList (parseTimeM True defaultTimeLocale formatter (T.unpack trimmed) :: Maybe TimeOfDay)
          ]
  in maybe (Left "startTime debe tener formato HH:MM") Right parsed

formatArtistPromoTime :: TimeOfDay -> Text
formatArtistPromoTime = T.pack . formatTime defaultTimeLocale "%H:%M"

formatArtistPromoDayHeader :: Day -> Text
formatArtistPromoDayHeader dayVal =
  let (yearVal, monthVal, dayNum) = toGregorian dayVal
      (_, _, weekdayNum) = toWeekDate dayVal
      weekdayLabel = weekdayName weekdayNum
      monthLabel = monthName monthVal
  in T.concat
      [ weekdayLabel
      , " "
      , T.pack (show dayNum)
      , " de "
      , monthLabel
      , " de "
      , T.pack (show yearVal)
      ]

weekdayName :: Int -> Text
weekdayName weekdayNum =
  case weekdayNum of
    1 -> "Lunes"
    2 -> "Martes"
    3 -> "Miércoles"
    4 -> "Jueves"
    5 -> "Viernes"
    6 -> "Sábado"
    7 -> "Domingo"
    _ -> "Día"

monthName :: Int -> Text
monthName monthNum =
  case monthNum of
    1  -> "enero"
    2  -> "febrero"
    3  -> "marzo"
    4  -> "abril"
    5  -> "mayo"
    6  -> "junio"
    7  -> "julio"
    8  -> "agosto"
    9  -> "septiembre"
    10 -> "octubre"
    11 -> "noviembre"
    12 -> "diciembre"
    _  -> "mes"

normalizeCellText :: Text -> Text
normalizeCellText = T.unwords . T.words

latexEscape :: Text -> Text
latexEscape = T.concatMap escapeChar . normalizeCellText
  where
    escapeChar c = case c of
      '&'  -> "\\&"
      '%'  -> "\\%"
      '$'  -> "\\$"
      '#'  -> "\\#"
      '_'  -> "\\_"
      '{'  -> "\\{"
      '}'  -> "\\}"
      '~'  -> "\\textasciitilde{}"
      '^'  -> "\\textasciicircum{}"
      '\\' -> "\\textbackslash{}"
      _    -> T.singleton c

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]
