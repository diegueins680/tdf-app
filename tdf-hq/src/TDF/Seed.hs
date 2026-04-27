{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Seed where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Data.Aeson (FromJSON, Value, decode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (
    NominalDiffTime,
    UTCTime (..),
    addUTCTime,
    fromGregorian,
    getCurrentTime,
    secondsToDiffTime,
 )
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment, lookupEnv)
import qualified TDF.CMS.Models as CMS
import TDF.Config (resolveAppBase)
import TDF.Models
import TDF.ModelsExtra (DropdownOption (..))
import qualified TDF.ModelsExtra as ME
import TDF.Pipelines (canonicalStage, defaultStage)
import qualified TDF.Trials.Models as Trials

-- Seed data from Diego's YAML (normalized)
seedAll :: SqlPersistT IO ()
seedAll = do
    now <- liftIO getCurrentTime
    allowSeededCredentials <- liftIO seededCredentialSeedingAllowedFromEnv

    -- Parties: Artists & Teachers
    let artists =
            [ ("Arkabuz", Nothing)
            , ("El Bloque", Nothing)
            , ("Skanka Fe", Nothing)
            , ("Quimika Soul", Nothing)
            , ("Juano Ledesma", Just "Juan Ledesma")
            ]
    mapM_
        ( \(disp, mlegal) -> do
            _ <- ensurePartyRecord now disp mlegal
            pure ()
        )
        artists

    let teachers =
            [ ("César Galarza", Nothing)
            , ("Fabricio Alomía", Nothing)
            , ("Juan Ledesma", Nothing)
            ]
    teacherPairs <- forM teachers $ \(disp, mlegal) -> do
        pid <- ensurePartyRecord now disp mlegal
        _ <-
            upsert
                (PartyRole pid Teacher True)
                [PartyRoleActive =. True]
        pure (disp, pid)

    -- Service Catalog
    let svcSeeds =
            [ ("Grabación de Banda", Recording, Hourly, Just (25 * 100), Just 1200, "USD", Just "hora")
            , ("Grabación de Voz", Recording, Hourly, Just (35 * 100), Just 1200, "USD", Just "hora")
            , ("Mezcla", Mixing, PerSong, Just (120 * 100), Just 1200, "USD", Just "canción")
            , ("Mastering", Mastering, PerSong, Just (70 * 100), Just 1200, "USD", Just "canción")
            , ("Ensayo", Rehearsal, Hourly, Just (30 * 100), Just 1200, "USD", Just "hora")
            , ("Práctica en DJ Booth", Rehearsal, Hourly, Just (15 * 100), Just 1200, "USD", Just "hora")
            , ("Podcast", EventProduction, PerSong, Just (80 * 100), Just 1200, "USD", Just "episodio")
            , ("Clases", Classes, Package, Nothing, Just 1200, "USD", Just "paquete")
            , ("Producción de eventos", EventProduction, Quote, Nothing, Just 1200, "USD", Nothing)
            ]
    mapM_ ensureServiceCatalog svcSeeds

    -- Pipelines: seed sample cards for Mixing/Mastering
    let pipelineSeeds =
            [ (Mixing, "Arkabuz - Single A", Just "Arkabuz", Just "Brief", 10)
            , (Mixing, "Quimika - EP", Just "Quimika Soul", Just "Prep", 20)
            , (Mastering, "Skanka Fe - LP", Just "Skanka Fe", Just "v1", 10)
            , (Mastering, "El Bloque - Single", Just "El Bloque", Just "Approved", 20)
            ]
        ensurePipelineCard ::
            (ServiceKind, Text, Maybe Text, Maybe Text, Int) ->
            SqlPersistT IO ()
        ensurePipelineCard (kind, titleTxt, artistTxt, stageTxt, sortOrder) = do
            existing <-
                selectFirst
                    [ ME.PipelineCardServiceKind ==. kind
                    , ME.PipelineCardTitle ==. titleTxt
                    ]
                    []
            case existing of
                Just _ -> pure ()
                Nothing -> do
                    let stageValue = maybe (defaultStage kind) id (stageTxt >>= canonicalStage kind)
                    _ <-
                        insert
                            ME.PipelineCard
                                { ME.pipelineCardServiceKind = kind
                                , ME.pipelineCardTitle = titleTxt
                                , ME.pipelineCardArtist = artistTxt
                                , ME.pipelineCardStage = stageValue
                                , ME.pipelineCardSortOrder = sortOrder
                                , ME.pipelineCardNotes = Nothing
                                , ME.pipelineCardCreatedAt = now
                                , ME.pipelineCardUpdatedAt = now
                                }
                    pure ()
    mapM_ ensurePipelineCard pipelineSeeds

    -- Package Product: Guitar 24h
    _ <-
        insertUnique $
            PackageProduct
                { packageProductName = "Guitar 24h"
                , packageProductServiceKind = Classes
                , packageProductUnitsKind = Hours
                , packageProductUnitsQty = 24
                , packageProductPriceCents = 500 * 100
                , packageProductExpiresDays = Just 120
                , packageProductTransferable = False
                , packageProductRefundPolicy = CreditOnly
                , packageProductActive = True
                }

    -- Resources (rooms)
    let rooms = ["Booth A", "Booth B", "Booth C", "Booth D", "Live Room", "Control Room", "Synth Room", "Studio A", "Studio B", "Rehearsal 1", "Classroom"]
    roomPairs <- forM rooms $ \r -> do
        existing <- selectFirst [ResourceName ==. r] []
        case existing of
            Just (Entity rid _) -> pure (r, rid)
            Nothing -> do
                rid <- insert $ Resource r (slugify r) Room Nothing True
                pure (r, rid)

    -- Subjects and room availability preferences
    let subjectSeeds =
            [ ("DJ", True, ["Classroom", "Studio B"])
            , ("Producción Musical", True, ["Studio B", "Control Room"])
            , ("Grabación", True, ["Studio A", "Control Room"])
            ]
    subjectPairs <- forM subjectSeeds $ \(subjectName, isActive, roomNames) -> do
        sid <- ensureSubjectRecord subjectName isActive
        forM_ (zip [1 :: Int ..] roomNames) $ \(priority, roomName) -> do
            case lookup roomName roomPairs of
                Nothing -> pure ()
                Just roomId -> ensureSubjectRoomPref sid roomId priority
        pure (subjectName, sid)

    let teacherSubjectSeeds =
            [ ("César Galarza", "DJ")
            , ("César Galarza", "Grabación")
            , ("Fabricio Alomía", "Producción Musical")
            , ("Juan Ledesma", "DJ")
            ]
    forM_ teacherSubjectSeeds $ \(teacherName, subjectName) ->
        case (lookup teacherName teacherPairs, lookup subjectName subjectPairs) of
            (Just teacherId, Just subjectId) -> ensureTeacherSubjectLink teacherId subjectId
            _ -> pure ()

    -- Publish sample availability windows (45 minutes each)
    let minutes :: Int -> NominalDiffTime
        minutes m = realToFrac (m * 60)
        addMinutes m = addUTCTime (minutes m)
        availabilitySeeds =
            [ ("César Galarza", "DJ", "Classroom", 24 * 60 + 540) -- mañana 09:00
            , ("César Galarza", "DJ", "Classroom", 24 * 60 + 600) -- mañana 10:00
            , ("Fabricio Alomía", "Producción Musical", "Studio B", 36 * 60 + 600)
            , ("Juan Ledesma", "Grabación", "Studio A", 48 * 60 + 480)
            ]
    forM_ availabilitySeeds $ \(teacherName, subjectName, roomName, startMinutes) ->
        case ( lookup teacherName teacherPairs
             , lookup subjectName subjectPairs
             , lookup roomName roomPairs
             ) of
            (Just teacherId, Just subjectId, Just roomId) -> do
                let slotStart = addMinutes startMinutes now
                    slotEnd = addMinutes (startMinutes + 45) now
                ensureTeacherAvailabilitySlot now teacherId subjectId roomId slotStart slotEnd
            _ -> pure ()

    -- Staff accounts + API tokens for authentication examples
    let staffAccounts =
            [ ("TDF Admin", Just "TDF Admin", Admin, "admin-token", "admin", "password123")
            , ("Front Desk Manager", Nothing, Manager, "manager-token", "manager", "password123")
            , ("Reception", Nothing, Reception, "reception-token", "reception", "password123")
            , ("Accounting", Nothing, Accounting, "accounting-token", "accounting", "password123")
            , ("Scheduling", Nothing, Engineer, "scheduling-token", "scheduling", "password123")
            , ("Packages", Nothing, Customer, "packages-token", "packages", "password123")
            ]
    if allowSeededCredentials
        then
            mapM_
                ( \(disp, mlegal, role, token, uname, pwd) -> do
                    _ <- ensureStaff now disp mlegal role token uname pwd
                    pure ()
                )
                staffAccounts
        else
            liftIO $
                putStrLn
                    "Skipping static demo staff credentials/tokens in hosted or production runtime."

    seedCoreStaffRoles allowSeededCredentials now

    -- Dropdown options for admin-managed metadata
    let dropdowns =
            [ ("band-role", "Singer", Nothing, Just 1)
            , ("band-role", "Bassist", Nothing, Just 2)
            , ("band-role", "Guitar Player", Nothing, Just 3)
            , ("band-role", "Drummer", Nothing, Just 4)
            , ("band-genre", "Rock", Nothing, Just 1)
            , ("band-genre", "Pop", Nothing, Just 2)
            , ("band-genre", "Jazz", Nothing, Just 3)
            , ("band-genre", "Metal", Nothing, Just 4)
            , ("band-genre", "Reggae", Nothing, Just 5)
            ]
    mapM_ (ensureDropdownOption now) dropdowns

    seedInventoryAssets
    seedMarketplaceListings
    seedProductionCourse now
    seedRecordsCmsContent now
    seedHolgerSession now
    seedAcademy now

    pure ()

slugify :: Text -> Text
slugify = T.toLower . T.replace " " "-"

seedProductionCourse :: UTCTime -> SqlPersistT IO ()
seedProductionCourse now = do
    slugEnv <- liftIO (lookupEnv "COURSE_DEFAULT_SLUG")
    mapEnv <- liftIO (lookupEnv "COURSE_DEFAULT_MAP_URL")
    whatsappEnv <- liftIO (lookupEnv "COURSE_DEFAULT_WHATSAPP_URL")
    baseEnv <- liftIO (lookupEnv "HQ_APP_URL")
    instructorAvatarEnv <- liftIO (lookupEnv "COURSE_DEFAULT_INSTRUCTOR_AVATAR")
    let nonEmptyText raw =
            let txt = T.strip (T.pack raw)
             in if T.null txt then Nothing else Just txt
        baseUrl = resolveAppBase (fmap T.pack baseEnv)
        slugVal = fromMaybe "produccion-musical-abr-2026" (slugEnv >>= nonEmptyText)
        courseTitle = "Curso de Producción Musical"
        subtitleTxt = Just "Presencial · Cuatro sábados · 16 horas en total · Próximo inicio: sábado 2 de mayo"
        formatTxt = Just "Presencial"
        durationTxt = Just "Cuatro sábados (16 horas en total)"
        priceCentsVal = 15000
        currencyVal = "USD"
        capacityVal = 16
        sessionStart = Just 15
        sessionDuration = Just 4
        locationLabel = Just "TDF Records – Quito"
        locationMap = Just (fromMaybe "https://maps.app.goo.gl/6pVYZ2CsbvQfGhAz6" (mapEnv >>= nonEmptyText))
        whatsappCta = Just (fromMaybe "https://wa.me/593995413168?text=Quiero%20inscribirme%20al%20curso" (whatsappEnv >>= nonEmptyText))
        landingUrl = Just (baseUrl <> "/curso/" <> slugVal)
        instructorName = Just "Esteban Muñoz"
        instructorBio = Just "Productor en TDF Records. 10+ años grabando bandas, rap y electrónica."
        instructorAvatar = Just (fromMaybe (baseUrl <> "/assets/esteban-munoz.jpg") (instructorAvatarEnv >>= nonEmptyText))
        sessions =
            [ ("Sábado 1 · Introducción", fromGregorian 2026 5 2)
            , ("Sábado 2 · Grabación", fromGregorian 2026 5 9)
            , ("Sábado 3 · Mezcla", fromGregorian 2026 5 16)
            , ("Sábado 4 · Masterización", fromGregorian 2026 5 23)
            ]
        syllabus =
            [ ("Introducción a la producción musical", ["Conceptos básicos", "Herramientas esenciales"])
            , ("Grabación y captura de audio", ["Técnicas de grabación", "Configuración de micrófonos"])
            , ("Mezcla y edición", ["Ecualización y compresión", "Balance y panoramización"])
            , ("Masterización y publicación", ["Mastering", "Distribución digital"])
            ]
    mCourse <- getBy (Trials.UniqueCourseSlug slugVal)
    courseId <- case mCourse of
        Nothing ->
            insert
                Trials.Course
                    { Trials.courseSlug = slugVal
                    , Trials.courseTitle = courseTitle
                    , Trials.courseSubtitle = subtitleTxt
                    , Trials.courseFormat = formatTxt
                    , Trials.courseDuration = durationTxt
                    , Trials.coursePriceCents = priceCentsVal
                    , Trials.courseCurrency = currencyVal
                    , Trials.courseCapacity = capacityVal
                    , Trials.courseSessionStartHour = sessionStart
                    , Trials.courseSessionDurationHours = sessionDuration
                    , Trials.courseLocationLabel = locationLabel
                    , Trials.courseLocationMapUrl = locationMap
                    , Trials.courseWhatsappCtaUrl = whatsappCta
                    , Trials.courseLandingUrl = landingUrl
                    , Trials.courseDaws = Nothing
                    , Trials.courseIncludes = Nothing
                    , Trials.courseInstructorName = instructorName
                    , Trials.courseInstructorBio = instructorBio
                    , Trials.courseInstructorAvatarUrl = instructorAvatar
                    , Trials.courseCreatedAt = now
                    , Trials.courseUpdatedAt = now
                    }
        Just (Entity cid existing) -> do
            replace
                cid
                existing
                    { Trials.courseTitle = courseTitle
                    , Trials.courseSubtitle = subtitleTxt
                    , Trials.courseFormat = formatTxt
                    , Trials.courseDuration = durationTxt
                    , Trials.coursePriceCents = priceCentsVal
                    , Trials.courseCurrency = currencyVal
                    , Trials.courseCapacity = capacityVal
                    , Trials.courseSessionStartHour = sessionStart
                    , Trials.courseSessionDurationHours = sessionDuration
                    , Trials.courseLocationLabel = locationLabel
                    , Trials.courseLocationMapUrl = locationMap
                    , Trials.courseWhatsappCtaUrl = whatsappCta
                    , Trials.courseLandingUrl = landingUrl
                    , Trials.courseDaws = Nothing
                    , Trials.courseIncludes = Nothing
                    , Trials.courseInstructorName = instructorName
                    , Trials.courseInstructorBio = instructorBio
                    , Trials.courseInstructorAvatarUrl = instructorAvatar
                    , Trials.courseCreatedAt = Trials.courseCreatedAt existing
                    , Trials.courseUpdatedAt = now
                    }
            pure cid

    -- Fuerza arrays como text[] para evitar literales heredados en formato JSON
    rawExecute
        "UPDATE course SET daws = ARRAY['Logic','Luna']::text[], includes = ARRAY['Acceso a grabaciones','Certificado de participación','Mentorías','Grupo de WhatsApp','Acceso a la plataforma de TDF Records']::text[] WHERE slug = ?"
        [PersistText slugVal]

    deleteWhere [Trials.CourseSessionModelCourseId ==. courseId]
    forM_ (zip [1 :: Int ..] sessions) $ \(idx, (labelTxt, dayVal)) ->
        insert_
            Trials.CourseSessionModel
                { Trials.courseSessionModelCourseId = courseId
                , Trials.courseSessionModelLabel = labelTxt
                , Trials.courseSessionModelDate = dayVal
                , Trials.courseSessionModelOrder = Just idx
                }

    deleteWhere [Trials.CourseSyllabusItemCourseId ==. courseId]
    forM_ (zip [1 :: Int ..] syllabus) $ \(idx, (titleTxt, topics)) ->
        insert_
            Trials.CourseSyllabusItem
                { Trials.courseSyllabusItemCourseId = courseId
                , Trials.courseSyllabusItemTitle = titleTxt
                , Trials.courseSyllabusItemTopics = topics
                , Trials.courseSyllabusItemOrder = Just idx
                }

data CmsSeed = CmsSeed
    { cmsSeedSlug :: Text
    , cmsSeedLocale :: Text
    , cmsSeedTitle :: Text
    , cmsSeedPayload :: Value
    }

seedRecordsCmsContent :: UTCTime -> SqlPersistT IO ()
seedRecordsCmsContent now =
    mapM_
        (ensurePublishedCmsSeed now)
        [ CmsSeed
            { cmsSeedSlug = "records-releases"
            , cmsSeedLocale = "es"
            , cmsSeedTitle = "RELEASES by TDF"
            , cmsSeedPayload =
                object
                    [ "playlistName" .= ("RELEASES by TDF" :: Text)
                    , "seedVersion" .= (3 :: Int)
                    , "playlistUrl" .= recordsReleasesPlaylistUrl
                    , "cover" .= recordsReleasesCover
                    , "playlistCover" .= recordsReleasesCover
                    , "tracks" .= recordsReleaseTracks
                    ]
            }
        , CmsSeed
            { cmsSeedSlug = "records-recordings"
            , cmsSeedLocale = "es"
            , cmsSeedTitle = "Videos recientes TDF Records"
            , cmsSeedPayload =
                object
                    [ "channelName" .= ("TDF Records" :: Text)
                    , "channelUrl" .= ("https://www.youtube.com/@tdf.records" :: Text)
                    , "seedVersion" .= (1 :: Int)
                    , "videos" .= recordsRecordingVideos
                    ]
            }
        , CmsSeed
            { cmsSeedSlug = "records-sessions"
            , cmsSeedLocale = "es"
            , cmsSeedTitle = "TDF Live Sessions"
            , cmsSeedPayload =
                object
                    [ "playlistUrl" .= ("https://www.youtube.com/watch?v=9387ent0ELc&list=PLORPSiW9rnkjSYKaBSAX-QqoVf_9b29EP" :: Text)
                    , "videos"
                        .= [ object
                                [ "title" .= ("Holger Quiñonez - TDF Live Sessions E05" :: Text)
                                , "guests" .= ("Holger Quiñonez" :: Text)
                                , "youtubeId" .= ("9387ent0ELc" :: Text)
                                , "url" .= ("https://www.youtube.com/watch?v=9387ent0ELc&list=PLORPSiW9rnkjSYKaBSAX-QqoVf_9b29EP" :: Text)
                                , "duration" .= ("05:56" :: Text)
                                , "description" .= ("Sesión en vivo de Holger Quiñonez para TDF Live Sessions." :: Text)
                                , "sortOrder" .= (1 :: Int)
                                ]
                           , object
                                [ "title" .= ("Categal - TDF Live Sessions E04" :: Text)
                                , "guests" .= ("Categal" :: Text)
                                , "youtubeId" .= ("5SpnEELSNqw" :: Text)
                                , "url" .= ("https://www.youtube.com/watch?v=5SpnEELSNqw&list=PLORPSiW9rnkjSYKaBSAX-QqoVf_9b29EP" :: Text)
                                , "duration" .= ("18:47" :: Text)
                                , "description" .= ("Sesión en vivo de Categal para TDF Live Sessions." :: Text)
                                , "sortOrder" .= (2 :: Int)
                                ]
                           , object
                                [ "title" .= ("Los Morrison - TDF Live Sessions E03" :: Text)
                                , "guests" .= ("Los Morrison" :: Text)
                                , "youtubeId" .= ("97PnHRn8IGs" :: Text)
                                , "url" .= ("https://www.youtube.com/watch?v=97PnHRn8IGs&list=PLORPSiW9rnkjSYKaBSAX-QqoVf_9b29EP" :: Text)
                                , "duration" .= ("33:19" :: Text)
                                , "description" .= ("Sesión en vivo de Los Morrison para TDF Live Sessions." :: Text)
                                , "sortOrder" .= (3 :: Int)
                                ]
                           , object
                                [ "title" .= ("Barrelshots - TDF Live Sessions E02" :: Text)
                                , "guests" .= ("Barrelshots" :: Text)
                                , "youtubeId" .= ("e24-id_Ix8s" :: Text)
                                , "url" .= ("https://www.youtube.com/watch?v=e24-id_Ix8s&list=PLORPSiW9rnkjSYKaBSAX-QqoVf_9b29EP" :: Text)
                                , "duration" .= ("11:36" :: Text)
                                , "description" .= ("Sesión en vivo de Barrelshots para TDF Live Sessions." :: Text)
                                , "sortOrder" .= (4 :: Int)
                                ]
                           , object
                                [ "title" .= ("Machaka - TDF Live Sessions E01" :: Text)
                                , "guests" .= ("Machaka" :: Text)
                                , "youtubeId" .= ("z7RpdrL4P4A" :: Text)
                                , "url" .= ("https://www.youtube.com/watch?v=z7RpdrL4P4A&list=PLORPSiW9rnkjSYKaBSAX-QqoVf_9b29EP" :: Text)
                                , "duration" .= ("14:58" :: Text)
                                , "description" .= ("Sesión en vivo de Machaka para TDF Live Sessions." :: Text)
                                , "sortOrder" .= (5 :: Int)
                                ]
                           ]
                    ]
            }
        ]
  where
    recordsReleasesPlaylistUrl :: Text
    recordsReleasesPlaylistUrl = "https://open.spotify.com/playlist/4FSMAk7z9GFk4pUH9Uffbt"

    recordsReleasesCover :: Text
    recordsReleasesCover = "https://image-cdn-ak.spotifycdn.com/image/ab67706c0000da844452c00a761b4307854c4c9a"

    recordsRecordingVideos :: [Value]
    recordsRecordingVideos =
        [ recordingVideo 1 "Federico Molinari @ TDF Electro Sessions" "Federico Molinari" "f2BabxM1Pjc" "44:14" "TDF Electro Sessions" "DJ set publicado en el canal TDF Records."
        , recordingVideo 2 "Just One Nite @ TDF Electro Sessions" "Just One Nite" "rRkAeNB0R14" "56:21" "TDF Electro Sessions" "DJ set publicado en el canal TDF Records."
        , recordingVideo 3 "Morex DJ Set @ TDF Electro Sessions" "Morex" "wZQAlIqllQY" "1:02:21" "TDF Electro Sessions" "DJ set publicado en el canal TDF Records."
        , recordingVideo 4 "Diego Saá @ TDF Electro Sessions" "Diego Saá" "YDODXZ4lyRk" "48:09" "TDF Electro Sessions" "Live set publicado en el canal TDF Records."
        , recordingVideo 5 "Everaldo Vasco @ TDF Sessions" "Everaldo Vasco" "1hKWOram3aw" "1:26:18" "TDF Sessions" "Sesión publicada en el canal TDF Records."
        , recordingVideo 6 "COHEMA @ TDF Sessions" "COHEMA" "xqeey8SrH8M" "1:00:00" "TDF Sessions" "Sesión publicada en el canal TDF Records."
        ]

    recordingVideo :: Int -> Text -> Text -> Text -> Text -> Text -> Text -> Value
    recordingVideo sortOrder title artist youtubeId duration vibe description =
        object
            [ "title" .= title
            , "artist" .= artist
            , "youtubeId" .= youtubeId
            , "url" .= ("https://www.youtube.com/watch?v=" <> youtubeId)
            , "duration" .= duration
            , "vibe" .= vibe
            , "description" .= description
            , "sortOrder" .= sortOrder
            ]

    recordsReleaseTracks :: [Value]
    recordsReleaseTracks =
        [ spotifyTrack 1 "You Dont Know" "Rizlo" "30952jHviNHyvoK3eEorNL" 147840 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02d2530aefcbc69e94cc72c38f"
        , spotifyTrack 2 "MYLOBA" "JOSSEFINA, Machaka" "0i0iq0pUAh121udqZqPBwV" 226667 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02a1686e00c0c4e8e72f95724e"
        , spotifyTrack 3 "Chapa Urbano" "Quimika Soul, Skankafe" "7DItzZJ9WybzqIogJlFEEZ" 234666 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02c81a8164519b116b00f90ed3"
        , spotifyTrack 4 "Akuba" "Skankafe, Ali G T Kadjal" "5hIOAt1SfyBdg6FEyH76fQ" 240487 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e022ec7501d7fee169b7a0eec0d"
        , spotifyTrack 5 "Shadowboxing" "HNO" "2wyXTmJfLY9wQW6epTbl9c" 174545 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02829d5f2b1294e2e4f3889233"
        , spotifyTrack 6 "Parverso" "HNO" "13enVmpdDijFCOhhQ1dDj7" 263169 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02271b47e2551aebbb99118d99"
        , spotifyTrack 7 "DMT" "Skankafe" "1CZYNHMlrxZg1Cxp8mlYDJ" 245500 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02b3cce40fae5ff753d292ba33"
        , spotifyTrack 8 "Dubkuba - Akuba Dub" "Skankafe" "7xay41OfiKO1H5zuRbSVYe" 226000 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e023ca3c2f8ad4a04fe80c1dda9"
        , spotifyTrack 9 "Soltar" "Quimika Soul, Paula García" "0vAzXcWC5Ydovb922U3VI2" 194000 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02c2f603d48669a546c0da8e78"
        , spotifyTrack 10 "Escamas de Plasma" "Llama Este Pez" "70BHut1QVhq82Cd4BJRUjv" 191055 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02733bfce6a96465a3dbd5e89a"
        , spotifyTrack 11 "Danza de Llamas Electricas" "Llama Este Pez" "4Aeu8EgNebXMCgItaQgmRX" 924482 "https://image-cdn-fa.spotifycdn.com/image/ab67616d00001e02733bfce6a96465a3dbd5e89a"
        , spotifyTrack 12 "Algas y Cables" "Llama Este Pez" "49hsoYIJE92C8FMTWBxqBA" 160098 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02733bfce6a96465a3dbd5e89a"
        , spotifyTrack 13 "Pecera de Neon" "Llama Este Pez" "0klbrDUIJq0UxdNcKcTMHk" 706683 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02733bfce6a96465a3dbd5e89a"
        , spotifyTrack 14 "Blanks on Whichever" "Ruz" "0uUtPZpEC9aE0gARu3UmuY" 196527 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 15 "Full Up (Never Come Back Down)" "Ruz" "3gBXFvuNyS8gwxXcNUHfqL" 198750 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 16 "Todo Es El Río" "Ruz" "6mKnzQuxfyjifArXE80fAr" 310000 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 17 "Chicha" "Ruz" "7pUwtokDaVRUkohzkSJz2v" 215625 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 18 "I'm Not Ready to Go" "Ruz" "41RjtcNASM7Wd75fpO1EQs" 131291 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 19 "Gestowap" "Ruz" "71jkA9GpqhhrwloBpxnmoa" 202492 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 20 "The Stars Don’t Have To Align" "Ruz" "3lsn6EW5WDWkc6EBvZK6qn" 199137 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 21 "Wanabelem" "Ruz" "2EnRhHlIIoGhMyq9CygQiH" 70603 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 22 "Tribal Anthem" "Ruz" "73RYZ5msy79FhATkjo5FdJ" 165000 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 23 "When You Know Me You'll Be Calling Out" "Ruz" "61WXfNarokjluJHLMsKmuH" 195498 "https://image-cdn-fa.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 24 "Truer Than Another One" "Ruz" "1goSpdddNCO7uAFUyTFEdP" 184794 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 25 "I'm Here" "Ruz" "6bqQ9mYHHm3GdvqiUEDX5e" 205406 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 26 "I'm a Lover" "Ruz" "52qhoxlfUqLvygUT7ZqwhU" 173857 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 27 "Calculations Don't Make You What You Are" "Ruz" "1muWzwdQ7NvKLm15rlgfrg" 189375 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 28 "Give Me All of You" "Ruz" "3MyYgs8Wgx7UJj6dHYva3n" 151107 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02aab9fe081c840fc4816105a1"
        , spotifyTrack 29 "Luz Al Sur" "La Gente Naranja" "115GBVzpqUijnLYz6TNa7j" 194426 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02440f425cec2171c66f367f89"
        , spotifyTrack 30 "Misil" "La Gente Naranja" "7qmR64PrOEZr66JVXtY3Jm" 186773 "https://image-cdn-fa.spotifycdn.com/image/ab67616d00001e02440f425cec2171c66f367f89"
        , spotifyTrack 31 "Rosa en Mi Jardin" "La Gente Naranja" "5fBaFpC0CjZbRb1Bcb7bzD" 266826 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02440f425cec2171c66f367f89"
        , spotifyTrack 32 "Elevador" "La Gente Naranja" "2yX7F94Gh0Q1UCwNVb3Sum" 205600 "https://image-cdn-fa.spotifycdn.com/image/ab67616d00001e02440f425cec2171c66f367f89"
        , spotifyTrack 33 "Visión Platónica" "La Gente Naranja" "6SjUhXo9BoCojLdnT5KCCD" 265800 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02440f425cec2171c66f367f89"
        , spotifyTrack 34 "Un Día Más Sin Tí" "La Gente Naranja" "4wvjeDmhuOA2sH2EucJ4iG" 218280 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02440f425cec2171c66f367f89"
        , spotifyTrack 35 "Lo Inexplicable" "La Gente Naranja" "3nkTKhi7tz4qF0ZZcLLoUu" 244773 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02440f425cec2171c66f367f89"
        , spotifyTrack 36 "Diamante" "La Gente Naranja" "717FCgZKEyVzbqCuzUYeRA" 276466 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02440f425cec2171c66f367f89"
        , spotifyTrack 37 "Botellas Vacías" "La Gente Naranja" "0k6kiJWRC1ogXQVuDTsIH6" 223106 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02440f425cec2171c66f367f89"
        , spotifyTrack 38 "Ghetto Booty" "La Gente Naranja" "4Tw7ZLWcewEODuVl4mPIPd" 246493 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02440f425cec2171c66f367f89"
        , spotifyTrack 39 "Zapping" "Skankafe" "1nFJyBAJegQyyCpjYFzr6h" 25704 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 40 "La TV" "Skankafe, Xavier Muller Teclados" "6HWZ0r7LhZf6eEDOL9xdU3" 219083 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 41 "Hit Del Diario" "Skankafe" "3dacpvWHs6vIp1E6KZ5RTO" 147779 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 42 "Grillete Con Esposas" "Skankafe" "3WISYfVvFerhMatowZa4fk" 163196 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 43 "Pose" "Skankafe" "0L709u9MSdCTWlOCrvG4Ae" 202219 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 44 "Sancho" "Skankafe" "0k7nhafdMX83t6LP2OYH0Z" 283436 "https://image-cdn-fa.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 45 "Chancho en Feria" "Skankafe" "5Txabd0AqbBGEYMeV8E9ao" 143002 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 46 "Muerte en Un 222 X 3" "Skankafe" "6ylSkDDXNqPe6XVx11lsK3" 131618 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 47 "Tú No Man" "Skankafe" "3pjfoaqvUHVlDW6aLXl17H" 168891 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 48 "Lo Monótono" "Skankafe, Holger Quiñonez" "2TOJMwgPGJFTImmpDdEj67" 160495 "https://image-cdn-fa.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 49 "Vicente" "Skankafe, Xavier Muller Teclados" "1TtCSfhObueW7Bu8SVYzaS" 166384 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02cbc5759516aa0250d63ee3ac"
        , spotifyTrack 50 "Pirotécnicos Juegos )" "HNO, Lil Weed (Per-versos)" "7B0YEcOuANKOamQtNFJ4UP" 195813 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02ccfec7751f8d86f0e28fe7e3"
        , spotifyTrack 51 "NEW BEGINNING" "Juano Ledesma, Diego Saa" "51ehdjbxjK8fx97NfbZmQl" 365009 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02658eea38098075ce0e740049"
        , spotifyTrack 52 "FARAONES" "Double OG" "5kR3SFiBF5a0XwaDAtIUHS" 295813 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e0200785ba832fbc9bf34d891d4"
        , spotifyTrack 53 "Rain (HNO & Kapuly)" "HNO" "1hSQg3MkSbrMOfKkHEpwuK" 204405 "https://image-cdn-fa.spotifycdn.com/image/ab67616d00001e02b5f148c8aa40f79b54681401"
        , spotifyTrack 54 "A veces" "Skankafe" "7HXrVPZphrFL4TaDWNWTIH" 215665 "https://image-cdn-fa.spotifycdn.com/image/ab67616d00001e0277e6cce24f8c46c2bd994dea"
        , spotifyTrack 55 "Now" "Juano Ledesma" "5PJlf7UEb0fTtVtjESBzus" 468829 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e0243c844df8b9fa67ca9f314de"
        , spotifyTrack 56 "Yo, Vos y Dios" "Skankafe" "0Fcx8EhZbitStrQUnmMLzt" 329379 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e0233452cba4ede725a9ee8c8da"
        , spotifyTrack 57 "Cerdicornio" "Juano Ledesma, Diego Saa, Alejandro Soria, Fabro" "4irFDRIK80lGNpOXWtfVet" 354098 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02f9def41db12d8eccfdcb6d9a"
        , spotifyTrack 58 "FARAONES" "Double OG" "2zOhoAbgL62OUa6U4gAMeT" 200930 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e024e218942ea21f82c2a6c6de8"
        , spotifyTrack 59 "Kamikaze" "OSPINA" "5cwv3eclu8Z7WevferI2KJ" 157714 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02805bd6cfce0f8933c1865084"
        , spotifyTrack 60 "Lune Vocale" "Diego Saa, Juano Ledesma" "2wL60aCIrgsf3GmEHvh9vJ" 503999 "https://image-cdn-fa.spotifycdn.com/image/ab67616d00001e0246503952d9ca883fe06b1f21"
        , spotifyTrack 61 "Shinobi" "FABRO, Juano Ledesma" "0veaB6o3uvC0dibDfFnFVS" 435692 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02076a522203cbf720a14939f5"
        , spotifyTrack 62 "Moment" "MELANIA" "1IYBR9v75SuErg5Yr3l8i1" 332083 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02c781a951f77c2b7c529f0f1a"
        , spotifyTrack 63 "Tripfásico" "Diego Saa" "52vhHwMHBR6tMTcTjt9N4k" 286500 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02705de9807e5ea5305e62b1e9"
        , spotifyTrack 64 "No Van a Parar" "R de Nexo" "7gbSbZM1XG3KHUOdDHutgm" 184000 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e0211d4efcd9f804118c05b053a"
        , spotifyTrack 65 "King Mota" "El Bloque" "2R2vAnOubJWDuWLBdogIkj" 248000 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e02e72e2e98a63a18507cbe5d5c"
        , spotifyTrack 66 "Esencia Eterna" "Arkabuz" "6862eD7dxnm3AAH8jj0CrZ" 218325 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e024b66b791cde520a4dcb60bf4"
        , spotifyTrack 67 "El Sobreviviente" "Lucas Napolitano" "2zXjR6tcBpyCkYyL3iIVbM" 245624 "https://image-cdn-ak.spotifycdn.com/image/ab67616d00001e026fb7b42c808fb536e541e7e5"
        ]

    spotifyTrack :: Int -> Text -> Text -> Text -> Int -> Text -> Value
    spotifyTrack sortOrder title artist trackId durationMs coverUrl =
        let spotifyUrl = "https://open.spotify.com/track/" <> trackId
         in object
                [ "title" .= title
                , "artist" .= artist
                , "durationMs" .= durationMs
                , "duration" .= formatSpotifyDuration durationMs
                , "spotifyUrl" .= spotifyUrl
                , "url" .= spotifyUrl
                , "cover" .= coverUrl
                , "sortOrder" .= sortOrder
                , "links"
                    .= catMaybes
                        [ Just $
                            object
                                [ "platform" .= ("Spotify" :: Text)
                                , "url" .= spotifyUrl
                                , "accent" .= ("#1db954" :: Text)
                                ]
                        , fmap youtubeLink (trackYoutubeId sortOrder)
                        ]
                ]

    trackYoutubeId :: Int -> Maybe Text
    trackYoutubeId sortOrder =
        Map.lookup
            sortOrder
            ( Map.fromList
                [ (2, "MfWzJm-kIV0")
                , (3, "_mZt3FN_ZLo")
                , (4, "7j4zau8JVcU")
                , (5, "LtNll90DdRU")
                , (6, "M5We2bMW4Ck")
                , (7, "v3rW0cybrCA")
                , (8, "kWhKDv8O2qQ")
                ]
            )

    youtubeLink :: Text -> Value
    youtubeLink videoId =
        object
            [ "platform" .= ("YouTube" :: Text)
            , "url" .= ("https://www.youtube.com/watch?v=" <> videoId)
            , "accent" .= ("#ff0033" :: Text)
            ]

    formatSpotifyDuration :: Int -> Text
    formatSpotifyDuration durationMs =
        let totalSeconds = max 0 (durationMs `div` 1000)
            minutes = totalSeconds `div` 60
            seconds = totalSeconds `mod` 60
            pad n = if n < 10 then "0" <> show n else show n
         in T.pack (show minutes <> ":" <> pad seconds)

ensurePublishedCmsSeed :: UTCTime -> CmsSeed -> SqlPersistT IO ()
ensurePublishedCmsSeed now CmsSeed{..} = do
    mExisting <-
        selectFirst
            [ CMS.CmsContentSlug ==. cmsSeedSlug
            , CMS.CmsContentLocale ==. cmsSeedLocale
            ]
            []
    case mExisting of
        Just (Entity cid existing) ->
            when (shouldRefreshCmsSeed existing) $
                update
                    cid
                    [ CMS.CmsContentStatus =. "published"
                    , CMS.CmsContentTitle =. Just cmsSeedTitle
                    , CMS.CmsContentPayload =. Just (CMS.AesonValue cmsSeedPayload)
                    , CMS.CmsContentUpdatedAt =. now
                    , CMS.CmsContentPublishedAt =. Just now
                    ]
        Nothing ->
            insert_
                CMS.CmsContent
                    { CMS.cmsContentSlug = cmsSeedSlug
                    , CMS.cmsContentLocale = cmsSeedLocale
                    , CMS.cmsContentVersion = 1
                    , CMS.cmsContentStatus = "published"
                    , CMS.cmsContentTitle = Just cmsSeedTitle
                    , CMS.cmsContentPayload = Just (CMS.AesonValue cmsSeedPayload)
                    , CMS.cmsContentCreatedBy = Nothing
                    , CMS.cmsContentCreatedAt = now
                    , CMS.cmsContentUpdatedAt = now
                    , CMS.cmsContentPublishedAt = Just now
                    }
  where
    shouldRefreshCmsSeed existing =
        case cmsSeedPayloadVersion cmsSeedPayload of
            Nothing -> False
            Just nextVersion ->
                cmsSeedPayloadVersion
                    (maybe (object []) CMS.unAesonValue (CMS.cmsContentPayload existing))
                    /= Just nextVersion

cmsSeedPayloadVersion :: Value -> Maybe Int
cmsSeedPayloadVersion =
    parseMaybe (withObject "CmsSeedPayload" (.: "seedVersion"))

seedAcademy :: UTCTime -> SqlPersistT IO ()
seedAcademy now = do
    microId <- ensureMicrocourse "release-readiness" "Release Readiness" (Just summary)
    mapM_
        (ensureLesson microId)
        [ (1, "Bienvenida", "Cómo preparamos tu lanzamiento en TDF")
        , (2, "Producción", "Checklist de mezcla y mastering antes de publicar")
        , (3, "Distribución", "Configuración de perfiles y agregadores")
        , (4, "Marketing", "Campañas, anuncios y redes sociales para tu release")
        , (5, "Release Day", "Plan de lanzamiento y métricas a monitorear")
        ]
    mapM_ ensureReferralCodeRow ["RR-ALFA", "RR-BETA"]
    let inDays :: Integer -> UTCTime -> UTCTime
        inDays d = addUTCTime (realToFrac (d * 86400))
    ensureCohortRow "rr-sprint" "Release Readiness Sprint" (inDays 14 now) (inDays 28 now) 40
    ensureCohortRow "rr-q4" "Academy Q4" (inDays 60 now) (inDays 90 now) 50
  where
    summary = "Programa intensivo de 5 días para preparar lanzamientos profesionales"

ensureMicrocourse :: Text -> Text -> Maybe Text -> SqlPersistT IO AcademyMicrocourseId
ensureMicrocourse slug titleTxt summaryTxt = do
    mExisting <- getBy (UniqueAcademyMicrocourseSlug slug)
    case mExisting of
        Just (Entity mid _) -> do
            update
                mid
                [ AcademyMicrocourseTitle =. titleTxt
                , AcademyMicrocourseSummary =. summaryTxt
                ]
            pure mid
        Nothing -> do
            now <- liftIO getCurrentTime
            mid <-
                insert
                    AcademyMicrocourse
                        { academyMicrocourseSlug = slug
                        , academyMicrocourseTitle = titleTxt
                        , academyMicrocourseSummary = summaryTxt
                        , academyMicrocourseCreatedAt = now
                        }
            pure mid

ensureLesson :: AcademyMicrocourseId -> (Int, Text, Text) -> SqlPersistT IO ()
ensureLesson microId (dayNum, titleTxt, bodyTxt) = do
    void $
        upsert
            AcademyLesson
                { academyLessonMicrocourseId = microId
                , academyLessonDay = dayNum
                , academyLessonTitle = titleTxt
                , academyLessonBody = bodyTxt
                }
            [ AcademyLessonTitle =. titleTxt
            , AcademyLessonBody =. bodyTxt
            ]

ensureReferralCodeRow :: Text -> SqlPersistT IO ()
ensureReferralCodeRow codeTxt = do
    let normalized = T.toUpper codeTxt
        key = ReferralCodeKey normalized
    existing <- get key
    case existing of
        Just _ -> pure ()
        Nothing -> do
            now <- liftIO getCurrentTime
            insertKey
                key
                ReferralCode
                    { referralCodeOwnerUserId = Nothing
                    , referralCodeCreatedAt = now
                    }

ensureCohortRow :: Text -> Text -> UTCTime -> UTCTime -> Int -> SqlPersistT IO ()
ensureCohortRow slug titleTxt starts ends seats = do
    mExisting <- getBy (UniqueCohortSlug slug)
    case mExisting of
        Just (Entity cid _) ->
            update
                cid
                [ CohortTitle =. titleTxt
                , CohortStartsAt =. starts
                , CohortEndsAt =. ends
                , CohortSeatCap =. seats
                ]
        Nothing -> do
            void $
                insert
                    Cohort
                        { cohortSlug = slug
                        , cohortTitle = titleTxt
                        , cohortStartsAt = starts
                        , cohortEndsAt = ends
                        , cohortSeatCap = seats
                        }

-- Core staff & RBAC seeds -------------------------------------------------

data StaffSeed = StaffSeed
    { ssName :: Text
    , ssEmail :: Text
    , ssRoles :: [RoleEnum]
    }

coreStaffDefaultPassword :: Text
coreStaffDefaultPassword = "changeme123"

coreStaffSeeds :: [StaffSeed]
coreStaffSeeds =
    [ StaffSeed "Esteban Muñoz" "mixandlivesound@gmail.com" [Engineer, Teacher, StudioManager]
    , StaffSeed "Diego Saa" "diego@tdfrecords.net" [Engineer]
    , StaffSeed "Emanuele Pilo-Pais" "interfacerandom@gmail.com" [AandR]
    , StaffSeed "Claudia Palma" "unaclaudiapalma@gmail.com" [LiveSessionsProducer]
    , StaffSeed "Fabricio Alomía" "fabro.sounds@gmail.com" [Teacher]
    , StaffSeed "Juan Ledesma" "juan.ledesma@tdfrecords.com" [Teacher, Producer, Artist]
    ]

data PartyChange
    = PartyCreated
    | PartyUpdated
    | PartyUnchanged

data CredentialStatus
    = CredentialCreated
    | CredentialUpdated
    | CredentialUnchanged
    | CredentialSkipped

seededCredentialSeedingAllowedFromEnv :: IO Bool
seededCredentialSeedingAllowedFromEnv = seededCredentialSeedingAllowed <$> getEnvironment

seededCredentialSeedingAllowed :: [(String, String)] -> Bool
seededCredentialSeedingAllowed env =
    not (hasHostedRuntimeEnv || hasProductionEnv)
  where
    hasHostedRuntimeEnv =
        any
            hasNonEmptyEnv
            [ "FLY_APP_NAME"
            , "KOYEB_APP_NAME"
            , "RENDER"
            , "RAILWAY_ENVIRONMENT"
            , "HEROKU_APP_NAME"
            , "VERCEL"
            , "CF_PAGES"
            , "K_SERVICE"
            ]
    hasProductionEnv =
        any
            (maybe False isProductionValue . lookupNonEmptyEnv)
            [ "APP_ENV"
            , "ENVIRONMENT"
            , "NODE_ENV"
            , "RUNTIME_ENV"
            ]
    hasNonEmptyEnv key = maybe False (const True) (lookupNonEmptyEnv key)
    lookupNonEmptyEnv key =
        case lookup key env of
            Just raw | not (T.null (T.strip (T.pack raw))) -> Just raw
            _ -> Nothing
    isProductionValue raw =
        T.toLower (T.strip (T.pack raw)) `elem` ["prod", "production", "live"]

seedCoreStaffRoles :: Bool -> UTCTime -> SqlPersistT IO ()
seedCoreStaffRoles allowCredentials now = do
    liftIO $ putStrLn "Seeding core staff roles..."
    mapM_ (seedStaff allowCredentials now) coreStaffSeeds

seedStaff :: Bool -> UTCTime -> StaffSeed -> SqlPersistT IO ()
seedStaff allowCredentials now StaffSeed{ssName = nameVal, ssEmail = emailVal, ssRoles = rolesVal} = do
    let normalizedEmail = normalizeEmail emailVal
        cleanName = T.strip nameVal
    (pid, partyChange) <- ensureStaffParty now cleanName normalizedEmail
    newRoles <- ensureStaffRoles pid rolesVal
    credStatus <-
        if allowCredentials
            then ensureStaffCredential pid normalizedEmail
            else pure CredentialSkipped
    logStaffSeed cleanName rolesVal partyChange newRoles credStatus

ensureStaffParty :: UTCTime -> Text -> Text -> SqlPersistT IO (Key Party, PartyChange)
ensureStaffParty now displayName email = do
    mPartyFromCredential <- lookupPartyByCredential email
    mPartyByEmail <- maybe (lookupPartyByEmail email) (const (pure Nothing)) mPartyFromCredential
    mPartyByName <- case mPartyFromCredential <|> mPartyByEmail of
        Just _ -> pure Nothing
        Nothing -> lookupPartyByName displayName
    let chosen = mPartyFromCredential <|> mPartyByEmail <|> mPartyByName
    case chosen of
        Just (Entity pid party) -> do
            let updates =
                    catMaybes
                        [ if partyDisplayName party == displayName
                            then Nothing
                            else Just (PartyDisplayName =. displayName)
                        , case partyPrimaryEmail party of
                            Nothing -> Just (PartyPrimaryEmail =. Just email)
                            Just existing
                                | T.toLower existing == email -> Nothing
                                | otherwise -> Just (PartyPrimaryEmail =. Just email)
                        ]
            unless (null updates) $ update pid updates
            pure (pid, if null updates then PartyUnchanged else PartyUpdated)
        Nothing -> do
            pid <-
                insert $
                    Party
                        { partyLegalName = Nothing
                        , partyDisplayName = displayName
                        , partyIsOrg = False
                        , partyTaxId = Nothing
                        , partyPrimaryEmail = Just email
                        , partyPrimaryPhone = Nothing
                        , partyWhatsapp = Nothing
                        , partyInstagram = Nothing
                        , partyEmergencyContact = Nothing
                        , partyNotes = Nothing
                        , partyCreatedAt = now
                        }
            pure (pid, PartyCreated)
  where
    lookupPartyByCredential :: Text -> SqlPersistT IO (Maybe (Entity Party))
    lookupPartyByCredential username = do
        mCred <- getBy (UniqueCredentialUsername username)
        case mCred of
            Nothing -> pure Nothing
            Just (Entity _ cred) -> do
                mParty <- get (userCredentialPartyId cred)
                case mParty of
                    Nothing -> pure Nothing
                    Just party -> pure (Just (Entity (userCredentialPartyId cred) party))

    lookupPartyByEmail :: Text -> SqlPersistT IO (Maybe (Entity Party))
    lookupPartyByEmail emailTxt = selectFirst [PartyPrimaryEmail ==. Just emailTxt] []

    lookupPartyByName :: Text -> SqlPersistT IO (Maybe (Entity Party))
    lookupPartyByName nameTxt = selectFirst [PartyDisplayName ==. nameTxt] []

ensureStaffRoles :: PartyId -> [RoleEnum] -> SqlPersistT IO [RoleEnum]
ensureStaffRoles pid rolesList = do
    added <- mapM (ensureRole pid) rolesList
    pure (catMaybes added)
  where
    ensureRole partyId role = do
        mExisting <- getBy (UniquePartyRole partyId role)
        case mExisting of
            Nothing -> do
                _ <- insert (PartyRole partyId role True)
                pure (Just role)
            Just (Entity roleId pr)
                | partyRoleActive pr -> pure Nothing
                | otherwise -> do
                    update roleId [PartyRoleActive =. True]
                    pure (Just role)

ensureStaffCredential :: PartyId -> Text -> SqlPersistT IO CredentialStatus
ensureStaffCredential pid username = do
    mCred <- getBy (UniqueCredentialUsername username)
    case mCred of
        Nothing -> do
            hashed <- liftIO (hashPasswordText coreStaffDefaultPassword)
            void $
                insert
                    UserCredential
                        { userCredentialPartyId = pid
                        , userCredentialUsername = username
                        , userCredentialPasswordHash = hashed
                        , userCredentialActive = True
                        }
            pure CredentialCreated
        Just (Entity credId cred) -> do
            let updates =
                    catMaybes
                        [ if userCredentialPartyId cred == pid
                            then Nothing
                            else Just (UserCredentialPartyId =. pid)
                        , if userCredentialActive cred
                            then Nothing
                            else Just (UserCredentialActive =. True)
                        ]
            unless (null updates) $ update credId updates
            pure $ if null updates then CredentialUnchanged else CredentialUpdated

logStaffSeed ::
    Text ->
    [RoleEnum] ->
    PartyChange ->
    [RoleEnum] ->
    CredentialStatus ->
    SqlPersistT IO ()
logStaffSeed nameTxt rolesList partyChange newRoles credStatus = do
    let roleListTxt = T.intercalate ", " (map roleSlug rolesList)
        changes =
            catMaybes
                [ case partyChange of
                    PartyCreated -> Just "created profile"
                    PartyUpdated -> Just "updated profile"
                    PartyUnchanged -> Nothing
                , case credStatus of
                    CredentialCreated -> Just "created credential"
                    CredentialUpdated -> Just "updated credential"
                    CredentialUnchanged -> Nothing
                    CredentialSkipped -> Just "skipped credential"
                , if null newRoles
                    then Nothing
                    else Just ("added roles: " <> T.intercalate ", " (map roleSlug newRoles))
                ]
        prefix = case partyChange of
            PartyCreated -> "Created staff user "
            PartyUpdated -> "Updated staff user "
            PartyUnchanged -> "Ensured staff user "
        suffix = if null changes then "" else " [" <> T.intercalate "; " changes <> "]"
        msg = prefix <> nameTxt <> " (" <> roleListTxt <> ")" <> suffix
    liftIO $ putStrLn (T.unpack msg)

normalizeEmail :: Text -> Text
normalizeEmail = T.toLower . T.strip

roleSlug :: RoleEnum -> Text
roleSlug Admin = "admin"
roleSlug Manager = "manager"
roleSlug StudioManager = "studio-manager"
roleSlug Engineer = "engineer"
roleSlug Teacher = "teacher"
roleSlug Reception = "reception"
roleSlug Accounting = "accounting"
roleSlug LiveSessionsProducer = "live-sessions-producer"
roleSlug Intern = "intern"
roleSlug Artist = "artist"
roleSlug Artista = "artista"
roleSlug Webmaster = "webmaster"
roleSlug Promotor = "promotor"
roleSlug Promoter = "promoter"
roleSlug Producer = "producer"
roleSlug Songwriter = "songwriter"
roleSlug DJ = "dj"
roleSlug Publicist = "publicist"
roleSlug TourManager = "tourmanager"
roleSlug LabelRep = "labelrep"
roleSlug StageManager = "stagemanager"
roleSlug RoadCrew = "roadcrew"
roleSlug Photographer = "photographer"
roleSlug AandR = "ar"
roleSlug Student = "student"
roleSlug Vendor = "vendor"
roleSlug ReadOnly = "readonly"
roleSlug Customer = "customer"
roleSlug Fan = "fan"
roleSlug Maintenance = "maintenance"

ensureServiceCatalog ::
    (Text, ServiceKind, PricingModel, Maybe Int, Maybe Int, Text, Maybe Text) ->
    SqlPersistT IO ()
ensureServiceCatalog (nameTxt, kind, pricing, rateCents, taxBps, currencyTxt, billingUnit) = do
    let nameClean = T.strip nameTxt
        currencyClean =
            let c = T.strip currencyTxt
             in if T.null c then "USD" else c
    existing <- selectFirst [ServiceCatalogName ==. nameClean] []
    case existing of
        Just (Entity svcId _) ->
            update
                svcId
                [ ServiceCatalogKind =. kind
                , ServiceCatalogPricingModel =. pricing
                , ServiceCatalogDefaultRateCents =. rateCents
                , ServiceCatalogTaxBps =. taxBps
                , ServiceCatalogCurrency =. currencyClean
                , ServiceCatalogBillingUnit =. billingUnit
                , ServiceCatalogActive =. True
                ]
        Nothing -> do
            void $
                insert
                    ServiceCatalog
                        { serviceCatalogName = nameClean
                        , serviceCatalogKind = kind
                        , serviceCatalogPricingModel = pricing
                        , serviceCatalogDefaultRateCents = rateCents
                        , serviceCatalogTaxBps = taxBps
                        , serviceCatalogCurrency = currencyClean
                        , serviceCatalogBillingUnit = billingUnit
                        , serviceCatalogActive = True
                        }

ensureStaff :: UTCTime -> Text -> Maybe Text -> RoleEnum -> Text -> Text -> Text -> SqlPersistT IO (Key Party)
ensureStaff now name mlegal role token uname pwd = do
    pid <- ensurePartyRecord now name mlegal
    _ <- upsert (PartyRole pid role True) [PartyRoleActive =. True]
    upsertToken token pid (Just (roleLabel role))
    ensureCredential pid uname pwd
    pure pid

ensurePartyRecord :: UTCTime -> Text -> Maybe Text -> SqlPersistT IO (Key Party)
ensurePartyRecord now name mlegal = do
    existing <- selectFirst [PartyDisplayName ==. name] []
    case existing of
        Just (Entity pid _) -> pure pid
        Nothing -> insert $ Party mlegal name False Nothing Nothing Nothing Nothing Nothing Nothing Nothing now

upsertToken :: Text -> PartyId -> Maybe Text -> SqlPersistT IO ()
upsertToken token pid label = do
    mTok <- getBy (UniqueApiToken token)
    case mTok of
        Just (Entity tokId _) -> update tokId [ApiTokenPartyId =. pid, ApiTokenActive =. True, ApiTokenLabel =. label]
        Nothing -> do
            _ <- insert $ ApiToken token pid label True
            pure ()

ensureCredential :: PartyId -> Text -> Text -> SqlPersistT IO ()
ensureCredential pid uname pwd = do
    hashed <- liftIO (hashPasswordText pwd)
    _ <-
        upsert
            (UserCredential pid uname hashed True)
            [ UserCredentialPasswordHash =. hashed
            , UserCredentialActive =. True
            ]
    pure ()

hashPasswordText :: Text -> IO Text
hashPasswordText pwd = do
    let raw = TE.encodeUtf8 pwd
    mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy raw
    case mHash of
        Nothing -> fail "Failed to hash password"
        Just hash -> pure (TE.decodeUtf8 hash)

roleLabel :: RoleEnum -> Text
roleLabel = T.pack . show

ensureDropdownOption ::
    UTCTime ->
    (Text, Text, Maybe Text, Maybe Int) ->
    SqlPersistT IO ()
ensureDropdownOption now (categoryKey, valueTxt, mLabel, sortOrder) = do
    let labelValue = fromMaybe valueTxt mLabel
    existing <-
        selectFirst
            [ ME.DropdownOptionCategory ==. categoryKey
            , ME.DropdownOptionValue ==. valueTxt
            ]
            []
    case existing of
        Just (Entity optionId _) ->
            update
                optionId
                [ ME.DropdownOptionLabel =. Just labelValue
                , ME.DropdownOptionSortOrder =. sortOrder
                , ME.DropdownOptionActive =. True
                , ME.DropdownOptionUpdatedAt =. now
                ]
        Nothing -> do
            _ <-
                insert
                    DropdownOption
                        { dropdownOptionCategory = categoryKey
                        , dropdownOptionValue = valueTxt
                        , dropdownOptionLabel = Just labelValue
                        , dropdownOptionActive = True
                        , dropdownOptionSortOrder = sortOrder
                        , dropdownOptionCreatedAt = now
                        , dropdownOptionUpdatedAt = now
                        }
            pure ()

ensureSubjectRecord :: Text -> Bool -> SqlPersistT IO (Key Trials.Subject)
ensureSubjectRecord name isActive = do
    existing <- getBy (Trials.UniqueSubjectName name)
    case existing of
        Just (Entity sid subj) -> do
            when (Trials.subjectActive subj /= isActive) $
                update sid [Trials.SubjectActive =. isActive]
            pure sid
        Nothing ->
            insert
                Trials.Subject
                    { Trials.subjectName = name
                    , Trials.subjectActive = isActive
                    }

ensureSubjectRoomPref :: Key Trials.Subject -> ResourceId -> Int -> SqlPersistT IO ()
ensureSubjectRoomPref subjectId roomId priority = do
    void $
        upsert
            (Trials.SubjectRoomPreference subjectId roomId priority)
            [Trials.SubjectRoomPreferencePriority =. priority]

ensureTeacherSubjectLink :: PartyId -> Key Trials.Subject -> SqlPersistT IO ()
ensureTeacherSubjectLink teacherId subjectId = do
    _ <-
        insertUnique $
            Trials.TeacherSubject
                { Trials.teacherSubjectTeacherId = teacherId
                , Trials.teacherSubjectSubjectId = subjectId
                , Trials.teacherSubjectLevelMin = Nothing
                , Trials.teacherSubjectLevelMax = Nothing
                }
    pure ()

ensureTeacherAvailabilitySlot ::
    UTCTime ->
    PartyId ->
    Key Trials.Subject ->
    ResourceId ->
    UTCTime ->
    UTCTime ->
    SqlPersistT IO ()
ensureTeacherAvailabilitySlot createdAt teacherId subjectId roomId startAt endAt = do
    existing <-
        selectFirst
            [ Trials.TeacherAvailabilityTeacherId ==. teacherId
            , Trials.TeacherAvailabilitySubjectId ==. subjectId
            , Trials.TeacherAvailabilityStartAt ==. startAt
            , Trials.TeacherAvailabilityEndAt ==. endAt
            ]
            []
    case existing of
        Just _ -> pure ()
        Nothing -> do
            void $
                insert
                    Trials.TeacherAvailability
                        { Trials.teacherAvailabilityTeacherId = teacherId
                        , Trials.teacherAvailabilitySubjectId = subjectId
                        , Trials.teacherAvailabilityRoomId = roomId
                        , Trials.teacherAvailabilityStartAt = startAt
                        , Trials.teacherAvailabilityEndAt = endAt
                        , Trials.teacherAvailabilityNotes = Nothing
                        , Trials.teacherAvailabilityCreatedAt = createdAt
                        }

seedInventoryAssets :: SqlPersistT IO ()
seedInventoryAssets = mapM_ ensureInventoryAsset inventorySeeds

ensureInventoryAsset :: (Text, Maybe Text, Maybe Text, Text, Maybe Text, Maybe Int, Maybe Text) -> SqlPersistT IO ()
ensureInventoryAsset (nameTxt, brandTxt, modelTxt, categoryTxt, mDesc, mPriceCents, mPhotoUrl) = do
    resolvedPhoto <- case mPhotoUrl of
        Nothing -> pure Nothing
        Just raw ->
            if "inventory/" `T.isPrefixOf` raw
                then do
                    fileExists <- liftIO $ doesFileExist ("assets/" <> T.unpack raw)
                    pure (if fileExists then Just raw else Nothing)
                else pure (Just raw)
    existing <- selectFirst [ME.AssetName ==. nameTxt] []
    case existing of
        Just (Entity assetId asset) -> do
            let updates =
                    catMaybes
                        [ if ME.assetCategory asset == categoryTxt
                            then Nothing
                            else Just (ME.AssetCategory =. categoryTxt)
                        , case (ME.assetBrand asset, brandTxt) of
                            (Nothing, Just brandVal) -> Just (ME.AssetBrand =. Just brandVal)
                            _ -> Nothing
                        , case (ME.assetModel asset, modelTxt) of
                            (Nothing, Just modelVal) -> Just (ME.AssetModel =. Just modelVal)
                            _ -> Nothing
                        , case (ME.assetPhotoUrl asset, mPhotoUrl, resolvedPhoto) of
                            (Just current, Just seeded, Nothing) | current == seeded -> Just (ME.AssetPhotoUrl =. Nothing)
                            _ -> Nothing
                        ]
            unless (null updates) (update assetId updates)
        Nothing -> do
            void $
                insert
                    ME.Asset
                        { ME.assetName = nameTxt
                        , ME.assetCategory = categoryTxt
                        , ME.assetBrand = brandTxt
                        , ME.assetModel = modelTxt
                        , ME.assetSerialNumber = Nothing
                        , ME.assetPurchaseDate = Nothing
                        , ME.assetPurchasePriceUsdCents = mPriceCents
                        , ME.assetCondition = ME.Good
                        , ME.assetStatus = ME.Active
                        , ME.assetLocationId = Nothing
                        , ME.assetOwner = "TDF"
                        , ME.assetQrCode = Nothing
                        , ME.assetPhotoUrl = resolvedPhoto
                        , ME.assetNotes = mDesc
                        , ME.assetWarrantyExpires = Nothing
                        , ME.assetMaintenancePolicy = ME.None
                        , ME.assetNextMaintenanceDue = Nothing
                        }
            pure ()

data RentSaleRow = RentSaleRow
    { code :: Text
    , name :: Text
    , category :: Text
    , rent_recommended :: Maybe Double
    , sale_recommended :: Maybe Double
    , source :: Maybe Text
    , note :: Maybe Text
    }
    deriving (Show, Generic)
instance FromJSON RentSaleRow

loadRentSaleRows :: IO (Maybe [RentSaleRow])
loadRentSaleRows = do
    fileExists <- doesFileExist "data/inventory_rent_sale.json"
    if not fileExists
        then pure Nothing
        else do
            raw <- BL.readFile "data/inventory_rent_sale.json"
            pure (decode raw :: Maybe [RentSaleRow])

seedMarketplaceListings :: SqlPersistT IO ()
seedMarketplaceListings = do
    now <- liftIO getCurrentTime
    mRows <- liftIO loadRentSaleRows
    case mRows of
        Nothing -> seedFromStatic now
        Just rows -> do
            forM_ rows $ \RentSaleRow{..} -> do
                let nameClean = T.strip name
                    catClean = T.toLower (T.strip category)
                mExisting <- selectFirst [ME.AssetName ==. nameClean] []
                assetId <- case mExisting of
                    Just (Entity aid _) -> pure aid
                    Nothing -> do
                        aid <-
                            insert
                                ME.Asset
                                    { ME.assetName = nameClean
                                    , ME.assetCategory = catClean
                                    , ME.assetBrand = Nothing
                                    , ME.assetModel = Nothing
                                    , ME.assetSerialNumber = Nothing
                                    , ME.assetPurchaseDate = Nothing
                                    , ME.assetPurchasePriceUsdCents = Nothing
                                    , ME.assetCondition = ME.Good
                                    , ME.assetStatus = ME.Active
                                    , ME.assetLocationId = Nothing
                                    , ME.assetOwner = "TDF"
                                    , ME.assetQrCode = Nothing
                                    , ME.assetPhotoUrl = Nothing
                                    , ME.assetNotes = note
                                    , ME.assetWarrantyExpires = Nothing
                                    , ME.assetMaintenancePolicy = ME.None
                                    , ME.assetNextMaintenanceDue = Nothing
                                    }
                        pure aid
                let upsertListing purpose price maybeMarkup = do
                        let priceCents = maybe 0 id price
                            titleTxt = nameClean
                            mk = maybe 40 id maybeMarkup
                        when (priceCents > 0) $ do
                            existing <- getBy (ME.UniqueMarketplaceAsset assetId purpose)
                            case existing of
                                Just (Entity lid listing) -> do
                                    let updates =
                                            catMaybes
                                                [ if ME.marketplaceListingPriceUsdCents listing == priceCents
                                                    then Nothing
                                                    else Just (ME.MarketplaceListingPriceUsdCents =. priceCents)
                                                , if ME.marketplaceListingTitle listing == titleTxt
                                                    then Nothing
                                                    else Just (ME.MarketplaceListingTitle =. titleTxt)
                                                , if ME.marketplaceListingMarkupPct listing == mk
                                                    then Nothing
                                                    else Just (ME.MarketplaceListingMarkupPct =. mk)
                                                , if ME.marketplaceListingActive listing
                                                    then Nothing
                                                    else Just (ME.MarketplaceListingActive =. True)
                                                , Just (ME.MarketplaceListingUpdatedAt =. now)
                                                ]
                                    unless (null updates) (update lid updates)
                                Nothing ->
                                    void $
                                        insert
                                            ME.MarketplaceListing
                                                { ME.marketplaceListingAssetId = assetId
                                                , ME.marketplaceListingTitle = titleTxt
                                                , ME.marketplaceListingPurpose = purpose
                                                , ME.marketplaceListingPriceUsdCents = priceCents
                                                , ME.marketplaceListingMarkupPct = mk
                                                , ME.marketplaceListingCurrency = "USD"
                                                , ME.marketplaceListingActive = True
                                                , ME.marketplaceListingCreatedAt = now
                                                , ME.marketplaceListingUpdatedAt = now
                                                }
                let rentPrice = fmap (\r -> round (r * 1.4 * 100)) rent_recommended
                    salePrice = fmap (\s -> round (s * 1.4 * 100)) sale_recommended
                upsertListing "rent" rentPrice (Just 40)
                upsertListing "sale" salePrice (Just 40)
  where
    seedFromStatic now = do
        assets <- selectList [] [Asc ME.AssetName]
        forM_ assets $ \(Entity assetId asset) -> do
            let basePrice =
                    Map.lookup (ME.assetName asset) marketplacePriceCents
                        <|> ME.assetPurchasePriceUsdCents asset
            case basePrice of
                Nothing -> pure ()
                Just baseCents -> do
                    let listingPrice = applyMarketplaceMarkup baseCents
                        titleTxt = ME.assetName asset
                    existing <- getBy (ME.UniqueMarketplaceAsset assetId "sale")
                    case existing of
                        Just (Entity listingId listing) -> do
                            let updates =
                                    catMaybes
                                        [ if ME.marketplaceListingPriceUsdCents listing == listingPrice
                                            then Nothing
                                            else Just (ME.MarketplaceListingPriceUsdCents =. listingPrice)
                                        , if ME.marketplaceListingTitle listing == titleTxt
                                            then Nothing
                                            else Just (ME.MarketplaceListingTitle =. titleTxt)
                                        , if ME.marketplaceListingActive listing
                                            then Nothing
                                            else Just (ME.MarketplaceListingActive =. True)
                                        , Just (ME.MarketplaceListingUpdatedAt =. now)
                                        ]
                            unless (null updates) (update listingId updates)
                        Nothing -> do
                            void $
                                insert
                                    ME.MarketplaceListing
                                        { ME.marketplaceListingAssetId = assetId
                                        , ME.marketplaceListingTitle = titleTxt
                                        , ME.marketplaceListingPurpose = "sale"
                                        , ME.marketplaceListingPriceUsdCents = listingPrice
                                        , ME.marketplaceListingMarkupPct = 25
                                        , ME.marketplaceListingCurrency = "USD"
                                        , ME.marketplaceListingActive = True
                                        , ME.marketplaceListingCreatedAt = now
                                        , ME.marketplaceListingUpdatedAt = now
                                        }
            pure ()

inventorySeeds :: [(Text, Maybe Text, Maybe Text, Text, Maybe Text, Maybe Int, Maybe Text)]
inventorySeeds =
    [ ("MXR Bass Envelope Filter M82", Just "MXR", Just "M82", "fx/pedal", Just "Bass envelope filter pedal with Dry/FX blend, Decay, Q, and Sensitivity controls.", Just 18000, Just "inventory/mxr-bass-envelope-filter.jpeg")
    , ("Boss LMB-3 Bass Limiter Enhancer", Just "Boss", Just "LMB-3", "fx/pedal", Just "Bass limiter/enhancer pedal with threshold, enhance, level, and ratio controls.", Just 11000, Just "inventory/boss-lmb-3.jpeg")
    , ("Boss BF-3 Flanger", Just "Boss", Just "BF-3", "fx/pedal", Just "Stereo flanger with guitar/bass inputs and gate/pan, ultra, and momentary modes.", Just 12000, Just "inventory/unknown-05dfa8a8.jpeg")
    , ("Boss GEB-7 Bass Equalizer", Just "Boss", Just "GEB-7", "fx/pedal", Just "7-band bass EQ pedal (50Hz–10kHz) with level slider.", Just 11000, Just "inventory/boss-geb-7.jpeg")
    , ("Electro-Harmonix Q-Tron Plus", Just "Electro-Harmonix", Just "Q-Tron Plus", "fx/pedal", Just "Envelope filter with LP/BP/HP modes, sweep range, peak/gain/boost controls, and effects loop.", Just 19000, Just "inventory/ehx-q-tron-plus.jpeg")
    , ("Moog Minitaur Analog Bass Synthesizer", Just "Moog", Just "Minitaur", "synth", Just "Analog bass synth with dual oscillators, ladder filter, envelopes, modulation, and glide.", Just 65000, Just "inventory/moog-minitaur.jpeg")
    , ("Boss OS-2 OverDrive/Distortion", Just "Boss", Just "OS-2", "fx/pedal", Just "Overdrive/distortion pedal with level, tone, drive, and OD/DS color blend.", Just 9000, Just "inventory/boss-os-2.jpeg")
    , ("Custom Spring Reverb Filter Unit", Just "Custom", Just "Spring Filter FX", "fx/outboard", Just "Outboard spring reverb with voltage-controlled filter, LFO modulation, and wet/dry output mixer.", Just 35000, Just "inventory/custom-spring-filter.jpeg")
    , ("Boss CH-1 Super Chorus", Just "Boss", Just "CH-1", "fx/pedal", Just "Stereo chorus pedal with level, EQ hi/lo, rate, and depth controls.", Just 9000, Just "inventory/boss-ch-1.jpeg")
    , ("Electro-Harmonix V256 Vocoder", Just "Electro-Harmonix", Just "V256", "fx/pedal", Just "Vocoder with multiple robot/drone modes, blend, bands, tone, pitch, gender-bender, and mic gain.", Just 25000, Just "inventory/ehx-v256.jpeg")
    , ("CMATMods Signa Drive", Just "CMATMods", Just "Signa Drive", "fx/pedal", Just "Overdrive pedal with gain/tone/level controls and mid-toggle voicing.", Just 14000, Just "inventory/custom-signa-drive.jpeg")
    , ("EarthQuaker Devices Disaster Transport SR", Just "EarthQuaker Devices", Just "Disaster Transport SR", "fx/pedal", Just "Dual delay and reverb machine with modulation (depth/rate), blend, repeats, and independent delay channels.", Just 32000, Just "inventory/eqd-disaster-transport-sr.jpeg")
    , ("Death By Audio Robot", Just "Death By Audio", Just "Robot", "fx/pedal", Just "8-bit/lo-fi pitch-shifting robot fuzz with volume, intensity, and 4-mode selector.", Just 24000, Just "inventory/death-by-audio-robot.jpeg")
    , ("TC Electronic PolyTune", Just "TC Electronic", Just "PolyTune", "fx/pedal", Just "Polyphonic tuner pedal with true bypass and selectable display modes.", Just 9000, Just "inventory/tc-polytune.jpeg")
    , ("Tech 21 SansAmp Bass Driver Deluxe", Just "Tech 21", Just "SansAmp Bass Driver Deluxe", "fx/pedal", Just "Bass preamp/DI with drive, 3-band EQ, presence, blend, dual inputs, and programmable channels.", Just 23000, Just "inventory/tech21-sansamp-bass-driver-deluxe.jpeg")
    , ("Electro-Harmonix Q-Tron Plus (Large)", Just "Electro-Harmonix", Just "Q-Tron Plus", "fx/pedal", Just "Envelope filter with peak/gain/range controls, fast/slow response, and FX loop.", Just 19000, Just "inventory/ehx-q-tron-plus-large.jpeg")
    , ("BBE Sonic Stomp Mini", Just "BBE", Just "Sonic Stomp Mini", "fx/pedal", Just "Sonic maximizer-style enhancer with process and low contour controls.", Just 9000, Just "inventory/bbe-sonic-stomp.jpeg")
    , ("Electro-Harmonix HOG2 Harmonic Octave Generator", Just "Electro-Harmonix", Just "HOG2", "fx/pedal", Just "Harmonic octave generator with sliders for harmonic levels, envelope/filter, presets, and expression control.", Just 42000, Just "inventory/ehx-hog2.jpeg")
    , ("Moog Moogerfooger Ring Modulator MF-102", Just "Moog", Just "MF-102", "fx/pedal", Just "Analog ring modulator with LFO amount/rate, drive, mix, and frequency controls.", Just 32000, Just "inventory/moog-moogerfooger-ring-mod.jpeg")
    , ("EarthQuaker Devices Plumes", Just "EarthQuaker Devices", Just "Plumes", "fx/pedal", Just "Overdrive pedal with three clipping modes plus level/tone/gain controls.", Just 12000, Just "inventory/eqd-plumes.jpeg")
    , ("Boss RC-20XL Loop Station", Just "Boss", Just "RC-20XL", "fx/looper", Just "Twin-pedal looper with phrase storage, tap tempo, reverse, auto start, and mic/inst inputs.", Just 22000, Just "inventory/boss-rc-20xl.jpeg")
    , ("Moog CP-251 Control Processor", Just "Moog", Just "CP-251", "fx/outboard", Just "Control processor with mixer, lag, LFO, attenuators, S&H, noise, and multiples for CV routing.", Just 43000, Just "inventory/moog-cp251.jpeg")
    , ("Chandler Limited Little Devil Colored Boost", Just "Chandler Limited", Just "Little Devil Boost", "fx/pedal", Just "Colored boost with color, feedback/bias controls and mids/highs/bright voicing toggles.", Just 25000, Just "inventory/chandler-limited-little-devil-boost.jpeg")
    , ("DOD Meatbox SubSynth", Just "DOD", Just "Meatbox", "fx/pedal", Just "Sub-synth/low-end enhancer pedal with sub and low controls for deep bass reinforcement.", Just 18000, Just "inventory/dod-meatbox.jpeg")
    , ("MFB Tanzbär Analog Drum Machine", Just "MFB", Just "Tanzbär", "synth/drum", Just "Analog drum machine with multiple drum voices, filters, modulation, and individual outs.", Just 70000, Just "inventory/mfb-tanzbar.jpeg")
    , ("Moog MF Drive", Just "Moog", Just "MF Drive", "fx/pedal", Just "Ladder-filter drive pedal with gain, tone, output, filter, and peak switch; expression input.", Just 17000, Just "inventory/moog-mf-drive.jpeg")
    , ("Musitronics Mu-Tron III", Just "Musitronics", Just "Mu-Tron III", "fx/pedal", Just "Classic envelope filter/auto-wah with LP/BP/HP modes, peak, gain, range, and drive switches.", Just 30000, Just "inventory/mutron-iii.jpeg")
    , ("Moog Moogerfooger Bass MuRF", Just "Moog", Just "Bass MuRF", "fx/pedal", Just "Bass MuRF filter sequencer with animation patterns, envelope, mix, rate, and 8 filter sliders.", Just 48000, Just "inventory/moog-bass-murf.jpeg")
    , ("Electro-Harmonix The Mole Bass Booster", Just "Electro-Harmonix", Just "The Mole", "fx/pedal", Just "Bass booster pedal with single boost control in nano form factor.", Just 7000, Just "inventory/ehx-the-mole.jpeg")
    , ("Electro-Harmonix Deluxe Bass Big Muff Pi", Just "Electro-Harmonix", Just "Deluxe Bass Big Muff", "fx/pedal", Just "Bass fuzz with blend, tone, sustain, gate, and crossover controls plus crossover footswitch.", Just 17000, Just "inventory/ehx-deluxe-bass-big-muff.jpeg")
    , ("Keeley Katana Clean Boost", Just "Keeley Electronics", Just "Katana", "fx/pedal", Just "Clean boost with pull-boost mode and external level control (trimpot).", Just 14000, Just "inventory/keeley-katana.jpeg")
    , ("Boss DD-7 Digital Delay", Just "Boss", Just "DD-7", "fx/pedal", Just "Digital delay with multiple modes (analog, modulate, reverse), tap tempo/exp, stereo I/O.", Just 12000, Just "inventory/boss-dd-7.jpeg")
    , ("Vertex Compressor", Just "Vertex Effects", Just "Compressor", "fx/pedal", Just "Compressor pedal with attack, clipping/ratio, and level controls.", Just 18000, Just "inventory/vertex-compressor.jpeg")
    , ("QHA-4 4-Channel Headphone Amplifier", Just "QHA", Just "QHA-4", "amp/headphone", Just "Desktop 4-channel stereo headphone amplifier with independent level controls for each output.", Just 6000, Just "inventory/headphone-amp-qha.jpeg")
    , ("AKG D112", Just "AKG", Just "D112", "mic", Nothing, Nothing, Nothing)
    , ("Shure SM57", Just "Shure", Just "SM57", "mic", Nothing, Nothing, Nothing)
    , ("Sennheiser MD421", Just "Sennheiser", Just "MD421", "mic", Nothing, Nothing, Nothing)
    , ("AKG C414", Just "AKG", Just "C414", "mic", Nothing, Nothing, Nothing)
    , ("Electro-Voice RE20", Just "Electro-Voice", Just "RE20", "mic", Nothing, Nothing, Nothing)
    , ("Neumann KM184", Just "Neumann", Just "KM184", "mic", Nothing, Nothing, Nothing)
    , ("Royer R121", Just "Royer", Just "R121", "mic", Nothing, Nothing, Nothing)
    , ("Sennheiser e906", Just "Sennheiser", Just "e906", "mic", Nothing, Nothing, Nothing)
    , ("Sennheiser e835", Just "Sennheiser", Just "e835", "mic", Nothing, Nothing, Nothing)
    , ("Sennheiser MKE600", Just "Sennheiser", Just "MKE600", "mic", Nothing, Nothing, Nothing)
    , ("Neumann KU-100", Just "Neumann", Just "KU-100", "mic", Nothing, Nothing, Nothing)
    , ("Neve RNDI", Just "Rupert Neve Designs", Just "RNDI", "di", Nothing, Nothing, Nothing)
    , ("Aguilar ToneHammer", Just "Aguilar", Just "ToneHammer", "di/pre", Nothing, Nothing, Nothing)
    , ("Avalon 737sp", Just "Avalon", Just "VT-737sp", "pre", Nothing, Nothing, Nothing)
    , ("UA 2-610", Just "Universal Audio", Just "2-610", "pre", Nothing, Nothing, Nothing)
    , ("API 512v", Just "API", Just "512v", "pre", Nothing, Nothing, Nothing)
    , ("Chandler Limited REDD.47", Just "Chandler Limited", Just "REDD.47", "pre", Nothing, Nothing, Nothing)
    , ("Burl BAD8", Just "Burl", Just "BAD8", "converter-ad", Nothing, Nothing, Nothing)
    , ("Burl B4", Just "Burl", Just "B4", "pre/converter", Nothing, Nothing, Nothing)
    , ("RedNet MP8R", Just "Focusrite", Just "MP8R", "pre/dante", Nothing, Nothing, Nothing)
    , ("Red 8Pre", Just "Focusrite", Just "Red 8Pre", "interface", Nothing, Nothing, Nothing)
    , ("PSM-900", Just "Shure", Just "PSM-900", "iem", Nothing, Nothing, Nothing)
    , ("Shure SM58", Just "Shure", Just "SM58", "mic", Nothing, Nothing, Nothing)
    ]

-- Approximate 2025 USD street prices (before markup). Each listing is published at +25%.
marketplacePriceCents :: Map.Map Text Int
marketplacePriceCents =
    Map.fromList
        [ ("AKG D112", 19900)
        , ("Shure SM57", 9900)
        , ("Sennheiser MD421", 37900)
        , ("AKG C414", 109900)
        , ("Electro-Voice RE20", 44900)
        , ("Neumann KM184", 84900)
        , ("Royer R121", 139500)
        , ("Sennheiser e906", 18900)
        , ("Sennheiser e835", 9900)
        , ("Sennheiser MKE600", 32900)
        , ("Neumann KU-100", 899900)
        , ("Neve RNDI", 26900)
        , ("Aguilar ToneHammer", 29900)
        , ("Avalon 737sp", 299500)
        , ("UA 2-610", 299900)
        , ("API 512v", 119500)
        , ("Chandler Limited REDD.47", 299500)
        , ("Burl BAD8", 359900)
        , ("Burl B4", 249900)
        , ("RedNet MP8R", 449900)
        , ("Red 8Pre", 299900)
        , ("PSM-900", 99900)
        , ("Shure SM58", 9900)
        ]

applyMarketplaceMarkup :: Int -> Int
applyMarketplaceMarkup baseCents = (baseCents * 125) `div` 100

seedHolgerSession :: UTCTime -> SqlPersistT IO ()
seedHolgerSession now = do
    let clientName = "Holger Qui\x00f1\x00f3nez"
        sessionStart = UTCTime (fromGregorian 2025 11 1) (secondsToDiffTime (10 * 60 * 60))
        sessionEnd = UTCTime (fromGregorian 2025 11 1) (secondsToDiffTime (19 * 60 * 60))
    existing <-
        selectFirst
            [ ME.SessionClientPartyRef ==. Just clientName
            , ME.SessionService ==. "Recording"
            ]
            []
    sessionId <- case existing of
        Just (Entity sid _) -> pure sid
        Nothing ->
            insert
                ME.Session
                    { ME.sessionBookingRef = Nothing
                    , ME.sessionBandId = Nothing
                    , ME.sessionClientPartyRef = Just clientName
                    , ME.sessionService = "Recording"
                    , ME.sessionStartAt = sessionStart
                    , ME.sessionEndAt = sessionEnd
                    , ME.sessionEngineerRef = "Seed Engineer"
                    , ME.sessionAssistantRef = Nothing
                    , ME.sessionStatus = ME.InPrep
                    , ME.sessionSampleRate = Just 48000
                    , ME.sessionBitDepth = Just 24
                    , ME.sessionDaw = Just "Pro Tools"
                    , ME.sessionSessionFolderDriveId = Nothing
                    , ME.sessionNotes = Just "Seeded session for Holger Qui\x00f1\x00f3nez (inventory demo)"
                    }
    ensureHolgerSessionRoom sessionId
    ensureHolgerInputList now sessionId

ensureHolgerSessionRoom :: ME.SessionId -> SqlPersistT IO ()
ensureHolgerSessionRoom sessionId = do
    existing <- selectFirst [ME.SessionRoomSessionId ==. sessionId] []
    case existing of
        Just _ -> pure ()
        Nothing -> do
            mRoom <- selectFirst [ME.RoomName ==. "Studio A"] []
            forM_ mRoom $ \(Entity roomId _) ->
                void $
                    insertUnique
                        ME.SessionRoom
                            { ME.sessionRoomSessionId = sessionId
                            , ME.sessionRoomRoomId = roomId
                            }

ensureHolgerInputList :: UTCTime -> ME.SessionId -> SqlPersistT IO ()
ensureHolgerInputList now sessionId = do
    listId <- ensureInputListRecord sessionId now
    versionId <- ensureInputListVersionRecord listId now
    mapM_ (ensureInputRow versionId) holgerInputSeeds

ensureInputListRecord :: ME.SessionId -> UTCTime -> SqlPersistT IO ME.InputListId
ensureInputListRecord sessionId now = do
    existing <- selectFirst [ME.InputListSessionId ==. sessionId] []
    case existing of
        Just (Entity listId _) -> pure listId
        Nothing ->
            insert
                ME.InputList
                    { ME.inputListSessionId = sessionId
                    , ME.inputListCreatedAt = now
                    }

ensureInputListVersionRecord ::
    ME.InputListId ->
    UTCTime ->
    SqlPersistT IO ME.InputListVersionId
ensureInputListVersionRecord listId now = do
    existing <-
        selectFirst
            [ ME.InputListVersionInputListId ==. listId
            , ME.InputListVersionVersion ==. 1
            ]
            []
    case existing of
        Just (Entity versionId _) -> pure versionId
        Nothing ->
            insert
                ME.InputListVersion
                    { ME.inputListVersionInputListId = listId
                    , ME.inputListVersionVersion = 1
                    , ME.inputListVersionCreatedAt = now
                    , ME.inputListVersionCreatedByRef = Just "seed"
                    , ME.inputListVersionNotes = Just "Holger Qui\x00f1\x00f3nez session input list"
                    }

ensureInputRow :: ME.InputListVersionId -> InputSeed -> SqlPersistT IO ()
ensureInputRow versionId entry = do
    let notesParts =
            catMaybes
                [ Just ("Mic/DI: " <> isMic entry)
                , fmap ("Medusa: " <>) (isMedusa entry)
                , fmap ("Preamp: " <>) (isPreamp entry)
                , Just ("Interface: " <> isInterface entry)
                , Just ("DAW Ch: " <> T.pack (show (isDawChannel entry)))
                , fmap ("Notes: " <>) (isExtraNotes entry)
                ]
        notesText = case notesParts of
            [] -> Nothing
            xs -> Just (T.intercalate " | " xs)
    void $
        insertUnique
            ME.InputRow
                { ME.inputRowVersionId = versionId
                , ME.inputRowChannelNumber = isChannel entry
                , ME.inputRowTrackName = Just (isSource entry)
                , ME.inputRowInstrument = Just (isMic entry)
                , ME.inputRowMicId = Nothing
                , ME.inputRowStandId = Nothing
                , ME.inputRowCableId = Nothing
                , ME.inputRowPreampId = Nothing
                , ME.inputRowInsertOutboardId = Nothing
                , ME.inputRowConverterChannel = Just (isInterface entry)
                , ME.inputRowPhantom = Nothing
                , ME.inputRowPolarity = Nothing
                , ME.inputRowHpf = Nothing
                , ME.inputRowPad = Nothing
                , ME.inputRowNotes = notesText
                }

data InputSeed = InputSeed
    { isChannel :: Int
    , isSource :: Text
    , isMic :: Text
    , isMedusa :: Maybe Text
    , isPreamp :: Maybe Text
    , isInterface :: Text
    , isDawChannel :: Int
    , isExtraNotes :: Maybe Text
    }

holgerInputSeeds :: [InputSeed]
holgerInputSeeds =
    [ InputSeed 1 "Kick In" "AKG D112" (Just "M1") (Just "Shelford 1") "BMB3 - 01 BAD8 1" 1 Nothing
    , InputSeed 2 "Snare Up" "Shure SM57" (Just "M2") (Just "Shelford 2") "BMB3 - 02 BAD8 2" 2 Nothing
    , InputSeed 3 "Snare Down" "Shure SM57" (Just "M3") (Just "Shelford 3") "BMB3 - 03 BAD8 3" 3 (Just "Flip en DAW")
    , InputSeed 4 "Hi-Hat" "Sennheiser MKE600" (Just "M4") (Just "Avalon 737sp (suave)") "BMB3 - 04 BAD8 4" 4 Nothing
    , InputSeed 5 "Tom 1" "Sennheiser MD421" (Just "M5") (Just "UA 2-610 L") "BMB3 - 05 BAD8 5" 5 Nothing
    , InputSeed 6 "Tom Floor" "Sennheiser MD421" (Just "M6") (Just "UA 2-610 R") "BMB3 - 06 BAD8 6" 6 Nothing
    , InputSeed 7 "OH L" "AKG C414 (HC)" (Just "M7") (Just "API 512v 1") "BMB3 - 07 BAD8 7" 7 Nothing
    , InputSeed 8 "OH R" "AKG C414 (HC)" (Just "M8") (Just "API 512v 2") "BMB3 - 08 BAD8 8" 8 Nothing
    , InputSeed 9 "Bass DI (post)" "Neve RNDI" (Just "M9") (Just "B4-1") "BMB3 - 17 B4 1 1" 9 Nothing
    , InputSeed 10 "Bass Mic 1 (cab)" "AKG D112" (Just "M10") (Just "B4-1") "BMB3 - 18 B4 1 2" 10 Nothing
    , InputSeed 11 "Bass Mic 2 (ataque)" "Neumann KM184" (Just "M11") (Just "B4-1") "BMB3 - 19 B4 1 3" 11 Nothing
    , InputSeed 12 "Gtr 1" "Sennheiser e906" (Just "M12") (Just "B4-1") "BMB3 - 20 B4 1 4" 12 Nothing
    , InputSeed 13 "Gtr 1 Ribbon" "Royer R121" (Just "M13") (Just "B4-2") "BMB3 - 25 B4 2 1" 13 Nothing
    , InputSeed 14 "Gtr 2" "Sennheiser e906" (Just "M14") (Just "B4-2") "BMB3 - 26 B4 2 2" 14 Nothing
    , InputSeed 15 "Gtr 2 Ribbon" "Royer R121" (Just "M15") (Just "B4-2") "BMB3 - 27 B4 2 3" 15 Nothing
    , InputSeed 16 "Vox 1" "Electro-Voice RE20" (Just "M16") (Just "Chandler Limited REDD.47") "BMB3 - 09 BAD4 1" 25 Nothing
    , InputSeed 17 "Vox 2" "Sennheiser e835" Nothing (Just "MP8R (Vox2)") "MP8R - Vox2" 17 Nothing
    , InputSeed 18 "Vox 3" "Shure SM58" Nothing (Just "MP8R (Vox3)") "MP8R - Vox3" 18 Nothing
    , InputSeed 19 "Vox 4" "Shure SM58" Nothing (Just "MP8R (Vox4)") "MP8R - Vox4" 19 Nothing
    , InputSeed 20 "KU-100 L" "Neumann KU-100 L" Nothing (Just "MP8R (KU100L)") "MP8R - KU100L" 20 (Just "Room, frente a banda")
    , InputSeed 21 "KU-100 R" "Neumann KU-100 R" Nothing (Just "MP8R (KU100R)") "MP8R - KU100R" 21 Nothing
    ]
