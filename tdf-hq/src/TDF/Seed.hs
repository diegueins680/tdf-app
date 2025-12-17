{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module TDF.Seed where

import           Control.Applicative   ((<|>))
import           Control.Monad          (forM, forM_, unless, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Crypto.BCrypt          (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Database.Persist
import           Database.Persist.Sql
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Aeson             (decode, FromJSON)
import qualified Data.ByteString.Lazy  as BL
import           Data.Text              (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Time              (NominalDiffTime, UTCTime(..), addUTCTime, fromGregorian, getCurrentTime,
                                         secondsToDiffTime)
import qualified Data.Map.Strict       as Map
import           System.Directory       (doesFileExist)
import           System.Environment     (lookupEnv)
import           GHC.Generics           (Generic)
import           TDF.Models
import           TDF.ModelsExtra        (DropdownOption(..))
import qualified TDF.ModelsExtra       as ME
import qualified TDF.Trials.Models     as Trials
import           TDF.Pipelines          (canonicalStage, defaultStage)
import           TDF.Config             (resolveAppBase)

-- Seed data from Diego's YAML (normalized)
seedAll :: SqlPersistT IO ()
seedAll = do
  now <- liftIO getCurrentTime

  -- Parties: Artists & Teachers
  let artists =
        [ ("Arkabuz", Nothing)
        , ("El Bloque", Nothing)
        , ("Skanka Fe", Nothing)
        , ("Quimika Soul", Nothing)
        , ("Juano Ledesma", Just "Juan Ledesma")
        ]
  mapM_ (\(disp, mlegal) -> do
           _ <- ensurePartyRecord now disp mlegal
           pure ()
        ) artists

  let teachers =
        [ ("César Galarza", Nothing)
        , ("Fabricio Alomía", Nothing)
        , ("Juan Ledesma", Nothing)
        ]
  teacherPairs <- forM teachers $ \(disp, mlegal) -> do
    pid <- ensurePartyRecord now disp mlegal
    _ <- upsert (PartyRole pid Teacher True)
      [ PartyRoleActive =. True ]
    pure (disp, pid)

  -- Service Catalog
  let svcSeeds =
        [ ("Grabación de Banda", Recording, Hourly, Just (25 * 100), Just 1200, "USD", Just "hora")
        , ("Grabación de Voz", Recording, Hourly, Just (35 * 100), Just 1200, "USD", Just "hora")
        , ("Mezcla", Mixing, PerSong, Just (120 * 100), Just 1200, "USD", Just "canción")
        , ("Mastering", Mastering, PerSong, Just (70 * 100), Just 1200, "USD", Just "canción")
        , ("Ensayo", Rehearsal, Hourly, Just (30 * 100), Just 1200, "USD", Just "hora")
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
      ensurePipelineCard
        :: (ServiceKind, Text, Maybe Text, Maybe Text, Int)
        -> SqlPersistT IO ()
      ensurePipelineCard (kind, titleTxt, artistTxt, stageTxt, sortOrder) = do
        existing <- selectFirst
          [ ME.PipelineCardServiceKind ==. kind
          , ME.PipelineCardTitle ==. titleTxt
          ]
          []
        case existing of
          Just _  -> pure ()
          Nothing -> do
            let stageValue = maybe (defaultStage kind) id (stageTxt >>= canonicalStage kind)
            _ <- insert ME.PipelineCard
                  { ME.pipelineCardServiceKind = kind
                  , ME.pipelineCardTitle       = titleTxt
                  , ME.pipelineCardArtist      = artistTxt
                  , ME.pipelineCardStage       = stageValue
                  , ME.pipelineCardSortOrder   = sortOrder
                  , ME.pipelineCardNotes       = Nothing
                  , ME.pipelineCardCreatedAt   = now
                  , ME.pipelineCardUpdatedAt   = now
                  }
            pure ()
  mapM_ ensurePipelineCard pipelineSeeds

  -- Package Product: Guitar 24h
  _ <- insertUnique $ PackageProduct
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
  let rooms = ["Booth A","Booth B","Booth C","Booth D","Live Room","Control Room","Synth Room","Studio A","Studio B","Rehearsal 1","Classroom"]
  roomPairs <- forM rooms $ \r -> do
    existing <- selectFirst [ResourceName ==. r] []
    case existing of
      Just (Entity rid _) -> pure (r, rid)
      Nothing -> do
        rid <- insert $ Resource r (slugify r) Room Nothing True
        pure (r, rid)

  -- Subjects and room availability preferences
  let subjectSeeds =
        [ ("DJ", True, ["Classroom","Studio B"])
        , ("Producción Musical", True, ["Studio B","Control Room"])
        , ("Grabación", True, ["Studio A","Control Room"])
        ]
  subjectPairs <- forM subjectSeeds $ \(subjectName, isActive, roomNames) -> do
    sid <- ensureSubjectRecord subjectName isActive
    forM_ (zip [1 :: Int ..] roomNames) $ \(priority, roomName) -> do
      case lookup roomName roomPairs of
        Nothing     -> pure ()
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
      _                                -> pure ()

  -- Publish sample availability windows (45 minutes each)
  let minutes :: Int -> NominalDiffTime
      minutes m = realToFrac (m * 60)
      addMinutes m = addUTCTime (minutes m)
      availabilitySeeds =
        [ ("César Galarza", "DJ", "Classroom", 24*60 + 540)           -- mañana 09:00
        , ("César Galarza", "DJ", "Classroom", 24*60 + 600)           -- mañana 10:00
        , ("Fabricio Alomía", "Producción Musical", "Studio B", 36*60 + 600)
        , ("Juan Ledesma", "Grabación", "Studio A", 48*60 + 480)
        ]
  forM_ availabilitySeeds $ \(teacherName, subjectName, roomName, startMinutes) ->
    case ( lookup teacherName teacherPairs
         , lookup subjectName subjectPairs
         , lookup roomName roomPairs
         ) of
      (Just teacherId, Just subjectId, Just roomId) -> do
        let slotStart = addMinutes startMinutes now
            slotEnd   = addMinutes (startMinutes + 45) now
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
  mapM_ (\(disp, mlegal, role, token, uname, pwd) -> do
           _ <- ensureStaff now disp mlegal role token uname pwd
           pure ()
        ) staffAccounts

  seedCoreStaffRoles now

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
      slugVal = fromMaybe "produccion-musical-dic-2025" (slugEnv >>= nonEmptyText)
      courseTitle = "Curso de Producción Musical"
      subtitleTxt = Just "Presencial · 4 sábados · 16 horas"
      formatTxt = Just "Presencial"
      durationTxt = Just "4 sábados · 16 horas"
      priceCentsVal = 15000
      currencyVal = "USD"
      capacityVal = 16
      sessionStart = Just 15
      sessionDuration = Just 4
      locationLabel = Just "TDF Records – Quito"
      locationMap = Just (fromMaybe "https://maps.app.goo.gl/6pVYZ2CsbvQfGhAz6" (mapEnv >>= nonEmptyText))
      whatsappCta = Just (fromMaybe "https://wa.me/593995413168?text=Quiero%20inscribirme%20al%20curso" (whatsappEnv >>= nonEmptyText))
      landingUrl = Just (baseUrl <> "/curso/" <> slugVal)
      dawsList = Just ["Logic", "Luna"]
      includesList = Just
        [ "Acceso a grabaciones"
        , "Certificado de participación"
        , "Mentorías"
        , "Grupo de WhatsApp"
        , "Acceso a la plataforma de TDF Records"
        ]
      instructorName = Just "Esteban Muñoz"
      instructorBio = Just "Productor en TDF Records. 10+ años grabando bandas, rap y electrónica."
      instructorAvatar = Just (fromMaybe (baseUrl <> "/assets/esteban-munoz.jpg") (instructorAvatarEnv >>= nonEmptyText))
      sessions =
        [ ("Sábado 1 · Introducción", fromGregorian 2025 12 13)
        , ("Sábado 2 · Grabación", fromGregorian 2025 12 20)
        , ("Sábado 3 · Mezcla", fromGregorian 2025 12 27)
        , ("Sábado 4 · Masterización", fromGregorian 2026 1 3)
        ]
      syllabus =
        [ ("Introducción a la producción musical", ["Conceptos básicos", "Herramientas esenciales"])
        , ("Grabación y captura de audio", ["Técnicas de grabación", "Configuración de micrófonos"])
        , ("Mezcla y edición", ["Ecualización y compresión", "Balance y panoramización"])
        , ("Masterización y publicación", ["Mastering", "Distribución digital"])
        ]
  mCourse <- getBy (Trials.UniqueCourseSlug slugVal)
  courseId <- case mCourse of
    Nothing -> insert Trials.Course
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
      replace cid existing
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
    insert_ Trials.CourseSessionModel
      { Trials.courseSessionModelCourseId = courseId
      , Trials.courseSessionModelLabel = labelTxt
      , Trials.courseSessionModelDate = dayVal
      , Trials.courseSessionModelOrder = Just idx
      }

  deleteWhere [Trials.CourseSyllabusItemCourseId ==. courseId]
  forM_ (zip [1 :: Int ..] syllabus) $ \(idx, (titleTxt, topics)) ->
    insert_ Trials.CourseSyllabusItem
      { Trials.courseSyllabusItemCourseId = courseId
      , Trials.courseSyllabusItemTitle = titleTxt
      , Trials.courseSyllabusItemTopics = topics
      , Trials.courseSyllabusItemOrder = Just idx
      }

seedAcademy :: UTCTime -> SqlPersistT IO ()
seedAcademy now = do
  microId <- ensureMicrocourse "release-readiness" "Release Readiness" (Just summary)
  mapM_ (ensureLesson microId)
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
      update mid
        [ AcademyMicrocourseTitle =. titleTxt
        , AcademyMicrocourseSummary =. summaryTxt
        ]
      pure mid
    Nothing -> do
      now <- liftIO getCurrentTime
      mid <- insert AcademyMicrocourse
        { academyMicrocourseSlug = slug
        , academyMicrocourseTitle = titleTxt
        , academyMicrocourseSummary = summaryTxt
        , academyMicrocourseCreatedAt = now
        }
      pure mid


ensureLesson :: AcademyMicrocourseId -> (Int, Text, Text) -> SqlPersistT IO ()
ensureLesson microId (dayNum, titleTxt, bodyTxt) = do
  void $ upsert
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
    Just _  -> pure ()
    Nothing -> do
      now <- liftIO getCurrentTime
      insertKey key ReferralCode
        { referralCodeOwnerUserId = Nothing
        , referralCodeCreatedAt = now
        }

ensureCohortRow :: Text -> Text -> UTCTime -> UTCTime -> Int -> SqlPersistT IO ()
ensureCohortRow slug titleTxt starts ends seats = do
  mExisting <- getBy (UniqueCohortSlug slug)
  case mExisting of
    Just (Entity cid _) ->
      update cid
        [ CohortTitle =. titleTxt
        , CohortStartsAt =. starts
        , CohortEndsAt =. ends
        , CohortSeatCap =. seats
        ]
    Nothing -> do
      void $ insert Cohort
        { cohortSlug = slug
        , cohortTitle = titleTxt
        , cohortStartsAt = starts
        , cohortEndsAt = ends
        , cohortSeatCap = seats
        }

-- Core staff & RBAC seeds -------------------------------------------------

data StaffSeed = StaffSeed
  { ssName  :: Text
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

seedCoreStaffRoles :: UTCTime -> SqlPersistT IO ()
seedCoreStaffRoles now = do
  liftIO $ putStrLn "Seeding core staff roles..."
  mapM_ (seedStaff now) coreStaffSeeds

seedStaff :: UTCTime -> StaffSeed -> SqlPersistT IO ()
seedStaff now StaffSeed{ssName = nameVal, ssEmail = emailVal, ssRoles = rolesVal} = do
  let normalizedEmail = normalizeEmail emailVal
      cleanName       = T.strip nameVal
  (pid, partyChange) <- ensureStaffParty now cleanName normalizedEmail
  newRoles <- ensureStaffRoles pid rolesVal
  credStatus <- ensureStaffCredential pid normalizedEmail
  logStaffSeed cleanName rolesVal partyChange newRoles credStatus

ensureStaffParty :: UTCTime -> Text -> Text -> SqlPersistT IO (Key Party, PartyChange)
ensureStaffParty now displayName email = do
  mPartyFromCredential <- lookupPartyByCredential email
  mPartyByEmail <- maybe (lookupPartyByEmail email) (const (pure Nothing)) mPartyFromCredential
  mPartyByName <- case mPartyFromCredential <|> mPartyByEmail of
    Just _  -> pure Nothing
    Nothing -> lookupPartyByName displayName
  let chosen = mPartyFromCredential <|> mPartyByEmail <|> mPartyByName
  case chosen of
    Just (Entity pid party) -> do
      let updates = catMaybes
            [ if partyDisplayName party == displayName
                then Nothing
                else Just (PartyDisplayName =. displayName)
            , case partyPrimaryEmail party of
                Nothing -> Just (PartyPrimaryEmail =. Just email)
                Just existing
                  | T.toLower existing == email -> Nothing
                  | otherwise                   -> Just (PartyPrimaryEmail =. Just email)
            ]
      unless (null updates) $ update pid updates
      pure (pid, if null updates then PartyUnchanged else PartyUpdated)
    Nothing -> do
      pid <- insert $ Party
        { partyLegalName        = Nothing
        , partyDisplayName      = displayName
        , partyIsOrg            = False
        , partyTaxId            = Nothing
        , partyPrimaryEmail     = Just email
        , partyPrimaryPhone     = Nothing
        , partyWhatsapp         = Nothing
        , partyInstagram        = Nothing
        , partyEmergencyContact = Nothing
        , partyNotes            = Nothing
        , partyCreatedAt        = now
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
            Nothing    -> pure Nothing
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
      void $ insert UserCredential
        { userCredentialPartyId      = pid
        , userCredentialUsername     = username
        , userCredentialPasswordHash = hashed
        , userCredentialActive       = True
        }
      pure CredentialCreated
    Just (Entity credId cred) -> do
      let updates = catMaybes
            [ if userCredentialPartyId cred == pid
                then Nothing
                else Just (UserCredentialPartyId =. pid)
            , if userCredentialActive cred
                then Nothing
                else Just (UserCredentialActive =. True)
            ]
      unless (null updates) $ update credId updates
      pure $ if null updates then CredentialUnchanged else CredentialUpdated

logStaffSeed
  :: Text
  -> [RoleEnum]
  -> PartyChange
  -> [RoleEnum]
  -> CredentialStatus
  -> SqlPersistT IO ()
logStaffSeed nameTxt rolesList partyChange newRoles credStatus = do
  let roleListTxt   = T.intercalate ", " (map roleSlug rolesList)
      changes       = catMaybes
        [ case partyChange of
            PartyCreated   -> Just "created profile"
            PartyUpdated   -> Just "updated profile"
            PartyUnchanged -> Nothing
        , case credStatus of
            CredentialCreated   -> Just "created credential"
            CredentialUpdated   -> Just "updated credential"
            CredentialUnchanged -> Nothing
        , if null newRoles
            then Nothing
            else Just ("added roles: " <> T.intercalate ", " (map roleSlug newRoles))
        ]
      prefix = case partyChange of
        PartyCreated   -> "Created staff user "
        PartyUpdated   -> "Updated staff user "
        PartyUnchanged -> "Ensured staff user "
      suffix = if null changes then "" else " [" <> T.intercalate "; " changes <> "]"
      msg = prefix <> nameTxt <> " (" <> roleListTxt <> ")" <> suffix
  liftIO $ putStrLn (T.unpack msg)

normalizeEmail :: Text -> Text
normalizeEmail = T.toLower . T.strip

roleSlug :: RoleEnum -> Text
roleSlug Admin         = "admin"
roleSlug Manager       = "manager"
roleSlug StudioManager = "studio-manager"
roleSlug Engineer      = "engineer"
roleSlug Teacher       = "teacher"
roleSlug Reception     = "reception"
roleSlug Accounting    = "accounting"
roleSlug LiveSessionsProducer = "live-sessions-producer"
roleSlug Artist        = "artist"
roleSlug Artista       = "artista"
roleSlug Webmaster     = "webmaster"
roleSlug Promotor      = "promotor"
roleSlug Promoter      = "promoter"
roleSlug Producer      = "producer"
roleSlug Songwriter    = "songwriter"
roleSlug DJ            = "dj"
roleSlug Publicist     = "publicist"
roleSlug TourManager   = "tourmanager"
roleSlug LabelRep      = "labelrep"
roleSlug StageManager  = "stagemanager"
roleSlug RoadCrew      = "roadcrew"
roleSlug Photographer  = "photographer"
roleSlug AandR         = "ar"
roleSlug Student       = "student"
roleSlug Vendor        = "vendor"
roleSlug ReadOnly      = "readonly"
roleSlug Customer      = "customer"
roleSlug Fan           = "fan"
roleSlug Maintenance   = "maintenance"

ensureServiceCatalog
  :: (Text, ServiceKind, PricingModel, Maybe Int, Maybe Int, Text, Maybe Text)
  -> SqlPersistT IO ()
ensureServiceCatalog (nameTxt, kind, pricing, rateCents, taxBps, currencyTxt, billingUnit) = do
  let nameClean = T.strip nameTxt
      currencyClean =
        let c = T.strip currencyTxt
        in if T.null c then "USD" else c
  existing <- selectFirst [ServiceCatalogName ==. nameClean] []
  case existing of
    Just (Entity svcId _) ->
      update svcId
        [ ServiceCatalogKind =. kind
        , ServiceCatalogPricingModel =. pricing
        , ServiceCatalogDefaultRateCents =. rateCents
        , ServiceCatalogTaxBps =. taxBps
        , ServiceCatalogCurrency =. currencyClean
        , ServiceCatalogBillingUnit =. billingUnit
        , ServiceCatalogActive =. True
        ]
    Nothing -> do
      void $ insert ServiceCatalog
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
    Just (Entity tokId _) -> update tokId [ ApiTokenPartyId =. pid, ApiTokenActive =. True, ApiTokenLabel =. label ]
    Nothing -> do
      _ <- insert $ ApiToken token pid label True
      pure ()

ensureCredential :: PartyId -> Text -> Text -> SqlPersistT IO ()
ensureCredential pid uname pwd = do
  hashed <- liftIO (hashPasswordText pwd)
  _ <- upsert (UserCredential pid uname hashed True)
         [ UserCredentialPasswordHash =. hashed
         , UserCredentialActive =. True
         ]
  pure ()

hashPasswordText :: Text -> IO Text
hashPasswordText pwd = do
  let raw = TE.encodeUtf8 pwd
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy raw
  case mHash of
    Nothing   -> fail "Failed to hash password"
    Just hash -> pure (TE.decodeUtf8 hash)

roleLabel :: RoleEnum -> Text
roleLabel = T.pack . show

ensureDropdownOption
  :: UTCTime
  -> (Text, Text, Maybe Text, Maybe Int)
  -> SqlPersistT IO ()
ensureDropdownOption now (categoryKey, valueTxt, mLabel, sortOrder) = do
  let labelValue = fromMaybe valueTxt mLabel
  existing <- selectFirst
    [ ME.DropdownOptionCategory ==. categoryKey
    , ME.DropdownOptionValue ==. valueTxt
    ]
    []
  case existing of
    Just (Entity optionId _) ->
      update optionId
        [ ME.DropdownOptionLabel =. Just labelValue
        , ME.DropdownOptionSortOrder =. sortOrder
        , ME.DropdownOptionActive =. True
        , ME.DropdownOptionUpdatedAt =. now
        ]
    Nothing -> do
      _ <- insert DropdownOption
        { dropdownOptionCategory  = categoryKey
        , dropdownOptionValue     = valueTxt
        , dropdownOptionLabel     = Just labelValue
        , dropdownOptionActive    = True
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
    Nothing -> insert Trials.Subject
      { Trials.subjectName   = name
      , Trials.subjectActive = isActive
      }

ensureSubjectRoomPref :: Key Trials.Subject -> ResourceId -> Int -> SqlPersistT IO ()
ensureSubjectRoomPref subjectId roomId priority = do
  void $
    upsert (Trials.SubjectRoomPreference subjectId roomId priority)
      [ Trials.SubjectRoomPreferencePriority =. priority ]

ensureTeacherSubjectLink :: PartyId -> Key Trials.Subject -> SqlPersistT IO ()
ensureTeacherSubjectLink teacherId subjectId = do
  _ <- insertUnique $ Trials.TeacherSubject
    { Trials.teacherSubjectTeacherId = teacherId
    , Trials.teacherSubjectSubjectId = subjectId
    , Trials.teacherSubjectLevelMin  = Nothing
    , Trials.teacherSubjectLevelMax  = Nothing
    }
  pure ()

ensureTeacherAvailabilitySlot
  :: UTCTime
  -> PartyId
  -> Key Trials.Subject
  -> ResourceId
  -> UTCTime
  -> UTCTime
  -> SqlPersistT IO ()
ensureTeacherAvailabilitySlot createdAt teacherId subjectId roomId startAt endAt = do
  existing <- selectFirst
    [ Trials.TeacherAvailabilityTeacherId ==. teacherId
    , Trials.TeacherAvailabilitySubjectId ==. subjectId
    , Trials.TeacherAvailabilityStartAt   ==. startAt
    , Trials.TeacherAvailabilityEndAt     ==. endAt
    ]
    []
  case existing of
    Just _  -> pure ()
    Nothing -> do
      void $ insert Trials.TeacherAvailability
        { Trials.teacherAvailabilityTeacherId = teacherId
        , Trials.teacherAvailabilitySubjectId = subjectId
        , Trials.teacherAvailabilityRoomId    = roomId
        , Trials.teacherAvailabilityStartAt   = startAt
        , Trials.teacherAvailabilityEndAt     = endAt
        , Trials.teacherAvailabilityNotes     = Nothing
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
      let updates = catMaybes
            [ if ME.assetCategory asset == categoryTxt
                then Nothing
                else Just (ME.AssetCategory =. categoryTxt)
            , case (ME.assetBrand asset, brandTxt) of
                (Nothing, Just brandVal) -> Just (ME.AssetBrand =. Just brandVal)
                _                        -> Nothing
            , case (ME.assetModel asset, modelTxt) of
                (Nothing, Just modelVal) -> Just (ME.AssetModel =. Just modelVal)
                _                        -> Nothing
            , case (ME.assetPhotoUrl asset, mPhotoUrl, resolvedPhoto) of
                (Just current, Just seeded, Nothing) | current == seeded -> Just (ME.AssetPhotoUrl =. Nothing)
                _ -> Nothing
            ]
      unless (null updates) (update assetId updates)
    Nothing -> do
      void $ insert ME.Asset
        { ME.assetName                  = nameTxt
        , ME.assetCategory              = categoryTxt
        , ME.assetBrand                 = brandTxt
        , ME.assetModel                 = modelTxt
        , ME.assetSerialNumber          = Nothing
        , ME.assetPurchaseDate          = Nothing
        , ME.assetPurchasePriceUsdCents = mPriceCents
        , ME.assetCondition             = ME.Good
        , ME.assetStatus                = ME.Active
        , ME.assetLocationId            = Nothing
        , ME.assetOwner                 = "TDF"
        , ME.assetQrCode                = Nothing
        , ME.assetPhotoUrl              = resolvedPhoto
        , ME.assetNotes                 = mDesc
        , ME.assetWarrantyExpires       = Nothing
        , ME.assetMaintenancePolicy     = ME.None
        , ME.assetNextMaintenanceDue    = Nothing
        }
      pure ()

data RentSaleRow = RentSaleRow
  { code             :: Text
  , name             :: Text
  , category         :: Text
  , rent_recommended :: Maybe Double
  , sale_recommended :: Maybe Double
  , source           :: Maybe Text
  , note             :: Maybe Text
  } deriving (Show, Generic)
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
            catClean  = T.toLower (T.strip category)
        mExisting <- selectFirst [ME.AssetName ==. nameClean] []
        assetId <- case mExisting of
          Just (Entity aid _) -> pure aid
          Nothing -> do
            aid <- insert ME.Asset
              { ME.assetName                  = nameClean
              , ME.assetCategory              = catClean
              , ME.assetBrand                 = Nothing
              , ME.assetModel                 = Nothing
              , ME.assetSerialNumber          = Nothing
              , ME.assetPurchaseDate          = Nothing
              , ME.assetPurchasePriceUsdCents = Nothing
              , ME.assetCondition             = ME.Good
              , ME.assetStatus                = ME.Active
              , ME.assetLocationId            = Nothing
              , ME.assetOwner                 = "TDF"
              , ME.assetQrCode                = Nothing
              , ME.assetPhotoUrl              = Nothing
              , ME.assetNotes                 = note
              , ME.assetWarrantyExpires       = Nothing
              , ME.assetMaintenancePolicy     = ME.None
              , ME.assetNextMaintenanceDue    = Nothing
              }
            pure aid
        let upsertListing purpose price maybeMarkup = do
              let priceCents = maybe 0 id price
                  titleTxt   = nameClean
                  mk         = maybe 40 id maybeMarkup
              when (priceCents > 0) $ do
                existing <- getBy (ME.UniqueMarketplaceAsset assetId purpose)
                case existing of
                  Just (Entity lid listing) -> do
                    let updates = catMaybes
                          [ if ME.marketplaceListingPriceUsdCents listing == priceCents
                              then Nothing else Just (ME.MarketplaceListingPriceUsdCents =. priceCents)
                          , if ME.marketplaceListingTitle listing == titleTxt
                              then Nothing else Just (ME.MarketplaceListingTitle =. titleTxt)
                          , if ME.marketplaceListingMarkupPct listing == mk
                              then Nothing else Just (ME.MarketplaceListingMarkupPct =. mk)
                          , if ME.marketplaceListingActive listing
                              then Nothing else Just (ME.MarketplaceListingActive =. True)
                          , Just (ME.MarketplaceListingUpdatedAt =. now)
                          ]
                    unless (null updates) (update lid updates)
                  Nothing -> void $ insert ME.MarketplaceListing
                    { ME.marketplaceListingAssetId       = assetId
                    , ME.marketplaceListingTitle         = titleTxt
                    , ME.marketplaceListingPurpose       = purpose
                    , ME.marketplaceListingPriceUsdCents = priceCents
                    , ME.marketplaceListingMarkupPct     = mk
                    , ME.marketplaceListingCurrency      = "USD"
                    , ME.marketplaceListingActive        = True
                    , ME.marketplaceListingCreatedAt     = now
                    , ME.marketplaceListingUpdatedAt     = now
                    }
        let rentPrice = fmap (\r -> round (r * 1.4 * 100)) rent_recommended
            salePrice = fmap (\s -> round (s * 1.4 * 100)) sale_recommended
        upsertListing "rent" rentPrice (Just 40)
        upsertListing "sale" salePrice (Just 40)
  where
    seedFromStatic now = do
      assets <- selectList [] [Asc ME.AssetName]
      forM_ assets $ \(Entity assetId asset) -> do
        let basePrice = Map.lookup (ME.assetName asset) marketplacePriceCents
                         <|> ME.assetPurchasePriceUsdCents asset
        case basePrice of
          Nothing         -> pure ()
          Just baseCents -> do
            let listingPrice = applyMarketplaceMarkup baseCents
                titleTxt = ME.assetName asset
            existing <- getBy (ME.UniqueMarketplaceAsset assetId "sale")
            case existing of
              Just (Entity listingId listing) -> do
                let updates = catMaybes
                      [ if ME.marketplaceListingPriceUsdCents listing == listingPrice
                          then Nothing else Just (ME.MarketplaceListingPriceUsdCents =. listingPrice)
                      , if ME.marketplaceListingTitle listing == titleTxt
                          then Nothing else Just (ME.MarketplaceListingTitle =. titleTxt)
                      , if ME.marketplaceListingActive listing
                          then Nothing else Just (ME.MarketplaceListingActive =. True)
                      , Just (ME.MarketplaceListingUpdatedAt =. now)
                      ]
                unless (null updates) (update listingId updates)
              Nothing -> do
                void $ insert ME.MarketplaceListing
                  { ME.marketplaceListingAssetId       = assetId
                  , ME.marketplaceListingTitle         = titleTxt
                  , ME.marketplaceListingPurpose       = "sale"
                  , ME.marketplaceListingPriceUsdCents = listingPrice
                  , ME.marketplaceListingMarkupPct     = 25
                  , ME.marketplaceListingCurrency      = "USD"
                  , ME.marketplaceListingActive        = True
                  , ME.marketplaceListingCreatedAt     = now
                  , ME.marketplaceListingUpdatedAt     = now
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
marketplacePriceCents = Map.fromList
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
      sessionEnd   = UTCTime (fromGregorian 2025 11 1) (secondsToDiffTime (19 * 60 * 60))
  existing <- selectFirst
    [ ME.SessionClientPartyRef ==. Just clientName
    , ME.SessionService        ==. "Recording"
    ]
    []
  sessionId <- case existing of
    Just (Entity sid _) -> pure sid
    Nothing -> insert ME.Session
      { ME.sessionBookingRef         = Nothing
      , ME.sessionBandId             = Nothing
      , ME.sessionClientPartyRef     = Just clientName
      , ME.sessionService            = "Recording"
      , ME.sessionStartAt            = sessionStart
      , ME.sessionEndAt              = sessionEnd
      , ME.sessionEngineerRef        = "Seed Engineer"
      , ME.sessionAssistantRef       = Nothing
      , ME.sessionStatus             = ME.InPrep
      , ME.sessionSampleRate         = Just 48000
      , ME.sessionBitDepth           = Just 24
      , ME.sessionDaw                = Just "Pro Tools"
      , ME.sessionSessionFolderDriveId = Nothing
      , ME.sessionNotes              = Just "Seeded session for Holger Qui\x00f1\x00f3nez (inventory demo)"
      }
  ensureHolgerSessionRoom sessionId
  ensureHolgerInputList now sessionId

ensureHolgerSessionRoom :: ME.SessionId -> SqlPersistT IO ()
ensureHolgerSessionRoom sessionId = do
  existing <- selectFirst [ME.SessionRoomSessionId ==. sessionId] []
  case existing of
    Just _  -> pure ()
    Nothing -> do
      mRoom <- selectFirst [ME.RoomName ==. "Studio A"] []
      forM_ mRoom $ \(Entity roomId _) ->
        void $ insertUnique ME.SessionRoom
          { ME.sessionRoomSessionId = sessionId
          , ME.sessionRoomRoomId    = roomId
          }

ensureHolgerInputList :: UTCTime -> ME.SessionId -> SqlPersistT IO ()
ensureHolgerInputList now sessionId = do
  listId    <- ensureInputListRecord sessionId now
  versionId <- ensureInputListVersionRecord listId now
  mapM_ (ensureInputRow versionId) holgerInputSeeds

ensureInputListRecord :: ME.SessionId -> UTCTime -> SqlPersistT IO ME.InputListId
ensureInputListRecord sessionId now = do
  existing <- selectFirst [ME.InputListSessionId ==. sessionId] []
  case existing of
    Just (Entity listId _) -> pure listId
    Nothing -> insert ME.InputList
      { ME.inputListSessionId = sessionId
      , ME.inputListCreatedAt = now
      }

ensureInputListVersionRecord
  :: ME.InputListId
  -> UTCTime
  -> SqlPersistT IO ME.InputListVersionId
ensureInputListVersionRecord listId now = do
  existing <- selectFirst
    [ ME.InputListVersionInputListId ==. listId
    , ME.InputListVersionVersion     ==. 1
    ]
    []
  case existing of
    Just (Entity versionId _) -> pure versionId
    Nothing -> insert ME.InputListVersion
      { ME.inputListVersionInputListId = listId
      , ME.inputListVersionVersion     = 1
      , ME.inputListVersionCreatedAt   = now
      , ME.inputListVersionCreatedByRef = Just "seed"
      , ME.inputListVersionNotes       = Just "Holger Qui\x00f1\x00f3nez session input list"
      }

ensureInputRow :: ME.InputListVersionId -> InputSeed -> SqlPersistT IO ()
ensureInputRow versionId entry = do
  let notesParts = catMaybes
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
  void $ insertUnique ME.InputRow
    { ME.inputRowVersionId         = versionId
    , ME.inputRowChannelNumber     = isChannel entry
    , ME.inputRowTrackName         = Just (isSource entry)
    , ME.inputRowInstrument        = Just (isMic entry)
    , ME.inputRowMicId             = Nothing
    , ME.inputRowStandId           = Nothing
    , ME.inputRowCableId           = Nothing
    , ME.inputRowPreampId          = Nothing
    , ME.inputRowInsertOutboardId  = Nothing
    , ME.inputRowConverterChannel  = Just (isInterface entry)
    , ME.inputRowPhantom           = Nothing
    , ME.inputRowPolarity          = Nothing
    , ME.inputRowHpf               = Nothing
    , ME.inputRowPad               = Nothing
    , ME.inputRowNotes             = notesText
    }

data InputSeed = InputSeed
  { isChannel    :: Int
  , isSource     :: Text
  , isMic        :: Text
  , isMedusa     :: Maybe Text
  , isPreamp     :: Maybe Text
  , isInterface  :: Text
  , isDawChannel :: Int
  , isExtraNotes :: Maybe Text
  }

holgerInputSeeds :: [InputSeed]
holgerInputSeeds =
  [ InputSeed 1  "Kick In"              "AKG D112"              (Just "M1")  (Just "Shelford 1")                   "BMB3 - 01 BAD8 1" 1  Nothing
  , InputSeed 2  "Snare Up"             "Shure SM57"            (Just "M2")  (Just "Shelford 2")                   "BMB3 - 02 BAD8 2" 2  Nothing
  , InputSeed 3  "Snare Down"           "Shure SM57"            (Just "M3")  (Just "Shelford 3")                   "BMB3 - 03 BAD8 3" 3  (Just "Flip en DAW")
  , InputSeed 4  "Hi-Hat"               "Sennheiser MKE600"     (Just "M4")  (Just "Avalon 737sp (suave)")         "BMB3 - 04 BAD8 4" 4  Nothing
  , InputSeed 5  "Tom 1"                "Sennheiser MD421"      (Just "M5")  (Just "UA 2-610 L")                   "BMB3 - 05 BAD8 5" 5  Nothing
  , InputSeed 6  "Tom Floor"            "Sennheiser MD421"      (Just "M6")  (Just "UA 2-610 R")                   "BMB3 - 06 BAD8 6" 6  Nothing
  , InputSeed 7  "OH L"                 "AKG C414 (HC)"         (Just "M7")  (Just "API 512v 1")                   "BMB3 - 07 BAD8 7" 7  Nothing
  , InputSeed 8  "OH R"                 "AKG C414 (HC)"         (Just "M8")  (Just "API 512v 2")                   "BMB3 - 08 BAD8 8" 8  Nothing
  , InputSeed 9  "Bass DI (post)"       "Neve RNDI"             (Just "M9")  (Just "B4-1")                         "BMB3 - 17 B4 1 1" 9  Nothing
  , InputSeed 10 "Bass Mic 1 (cab)"     "AKG D112"              (Just "M10") (Just "B4-1")                         "BMB3 - 18 B4 1 2" 10 Nothing
  , InputSeed 11 "Bass Mic 2 (ataque)"  "Neumann KM184"         (Just "M11") (Just "B4-1")                         "BMB3 - 19 B4 1 3" 11 Nothing
  , InputSeed 12 "Gtr 1"                "Sennheiser e906"       (Just "M12") (Just "B4-1")                         "BMB3 - 20 B4 1 4" 12 Nothing
  , InputSeed 13 "Gtr 1 Ribbon"         "Royer R121"            (Just "M13") (Just "B4-2")                         "BMB3 - 25 B4 2 1" 13 Nothing
  , InputSeed 14 "Gtr 2"                "Sennheiser e906"       (Just "M14") (Just "B4-2")                         "BMB3 - 26 B4 2 2" 14 Nothing
  , InputSeed 15 "Gtr 2 Ribbon"         "Royer R121"            (Just "M15") (Just "B4-2")                         "BMB3 - 27 B4 2 3" 15 Nothing
  , InputSeed 16 "Vox 1"                "Electro-Voice RE20"    (Just "M16") (Just "Chandler Limited REDD.47")      "BMB3 - 09 BAD4 1" 25 Nothing
  , InputSeed 17 "Vox 2"                "Sennheiser e835"       Nothing      (Just "MP8R (Vox2)")                  "MP8R - Vox2"       17 Nothing
  , InputSeed 18 "Vox 3"                "Shure SM58"            Nothing      (Just "MP8R (Vox3)")                  "MP8R - Vox3"       18 Nothing
  , InputSeed 19 "Vox 4"                "Shure SM58"            Nothing      (Just "MP8R (Vox4)")                  "MP8R - Vox4"       19 Nothing
  , InputSeed 20 "KU-100 L"             "Neumann KU-100 L"      Nothing      (Just "MP8R (KU100L)")                "MP8R - KU100L"     20 (Just "Room, frente a banda")
  , InputSeed 21 "KU-100 R"             "Neumann KU-100 R"      Nothing      (Just "MP8R (KU100R)")                "MP8R - KU100R"     21 Nothing
  ]
