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
import           GHC.Generics           (Generic)
import           TDF.Models
import           TDF.ModelsExtra        (DropdownOption(..))
import qualified TDF.ModelsExtra       as ME
import qualified TDF.Trials.Models     as Trials
import           TDF.Pipelines          (canonicalStage, defaultStage)

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
  let svc name kind pm rate tax = ServiceCatalog name kind pm rate tax True
  _ <- insertUnique (svc "Recording" Recording Hourly Nothing (Just 1200))
  _ <- insertUnique (svc "Mixing" Mixing PerSong Nothing (Just 1200))
  _ <- insertUnique (svc "Mastering" Mastering PerSong Nothing (Just 1200))
  _ <- insertUnique (svc "Rehearsal" Rehearsal Hourly (Just (15*100)) (Just 1200))
  _ <- insertUnique (svc "Classes" Classes Package Nothing (Just 1200))
  _ <- insertUnique (svc "Event Production" EventProduction Quote Nothing (Just 1200))

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
  seedHolgerSession now
  seedAcademy now

  pure ()

slugify :: Text -> Text
slugify = T.toLower . T.replace " " "-"

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

ensureInventoryAsset :: (Text, Maybe Text, Maybe Text, Text) -> SqlPersistT IO ()
ensureInventoryAsset (nameTxt, brandTxt, modelTxt, categoryTxt) = do
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
        , ME.assetPurchasePriceUsdCents = Nothing
        , ME.assetCondition             = ME.Good
        , ME.assetStatus                = ME.Active
        , ME.assetLocationId            = Nothing
        , ME.assetOwner                 = "TDF"
        , ME.assetQrCode                = Nothing
        , ME.assetPhotoUrl              = Nothing
        , ME.assetNotes                 = Nothing
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
  exists <- doesFileExist "data/inventory_rent_sale.json"
  if not exists
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
        case Map.lookup (ME.assetName asset) marketplacePriceCents of
          Nothing -> pure ()
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

inventorySeeds :: [(Text, Maybe Text, Maybe Text, Text)]
inventorySeeds =
  [ ("AKG D112", Just "AKG", Just "D112", "mic")
  , ("Shure SM57", Just "Shure", Just "SM57", "mic")
  , ("Sennheiser MD421", Just "Sennheiser", Just "MD421", "mic")
  , ("AKG C414", Just "AKG", Just "C414", "mic")
  , ("Electro-Voice RE20", Just "Electro-Voice", Just "RE20", "mic")
  , ("Neumann KM184", Just "Neumann", Just "KM184", "mic")
  , ("Royer R121", Just "Royer", Just "R121", "mic")
  , ("Sennheiser e906", Just "Sennheiser", Just "e906", "mic")
  , ("Sennheiser e835", Just "Sennheiser", Just "e835", "mic")
  , ("Sennheiser MKE600", Just "Sennheiser", Just "MKE600", "mic")
  , ("Neumann KU-100", Just "Neumann", Just "KU-100", "mic")
  , ("Neve RNDI", Just "Rupert Neve Designs", Just "RNDI", "di")
  , ("Aguilar ToneHammer", Just "Aguilar", Just "ToneHammer", "di/pre")
  , ("Avalon 737sp", Just "Avalon", Just "VT-737sp", "pre")
  , ("UA 2-610", Just "Universal Audio", Just "2-610", "pre")
  , ("API 512v", Just "API", Just "512v", "pre")
  , ("Chandler Limited REDD.47", Just "Chandler Limited", Just "REDD.47", "pre")
  , ("Burl BAD8", Just "Burl", Just "BAD8", "converter-ad")
  , ("Burl B4", Just "Burl", Just "B4", "pre/converter")
  , ("RedNet MP8R", Just "Focusrite", Just "MP8R", "pre/dante")
  , ("Red 8Pre", Just "Focusrite", Just "Red 8Pre", "interface")
  , ("PSM-900", Just "Shure", Just "PSM-900", "iem")
  , ("Shure SM58", Just "Shure", Just "SM58", "mic")
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
