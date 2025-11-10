{-# LANGUAGE OverloadedStrings #-}
module TDF.Seed where

import           Control.Monad          (forM, forM_, unless, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Crypto.BCrypt          (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Database.Persist
import           Database.Persist.Sql
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Time              (NominalDiffTime, UTCTime(..), addUTCTime, fromGregorian, getCurrentTime,
                                         secondsToDiffTime)
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
  seedHolgerSession now

  pure ()

slugify :: Text -> Text
slugify = T.toLower . T.replace " " "-"

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
