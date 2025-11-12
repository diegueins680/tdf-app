{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TDF.ServerExtra where

import           Control.Monad              (filterM, unless, when)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Data.Foldable              (for_)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.ByteString.Lazy       as BL
import           Data.Time                  (getCurrentTime)
import           Database.Persist        hiding (Active)
import           Database.Persist.Sql       (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant
import           Web.PathPieces             (PathPiece, fromPathPiece, toPathPiece)

import           TDF.API.Inventory          (InventoryAPI)
import           TDF.API.Bands              (BandsAPI)
import           TDF.API.Pipelines          (PipelinesAPI)
import           TDF.API.Rooms              (RoomsAPI)
import           TDF.API.Sessions           (SessionsAPI)
import           TDF.API.Types
import           TDF.Auth                   (AuthedUser, ModuleAccess(..), hasModuleAccess)
import           TDF.DB                     (Env(..))
import           TDF.Models                 (Party(..))
import qualified TDF.Models                 as M
import           TDF.ModelsExtra
import qualified TDF.ModelsExtra as ME
import           TDF.Pipelines              (canonicalStage, defaultStage, pipelineStages, pipelineTypeSlug, parsePipelineType)
import qualified TDF.Handlers.InputList     as InputList

inventoryServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT InventoryAPI m
inventoryServer user =
       listAssets
  :<|> createAssetH
  :<|> getAssetH
  :<|> patchAssetH
  :<|> deleteAssetH
  where
    listAssets _mq mp mps = do
      ensureModule ModuleAdmin user
      let pageNum    = clampPage (fromMaybe 1 mp)
          pageSize'  = clampPageSize (fromMaybe 50 mps)
          pageOffset = (pageNum - 1) * pageSize'
          opts       = [Asc AssetName, LimitTo pageSize', OffsetBy pageOffset]
      entities <- withPool $ selectList ([] :: [Filter Asset]) opts
      totalCount <- withPool $ count ([] :: [Filter Asset])
      pure (mkPage pageNum pageSize' totalCount (map toAssetDTO entities))

    createAssetH req = do
      ensureModule ModuleAdmin user
      entity <- withPool $ do
        newAssetId <- insert Asset
          { assetName                  = cName req
          , assetCategory              = cCategory req
          , assetBrand                 = Nothing
          , assetModel                 = Nothing
          , assetSerialNumber          = Nothing
          , assetPurchaseDate          = Nothing
          , assetPurchasePriceUsdCents = Nothing
          , assetCondition             = Good
          , assetStatus                = Active
          , assetLocationId            = Nothing
          , assetOwner                 = "TDF"
          , assetQrCode                = Nothing
          , assetPhotoUrl              = Nothing
          , assetNotes                 = Nothing
          , assetWarrantyExpires       = Nothing
          , assetMaintenancePolicy     = None
          , assetNextMaintenanceDue    = Nothing
          }
        getJustEntity newAssetId
      pure (toAssetDTO entity)

    getAssetH rawId = do
      ensureModule ModuleAdmin user
      assetKey <- parseKey @Asset rawId
      mEntity <- withPool $ getEntity assetKey
      maybe (throwError err404) (pure . toAssetDTO) mEntity

    patchAssetH rawId req = do
      ensureModule ModuleAdmin user
      assetKey    <- parseKey @Asset rawId
      locationKey <- traverse (parseKey @Room) (uLocationId req)
      let statusValue = uStatus req >>= parseAssetStatus
      let updates = catMaybes
            [ (AssetName =.) <$> uName req
            , (AssetCategory =.) <$> uCategory req
            , (AssetStatus =.) <$> statusValue
            , fmap (\rid -> AssetLocationId =. Just rid) locationKey
            , fmap (\noteTxt -> AssetNotes =. Just noteTxt) (uNotes req)
            ]
      result <- withPool $ do
        mEntity <- getEntity assetKey
        case mEntity of
          Nothing -> pure Nothing
          Just _  -> do
            unless (null updates) (update assetKey updates)
            getEntity assetKey
      maybe (throwError err404) (pure . toAssetDTO) result

    deleteAssetH rawId = do
      ensureModule ModuleAdmin user
      assetKey <- parseKey @Asset rawId
      deleted <- withPool $ do
        mAsset <- get assetKey
        case mAsset of
          Nothing -> pure False
          Just _  -> delete assetKey >> pure True
      unless deleted (throwError err404)
      pure NoContent

    toAssetDTO (Entity key asset) = AssetDTO
      { assetId  = toPathPiece key
      , name     = assetName asset
      , category = assetCategory asset
      , status   = T.pack (show (assetStatus asset))
      , location = fmap toPathPiece (assetLocationId asset)
      }

bandsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT BandsAPI m
bandsServer user =
       listBands
  :<|> createBandH
  :<|> bandOptionsH
  :<|> getBandH
  where
    listBands mp mps = do
      ensureModule ModuleCRM user
      let pageNum    = clampPage (fromMaybe 1 mp)
          pageSize'  = clampPageSize (fromMaybe 50 mps)
          pageOffset = (pageNum - 1) * pageSize'
          opts       = [Asc BandName, LimitTo pageSize', OffsetBy pageOffset]
      (bandEntities, totalCount, memberEntities, partyEntities) <- withPool $ do
        bs <- selectList ([] :: [Filter Band]) opts
        totalCount <- count ([] :: [Filter Band])
        members <- if null bs
          then pure []
          else selectList [BandMemberBandId <-. map entityKey bs] [Asc BandMemberId]
        let bandPartyIds = map (bandPartyId . entityVal) bs
            memberPartyIds = map (bandMemberPartyId . entityVal) members
            neededPartyIds = Set.toList (Set.fromList (bandPartyIds ++ memberPartyIds))
        parties <- if null neededPartyIds
          then pure []
          else selectList [M.PartyId <-. neededPartyIds] []
        pure (bs, totalCount, members, parties)
      let memberMap = Map.fromListWith (++)
            [ (bandMemberBandId (entityVal entity), [entity])
            | entity <- memberEntities
            ]
          partyMap = Map.fromList
            [ (entityKey partyEnt, partyEnt)
            | partyEnt <- partyEntities
            ]
          toMembers bandKey = Map.findWithDefault [] bandKey memberMap
          dtos = map
            (\bandEnt -> toBandDTO partyMap bandEnt (toMembers (entityKey bandEnt)))
            bandEntities
      pure (mkPage pageNum pageSize' totalCount dtos)

    createBandH req = do
      ensureModule ModuleCRM user
      let trimmedName = T.strip (bcName req)
          toPartyKey pid = toSqlKey pid :: Key Party
          memberPartyKeys = map (toPartyKey . bmiPartyId) (bcMembers req)
      when (trimmedName == "") $
        throwError err400 { errBody = "Band name is required" }
      nameExists <- withPool $ do
        existing <- selectFirst [BandName ==. trimmedName] []
        pure (isJust existing)
      when nameExists $
        throwError err409 { errBody = "A band with this name already exists" }
      missingMemberKeys <- if null memberPartyKeys
        then pure []
        else withPool $ filterM (fmap isNothing . get) memberPartyKeys
      unless (null missingMemberKeys) $
        throwError err400 { errBody = "One or more band members reference an unknown party" }
      now <- liftIO getCurrentTime
      (bandEntity, memberEntities, partyEntities) <- withPool $ do
        partyId <- insert Party
          { partyLegalName        = Nothing
          , partyDisplayName      = trimmedName
          , partyIsOrg            = True
          , partyTaxId            = Nothing
          , partyPrimaryEmail     = Nothing
          , partyPrimaryPhone     = Nothing
          , partyWhatsapp         = Nothing
          , partyInstagram        = Nothing
          , partyEmergencyContact = Nothing
          , partyNotes            = Nothing
          , partyCreatedAt        = now
          }
        let band = Band
              { bandPartyId      = partyId
              , bandName         = trimmedName
              , bandLabelArtist  = fromMaybe False (bcLabelArtist req)
              , bandPrimaryGenre = bcPrimaryGenre req
              , bandHomeCity     = bcHomeCity req
              , bandPhotoUrl     = bcPhotoUrl req
              , bandContractFlags= bcContractFlags req
              }
        newBandId <- insert band
        for_ (bcMembers req) $ \BandMemberInput{bmiPartyId, bmiRole} -> do
          let memberPartyKey = toSqlKey bmiPartyId :: Key Party
          insert_ BandMember
            { bandMemberBandId     = newBandId
            , bandMemberPartyId    = memberPartyKey
            , bandMemberRoleInBand = bmiRole
            }
        bandEnt <- getJustEntity newBandId
        members <- selectList [BandMemberBandId ==. newBandId] [Asc BandMemberId]
        let requiredPartyIds = Set.toList . Set.fromList $ partyId : map (bandMemberPartyId . entityVal) members
        partyList <- if null requiredPartyIds
          then pure []
          else selectList [M.PartyId <-. requiredPartyIds] []
        pure (bandEnt, members, partyList)
      let partyMap = Map.fromList [ (entityKey p, p) | p <- partyEntities ]
      pure (toBandDTO partyMap bandEntity memberEntities)

    getBandH rawId = do
      ensureModule ModuleCRM user
      bandKey <- parseKey @Band rawId
      result <- withPool $ do
        mBand <- getEntity bandKey
        case mBand of
          Nothing -> pure Nothing
          Just bandEnt -> do
            members <- selectList [BandMemberBandId ==. bandKey] [Asc BandMemberId]
            let requiredPartyIds = Set.toList . Set.fromList $ bandPartyId (entityVal bandEnt) : map (bandMemberPartyId . entityVal) members
            partyList <- if null requiredPartyIds
              then pure []
              else selectList [M.PartyId <-. requiredPartyIds] []
            pure (Just (bandEnt, members, partyList))
      maybe (throwError err404)
        (\(bandEnt, members, parties) ->
          let partyMap = Map.fromList [ (entityKey p, p) | p <- parties ]
          in pure (toBandDTO partyMap bandEnt members))
        result

    bandOptionsH = do
      ensureModule ModuleCRM user
      roleOptions <- withPool $ selectList
        [ ME.DropdownOptionCategory ==. "band-role"
        , ME.DropdownOptionActive ==. True
        ]
        [ Asc ME.DropdownOptionSortOrder
        , Asc ME.DropdownOptionLabel
        , Asc ME.DropdownOptionValue
        ]
      genreOptions <- withPool $ selectList
        [ ME.DropdownOptionCategory ==. "band-genre"
        , ME.DropdownOptionActive ==. True
        ]
        [ Asc ME.DropdownOptionSortOrder
        , Asc ME.DropdownOptionLabel
        , Asc ME.DropdownOptionValue
        ]
      pure BandOptionsDTO
        { roles  = map toOptionDTO roleOptions
        , genres = map toOptionDTO genreOptions
        }

    toOptionDTO (Entity optKey option) = DropdownOptionDTO
      { optionId  = toPathPiece optKey
      , category  = dropdownOptionCategory option
      , value     = dropdownOptionValue option
      , label     = dropdownOptionLabel option
      , active    = dropdownOptionActive option
      , sortOrder = dropdownOptionSortOrder option
      }

sessionsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT SessionsAPI m
sessionsServer user =
       listSessions
  :<|> createSessionH
  :<|> sessionOptionsH
  :<|> getSessionH
  :<|> patchSessionH
  :<|> sessionInputListH
  :<|> sessionInputListPdfH
  where
    listSessions mp mps = do
      ensureModule ModuleScheduling user
      let pageNum    = clampPage (fromMaybe 1 mp)
          pageSize'  = clampPageSize (fromMaybe 50 mps)
          pageOffset = (pageNum - 1) * pageSize'
          opts       = [Desc SessionStartAt, LimitTo pageSize', OffsetBy pageOffset]
      (sessions, totalCount, roomEntities) <- withPool $ do
        ss <- selectList ([] :: [Filter Session]) opts
        totalCount <- count ([] :: [Filter Session])
        rooms <- if null ss
          then pure []
          else selectList [SessionRoomSessionId <-. map entityKey ss] [Asc SessionRoomId]
        pure (ss, totalCount, rooms)
      let roomMap = Map.fromListWith (++)
            [ (sessionRoomSessionId (entityVal entity), [entity])
            | entity <- roomEntities
            ]
          toRooms sessionKey = Map.findWithDefault [] sessionKey roomMap
          dtos = map (\sessionEnt -> toSessionDTO sessionEnt (toRooms (entityKey sessionEnt))) sessions
      pure (mkPage pageNum pageSize' totalCount dtos)

    createSessionH req = do
      ensureModule ModuleScheduling user
      roomKeys <- traverse (parseKey @Room) (scRoomIds req)
      bandKey  <- traverse (parseKey @Band) (scBandId req)
      let statusVal = scStatus req >>= parseSessionStatus
          status'   = fromMaybe InPrep statusVal
      (sessionEnt, rooms) <- withPool $ do
        newSessionId <- insert Session
          { sessionBookingRef           = scBookingRef req
          , sessionBandId               = bandKey
          , sessionClientPartyRef       = scClientPartyRef req
          , sessionService              = scService req
          , sessionStartAt              = scStartAt req
          , sessionEndAt                = scEndAt req
          , sessionEngineerRef          = scEngineerRef req
          , sessionAssistantRef         = scAssistantRef req
          , sessionStatus               = status'
          , sessionSampleRate           = scSampleRate req
          , sessionBitDepth             = scBitDepth req
          , sessionDaw                  = scDaw req
          , sessionSessionFolderDriveId = scSessionFolderDriveId req
          , sessionNotes                = scNotes req
          }
        for_ roomKeys $ \roomKey ->
          insert_ SessionRoom
            { sessionRoomSessionId = newSessionId
            , sessionRoomRoomId    = roomKey
            }
        ent <- getJustEntity newSessionId
        roomEnts <- selectList [SessionRoomSessionId ==. newSessionId] [Asc SessionRoomId]
        pure (ent, roomEnts)
      pure (toSessionDTO sessionEnt rooms)

    getSessionH rawId = do
      ensureModule ModuleScheduling user
      sessionKey <- parseKey @Session rawId
      result <- withPool $ do
        mSession <- getEntity sessionKey
        case mSession of
          Nothing -> pure Nothing
          Just ent -> do
            rooms <- selectList [SessionRoomSessionId ==. sessionKey] [Asc SessionRoomId]
            pure (Just (ent, rooms))
      maybe (throwError err404) (\(ent, rooms) -> pure (toSessionDTO ent rooms)) result

    patchSessionH rawId req = do
      ensureModule ModuleScheduling user
      sessionKey <- parseKey @Session rawId
      bandUpdate <- case suBandId req of
        Nothing          -> pure Nothing
        Just Nothing     -> pure (Just Nothing)
        Just (Just raw)  -> do
          parsed <- parseKey @Band raw
          pure (Just (Just parsed))
      roomKeysUpdate <- case suRoomIds req of
        Nothing     -> pure Nothing
        Just rooms  -> Just <$> traverse (parseKey @Room) rooms
      let statusVal   = suStatus req >>= parseSessionStatus
          updates     = catMaybes
            [ fmap (SessionBookingRef =.)           (suBookingRef req)
            , fmap (SessionBandId =.)               bandUpdate
            , fmap (SessionClientPartyRef =.)       (suClientPartyRef req)
            , fmap (SessionService =.)              (suService req)
            , fmap (SessionStartAt =.)              (suStartAt req)
            , fmap (SessionEndAt =.)                (suEndAt req)
            , fmap (SessionEngineerRef =.)          (suEngineerRef req)
            , fmap (SessionAssistantRef =.)         (suAssistantRef req)
            , fmap (SessionStatus =.)               statusVal
            , fmap (SessionSampleRate =.)           (suSampleRate req)
            , fmap (SessionBitDepth =.)             (suBitDepth req)
            , fmap (SessionDaw =.)                  (suDaw req)
            , fmap (SessionSessionFolderDriveId =.) (suSessionFolderDriveId req)
            , fmap (SessionNotes =.)                (suNotes req)
            ]
      result <- withPool $ do
        mSession <- getEntity sessionKey
        case mSession of
          Nothing -> pure Nothing
          Just _  -> do
            unless (null updates) (update sessionKey updates)
            case roomKeysUpdate of
              Nothing      -> pure ()
              Just roomIds -> do
                deleteWhere [SessionRoomSessionId ==. sessionKey]
                for_ roomIds $ \roomKey ->
                  insert_ SessionRoom
                    { sessionRoomSessionId = sessionKey
                    , sessionRoomRoomId    = roomKey
                    }
            ent <- getJustEntity sessionKey
            rooms <- selectList [SessionRoomSessionId ==. sessionKey] [Asc SessionRoomId]
            pure (Just (ent, rooms))
      maybe (throwError err404) (\(ent, rooms) -> pure (toSessionDTO ent rooms)) result

    sessionInputListH rawId = do
      ensureModule ModuleScheduling user
      sessionKey <- parseKey @Session rawId
      result <- withPool (InputList.fetchSessionInputRowsByKey sessionKey)
      case result of
        Nothing                -> throwError err404
        Just (_session, rows)  -> pure (map toSessionInputRowDTO rows)

    sessionInputListPdfH rawId = do
      ensureModule ModuleScheduling user
      sessionKey <- parseKey @Session rawId
      result <- withPool (InputList.fetchSessionInputRowsByKey sessionKey)
      case result of
        Nothing -> throwError err404
        Just (Entity _ session, rows) -> do
          let title = fromMaybe (sessionService session <> " session") (sessionClientPartyRef session)
              latex = InputList.renderInputListLatex title rows
          pdfResult <- liftIO (InputList.generateInputListPdf latex)
          case pdfResult of
            Left errMsg ->
              throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 errMsg) }
            Right pdf -> do
              let fileName   = InputList.sanitizeFileName title <> ".pdf"
                  disposition = T.concat ["attachment; filename=\"", fileName, "\""]
              pure (addHeader disposition pdf)

    toSessionInputRowDTO (Entity _ row) = SessionInputRow
      { channelNumber    = ME.inputRowChannelNumber row
      , trackName        = ME.inputRowTrackName row
      , instrument       = ME.inputRowInstrument row
      , micId            = fmap toPathPiece (ME.inputRowMicId row)
      , standId          = fmap toPathPiece (ME.inputRowStandId row)
      , cableId          = fmap toPathPiece (ME.inputRowCableId row)
      , preampId         = fmap toPathPiece (ME.inputRowPreampId row)
      , insertOutboardId = fmap toPathPiece (ME.inputRowInsertOutboardId row)
      , converterChannel = ME.inputRowConverterChannel row
      , phantom          = ME.inputRowPhantom row
      , polarity         = ME.inputRowPolarity row
      , hpf              = ME.inputRowHpf row
      , pad              = ME.inputRowPad row
      , notes            = ME.inputRowNotes row
      }

    sessionOptionsH = do
      ensureModule ModuleScheduling user
      (bandEntities, partyEntities) <- withPool $ do
        bs <- selectList ([] :: [Filter Band]) [Asc BandName]
        let partyIds = map (bandPartyId . entityVal) bs
        parties <- if null partyIds
          then pure []
          else selectList [M.PartyId <-. partyIds] []
        pure (bs, parties)
      let partyMap = Map.fromList [ (entityKey p, p) | p <- partyEntities ]
      pure SessionOptionsDTO
        { bands = map (toBandChoice partyMap) bandEntities
        }

    toSessionDTO (Entity key session) rooms = SessionDTO
      { sessionId             = toPathPiece key
      , sStartAt              = sessionStartAt session
      , sEndAt                = sessionEndAt session
      , sStatus               = T.pack (show (sessionStatus session))
      , sBookingRef           = sessionBookingRef session
      , sBandId               = fmap toPathPiece (sessionBandId session)
      , sClientPartyRef       = sessionClientPartyRef session
      , sService              = sessionService session
      , sEngineerRef          = sessionEngineerRef session
      , sAssistantRef         = sessionAssistantRef session
      , sRoomIds              = map (toPathPiece . sessionRoomRoomId . entityVal) rooms
      , sSampleRate           = sessionSampleRate session
      , sBitDepth             = sessionBitDepth session
      , sDaw                  = sessionDaw session
      , sSessionFolderDriveId = sessionSessionFolderDriveId session
      , sNotes                = sessionNotes session
      , sInputListRows        = []
      }

    toBandChoice partyMap (Entity bandKey band) = BandChoiceDTO
      { bandId = toPathPiece bandKey
      , name   = maybe (bandName band) (partyDisplayName . entityVal) (Map.lookup (bandPartyId band) partyMap)
      }

pipelinesServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT PipelinesAPI m
pipelinesServer user rawType =
  case parsePipelineType rawType of
    Nothing   -> notFoundHandlers
    Just kind ->
      (     listCards kind
       :<|> listStages kind
       :<|> createCard kind
       :<|> cardServer kind
      )
  where
    throw404 = throwError err404 { errBody = "Unknown pipeline type" }

    notFoundHandlers =
          throw404
      :<|> throw404
      :<|> (\_ -> throw404)
      :<|> (\_ ->
            throw404
        :<|> (\_ -> throw404)
        :<|> throw404
        )

    listCards kind = do
      ensureModule ModuleScheduling user
      entities <- withPool $ selectList
        [ ME.PipelineCardServiceKind ==. kind ]
        [ Asc ME.PipelineCardSortOrder
        , Asc ME.PipelineCardCreatedAt
        ]
      pure (map toPipelineDTO entities)

    listStages kind = do
      ensureModule ModuleScheduling user
      pure (pipelineStages kind)

    createCard kind req = do
      ensureModule ModuleScheduling user
      stageValue <- resolveStage kind (pccStage req)
      now <- liftIO getCurrentTime
      entity <- withPool $ do
        newId <- insert ME.PipelineCard
          { ME.pipelineCardServiceKind = kind
          , ME.pipelineCardTitle       = pccTitle req
          , ME.pipelineCardArtist      = pccArtist req
          , ME.pipelineCardStage       = stageValue
          , ME.pipelineCardSortOrder   = fromMaybe 0 (pccSortOrder req)
          , ME.pipelineCardNotes       = pccNotes req
          , ME.pipelineCardCreatedAt   = now
          , ME.pipelineCardUpdatedAt   = now
          }
        getJustEntity newId
      pure (toPipelineDTO entity)

    cardServer kind rawId =
          getCard kind rawId
     :<|> updateCard kind rawId
     :<|> deleteCard kind rawId

    getCard kind rawId = do
      ensureModule ModuleScheduling user
      cardKey <- parseKey @ME.PipelineCard rawId
      mEntity <- withPool $ getEntity cardKey
      case mEntity of
        Nothing -> throwError err404
        Just ent ->
          if ME.pipelineCardServiceKind (entityVal ent) /= kind
            then throwError err404
            else pure (toPipelineDTO ent)

    updateCard kind rawId req = do
      ensureModule ModuleScheduling user
      cardKey <- parseKey @ME.PipelineCard rawId
      stageUpdate <- case pcuStage req of
        Nothing   -> pure Nothing
        Just raw  -> Just <$> resolveStage kind (Just raw)
      now <- liftIO getCurrentTime
      result <- withPool $ do
        mEntity <- getEntity cardKey
        case mEntity of
          Nothing -> pure Nothing
          Just (Entity key card)
            | ME.pipelineCardServiceKind card /= kind -> pure Nothing
            | otherwise -> do
                let updates = catMaybes
                      [ fmap (ME.PipelineCardTitle =.) (pcuTitle req)
                      , fmap (ME.PipelineCardArtist =.) (pcuArtist req)
                      , fmap (ME.PipelineCardStage =.) stageUpdate
                      , fmap (ME.PipelineCardSortOrder =.) (pcuSortOrder req)
                      , fmap (ME.PipelineCardNotes =.) (pcuNotes req)
                      ]
                    updates' = if null updates
                      then []
                      else updates ++ [ME.PipelineCardUpdatedAt =. now]
                unless (null updates') (update key updates')
                getEntity key
      maybe (throwError err404) (pure . toPipelineDTO) result

    deleteCard kind rawId = do
      ensureModule ModuleScheduling user
      cardKey <- parseKey @ME.PipelineCard rawId
      deleted <- withPool $ do
        mCard <- get cardKey
        case mCard of
          Nothing -> pure False
          Just card ->
            if ME.pipelineCardServiceKind card /= kind
              then pure False
              else delete cardKey >> pure True
      unless deleted (throwError err404)
      pure NoContent

    resolveStage kind Nothing  = pure (defaultStage kind)
    resolveStage kind (Just raw) =
      case canonicalStage kind raw of
        Nothing   -> throwError err400 { errBody = "Invalid stage for pipeline" }
        Just val  -> pure val

    toPipelineDTO (Entity key card) = PipelineCardDTO
      { pcId        = toPathPiece key
      , pcTitle     = ME.pipelineCardTitle card
      , pcArtist    = ME.pipelineCardArtist card
      , pcType      = pipelineTypeSlug (ME.pipelineCardServiceKind card)
      , pcStage     = ME.pipelineCardStage card
      , pcSortOrder = ME.pipelineCardSortOrder card
      , pcNotes     = ME.pipelineCardNotes card
      }

roomsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT RoomsAPI m
roomsServer user = listRooms :<|> createRoomH :<|> patchRoomH
  where
    listRooms = do
      ensureModule ModuleScheduling user
      entities <- withPool $ selectList ([] :: [Filter Room]) [Asc RoomName]
      pure (map toRoomDTO entities)

    createRoomH req = do
      ensureModule ModuleScheduling user
      entity <- withPool $ do
        newRoomId <- insert Room
          { roomName              = rcName req
          , roomIsBookable        = True
          , roomCapacity          = Nothing
          , roomChannelCount      = Nothing
          , roomDefaultSampleRate = Nothing
          , roomPatchbayNotes     = Nothing
          }
        getJustEntity newRoomId
      pure (toRoomDTO entity)

    patchRoomH rawId req = do
      ensureModule ModuleScheduling user
      roomKey <- parseKey @Room rawId
      let updates = catMaybes
            [ (RoomName =.)       <$> ruName req
            , (RoomIsBookable =.) <$> ruIsBookable req
            ]
      result <- withPool $ do
        mEntity <- getEntity roomKey
        case mEntity of
          Nothing -> pure Nothing
          Just _  -> do
            unless (null updates) (update roomKey updates)
            getEntity roomKey
      maybe (throwError err404) (pure . toRoomDTO) result

    toRoomDTO (Entity key room) = RoomDTO
      { roomId    = toPathPiece key
      , rName     = roomName room
      , rBookable = roomIsBookable room
      }

mkPage :: Int -> Int -> Int -> [a] -> Page a
mkPage current size totalCount values =
  Page { items = values, page = current, pageSize = size, total = totalCount }

clampPage :: Int -> Int
clampPage = max 1

clampPageSize :: Int -> Int
clampPageSize = max 1 . min 100

withPool
  :: (MonadReader Env m, MonadIO m)
  => SqlPersistT IO a
  -> m a
withPool action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

parseKey
  :: forall record m.
     ( PathPiece (Key record)
     , MonadError ServerError m
     )
  => Text
  -> m (Key record)
parseKey raw =
  maybe (throwError err400 { errBody = "Invalid identifier" }) pure (fromPathPiece raw)

parseAssetStatus :: Text -> Maybe AssetStatus
parseAssetStatus = lookupStatus . normalise
  where
    lookupStatus t = case t of
      "active"             -> Just Active
      "booked"             -> Just Booked
      "outformaintenance"  -> Just OutForMaintenance
      "retired"            -> Just Retired
      _                     -> Nothing
    normalise = T.toLower . T.filter (`notElem` [' ', '_'])

parseSessionStatus :: Text -> Maybe SessionStatus
parseSessionStatus = lookupStatus . normalise
  where
    lookupStatus t = case t of
      "inprep"     -> Just InPrep
      "insession"  -> Just InSession
      "break"      -> Just Break
      "editing"    -> Just Editing
      "approved"   -> Just Approved
      "delivered"  -> Just Delivered
      "closed"     -> Just Closed
      _             -> Nothing
    normalise = T.toLower . T.filter (`notElem` [' ', '_'])

ensureModule
  :: (MonadError ServerError m)
  => ModuleAccess
  -> AuthedUser
  -> m ()
ensureModule moduleTag user =
  unless (hasModuleAccess moduleTag user) $
    throwError err403 { errBody = "Missing required module access" }

-- Shared helpers ----------------------------------------------------------

toBandDTO
  :: Map.Map (Key Party) (Entity Party)
  -> Entity Band
  -> [Entity BandMember]
  -> BandDTO
toBandDTO partyMap (Entity key band) members = BandDTO
  { bandId         = toPathPiece key
  , partyId        = fromSqlKey (bandPartyId band)
  , bName          = maybe (bandName band) (partyDisplayName . entityVal) (Map.lookup (bandPartyId band) partyMap)
  , bLabelArtist   = bandLabelArtist band
  , bPrimaryGenre  = bandPrimaryGenre band
  , bHomeCity      = bandHomeCity band
  , bPhotoUrl      = bandPhotoUrl band
  , bContractFlags = bandContractFlags band
  , bMembers       = map (toMemberDTO partyMap) members
  }
  where
    toMemberDTO pMap (Entity memberKey member) = BandMemberDTO
      { bmId         = toPathPiece memberKey
      , bmPartyId    = fromSqlKey (bandMemberPartyId member)
      , bmPartyName  = maybe
          (T.pack . show $ fromSqlKey (bandMemberPartyId member))
          (partyDisplayName . entityVal)
          (Map.lookup (bandMemberPartyId member) pMap)
      , bmRole       = bandMemberRoleInBand member
      }

loadBandForParty
  :: Key Party
  -> SqlPersistT IO (Maybe BandDTO)
loadBandForParty partyKey = do
  mBand <- selectFirst [BandPartyId ==. partyKey] []
  case mBand of
    Nothing -> pure Nothing
    Just bandEnt -> do
      let bandKey = entityKey bandEnt
      members <- selectList [BandMemberBandId ==. bandKey] [Asc BandMemberId]
      let requiredPartyIds = Set.toList . Set.fromList
            $ bandPartyId (entityVal bandEnt)
            : map (bandMemberPartyId . entityVal) members
      partyList <- if null requiredPartyIds
        then pure []
        else selectList [M.PartyId <-. requiredPartyIds] []
      let partyMap = Map.fromList [ (entityKey p, p) | p <- partyList ]
      pure (Just (toBandDTO partyMap bandEnt members))
