{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TDF.ServerExtra where

import           Control.Monad              (filterM, unless, when, join, guard)
import           Control.Applicative        ((<|>))
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask, asks)
import           Data.Foldable              (for_)
import           Data.List                  (sortOn)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set                   as Set
import           Data.Bits                  (xor)
import           Data.Char                  (isAlphaNum, isAscii, ord)
import           Data.Word                  (Word64)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Time                  (Day, UTCTime(..), defaultTimeLocale, getCurrentTime, parseTimeM)
import           Data.UUID                  (toText)
import           Data.UUID.V4               (nextRandom)
import           Data.Aeson                 (object, (.:), (.:?), (.=))
import           Data.Aeson.Types           (Parser, parseMaybe, withObject, (.!=))
import qualified Data.Aeson                as A
import qualified Data.Scientific            as Sci
import           Numeric                    (showHex)
import           System.Directory           (copyFile, createDirectoryIfMissing)
import           System.FilePath            ((</>), takeExtension, takeFileName)
import           System.IO                  (hPutStrLn, stderr)
import           Database.Persist        hiding (Active)
import           Database.Persist.Sql       (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant
import           Servant.Multipart          (FileData(..))
import           Web.PathPieces             (PathPiece, fromPathPiece, toPathPiece)

import           TDF.API.Inventory          (InventoryAPI, AssetUploadForm(..))
import           TDF.API.Bands              (BandsAPI)
import           TDF.API.Pipelines          (PipelinesAPI)
import           TDF.API.Rooms              (RoomsAPI, RoomsPublicAPI)
import           TDF.API.Sessions           (SessionsAPI)
import           TDF.API.Services           (ServiceCatalogAPI, ServiceCatalogPublicAPI)
import           TDF.API.Types
import           TDF.Auth                   (AuthedUser(..), ModuleAccess(..), hasModuleAccess)
import           TDF.API.Payments          (PaymentDTO(..), PaymentCreate(..), PaymentsAPI)
import qualified TDF.API.Facebook          as FB
import qualified TDF.API.Instagram         as IG
import           TDF.DB                     (Env(..))
import           TDF.Config                 (assetsRootDir, instagramAppToken, instagramMessagingToken, instagramVerifyToken, resolveConfiguredAppBase, resolveConfiguredAssetsBase)
import           TDF.Services.InstagramMessaging (sendInstagramText)
import           TDF.Services.FacebookMessaging (sendFacebookText)
import           TDF.Models                 (Party(..), Payment(..), PaymentMethod(..))
import qualified TDF.Models                 as M
import           TDF.ModelsExtra
import qualified TDF.ModelsExtra as ME
import           TDF.Pipelines              (canonicalStage, defaultStage, pipelineStages, pipelineTypeSlug, parsePipelineType)
import qualified TDF.Handlers.InputList     as InputList

-- Helpers for simple date parsing (YYYY-MM-DD)
parseDayText :: MonadError ServerError m => Text -> m Day
parseDayText t =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
    Just d  -> pure d
    Nothing -> throwError err400 { errBody = "Invalid date format, expected YYYY-MM-DD" }

parseUTCTimeText :: MonadError ServerError m => Text -> m UTCTime
parseUTCTimeText t =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
    Just d  -> pure (UTCTime d 0)
    Nothing -> throwError err400 { errBody = "Invalid date format, expected YYYY-MM-DD" }
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
  :<|> uploadAssetPhotoH
  :<|> getAssetH
  :<|> patchAssetH
  :<|> deleteAssetH
  :<|> checkoutAssetH
  :<|> checkinAssetH
  :<|> checkoutHistoryH
  :<|> refreshQrH
  :<|> resolveByQrH
  where
    ensureInventoryAccess =
      unless (hasModuleAccess ModuleAdmin user || hasInventoryRole user) $
        throwError err403 { errBody = "Missing required module access" }

    hasInventoryRole u =
      any (`elem` auRoles u) [M.Manager, M.Maintenance]

    listAssets _mq mp mps = do
      ensureInventoryAccess
      let pageNum    = clampPage (fromMaybe 1 mp)
          pageSize'  = clampPageSize (fromMaybe 50 mps)
          pageOffset = (pageNum - 1) * pageSize'
          opts       = [Asc AssetName, LimitTo pageSize', OffsetBy pageOffset]
      entities <- withPool $ selectList ([] :: [Filter Asset]) opts
      totalCount <- withPool $ count ([] :: [Filter Asset])
      pure (mkPage pageNum pageSize' totalCount (map toAssetDTO entities))

    createAssetH req = do
      ensureInventoryAccess
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
          , assetPhotoUrl              = cPhotoUrl req
          , assetNotes                 = Nothing
          , assetWarrantyExpires       = Nothing
          , assetMaintenancePolicy     = None
          , assetNextMaintenanceDue    = Nothing
          }
        getJustEntity newAssetId
      pure (toAssetDTO entity)

    uploadAssetPhotoH AssetUploadForm{..} = do
      ensureInventoryAccess
      Env{envConfig} <- ask
      let assetsBase = resolveConfiguredAssetsBase envConfig
          assetsRoot = assetsRootDir envConfig
          fallbackName = nonEmptyText (fdFileName aufFile)
          requestedName = aufName >>= nonEmptyText
          nameWithExt = applyExtension (requestedName <|> fallbackName) fallbackName
          safeName = sanitizeAssetName nameWithExt
      uuid <- liftIO nextRandom
      let storedName = toText uuid <> "-" <> safeName
          relPath = "inventory/" <> storedName
          targetDir = assetsRoot </> "inventory"
          targetPath = targetDir </> T.unpack storedName
      liftIO $ createDirectoryIfMissing True targetDir
      liftIO $ copyFile (fdPayload aufFile) targetPath
      let publicUrl = buildAssetUrl assetsBase relPath
      pure AssetUploadDTO
        { auFileName = storedName
        , auPath = relPath
        , auPublicUrl = publicUrl
        }

    getAssetH rawId = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      mEntity <- withPool $ getEntity assetKey
      maybe (throwError err404) (pure . toAssetDTO) mEntity

    patchAssetH rawId req = do
      ensureInventoryAccess
      assetKey    <- parseKey @Asset rawId
      locationKey <- traverse (parseKey @Room) (uLocationId req)
      let statusValue = uStatus req >>= parseAssetStatus
      let updates = catMaybes
            [ (AssetName =.) <$> uName req
            , (AssetCategory =.) <$> uCategory req
            , (AssetStatus =.) <$> statusValue
            , fmap (\rid -> AssetLocationId =. Just rid) locationKey
            , fmap (\noteTxt -> AssetNotes =. Just noteTxt) (uNotes req)
            , fmap (\url -> AssetPhotoUrl =. Just url) (uPhotoUrl req)
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
      ensureInventoryAccess
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
      , condition = Just (T.pack (show (assetCondition asset)))
      , brand    = assetBrand asset
      , model    = assetModel asset
      , location = fmap toPathPiece (assetLocationId asset)
      , qrToken  = assetQrCode asset
      , photoUrl = assetPhotoUrl asset
      }

    nonEmptyText txt =
      let trimmed = T.strip txt
      in if T.null trimmed then Nothing else Just trimmed

    applyExtension name fallback =
      let resolved = fromMaybe "upload" name
          extFromFallback =
            case fallback of
              Nothing -> ""
              Just raw -> T.pack (takeExtension (T.unpack raw))
          extFromName = T.pack (takeExtension (T.unpack resolved))
      in if T.null extFromName && not (T.null extFromFallback)
          then resolved <> extFromFallback
          else resolved

    sanitizeAssetName raw =
      let trimmed = T.strip raw
          baseName = T.pack (takeFileName (T.unpack trimmed))
          cleaned = T.map normalizeChar baseName
          stripped = T.dropWhile (== '-') (T.dropWhileEnd (== '-') cleaned)
      in if T.null stripped || stripped == "." || stripped == ".."
          then "upload"
          else stripped

    normalizeChar ch
      | isAscii ch && isAlphaNum ch = ch
      | ch == '.' || ch == '-' || ch == '_' = ch
      | ch == ' ' = '-'
      | otherwise = '-'

    buildAssetUrl assetsBase relPath =
      let base = T.dropWhileEnd (== '/') assetsBase
          path = T.dropWhile (== '/') relPath
      in base <> "/" <> path

    checkoutAssetH rawId req = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      asset <- withPool $ get assetKey
      _ <- maybe (throwError err404) pure asset
      now <- liftIO getCurrentTime
      let targetKind = maybe TargetParty parseTarget (coTargetKind req)
          targetRoomKey = coTargetRoom req >>= parseKeyMaybe @Room
          targetSessionKey = coTargetSession req >>= parseKeyMaybe @ME.Session
          checkedOutBy = T.pack (show (fromSqlKey (auPartyId user)))
      active <- withPool $ selectFirst [AssetCheckoutAssetId ==. assetKey, AssetCheckoutReturnedAt ==. Nothing] [Desc AssetCheckoutCheckedOutAt]
      when (isJust active) $
        throwError err409 { errBody = "Asset already checked out" }
      (mRoom, mSession) <- validateTargets targetKind targetRoomKey targetSessionKey
      recEnt <- withPool $ do
        checkoutId <- insert AssetCheckout
          { assetCheckoutAssetId          = assetKey
          , assetCheckoutTargetKind       = targetKind
          , assetCheckoutTargetSessionId  = mSession
          , assetCheckoutTargetPartyRef   = coTargetParty req
          , assetCheckoutTargetRoomId     = mRoom
          , assetCheckoutCheckedOutByRef  = checkedOutBy
          , assetCheckoutCheckedOutAt     = now
          , assetCheckoutDueAt            = coDueAt req
          , assetCheckoutConditionOut     = coConditionOut req
          , assetCheckoutPhotoDriveFileId = Nothing
          , assetCheckoutReturnedAt       = Nothing
          , assetCheckoutConditionIn      = Nothing
          , assetCheckoutNotes            = coNotes req
          }
        update assetKey [AssetStatus =. Booked]
        getJustEntity checkoutId
      pure (toCheckoutDTO recEnt)

    checkinAssetH rawId req = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      now <- liftIO getCurrentTime
      mOpen <- withPool $ selectFirst [AssetCheckoutAssetId ==. assetKey, AssetCheckoutReturnedAt ==. Nothing] [Desc AssetCheckoutCheckedOutAt]
      case mOpen of
        Nothing -> throwError err404 { errBody = "No active checkout" }
        Just (Entity checkoutId _) -> do
          recEnt <- withPool $ do
            update checkoutId
              [ AssetCheckoutReturnedAt   =. Just now
              , AssetCheckoutConditionIn  =. ciConditionIn req
              , AssetCheckoutNotes        =. ciNotes req
              ]
            update assetKey [AssetStatus =. Active]
            getJustEntity checkoutId
          pure (toCheckoutDTO recEnt)

    checkoutHistoryH rawId = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      recs <- withPool $ selectList [AssetCheckoutAssetId ==. assetKey] [Desc AssetCheckoutCheckedOutAt, LimitTo 50]
      pure (map toCheckoutDTO recs)

    refreshQrH rawId = do
      ensureInventoryAccess
      assetKey <- parseKey @Asset rawId
      token <- liftIO (fmap (T.pack . show) nextRandom)
      Env{envConfig} <- ask
      let qrBase = resolveConfiguredAppBase envConfig <> "/inventario/scan/"
          qrUrl tokenVal = qrBase <> tokenVal
      withPool $ update assetKey [AssetQrCode =. Just token]
      pure AssetQrDTO { qrToken = token, qrUrl = qrUrl token }

    resolveByQrH token = do
      ensureInventoryAccess
      mAsset <- withPool $ selectFirst [AssetQrCode ==. Just token] []
      maybe (throwError err404) (pure . toAssetDTO) mAsset

    parseTarget raw =
      case T.toLower (T.strip raw) of
        "session" -> TargetSession
        "party"   -> TargetParty
        "room"    -> TargetRoom
        _         -> TargetParty

    parseKeyMaybe
      :: forall record.
         PathPiece (Key record)
      => Text
      -> Maybe (Key record)
    parseKeyMaybe t = fromPathPiece t

    validateTargets targetKind mRoom mSession = do
      case targetKind of
        TargetRoom ->
          case mRoom of
            Nothing -> throwError err400 { errBody = "targetRoom required for room checkout" }
            Just _  -> pure (mRoom, Nothing)
        TargetSession ->
          case mSession of
            Nothing -> throwError err400 { errBody = "targetSession required for session checkout" }
            Just _  -> pure (Nothing, mSession)
        TargetParty ->
          pure (Nothing, Nothing)

    toCheckoutDTO (Entity key rec) = AssetCheckoutDTO
      { checkoutId      = toPathPiece key
      , assetId         = toPathPiece (assetCheckoutAssetId rec)
      , targetKind      = T.pack (show (assetCheckoutTargetKind rec))
      , targetSessionId = fmap toPathPiece (assetCheckoutTargetSessionId rec)
      , targetPartyRef  = assetCheckoutTargetPartyRef rec
      , targetRoomId    = fmap toPathPiece (assetCheckoutTargetRoomId rec)
      , checkedOutBy    = assetCheckoutCheckedOutByRef rec
      , checkedOutAt    = assetCheckoutCheckedOutAt rec
      , dueAt           = assetCheckoutDueAt rec
      , conditionOut    = assetCheckoutConditionOut rec
      , conditionIn     = assetCheckoutConditionIn rec
      , returnedAt      = assetCheckoutReturnedAt rec
      , notes           = assetCheckoutNotes rec
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

toRoomDTO :: Entity Room -> RoomDTO
toRoomDTO (Entity key room) = RoomDTO
  { roomId    = toPathPiece key
  , rName     = roomName room
  , rBookable = roomIsBookable room
  }

roomsPublicServer
  :: ( MonadReader Env m
     , MonadIO m
     )
  => ServerT RoomsPublicAPI m
roomsPublicServer = do
  entities <- withPool $ selectList
    [RoomIsBookable ==. True]
    [Asc RoomName]
  pure (map toRoomDTO entities)

serviceCatalogPublicServer
  :: ( MonadReader Env m
     , MonadIO m
     )
  => ServerT ServiceCatalogPublicAPI m
serviceCatalogPublicServer = do
  entities <- withPool $ selectList [M.ServiceCatalogActive ==. True] []
  let sorted = sortOn serviceCatalogSortKey entities
  pure (map serviceCatalogToDTO sorted)
  where
    serviceCatalogSortKey (Entity _ svc) =
      let nameNorm = normalizeServiceName (M.serviceCatalogName svc)
      in ( groupRank (M.serviceCatalogKind svc) nameNorm
         , nameRank nameNorm
         , nameNorm
         )
    groupRank kind nameNorm
      | nameNorm == "podcast" = 0 :: Int
      | otherwise =
          case kind of
            M.Recording       -> 0
            M.Rehearsal       -> 1
            M.Mixing          -> 2
            M.Mastering       -> 3
            M.Classes         -> 4
            M.EventProduction -> 5
    nameRank nameNorm =
      case nameNorm of
        "grabacion de banda"     -> 0 :: Int
        "grabacion de voz"       -> 1
        "podcast"                -> 2
        "ensayo"                 -> 0
        "practica en dj booth"   -> 1
        _                        -> 999
    normalizeServiceName =
      stripDiacritics . T.unwords . T.words . T.toLower . T.strip
    stripDiacritics = T.map replaceChar
      where
        replaceChar c =
          case c of
            'á' -> 'a'
            'é' -> 'e'
            'í' -> 'i'
            'ó' -> 'o'
            'ú' -> 'u'
            'ü' -> 'u'
            'ñ' -> 'n'
            _   -> c

serviceCatalogServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT ServiceCatalogAPI m
serviceCatalogServer user = listH :<|> createH :<|> updateH :<|> deleteH
  where
    listH mIncludeInactive = do
      ensureModule ModuleScheduling user
      let filters = [M.ServiceCatalogActive ==. True | not (fromMaybe False mIncludeInactive)]
      entities <- withPool $ selectList filters [Asc M.ServiceCatalogName]
      pure (map serviceCatalogToDTO entities)

    createH ServiceCatalogCreate{..} = do
      ensureModule ModuleScheduling user
      nameClean <- normalizeName sccName
      when (maybe False (< 0) sccRateCents) $
        throwError err400 { errBody = "La tarifa debe ser mayor o igual a cero" }
      when (maybe False (< 0) sccTaxBps) $
        throwError err400 { errBody = "Impuesto inválido" }
      duplicate <- withPool $ selectFirst [M.ServiceCatalogName ==. nameClean] []
      when (isJust duplicate) $
        throwError err409 { errBody = "Ya existe un servicio con ese nombre" }
      entity <- withPool $ do
        let record = M.ServiceCatalog
              { M.serviceCatalogName = nameClean
              , M.serviceCatalogKind = fromMaybe M.Recording sccKind
              , M.serviceCatalogPricingModel = fromMaybe M.Hourly sccPricingModel
              , M.serviceCatalogDefaultRateCents = sccRateCents
              , M.serviceCatalogTaxBps = sccTaxBps
              , M.serviceCatalogCurrency = normalizeCurrency (fromMaybe "USD" sccCurrency)
              , M.serviceCatalogBillingUnit = normalizeTextMaybe sccBillingUnit
              , M.serviceCatalogActive = fromMaybe True sccActive
              }
        newId <- insert record
        getJustEntity newId
      pure (serviceCatalogToDTO entity)

    updateH rawId ServiceCatalogUpdate{..} = do
      ensureModule ModuleScheduling user
      let svcKey = toSqlKey rawId :: Key M.ServiceCatalog
      let rateCandidate = join scuRateCents
          taxCandidate  = join scuTaxBps
      when (maybe False (< 0) rateCandidate) $
        throwError err400 { errBody = "La tarifa debe ser mayor o igual a cero" }
      when (maybe False (< 0) taxCandidate) $
        throwError err400 { errBody = "Impuesto inválido" }
      let nameClean :: Maybe Text
          nameClean = case scuName of
            Nothing   -> Nothing
            Just nm ->
              let trimmed = T.strip nm
              in if T.null trimmed then Nothing else Just trimmed
      case nameClean of
        Just nm -> do
          conflict <- withPool $ selectFirst
            [ M.ServiceCatalogName ==. nm
            , M.ServiceCatalogId !=. svcKey
            ]
            []
          when (isJust conflict) $
            throwError err409 { errBody = "Ya existe un servicio con ese nombre" }
        Nothing -> pure ()
      updated <- withPool $ do
        mExisting <- getEntity svcKey
        case mExisting of
          Nothing -> pure Nothing
          Just _ -> do
            let billingClean :: Maybe (Maybe Text)
                billingClean = case scuBillingUnit of
                  Nothing -> Nothing
                  Just inner -> case inner of
                    Nothing -> Just Nothing
                    Just val ->
                      let trimmed = T.strip val
                      in Just (if T.null trimmed then Nothing else Just trimmed)
                updates = catMaybes
                  [ (M.ServiceCatalogName =.) <$> nameClean
                  , (M.ServiceCatalogKind =.) <$> scuKind
                  , (M.ServiceCatalogPricingModel =.) <$> scuPricingModel
                  , (M.ServiceCatalogDefaultRateCents =.) <$> scuRateCents
                  , (M.ServiceCatalogTaxBps =.) <$> scuTaxBps
                  , (M.ServiceCatalogCurrency =.) . normalizeCurrency <$> scuCurrency
                  , (M.ServiceCatalogBillingUnit =.) <$> billingClean
                  , (M.ServiceCatalogActive =.) <$> scuActive
                  ]
            unless (null updates) (update svcKey updates)
            getEntity svcKey
      maybe (throwError err404) (pure . serviceCatalogToDTO) updated

    deleteH rawId = do
      ensureModule ModuleScheduling user
      let svcKey = toSqlKey rawId :: Key M.ServiceCatalog
      found <- withPool $ do
        mSvc <- getEntity svcKey
        case mSvc of
          Nothing -> pure False
          Just _ -> update svcKey [M.ServiceCatalogActive =. False] >> pure True
      if found then pure NoContent else throwError err404

    normalizeName txt =
      let trimmed = T.strip txt
      in if T.null trimmed then throwError err400 { errBody = "Nombre requerido" } else pure trimmed

    normalizeCurrency txt =
      let trimmed = T.toUpper (T.strip txt)
      in if T.null trimmed then "USD" else trimmed

    normalizeTextMaybe mTxt = case mTxt of
      Nothing -> Nothing
      Just raw ->
        let trimmed = T.strip raw
        in if T.null trimmed then Nothing else Just trimmed

serviceCatalogToDTO :: Entity M.ServiceCatalog -> ServiceCatalogDTO
serviceCatalogToDTO (Entity key svc) = ServiceCatalogDTO
  { scId           = fromSqlKey key
  , scName         = M.serviceCatalogName svc
  , scKind         = M.serviceCatalogKind svc
  , scPricingModel = M.serviceCatalogPricingModel svc
  , scRateCents    = M.serviceCatalogDefaultRateCents svc
  , scCurrency     = M.serviceCatalogCurrency svc
  , scBillingUnit  = M.serviceCatalogBillingUnit svc
  , scTaxBps       = M.serviceCatalogTaxBps svc
  , scActive       = M.serviceCatalogActive svc
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

-- Basic payments server (manual payouts / honorarios)
paymentsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT PaymentsAPI m
paymentsServer user =
       listPaymentsH
  :<|> createPaymentH
  :<|> getPaymentH
  where
    listPaymentsH mPartyId = do
      ensureModule ModuleAdmin user
      let filt = maybe [] (\pid -> [M.PaymentPartyId ==. toSqlKey pid]) mPartyId
      recs <- withPool $ selectList filt [Desc M.PaymentReceivedAt, LimitTo 200]
      pure (map toPaymentDTO recs)

    createPaymentH PaymentCreate{..} = do
      ensureModule ModuleAdmin user
      paidAt <- parseUTCTimeText pcPaidAt
      now <- liftIO getCurrentTime
      let partyKey   = toSqlKey pcPartyId
          mOrderKey  = toSqlKey <$> pcOrderId
          mInvoiceKey= toSqlKey <$> pcInvoiceId
      ent <- withPool $ do
        payId <- insert Payment
          { paymentInvoiceId   = mInvoiceKey
          , paymentOrderId     = mOrderKey
          , paymentPartyId     = partyKey
          , paymentMethod      = parseMethod pcMethod
          , paymentAmountCents = pcAmountCents
          , paymentReceivedAt  = paidAt
          , paymentReference   = pcReference
          , paymentConcept     = Just pcConcept
          , paymentPeriod      = pcPeriod
          , paymentAttachment  = pcAttachmentUrl
          , paymentCreatedBy   = Just (auPartyId user)
          , paymentCreatedAt   = Just now
          }
        getJustEntity payId
      pure (toPaymentDTO ent)

    getPaymentH pid = do
      ensureModule ModuleAdmin user
      mEnt <- withPool $ getEntity (toSqlKey pid :: Key Payment)
      maybe (throwError err404) (pure . toPaymentDTO) mEnt

    toPaymentDTO (Entity key p) = PaymentDTO
      { payId          = fromSqlKey key
      , payPartyId     = fromSqlKey (paymentPartyId p)
      , payOrderId     = fmap fromSqlKey (paymentOrderId p)
      , payInvoiceId   = fmap fromSqlKey (paymentInvoiceId p)
      , payAmountCents = paymentAmountCents p
      , payCurrency    = "USD"
      , payMethod      = T.pack (show (paymentMethod p))
      , payReference   = paymentReference p
      , payPaidAt      = T.pack (show (paymentReceivedAt p))
      , payConcept     = fromMaybe "" (paymentConcept p)
      , payPeriod      = paymentPeriod p
      , payAttachment  = paymentAttachment p
      }

parseMethod :: Text -> PaymentMethod
parseMethod t =
  case T.toLower (T.strip t) of
    "bank" -> BankTransferM
    "banktransfer" -> BankTransferM
    "transferencia" -> BankTransferM
    "produbanco" -> BankTransferM
    "cash" -> CashM
    "efectivo" -> CashM
    "crypto" -> CryptoM
    _ -> BankTransferM

data MetaChannel = MetaInstagram | MetaFacebook
  deriving (Eq, Show)

metaChannelLabel :: MetaChannel -> Text
metaChannelLabel MetaInstagram = "instagram"
metaChannelLabel MetaFacebook = "facebook"

-- Meta webhook parser: accepts Messenger-style `entry.messaging` and Graph-style
-- `entry.changes[].value` message envelopes used by Instagram/Facebook webhooks.
data IGWebhook = IGWebhook
  { igEntries :: [IGEntry]
  } deriving (Show)

data IGEntry = IGEntry
  { igMessaging :: [IGMessaging]
  , igChanges :: [IGChange]
  } deriving (Show)

data IGActor = IGActor
  { igId :: Text
  } deriving (Show)

data IGMessage = IGMessage
  { igMid    :: Maybe Text
  , igText   :: Maybe Text
  , igIsEcho :: Maybe Bool
  , igReferral :: Maybe IGReferral
  , igAttachments :: Maybe [A.Value]
  } deriving (Show)

data IGReferral = IGReferral
  { igRefAdId        :: Maybe Text
  , igRefAdTitle     :: Maybe Text
  , igRefCampaignId  :: Maybe Text
  , igRefCampaignName :: Maybe Text
  , igRefSourceType  :: Maybe Text
  , igRefSourceId    :: Maybe Text
  } deriving (Show)

data IGMessaging = IGMessaging
  { igSender    :: IGActor
  , igRecipient :: Maybe IGActor
  , igMessage   :: Maybe IGMessage
  , igReferral  :: Maybe IGReferral
  , igTimestamp :: Maybe Int
  } deriving (Show)

data IGChange = IGChange
  { igChangeField :: Maybe Text
  , igChangeValue :: Maybe IGChangeValue
  } deriving (Show)

data IGChangeValue = IGChangeValue
  { igChangeMessage :: Maybe IGMessage
  , igChangeFrom :: Maybe IGChangeActor
  , igChangeTimestamp :: Maybe Int
  , igChangeReferral :: Maybe IGReferral
  } deriving (Show)

data IGChangeActor = IGChangeActor
  { igActorId :: Text
  , igActorName :: Maybe Text
  } deriving (Show)

instance A.FromJSON IGWebhook where
  parseJSON = withObject "IGWebhook" $ \o -> do
    igEntries <- o .:? "entry" .!= []
    pure IGWebhook{..}

instance A.FromJSON IGEntry where
  parseJSON = withObject "IGEntry" $ \o -> do
    igMessaging <- o .:? "messaging" .!= []
    igChanges <- o .:? "changes" .!= []
    pure IGEntry{..}

instance A.FromJSON IGActor where
  parseJSON = withObject "IGActor" $ \o -> do
    igId <- o .: "id"
    pure IGActor{..}

instance A.FromJSON IGMessage where
  parseJSON = withObject "IGMessage" $ \o -> do
    igMid <- o .:? "mid"
    igText <- o .:? "text"
    igIsEcho <- o .:? "is_echo"
    igReferral <- o .:? "referral"
    igAttachments <- o .:? "attachments"
    pure IGMessage{..}

instance A.FromJSON IGReferral where
  parseJSON = withObject "IGReferral" $ \o -> do
    igRefAdId <- o .:? "ad_id"
    igRefAdTitle <- o .:? "ad_title"
    igRefCampaignId <- o .:? "campaign_id"
    igRefCampaignName <- o .:? "campaign_name"
    igRefSourceType <- o .:? "source_type"
    igRefSourceId <- o .:? "source_id"
    pure IGReferral{..}

instance A.FromJSON IGMessaging where
  parseJSON = withObject "IGMessaging" $ \o -> do
    igSender <- o .: "sender"
    igRecipient <- o .:? "recipient"
    igMessage <- o .:? "message"
    igReferral <- o .:? "referral"
    rawTs <- o .:? "timestamp"
    igTimestamp <- parseTimestampMaybe rawTs
    pure IGMessaging{..}

instance A.FromJSON IGChange where
  parseJSON = withObject "IGChange" $ \o -> do
    igChangeField <- o .:? "field"
    igChangeValue <- o .:? "value"
    pure IGChange{..}

instance A.FromJSON IGChangeValue where
  parseJSON = withObject "IGChangeValue" $ \o -> do
    igChangeMessage <- o .:? "message"
    igChangeFrom <- o .:? "from"
    rawTs <- o .:? "timestamp"
    igChangeTimestamp <- parseTimestampMaybe rawTs
    igChangeReferral <- o .:? "referral"
    pure IGChangeValue{..}

instance A.FromJSON IGChangeActor where
  parseJSON = withObject "IGChangeActor" $ \o -> do
    igActorId <- o .: "id"
    igActorName <- o .:? "username"
      <|> o .:? "name"
    pure IGChangeActor{..}

parseTimestampMaybe :: Maybe A.Value -> Parser (Maybe Int)
parseTimestampMaybe Nothing = pure Nothing
parseTimestampMaybe (Just raw) =
  case raw of
    A.Number n ->
      case Sci.toBoundedInteger n of
        Just v -> pure (Just v)
        Nothing -> fail "Invalid timestamp number"
    A.String txt ->
      case reads (T.unpack (T.strip txt)) of
        [(v, "")] -> pure (Just v)
        _ -> fail "Invalid timestamp text"
    _ -> fail "Invalid timestamp type"

data IGInbound = IGInbound
  { igInboundExternalId :: Text
  , igInboundSenderId   :: Text
  , igInboundSenderName :: Maybe Text
  , igInboundText       :: Text
  , igInboundAdExternalId :: Maybe Text
  , igInboundAdName     :: Maybe Text
  , igInboundCampaignExternalId :: Maybe Text
  , igInboundCampaignName :: Maybe Text
  , igInboundMetadata   :: Maybe Text
  } deriving (Show)

extractMetaInbound :: A.Value -> [IGInbound]
extractMetaInbound payload =
  case parseMaybe A.parseJSON payload of
    Nothing -> []
    Just IGWebhook{igEntries} -> concatMap extractEntry igEntries
  where
    extractEntry IGEntry{igMessaging, igChanges} =
      mapMaybe extractMessagingEvent igMessaging <> mapMaybe extractChangeEvent igChanges

    extractMessagingEvent IGMessaging{igSender, igMessage, igReferral = eventReferral, igTimestamp} = do
      IGMessage{igMid, igText, igIsEcho, igReferral = msgReferral, igAttachments} <- igMessage
      buildInbound
        (igId igSender)
        Nothing
        igMid
        igText
        igIsEcho
        (eventReferral <|> msgReferral)
        igAttachments
        igTimestamp

    extractChangeEvent IGChange{igChangeField, igChangeValue} = do
      guard (maybe True (\raw -> T.toCaseFold (T.strip raw) == "messages") igChangeField)
      IGChangeValue{igChangeMessage, igChangeFrom, igChangeTimestamp, igChangeReferral} <- igChangeValue
      IGChangeActor{igActorId, igActorName} <- igChangeFrom
      IGMessage{igMid, igText, igIsEcho, igReferral = msgReferral, igAttachments} <- igChangeMessage
      buildInbound
        igActorId
        igActorName
        igMid
        igText
        igIsEcho
        (igChangeReferral <|> msgReferral)
        igAttachments
        igChangeTimestamp

    buildInbound senderId senderName mMid mText mIsEcho mReferral mAttachments mTs = do
      guard (not (fromMaybe False mIsEcho))
      let (adExt, adName, campExt, campName, refMeta) = toReferralMeta mReferral
          attachmentPairs = case mAttachments of
            Just xs | not (null xs) -> ["attachments" .= xs]
            _ -> []
          senderNamePairs = case senderName of
            Just nm | not (T.null (T.strip nm)) -> ["sender_name" .= nm]
            _ -> []
          metaPairs = refMeta ++ attachmentPairs ++ senderNamePairs
          meta = if null metaPairs
            then Nothing
            else Just (TE.decodeUtf8 (BL.toStrict (A.encode (object metaPairs))))
          rawText = fromMaybe "" mText
          body = if not (T.null (T.strip rawText))
            then rawText
            else if null attachmentPairs
              then ""
              else "[attachment]"
      guard (not (T.null (T.strip body)))
      let fallbackBase = T.intercalate "|"
            [ senderId
            , fromMaybe "" senderName
            , maybe "" (T.pack . show) mTs
            , body
            , fromMaybe "" meta
            ]
          fallbackId = senderId <> "-" <> toHashText fallbackBase
          externalId = fromMaybe fallbackId mMid
      pure IGInbound
        { igInboundExternalId = externalId
        , igInboundSenderId = senderId
        , igInboundSenderName = senderName
        , igInboundText = body
        , igInboundAdExternalId = adExt
        , igInboundAdName = adName
        , igInboundCampaignExternalId = campExt
        , igInboundCampaignName = campName
        , igInboundMetadata = meta
        }

    simpleHash64 = T.foldl' step (14695981039346656037 :: Word64)
      where
        step h c = (h `xor` fromIntegral (ord c)) * 1099511628211

    toHashText txt =
      let hexTxt = T.pack (showHex (simpleHash64 txt) "")
      in T.justifyRight 16 '0' hexTxt

    toReferralMeta Nothing = (Nothing, Nothing, Nothing, Nothing, [])
    toReferralMeta (Just IGReferral{..}) =
      let adExt = igRefAdId <|> igRefSourceId
          adName = igRefAdTitle
          campExt = igRefCampaignId
          campName = igRefCampaignName
          metaPairs =
            [ "ad_id" .= igRefAdId
            , "ad_title" .= igRefAdTitle
            , "campaign_id" .= igRefCampaignId
            , "campaign_name" .= igRefCampaignName
            , "source_type" .= igRefSourceType
            , "source_id" .= igRefSourceId
            ]
      in (adExt, adName, campExt, campName, metaPairs)

extractMetaChannel :: A.Value -> Maybe MetaChannel
extractMetaChannel payload =
  join (parseMaybe parseObject payload)
  where
    parseObject = withObject "MetaWebhookObject" $ \o -> do
      mObj <- o .:? "object"
      pure $ case fmap (T.toCaseFold . T.strip) mObj of
        Just "instagram" -> Just MetaInstagram
        Just "page" -> Just MetaFacebook
        Just "facebook" -> Just MetaFacebook
        _ -> Nothing

persistMetaInbound
  :: MonadIO m
  => MetaChannel
  -> UTCTime
  -> [IGInbound]
  -> SqlPersistT m ()
persistMetaInbound channel now incoming =
  for_ incoming $ \IGInbound{..} ->
    case channel of
      MetaInstagram ->
        upsertInstagram igInboundExternalId igInboundSenderId igInboundSenderName igInboundText
          igInboundAdExternalId igInboundAdName igInboundCampaignExternalId igInboundCampaignName igInboundMetadata
      MetaFacebook ->
        upsertFacebook igInboundExternalId igInboundSenderId igInboundSenderName igInboundText
          igInboundAdExternalId igInboundAdName igInboundCampaignExternalId igInboundCampaignName igInboundMetadata
  where
    upsertInstagram externalId senderId senderName body adExt adName campExt campName meta = do
      _ <- upsert (M.InstagramMessage externalId
                    senderId
                    senderName
                    (Just body)
                    "incoming"
                    adExt
                    adName
                    campExt
                    campName
                    meta
                    Nothing
                    Nothing
                    Nothing
                    now)
           [ M.InstagramMessageSenderName =. senderName
           , M.InstagramMessageText =. Just body
           , M.InstagramMessageDirection =. "incoming"
           , M.InstagramMessageAdExternalId =. adExt
           , M.InstagramMessageAdName =. adName
           , M.InstagramMessageCampaignExternalId =. campExt
           , M.InstagramMessageCampaignName =. campName
           , M.InstagramMessageMetadata =. meta
           ]
      pure ()

    upsertFacebook externalId senderId senderName body adExt adName campExt campName meta = do
      _ <- upsert (ME.FacebookMessage externalId
                    senderId
                    senderName
                    (Just body)
                    "incoming"
                    adExt
                    adName
                    campExt
                    campName
                    meta
                    Nothing
                    Nothing
                    Nothing
                    now)
           [ ME.FacebookMessageSenderName =. senderName
           , ME.FacebookMessageText =. Just body
           , ME.FacebookMessageDirection =. "incoming"
           , ME.FacebookMessageAdExternalId =. adExt
           , ME.FacebookMessageAdName =. adName
           , ME.FacebookMessageCampaignExternalId =. campExt
           , ME.FacebookMessageCampaignName =. campName
           , ME.FacebookMessageMetadata =. meta
           ]
      pure ()

verifyMetaWebhook
  :: ( MonadReader Env m
     , MonadError ServerError m
     )
  => Text
  -> Maybe Text
  -> Maybe Text
  -> m Text
verifyMetaWebhook platformLabel mToken mChallenge = do
  Env{envConfig} <- ask
  let expected =
        instagramVerifyToken envConfig
          <|> instagramMessagingToken envConfig
          <|> instagramAppToken envConfig
  case expected of
    Nothing -> throwError err403 { errBody = "Meta verify token not configured" }
    Just token ->
      case mToken of
        Just provided | provided == token -> pure (fromMaybe "" mChallenge)
        _ -> throwError err403 { errBody = BL8.pack ("Meta verify token mismatch for " <> T.unpack platformLabel) }

instagramWebhookServer
  :: ( MonadIO m
     , MonadReader Env m
     , MonadError ServerError m
     )
  => ServerT IG.InstagramWebhookAPI m
instagramWebhookServer =
       verifyWebhook
  :<|> handleWebhook
  where
    verifyWebhook _ mToken mChallenge = verifyMetaWebhook "instagram" mToken mChallenge

    handleWebhook payload = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      let incoming = extractMetaInbound payload
          channel = fromMaybe MetaInstagram (extractMetaChannel payload)
      liftIO $ do
        hPutStrLn stderr ("[" <> T.unpack (metaChannelLabel channel) <> "] received webhook payload")
        BL8.hPutStrLn stderr (A.encode payload)
        flip runSqlPool envPool (persistMetaInbound channel now incoming)
      pure NoContent

facebookWebhookServer
  :: ( MonadIO m
     , MonadReader Env m
     , MonadError ServerError m
     )
  => ServerT FB.FacebookWebhookAPI m
facebookWebhookServer =
       verifyWebhook
  :<|> handleWebhook
  where
    verifyWebhook _ mToken mChallenge = verifyMetaWebhook "facebook" mToken mChallenge

    handleWebhook payload = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      let incoming = extractMetaInbound payload
          channel = fromMaybe MetaFacebook (extractMetaChannel payload)
      liftIO $ do
        hPutStrLn stderr ("[" <> T.unpack (metaChannelLabel channel) <> "] received webhook payload")
        BL8.hPutStrLn stderr (A.encode payload)
        flip runSqlPool envPool (persistMetaInbound channel now incoming)
      pure NoContent

instagramServer
  :: ( MonadIO m
     , MonadReader Env m
     , MonadError ServerError m
     )
  => ServerT IG.InstagramAPI m
instagramServer =
       handleReply
  :<|> listMessages
  where
    handleReply req = do
      now <- liftIO getCurrentTime
      Env{..} <- ask
      let recipient = IG.irSenderId req
          body = T.strip (IG.irMessage req)
          mExternalId =
            IG.irExternalId req >>= (\raw -> let trimmed = T.strip raw in if T.null trimmed then Nothing else Just trimmed)
      when (T.null body) $
        throwError err400 { errBody = "Empty message" }
      sendResult <- liftIO $ sendInstagramText envConfig recipient body
      liftIO $ flip runSqlPool envPool $ do
        insert_ (M.InstagramMessage (recipient <> "-out-" <> T.pack (show now))
                  recipient
                  Nothing
                  (Just body)
                  "outgoing"
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  (either Just (const Nothing) sendResult)
                  now)
        for_ mExternalId $ \extId -> do
          let baseFilters =
                [ M.InstagramMessageExternalId ==. extId
                , M.InstagramMessageDirection ==. "incoming"
                , M.InstagramMessageRepliedAt ==. Nothing
                ]
          case sendResult of
            Left err ->
              updateWhere baseFilters
                [ M.InstagramMessageReplyError =. Just err
                ]
            Right _ ->
              updateWhere baseFilters
                [ M.InstagramMessageRepliedAt =. Just now
                , M.InstagramMessageReplyText =. Just body
                , M.InstagramMessageReplyError =. Nothing
                ]
      case sendResult of
        Left err ->
          pure (object ["status" .= ("error" :: Text), "message" .= err])
        Right responseBody ->
          pure (object ["status" .= ("ok" :: Text), "message" .= ("sent" :: Text), "response" .= responseBody])


    listMessages mLimit mDirection mRepliedOnly = do
      Env{..} <- ask
      let limit = normalizeSocialLimit mLimit
      direction <- parseSocialDirectionParam mDirection
      repliedOnly <- parseSocialBoolParam mRepliedOnly
      let filters =
            concat
              [ maybe [] (\dir -> [M.InstagramMessageDirection ==. dir]) direction
              , if repliedOnly then [M.InstagramMessageRepliedAt !=. Nothing] else []
              ]
      rows <- liftIO $
        flip runSqlPool envPool $
          selectList filters [Desc M.InstagramMessageCreatedAt, LimitTo limit]
      let toObj (Entity _ m) = object
            [ "externalId" .= M.instagramMessageExternalId m
            , "senderId"   .= M.instagramMessageSenderId m
            , "senderName" .= M.instagramMessageSenderName m
            , "text"       .= M.instagramMessageText m
            , "direction"  .= M.instagramMessageDirection m
            , "repliedAt"  .= M.instagramMessageRepliedAt m
            , "replyText"  .= M.instagramMessageReplyText m
            , "replyError" .= M.instagramMessageReplyError m
            , "createdAt"  .= M.instagramMessageCreatedAt m
            ]
      pure (A.toJSON (map toObj rows))

facebookServer
  :: ( MonadIO m
     , MonadReader Env m
     , MonadError ServerError m
     )
  => ServerT FB.FacebookAPI m
facebookServer =
       handleReply
  :<|> listMessages
  where
    handleReply req = do
      now <- liftIO getCurrentTime
      Env{..} <- ask
      let recipient = FB.frSenderId req
          body = T.strip (FB.frMessage req)
          mExternalId =
            FB.frExternalId req >>= (\raw -> let trimmed = T.strip raw in if T.null trimmed then Nothing else Just trimmed)
      when (T.null body) $
        throwError err400 { errBody = "Empty message" }
      sendResult <- liftIO $ sendFacebookText envConfig recipient body
      liftIO $ flip runSqlPool envPool $ do
        insert_ (ME.FacebookMessage (recipient <> "-out-" <> T.pack (show now))
                  recipient
                  Nothing
                  (Just body)
                  "outgoing"
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  (either Just (const Nothing) sendResult)
                  now)
        for_ mExternalId $ \extId -> do
          let baseFilters =
                [ ME.FacebookMessageExternalId ==. extId
                , ME.FacebookMessageDirection ==. "incoming"
                , ME.FacebookMessageRepliedAt ==. Nothing
                ]
          case sendResult of
            Left err ->
              updateWhere baseFilters
                [ ME.FacebookMessageReplyError =. Just err
                ]
            Right _ ->
              updateWhere baseFilters
                [ ME.FacebookMessageRepliedAt =. Just now
                , ME.FacebookMessageReplyText =. Just body
                , ME.FacebookMessageReplyError =. Nothing
                ]
      case sendResult of
        Left err ->
          pure (object ["status" .= ("error" :: Text), "message" .= err])
        Right responseBody ->
          pure (object ["status" .= ("ok" :: Text), "message" .= ("sent" :: Text), "response" .= responseBody])

    listMessages mLimit mDirection mRepliedOnly = do
      Env{..} <- ask
      let limit = normalizeSocialLimit mLimit
      direction <- parseSocialDirectionParam mDirection
      repliedOnly <- parseSocialBoolParam mRepliedOnly
      let filters =
            concat
              [ maybe [] (\dir -> [ME.FacebookMessageDirection ==. dir]) direction
              , if repliedOnly then [ME.FacebookMessageRepliedAt !=. Nothing] else []
              ]
      rows <- liftIO $
        flip runSqlPool envPool $
          selectList filters [Desc ME.FacebookMessageCreatedAt, LimitTo limit]
      let toObj (Entity _ m) = object
            [ "externalId" .= ME.facebookMessageExternalId m
            , "senderId"   .= ME.facebookMessageSenderId m
            , "senderName" .= ME.facebookMessageSenderName m
            , "text"       .= ME.facebookMessageText m
            , "direction"  .= ME.facebookMessageDirection m
            , "repliedAt"  .= ME.facebookMessageRepliedAt m
            , "replyText"  .= ME.facebookMessageReplyText m
            , "replyError" .= ME.facebookMessageReplyError m
            , "createdAt"  .= ME.facebookMessageCreatedAt m
            ]
      pure (A.toJSON (map toObj rows))


normalizeSocialLimit :: Maybe Int -> Int
normalizeSocialLimit = max 1 . min 200 . fromMaybe 100

parseSocialBoolParam :: MonadError ServerError m => Maybe Text -> m Bool
parseSocialBoolParam Nothing = pure False
parseSocialBoolParam (Just raw) =
  case T.toCaseFold (T.strip raw) of
    "true" -> pure True
    "1" -> pure True
    "yes" -> pure True
    "false" -> pure False
    "0" -> pure False
    "no" -> pure False
    "" -> pure False
    _ -> throwError err400 { errBody = "Invalid repliedOnly value" }

parseSocialDirectionParam :: MonadError ServerError m => Maybe Text -> m (Maybe Text)
parseSocialDirectionParam Nothing = pure Nothing
parseSocialDirectionParam (Just raw) =
  case T.toCaseFold (T.strip raw) of
    "" -> pure Nothing
    "all" -> pure Nothing
    "incoming" -> pure (Just "incoming")
    "outgoing" -> pure (Just "outgoing")
    _ -> throwError err400 { errBody = "Invalid direction value" }

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
