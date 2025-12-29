{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TDF.ServerInternships where

import           Control.Applicative        ((<|>))
import           Control.Monad              (unless, when)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Data.Int                   (Int64)
import           Data.List                  (nub)
import           Data.Maybe                 (catMaybes, fromMaybe, isJust)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (diffUTCTime, getCurrentTime)
import           Database.Persist           (Entity(..), Key, SelectOpt(..), delete, getBy, getEntity, getJustEntity, insert, selectFirst, selectList, update, (==.), (=.), (<-.))
import           Database.Persist.Sql       (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant
import           Web.PathPieces             (PathPiece, fromPathPiece, toPathPiece)

import           TDF.API.Internships        (InternshipsAPI)
import           TDF.API.Types
import           TDF.Auth                   (AuthedUser(..))
import           TDF.DB                     (Env(..))
import qualified TDF.Models                 as M
import qualified TDF.ModelsExtra            as ME

internshipsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT InternshipsAPI m
internshipsServer user =
       listInternsH
  :<|> getProfileH
  :<|> updateProfileH
  :<|> (listProjectsH :<|> createProjectH)
  :<|> updateProjectH
  :<|> (listTasksH :<|> createTaskH)
  :<|> updateTaskH
  :<|> (listTodosH :<|> createTodoH :<|> updateTodoH :<|> deleteTodoH)
  :<|> listTimeEntriesH
  :<|> clockInH
  :<|> clockOutH
  :<|> (listPermissionsH :<|> createPermissionH :<|> updatePermissionH)
  where
    ensureAdmin :: MonadError ServerError m => m ()
    ensureAdmin = unless (isAdmin user) $
      throwError err403 { errBody = "Admin access required" }

    ensureInternAccess :: MonadError ServerError m => m ()
    ensureInternAccess =
      unless (isAdmin user || isIntern user) $
        throwError err403 { errBody = "Intern access required" }

    isAdmin :: AuthedUser -> Bool
    isAdmin AuthedUser{..} = any (`elem` auRoles) [M.Admin, M.Manager, M.StudioManager]

    isIntern :: AuthedUser -> Bool
    isIntern AuthedUser{..} = M.Intern `elem` auRoles

    listInternsH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => m [InternSummaryDTO]
    listInternsH = do
      ensureAdmin
      roles <- withPool $ selectList [M.PartyRoleRole ==. M.Intern, M.PartyRoleActive ==. True] []
      let partyIds = map (M.partyRolePartyId . entityVal) roles
      if null partyIds
        then pure []
        else do
          parties <- withPool $ selectList [M.PartyId <-. partyIds] [Asc M.PartyDisplayName]
          allRoles <- withPool $ selectList [M.PartyRolePartyId <-. partyIds, M.PartyRoleActive ==. True] [Asc M.PartyRoleRole]
          let rolesByParty = Map.fromListWith (++)
                [ (M.partyRolePartyId (entityVal role), [M.partyRoleRole (entityVal role)])
                | role <- allRoles
                ]
          pure
            [ InternSummaryDTO
                { isPartyId = fromSqlKey pid
                , isName    = M.partyDisplayName party
                , isEmail   = M.partyPrimaryEmail party
                , isRoles   = fromMaybe [] (Map.lookup pid rolesByParty)
                }
            | Entity pid party <- parties
            ]

    getProfileH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => m InternProfileDTO
    getProfileH = do
      ensureInternAccess
      now <- liftIO getCurrentTime
      ent <- withPool $ do
        mProfile <- getBy (ME.UniqueInternProfile (auPartyId user))
        case mProfile of
          Just profile -> pure profile
          Nothing -> do
            newId <- insert ME.InternProfile
              { ME.internProfilePartyId  = auPartyId user
              , ME.internProfileStartAt  = Nothing
              , ME.internProfileEndAt    = Nothing
              , ME.internProfileRequiredHours = Nothing
              , ME.internProfileSkills   = Nothing
              , ME.internProfileAreas    = Nothing
              , ME.internProfileCreatedAt = now
              , ME.internProfileUpdatedAt = now
              }
            getJustEntity newId
      pure (toProfileDTO ent)

    updateProfileH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => InternProfileUpdate -> m InternProfileDTO
    updateProfileH InternProfileUpdate{..} = do
      ensureInternAccess
      now <- liftIO getCurrentTime
      let invalidHours = case ipuRequiredHours of
            Just (Just hours) | hours < 0 -> True
            _ -> False
      when invalidHours $
        throwError err400 { errBody = "Required hours must be non-negative" }
      let cleanedSkills = fmap normalizeOptionalText ipuSkills
          cleanedAreas = fmap normalizeOptionalText ipuAreas
          updates = catMaybes
            [ fmap (ME.InternProfileStartAt =.) ipuStartAt
            , fmap (ME.InternProfileEndAt =.) ipuEndAt
            , fmap (ME.InternProfileRequiredHours =.) ipuRequiredHours
            , fmap (ME.InternProfileSkills =.) cleanedSkills
            , fmap (ME.InternProfileAreas =.) cleanedAreas
            ]
      ent <- withPool $ do
        mProfile <- getBy (ME.UniqueInternProfile (auPartyId user))
        case mProfile of
          Just (Entity key _) -> do
            unless (null updates) (update key (updates ++ [ME.InternProfileUpdatedAt =. now]))
            getEntity key
          Nothing -> do
            newId <- insert ME.InternProfile
              { ME.internProfilePartyId  = auPartyId user
              , ME.internProfileStartAt  = fromMaybe Nothing ipuStartAt
              , ME.internProfileEndAt    = fromMaybe Nothing ipuEndAt
              , ME.internProfileRequiredHours = fromMaybe Nothing ipuRequiredHours
              , ME.internProfileSkills   = fromMaybe Nothing cleanedSkills
              , ME.internProfileAreas    = fromMaybe Nothing cleanedAreas
              , ME.internProfileCreatedAt = now
              , ME.internProfileUpdatedAt = now
              }
            getEntity newId
      ent' <- maybe (throwError err404) pure ent
      pure (toProfileDTO ent')

    listProjectsH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => m [InternProjectDTO]
    listProjectsH = do
      ensureInternAccess
      projects <- if isAdmin user
        then withPool $ selectList [] [Desc ME.InternProjectCreatedAt]
        else do
          tasks <- withPool $ selectList [ME.InternTaskAssignedTo ==. Just (auPartyId user)] []
          let projectIds = nub (map (ME.internTaskProjectId . entityVal) tasks)
          if null projectIds
            then pure []
            else withPool $ selectList [ME.InternProjectId <-. projectIds] [Desc ME.InternProjectCreatedAt]
      pure (map toProjectDTO projects)

    createProjectH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => InternProjectCreate -> m InternProjectDTO
    createProjectH InternProjectCreate{..} = do
      ensureAdmin
      now <- liftIO getCurrentTime
      let statusVal = fromMaybe "active" ipcStatus
      ent <- withPool $ do
        newId <- insert ME.InternProject
          { ME.internProjectTitle       = ipcTitle
          , ME.internProjectDescription = ipcDescription
          , ME.internProjectStatus      = statusVal
          , ME.internProjectStartAt     = ipcStartAt
          , ME.internProjectDueAt       = ipcDueAt
          , ME.internProjectCreatedBy   = auPartyId user
          , ME.internProjectCreatedAt   = now
          , ME.internProjectUpdatedAt   = now
          }
        getJustEntity newId
      pure (toProjectDTO ent)

    updateProjectH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => Text -> InternProjectUpdate -> m InternProjectDTO
    updateProjectH rawId InternProjectUpdate{..} = do
      ensureAdmin
      projectKey <- parseKey @ME.InternProject rawId
      now <- liftIO getCurrentTime
      let updates = catMaybes
            [ fmap (ME.InternProjectTitle =.) ipuTitle
            , fmap (ME.InternProjectDescription =.) ipuDescription
            , fmap (ME.InternProjectStatus =.) ipuStatus
            , fmap (ME.InternProjectStartAt =.) ipuStartAt
            , fmap (ME.InternProjectDueAt =.) ipuDueAt
            ]
      result <- withPool $ do
        mEntity <- getEntity projectKey
        case mEntity of
          Nothing -> pure Nothing
          Just _  -> do
            unless (null updates) (update projectKey (updates ++ [ME.InternProjectUpdatedAt =. now]))
            getEntity projectKey
      maybe (throwError err404) (pure . toProjectDTO) result

    listTasksH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => m [InternTaskDTO]
    listTasksH = do
      ensureInternAccess
      let baseFilters = if isAdmin user
            then []
            else [ME.InternTaskAssignedTo ==. Just (auPartyId user)]
      tasks <- withPool $ selectList baseFilters [Desc ME.InternTaskUpdatedAt]
      let projectIds = nub (map (ME.internTaskProjectId . entityVal) tasks)
      projectMap <- loadProjectMap projectIds
      let assignees = catMaybes (map (ME.internTaskAssignedTo . entityVal) tasks)
      partyMap <- loadPartyMap assignees
      pure (map (toTaskDTO projectMap partyMap) tasks)

    createTaskH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => InternTaskCreate -> m InternTaskDTO
    createTaskH InternTaskCreate{..} = do
      ensureAdmin
      projectKey <- parseKey @ME.InternProject itcProjectId
      now <- liftIO getCurrentTime
      mProject <- withPool $ getEntity projectKey
      _ <- maybe (throwError err404) pure mProject
      let assignedKey = fmap toSqlKey itcAssignedTo
      ent <- withPool $ do
        newId <- insert ME.InternTask
          { ME.internTaskProjectId   = projectKey
          , ME.internTaskTitle       = itcTitle
          , ME.internTaskDescription = itcDescription
          , ME.internTaskStatus      = "todo"
          , ME.internTaskProgress    = 0
          , ME.internTaskAssignedTo  = assignedKey
          , ME.internTaskDueAt       = itcDueAt
          , ME.internTaskCreatedBy   = auPartyId user
          , ME.internTaskCreatedAt   = now
          , ME.internTaskUpdatedAt   = now
          }
        getJustEntity newId
      projectMap <- loadProjectMap [projectKey]
      partyMap <- loadPartyMap (maybe [] pure assignedKey)
      pure (toTaskDTO projectMap partyMap ent)

    updateTaskH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => Text -> InternTaskUpdate -> m InternTaskDTO
    updateTaskH rawId InternTaskUpdate{..} = do
      ensureInternAccess
      taskKey <- parseKey @ME.InternTask rawId
      now <- liftIO getCurrentTime
      mEntity <- withPool $ getEntity taskKey
      ent <- maybe (throwError err404) pure mEntity
      let task = entityVal ent
          assignedKey = ME.internTaskAssignedTo task
          isOwner = assignedKey == Just (auPartyId user)
          isAdminUser = isAdmin user
      unless (isAdminUser || isOwner) $
        throwError err403 { errBody = "Only admins or assignees can update tasks" }
      let safeProgress = fmap (clamp 0 100) ituProgress
          adminUpdates =
            [ fmap (ME.InternTaskTitle =.) ituTitle
            , fmap (ME.InternTaskDescription =.) ituDescription
            , fmap (ME.InternTaskAssignedTo =.) (fmap (fmap toSqlKey) ituAssignedTo)
            , fmap (ME.InternTaskDueAt =.) ituDueAt
            ]
          commonUpdates =
            [ fmap (ME.InternTaskStatus =.) ituStatus
            , fmap (ME.InternTaskProgress =.) safeProgress
            ]
          updates =
            if isAdminUser
              then catMaybes (adminUpdates ++ commonUpdates)
              else catMaybes commonUpdates
      result <- withPool $ do
        unless (null updates) (update taskKey (updates ++ [ME.InternTaskUpdatedAt =. now]))
        getEntity taskKey
      entUpdated <- maybe (throwError err404) pure result
      let projectKey = ME.internTaskProjectId (entityVal entUpdated)
          assigned = ME.internTaskAssignedTo (entityVal entUpdated)
      projectMap <- loadProjectMap [projectKey]
      partyMap <- loadPartyMap (maybe [] pure assigned)
      pure (toTaskDTO projectMap partyMap entUpdated)

    listTodosH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => m [InternTodoDTO]
    listTodosH = do
      ensureInternAccess
      rows <- withPool $ selectList [ME.InternTodoOwnerPartyId ==. auPartyId user] [Desc ME.InternTodoCreatedAt]
      pure (map toTodoDTO rows)

    createTodoH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => InternTodoCreate -> m InternTodoDTO
    createTodoH InternTodoCreate{..} = do
      ensureInternAccess
      now <- liftIO getCurrentTime
      ent <- withPool $ do
        newId <- insert ME.InternTodo
          { ME.internTodoOwnerPartyId = auPartyId user
          , ME.internTodoText         = itdcText
          , ME.internTodoDone         = False
          , ME.internTodoCreatedAt    = now
          , ME.internTodoUpdatedAt    = now
          }
        getJustEntity newId
      pure (toTodoDTO ent)

    updateTodoH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => Text -> InternTodoUpdate -> m InternTodoDTO
    updateTodoH rawId InternTodoUpdate{..} = do
      ensureInternAccess
      todoKey <- parseKey @ME.InternTodo rawId
      now <- liftIO getCurrentTime
      mEntity <- withPool $ getEntity todoKey
      ent <- maybe (throwError err404) pure mEntity
      let todo = entityVal ent
      when (ME.internTodoOwnerPartyId todo /= auPartyId user) $
        throwError err403 { errBody = "Cannot edit another user's todo" }
      let updates = catMaybes
            [ fmap (ME.InternTodoText =.) itduText
            , fmap (ME.InternTodoDone =.) itduDone
            ]
      result <- withPool $ do
        unless (null updates) (update todoKey (updates ++ [ME.InternTodoUpdatedAt =. now]))
        getEntity todoKey
      entUpdated <- maybe (throwError err404) pure result
      pure (toTodoDTO entUpdated)

    deleteTodoH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => Text -> m NoContent
    deleteTodoH rawId = do
      ensureInternAccess
      todoKey <- parseKey @ME.InternTodo rawId
      mEntity <- withPool $ getEntity todoKey
      case mEntity of
        Nothing -> throwError err404
        Just (Entity _ todo) -> do
          when (ME.internTodoOwnerPartyId todo /= auPartyId user) $
            throwError err403 { errBody = "Cannot delete another user's todo" }
          withPool $ delete todoKey
          pure NoContent

    listTimeEntriesH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => Maybe Int64 -> m [InternTimeEntryDTO]
    listTimeEntriesH mPartyId = do
      ensureInternAccess
      let partyFilter =
            if isAdmin user
              then maybe [] (\pid -> [ME.InternTimeEntryPartyId ==. toSqlKey pid]) mPartyId
              else [ME.InternTimeEntryPartyId ==. auPartyId user]
      entries <- withPool $ selectList partyFilter [Desc ME.InternTimeEntryClockIn, LimitTo 200]
      let partyIds = nub (map (ME.internTimeEntryPartyId . entityVal) entries)
      partyMap <- loadPartyMap partyIds
      pure (map (toTimeEntryDTO partyMap) entries)

    clockInH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => ClockInRequest -> m InternTimeEntryDTO
    clockInH ClockInRequest{..} = do
      ensureInternAccess
      now <- liftIO getCurrentTime
      let partyId = auPartyId user
      active <- withPool $ selectFirst [ME.InternTimeEntryPartyId ==. partyId, ME.InternTimeEntryClockOut ==. Nothing] [Desc ME.InternTimeEntryClockIn]
      when (isJust active) $
        throwError err409 { errBody = "Already clocked in" }
      ent <- withPool $ do
        newId <- insert ME.InternTimeEntry
          { ME.internTimeEntryPartyId   = partyId
          , ME.internTimeEntryClockIn   = now
          , ME.internTimeEntryClockOut  = Nothing
          , ME.internTimeEntryNotes     = cirNotes
          , ME.internTimeEntryCreatedAt = now
          , ME.internTimeEntryUpdatedAt = now
          }
        getJustEntity newId
      partyMap <- loadPartyMap [partyId]
      pure (toTimeEntryDTO partyMap ent)

    clockOutH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => ClockOutRequest -> m InternTimeEntryDTO
    clockOutH ClockOutRequest{..} = do
      ensureInternAccess
      now <- liftIO getCurrentTime
      let partyId = auPartyId user
      mOpen <- withPool $ selectFirst [ME.InternTimeEntryPartyId ==. partyId, ME.InternTimeEntryClockOut ==. Nothing] [Desc ME.InternTimeEntryClockIn]
      case mOpen of
        Nothing -> throwError err404 { errBody = "No active clock-in" }
        Just (Entity entryId entry) -> do
          let updates = catMaybes
                [ Just (ME.InternTimeEntryClockOut =. Just now)
                , fmap (ME.InternTimeEntryNotes =.) (fmap Just corNotes)
                ]
          withPool $ update entryId (updates ++ [ME.InternTimeEntryUpdatedAt =. now])
          partyMap <- loadPartyMap [partyId]
          pure (toTimeEntryDTO partyMap (Entity entryId entry { ME.internTimeEntryClockOut = Just now, ME.internTimeEntryNotes = corNotes <|> ME.internTimeEntryNotes entry }))

    listPermissionsH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => m [InternPermissionDTO]
    listPermissionsH = do
      ensureInternAccess
      let filt =
            if isAdmin user
              then []
              else [ME.InternPermissionRequestPartyId ==. auPartyId user]
      rows <- withPool $ selectList filt [Desc ME.InternPermissionRequestCreatedAt]
      let partyIds = nub $ concatMap collectParties rows
      partyMap <- loadPartyMap partyIds
      pure (map (toPermissionDTO partyMap) rows)
      where
        collectParties (Entity _ row) =
          catMaybes [Just (ME.internPermissionRequestPartyId row), ME.internPermissionRequestReviewedBy row]

    createPermissionH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => InternPermissionCreate -> m InternPermissionDTO
    createPermissionH InternPermissionCreate{..} = do
      ensureInternAccess
      now <- liftIO getCurrentTime
      let partyId = auPartyId user
      ent <- withPool $ do
        newId <- insert ME.InternPermissionRequest
          { ME.internPermissionRequestPartyId    = partyId
          , ME.internPermissionRequestCategory   = ipcCategory
          , ME.internPermissionRequestReason     = ipcReason
          , ME.internPermissionRequestStartAt    = ipcStartAt
          , ME.internPermissionRequestEndAt      = ipcEndAt
          , ME.internPermissionRequestStatus     = "pending"
          , ME.internPermissionRequestReviewedBy = Nothing
          , ME.internPermissionRequestReviewedAt = Nothing
          , ME.internPermissionRequestDecisionNotes = Nothing
          , ME.internPermissionRequestCreatedAt  = now
          , ME.internPermissionRequestUpdatedAt  = now
          }
        getJustEntity newId
      partyMap <- loadPartyMap [partyId]
      pure (toPermissionDTO partyMap ent)

    updatePermissionH :: (MonadReader Env m, MonadIO m, MonadError ServerError m) => Text -> InternPermissionUpdate -> m InternPermissionDTO
    updatePermissionH rawId InternPermissionUpdate{..} = do
      ensureAdmin
      permKey <- parseKey @ME.InternPermissionRequest rawId
      now <- liftIO getCurrentTime
      let updates = catMaybes
            [ fmap (ME.InternPermissionRequestStatus =.) ipuStatus
            , fmap (ME.InternPermissionRequestDecisionNotes =.) ipuDecisionNotes
            ]
          reviewUpdates =
            if ipuStatus /= Nothing
              then [ ME.InternPermissionRequestReviewedBy =. Just (auPartyId user)
                   , ME.InternPermissionRequestReviewedAt =. Just now
                   ]
              else []
      result <- withPool $ do
        mEntity <- getEntity permKey
        case mEntity of
          Nothing -> pure Nothing
          Just _  -> do
            unless (null updates && null reviewUpdates) $
              update permKey (updates ++ reviewUpdates ++ [ME.InternPermissionRequestUpdatedAt =. now])
            getEntity permKey
      case result of
        Nothing -> throwError err404
        Just ent -> do
          let partyIds = catMaybes
                [ Just (ME.internPermissionRequestPartyId (entityVal ent))
                , ME.internPermissionRequestReviewedBy (entityVal ent)
                ]
          partyMap <- loadPartyMap partyIds
          pure (toPermissionDTO partyMap ent)

    toProjectDTO :: Entity ME.InternProject -> InternProjectDTO
    toProjectDTO (Entity key project) = InternProjectDTO
      { ipId          = toPathPiece key
      , ipTitle       = ME.internProjectTitle project
      , ipDescription = ME.internProjectDescription project
      , ipStatus      = ME.internProjectStatus project
      , ipStartAt     = ME.internProjectStartAt project
      , ipDueAt       = ME.internProjectDueAt project
      , ipCreatedAt   = ME.internProjectCreatedAt project
      , ipUpdatedAt   = ME.internProjectUpdatedAt project
      }

    toTaskDTO
      :: Map.Map (Key ME.InternProject) Text
      -> Map.Map M.PartyId Text
      -> Entity ME.InternTask
      -> InternTaskDTO
    toTaskDTO projectMap partyMap (Entity key task) = InternTaskDTO
      { itId           = toPathPiece key
      , itProjectId    = toPathPiece (ME.internTaskProjectId task)
      , itProjectName  = fromMaybe "Proyecto" (Map.lookup (ME.internTaskProjectId task) projectMap)
      , itTitle        = ME.internTaskTitle task
      , itDescription  = ME.internTaskDescription task
      , itStatus       = ME.internTaskStatus task
      , itProgress     = ME.internTaskProgress task
      , itAssignedTo   = fmap fromSqlKey (ME.internTaskAssignedTo task)
      , itAssignedName = ME.internTaskAssignedTo task >>= (`Map.lookup` partyMap)
      , itDueAt        = ME.internTaskDueAt task
      , itCreatedAt    = ME.internTaskCreatedAt task
      , itUpdatedAt    = ME.internTaskUpdatedAt task
      }

    toTodoDTO :: Entity ME.InternTodo -> InternTodoDTO
    toTodoDTO (Entity key todo) = InternTodoDTO
      { itdId        = toPathPiece key
      , itdText      = ME.internTodoText todo
      , itdDone      = ME.internTodoDone todo
      , itdCreatedAt = ME.internTodoCreatedAt todo
      , itdUpdatedAt = ME.internTodoUpdatedAt todo
      }

    toTimeEntryDTO :: Map.Map M.PartyId Text -> Entity ME.InternTimeEntry -> InternTimeEntryDTO
    toTimeEntryDTO partyMap (Entity key entry) =
      let duration = do
            out <- ME.internTimeEntryClockOut entry
            let rawMinutes = floor (realToFrac (diffUTCTime out (ME.internTimeEntryClockIn entry)) / 60 :: Double)
            pure (max 0 rawMinutes)
          partyName = fromMaybe "Pasante" (Map.lookup (ME.internTimeEntryPartyId entry) partyMap)
      in InternTimeEntryDTO
        { iteId              = toPathPiece key
        , itePartyId         = fromSqlKey (ME.internTimeEntryPartyId entry)
        , itePartyName       = partyName
        , iteClockIn         = ME.internTimeEntryClockIn entry
        , iteClockOut        = ME.internTimeEntryClockOut entry
        , iteDurationMinutes = duration
        , iteNotes           = ME.internTimeEntryNotes entry
        }

    toPermissionDTO :: Map.Map M.PartyId Text -> Entity ME.InternPermissionRequest -> InternPermissionDTO
    toPermissionDTO partyMap (Entity key row) =
      let partyName = fromMaybe "Pasante" (Map.lookup (ME.internPermissionRequestPartyId row) partyMap)
          reviewerName = ME.internPermissionRequestReviewedBy row >>= (`Map.lookup` partyMap)
      in InternPermissionDTO
        { iprId              = toPathPiece key
        , iprPartyId         = fromSqlKey (ME.internPermissionRequestPartyId row)
        , iprPartyName       = partyName
        , iprCategory        = ME.internPermissionRequestCategory row
        , iprReason          = ME.internPermissionRequestReason row
        , iprStartAt         = ME.internPermissionRequestStartAt row
        , iprEndAt           = ME.internPermissionRequestEndAt row
        , iprStatus          = ME.internPermissionRequestStatus row
        , iprReviewedBy      = fmap fromSqlKey (ME.internPermissionRequestReviewedBy row)
        , iprReviewedByName  = reviewerName
        , iprReviewedAt      = ME.internPermissionRequestReviewedAt row
        , iprDecisionNotes   = ME.internPermissionRequestDecisionNotes row
        , iprCreatedAt       = ME.internPermissionRequestCreatedAt row
        , iprUpdatedAt       = ME.internPermissionRequestUpdatedAt row
        }

    toProfileDTO :: Entity ME.InternProfile -> InternProfileDTO
    toProfileDTO (Entity _ profile) = InternProfileDTO
      { ipPartyId  = fromSqlKey (ME.internProfilePartyId profile)
      , ipStartAt  = ME.internProfileStartAt profile
      , ipEndAt    = ME.internProfileEndAt profile
      , ipRequiredHours = ME.internProfileRequiredHours profile
      , ipSkills   = ME.internProfileSkills profile
      , ipAreas    = ME.internProfileAreas profile
      , ipCreatedAt = ME.internProfileCreatedAt profile
      , ipUpdatedAt = ME.internProfileUpdatedAt profile
      }

    loadPartyMap :: (MonadReader Env m, MonadIO m) => [M.PartyId] -> m (Map.Map M.PartyId Text)
    loadPartyMap partyIds =
      if null partyIds
        then pure Map.empty
        else do
          parties <- withPool $ selectList [M.PartyId <-. partyIds] []
          pure $ Map.fromList
            [ (pid, M.partyDisplayName party)
            | Entity pid party <- parties
            ]

    loadProjectMap :: (MonadReader Env m, MonadIO m) => [Key ME.InternProject] -> m (Map.Map (Key ME.InternProject) Text)
    loadProjectMap projectIds =
      if null projectIds
        then pure Map.empty
        else do
          projects <- withPool $ selectList [ME.InternProjectId <-. projectIds] []
          pure $ Map.fromList
            [ (pid, ME.internProjectTitle project)
            | Entity pid project <- projects
            ]

    normalizeOptionalText :: Maybe Text -> Maybe Text
    normalizeOptionalText Nothing = Nothing
    normalizeOptionalText (Just txt) =
      let trimmed = T.strip txt
      in if T.null trimmed then Nothing else Just trimmed

    clamp :: Int -> Int -> Int -> Int
    clamp lo hi val = max lo (min hi val)

withPool
  :: (MonadReader Env m, MonadIO m)
  => SqlPersistT IO a
  -> m a
withPool action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

parseKey
  :: forall record m.
     ( MonadError ServerError m
     , PathPiece (Key record)
     )
  => Text
  -> m (Key record)
parseKey raw =
  maybe (throwError err400 { errBody = "Invalid identifier" }) pure (fromPathPiece raw)
