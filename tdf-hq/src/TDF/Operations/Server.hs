{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.Operations.Server (operationsServer) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson (FromJSON, ToJSON(..), Value, decodeStrict', encode)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Servant

import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.Models (RoleEnum(..), roleToText)
import TDF.Operations.API (OperationsAPI)
import TDF.Operations.Model
  ( TransitionContext(..)
  , allowedTargets
  , canViewEntityType
  , requiresTwoPersonApproval
  , validateTransition
  )
import qualified TDF.Operations.Types as Ops

type OperationsM = ReaderT Env Handler

data OperationsScope = OperationsScope
  { scopeOrganizationId :: UUID
  , scopeBranchId :: UUID
  } deriving (Show, Eq)

operationsServer :: AuthedUser -> ServerT OperationsAPI OperationsM
operationsServer user =
       metricsHandler user
  :<|> listWorkItemsHandler user
  :<|> createManualWorkItemHandler user
  :<|> getWorkItemHandler user
  :<|> markSeenHandler user
  :<|> transitionHandler user
  :<|> assignmentHandler user
  :<|> priorityHandler user
  :<|> createNoteHandler user
  :<|> createApprovalHandler user
  :<|> decideApprovalHandler user
  :<|> listFailuresHandler user
  :<|> replayFailureHandler user
  :<|> streamEventsHandler user
  :<|> listSavedViewsHandler user
  :<|> createSavedViewHandler user
  :<|> createPushSubscriptionHandler user

runOperationsDb :: SqlPersistT IO a -> OperationsM a
runOperationsDb action = do
  Env{envPool} <- ask
  result <- liftIO (try (runSqlPool action envPool))
  case result of
    Right value -> pure value
    Left (err :: SomeException) -> do
      liftIO $ putStrLn ("[operations] database error: " <> displayException err)
      throwError err500 {errBody = "Operational data is temporarily unavailable"}

requireOperationsRole :: AuthedUser -> OperationsM ()
requireOperationsRole AuthedUser{auRoles}
  | any (`elem` auRoles)
      [ Admin, Manager, StudioManager, Accounting, Reception, Teacher, Engineer
      , LiveSessionsProducer, Producer, AandR, Maintenance, ReadOnly
      ] = pure ()
  | otherwise = throwError err403 {errBody = "Operations access denied"}

requireMutatingRole :: AuthedUser -> OperationsM ()
requireMutatingRole AuthedUser{auRoles}
  | ReadOnly `elem` auRoles && not (any (`elem` auRoles) mutatingRoles) =
      throwError err403 {errBody = "Read-only operations access"}
  | any (`elem` auRoles) mutatingRoles = pure ()
  | otherwise = throwError err403 {errBody = "Operations mutation denied"}
  where
    mutatingRoles =
      [Admin, Manager, StudioManager, Accounting, Reception, Teacher, Engineer,
       LiveSessionsProducer, Producer, AandR, Maintenance]

requireManagerRole :: AuthedUser -> OperationsM ()
requireManagerRole AuthedUser{auRoles}
  | any (`elem` auRoles) [Admin, Manager, StudioManager] = pure ()
  | otherwise = throwError err403 {errBody = "Manager permission required"}

requireFinancialApprovalRole :: AuthedUser -> OperationsM ()
requireFinancialApprovalRole AuthedUser{auRoles}
  | any (`elem` auRoles) [Admin, Manager, Accounting] = pure ()
  | otherwise = throwError err403 {errBody = "Financial approval permission required"}

auditRole :: AuthedUser -> Text
auditRole AuthedUser{auRoles} =
  maybe "Unknown" roleToText $ find (`elem` auRoles)
    [Admin, Manager, StudioManager, Accounting, Reception, Teacher, Engineer,
     LiveSessionsProducer, Producer, AandR, Maintenance, ReadOnly]

resolveScope
  :: AuthedUser
  -> Maybe UUID
  -> Maybe UUID
  -> OperationsM OperationsScope
resolveScope user@AuthedUser{auPartyId} requestedOrganization requestedBranch = do
  requireOperationsRole user
  rows <- runOperationsDb $ rawSql
    "SELECT member.organization_id::text, member.branch_id::text \
    \FROM operations_scope_member member \
    \JOIN operations_organization organization ON organization.id = member.organization_id \
    \JOIN operations_branch branch ON branch.id = member.branch_id \
    \WHERE member.party_id = ? AND member.active = TRUE \
    \  AND organization.operations_enabled = TRUE AND branch.active = TRUE \
    \  AND (?::text IS NULL OR member.organization_id = ?::uuid) \
    \  AND (?::text IS NULL OR member.branch_id = ?::uuid) \
    \ORDER BY member.created_at, member.organization_id, member.branch_id LIMIT 1"
    [ PersistInt64 (fromSqlKey auPartyId)
    , maybe PersistNull (PersistText . UUID.toText) requestedOrganization
    , maybe PersistNull (PersistText . UUID.toText) requestedOrganization
    , maybe PersistNull (PersistText . UUID.toText) requestedBranch
    , maybe PersistNull (PersistText . UUID.toText) requestedBranch
    ] :: OperationsM [(Single Text, Single Text)]
  case rows of
    [(Single organizationText, Single branchText)] ->
      case (UUID.fromText organizationText, UUID.fromText branchText) of
        (Just organizationId, Just branchId) -> pure (OperationsScope organizationId branchId)
        _ -> throwError err500 {errBody = "Invalid operations scope configuration"}
    _ -> throwError err403 {errBody = "No enabled organization and branch scope"}

partyIdValue :: AuthedUser -> PersistValue
partyIdValue AuthedUser{auPartyId} = PersistInt64 (fromSqlKey auPartyId)

uuidValue :: UUID -> PersistValue
uuidValue = PersistText . UUID.toText

maybeUuidValue :: Maybe UUID -> PersistValue
maybeUuidValue = maybe PersistNull uuidValue

maybeTextValue :: Maybe Text -> PersistValue
maybeTextValue = maybe PersistNull PersistText

maybeInt64Value :: Maybe Int64 -> PersistValue
maybeInt64Value = maybe PersistNull PersistInt64

jsonValue :: ToJSON value => value -> PersistValue
jsonValue = PersistText . TE.decodeUtf8 . BL.toStrict . encode

decodeJsonText :: FromJSON value => Text -> OperationsM value
decodeJsonText raw =
  maybe
    (throwError err500 {errBody = "Invalid operational database projection"})
    pure
    (decodeStrict' (TE.encodeUtf8 raw))

workItemJson :: Text -> Text
workItemJson alias = T.concat
  [ "jsonb_build_object("
  , "'id', ", alias, ".id, "
  , "'organizationId', ", alias, ".organization_id, "
  , "'branchId', ", alias, ".branch_id, "
  , "'sourceSystem', ", alias, ".source_system, "
  , "'sourceChannel', ", alias, ".source_channel, "
  , "'entityType', ", alias, ".entity_type, "
  , "'entityId', ", alias, ".entity_id, "
  , "'uncorrelated', ", alias, ".uncorrelated, "
  , "'correlationKey', ", alias, ".correlation_key, "
  , "'titleEs', ", alias, ".title_es, "
  , "'titleEn', ", alias, ".title_en, "
  , "'descriptionEs', ", alias, ".description_es, "
  , "'descriptionEn', ", alias, ".description_en, "
  , "'status', ", alias, ".status, "
  , "'priority', ", alias, ".priority, "
  , "'recommendedPriority', ", alias, ".recommended_priority, "
  , "'severity', ", alias, ".severity, "
  , "'seen', (", alias, ".first_seen_at IS NOT NULL), "
  , "'firstSeenBy', ", alias, ".first_seen_by, "
  , "'firstSeenAt', ", alias, ".first_seen_at, "
  , "'assigneePartyId', ", alias, ".assignee_party_id, "
  , "'responsibleTeam', ", alias, ".responsible_team, "
  , "'customerPartyId', ", alias, ".customer_party_id, "
  , "'serviceKey', ", alias, ".service_key, "
  , "'amountMinor', ", alias, ".amount_minor, "
  , "'currency', ", alias, ".currency, "
  , "'paymentState', ", alias, ".payment_state, "
  , "'createdAt', ", alias, ".created_at, "
  , "'updatedAt', ", alias, ".updated_at, "
  , "'dueAt', ", alias, ".due_at, "
  , "'snoozedUntil', ", alias, ".snoozed_until, "
  , "'waitingReason', ", alias, ".waiting_reason, "
  , "'waitingExternalDependency', ", alias, ".waiting_external_dependency, "
  , "'resumeAt', ", alias, ".resume_at, "
  , "'resolvedAt', ", alias, ".resolved_at, "
  , "'archivedAt', ", alias, ".archived_at, "
  , "'slaState', CASE "
  , "WHEN ", alias, ".status = 'waiting' AND ", alias, ".waiting_external_dependency THEN 'paused' "
  , "WHEN ", alias, ".sla_breached_at IS NOT NULL THEN 'breached' "
  , "WHEN ", alias, ".due_at IS NOT NULL AND ", alias, ".due_at <= now() THEN 'due' "
  , "WHEN ", alias, ".due_at IS NOT NULL AND now() >= ", alias, ".created_at + ((", alias, ".due_at - ", alias, ".created_at) * 0.8) THEN 'at_risk' "
  , "ELSE 'on_track' END, "
  , "'version', ", alias, ".version, "
  , "'metadata', COALESCE(", alias, ".metadata, '{}'::jsonb) - ARRAY["
  , "'token','accessToken','refreshToken','authorization','signature','secret','certificate','privateKey',"
  , "'pan','cvv','seedPhrase','rawPayload','taxId','address','email','phone'])"
  ]

roleMode :: AuthedUser -> Text
roleMode AuthedUser{auRoles}
  | any (`elem` auRoles) [Admin, Manager, StudioManager, ReadOnly] = "broad"
  | Accounting `elem` auRoles = "accounting"
  | Reception `elem` auRoles || LiveSessionsProducer `elem` auRoles || Producer `elem` auRoles || AandR `elem` auRoles = "reception"
  | Maintenance `elem` auRoles = "maintenance"
  | otherwise = "assigned"

scopeFilterSql :: Text
scopeFilterSql =
  " AND CASE ?::text \
  \ WHEN 'accounting' THEN item.entity_type IN ('invoice','payment','marketplace_order','course_registration') \
  \ WHEN 'reception' THEN item.entity_type IN ('course_registration','booking','party','invoice','payment','manual','uncorrelated_inbound','marketplace_order','proposal','social_event') \
  \ WHEN 'maintenance' THEN item.entity_type IN ('maintenance_ticket','stock_item','booking','manual') \
  \ WHEN 'assigned' THEN item.assignee_party_id = ? AND item.entity_type IN ('course_registration','booking','maintenance_ticket','manual','social_event','intern_project') \
  \ ELSE item.entity_type <> 'security_incident' OR ?::boolean \
  \ END "

listWorkItemsHandler
  :: AuthedUser
  -> Maybe Text
  -> Maybe Int
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int64
  -> Maybe Int64
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int64
  -> Maybe Int64
  -> Maybe Text
  -> Maybe UUID
  -> Maybe UUID
  -> Maybe Text
  -> OperationsM Ops.WorkItemPageDTO
listWorkItemsHandler user cursor requestedLimit query seenFilter entityFilter statusFilter
  priorityFilter slaFilter assigneeFilter customerFilter serviceFilter fromFilter toFilter
  minAmountFilter maxAmountFilter paymentFilter organizationFilter branchFilter channelFilter = do
  scope <- resolveScope user organizationFilter branchFilter
  (cursorTime, cursorId) <- validateCursor cursor
  validatedFrom <- validateDateFilter "from" fromFilter
  validatedTo <- validateDateFilter "to" toFilter
  let limit = min 100 (max 1 (fromMaybe 30 requestedLimit))
      querySql = T.concat
        [ "SELECT (", workItemJson "item", ")::text FROM operations_work_item item "
        , "WHERE item.organization_id = ?::uuid AND item.branch_id = ?::uuid "
        , "AND (?::text IS NULL OR to_tsvector('simple', COALESCE(item.title_es,'') || ' ' || COALESCE(item.title_en,'') || ' ' || COALESCE(item.description_es,'') || ' ' || COALESCE(item.description_en,'') || ' ' || COALESCE(item.entity_id,'') || ' ' || COALESCE(item.correlation_key,'')) @@ plainto_tsquery('simple', ?::text)) "
        , "AND (?::boolean IS NULL OR (item.first_seen_at IS NOT NULL) = ?::boolean) "
        , "AND (?::text IS NULL OR item.entity_type = ?::text) "
        , "AND (?::text IS NULL OR item.status = ?::text) "
        , "AND (?::text IS NULL OR item.priority = ?::text) "
        , "AND (?::text IS NULL OR CASE ?::text "
        , "  WHEN 'breached' THEN item.sla_breached_at IS NOT NULL "
        , "  WHEN 'paused' THEN item.status = 'waiting' AND item.waiting_external_dependency "
        , "  WHEN 'due' THEN item.sla_breached_at IS NULL AND item.due_at <= now() "
        , "  WHEN 'at_risk' THEN item.sla_breached_at IS NULL AND item.due_at > now() AND now() >= item.created_at + ((item.due_at - item.created_at) * 0.8) "
        , "  ELSE item.sla_breached_at IS NULL AND (item.due_at IS NULL OR now() < item.created_at + ((item.due_at - item.created_at) * 0.8)) END) "
        , "AND (?::bigint IS NULL OR item.assignee_party_id = ?::bigint) "
        , "AND (?::bigint IS NULL OR item.customer_party_id = ?::bigint) "
        , "AND (?::text IS NULL OR item.service_key = ?::text) "
        , "AND (?::text IS NULL OR item.created_at >= ?::timestamptz) "
        , "AND (?::text IS NULL OR item.created_at <= ?::timestamptz) "
        , "AND (?::bigint IS NULL OR item.amount_minor >= ?::bigint) "
        , "AND (?::bigint IS NULL OR item.amount_minor <= ?::bigint) "
        , "AND (?::text IS NULL OR item.payment_state = ?::text) "
        , "AND (?::text IS NULL OR item.source_channel = ?::text) "
        , "AND (?::text IS NULL OR (item.updated_at, item.id) < (?::timestamptz, ?::uuid)) "
        , scopeFilterSql
        , " ORDER BY item.updated_at DESC, item.id DESC LIMIT ?"
        ]
      doubled value = [value, value]
      textPair = doubled . maybeTextValue
      boolPair value = doubled (maybe PersistNull PersistBool value)
      intPair = doubled . maybeInt64Value
      params =
        [uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope)]
        <> textPair query <> boolPair seenFilter <> textPair entityFilter
        <> textPair (normalizeStatusFilter statusFilter)
        <> textPair (normalizePriorityFilter priorityFilter)
        <> textPair (normalizeSlaFilter slaFilter)
        <> intPair assigneeFilter <> intPair customerFilter <> textPair serviceFilter
        <> textPair validatedFrom <> textPair validatedTo
        <> intPair minAmountFilter <> intPair maxAmountFilter
        <> textPair paymentFilter <> textPair channelFilter
        <> [ maybe PersistNull PersistText cursorTime
           , maybe PersistNull PersistText cursorTime
           , maybe PersistNull uuidValue cursorId
           , PersistText (roleMode user)
           , partyIdValue user
           , PersistBool (Admin `elem` auRoles user)
           , PersistInt64 (fromIntegral (limit + 1))
           ]
  rows <- runOperationsDb (rawSql querySql params :: SqlPersistT IO [Single Text])
  decoded <- mapM (decodeJsonText . unSingle) rows
  let hasMore = length decoded > limit
      pageItems = take limit decoded
      next = if hasMore then cursorForLast pageItems else Nothing
  pure Ops.WorkItemPageDTO {Ops.items = pageItems, Ops.nextCursor = next, Ops.hasMore = hasMore}

normalizeStatusFilter :: Maybe Text -> Maybe Text
normalizeStatusFilter Nothing = Nothing
normalizeStatusFilter (Just raw) = Ops.workStatusText <$> Ops.parseWorkStatus raw

normalizePriorityFilter :: Maybe Text -> Maybe Text
normalizePriorityFilter Nothing = Nothing
normalizePriorityFilter (Just raw) = Ops.workPriorityText <$> Ops.parseWorkPriority raw

normalizeSlaFilter :: Maybe Text -> Maybe Text
normalizeSlaFilter Nothing = Nothing
normalizeSlaFilter (Just raw)
  | normalized `elem` ["on_track", "at_risk", "due", "breached", "paused"] = Just normalized
  | otherwise = Nothing
  where normalized = T.toLower (T.strip raw)

validateDateFilter :: Text -> Maybe Text -> OperationsM (Maybe Text)
validateDateFilter _ Nothing = pure Nothing
validateDateFilter label (Just raw)
  | isJust (iso8601ParseM (T.unpack raw) :: Maybe UTCTime) = pure (Just raw)
  | otherwise = throwError err400 {errBody = BL.fromStrict (TE.encodeUtf8 (label <> " must be an ISO-8601 date-time"))}

validateCursor :: Maybe Text -> OperationsM (Maybe Text, Maybe UUID)
validateCursor Nothing = pure (Nothing, Nothing)
validateCursor (Just raw) = case T.breakOnEnd "|" raw of
  (timeWithSeparator, identifier) ->
    let timeText = T.dropEnd 1 timeWithSeparator
    in case (iso8601ParseM (T.unpack timeText) :: Maybe UTCTime, UUID.fromText identifier) of
      (Just _, Just itemId) -> pure (Just timeText, Just itemId)
      _ -> invalid
  where invalid = throwError err400 {errBody = "Invalid cursor"}

cursorForLast :: [Ops.WorkItemDTO] -> Maybe Text
cursorForLast = fmap renderCursor . foldl' (\_ item -> Just item) Nothing
  where
    renderCursor :: Ops.WorkItemDTO -> Text
    renderCursor item = T.pack (show (item.updatedAt)) <> "|" <> UUID.toText (item.id)

metricsHandler :: AuthedUser -> Maybe UUID -> Maybe UUID -> OperationsM Ops.OperationsMetricsDTO
metricsHandler user organizationFilter branchFilter = do
  scope <- resolveScope user organizationFilter branchFilter
  now <- liftIO getCurrentTime
  settings <- runOperationsDb $ rawSql
    "SELECT COALESCE(branch.timezone, organization.default_timezone), organization.default_currency::text \
    \FROM operations_organization organization JOIN operations_branch branch ON branch.organization_id = organization.id \
    \WHERE organization.id = ?::uuid AND branch.id = ?::uuid"
    [uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope)]
    :: OperationsM [(Single Text, Single Text)]
  (timezone, currency) <- case settings of
    [(Single configuredTimezone, Single configuredCurrency)] -> pure (configuredTimezone, configuredCurrency)
    _ -> throwError err500 {errBody = "Operations locale configuration is unavailable"}
  rows <- runOperationsDb $ rawSql
    "SELECT jsonb_build_object( \
    \ 'newRegistrations', count(*) FILTER (WHERE entity_type = 'course_registration' AND status = 'new'), \
    \ 'registrationsRequiringAttention', count(*) FILTER (WHERE entity_type = 'course_registration' AND status NOT IN ('resolved','archived')), \
    \ 'reservationsAwaitingConfirmation', count(*) FILTER (WHERE entity_type = 'booking' AND status NOT IN ('resolved','archived')), \
    \ 'todaySessions', count(*) FILTER (WHERE entity_type = 'booking' AND ((metadata->>'startsAt')::timestamptz AT TIME ZONE ?::text)::date = (now() AT TIME ZONE ?::text)::date), \
    \ 'schedulingConflicts', count(*) FILTER (WHERE metadata->>'conflict' = 'true' AND status NOT IN ('resolved','archived')), \
    \ 'unpaidInvoices', count(*) FILTER (WHERE entity_type = 'invoice' AND COALESCE(payment_state,'unpaid') <> 'paid' AND status NOT IN ('resolved','archived')), \
    \ 'overdueInvoices', count(*) FILTER (WHERE entity_type = 'invoice' AND correlation_key LIKE 'invoice:%' AND priority IN ('urgent','high') AND status NOT IN ('resolved','archived')), \
    \ 'paymentsAwaitingVerification', count(*) FILTER (WHERE entity_type = 'payment' AND payment_state IN ('pending','verification_required')), \
    \ 'revenueReceivedTodayMinor', COALESCE(sum(amount_minor) FILTER (WHERE entity_type = 'payment' AND payment_state = 'completed' AND (created_at AT TIME ZONE ?::text)::date = (now() AT TIME ZONE ?::text)::date), 0), \
    \ 'unassignedWork', count(*) FILTER (WHERE assignee_party_id IS NULL AND status NOT IN ('resolved','archived')), \
    \ 'slaBreaches', count(*) FILTER (WHERE sla_breached_at IS NOT NULL AND status NOT IN ('resolved','archived')), \
    \ 'averageFirstResponseSeconds', avg(extract(epoch FROM (first_seen_at - created_at))) FILTER (WHERE first_seen_at IS NOT NULL), \
    \ 'averageResolutionSeconds', avg(extract(epoch FROM (resolved_at - created_at))) FILTER (WHERE resolved_at IS NOT NULL), \
    \ 'integrationFailures', (SELECT count(*) FROM operations_integration_failure failure WHERE failure.organization_id = ?::uuid AND failure.status IN ('open','retrying','dead_letter')), \
    \ 'currency', ?::text, 'calculatedAt', ?::timestamptz)::text \
    \FROM operations_work_item WHERE organization_id = ?::uuid AND branch_id = ?::uuid"
    [ PersistText timezone, PersistText timezone
    , PersistText timezone, PersistText timezone
    , uuidValue (scopeOrganizationId scope)
    , PersistText currency
    , PersistUTCTime now
    , uuidValue (scopeOrganizationId scope)
    , uuidValue (scopeBranchId scope)
    ] :: OperationsM [Single Text]
  case rows of
    [Single payload] -> decodeJsonText payload
    _ -> throwError err500 {errBody = "Could not calculate operations metrics"}

loadVisibleWorkItem :: AuthedUser -> UUID -> OperationsM (OperationsScope, Ops.WorkItemDTO)
loadVisibleWorkItem user itemId = do
  requireOperationsRole user
  rows <- runOperationsDb $ rawSql
    (T.concat
      [ "SELECT member.organization_id::text, member.branch_id::text, (", workItemJson "item", ")::text "
      , "FROM operations_work_item item JOIN operations_scope_member member "
      , "ON member.organization_id = item.organization_id AND member.branch_id = item.branch_id "
      , "JOIN operations_organization organization ON organization.id = member.organization_id "
      , "WHERE item.id = ?::uuid AND member.party_id = ? AND member.active = TRUE "
      , "AND organization.operations_enabled = TRUE LIMIT 1"
      ])
    [uuidValue itemId, partyIdValue user] :: OperationsM [(Single Text, Single Text, Single Text)]
  case rows of
    [(Single orgText, Single branchText, Single payload)] -> do
      item <- decodeJsonText payload
      let assigned = item.assigneePartyId == Just (fromSqlKey (auPartyId user))
      unless (canViewEntityType (auRoles user) assigned (item.entityType)) $
        throwError err404 {errBody = "Work item not found"}
      case (UUID.fromText orgText, UUID.fromText branchText) of
        (Just orgId, Just branchId) -> pure (OperationsScope orgId branchId, item)
        _ -> throwError err500 {errBody = "Invalid operations scope"}
    _ -> throwError err404 {errBody = "Work item not found"}

getWorkItemHandler :: AuthedUser -> UUID -> OperationsM Ops.WorkItemDetailDTO
getWorkItemHandler user itemId = do
  (_, item) <- loadVisibleWorkItem user itemId
  eventRows <- runOperationsDb $ rawSql
    "SELECT jsonb_build_object('id', event.id, 'eventType', event.event_type, \
    \ 'actorPartyId', event.actor_party_id, 'actorRole', event.actor_role, \
    \ 'bodyEs', event.body_es, 'bodyEn', event.body_en, 'metadata', event.metadata, \
    \ 'occurredAt', event.occurred_at)::text \
    \FROM operations_work_item_event event WHERE event.work_item_id = ?::uuid \
    \ORDER BY event.occurred_at, event.id"
    [uuidValue itemId] :: OperationsM [Single Text]
  noteRows <- runOperationsDb $ rawSql
    "SELECT jsonb_build_object('id', note.id, 'authorPartyId', note.author_party_id, \
    \ 'body', note.body, 'mentionedPartyIds', COALESCE((SELECT jsonb_agg(mention.mentioned_party_id) FROM operations_mention mention WHERE mention.note_id = note.id), '[]'::jsonb), \
    \ 'createdAt', note.created_at, 'editedAt', note.edited_at)::text \
    \FROM operations_note note WHERE note.work_item_id = ?::uuid ORDER BY note.created_at, note.id"
    [uuidValue itemId] :: OperationsM [Single Text]
  itemEvents <- mapM (decodeJsonText . unSingle) eventRows
  itemNotes <- mapM (decodeJsonText . unSingle) noteRows
  pure Ops.WorkItemDetailDTO
    { Ops.workItem = item
    , Ops.events = itemEvents
    , Ops.notes = itemNotes
    , Ops.allowedTransitions = allowedTargets (item.status)
    , Ops.sourceRecordUrl = sourceRecordUrl item
    , Ops.quickActions = quickActionsFor (item.entityType)
    }

sourceRecordUrl :: Ops.WorkItemDTO -> Maybe Text
sourceRecordUrl item = case (item.entityType, item.entityId) of
  ("course_registration", Just entityId) -> Just ("/cursos/admin/registrations/" <> entityId)
  ("booking", Just entityId) -> Just ("/bookings?bookingId=" <> entityId)
  ("invoice", Just entityId) -> Just ("/invoices/" <> entityId)
  ("payment", Just entityId) -> Just ("/payments?paymentId=" <> entityId)
  ("party", Just entityId) -> Just ("/parties/" <> entityId)
  ("marketplace_order", Just entityId) -> Just ("/marketplace/orders/" <> entityId)
  ("maintenance_ticket", Just entityId) -> Just ("/inventory?maintenanceTicket=" <> entityId)
  ("proposal", Just entityId) -> Just ("/proposals/" <> entityId)
  ("stock_item", Just entityId) -> Just ("/inventory?stockItem=" <> entityId)
  ("intern_project", Just entityId) -> Just ("/internships/projects/" <> entityId)
  ("social_event", Just entityId) -> Just ("/events/" <> entityId)
  ("feature_access_request", Just entityId) -> Just ("/access-requests/review?requestId=" <> entityId)
  _ -> Nothing

quickActionsFor :: Text -> [Text]
quickActionsFor entityType = case entityType of
  "course_registration" -> ["review", "approve", "reject", "waitlist", "assign", "create_invoice", "record_payment", "confirm"]
  "booking" -> ["review_conflicts", "approve", "reject", "reschedule", "assign_resources", "request_deposit", "create_quote", "create_invoice"]
  "invoice" -> ["edit_draft", "issue", "download", "deliver", "duplicate_draft", "request_credit_note", "request_debit_note"]
  "payment" -> ["verify_transfer", "reconcile", "request_refund", "open_dispute"]
  "maintenance_ticket" -> ["assign", "schedule", "mark_safe", "resolve"]
  "proposal" -> ["review", "send", "revise", "accept", "reject"]
  "stock_item" -> ["record_movement", "set_reorder_point", "open_inventory"]
  "intern_project" -> ["assign", "add_task", "update_due_date", "open_project"]
  "social_event" -> ["assign_production", "review_venue", "review_tasks", "open_event"]
  "feature_access_request" -> ["review_scope", "approve", "reject"]
  "uncorrelated_inbound" -> ["correlate_identity", "assign", "reply", "add_note"]
  _ -> ["open_source", "assign", "add_note"]

fetchUpdatedItem :: UUID -> SqlPersistT IO Ops.WorkItemDTO
fetchUpdatedItem itemId = do
  rows <- rawSql
    ("SELECT (" <> workItemJson "item" <> ")::text FROM operations_work_item item WHERE item.id = ?::uuid")
    [uuidValue itemId] :: SqlPersistT IO [Single Text]
  case rows of
    [Single payload] ->
      maybe (fail "invalid work item projection") pure (decodeStrict' (TE.encodeUtf8 payload))
    _ -> fail "work item not found after update"

recordWorkItemEffect
  :: AuthedUser
  -> OperationsScope
  -> UUID
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Value
  -> Value
  -> SqlPersistT IO ()
recordWorkItemEffect user scope itemId action eventType requestId reason previousValue newValue = do
  now <- liftIO getCurrentTime
  rawExecute
    "INSERT INTO operations_work_item_event (organization_id, work_item_id, event_type, actor_party_id, actor_role, body_es, body_en, occurred_at) \
    \VALUES (?::uuid, ?::uuid, ?::text, ?, ?::text, ?::text, ?::text, ?)"
    [ uuidValue (scopeOrganizationId scope), uuidValue itemId, PersistText eventType
    , partyIdValue user, PersistText (auditRole user)
    , PersistText (fromMaybe action reason), PersistText (fromMaybe action reason), PersistUTCTime now
    ]
  rawExecute
    "INSERT INTO operations_stream_event (organization_id, branch_id, event_type, work_item_id, payload) \
    \VALUES (?::uuid, ?::uuid, 'work_item.updated', ?::uuid, jsonb_build_object('workItemId', ?::uuid, 'action', ?::text))"
    [uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), uuidValue itemId, uuidValue itemId, PersistText action]
  rawExecute
    "INSERT INTO operations_admin_audit (organization_id, branch_id, actor_party_id, acting_role, source_client, action, target_entity_type, target_entity_id, previous_value, new_value, request_id, correlation_id, reason) \
    \VALUES (?::uuid, ?::uuid, ?, ?::text, 'api', ?::text, 'operations_work_item', ?::text, ?::jsonb, ?::jsonb, ?::text, ?::text, ?::text)"
    [ uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), partyIdValue user
    , PersistText (auditRole user), PersistText action, PersistText (UUID.toText itemId)
    , jsonValue previousValue, jsonValue newValue, PersistText requestId
    , PersistText (UUID.toText itemId), maybe PersistNull PersistText reason
    ]

markSeenHandler :: AuthedUser -> UUID -> Ops.VersionedCommand -> OperationsM Ops.WorkItemDTO
markSeenHandler user itemId command = do
  requireMutatingRole user
  (scope, before) <- loadVisibleWorkItem user itemId
  when (command.expectedVersion /= before.version) conflict
  updated <- runOperationsDb $ do
    now <- liftIO getCurrentTime
    rawExecute
      "UPDATE operations_work_item SET first_seen_by = COALESCE(first_seen_by, ?), \
      \ first_seen_at = COALESCE(first_seen_at, ?), status = CASE WHEN status = 'new' THEN 'seen' ELSE status END, \
      \ updated_at = ?, version = version + 1 WHERE id = ?::uuid AND version = ?"
      [partyIdValue user, PersistUTCTime now, PersistUTCTime now, uuidValue itemId, PersistInt64 (command.expectedVersion)]
    after <- fetchUpdatedItem itemId
    recordWorkItemEffect user scope itemId "mark_seen" "work_item.seen" (command.requestId)
      (command.reason) (toJson before) (toJson after)
    pure after
  pure updated

transitionHandler :: AuthedUser -> UUID -> Ops.TransitionCommand -> OperationsM Ops.WorkItemDTO
transitionHandler user itemId command = do
  requireMutatingRole user
  (scope, before) <- loadVisibleWorkItem user itemId
  when (command.expectedVersion /= before.version) conflict
  either (const invalidTransition) pure $ validateTransition TransitionContext
    { currentStatus = before.status
    , targetStatus = command.targetStatus
    , actorRoles = auRoles user
    , hasAssignee = isJust (before.assigneePartyId)
    , reason = command.reason
    , waitingExternalDependency = command.waitingExternalDependency
    , resumeAtPresent = isJust (command.resumeAt)
    }
  updated <- runOperationsDb $ do
    now <- liftIO getCurrentTime
    let target = command.targetStatus
        isWaiting = target == Ops.WorkWaiting
        isResolved = target == Ops.WorkResolved
        isArchived = target == Ops.WorkArchived
        leavingWaiting = before.status == Ops.WorkWaiting && target /= Ops.WorkWaiting
    when leavingWaiting $ rawExecute
      "UPDATE operations_sla_timer SET due_at = due_at + (now() - paused_at), \
      \ paused_seconds = paused_seconds + extract(epoch FROM (now() - paused_at))::bigint, paused_at = NULL \
      \ WHERE work_item_id = ?::uuid AND paused_at IS NOT NULL"
      [uuidValue itemId]
    when (isWaiting && fromMaybe False (command.waitingExternalDependency)) $ rawExecute
      "UPDATE operations_sla_timer SET paused_at = COALESCE(paused_at, now()) \
      \ WHERE work_item_id = ?::uuid AND completed_at IS NULL"
      [uuidValue itemId]
    when isResolved $ rawExecute
      "UPDATE operations_sla_timer SET completed_at = COALESCE(completed_at, now()) \
      \ WHERE work_item_id = ?::uuid AND completed_at IS NULL"
      [uuidValue itemId]
    rawExecute
      "UPDATE operations_work_item SET status = ?::text, \
      \ waiting_started_at = CASE WHEN ?::boolean THEN ? ELSE NULL END, \
      \ waiting_reason = CASE WHEN ?::boolean THEN ?::text ELSE NULL END, \
      \ waiting_external_dependency = CASE WHEN ?::boolean THEN ?::boolean ELSE FALSE END, \
      \ resume_at = CASE WHEN ?::boolean THEN ? ELSE NULL END, \
      \ resolved_at = CASE WHEN ?::boolean THEN ? ELSE CASE WHEN ?::boolean THEN NULL ELSE resolved_at END END, \
      \ archived_at = CASE WHEN ?::boolean THEN ? ELSE CASE WHEN ?::boolean THEN NULL ELSE archived_at END END, \
      \ updated_at = ?, version = version + 1 WHERE id = ?::uuid AND version = ?"
      [ PersistText (Ops.workStatusText target)
      , PersistBool isWaiting, PersistUTCTime now
      , PersistBool isWaiting, maybe PersistNull PersistText (command.reason)
      , PersistBool isWaiting, PersistBool (fromMaybe False (command.waitingExternalDependency))
      , PersistBool isWaiting, maybe PersistNull PersistUTCTime (command.resumeAt)
      , PersistBool isResolved, PersistUTCTime now, PersistBool (target == Ops.WorkInProgress)
      , PersistBool isArchived, PersistUTCTime now, PersistBool (target == Ops.WorkInProgress)
      , PersistUTCTime now, uuidValue itemId, PersistInt64 (command.expectedVersion)
      ]
    after <- fetchUpdatedItem itemId
    recordWorkItemEffect user scope itemId "transition" "work_item.transitioned"
      (command.requestId) (command.reason) (toJson before) (toJson after)
    pure after
  pure updated

assignmentHandler :: AuthedUser -> UUID -> Ops.AssignmentCommand -> OperationsM Ops.WorkItemDTO
assignmentHandler user itemId command = do
  requireMutatingRole user
  (scope, before) <- loadVisibleWorkItem user itemId
  when (command.expectedVersion /= before.version) conflict
  case command.assigneePartyId of
    Nothing -> pure ()
    Just assignee -> do
      membership <- runOperationsDb $ rawSql
        "SELECT count(*) FROM operations_scope_member WHERE organization_id = ?::uuid AND branch_id = ?::uuid AND party_id = ? AND active = TRUE"
        [uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), PersistInt64 assignee] :: OperationsM [Single Int64]
      unless (membership == [Single 1]) $
        throwError err422 {errBody = "Assignee is outside the work-item scope"}
  updated <- runOperationsDb $ do
    now <- liftIO getCurrentTime
    rawExecute
      "UPDATE operations_work_item SET assignee_party_id = ?, responsible_team = ?::text, \
      \ status = CASE WHEN ?::bigint IS NOT NULL AND status IN ('new','seen') THEN 'assigned' ELSE status END, \
      \ updated_at = ?, version = version + 1 WHERE id = ?::uuid AND version = ?"
      [ maybeInt64Value (command.assigneePartyId), maybeTextValue (command.responsibleTeam)
      , maybeInt64Value (command.assigneePartyId), PersistUTCTime now, uuidValue itemId
      , PersistInt64 (command.expectedVersion)
      ]
    after <- fetchUpdatedItem itemId
    recordWorkItemEffect user scope itemId "assign" "work_item.assigned"
      (command.requestId) (command.reason) (toJson before) (toJson after)
    pure after
  pure updated

priorityHandler :: AuthedUser -> UUID -> Ops.PriorityCommand -> OperationsM Ops.WorkItemDTO
priorityHandler user itemId command = do
  requireManagerRole user
  when (T.null (T.strip (command.reason))) $
    throwError err422 {errBody = "Priority override reason is required"}
  (scope, before) <- loadVisibleWorkItem user itemId
  when (command.expectedVersion /= before.version) conflict
  updated <- runOperationsDb $ do
    now <- liftIO getCurrentTime
    rawExecute
      "UPDATE operations_work_item SET priority = ?::text, priority_override_reason = ?::text, \
      \ updated_at = ?, version = version + 1 WHERE id = ?::uuid AND version = ?"
      [ PersistText (Ops.workPriorityText (command.priority)), PersistText (T.strip (command.reason))
      , PersistUTCTime now, uuidValue itemId, PersistInt64 (command.expectedVersion)
      ]
    after <- fetchUpdatedItem itemId
    recordWorkItemEffect user scope itemId "override_priority" "work_item.priority_overridden"
      (command.requestId) (Just (command.reason)) (toJson before) (toJson after)
    pure after
  pure updated

createNoteHandler :: AuthedUser -> UUID -> Ops.NoteCreate -> OperationsM Ops.WorkItemNoteDTO
createNoteHandler user itemId command = do
  requireMutatingRole user
  (scope, _) <- loadVisibleWorkItem user itemId
  let noteBody = T.strip (command.body)
  when (T.null noteBody || T.length noteBody > 5000) $
    throwError err422 {errBody = "Note body must contain 1 to 5000 characters"}
  rows <- runOperationsDb $ do
    forM_ (command.mentionedPartyIds) $ \mentionedPartyId -> do
      membership <- rawSql
        "SELECT count(*) FROM operations_scope_member member \
        \WHERE member.organization_id = ?::uuid AND member.branch_id = ?::uuid \
        \AND member.party_id = ? AND member.active"
        [ uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope)
        , PersistInt64 mentionedPartyId
        ] :: SqlPersistT IO [Single Int64]
      unless (membership == [Single 1]) (fail "mention outside operations scope")
    inserted <- rawSql
      "INSERT INTO operations_note (organization_id, work_item_id, author_party_id, body) \
      \VALUES (?::uuid, ?::uuid, ?, ?::text) RETURNING id::text, created_at"
      [uuidValue (scopeOrganizationId scope), uuidValue itemId, partyIdValue user, PersistText noteBody]
      :: SqlPersistT IO [(Single Text, Single UTCTime)]
    case inserted of
      [(Single noteIdText, Single created)] -> do
        forM_ (command.mentionedPartyIds) $ \mentionedPartyId ->
          rawExecute
            "INSERT INTO operations_mention (note_id, mentioned_party_id) \
            \VALUES (?::uuid, ?) ON CONFLICT DO NOTHING"
            [PersistText noteIdText, PersistInt64 mentionedPartyId]
        rawExecute
          "INSERT INTO operations_stream_event (organization_id, branch_id, event_type, work_item_id, payload) \
          \VALUES (?::uuid, ?::uuid, 'work_item.note_added', ?::uuid, jsonb_build_object('workItemId', ?::uuid, 'noteId', ?::uuid))"
          [uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), uuidValue itemId, uuidValue itemId, PersistText noteIdText]
        rawExecute
          "INSERT INTO operations_admin_audit (organization_id, branch_id, actor_party_id, acting_role, source_client, action, target_entity_type, target_entity_id, new_value, request_id, correlation_id) \
          \VALUES (?::uuid, ?::uuid, ?, ?::text, ?::text, 'add_note', 'operations_note', ?::text, jsonb_build_object('workItemId', ?::uuid), ?::text, ?::text)"
          [uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), partyIdValue user,
           PersistText (auditRole user), PersistText (command.sourceClient), PersistText noteIdText,
           uuidValue itemId, PersistText (command.requestId), PersistText (UUID.toText itemId)]
        case UUID.fromText noteIdText of
          Just noteId -> pure [Ops.WorkItemNoteDTO noteId (fromSqlKey (auPartyId user)) noteBody (command.mentionedPartyIds) created Nothing]
          Nothing -> fail "invalid note identifier"
      _ -> fail "could not create note"
  case rows of
    [note] -> pure note
    _ -> throwError err500 {errBody = "Could not create note"}

createManualWorkItemHandler :: AuthedUser -> Ops.ManualWorkItemCreate -> OperationsM Ops.WorkItemDTO
createManualWorkItemHandler user command = do
  requireMutatingRole user
  scope <- resolveScope user (Just (command.organizationId)) (command.branchId)
  when (T.null (T.strip (command.correlationKey))) $
    throwError err422 {errBody = "Correlation key is required"}
  when (command.uncorrelated /= not (isJust (command.entityId))) $
    throwError err422 {errBody = "Uncorrelated items must omit entityId; correlated items must include it"}
  item <- runOperationsDb $ do
    now <- liftIO getCurrentTime
    let aggregateId = fromMaybe (UUID.toText (command.organizationId) <> ":uncorrelated") (command.entityId)
        eventType = if command.uncorrelated then "manual.uncorrelated_created" else "manual.created"
        dedup = command.correlationKey <> ":manual-created"
        payload = jsonValue command
    rawExecute
      "INSERT INTO operations_domain_event (organization_id, branch_id, event_type, aggregate_type, aggregate_id, source_system, source_channel, correlation_key, deduplication_key, occurred_at, payload) \
      \VALUES (?::uuid, ?::uuid, ?::text, ?::text, ?::text, 'tdf-hq', 'manual', ?::text, encode(digest(?::text, 'sha256'), 'hex'), ?, ?::jsonb) \
      \ON CONFLICT (organization_id, deduplication_key) DO NOTHING"
      [ uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), PersistText eventType
      , PersistText (if command.uncorrelated then "uncorrelated_inbound" else command.entityType)
      , PersistText aggregateId, PersistText (command.correlationKey), PersistText dedup
      , PersistUTCTime now, payload
      ]
    _ <- rawSql "SELECT processed, failed, dead_lettered FROM operations_process_outbox_batch(100, 'api-manual')" []
      :: SqlPersistT IO [(Single Int, Single Int, Single Int)]
    rows <- rawSql
      ("SELECT (" <> workItemJson "item" <> ")::text FROM operations_work_item item WHERE item.organization_id = ?::uuid AND item.correlation_key = ?::text")
      [uuidValue (scopeOrganizationId scope), PersistText (command.correlationKey)] :: SqlPersistT IO [Single Text]
    case rows of
      [Single payloadText] -> maybe (fail "invalid manual work item") pure (decodeStrict' (TE.encodeUtf8 payloadText))
      _ -> fail "manual work item projection unavailable"
  pure item

createApprovalHandler :: AuthedUser -> Ops.ApprovalCreate -> OperationsM Ops.ApprovalDTO
createApprovalHandler user command = do
  requireFinancialApprovalRole user
  scope <- resolveScope user (Just (command.organizationId)) (command.branchId)
  unless (requiresTwoPersonApproval (command.actionType) Nothing (fromMaybe 0 (command.amountMinor))) $
    throwError err422 {errBody = "Action does not require dual approval"}
  rows <- runOperationsDb $ do
    approvalRows <- rawSql
      "INSERT INTO operations_approval_request (organization_id, branch_id, work_item_id, action_type, target_entity_type, target_entity_id, amount_minor, currency, requester_party_id, requester_role, request_reason, expires_at, idempotency_key) \
      \VALUES (?::uuid, ?::uuid, ?::uuid, ?::text, ?::text, ?::text, ?, ?::text, ?, ?::text, ?::text, ?, ?::text) \
      \ON CONFLICT (organization_id, idempotency_key) DO UPDATE SET idempotency_key = EXCLUDED.idempotency_key \
      \RETURNING jsonb_build_object('id', id, 'organizationId', organization_id, 'branchId', branch_id, 'workItemId', work_item_id, 'actionType', action_type, 'targetEntityType', target_entity_type, 'targetEntityId', target_entity_id, 'amountMinor', amount_minor, 'currency', currency, 'requesterPartyId', requester_party_id, 'requesterRole', requester_role, 'requestReason', request_reason, 'requestedAt', requested_at, 'approverPartyId', approver_party_id, 'approverRole', approver_role, 'decision', decision, 'decisionReason', decision_reason, 'decidedAt', decided_at, 'expiresAt', expires_at, 'executionStatus', execution_status)::text"
      [ uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), maybeUuidValue (command.workItemId)
      , PersistText (command.actionType), PersistText (command.targetEntityType), PersistText (command.targetEntityId)
      , maybeInt64Value (command.amountMinor), maybeTextValue (command.currency), partyIdValue user
      , PersistText (auditRole user), PersistText (T.strip (command.reason)), maybe PersistNull PersistUTCTime (command.expiresAt)
      , PersistText (command.idempotencyKey)
      ] :: SqlPersistT IO [Single Text]
    rawExecute
      "INSERT INTO operations_admin_audit (organization_id, branch_id, actor_party_id, acting_role, source_client, action, target_entity_type, target_entity_id, new_value, request_id, correlation_id, reason, approval_request_id) \
      \VALUES (?::uuid, ?::uuid, ?, ?::text, ?::text, 'approval_requested', ?::text, ?::text, jsonb_build_object('actionType', ?::text, 'amountMinor', ?::bigint, 'currency', ?::text), ?::text, ?::text, ?::text, (SELECT id FROM operations_approval_request WHERE organization_id = ?::uuid AND idempotency_key = ?::text))"
      [ uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), partyIdValue user
      , PersistText (auditRole user), PersistText (command.sourceClient), PersistText (command.targetEntityType)
      , PersistText (command.targetEntityId), PersistText (command.actionType), maybeInt64Value (command.amountMinor)
      , maybeTextValue (command.currency), PersistText (command.requestId), PersistText (command.idempotencyKey)
      , PersistText (T.strip (command.reason)), uuidValue (scopeOrganizationId scope)
      , PersistText (command.idempotencyKey)
      ]
    pure approvalRows
  case rows of
    [Single payload] -> decodeJsonText payload
    _ -> throwError err500 {errBody = "Could not create approval request"}

decideApprovalHandler :: AuthedUser -> UUID -> Ops.ApprovalDecision -> OperationsM Ops.ApprovalDTO
decideApprovalHandler user approvalId command = do
  requireFinancialApprovalRole user
  let normalizedDecision = T.toLower (T.strip (command.decision))
  unless (normalizedDecision `elem` ["approved", "rejected"]) $
    throwError err422 {errBody = "Decision must be approved or rejected"}
  rows <- runOperationsDb $ do
    updatedRows <- rawSql
      "UPDATE operations_approval_request approval SET approver_party_id = ?, approver_role = ?::text, decision = ?::text, decision_reason = ?::text, decided_at = now(), execution_status = CASE WHEN ?::text = 'approved' THEN 'pending' ELSE 'not_started' END \
      \WHERE approval.id = ?::uuid AND approval.decision = ?::text AND approval.requester_party_id <> ? \
      \AND EXISTS (SELECT 1 FROM operations_scope_member member JOIN operations_organization organization ON organization.id = member.organization_id WHERE member.organization_id = approval.organization_id AND member.branch_id = approval.branch_id AND member.party_id = ? AND member.active AND organization.operations_enabled) \
      \RETURNING jsonb_build_object('id', id, 'organizationId', organization_id, 'branchId', branch_id, 'workItemId', work_item_id, 'actionType', action_type, 'targetEntityType', target_entity_type, 'targetEntityId', target_entity_id, 'amountMinor', amount_minor, 'currency', currency, 'requesterPartyId', requester_party_id, 'requesterRole', requester_role, 'requestReason', request_reason, 'requestedAt', requested_at, 'approverPartyId', approver_party_id, 'approverRole', approver_role, 'decision', decision, 'decisionReason', decision_reason, 'decidedAt', decided_at, 'expiresAt', expires_at, 'executionStatus', execution_status)::text, organization_id::text, branch_id::text, target_entity_type, target_entity_id"
      [ partyIdValue user, PersistText (auditRole user), PersistText normalizedDecision
      , PersistText (T.strip (command.reason)), PersistText normalizedDecision
      , uuidValue approvalId, PersistText (command.expectedDecision), partyIdValue user, partyIdValue user
      ] :: SqlPersistT IO [(Single Text, Single Text, Single Text, Single Text, Single Text)]
    case updatedRows of
      [(Single payload, Single organizationText, Single branchText, Single targetType, Single targetId)] -> do
        rawExecute
          "INSERT INTO operations_admin_audit (organization_id, branch_id, actor_party_id, acting_role, source_client, action, target_entity_type, target_entity_id, previous_value, new_value, request_id, correlation_id, approval_request_id, reason) \
          \VALUES (?::uuid, ?::uuid, ?, ?::text, ?::text, 'approval_decided', ?::text, ?::text, jsonb_build_object('decision', ?::text), jsonb_build_object('decision', ?::text), ?::text, ?::text, ?::uuid, ?::text)"
          [ PersistText organizationText, PersistText branchText, partyIdValue user
          , PersistText (auditRole user), PersistText (command.sourceClient), PersistText targetType
          , PersistText targetId, PersistText (command.expectedDecision), PersistText normalizedDecision
          , PersistText (command.requestId), PersistText (UUID.toText approvalId), uuidValue approvalId
          , PersistText (T.strip (command.reason))
          ]
        pure [Single payload]
      _ -> pure []
  case rows of
    [Single payload] -> decodeJsonText payload
    _ -> throwError err409 {errBody = "Approval changed, is unavailable, or cannot be self-approved"}

listFailuresHandler
  :: AuthedUser
  -> Maybe UUID
  -> Maybe Text
  -> Maybe Int
  -> OperationsM [Ops.IntegrationFailureDTO]
listFailuresHandler user organizationFilter statusFilter requestedLimit = do
  requireManagerRole user
  scope <- resolveScope user organizationFilter Nothing
  let limit = min 100 (max 1 (fromMaybe 30 requestedLimit))
  rows <- runOperationsDb $ rawSql
    "SELECT jsonb_build_object('id', id, 'organizationId', organization_id, 'branchId', branch_id, 'provider', provider, 'direction', direction, 'sourceRecordType', source_record_type, 'sourceRecordId', source_record_id, 'failureCode', failure_code, 'redactedSummary', redacted_summary, 'retryable', retryable, 'status', status, 'attemptCount', attempt_count, 'lastAttemptAt', last_attempt_at, 'nextAttemptAt', next_attempt_at, 'createdAt', created_at)::text \
    \FROM operations_integration_failure WHERE organization_id = ?::uuid AND (?::text IS NULL OR status = ?::text) ORDER BY created_at DESC, id DESC LIMIT ?"
    [uuidValue (scopeOrganizationId scope), maybeTextValue statusFilter, maybeTextValue statusFilter, PersistInt64 (fromIntegral limit)] :: OperationsM [Single Text]
  mapM (decodeJsonText . unSingle) rows

replayFailureHandler :: AuthedUser -> UUID -> Ops.ReplayCommand -> OperationsM Ops.IntegrationFailureDTO
replayFailureHandler user failureId command = do
  requireManagerRole user
  when (T.null (T.strip (command.reason))) $
    throwError err422 {errBody = "Replay reason is required"}
  rows <- runOperationsDb $ do
    replayRows <- rawSql
      "UPDATE operations_integration_failure failure SET status = 'retrying', next_attempt_at = now(), last_attempt_at = now(), attempt_count = attempt_count + 1 \
      \WHERE failure.id = ?::uuid AND failure.retryable = TRUE \
      \AND EXISTS (SELECT 1 FROM operations_scope_member member JOIN operations_organization organization ON organization.id = member.organization_id WHERE member.organization_id = failure.organization_id AND member.branch_id = failure.branch_id AND member.party_id = ? AND member.active AND organization.operations_enabled) \
      \RETURNING jsonb_build_object('id', id, 'organizationId', organization_id, 'branchId', branch_id, 'provider', provider, 'direction', direction, 'sourceRecordType', source_record_type, 'sourceRecordId', source_record_id, 'failureCode', failure_code, 'redactedSummary', redacted_summary, 'retryable', retryable, 'status', status, 'attemptCount', attempt_count, 'lastAttemptAt', last_attempt_at, 'nextAttemptAt', next_attempt_at, 'createdAt', created_at)::text, organization_id::text, branch_id::text, source_record_type, source_record_id"
      [uuidValue failureId, partyIdValue user]
      :: SqlPersistT IO [(Single Text, Single Text, Single Text, Single Text, Single Text)]
    case replayRows of
      [(Single payload, Single organizationText, Single branchText, Single sourceType, Single sourceId)] -> do
        rawExecute
          "INSERT INTO operations_admin_audit (organization_id, branch_id, actor_party_id, acting_role, source_client, action, target_entity_type, target_entity_id, previous_value, new_value, request_id, correlation_id, reason) \
          \VALUES (?::uuid, ?::uuid, ?, ?::text, ?::text, 'integration_failure_replayed', ?::text, ?::text, jsonb_build_object('status', 'open'), jsonb_build_object('status', 'retrying'), ?::text, ?::text, ?::text)"
          [ PersistText organizationText, PersistText branchText, partyIdValue user
          , PersistText (auditRole user), PersistText (command.sourceClient), PersistText sourceType
          , PersistText sourceId, PersistText (command.requestId), PersistText (UUID.toText failureId)
          , PersistText (T.strip (command.reason))
          ]
        pure [Single payload]
      _ -> pure []
  case rows of
    [Single payload] -> decodeJsonText payload
    _ -> throwError err404 {errBody = "Replayable failure not found"}

streamEventsHandler
  :: AuthedUser
  -> Maybe Int64
  -> Maybe Int
  -> Maybe UUID
  -> OperationsM Ops.StreamBatchDTO
streamEventsHandler user afterId requestedLimit organizationFilter = do
  scope <- resolveScope user organizationFilter Nothing
  let limit = min 250 (max 1 (fromMaybe 100 requestedLimit))
  rows <- runOperationsDb $ rawSql
    "SELECT jsonb_build_object('id', stream.id, 'eventType', stream.event_type, 'workItemId', stream.work_item_id, 'payload', stream.payload, 'createdAt', stream.created_at)::text \
    \FROM operations_stream_event stream WHERE stream.organization_id = ?::uuid AND stream.branch_id = ?::uuid AND stream.id > ? \
    \AND (stream.visible_to_party_id IS NULL OR stream.visible_to_party_id = ?) \
    \AND (stream.work_item_id IS NULL OR EXISTS (SELECT 1 FROM operations_work_item item WHERE item.id = stream.work_item_id AND CASE ?::text WHEN 'accounting' THEN item.entity_type IN ('invoice','payment','marketplace_order','course_registration') WHEN 'reception' THEN item.entity_type IN ('course_registration','booking','party','invoice','payment','manual','uncorrelated_inbound','marketplace_order','proposal','social_event') WHEN 'maintenance' THEN item.entity_type IN ('maintenance_ticket','stock_item','booking','manual') WHEN 'assigned' THEN item.assignee_party_id = ? AND item.entity_type IN ('course_registration','booking','maintenance_ticket','manual','social_event','intern_project') ELSE item.entity_type <> 'security_incident' OR ?::boolean END)) \
    \ORDER BY stream.id LIMIT ?"
    [ uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), PersistInt64 (fromMaybe 0 afterId)
    , partyIdValue user, PersistText (roleMode user), partyIdValue user
    , PersistBool (Admin `elem` auRoles user), PersistInt64 (fromIntegral limit)
    ] :: OperationsM [Single Text]
  events <- mapM (decodeJsonText . unSingle) rows
  pure Ops.StreamBatchDTO
    { Ops.events = events
    , Ops.lastEventId = case reverse events of
        event : _ -> Just (event.id)
        [] -> afterId
    , Ops.retryAfterMs = 15000
    }

listSavedViewsHandler :: AuthedUser -> Maybe UUID -> OperationsM [Ops.SavedViewDTO]
listSavedViewsHandler user organizationFilter = do
  scope <- resolveScope user organizationFilter Nothing
  rows <- runOperationsDb $ rawSql
    "SELECT jsonb_build_object('id', id, 'organizationId', organization_id, 'ownerPartyId', owner_party_id, 'name', name, 'shared', shared, 'filters', filters, 'columns', columns, 'widgets', widgets, 'subscribedEventTypes', subscribed_event_types, 'updatedAt', updated_at)::text \
    \FROM operations_saved_view WHERE organization_id = ?::uuid AND (shared OR owner_party_id = ?) ORDER BY shared DESC, name"
    [uuidValue (scopeOrganizationId scope), partyIdValue user] :: OperationsM [Single Text]
  mapM (decodeJsonText . unSingle) rows

createSavedViewHandler :: AuthedUser -> Ops.SavedViewCreate -> OperationsM Ops.SavedViewDTO
createSavedViewHandler user command = do
  requireMutatingRole user
  scope <- resolveScope user (Just command.organizationId) Nothing
  let viewName = T.strip command.name
  when (T.null viewName || T.length viewName > 120) $
    throwError err422 {errBody = "Saved-view name must contain 1 to 120 characters"}
  rows <- runOperationsDb $ do
    savedRows <- rawSql
      "INSERT INTO operations_saved_view (organization_id, owner_party_id, name, shared, filters, columns, widgets, subscribed_event_types) \
      \VALUES (?::uuid, ?, ?::text, ?::boolean, ?::jsonb, ?::jsonb, ?::jsonb, ?::jsonb) \
      \ON CONFLICT (organization_id, owner_party_id, name) DO UPDATE SET shared = EXCLUDED.shared, filters = EXCLUDED.filters, columns = EXCLUDED.columns, widgets = EXCLUDED.widgets, subscribed_event_types = EXCLUDED.subscribed_event_types, updated_at = now() \
      \RETURNING jsonb_build_object('id', id, 'organizationId', organization_id, 'ownerPartyId', owner_party_id, 'name', name, 'shared', shared, 'filters', filters, 'columns', columns, 'widgets', widgets, 'subscribedEventTypes', subscribed_event_types, 'updatedAt', updated_at)::text"
      [ uuidValue (scopeOrganizationId scope), partyIdValue user, PersistText viewName
      , PersistBool command.shared, jsonValue command.filters, jsonValue command.columns
      , jsonValue command.widgets, jsonValue command.subscribedEventTypes
      ] :: SqlPersistT IO [Single Text]
    rawExecute
      "INSERT INTO operations_admin_audit (organization_id, branch_id, actor_party_id, acting_role, source_client, action, target_entity_type, target_entity_id, new_value, request_id, correlation_id) \
      \VALUES (?::uuid, ?::uuid, ?, ?::text, ?::text, 'save_view', 'operations_saved_view', ?::text, jsonb_build_object('name', ?::text, 'shared', ?::boolean), ?::text, ?::text)"
      [ uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), partyIdValue user
      , PersistText (auditRole user), PersistText command.sourceClient, PersistText viewName
      , PersistText viewName, PersistBool command.shared, PersistText command.requestId
      , PersistText (UUID.toText (scopeOrganizationId scope))
      ]
    pure savedRows
  case rows of
    [Single payload] -> decodeJsonText payload
    _ -> throwError err500 {errBody = "Could not save operational view"}

createPushSubscriptionHandler :: AuthedUser -> Ops.PushSubscriptionCreate -> OperationsM Ops.PushSubscriptionDTO
createPushSubscriptionHandler user command = do
  scope <- resolveScope user (Just (command.organizationId)) Nothing
  unless (command.platform `elem` ["ios", "android", "web"]) $
    throwError err422 {errBody = "Unsupported push platform"}
  when (T.length (command.deviceToken) < 16 || T.length (command.deviceToken) > 4096) $
    throwError err422 {errBody = "Invalid push token"}
  rows <- runOperationsDb $ do
    subscriptionRows <- rawSql
      "WITH encryption AS (SELECT NULLIF(current_setting('tdf.push_encryption_key', true), '') AS key), inserted AS ( \
      \ INSERT INTO operations_push_subscription (organization_id, party_id, platform, device_token_digest, encrypted_device_token) \
      \ SELECT ?::uuid, ?, ?::text, encode(digest(?::text, 'sha256'), 'hex'), pgp_sym_encrypt(?::text, encryption.key) FROM encryption WHERE encryption.key IS NOT NULL \
      \ ON CONFLICT (organization_id, party_id, device_token_digest) DO UPDATE SET active = TRUE, platform = EXCLUDED.platform, encrypted_device_token = EXCLUDED.encrypted_device_token, updated_at = now() \
      \ RETURNING id, organization_id, platform, active, updated_at) \
      \SELECT jsonb_build_object('id', id, 'organizationId', organization_id, 'platform', platform, 'active', active, 'updatedAt', updated_at)::text, id::text FROM inserted"
      [ uuidValue (scopeOrganizationId scope), partyIdValue user, PersistText (command.platform)
      , PersistText (command.deviceToken), PersistText (command.deviceToken)
      ] :: SqlPersistT IO [(Single Text, Single Text)]
    case subscriptionRows of
      [(Single payload, Single subscriptionId)] -> do
        rawExecute
          "INSERT INTO operations_admin_audit (organization_id, branch_id, actor_party_id, acting_role, source_client, action, target_entity_type, target_entity_id, new_value, request_id, correlation_id) \
          \VALUES (?::uuid, ?::uuid, ?, ?::text, ?::text, 'push_subscription_registered', 'operations_push_subscription', ?::text, jsonb_build_object('platform', ?::text, 'active', true), ?::text, ?::text)"
          [ uuidValue (scopeOrganizationId scope), uuidValue (scopeBranchId scope), partyIdValue user
          , PersistText (auditRole user), PersistText (command.sourceClient), PersistText subscriptionId
          , PersistText (command.platform), PersistText (command.requestId), PersistText subscriptionId
          ]
        pure [Single payload]
      _ -> pure []
  case rows of
    [Single payload] -> decodeJsonText payload
    _ -> throwError err503 {errBody = "Push token encryption is not configured"}

toJson :: ToJSON value => value -> Value
toJson = toJSON

conflict :: OperationsM a
conflict = throwError err409 {errBody = "Work item changed; refresh and retry"}

invalidTransition :: OperationsM a
invalidTransition = throwError err422 {errBody = "Operational transition guard rejected the command"}
