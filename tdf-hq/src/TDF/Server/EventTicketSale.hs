{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | EVT-TICKET-SALE-001: an event manager asks to sell paid tickets under
-- TDF's standard terms and a strict administrator approves or declines.
--
-- The request is a @draft@ row of @event_ticket_checkout_policy@; approval
-- makes that same row the event's active approved policy, which is the only
-- thing public checkout reads. Nothing here opens checkout by itself: the
-- event state, the commerce flag and a payment provider still decide that.
module TDF.Server.EventTicketSale (
    EventTicketSaleDeps (..),
    SalePolicyRow (..),
    eventTicketSaleServer,
    saleStateFromRows,
    standardTicketSaleTerms,
    ticketSalePolicyVersion,
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (catMaybes, isJust, isNothing)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import qualified Data.UUID as UUID
import Database.Persist
import Database.Persist.Sql
import Servant

import TDF.API.SocialEventsAPI (EventTicketSaleRoutes)
import TDF.Auth (AuthedUser (..), hasStrictAdminAccess, modulesForRoles)
import TDF.Catalog.Security (loadCanonicalPartyRoles, selectCanonicalPartyIdsByRole)
import TDF.DB (Env (..))
import TDF.DTO.SocialEventsDTO (
    EventTicketSaleDTO (..),
    EventTicketSaleRequestDTO (..),
    EventTicketSaleTermsDTO (..),
    EventTicketSaleTierDTO (..),
 )
import qualified TDF.Email.Service as EmailSvc
import qualified TDF.LogBuffer as LogBuf
import TDF.Models (Party, PartyId, RoleEnum (Admin), partyDisplayName, partyPrimaryEmail)
import TDF.Models.SocialEventsModels
import TDF.Server.EventAdminDelegation (delegatedEventAdminExists)

type AppM = ReaderT Env Handler

-- | What the sale rules need from the event handlers.
data EventTicketSaleDeps = EventTicketSaleDeps
    { parseVisibleEventKey :: T.Text -> AppM SocialEventId
    , lockSocialEventForMutation :: SocialEventId -> SqlPersistT IO (Maybe (Entity SocialEvent))
    }

-- | The terms every approved sale carries. They are fixed: neither the
-- organizer nor the approving administrator edits them here.
standardTicketSaleTerms :: EventTicketSaleTermsDTO
standardTicketSaleTerms =
    EventTicketSaleTermsDTO
        { etstCurrency = "USD"
        , etstBuyerFeeBps = 200
        , etstOrganizerFeeBps = 200
        , etstTaxBps = 0
        , etstHoldMinutes = 15
        , etstMaxTicketsPerOrder = 100
        , etstRefundPolicy = "Reembolso total."
        , etstTermsSummary = "Al comprar, aceptas el precio, las tarifas y las condiciones mostradas en este checkout."
        }

standardTermsVersion :: T.Text
standardTermsVersion = "ticket-terms-v1"

-- The reserved version format identifies requests created by this workflow.
-- Legacy and operator-created drafts have their own versions.
workflowRequestSql :: T.Text
workflowRequestSql =
    "policy_version ~ ('^ticket-v1-event-' || event_id::text || '-req-[0-9]+-[0-9]{8}T[0-9]{12}$')"

standardTermsSql :: T.Text
standardTermsSql =
    "ROW(currency, buyer_fee_bps, organizer_fee_bps, tax_bps, hold_minutes,\
    \ terms_version, terms_summary, refund_policy, max_tickets_per_order)\
    \ = ROW(?::text, ?::integer, ?::integer, ?::integer, ?::integer,\
    \ ?::text, ?::text, ?::text, ?::integer)\
    \ AND transfer_allowed AND NOT tax_included AND NOT tax_invoice_required\
    \ AND transfer_deadline IS NULL AND effective_until IS NULL\
    \ AND manual_transfer_hold_minutes IS NULL AND manual_transfer_cutoff_at IS NULL"

standardTermValues :: [PersistValue]
standardTermValues =
    [ PersistText etstCurrency
    , PersistInt64 (fromIntegral etstBuyerFeeBps)
    , PersistInt64 (fromIntegral etstOrganizerFeeBps)
    , PersistInt64 (fromIntegral etstTaxBps)
    , PersistInt64 (fromIntegral etstHoldMinutes)
    , PersistText standardTermsVersion
    , PersistText etstTermsSummary
    , PersistText etstRefundPolicy
    , PersistInt64 (fromIntegral etstMaxTicketsPerOrder)
    ]
  where
    EventTicketSaleTermsDTO{..} = standardTicketSaleTerms

undecidedPolicySql :: T.Text
undecidedPolicySql =
    "NOT active AND approved_at IS NULL AND approved_by IS NULL AND effective_from IS NULL"

data SalePolicyRow = SalePolicyRow
    { sprId :: T.Text
    , sprApprovalStatus :: T.Text
    , sprActive :: Bool
    , sprDecidedAt :: Maybe UTCTime
    , sprCreatedAt :: UTCTime
    , sprWorkflowRequest :: Bool
    , sprStandardTerms :: Bool
    , sprUndecided :: Bool
    , sprTerms :: EventTicketSaleTermsDTO
    }
    deriving (Show, Eq)

-- | Rows are newest first. An active approved policy wins; otherwise a
-- pending compatible request; otherwise the newest workflow row decides whether it
-- was declined. A policy that was approved and later retired is not a
-- declined request: the manager may simply ask again.
saleStateFromRows :: [SalePolicyRow] -> (T.Text, Maybe SalePolicyRow)
saleStateFromRows rows
    | Just row <- find (\r -> sprActive r && sprApprovalStatus r == "approved") rows = ("approved", Just row)
    | Just row <- find isPendingRequest workflowRows = ("requested", Just row)
    | (row : _) <- workflowRows
    , sprUndecided row
    , sprApprovalStatus row == "retired" =
        ("declined", Just row)
    | otherwise = ("none", Nothing)
  where
    workflowRows = filter (\row -> sprWorkflowRequest row && sprStandardTerms row) rows

isPendingRequest :: SalePolicyRow -> Bool
isPendingRequest row = sprApprovalStatus row == "draft" && sprUndecided row

-- | Names the event, the requesting party and the instant, so the request's
-- author stays readable in the append-only policy history.
ticketSalePolicyVersion :: Int64 -> T.Text -> UTCTime -> T.Text
ticketSalePolicyVersion eventId partyId now =
    "ticket-v1-event-"
        <> T.pack (show eventId)
        <> "-req-"
        <> T.filter (`elem` ['0' .. '9']) partyId
        <> "-"
        <> T.pack (take 21 (formatTime defaultTimeLocale "%Y%m%dT%H%M%S%q" now))

eventTicketSaleServer :: AuthedUser -> T.Text -> EventTicketSaleDeps -> ServerT EventTicketSaleRoutes AppM
eventTicketSaleServer user currentPartyId EventTicketSaleDeps{..} =
    getTicketSale
        :<|> requestTicketSale
        :<|> listTicketSaleRequests
        :<|> decideTicketSaleRequest True
        :<|> decideTicketSaleRequest False
  where
    strictAdmin :: Bool
    strictAdmin = hasStrictAdminAccess user

    runSql :: SqlPersistT IO a -> AppM a
    runSql action = do
        Env{..} <- ask
        liftIO (runSqlPool action envPool)

    organizerOf :: SocialEvent -> Maybe T.Text
    organizerOf eventRow =
        case T.strip <$> socialEventOrganizerPartyId eventRow of
            Just owner | not (T.null owner) -> Just owner
            _ -> Nothing

    managesEvent :: SocialEventId -> SocialEvent -> SqlPersistT IO Bool
    managesEvent eventKey eventRow =
        case organizerOf eventRow of
            Nothing -> pure False
            Just owner
                | owner == currentPartyId -> pure True
                | otherwise -> delegatedEventAdminExists eventKey currentPartyId

    loadRows :: SocialEventId -> SqlPersistT IO [SalePolicyRow]
    loadRows eventKey = do
        rows <-
            rawSql
                ( "SELECT id::text, approval_status, active, created_at, "
                    <> " CASE WHEN approval_status = 'approved' THEN approved_at ELSE updated_at END, "
                    <> workflowRequestSql <> ", (" <> standardTermsSql <> "), (" <> undecidedPolicySql <> "),"
                    <> " currency, buyer_fee_bps, organizer_fee_bps, tax_bps, hold_minutes,"
                    <> " max_tickets_per_order, refund_policy, terms_summary"
                    <> " FROM event_ticket_checkout_policy WHERE event_id = ?"
                    <> " ORDER BY created_at DESC, id DESC"
                )
                (standardTermValues <> [toPersistValue eventKey])
        pure
            [ SalePolicyRow{sprTerms = EventTicketSaleTermsDTO{..}, ..}
            | ( Single sprId, Single sprApprovalStatus, Single sprActive, Single sprCreatedAt
              , Single sprDecidedAt, Single sprWorkflowRequest
              , Single sprStandardTerms, Single sprUndecided, Single etstCurrency
              , Single etstBuyerFeeBps, Single etstOrganizerFeeBps, Single etstTaxBps
              , Single etstHoldMinutes, Single etstMaxTicketsPerOrder, Single etstRefundPolicy
              , Single etstTermsSummary
              ) <- rows
            ]

    hasPayableTier :: SocialEventId -> SqlPersistT IO Bool
    hasPayableTier eventKey = do
        rows <-
            rawSql
                "SELECT count(*) FROM event_ticket_tier\
                \ WHERE event_id = ? AND is_active AND price_cents > 0 AND upper(currency) = ?"
                [toPersistValue eventKey, PersistText (etstCurrency standardTicketSaleTerms)]
        pure (case rows of [Single total] -> (total :: Int64) > 0; _ -> False)

    saleDTO :: SocialEventId -> Bool -> Bool -> [SalePolicyRow] -> EventTicketSaleDTO
    saleDTO eventKey manager payable rows =
        let (status, mRow) = saleStateFromRows rows
            requestRow = mRow >>= \row -> if sprWorkflowRequest row then Just row else Nothing
            blocked
                | status == "approved" = Just "already_approved"
                | status == "requested" = Just "already_requested"
                | not manager = Just "not_manager"
                | not payable = Just "no_paid_tier"
                | otherwise = Nothing
         in EventTicketSaleDTO
                { etsEventId = keyText eventKey
                , etsStatus = status
                , etsRequestId = sprId <$> requestRow
                , etsRequestedAt = sprCreatedAt <$> requestRow
                , etsDecidedAt = case status of
                    "approved" -> mRow >>= sprDecidedAt
                    "declined" -> mRow >>= sprDecidedAt
                    _ -> Nothing
                , etsCanRequest = isNothing blocked
                , etsBlockedReason = blocked
                , etsCanDecide = strictAdmin && status == "requested"
                , etsTerms = maybe standardTicketSaleTerms sprTerms mRow
                }

    currentSale :: SocialEventId -> SocialEvent -> SqlPersistT IO (Bool, EventTicketSaleDTO)
    currentSale eventKey eventRow = do
        manager <- managesEvent eventKey eventRow
        payable <- hasPayableTier eventKey
        rows <- loadRows eventKey
        pure (manager, saleDTO eventKey manager payable rows)

    getTicketSale :: T.Text -> AppM EventTicketSaleDTO
    getTicketSale rawEventId = do
        eventKey <- parseVisibleEventKey rawEventId
        result <- runSql $ do
            mEvent <- get eventKey
            forM mEvent (currentSale eventKey)
        case result of
            Nothing -> throwError err404{errBody = "Event not found"}
            Just (manager, dto)
                | manager || strictAdmin -> pure dto
                | otherwise -> throwError notManagerError

    requestTicketSale :: T.Text -> AppM EventTicketSaleDTO
    requestTicketSale rawEventId = do
        eventKey <- parseVisibleEventKey rawEventId
        now <- liftIO getCurrentTime
        result <- runSql $ do
            locked <- lockSocialEventForMutation eventKey
            case locked of
                Nothing -> pure (Left err404{errBody = "Event not found"})
                Just (Entity _ eventRow) -> do
                    (manager, before) <- currentSale eventKey eventRow
                    if not manager
                        then pure (Left notManagerError)
                        else case etsBlockedReason before of
                            -- Asking twice is not an error: the pending or
                            -- approved request is the answer.
                            Just reason | reason `elem` ["already_requested", "already_approved"] -> pure (Right (before, Nothing))
                            Just _ -> pure (Left err409{errBody = "Add an active paid ticket tier in USD before requesting paid sales"})
                            Nothing -> do
                                rawExecute
                                    "INSERT INTO event_ticket_checkout_policy\
                                    \ (event_id, policy_version, currency, buyer_fee_bps, organizer_fee_bps, tax_bps,\
                                    \  hold_minutes, terms_version, terms_summary, refund_policy, max_tickets_per_order,\
                                    \  transfer_allowed, approval_status, active, tax_included, tax_invoice_required)\
                                    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, TRUE, 'draft', FALSE, FALSE, FALSE)"
                                    ( [ toPersistValue eventKey
                                      , PersistText (ticketSalePolicyVersion (fromSqlKey eventKey) currentPartyId now)
                                      ] <> standardTermValues
                                    )
                                (_, after) <- currentSale eventKey eventRow
                                reviewers <- strictAdminContacts
                                pure (Right (after, Just (eventRow, reviewers)))
        (dto, created) <- either throwError pure result
        forM_ created $ \(eventRow, reviewers) ->
            notify reviewers
                ("Solicitud de venta de entradas: " <> socialEventTitle eventRow)
                [ "El evento «" <> socialEventTitle eventRow <> "» pidió vender entradas pagadas con las condiciones estándar de TDF."
                , "Revisa la solicitud y apruébala o recházala desde el evento."
                ]
                (fromSqlKey eventKey)
        pure dto

    listTicketSaleRequests :: AppM [EventTicketSaleRequestDTO]
    listTicketSaleRequests = do
        unless strictAdmin $ throwError strictAdminError
        runSql $ do
            rows <-
                rawSql
                    ( "SELECT id::text, event_id, created_at FROM event_ticket_checkout_policy"
                        <> " WHERE approval_status = 'draft' AND (" <> workflowRequestSql <> ")"
                        <> " AND (" <> undecidedPolicySql <> ") AND (" <> standardTermsSql <> ")"
                        <> " ORDER BY created_at ASC, id ASC LIMIT 200"
                    )
                    standardTermValues
            fmap catMaybes . forM rows $ \(Single requestId, Single rawEventId, Single requestedAt) -> do
                let eventKey = toSqlKey (rawEventId :: Int64) :: SocialEventId
                mEvent <- get eventKey
                forM mEvent $ \eventRow -> do
                    tiers <- selectList [EventTicketTierEventId ==. eventKey, EventTicketTierIsActive ==. True] [Asc EventTicketTierId]
                    organizerName <- maybe (pure Nothing) partyNameFor (organizerOf eventRow)
                    pure
                        EventTicketSaleRequestDTO
                            { etsrRequestId = requestId
                            , etsrEventId = keyText eventKey
                            , etsrEventTitle = socialEventTitle eventRow
                            , etsrEventStartsAt = socialEventStartTime eventRow
                            , etsrOrganizerName = organizerName
                            , etsrRequestedAt = requestedAt
                            , etsrTiers =
                                [ EventTicketSaleTierDTO
                                    { etstiName = eventTicketTierName tier
                                    , etstiPriceCents = eventTicketTierPriceCents tier
                                    , etstiCurrency = eventTicketTierCurrency tier
                                    , etstiQuantityTotal = eventTicketTierQuantityTotal tier
                                    }
                                | Entity _ tier <- tiers
                                ]
                            , etsrTerms = standardTicketSaleTerms
                            }

    decideTicketSaleRequest :: Bool -> T.Text -> AppM EventTicketSaleDTO
    decideTicketSaleRequest approve rawRequestId = do
        unless strictAdmin $ throwError strictAdminError
        requestId <- maybe (throwError err404{errBody = "Ticket sale request not found"}) (pure . UUID.toText) (UUID.fromText (T.strip rawRequestId))
        now <- liftIO getCurrentTime
        result <- runSql $ do
            owning <- rawSql "SELECT event_id FROM event_ticket_checkout_policy WHERE id = ?::uuid" [PersistText requestId]
            case owning of
                [Single rawEventId] -> do
                    let eventKey = toSqlKey (rawEventId :: Int64) :: SocialEventId
                    locked <- lockSocialEventForMutation eventKey
                    case locked of
                        Nothing -> pure (Left err404{errBody = "Event not found"})
                        Just (Entity _ eventRow) -> do
                            -- Keep a concurrent policy writer from changing the terms
                            -- between validation and activation.
                            _ <- (rawSql
                                "SELECT id::text FROM event_ticket_checkout_policy WHERE id = ?::uuid FOR UPDATE"
                                [PersistText requestId] :: SqlPersistT IO [Single T.Text])
                            rows <- loadRows eventKey
                            payable <- hasPayableTier eventKey
                            let target = find ((== requestId) . sprId) rows
                                otherActive = any (\r -> sprActive r && sprId r /= requestId) rows
                                alreadyDecided row
                                    | approve = sprApprovalStatus row == "approved" && sprActive row
                                    | otherwise = sprApprovalStatus row == "retired" && sprUndecided row
                            case target of
                                Nothing -> pure (Left err404{errBody = "Ticket sale request not found"})
                                -- Deciding the same way twice returns the
                                -- recorded decision and notifies nobody.
                                Just row
                                    | not (sprWorkflowRequest row) ->
                                        pure (Left err404{errBody = "Ticket sale request not found"})
                                    | not (sprStandardTerms row) ->
                                        pure (Left err409{errBody = "This request no longer matches the standard ticket sale terms"})
                                Just row | alreadyDecided row -> do
                                    (_, dto) <- currentSale eventKey eventRow
                                    pure (Right (dto, Nothing))
                                Just row
                                    | not (isPendingRequest row) ->
                                        pure (Left err409{errBody = "This ticket sale request was already decided"})
                                    | approve && otherActive ->
                                        pure (Left err409{errBody = "This event already has an active ticket policy"})
                                    | approve && not payable ->
                                        pure (Left err409{errBody = "The event no longer has an active paid ticket tier in USD"})
                                    | approve -> do
                                        rawExecute
                                            "UPDATE event_ticket_checkout_policy\
                                            \ SET approval_status = 'approved', active = TRUE, approved_at = ?,\
                                            \     approved_by = ?, effective_from = ?\
                                            \ WHERE id = ?::uuid AND approval_status = 'draft'"
                                            [PersistUTCTime now, PersistText ("party:" <> currentPartyId), PersistUTCTime now, PersistText requestId]
                                        decided eventKey eventRow
                                    | otherwise -> do
                                        rawExecute
                                            "UPDATE event_ticket_checkout_policy SET approval_status = 'retired'\
                                            \ WHERE id = ?::uuid AND approval_status = 'draft'"
                                            [PersistText requestId]
                                        decided eventKey eventRow
                _ -> pure (Left err404{errBody = "Ticket sale request not found"})
        (dto, notice) <- either throwError pure result
        forM_ notice $ \(eventRow, contacts, eventId) ->
            if approve
                then
                    notify contacts
                        ("Venta de entradas aprobada: " <> socialEventTitle eventRow)
                        [ "TDF aprobó la venta de entradas pagadas de «" <> socialEventTitle eventRow <> "»."
                        , "Las entradas se pueden comprar cuando el evento esté publicado y a la venta."
                        ]
                        eventId
                else
                    notify contacts
                        ("Venta de entradas no aprobada: " <> socialEventTitle eventRow)
                        [ "TDF no aprobó por ahora la venta de entradas pagadas de «" <> socialEventTitle eventRow <> "»."
                        , "Puedes escribirnos para revisar el caso y volver a solicitarla desde el evento."
                        ]
                        eventId
        pure dto
      where
        decided eventKey eventRow = do
            (_, dto) <- currentSale eventKey eventRow
            contacts <- maybe (pure []) organizerContact (organizerOf eventRow)
            pure (Right (dto, Just (eventRow, contacts, fromSqlKey eventKey)))

    partyKeyFromText :: T.Text -> Maybe PartyId
    partyKeyFromText raw =
        case reads (T.unpack raw) of
            [(value, "")] | value > (0 :: Int64) -> Just (toSqlKey value)
            _ -> Nothing

    partyNameFor :: T.Text -> SqlPersistT IO (Maybe T.Text)
    partyNameFor raw = fmap (fmap partyDisplayName) (maybe (pure Nothing) get (partyKeyFromText raw))

    contactOf :: Party -> Maybe (T.Text, T.Text)
    contactOf party =
        case T.strip <$> partyPrimaryEmail party of
            Just email | not (T.null email) -> Just (partyDisplayName party, email)
            _ -> Nothing

    organizerContact :: T.Text -> SqlPersistT IO [(T.Text, T.Text)]
    organizerContact raw = do
        mParty <- maybe (pure Nothing) get (partyKeyFromText raw)
        pure (maybe [] (maybe [] pure . contactOf) mParty)

    -- Every strict administrator except the requester; a role lookup that
    -- fails closes to "not a reviewer".
    strictAdminContacts :: SqlPersistT IO [(T.Text, T.Text)]
    strictAdminContacts = do
        candidates <- selectCanonicalPartyIdsByRole Admin
        fmap catMaybes . forM candidates $ \partyKey -> do
            rolesResult <- loadCanonicalPartyRoles partyKey
            let strict = case rolesResult of
                    Right roles -> hasStrictAdminAccess (AuthedUser partyKey roles (modulesForRoles roles) Nothing Nothing)
                    Left _ -> False
            if strict && keyText partyKey /= currentPartyId
                then (>>= contactOf) <$> get partyKey
                else pure Nothing

    -- The decision is already committed: mail is a courtesy that never
    -- fails or delays the request.
    notify :: [(T.Text, T.Text)] -> T.Text -> [T.Text] -> Int64 -> AppM ()
    notify contacts subject body eventId = do
        Env{..} <- ask
        let emailSvc = EmailSvc.mkEmailService envConfig
            link = fmap (\base -> T.dropWhileEnd (== '/') base <> "/social/eventos/" <> T.pack (show eventId) <> "#venta-entradas") (EmailSvc.esAppBase emailSvc)
        when (isJust (EmailSvc.esConfig emailSvc)) $
            void . liftIO . forkIO . forM_ contacts $ \(name, email) -> do
                sent <- try (EmailSvc.sendTestEmail emailSvc name email subject body link) :: IO (Either SomeException ())
                case sent of
                    Left err -> LogBuf.addLog LogBuf.LogError ("[EventTicketSale] Email failed: " <> T.pack (displayException err))
                    Right () -> pure ()

    notManagerError :: ServerError
    notManagerError = err403{errBody = "Only the event organizer or its administrators can manage ticket sales"}

    strictAdminError :: ServerError
    strictAdminError = err403{errBody = "Strict administrator role required"}

keyText :: (ToBackendKey SqlBackend record) => Key record -> T.Text
keyText = T.pack . show . fromSqlKey
