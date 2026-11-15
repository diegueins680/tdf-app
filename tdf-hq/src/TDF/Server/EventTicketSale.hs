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

data SalePolicyRow = SalePolicyRow
    { sprId :: T.Text
    , sprApprovalStatus :: T.Text
    , sprActive :: Bool
    , sprApprovedAt :: Maybe UTCTime
    , sprCreatedAt :: UTCTime
    , sprUpdatedAt :: UTCTime
    }
    deriving (Show, Eq)

-- | Rows are newest first. An active approved policy wins; otherwise a
-- pending draft; otherwise the newest row decides whether the last request
-- was declined. A policy that was approved and later retired is not a
-- declined request: the manager may simply ask again.
saleStateFromRows :: [SalePolicyRow] -> (T.Text, Maybe SalePolicyRow)
saleStateFromRows rows
    | Just row <- find (\r -> sprActive r && sprApprovalStatus r == "approved") rows = ("approved", Just row)
    | Just row <- find ((== "draft") . sprApprovalStatus) rows = ("requested", Just row)
    | (row : _) <- rows
    , sprApprovalStatus row == "retired"
    , isNothing (sprApprovedAt row) =
        ("declined", Just row)
    | otherwise = ("none", Nothing)

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
                "SELECT id::text, approval_status, active, approved_at, created_at, updated_at\
                \ FROM event_ticket_checkout_policy WHERE event_id = ?\
                \ ORDER BY created_at DESC, id DESC"
                [toPersistValue eventKey]
        pure
            [ SalePolicyRow{..}
            | (Single sprId, Single sprApprovalStatus, Single sprActive, Single sprApprovedAt, Single sprCreatedAt, Single sprUpdatedAt) <- rows
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
            blocked
                | status == "approved" = Just "already_approved"
                | status == "requested" = Just "already_requested"
                | not manager = Just "not_manager"
                | not payable = Just "no_paid_tier"
                | otherwise = Nothing
         in EventTicketSaleDTO
                { etsEventId = keyText eventKey
                , etsStatus = status
                , etsRequestId = sprId <$> mRow
                , etsRequestedAt = sprCreatedAt <$> mRow
                , etsDecidedAt = case status of
                    "approved" -> mRow >>= sprApprovedAt
                    "declined" -> sprUpdatedAt <$> mRow
                    _ -> Nothing
                , etsCanRequest = isNothing blocked
                , etsBlockedReason = blocked
                , etsCanDecide = strictAdmin && status == "requested"
                , etsTerms = standardTicketSaleTerms
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
                                let terms = standardTicketSaleTerms
                                rawExecute
                                    "INSERT INTO event_ticket_checkout_policy\
                                    \ (event_id, policy_version, currency, buyer_fee_bps, organizer_fee_bps, tax_bps,\
                                    \  hold_minutes, terms_version, terms_summary, refund_policy, transfer_allowed,\
                                    \  approval_status, active, max_tickets_per_order, tax_included, tax_invoice_required)\
                                    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, TRUE, 'draft', FALSE, ?, FALSE, FALSE)"
                                    [ toPersistValue eventKey
                                    , PersistText (ticketSalePolicyVersion (fromSqlKey eventKey) currentPartyId now)
                                    , PersistText (etstCurrency terms)
                                    , PersistInt64 (fromIntegral (etstBuyerFeeBps terms))
                                    , PersistInt64 (fromIntegral (etstOrganizerFeeBps terms))
                                    , PersistInt64 (fromIntegral (etstTaxBps terms))
                                    , PersistInt64 (fromIntegral (etstHoldMinutes terms))
                                    , PersistText standardTermsVersion
                                    , PersistText (etstTermsSummary terms)
                                    , PersistText (etstRefundPolicy terms)
                                    , PersistInt64 (fromIntegral (etstMaxTicketsPerOrder terms))
                                    ]
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
                    "SELECT policy.id::text, policy.event_id, policy.created_at\
                    \ FROM event_ticket_checkout_policy policy\
                    \ WHERE policy.approval_status = 'draft'\
                    \ ORDER BY policy.created_at ASC, policy.id ASC LIMIT 200"
                    []
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
                            rows <- loadRows eventKey
                            payable <- hasPayableTier eventKey
                            let target = find ((== requestId) . sprId) rows
                                otherActive = any (\r -> sprActive r && sprId r /= requestId) rows
                                alreadyDecided row
                                    | approve = sprApprovalStatus row == "approved" && sprActive row
                                    | otherwise = sprApprovalStatus row == "retired" && isNothing (sprApprovedAt row)
                            case target of
                                Nothing -> pure (Left err404{errBody = "Ticket sale request not found"})
                                -- Deciding the same way twice returns the
                                -- recorded decision and notifies nobody.
                                Just row | alreadyDecided row -> do
                                    (_, dto) <- currentSale eventKey eventRow
                                    pure (Right (dto, Nothing))
                                Just row
                                    | sprApprovalStatus row /= "draft" ->
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
