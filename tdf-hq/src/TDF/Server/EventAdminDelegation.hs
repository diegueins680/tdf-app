{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | EVT-ADMIN-DELEGATION-001: administrators an organizer delegates one event
-- to through a single-use link, and the courtesy tickets they may issue.
-- The event handlers supply their own event, ticket and lock primitives, so
-- this module adds the delegation rules without importing those handlers.
module TDF.Server.EventAdminDelegation (
    EventAdminDeps (..),
    eventAdminsServer,
    eventManagerPartyFor,
    delegatedEventAdminExists,
    isEventManager,
    requireEventManager,
    eventNotOrganizerError,
    eventWithoutOrganizerError,
    isTicketTierSaleOpen,
) where

import Control.Monad (forM, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import qualified Crypto.Hash as CryptoHash
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAscii, isControl, isHexDigit)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Database.Persist
import Database.Persist.Sql
import Servant
import Web.PathPieces (fromPathPiece)

import TDF.API.SocialEventsAPI (EventAdminRoutes)
import TDF.DB (Env (..))
import TDF.DTO.SocialEventsDTO (
    EventAdminAccessDTO (..),
    EventAdminInvitationCreateDTO (..),
    EventAdminInvitationDTO (..),
    EventAdminInvitationIssuedDTO (..),
    EventAdminInvitationPreviewDTO (..),
    EventAdminInvitationTokenDTO (..),
    EventAdminMemberDTO (..),
    EventAdminRosterDTO (..),
    EventCourtesyTicketDTO (..),
    TicketOrderDTO,
 )
import TDF.Models (PartyId, partyDisplayName)
import TDF.Models.SocialEventsModels
import qualified TDF.Ticketing.Inventory as TicketInventory

type AppM = ReaderT Env Handler

-- | What the delegation rules need from the event handlers.
data EventAdminDeps = EventAdminDeps
    { parseVisibleEventKey :: T.Text -> AppM SocialEventId
    , parseInvitationKey :: T.Text -> AppM EventAdminInvitationId
    , parseTierKey :: T.Text -> AppM EventTicketTierId
    , lockSocialEventForMutation :: SocialEventId -> SqlPersistT IO (Maybe (Entity SocialEvent))
    , isImportedEventHidden :: SocialEventId -> SqlPersistT IO Bool
    , validateTicketPurchaseBuyerName :: Maybe T.Text -> Either ServerError (Maybe T.Text)
    , validateTicketPurchaseBuyerEmail :: Maybe T.Text -> Either ServerError (Maybe T.Text)
    , issueMissingTicketsForOrder :: UTCTime -> EventTicketOrderId -> EventTicketOrder -> SqlPersistT IO [T.Text]
    , ticketOrderEntityToDTO :: Entity EventTicketOrder -> [Entity EventTicket] -> TicketOrderDTO
    , sendTicketConfirmationForOrder :: EventTicketOrder -> [T.Text] -> AppM ()
    }

-- | The party whose organizer authority the caller exercises on this event:
-- the organizer for a delegated administrator, otherwise the caller. The
-- grant never names a strict administrator role and never outlives its row:
-- revoking it removes the access on the next request.
eventManagerPartyFor :: T.Text -> SocialEventId -> SocialEvent -> AppM T.Text
eventManagerPartyFor currentPartyId eventKey eventRow =
    case cleanMaybeText (socialEventOrganizerPartyId eventRow) of
        Just owner | owner /= currentPartyId -> do
            Env{..} <- ask
            delegated <- liftIO $ runSqlPool (delegatedEventAdminExists eventKey currentPartyId) envPool
            pure (if delegated then owner else currentPartyId)
        _ -> pure currentPartyId

eventAdminsServer :: T.Text -> EventAdminDeps -> ServerT EventAdminRoutes AppM
eventAdminsServer currentPartyId EventAdminDeps{..} =
        listEventAdmins
            :<|> getEventAdminAccess
            :<|> createEventAdminInvitation
            :<|> revokeEventAdminInvitation
            :<|> removeEventAdmin
            :<|> previewEventAdminInvitation
            :<|> acceptEventAdminInvitation
            :<|> issueCourtesyTickets
  where
    isDelegatedEventAdmin :: SocialEventId -> AppM Bool
    isDelegatedEventAdmin eventKey = do
        Env{..} <- ask
        liftIO $ runSqlPool (delegatedEventAdminExists eventKey currentPartyId) envPool

    loadEventForAdmins :: T.Text -> AppM (SocialEventId, SocialEvent)
    loadEventForAdmins rawEventId = do
        Env{..} <- ask
        eventKey <- parseVisibleEventKey rawEventId
        mEvent <- liftIO $ runSqlPool (get eventKey) envPool
        eventRow <- maybe (throwError err404{errBody = "Event not found"}) pure mEvent
        pure (eventKey, eventRow)

    -- Granting, revoking and inviting administrators stays with the
    -- organizer: a delegated administrator cannot extend their own grant.
    requireEventOrganizerForAdmins :: T.Text -> AppM (SocialEventId, SocialEvent)
    requireEventOrganizerForAdmins rawEventId = do
        (eventKey, eventRow) <- loadEventForAdmins rawEventId
        _ <- requireEventManager currentPartyId eventRow
        pure (eventKey, eventRow)

    eventAdminAccessFor :: SocialEventId -> SocialEvent -> AppM T.Text
    eventAdminAccessFor eventKey eventRow
        | isEventManager currentPartyId eventRow = pure "owner"
        | isNothing (cleanMaybeText (socialEventOrganizerPartyId eventRow)) = pure "none"
        | otherwise = do
            delegated <- isDelegatedEventAdmin eventKey
            pure (if delegated then "admin" else "none")

    getEventAdminAccess :: T.Text -> AppM EventAdminAccessDTO
    getEventAdminAccess rawEventId = do
        (eventKey, eventRow) <- loadEventForAdmins rawEventId
        access <- eventAdminAccessFor eventKey eventRow
        pure EventAdminAccessDTO{eaaEventId = renderKeyText eventKey, eaaAccess = access}

    listEventAdmins :: T.Text -> AppM EventAdminRosterDTO
    listEventAdmins rawEventId = do
        Env{..} <- ask
        (eventKey, eventRow) <- loadEventForAdmins rawEventId
        access <- eventAdminAccessFor eventKey eventRow
        when (access == "none") $ throwError eventNotOrganizerError
        now <- liftIO getCurrentTime
        (members, invitations) <- liftIO $ flip runSqlPool envPool $ do
            memberRows <- selectList [EventAdminMemberEventId ==. eventKey] [Asc EventAdminMemberId]
            memberDtos <- forM memberRows $ \(Entity _ member) -> do
                mParty <- case fromPathPiece (eventAdminMemberPartyId member) :: Maybe PartyId of
                    Nothing -> pure Nothing
                    Just partyKey -> get partyKey
                pure EventAdminMemberDTO
                    { eamPartyId = eventAdminMemberPartyId member
                    , eamDisplayName = partyDisplayName <$> mParty
                    , eamGrantedAt = eventAdminMemberCreatedAt member
                    }
            invitationRows <-
                if access == "owner"
                    then selectList
                        [EventAdminInvitationEventId ==. eventKey]
                        [Desc EventAdminInvitationId, LimitTo 50]
                    else pure []
            pure (memberDtos, map (eventAdminInvitationToDTO now) invitationRows)
        pure EventAdminRosterDTO
            { earAccess = access
            , earAdmins = members
            , earInvitations = invitations
            }

    createEventAdminInvitation
        :: T.Text -> EventAdminInvitationCreateDTO -> AppM EventAdminInvitationIssuedDTO
    createEventAdminInvitation rawEventId EventAdminInvitationCreateDTO{..} = do
        Env{..} <- ask
        (eventKey, _) <- requireEventOrganizerForAdmins rawEventId
        inviteeName <- either throwError pure (validateEventAdminInviteeName eaicInviteeName)
        now <- liftIO getCurrentTime
        token <- liftIO newEventAdminInvitationToken
        let invitation = EventAdminInvitation
                { eventAdminInvitationEventId = eventKey
                , eventAdminInvitationTokenSha256 = eventAdminInvitationTokenHash token
                , eventAdminInvitationInviteeName = inviteeName
                , eventAdminInvitationCreatedBy = currentPartyId
                , eventAdminInvitationCreatedAt = now
                , eventAdminInvitationExpiresAt = addUTCTime eventAdminInvitationLifetime now
                , eventAdminInvitationAcceptedBy = Nothing
                , eventAdminInvitationAcceptedAt = Nothing
                , eventAdminInvitationRevokedAt = Nothing
                }
        result <- liftIO $ flip runSqlPool envPool $ do
            -- The event lock orders this against a concurrent organizer change
            -- and bounds the links that can be pending at once.
            locked <- lockSocialEventForMutation eventKey
            case locked of
                Nothing -> pure (Left err404{errBody = "Event not found"})
                Just (Entity _ lockedEvent)
                    | not (isEventManager currentPartyId lockedEvent) ->
                        pure (Left eventNotOrganizerError)
                    | otherwise -> do
                        pending <- count
                            [ EventAdminInvitationEventId ==. eventKey
                            , EventAdminInvitationAcceptedAt ==. Nothing
                            , EventAdminInvitationRevokedAt ==. Nothing
                            , EventAdminInvitationExpiresAt >. now
                            ]
                        if pending >= maxPendingEventAdminInvitations
                            then pure (Left err409
                                { errBody = "Too many pending administrator invitations; revoke one first" })
                            else Right <$> insert invitation
        invitationKey <- either throwError pure result
        pure EventAdminInvitationIssuedDTO
            { eaisInvitation = eventAdminInvitationToDTO now (Entity invitationKey invitation)
            , eaisToken = token
            }

    revokeEventAdminInvitation :: T.Text -> T.Text -> AppM NoContent
    revokeEventAdminInvitation rawEventId rawInvitationId = do
        Env{..} <- ask
        (eventKey, _) <- requireEventOrganizerForAdmins rawEventId
        invitationKey <- parseInvitationKey rawInvitationId
        now <- liftIO getCurrentTime
        result <- liftIO $ flip runSqlPool envPool $ do
            locked <- lockSocialEventForMutation eventKey
            mInvitation <- get invitationKey
            case (locked, mInvitation) of
                (Just (Entity _ lockedEvent), Just invitation)
                    | not (isEventManager currentPartyId lockedEvent) ->
                        pure (Left eventNotOrganizerError)
                    | eventAdminInvitationEventId invitation /= eventKey ->
                        pure (Left err404{errBody = "Administrator invitation not found"})
                    | isJust (eventAdminInvitationAcceptedAt invitation) ->
                        pure (Left err409
                            { errBody = "This invitation was already accepted; remove the administrator instead" })
                    | isJust (eventAdminInvitationRevokedAt invitation) -> pure (Right ())
                    | otherwise -> do
                        update invitationKey [EventAdminInvitationRevokedAt =. Just now]
                        pure (Right ())
                _ -> pure (Left err404{errBody = "Administrator invitation not found"})
        either throwError (const (pure NoContent)) result

    removeEventAdmin :: T.Text -> T.Text -> AppM NoContent
    removeEventAdmin rawEventId rawPartyId = do
        Env{..} <- ask
        (eventKey, _) <- requireEventOrganizerForAdmins rawEventId
        result <- liftIO $ flip runSqlPool envPool $ do
            locked <- lockSocialEventForMutation eventKey
            case locked of
                Just (Entity _ lockedEvent)
                    | isEventManager currentPartyId lockedEvent -> do
                        mMember <- getBy (UniqueEventAdminMember eventKey (T.strip rawPartyId))
                        case mMember of
                            Nothing -> pure (Left err404{errBody = "Event administrator not found"})
                            Just (Entity memberKey _) -> Right <$> delete memberKey
                    | otherwise -> pure (Left eventNotOrganizerError)
                Nothing -> pure (Left err404{errBody = "Event not found"})
        either throwError (const (pure NoContent)) result

    -- One refusal for every unusable link, so a caller learns nothing about
    -- which links exist, were revoked, or were used by someone else.
    eventAdminInvitationRefused :: ServerError
    eventAdminInvitationRefused =
        err404{errBody = "This invitation link is invalid, expired, revoked or already used"}

    -- The invitation, its event and whether the caller may still accept it,
    -- read under the event lock so acceptance and revocation are ordered.
    lockUsableEventAdminInvitation
        :: UTCTime -> T.Text
        -> SqlPersistT IO (Either ServerError (Entity EventAdminInvitation, Entity SocialEvent))
    lockUsableEventAdminInvitation now token = do
        mFound <- getBy (UniqueEventAdminInvitationToken (eventAdminInvitationTokenHash token))
        case mFound of
            Nothing -> pure (Left eventAdminInvitationRefused)
            Just (Entity invitationKey found) -> do
                locked <- lockSocialEventForMutation (eventAdminInvitationEventId found)
                mInvitation <- get invitationKey
                hidden <- isImportedEventHidden (eventAdminInvitationEventId found)
                pure $ case (locked, mInvitation) of
                    (Just eventEntity@(Entity _ eventRow), Just invitation)
                        | hidden -> Left eventAdminInvitationRefused
                        | isJust (eventAdminInvitationRevokedAt invitation) ->
                            Left eventAdminInvitationRefused
                        | eventAdminInvitationExpiresAt invitation <= now
                        , isNothing (eventAdminInvitationAcceptedAt invitation) ->
                            Left eventAdminInvitationRefused
                        | maybe False (/= currentPartyId) (eventAdminInvitationAcceptedBy invitation) ->
                            Left eventAdminInvitationRefused
                        -- The link carries its issuer's authority and stops
                        -- working when the issuer no longer organizes the event.
                        | not (isEventManager (eventAdminInvitationCreatedBy invitation) eventRow) ->
                            Left eventAdminInvitationRefused
                        | otherwise -> Right (Entity invitationKey invitation, eventEntity)
                    _ -> Left eventAdminInvitationRefused

    previewEventAdminInvitation
        :: EventAdminInvitationTokenDTO -> AppM EventAdminInvitationPreviewDTO
    previewEventAdminInvitation EventAdminInvitationTokenDTO{..} = do
        Env{..} <- ask
        token <- either throwError pure (validateEventAdminInvitationToken eaitToken)
        now <- liftIO getCurrentTime
        result <- liftIO $ runSqlPool (lockUsableEventAdminInvitation now token) envPool
        (Entity _ invitation, Entity eventKey eventRow) <- either throwError pure result
        pure EventAdminInvitationPreviewDTO
            { eaipEventId = renderKeyText eventKey
            , eaipEventTitle = socialEventTitle eventRow
            , eaipEventStart = socialEventStartTime eventRow
            , eaipInviteeName = eventAdminInvitationInviteeName invitation
            , eaipExpiresAt = eventAdminInvitationExpiresAt invitation
            }

    acceptEventAdminInvitation :: EventAdminInvitationTokenDTO -> AppM EventAdminAccessDTO
    acceptEventAdminInvitation EventAdminInvitationTokenDTO{..} = do
        Env{..} <- ask
        token <- either throwError pure (validateEventAdminInvitationToken eaitToken)
        now <- liftIO getCurrentTime
        result <- liftIO $ flip runSqlPool envPool $ do
            usable <- lockUsableEventAdminInvitation now token
            case usable of
                Left refused -> pure (Left refused)
                Right (Entity invitationKey invitation, Entity eventKey eventRow)
                    | isEventManager currentPartyId eventRow ->
                        pure (Left err409
                            { errBody = "The organizer already manages this event; share the link with the invited person" })
                    | otherwise -> do
                        -- The link binds to the first account that accepts it;
                        -- the same account accepting again changes nothing.
                        when (isNothing (eventAdminInvitationAcceptedBy invitation)) $ do
                            update invitationKey
                                [ EventAdminInvitationAcceptedBy =. Just currentPartyId
                                , EventAdminInvitationAcceptedAt =. Just now
                                ]
                            void $ insertUnique EventAdminMember
                                { eventAdminMemberEventId = eventKey
                                , eventAdminMemberPartyId = currentPartyId
                                , eventAdminMemberGrantedBy = eventAdminInvitationCreatedBy invitation
                                , eventAdminMemberCreatedAt = now
                                }
                        -- A grant the organizer later removed is not restored
                        -- by replaying its consumed link.
                        stillAdmin <- delegatedEventAdminExists eventKey currentPartyId
                        pure $ if stillAdmin
                            then Right (renderKeyText eventKey)
                            else Left eventAdminInvitationRefused
        eventIdText <- either throwError pure result
        pure EventAdminAccessDTO{eaaEventId = eventIdText, eaaAccess = "admin"}

    -- A courtesy ticket is a zero-amount paid order issued by the event's
    -- organizer or delegated administrator to a named guest. It consumes tier
    -- inventory under the same locks as a purchase and never reaches checkout.
    issueCourtesyTickets :: T.Text -> EventCourtesyTicketDTO -> AppM TicketOrderDTO
    issueCourtesyTickets rawEventId EventCourtesyTicketDTO{..} = do
        Env{..} <- ask
        now <- liftIO getCurrentTime
        (eventKey, eventRow) <- loadEventForAdmins rawEventId
        managerParty <- eventManagerPartyFor currentPartyId eventKey eventRow
        _ <- requireEventManager managerParty eventRow
        tierKey <- parseTierKey ectTierId
        when (ectQuantity < 1 || ectQuantity > maxCourtesyTicketsPerOrder) $
            throwError err400{errBody = "Courtesy quantity must be between 1 and 10"}
        holderName <-
            either throwError pure (validateTicketPurchaseBuyerName (Just ectHolderName))
                >>= maybe (throwError err400{errBody = "Courtesy holder name is required"}) pure
        holderEmail <- either throwError pure (validateTicketPurchaseBuyerEmail ectHolderEmail)
        result <- liftIO $ flip runSqlPool envPool $ do
            locked <- lockSocialEventForMutation eventKey
            stillDelegated <- delegatedEventAdminExists eventKey currentPartyId
            case locked of
                Nothing -> pure (Left err404{errBody = "Event not found"})
                Just (Entity _ lockedEvent)
                    | not (isEventManager currentPartyId lockedEvent
                        || (stillDelegated && isEventManager managerParty lockedEvent)) ->
                        pure (Left eventNotOrganizerError)
                    | otherwise -> do
                        mTier <- get tierKey
                        case mTier of
                            Just tier
                                | eventTicketTierEventId tier == eventKey -> do
                                    let orderRecord = EventTicketOrder
                                            { eventTicketOrderEventId = eventKey
                                            , eventTicketOrderTierId = tierKey
                                            , eventTicketOrderBuyerPartyId = Nothing
                                            , eventTicketOrderBuyerName = Just holderName
                                            , eventTicketOrderBuyerEmail = holderEmail
                                            , eventTicketOrderQuantity = ectQuantity
                                            , eventTicketOrderAmountCents = 0
                                            , eventTicketOrderCurrency = eventTicketTierCurrency tier
                                            , eventTicketOrderStatus = "paid"
                                            , eventTicketOrderMetadata = Just (courtesyOrderMetadata currentPartyId)
                                            , eventTicketOrderCheckoutIdempotencyKey = Nothing
                                            , eventTicketOrderPurchasedAt = now
                                            , eventTicketOrderStripePaymentIntentId = Nothing
                                            , eventTicketOrderPromoCodeId = Nothing
                                            , eventTicketOrderOriginalAmountCents = Nothing
                                            , eventTicketOrderPaymentMethod = Nothing
                                            , eventTicketOrderCreatedAt = now
                                            , eventTicketOrderUpdatedAt = now
                                            }
                                    admissionTime <- TicketInventory.ticketReservationTime
                                    reserved <- TicketInventory.reserveTicketInventory
                                        eventKey tierKey ectQuantity admissionTime
                                    if not (isTicketTierSaleOpen admissionTime tier)
                                        then pure (Left err409
                                            { errBody = "Courtesy tickets need an active ticket tier inside its sales window" })
                                    else if not reserved
                                        then pure (Left err409{errBody = "Not enough tickets available"})
                                        else do
                                            orderKey <- insert orderRecord
                                            codes <- issueMissingTicketsForOrder now orderKey orderRecord
                                            tickets <- selectList
                                                [EventTicketOrderRefId ==. orderKey]
                                                [Asc EventTicketId]
                                            pure (Right
                                                ( ticketOrderEntityToDTO (Entity orderKey orderRecord) tickets
                                                , orderRecord
                                                , codes
                                                ))
                            _ -> pure (Left err404{errBody = "Ticket tier not found"})
        (orderDto, orderRecord, ticketCodes) <- either throwError pure result
        sendTicketConfirmationForOrder orderRecord ticketCodes
        pure orderDto

isEventManager :: T.Text -> SocialEvent -> Bool
isEventManager currentParty eventRow =
    case cleanMaybeText (socialEventOrganizerPartyId eventRow) of
        Nothing -> False
        Just owner -> owner == currentParty

eventWithoutOrganizerError :: ServerError
eventWithoutOrganizerError =
    err403{errBody = "This event has no organizer; \
                    \a strict administrator must assign one by editing the event"}

eventNotOrganizerError :: ServerError
eventNotOrganizerError =
    err403{errBody = "Only the event organizer can manage this event"}

-- An event without an organizer is never taken by whoever reaches it first:
-- organizer-only routes refuse it and write nothing.
requireEventManager :: T.Text -> SocialEvent -> AppM SocialEvent
requireEventManager currentParty eventRow =
    case cleanMaybeText (socialEventOrganizerPartyId eventRow) of
        Just owner | owner == currentParty -> pure eventRow
        Just _ -> throwError eventNotOrganizerError
        Nothing -> throwError eventWithoutOrganizerError

isTicketTierSaleOpen :: UTCTime -> EventTicketTier -> Bool
isTicketTierSaleOpen now tier =
    eventTicketTierIsActive tier
        && maybe True (<= now) (eventTicketTierSalesStart tier)
        && maybe True (>= now) (eventTicketTierSalesEnd tier)

renderKeyText :: (ToBackendKey SqlBackend record) => Key record -> T.Text
renderKeyText = T.pack . show . fromSqlKey

cleanMaybeText :: Maybe T.Text -> Maybe T.Text
cleanMaybeText mVal =
    case fmap T.strip mVal of
        Just txt | not (T.null txt) -> Just txt
        _ -> Nothing

eventAdminInvitationLifetime :: NominalDiffTime
eventAdminInvitationLifetime = 14 * 86400

maxPendingEventAdminInvitations :: Int
maxPendingEventAdminInvitations = 10

maxCourtesyTicketsPerOrder :: Int
maxCourtesyTicketsPerOrder = 10

-- 244 random bits from two version-4 UUIDs, rendered as 64 hex characters.
newEventAdminInvitationToken :: IO T.Text
newEventAdminInvitationToken = do
    left <- UUIDV4.nextRandom
    right <- UUIDV4.nextRandom
    pure (T.replace "-" "" (UUID.toText left <> UUID.toText right))

eventAdminInvitationTokenHash :: T.Text -> T.Text
eventAdminInvitationTokenHash token =
    T.pack (show (CryptoHash.hash (TE.encodeUtf8 token) :: CryptoHash.Digest CryptoHash.SHA256))

validateEventAdminInvitationToken :: T.Text -> Either ServerError T.Text
validateEventAdminInvitationToken rawToken
    | T.length token == 64 && T.all (\c -> isAscii c && isHexDigit c) token = Right (T.toLower token)
    | otherwise =
        Left err404{errBody = "This invitation link is invalid, expired, revoked or already used"}
  where
    token = T.strip rawToken

validateEventAdminInviteeName :: T.Text -> Either ServerError T.Text
validateEventAdminInviteeName rawName
    | T.null name = Left err400{errBody = "Invitee name is required"}
    | T.length name > 120 = Left err400{errBody = "Invitee name must be 120 characters or fewer"}
    | T.any isControl name = Left err400{errBody = "Invitee name must not contain control characters"}
    | otherwise = Right name
  where
    name = T.strip rawName

eventAdminInvitationToDTO :: UTCTime -> Entity EventAdminInvitation -> EventAdminInvitationDTO
eventAdminInvitationToDTO now (Entity key invitation) = EventAdminInvitationDTO
    { eaiId = renderKeyText key
    , eaiInviteeName = eventAdminInvitationInviteeName invitation
    , eaiStatus = status
    , eaiCreatedAt = eventAdminInvitationCreatedAt invitation
    , eaiExpiresAt = eventAdminInvitationExpiresAt invitation
    , eaiAcceptedAt = eventAdminInvitationAcceptedAt invitation
    }
  where
    status
        | isJust (eventAdminInvitationAcceptedAt invitation) = "accepted"
        | isJust (eventAdminInvitationRevokedAt invitation) = "revoked"
        | eventAdminInvitationExpiresAt invitation <= now = "expired"
        | otherwise = "pending"

courtesyOrderMetadata :: T.Text -> T.Text
courtesyOrderMetadata issuerParty =
    TE.decodeUtf8 . BL.toStrict . Aeson.encode $
        Aeson.object ["courtesy" Aeson..= True, "courtesyIssuedBy" Aeson..= issuerParty]

-- Whether the organizer has delegated this event's administration to a party.
delegatedEventAdminExists :: SocialEventId -> T.Text -> SqlPersistT IO Bool
delegatedEventAdminExists eventKey party =
    isJust <$> getBy (UniqueEventAdminMember eventKey party)
