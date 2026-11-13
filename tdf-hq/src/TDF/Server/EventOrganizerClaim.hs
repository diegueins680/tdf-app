{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | EVT-ORGANIZER-CLAIM-001: an event without an organizer can be claimed the
-- way an artist profile without an account is claimed. A signed-in account
-- submits evidence; a strict administrator other than the claimant decides;
-- an approval makes the claimant the organizer through the one conditional
-- assignment of EVT-OWNER-CLAIM-001. Submitting a claim grants nothing.
module TDF.Server.EventOrganizerClaim (
    EventOrganizerClaimDeps (..),
    eventOrganizerClaimsServer,
    validateEventOrganizerClaimEvidence,
    validateEventOrganizerClaimNotes,
    maxOpenEventOrganizerClaimsPerAccount,
    maxOpenEventOrganizerClaimsPerEvent,
) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl)
import Data.Maybe (catMaybes, isJust, isNothing)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sql
import Servant
import Web.PathPieces (fromPathPiece)

import TDF.API.SocialEventsAPI (EventOrganizerClaimRoutes)
import TDF.DB (Env (..))
import TDF.DTO.SocialEventsDTO (
    EventOrganizerClaimCreateDTO (..),
    EventOrganizerClaimDTO (..),
    EventOrganizerClaimDecisionDTO (..),
    EventOrganizerClaimReviewDTO (..),
    EventOrganizerClaimStateDTO (..),
 )
import TDF.Models (PartyId, partyDisplayName)
import TDF.Models.SocialEventsModels

type AppM = ReaderT Env Handler

-- | What the claim rules need from the event handlers: the visibility check,
-- the event row lock and the single assignment of a missing organizer.
data EventOrganizerClaimDeps = EventOrganizerClaimDeps
    { claimParseVisibleEventKey :: T.Text -> AppM SocialEventId
    , claimLockEvent :: SocialEventId -> SqlPersistT IO (Maybe (Entity SocialEvent))
    , claimAssignOrganizer
        :: T.Text -> UTCTime -> SocialEventId -> SqlPersistT IO (Either ServerError SocialEvent)
    }

eventOrganizerClaimsServer
    :: T.Text -> Bool -> EventOrganizerClaimDeps -> ServerT EventOrganizerClaimRoutes AppM
eventOrganizerClaimsServer currentPartyId strictAdmin EventOrganizerClaimDeps{..} =
    getClaimState
        :<|> submitClaim
        :<|> listClaims
        :<|> decideClaim
  where
    runSql :: SqlPersistT IO a -> AppM a
    runSql action = do
        Env{..} <- ask
        liftIO (runSqlPool action envPool)

    -- The caller's most recent claim on the event, and whether the event can
    -- still be claimed. Discloses no other account's claim.
    getClaimState :: T.Text -> AppM EventOrganizerClaimStateDTO
    getClaimState rawEventId = do
        eventKey <- claimParseVisibleEventKey rawEventId
        (mEvent, mClaim) <- runSql $ do
            eventRow <- get eventKey
            claim <- selectFirst
                [ EventOrganizerClaimEventId ==. eventKey
                , EventOrganizerClaimClaimantPartyId ==. currentPartyId
                ]
                [Desc EventOrganizerClaimId]
            pure (eventRow, claim)
        eventRow <- maybe (throwError eventNotFound) pure mEvent
        pure EventOrganizerClaimStateDTO
            { eocsEventId = renderKeyText eventKey
            , eocsClaimable = eventLacksOrganizer eventRow
            , eocsClaim = claimToDTO <$> mClaim
            }

    submitClaim :: T.Text -> EventOrganizerClaimCreateDTO -> AppM EventOrganizerClaimDTO
    submitClaim rawEventId EventOrganizerClaimCreateDTO{..} = do
        eventKey <- claimParseVisibleEventKey rawEventId
        evidence <- either throwError pure (validateEventOrganizerClaimEvidence eoccEvidence)
        now <- liftIO getCurrentTime
        result <- runSql $ do
            -- The event lock orders this against an assignment and against a
            -- second submission by the same account.
            locked <- claimLockEvent eventKey
            case locked of
                Nothing -> pure (Left eventNotFound)
                Just (Entity _ eventRow)
                    | not (eventLacksOrganizer eventRow) -> pure (Left eventHasOrganizer)
                    | otherwise -> do
                        existing <- selectFirst
                            [ EventOrganizerClaimEventId ==. eventKey
                            , EventOrganizerClaimClaimantPartyId ==. currentPartyId
                            , EventOrganizerClaimStatus ==. statusSubmitted
                            ]
                            []
                        case existing of
                            -- A repeated submission answers with the open claim
                            -- and keeps the evidence first recorded.
                            Just claim -> pure (Right claim)
                            Nothing -> do
                                ownOpen <- count
                                    [ EventOrganizerClaimClaimantPartyId ==. currentPartyId
                                    , EventOrganizerClaimStatus ==. statusSubmitted
                                    ]
                                eventOpen <- count
                                    [ EventOrganizerClaimEventId ==. eventKey
                                    , EventOrganizerClaimStatus ==. statusSubmitted
                                    ]
                                if ownOpen >= maxOpenEventOrganizerClaimsPerAccount
                                    then pure (Left tooManyOwnClaims)
                                    else if eventOpen >= maxOpenEventOrganizerClaimsPerEvent
                                        then pure (Left tooManyEventClaims)
                                        else do
                                            let claim = EventOrganizerClaim
                                                    { eventOrganizerClaimEventId = eventKey
                                                    , eventOrganizerClaimClaimantPartyId = currentPartyId
                                                    , eventOrganizerClaimEvidence = evidence
                                                    , eventOrganizerClaimStatus = statusSubmitted
                                                    , eventOrganizerClaimSubmittedAt = now
                                                    , eventOrganizerClaimReviewerPartyId = Nothing
                                                    , eventOrganizerClaimReviewerNotes = Nothing
                                                    , eventOrganizerClaimReviewedAt = Nothing
                                                    }
                                            claimKey <- insert claim
                                            pure (Right (Entity claimKey claim))
        claimToDTO <$> either throwError pure result

    requireReviewer :: AppM ()
    requireReviewer =
        unless strictAdmin $
            throwError err403{errBody = "Organizer claims are reviewed by strict administrators"}

    -- Open claims oldest first, then the most recent decisions.
    listClaims :: AppM [EventOrganizerClaimReviewDTO]
    listClaims = do
        requireReviewer
        runSql $ do
            open <- selectList
                [EventOrganizerClaimStatus ==. statusSubmitted]
                [Asc EventOrganizerClaimSubmittedAt, Asc EventOrganizerClaimId, LimitTo 200]
            decided <- selectList
                [EventOrganizerClaimStatus !=. statusSubmitted]
                [Desc EventOrganizerClaimReviewedAt, Desc EventOrganizerClaimId, LimitTo 50]
            catMaybes <$> mapM claimToReviewDTO (open <> decided)

    decideClaim :: T.Text -> EventOrganizerClaimDecisionDTO -> AppM EventOrganizerClaimReviewDTO
    decideClaim rawClaimId EventOrganizerClaimDecisionDTO{..} = do
        requireReviewer
        claimKey <- maybe (throwError claimNotFound) pure
            (fromPathPiece (T.strip rawClaimId) :: Maybe EventOrganizerClaimId)
        decision <- case T.strip eocdDecision of
            "approved" -> pure statusApproved
            "rejected" -> pure statusRejected
            _ -> throwError err400{errBody = "Decision must be approved or rejected"}
        notes <- either throwError pure (validateEventOrganizerClaimNotes eocdNotes)
        now <- liftIO getCurrentTime
        result <- runSql $ do
            found <- get claimKey
            case found of
                Nothing -> pure (Left claimNotFound)
                Just unlocked -> do
                    let eventKey = eventOrganizerClaimEventId unlocked
                    -- Every decision on an event's claims takes the event lock
                    -- first, so the claim is judged as stored under it.
                    locked <- claimLockEvent eventKey
                    current <- get claimKey
                    outcome <- case (locked, current) of
                        (Just (Entity _ eventRow), Just claim) ->
                            applyDecision now decision notes eventKey eventRow claimKey claim
                        _ -> pure (Left claimNotFound)
                    case outcome of
                        Left refused -> pure (Left refused)
                        Right () -> do
                            stored <- get claimKey
                            review <- maybe (pure Nothing) (claimToReviewDTO . Entity claimKey) stored
                            pure (maybe (Left claimNotFound) Right review)
        either throwError pure result

    applyDecision
        :: UTCTime -> T.Text -> T.Text -> SocialEventId -> SocialEvent
        -> EventOrganizerClaimId -> EventOrganizerClaim
        -> SqlPersistT IO (Either ServerError ())
    applyDecision now decision notes eventKey eventRow claimKey claim
        | claimant == currentPartyId =
            pure (Left err403{errBody = "Claim review requires a distinct administrator"})
        -- Repeating the recorded decision reads it back and writes nothing.
        | eventOrganizerClaimStatus claim == decision = pure (Right ())
        | eventOrganizerClaimStatus claim /= statusSubmitted =
            pure (Left err409{errBody = "This organizer claim was already decided"})
        | decision == statusRejected = Right <$> recordDecision statusRejected
        | not (eventLacksOrganizer eventRow) = pure (Left eventHasOrganizer)
        | otherwise = do
            claimantExists <- case fromPathPiece claimant :: Maybe PartyId of
                Nothing -> pure False
                Just partyKey -> isJust <$> get partyKey
            if not claimantExists
                then pure (Left err409{errBody = "The claimant account no longer exists; reject the claim"})
                else do
                    assigned <- claimAssignOrganizer claimant now eventKey
                    case assigned of
                        Left refused -> pure (Left refused)
                        Right _ -> do
                            recordDecision statusApproved
                            -- The event now has an organizer: the other open
                            -- claims on it can no longer be approved.
                            updateWhere
                                [ EventOrganizerClaimEventId ==. eventKey
                                , EventOrganizerClaimStatus ==. statusSubmitted
                                ]
                                [ EventOrganizerClaimStatus =. statusSuperseded
                                , EventOrganizerClaimReviewerPartyId =. Just currentPartyId
                                , EventOrganizerClaimReviewerNotes
                                    =. Just "Another claim for this event was approved"
                                , EventOrganizerClaimReviewedAt =. Just now
                                ]
                            pure (Right ())
      where
        claimant = eventOrganizerClaimClaimantPartyId claim
        recordDecision status =
            update claimKey
                [ EventOrganizerClaimStatus =. status
                , EventOrganizerClaimReviewerPartyId =. Just currentPartyId
                , EventOrganizerClaimReviewerNotes =. Just notes
                , EventOrganizerClaimReviewedAt =. Just now
                ]

statusSubmitted, statusApproved, statusRejected, statusSuperseded :: T.Text
statusSubmitted = "submitted"
statusApproved = "approved"
statusRejected = "rejected"
statusSuperseded = "superseded"

-- | Pending claims one account may hold across all events. The count is read
-- under the lock of the event being claimed only, so simultaneous submissions
-- for different events can exceed it by the number of requests in flight.
maxOpenEventOrganizerClaimsPerAccount :: Int
maxOpenEventOrganizerClaimsPerAccount = 5

-- | Pending claims one event may hold; bounds the review queue per event.
maxOpenEventOrganizerClaimsPerEvent :: Int
maxOpenEventOrganizerClaimsPerEvent = 20

eventNotFound, claimNotFound, eventHasOrganizer, tooManyOwnClaims, tooManyEventClaims :: ServerError
tooManyOwnClaims = err409{errBody = "Too many pending organizer claims; wait for a decision first"}
tooManyEventClaims = err409{errBody = "This event already has too many pending organizer claims"}
eventNotFound = err404{errBody = "Event not found"}
claimNotFound = err404{errBody = "Organizer claim not found"}
eventHasOrganizer = err409{errBody = "This event already has an organizer"}

-- The same rule every organizer check uses: absent, or only whitespace.
eventLacksOrganizer :: SocialEvent -> Bool
eventLacksOrganizer eventRow =
    isNothing (cleanMaybeText (socialEventOrganizerPartyId eventRow))

validateEventOrganizerClaimEvidence :: T.Text -> Either ServerError T.Text
validateEventOrganizerClaimEvidence =
    validateClaimText "Evidence must be between 20 and 2000 characters" 20

validateEventOrganizerClaimNotes :: T.Text -> Either ServerError T.Text
validateEventOrganizerClaimNotes =
    validateClaimText "Review notes must be between 10 and 2000 characters" 10

validateClaimText :: BL.ByteString -> Int -> T.Text -> Either ServerError T.Text
validateClaimText lengthMessage minLength raw
    | T.length text < minLength || T.length text > 2000 = Left err400{errBody = lengthMessage}
    | T.any (\c -> isControl c && c `notElem` ['\n', '\r', '\t']) text =
        Left err400{errBody = "Text must not contain control characters"}
    | otherwise = Right text
  where
    text = T.strip raw

claimToDTO :: Entity EventOrganizerClaim -> EventOrganizerClaimDTO
claimToDTO (Entity key claim) = EventOrganizerClaimDTO
    { eocId = renderKeyText key
    , eocEventId = renderKeyText (eventOrganizerClaimEventId claim)
    , eocStatus = eventOrganizerClaimStatus claim
    , eocSubmittedAt = eventOrganizerClaimSubmittedAt claim
    , eocReviewedAt = eventOrganizerClaimReviewedAt claim
    }

claimToReviewDTO :: Entity EventOrganizerClaim -> SqlPersistT IO (Maybe EventOrganizerClaimReviewDTO)
claimToReviewDTO (Entity key claim) = do
    mEvent <- get (eventOrganizerClaimEventId claim)
    mParty <- case fromPathPiece (eventOrganizerClaimClaimantPartyId claim) :: Maybe PartyId of
        Nothing -> pure Nothing
        Just partyKey -> get partyKey
    pure $ flip fmap mEvent $ \eventRow -> EventOrganizerClaimReviewDTO
        { eocrId = renderKeyText key
        , eocrEventId = renderKeyText (eventOrganizerClaimEventId claim)
        , eocrEventTitle = socialEventTitle eventRow
        , eocrEventStart = socialEventStartTime eventRow
        , eocrEventHasOrganizer = not (eventLacksOrganizer eventRow)
        , eocrClaimantPartyId = eventOrganizerClaimClaimantPartyId claim
        , eocrClaimantName = partyDisplayName <$> mParty
        , eocrEvidence = eventOrganizerClaimEvidence claim
        , eocrStatus = eventOrganizerClaimStatus claim
        , eocrSubmittedAt = eventOrganizerClaimSubmittedAt claim
        , eocrReviewerPartyId = eventOrganizerClaimReviewerPartyId claim
        , eocrReviewerNotes = eventOrganizerClaimReviewerNotes claim
        , eocrReviewedAt = eventOrganizerClaimReviewedAt claim
        }

renderKeyText :: (ToBackendKey SqlBackend record) => Key record -> T.Text
renderKeyText = T.pack . show . fromSqlKey

cleanMaybeText :: Maybe T.Text -> Maybe T.Text
cleanMaybeText mVal =
    case fmap T.strip mVal of
        Just txt | not (T.null txt) -> Just txt
        _ -> Nothing
