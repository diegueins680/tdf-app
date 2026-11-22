{-# LANGUAGE OverloadedStrings #-}

-- | What a public event card may say about tickets: a starting price while
-- something is on sale, or that the event sold out. Uses the public ticket
-- storefront's tier rules so the card and the checkout agree.
module TDF.Server.PublicEventTicketSummary (
    PublicEventCardMetadata (..),
    PublicTicketCardSummary (..),
    loadPublicEventCard,
    summarizePublicTicketTiers,
    loadPublicTicketCardSummary,
) where

import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (Entity (..), entityVal, get, selectList, (==.))
import Database.Persist.Sql (SqlPersistT, fromSqlKey)

import TDF.DTO.SocialEventsDTO (PublicUpcomingEventDTO (..))
import TDF.Models.SocialEventsModels
import qualified TDF.Models.SocialEventsModels as SM
import TDF.Server.EventAdminDelegation (isTicketTierSaleOpen)
import TDF.Server.TicketSalesWindow (ticketSalesOpenBeforeEventEnd)
import qualified TDF.SocialEventLifecycle as EventLifecycle

-- The existing strict event-metadata decoder remains the only parser. This
-- projection receives only the four public fields it needs, avoiding a cycle
-- with the larger event handlers and retaining their failure semantics.
data PublicEventCardMetadata = PublicEventCardMetadata
    { pecmIsPublic :: Maybe Bool
    , pecmCurrency :: Maybe T.Text
    , pecmTicketUrl :: Maybe T.Text
    , pecmImageUrl :: Maybe T.Text
    }

loadPublicEventCard :: Maybe T.Text -> Entity SocialEvent -> Either T.Text PublicEventCardMetadata
    -> SqlPersistT IO (Maybe PublicUpcomingEventDTO)
loadPublicEventCard canonicalCity (Entity eventKey eventRow) metadataResult = do
    venue <- maybe (pure Nothing) get (socialEventVenueId eventRow)
    case (metadataResult, socialEventWorkflowStateId eventRow) of
        (Right metadata, Just workflowStateId) -> do
            workflow <- EventLifecycle.loadActiveSocialEventState workflowStateId
            publicListable <- EventLifecycle.socialEventStateHasCapability workflowStateId "public-listable"
            ticketSummary <- loadPublicTicketCardSummary eventKey eventRow
            pure $
                if publicListable && pecmIsPublic metadata /= Just False
                    then do
                        (stateCode, _, _) <- workflow
                        Just PublicUpcomingEventDTO
                            { publicUpcomingEventId = T.pack (show (fromSqlKey eventKey))
                            , publicUpcomingEventTitle = socialEventTitle eventRow
                            , publicUpcomingEventDescription = socialEventDescription eventRow
                            , publicUpcomingEventStart = socialEventStartTime eventRow
                            , publicUpcomingEventEnd = socialEventEndTime eventRow
                            , publicUpcomingEventTimezone = socialEventTimezone eventRow
                            , publicUpcomingEventVenueName = SM.venueName <$> venue
                            , publicUpcomingEventCity = canonicalCity
                            , publicUpcomingEventPriceCents = socialEventPriceCents eventRow
                            , publicUpcomingEventCurrency = pecmCurrency metadata
                            , publicUpcomingEventTicketUrl = pecmTicketUrl metadata
                            , publicUpcomingEventImageUrl = pecmImageUrl metadata
                            , publicUpcomingEventWorkflowStateCode = stateCode
                            , publicUpcomingEventTicketFromPriceCents = ptcsFromPriceCents ticketSummary
                            , publicUpcomingEventTicketCurrency = ptcsCurrency ticketSummary
                            , publicUpcomingEventTicketsSoldOut = ptcsSoldOut ticketSummary
                            }
                    else Nothing
        _ -> pure Nothing

data PublicTicketCardSummary = PublicTicketCardSummary
    { ptcsFromPriceCents :: Maybe Int
    , ptcsCurrency :: Maybe T.Text
    , ptcsSoldOut :: Bool
    }
    deriving (Show, Eq)

loadPublicTicketCardSummary :: SocialEventId -> SocialEvent -> SqlPersistT IO PublicTicketCardSummary
loadPublicTicketCardSummary eventKey eventRow = do
    now <- liftIO getCurrentTime
    tiers <- map entityVal <$> selectList [EventTicketTierEventId ==. eventKey] []
    pure (summarizePublicTicketTiers now (socialEventEndTime eventRow) (socialEventCapacity eventRow) tiers)

-- | Summarize ticket tiers for a public card with the storefront's own rules:
-- a tier counts when it is active, priced, inside its sale window, and the
-- event has not ended. Remaining stock is capped by the event capacity.
-- The card never infers "free" or "sold out" from a reference price.
summarizePublicTicketTiers :: UTCTime -> Maybe UTCTime -> Maybe Int -> [EventTicketTier] -> PublicTicketCardSummary
summarizePublicTicketTiers now eventEnd capacity tiers =
    case inStock of
        [] -> PublicTicketCardSummary Nothing Nothing (not (null onSale))
        _ -> case nub (map (T.toUpper . eventTicketTierCurrency) inStock) of
            [currency] ->
                PublicTicketCardSummary (Just (minimum (map eventTicketTierPriceCents inStock))) (Just currency) False
            _ -> PublicTicketCardSummary Nothing Nothing False
  where
    eventOpen = ticketSalesOpenBeforeEventEnd now eventEnd
    onSale =
        [ tier
        | eventOpen
        , tier <- tiers
        , eventTicketTierPriceCents tier > 0
        , isTicketTierSaleOpen now tier
        ]
    -- Aggregate with unbounded integers: several individually valid tier
    -- counts can exceed Int without creating inventory for a full event.
    totalSold = sum (map (toInteger . eventTicketTierQuantitySold) tiers)
    eventRemaining = (\limit -> max 0 (toInteger limit - totalSold)) <$> capacity
    remaining tier =
        let tierRemaining = max 0
                (toInteger (eventTicketTierQuantityTotal tier) - toInteger (eventTicketTierQuantitySold tier))
         in maybe tierRemaining (min tierRemaining) eventRemaining
    inStock = filter ((> 0) . remaining) onSale
