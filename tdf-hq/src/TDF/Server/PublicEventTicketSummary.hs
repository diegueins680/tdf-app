{-# LANGUAGE OverloadedStrings #-}

-- | What a public event card may say about tickets: a starting price while
-- something is on sale, or that the event sold out. Uses the public ticket
-- storefront's tier rules so the card and the checkout agree.
module TDF.Server.PublicEventTicketSummary (
    PublicTicketCardSummary (..),
    summarizePublicTicketTiers,
    loadPublicTicketCardSummary,
) where

import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (entityVal, selectList, (==.))
import Database.Persist.Sql (SqlPersistT)

import TDF.Models.SocialEventsModels
import TDF.Server.EventAdminDelegation (isTicketTierSaleOpen)

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
    eventOpen = maybe True (now <) eventEnd
    onSale =
        [ tier
        | eventOpen
        , tier <- tiers
        , eventTicketTierPriceCents tier > 0
        , isTicketTierSaleOpen now tier
        ]
    totalSold = sum (map eventTicketTierQuantitySold tiers)
    eventRemaining = (\limit -> max 0 (limit - totalSold)) <$> capacity
    remaining tier =
        let tierRemaining = max 0 (eventTicketTierQuantityTotal tier - eventTicketTierQuantitySold tier)
         in maybe tierRemaining (min tierRemaining) eventRemaining
    inStock = filter ((> 0) . remaining) onSale
