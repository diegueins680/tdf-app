-- | Wire view of an event's ticket tier, as managers see it.
module TDF.Ticketing.TierView
    ( ticketTierEntityToDTO
    ) where

import qualified Data.Text as T
import Database.Persist (Entity (..))
import Database.Persist.Sql (fromSqlKey)

import TDF.DTO.SocialEventsDTO (TicketTierDTO (..))
import TDF.Models.SocialEventsModels

ticketTierEntityToDTO :: SocialEventId -> Entity EventTicketTier -> TicketTierDTO
ticketTierEntityToDTO eventKey (Entity tierKey tier) =
    TicketTierDTO
        { ticketTierId = Just (T.pack (show (fromSqlKey tierKey)))
        , ticketTierEventId = Just (T.pack (show (fromSqlKey eventKey)))
        , ticketTierCode = eventTicketTierCode tier
        , ticketTierName = eventTicketTierName tier
        , ticketTierDescription = eventTicketTierDescription tier
        , ticketTierPriceCents = eventTicketTierPriceCents tier
        , ticketTierCurrency = eventTicketTierCurrency tier
        , ticketTierQuantityTotal = eventTicketTierQuantityTotal tier
        , ticketTierQuantitySold = eventTicketTierQuantitySold tier
        , ticketTierSalesStart = eventTicketTierSalesStart tier
        , ticketTierSalesEnd = eventTicketTierSalesEnd tier
        , ticketTierActive = eventTicketTierIsActive tier
        , ticketTierPosition = eventTicketTierPosition tier
        , ticketTierRegistersParticipant = Just (eventTicketTierRegistersParticipant tier)
        }
