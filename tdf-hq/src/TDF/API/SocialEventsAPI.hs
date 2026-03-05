{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.SocialEventsAPI
  ( SocialEventsAPI
  , EventsRoutes
  , VenuesRoutes
  , ArtistsRoutes
  , RsvpRoutes
  , InvitationsRoutes
  , TicketsRoutes
  , BudgetRoutes
  , FinanceRoutes
  , IdParam
  , EventImageUploadForm(..)
  , EventImageUploadDTO(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Servant
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant.Multipart (FileData, FromMultipart(..), MultipartForm, Tmp, lookupFile, lookupInput)

import TDF.DTO.SocialEventsDTO
  ( EventDTO
  , VenueDTO
  , ArtistDTO
  , ArtistFollowerDTO
  , ArtistFollowRequest
  , RsvpDTO
  , InvitationDTO
  , TicketTierDTO
  , TicketPurchaseRequestDTO
  , TicketOrderStatusUpdateDTO
  , TicketCheckInRequestDTO
  , TicketDTO
  , TicketOrderDTO
  , EventBudgetLineDTO
  , EventFinanceEntryDTO
  , EventFinanceSummaryDTO
  )

type IdParam = Capture "id" Text

data EventImageUploadForm = EventImageUploadForm
  { eiuFile :: FileData Tmp
  , eiuName :: Maybe Text
  }

instance FromMultipart Tmp EventImageUploadForm where
  fromMultipart multipart = do
    file <- lookupFile "file" multipart
    let mName =
          case lookupInput "name" multipart of
            Right v ->
              let trimmed = T.strip v
              in if T.null trimmed then Nothing else Just trimmed
            _ -> Nothing
    pure EventImageUploadForm
      { eiuFile = file
      , eiuName = mName
      }

data EventImageUploadDTO = EventImageUploadDTO
  { eiuEventId   :: Text
  , eiuFileName  :: Text
  , eiuPath      :: Text
  , eiuPublicUrl :: Text
  , eiuImageUrl  :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON EventImageUploadDTO
instance FromJSON EventImageUploadDTO

type EventsRoutes =
       "events"
         :> QueryParam "city" Text
         :> QueryParam "start_after" Text
         :> QueryParam "event_type" Text
         :> QueryParam "event_status" Text
         :> QueryParam "artistId" Text
         :> QueryParam "venueId" Text
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Get '[JSON] [EventDTO]
  :<|> "events" :> ReqBody '[JSON] EventDTO :> Post '[JSON] EventDTO
  :<|> "events" :> IdParam :> Get '[JSON] EventDTO
  :<|> "events" :> IdParam :> ReqBody '[JSON] EventDTO :> Put '[JSON] EventDTO
  :<|> "events" :> IdParam :> "image" :> MultipartForm Tmp EventImageUploadForm :> Post '[JSON] EventImageUploadDTO
  :<|> "events" :> IdParam :> DeleteNoContent

type VenuesRoutes =
       "venues" :> QueryParam "city" Text :> QueryParam "near" Text :> QueryParam "q" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [VenueDTO]
  :<|> "venues" :> ReqBody '[JSON] VenueDTO :> Post '[JSON] VenueDTO
  :<|> "venues" :> IdParam :> Get '[JSON] VenueDTO
  :<|> "venues" :> IdParam :> ReqBody '[JSON] VenueDTO :> Put '[JSON] VenueDTO

type ArtistsRoutes =
       "artists" :> QueryParam "name" Text :> QueryParam "genre" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [ArtistDTO]
  :<|> "artists" :> ReqBody '[JSON] ArtistDTO :> Post '[JSON] ArtistDTO
  :<|> "artists" :> IdParam :> Get '[JSON] ArtistDTO
  :<|> "artists" :> IdParam :> ReqBody '[JSON] ArtistDTO :> Put '[JSON] ArtistDTO
  :<|> "artists" :> Capture "artistId" Text :> "followers" :> Get '[JSON] [ArtistFollowerDTO]
  :<|> "artists" :> Capture "artistId" Text :> "follow" :> ReqBody '[JSON] ArtistFollowRequest :> Post '[JSON] ArtistFollowerDTO
  :<|> "artists" :> Capture "artistId" Text :> "follow" :> QueryParam "follower" Text :> DeleteNoContent

type RsvpRoutes =
       "events" :> Capture "eventId" Text :> "rsvps" :> Get '[JSON] [RsvpDTO]
  :<|> "events" :> Capture "eventId" Text :> "rsvps" :> ReqBody '[JSON] RsvpDTO :> Post '[JSON] RsvpDTO

type InvitationsRoutes =
       "events" :> Capture "eventId" Text :> "invitations" :>
         ( Get '[JSON] [InvitationDTO]
      :<|> ReqBody '[JSON] InvitationDTO :> Post '[JSON] InvitationDTO
      :<|> Capture "invitationId" Text :> ReqBody '[JSON] InvitationDTO :> Put '[JSON] InvitationDTO
         )

type TicketsRoutes =
       "events" :> Capture "eventId" Text :> "ticket-tiers" :> Get '[JSON] [TicketTierDTO]
  :<|> "events" :> Capture "eventId" Text :> "ticket-tiers" :> ReqBody '[JSON] TicketTierDTO :> Post '[JSON] TicketTierDTO
  :<|> "events" :> Capture "eventId" Text :> "ticket-tiers" :> Capture "tierId" Text :> ReqBody '[JSON] TicketTierDTO :> Put '[JSON] TicketTierDTO
  :<|> "events" :> Capture "eventId" Text :> "ticket-orders"
         :> QueryParam "buyerPartyId" Text
         :> QueryParam "status" Text
         :> Get '[JSON] [TicketOrderDTO]
  :<|> "events" :> Capture "eventId" Text :> "ticket-orders" :> ReqBody '[JSON] TicketPurchaseRequestDTO :> Post '[JSON] TicketOrderDTO
  :<|> "events" :> Capture "eventId" Text :> "ticket-orders" :> Capture "orderId" Text :> "status" :> ReqBody '[JSON] TicketOrderStatusUpdateDTO :> Put '[JSON] TicketOrderDTO
  :<|> "events" :> Capture "eventId" Text :> "tickets"
         :> QueryParam "orderId" Text
         :> QueryParam "status" Text
         :> Get '[JSON] [TicketDTO]
  :<|> "events" :> Capture "eventId" Text :> "tickets" :> "check-in" :> ReqBody '[JSON] TicketCheckInRequestDTO :> Post '[JSON] TicketDTO

type BudgetRoutes =
       "events" :> Capture "eventId" Text :> "budget-lines" :> Get '[JSON] [EventBudgetLineDTO]
  :<|> "events" :> Capture "eventId" Text :> "budget-lines" :> ReqBody '[JSON] EventBudgetLineDTO :> Post '[JSON] EventBudgetLineDTO
  :<|> "events" :> Capture "eventId" Text :> "budget-lines" :> Capture "lineId" Text :> ReqBody '[JSON] EventBudgetLineDTO :> Put '[JSON] EventBudgetLineDTO

type FinanceRoutes =
       "events" :> Capture "eventId" Text :> "finance-entries"
         :> QueryParam "direction" Text
         :> QueryParam "source" Text
         :> QueryParam "status" Text
         :> Get '[JSON] [EventFinanceEntryDTO]
  :<|> "events" :> Capture "eventId" Text :> "finance-entries" :> ReqBody '[JSON] EventFinanceEntryDTO :> Post '[JSON] EventFinanceEntryDTO
  :<|> "events" :> Capture "eventId" Text :> "finance-entries" :> Capture "entryId" Text :> ReqBody '[JSON] EventFinanceEntryDTO :> Put '[JSON] EventFinanceEntryDTO
  :<|> "events" :> Capture "eventId" Text :> "finance-summary" :> Get '[JSON] EventFinanceSummaryDTO

type SocialEventsAPI = EventsRoutes
               :<|> VenuesRoutes
               :<|> ArtistsRoutes
               :<|> RsvpRoutes
               :<|> InvitationsRoutes
               :<|> TicketsRoutes
               :<|> BudgetRoutes
               :<|> FinanceRoutes
