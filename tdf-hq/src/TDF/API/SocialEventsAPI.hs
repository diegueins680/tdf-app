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
  , MomentsRoutes
  , TicketsRoutes
  , BudgetRoutes
  , FinanceRoutes
  , IdParam
  , EventImageUploadForm(..)
  , EventImageUploadDTO(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isControl, isSpace)
import Servant
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant.Multipart
  ( FileData
  , FromMultipart(..)
  , Input(..)
  , MultipartData(..)
  , MultipartForm
  , Tmp
  , fdInputName
  , fdFileName
  )
import System.FilePath (takeExtension)

import TDF.DTO.SocialEventsDTO
  ( EventDTO
  , EventUpdateDTO
  , VenueDTO
  , VenueUpdateDTO
  , ArtistDTO
  , ArtistFollowerDTO
  , ArtistFollowRequest
  , RsvpCreateDTO
  , RsvpDTO
  , InvitationDTO
  , InvitationUpdateDTO
  , EventMomentDTO
  , EventMomentCreateDTO
  , EventMomentReactionRequestDTO
  , EventMomentCommentDTO
  , EventMomentCommentCreateDTO
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
    rejectUnexpectedParts multipart
    file <- lookupSingleFile "file" multipart
    mName <- lookupOptionalInput "name" multipart
    mapM_ (validateUploadName "Uploaded image name") mName
    validateUploadName "Uploaded browser file name" (fdFileName file)
    pure EventImageUploadForm
      { eiuFile = file
      , eiuName = mName
      }
    where
      rejectUnexpectedParts mp =
        case (unexpectedInputs, unexpectedFiles) of
          (fieldName : _, _) -> Left ("Unexpected field: " <> T.unpack fieldName)
          (_, fileField : _) -> Left ("Unexpected file field: " <> T.unpack fileField)
          _ -> Right ()
        where
          unexpectedInputs =
            [ name
            | Input name _ <- inputs mp
            , name /= "name"
            ]
          unexpectedFiles =
            [ fdInputName file
            | file <- files mp
            , fdInputName file /= "file"
            ]

      lookupOptionalInput name mp =
        case filter (\(Input inputName _) -> inputName == name) (inputs mp) of
          [] -> Right Nothing
          [Input _ raw] ->
            let trimmed = T.strip raw
            in Right (if T.null trimmed then Nothing else Just trimmed)
          _ -> Left ("Duplicate field: " <> T.unpack name)

      lookupSingleFile name mp =
        case [file | file <- files mp, fdInputName file == name] of
          [] -> Left ("Missing file field: " <> T.unpack name)
          [file] -> Right file
          _ -> Left ("Duplicate file field: " <> T.unpack name)

      validateUploadName label rawName
        | T.null (T.strip rawName) = Right ()
        | T.any isControl rawName =
            Left (label <> " must not contain control characters")
        | T.any isPathSeparator rawName =
            Left (label <> " must not contain path separators")
        | not (hasNonEmptyUploadBaseName rawName) =
            Left (label <> " must include a non-empty base name")
        | T.length (T.strip rawName) > maxUploadNameLength =
            Left (label <> " must be 180 characters or fewer")
        | otherwise = Right ()

      maxUploadNameLength = 180 :: Int

      isPathSeparator ch = ch == '/' || ch == '\\'

      hasNonEmptyUploadBaseName rawName =
        let trimmed = T.strip rawName
            ext = T.pack (takeExtension (T.unpack trimmed))
            baseName =
              if T.null ext
                then trimmed
                else T.dropEnd (T.length ext) trimmed
        in T.any (\ch -> ch /= '.' && not (isSpace ch)) baseName

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
  :<|> "events" :> IdParam :> ReqBody '[JSON] EventUpdateDTO :> Put '[JSON] EventDTO
  :<|> "events" :> IdParam :> "image" :> MultipartForm Tmp EventImageUploadForm :> Post '[JSON] EventImageUploadDTO
  :<|> "events" :> IdParam :> DeleteNoContent

type VenuesRoutes =
       "venues" :> QueryParam "city" Text :> QueryParam "near" Text :> QueryParam "q" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [VenueDTO]
  :<|> "venues" :> ReqBody '[JSON] VenueDTO :> Post '[JSON] VenueDTO
  :<|> "venues" :> IdParam :> Get '[JSON] VenueDTO
  :<|> "venues" :> IdParam :> ReqBody '[JSON] VenueUpdateDTO :> Put '[JSON] VenueDTO

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
  :<|> "events" :> Capture "eventId" Text :> "rsvps" :> ReqBody '[JSON] RsvpCreateDTO :> Post '[JSON] RsvpDTO

type InvitationsRoutes =
       "events" :> Capture "eventId" Text :> "invitations" :>
         ( Get '[JSON] [InvitationDTO]
      :<|> ReqBody '[JSON] InvitationDTO :> Post '[JSON] InvitationDTO
      :<|> Capture "invitationId" Text :> ReqBody '[JSON] InvitationUpdateDTO :> Put '[JSON] InvitationDTO
         )

type MomentsRoutes =
       "events" :> Capture "eventId" Text :> "moments" :> Get '[JSON] [EventMomentDTO]
  :<|> "events" :> Capture "eventId" Text :> "moments" :> ReqBody '[JSON] EventMomentCreateDTO :> Post '[JSON] EventMomentDTO
  :<|> "events" :> Capture "eventId" Text :> "moments" :> Capture "momentId" Text :> "reactions" :> ReqBody '[JSON] EventMomentReactionRequestDTO :> Post '[JSON] EventMomentDTO
  :<|> "events" :> Capture "eventId" Text :> "moments" :> Capture "momentId" Text :> "comments" :> ReqBody '[JSON] EventMomentCommentCreateDTO :> Post '[JSON] EventMomentCommentDTO

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
               :<|> MomentsRoutes
               :<|> TicketsRoutes
               :<|> BudgetRoutes
               :<|> FinanceRoutes
