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
  , LiveBroadcastsRoutes
  , TicketsRoutes
  , BudgetRoutes
  , FinanceRoutes
  , IdParam
  , EventImageUploadForm(..)
  , EventImageUploadDTO(..)
  , validateEventImageUploadForm
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator, Space)
  , generalCategory
  , isAlphaNum
  , isAscii
  , isControl
  )
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
  , fdFileCType
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
  , EventLiveBroadcastDTO
  , EventLiveBroadcastCreateDTO
  , EventLiveBroadcastEndDTO
  , EventLiveBroadcastHeartbeatDTO
  , TicketTierDTO
  , TicketPurchaseRequestDTO
  , TicketOrderStatusUpdateDTO
  , TicketCheckInRequestDTO
  , TicketDTO
  , TicketOrderDTO
  , PromoCodeDTO
  , TicketPurchaseWithPromoDTO
  , RefundRequestDTO
  , RefundDTO
  , RejectionReasonDTO
  , TicketTransferCreateDTO
  , TicketTransferDTO
  , WaitlistJoinDTO
  , WaitlistEntryDTO
  , StripePaymentIntentDTO
  , TicketWithQRDTO
  , EventBudgetLineDTO
  , EventFinanceEntryDTO
  , EventFinanceSummaryDTO
  )

type IdParam = Capture "id" Text

data EventImageUploadForm = EventImageUploadForm
  { eiuFile :: FileData Tmp
  , eiuName :: Maybe Text
  }

validateEventImageUploadForm :: EventImageUploadForm -> Either Text EventImageUploadForm
validateEventImageUploadForm (EventImageUploadForm file rawName) = do
  mName <- traverse normalizeEventImageUploadName rawName
  rejectUnnamedUpload mName file
  validateUploadName "Uploaded browser file name" (fdFileName file)
  validateImageUploadMetadata mName file
  pure EventImageUploadForm
    { eiuFile = file
    , eiuName = mName
    }

instance FromMultipart Tmp EventImageUploadForm where
  fromMultipart multipart = do
    rejectUnexpectedParts multipart
    file <- lookupSingleFile "file" multipart
    mName <- lookupOptionalInput "name" multipart
    let uploadForm = EventImageUploadForm { eiuFile = file, eiuName = mName }
    case validateEventImageUploadForm uploadForm of
      Left err -> Left (T.unpack err)
      Right validatedForm -> Right validatedForm
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
          [Input _ raw] -> Right (Just raw)
          _ -> Left ("Duplicate field: " <> T.unpack name)

      lookupSingleFile name mp =
        case [file | file <- files mp, fdInputName file == name] of
          [] -> Left ("Missing file field: " <> T.unpack name)
          [file] -> Right file
          _ -> Left ("Duplicate file field: " <> T.unpack name)

normalizeEventImageUploadName :: Text -> Either Text Text
normalizeEventImageUploadName rawName =
  let trimmed = T.strip rawName
  in if T.null trimmed
      then Left "name must not be blank; omit it to use the browser file name"
      else validateUploadName "Uploaded image name" trimmed *> Right trimmed

rejectUnnamedUpload :: Maybe Text -> FileData Tmp -> Either Text ()
rejectUnnamedUpload mName file =
  case (mName, T.strip (fdFileName file)) of
    (Nothing, fileName) | T.null fileName ->
      Left "Either image name or uploaded browser file name must be provided"
    _ -> Right ()

validateUploadName :: Text -> Text -> Either Text ()
validateUploadName label rawName
  | T.null (T.strip rawName) = Right ()
  | T.any isUnsafeUploadNameChar rawName =
      Left
        ( label
            <> " must not contain control characters or Unicode formatting marks"
            <> ", or non-ASCII spaces"
        )
  | T.any isPathSeparator rawName =
      Left (label <> " must not contain path separators")
  | T.any isUrlDelimiterUploadNameChar rawName =
      Left
        ( label
            <> " must not contain URL delimiters or percent-encoded path markers"
        )
  | hasDangerousInnerUploadExtension rawName =
      Left (label <> " must not hide executable or document extensions")
  | not (hasNonEmptyUploadBaseName rawName) =
      Left (label <> " must include a non-empty base name")
  | hasEmptyUploadNameSegment rawName =
      Left (label <> " must not contain leading, trailing, or repeated dots")
  | T.length (T.strip rawName) > maxUploadNameLength =
      Left (label <> " must be 180 characters or fewer")
  | otherwise = Right ()

maxUploadNameLength :: Int
maxUploadNameLength = 180

isPathSeparator :: Char -> Bool
isPathSeparator ch = ch == '/' || ch == '\\'

isUrlDelimiterUploadNameChar :: Char -> Bool
isUrlDelimiterUploadNameChar ch =
  ch == '?' || ch == '#' || ch == '%'

hasEmptyUploadNameSegment :: Text -> Bool
hasEmptyUploadNameSegment rawName =
  let name = T.strip rawName
      segments = T.splitOn "." name
  in not (T.null name) && any T.null segments

hasDangerousInnerUploadExtension :: Text -> Bool
hasDangerousInnerUploadExtension rawName =
  any (`elem` dangerousInnerUploadExtensionSegments) innerExtensions
  where
    parts = T.splitOn "." (T.toLower (T.strip rawName))
    extensions = drop 1 parts
    innerExtensions =
      case reverse extensions of
        [] -> []
        (_finalExtension:rest) -> reverse rest

dangerousInnerUploadExtensionSegments :: [Text]
dangerousInnerUploadExtensionSegments =
  [ "bat"
  , "cmd"
  , "com"
  , "doc"
  , "docx"
  , "exe"
  , "htm"
  , "html"
  , "jar"
  , "js"
  , "mjs"
  , "pdf"
  , "php"
  , "ppt"
  , "pptx"
  , "ps1"
  , "rtf"
  , "scr"
  , "sh"
  , "svg"
  , "svgz"
  , "xls"
  , "xlsx"
  , "xhtml"
  ]

isUnsafeUploadNameChar :: Char -> Bool
isUnsafeUploadNameChar ch =
  isControl ch
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]
    || (generalCategory ch == Space && ch /= ' ')

hasNonEmptyUploadBaseName :: Text -> Bool
hasNonEmptyUploadBaseName rawName =
  let trimmed = T.strip rawName
      ext = T.pack (takeExtension (T.unpack trimmed))
      baseName =
        if T.null ext
          then trimmed
          else T.dropEnd (T.length ext) trimmed
  in T.any isStableUploadBaseNameAtom baseName

isStableUploadBaseNameAtom :: Char -> Bool
isStableUploadBaseNameAtom ch =
  isAscii ch && isAlphaNum ch

validateImageUploadMetadata :: Maybe Text -> FileData Tmp -> Either Text ()
validateImageUploadMetadata mName file
  | T.any isUnsafeUploadNameChar (fdFileCType file) =
      Left
        ( "Uploaded image MIME type must not contain control characters "
            <> "or Unicode formatting marks, or non-ASCII spaces"
        )
  | hasUploadContentTypeNameParameter (fdFileCType file) =
      Left "Uploaded image MIME type must not include filename parameters"
  | otherwise =
      let uploadMimeType = normalizeUploadMimeType (fdFileCType file)
          requestedExt = maybe "" imageExtension mName
          browserExt = imageExtension (fdFileName file)
          resolvedExt =
            if T.null requestedExt
              then browserExt
              else requestedExt
          providedExts = filter (not . T.null) [requestedExt, browserExt]
      in case allowedImageExtensions uploadMimeType of
        Nothing ->
          Left
            ( "Uploaded image must be a raster image "
                <> "(jpg, jpeg, png, webp, gif, or bmp)"
            )
        Just allowedExts
          | T.null resolvedExt ->
              Left "Uploaded image file name must include a supported image extension"
          | any (`notElem` allImageExtensions) (resolvedExt : providedExts) ->
              Left
                ( "Uploaded image file name must end with "
                    <> ".jpg, .jpeg, .png, .webp, .gif, or .bmp"
                )
          | any (`notElem` allowedExts) (resolvedExt : providedExts) ->
              Left "Uploaded image extension must match its MIME type"
          | otherwise ->
              Right ()

normalizeUploadMimeType :: Text -> Text
normalizeUploadMimeType rawContentType =
  T.toLower (T.strip (fst (T.breakOn ";" rawContentType)))

hasUploadContentTypeNameParameter :: Text -> Bool
hasUploadContentTypeNameParameter rawContentType =
  any isNameParameter (drop 1 (T.splitOn ";" rawContentType))
  where
    isNameParameter rawParameter =
      let key = T.toLower (T.strip (fst (T.breakOn "=" rawParameter)))
      in key `elem` ["name", "name*", "filename", "filename*"]
           || "name*" `T.isPrefixOf` key
           || "filename*" `T.isPrefixOf` key

imageExtension :: Text -> Text
imageExtension rawName =
  T.toLower (T.pack (takeExtension (T.unpack (T.strip rawName))))

allowedImageExtensions :: Text -> Maybe [Text]
allowedImageExtensions mimeType =
  case mimeType of
    "image/jpeg" -> Just [".jpg", ".jpeg"]
    "image/png"  -> Just [".png"]
    "image/webp" -> Just [".webp"]
    "image/gif"  -> Just [".gif"]
    "image/bmp"  -> Just [".bmp"]
    _ -> Nothing

allImageExtensions :: [Text]
allImageExtensions =
  [".jpg", ".jpeg", ".png", ".webp", ".gif", ".bmp"]

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

type LiveBroadcastsRoutes =
       "events" :> Capture "eventId" Text :> "live-broadcasts" :> Get '[JSON] [EventLiveBroadcastDTO]
  :<|> "events" :> Capture "eventId" Text :> "live-broadcasts" :> ReqBody '[JSON] EventLiveBroadcastCreateDTO :> Post '[JSON] EventLiveBroadcastDTO
  :<|> "events" :> Capture "eventId" Text :> "live-broadcasts" :> Capture "broadcastId" Text :> "heartbeat" :> ReqBody '[JSON] EventLiveBroadcastHeartbeatDTO :> Post '[JSON] EventLiveBroadcastDTO
  :<|> "events" :> Capture "eventId" Text :> "live-broadcasts" :> Capture "broadcastId" Text :> "end" :> ReqBody '[JSON] EventLiveBroadcastEndDTO :> Post '[JSON] EventLiveBroadcastDTO

type TicketsRoutes =
       -- Existing routes
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

  -- Promo Codes
  :<|> "events" :> Capture "eventId" Text :> "promo-codes" :> Get '[JSON] [PromoCodeDTO]
  :<|> "events" :> Capture "eventId" Text :> "promo-codes" :> ReqBody '[JSON] PromoCodeDTO :> Post '[JSON] PromoCodeDTO
  :<|> "events" :> Capture "eventId" Text :> "promo-codes" :> Capture "codeId" Text :> ReqBody '[JSON] PromoCodeDTO :> Put '[JSON] PromoCodeDTO
  :<|> "events" :> Capture "eventId" Text :> "promo-codes" :> Capture "codeId" Text :> "validate"
         :> QueryParam "code" Text
         :> QueryParam "tierId" Text
         :> Get '[JSON] PromoCodeDTO

  -- Stripe Payment
  :<|> "stripe" :> "create-payment-intent" :> ReqBody '[JSON] TicketPurchaseWithPromoDTO :> Post '[JSON] StripePaymentIntentDTO
  :<|> "stripe" :> "webhook" :> ReqBody '[JSON] Value :> Header "Stripe-Signature" Text :> Post '[JSON] NoContent

  -- Refunds
  :<|> "events" :> Capture "eventId" Text :> "ticket-orders" :> Capture "orderId" Text :> "refund"
         :> ReqBody '[JSON] RefundRequestDTO :> Post '[JSON] RefundDTO
  :<|> "events" :> Capture "eventId" Text :> "refunds" :> Get '[JSON] [RefundDTO]
  :<|> "events" :> Capture "eventId" Text :> "refunds" :> Capture "refundId" Text :> "approve" :> Post '[JSON] RefundDTO
  :<|> "events" :> Capture "eventId" Text :> "refunds" :> Capture "refundId" Text :> "reject"
         :> ReqBody '[JSON] RejectionReasonDTO :> Post '[JSON] RefundDTO

  -- Transfers
  :<|> "events" :> Capture "eventId" Text :> "tickets" :> Capture "ticketId" Text :> "transfer"
         :> ReqBody '[JSON] TicketTransferCreateDTO :> Post '[JSON] TicketTransferDTO
  :<|> "events" :> Capture "eventId" Text :> "tickets" :> Capture "ticketId" Text :> "transfers" :> Get '[JSON] [TicketTransferDTO]
  :<|> "ticket-transfers" :> Capture "transferCode" Text :> "accept" :> Post '[JSON] TicketDTO
  :<|> "ticket-transfers" :> Capture "transferCode" Text :> "cancel" :> Post '[JSON] TicketTransferDTO

  -- Waitlist
  :<|> "events" :> Capture "eventId" Text :> "waitlist" :> ReqBody '[JSON] WaitlistJoinDTO :> Post '[JSON] WaitlistEntryDTO
  :<|> "events" :> Capture "eventId" Text :> "waitlist"
         :> QueryParam "tierId" Text
         :> Get '[JSON] [WaitlistEntryDTO]
  :<|> "events" :> Capture "eventId" Text :> "waitlist" :> Capture "entryId" Text :> "notify" :> Post '[JSON] WaitlistEntryDTO
  :<|> "events" :> Capture "eventId" Text :> "waitlist" :> Capture "entryId" Text :> DeleteNoContent

  -- QR Codes
  :<|> "events" :> Capture "eventId" Text :> "tickets" :> Capture "ticketId" Text :> "qr" :> Get '[JSON] TicketWithQRDTO

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
               :<|> LiveBroadcastsRoutes
               :<|> TicketsRoutes
               :<|> BudgetRoutes
               :<|> FinanceRoutes
