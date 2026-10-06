{-# LANGUAGE OverloadedStrings #-}

-- | Staff visibility of SRI invoices for an event's ticket orders. Only a
-- document the provider refused before creating it ('failed') may be resent;
-- an 'uncertain' submission must be reconciled in the provider first.
module TDF.Server.TicketTaxDocuments
  ( listTicketTaxDocuments
  , retryFailedTicketTaxDocument
  ) where

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.Persist.Sql
  ( PersistValue(..), Single(..), SqlPersistT, rawSql, toPersistValue )

import           TDF.DTO.SocialEventsDTO (TicketTaxDocumentDTO(..))
import qualified TDF.Models.SocialEventsModels as SM

listTicketTaxDocuments :: SM.SocialEventId -> SqlPersistT IO [TicketTaxDocumentDTO]
listTicketTaxDocuments eventKey = do
  rows <- rawSql
    "SELECT document.id::text, document.kind, document.domain_order_id, document.establishment,\
    \ document.emission_point, document.sequential, document.status, document.amount_minor,\
    \ document.access_key, document.authorization_number, document.authorized_at,\
    \ document.last_error, document.environment, document.created_at\
    \ FROM commerce_tax_document document\
    \ JOIN event_ticket_checkout_runtime runtime ON runtime.checkout_id = document.checkout_id\
    \ WHERE runtime.event_id = ? AND document.domain_type = 'event_ticket_order'\
    \ ORDER BY document.created_at, document.kind"
    [toPersistValue eventKey]
    :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Text, Single Text, Single Int64, Single Text
       , Single Int64, Single (Maybe Text), Single (Maybe Text), Single (Maybe UTCTime)
       , Single (Maybe Text), Single Text, Single UTCTime
       )]
  pure
    [ TicketTaxDocumentDTO
        { ttdId = documentId
        , ttdKind = kind
        , ttdOrderId = orderId
        , ttdNumber = establishment <> "-" <> emissionPoint <> "-" <> padded sequential
        , ttdStatus = status
        , ttdAmountMinor = fromIntegral amountMinor
        , ttdAccessKey = accessKey
        , ttdAuthorizationNumber = authorization
        , ttdAuthorizedAt = authorizedAt
        , ttdLastError = lastError
        , ttdEnvironment = environment
        , ttdCreatedAt = createdAt
        }
    | ( Single documentId, Single kind, Single orderId, Single establishment, Single emissionPoint
      , Single sequential, Single status, Single amountMinor, Single accessKey
      , Single authorization, Single authorizedAt, Single lastError, Single environment
      , Single createdAt
      ) <- rows
    ]
  where
    padded number = T.justifyRight 9 '0' (T.pack (show number))

retryFailedTicketTaxDocument :: SM.SocialEventId -> Text -> SqlPersistT IO Bool
retryFailedTicketTaxDocument eventKey documentId = do
  rows <- rawSql
    "UPDATE commerce_tax_document document\
    \ SET status = 'pending', attempts = 0, next_attempt_at = NOW(), last_error = NULL, submitted_at = NULL\
    \ FROM event_ticket_checkout_runtime runtime\
    \ WHERE document.id = ?::uuid AND document.status = 'failed'\
    \ AND runtime.checkout_id = document.checkout_id AND runtime.event_id = ?\
    \ RETURNING document.id::text"
    [PersistText documentId, toPersistValue eventKey]
    :: SqlPersistT IO [Single Text]
  pure (not (null rows))
