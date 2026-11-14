{-# LANGUAGE DeriveGeneric #-}

-- | Wire type for an uploaded ticket payment receipt. It lives with the
-- transfer types so the route contract does not import a handler module.
module TDF.DTO.TicketPaymentReceipt
  ( TicketPaymentReceipt(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON, defaultOptions, genericParseJSON, rejectUnknownFields)
import Data.Text (Text)
import GHC.Generics (Generic)

-- Returned only by the authorized event handler, never by public assets.
data TicketPaymentReceipt = TicketPaymentReceipt
  { contentType :: Text
  , base64 :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON TicketPaymentReceipt
instance FromJSON TicketPaymentReceipt where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }
