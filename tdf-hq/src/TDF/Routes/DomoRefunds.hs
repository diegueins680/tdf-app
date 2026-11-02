{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Routes.DomoRefunds (DomoRefundAdminAPI, ManualRefundEvidence(..), RefundProviderReference(..)) where

import Data.Aeson (FromJSON(..), Value, defaultOptions, genericParseJSON, rejectUnknownFields)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant

data ManualRefundEvidence = ManualRefundEvidence
  { externalReference :: Text
  , evidenceReference :: Text
  , amountMinor :: Int64
  , currency :: Text
  , occurredAt :: UTCTime
  } deriving (Eq, Show, Generic)
instance FromJSON ManualRefundEvidence where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields=True }

newtype RefundProviderReference = RefundProviderReference { providerRefundId :: Text }
  deriving (Eq, Show, Generic)
instance FromJSON RefundProviderReference where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields=True }

type DomoRefundAdminAPI = "domo" :> "quotes" :> Capture "quoteId" Text :> "refund" :>
  ( Get '[JSON] Value
    :<|> "approve" :> Post '[JSON] Value
    :<|> "execute" :> Post '[JSON] Value
    :<|> "reconcile" :> Post '[JSON] Value
    :<|> "manual-evidence" :> ReqBody '[JSON] ManualRefundEvidence :> Post '[JSON] Value
    :<|> "confirm-manual" :> Post '[JSON] Value
    :<|> "provider-reference" :> ReqBody '[JSON] RefundProviderReference :> Post '[JSON] Value
  )
