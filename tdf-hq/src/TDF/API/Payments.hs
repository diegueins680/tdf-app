{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.Payments where

import           Data.Int (Int64)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant

import           TDF.API.Types (LooseJSON)

data PaymentCreate = PaymentCreate
  { pcPartyId      :: Int64
  , pcOrderId      :: Maybe Int64
  , pcInvoiceId    :: Maybe Int64
  , pcAmountCents  :: Int
  , pcCurrency     :: Text
  , pcMethod       :: Text
  , pcReference    :: Maybe Text
  , pcPaidAt       :: Text
  , pcConcept      :: Text
  , pcPeriod       :: Maybe Text
  , pcAttachmentUrl:: Maybe Text
  } deriving (Show, Generic)

instance FromJSON PaymentCreate

data PaymentDTO = PaymentDTO
  { payId         :: Int64
  , payPartyId    :: Int64
  , payOrderId    :: Maybe Int64
  , payInvoiceId  :: Maybe Int64
  , payAmountCents:: Int
  , payCurrency   :: Text
  , payMethod     :: Text
  , payReference  :: Maybe Text
  , payPaidAt     :: Text
  , payConcept    :: Text
  , payPeriod     :: Maybe Text
  , payAttachment :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON PaymentDTO

type PaymentsAPI =
       QueryParam "partyId" Int64 :> Get '[JSON] [PaymentDTO]
  :<|> ReqBody '[JSON, LooseJSON] PaymentCreate :> Post '[JSON] PaymentDTO
  :<|> Capture "paymentId" Int64 :> Get '[JSON] PaymentDTO

