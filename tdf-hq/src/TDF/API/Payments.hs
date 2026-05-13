{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TDF.API.Payments where

import           Data.Int (Int64)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant
import           Data.Aeson (FromJSON (parseJSON), ToJSON, defaultOptions, eitherDecode, genericParseJSON, rejectUnknownFields)
import           Data.Aeson.Types (Parser)

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

instance FromJSON PaymentCreate where
  parseJSON value = do
    payload <- genericParseJSON defaultOptions { rejectUnknownFields = True } value
    validatePositiveInt64Field "pcPartyId" (pcPartyId payload)
    maybe (pure ()) (validatePositiveInt64Field "pcOrderId") (pcOrderId payload)
    maybe (pure ()) (validatePositiveInt64Field "pcInvoiceId") (pcInvoiceId payload)
    validatePositiveIntField "pcAmountCents" (pcAmountCents payload)
    pure payload

validatePositiveInt64Field :: String -> Int64 -> Parser ()
validatePositiveInt64Field fieldName rawValue =
  if rawValue > 0
    then pure ()
    else fail (fieldName <> " must be a positive integer")

validatePositiveIntField :: String -> Int -> Parser ()
validatePositiveIntField fieldName rawValue =
  if rawValue > 0
    then pure ()
    else fail (fieldName <> " must be a positive integer")

instance MimeUnrender LooseJSON PaymentCreate where
  mimeUnrender _ = eitherDecode

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
