{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TDF.API.Payments where

import           Data.Char
  ( GeneralCategory (Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isControl
  )
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Servant
import           Data.Aeson (FromJSON (parseJSON), ToJSON, Value (Null), defaultOptions, eitherDecode, genericParseJSON, rejectUnknownFields, withObject)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import           Data.Aeson.Types (Object, Parser)

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
    rejectNullOptionalPaymentFields value
    payload <- genericParseJSON defaultOptions { rejectUnknownFields = True } value
    validatePositiveInt64Field "pcPartyId" (pcPartyId payload)
    maybe (pure ()) (validatePositiveInt64Field "pcOrderId") (pcOrderId payload)
    maybe (pure ()) (validatePositiveInt64Field "pcInvoiceId") (pcInvoiceId payload)
    validatePositiveIntField "pcAmountCents" (pcAmountCents payload)
    validateRequiredPaymentTextField "pcCurrency" (pcCurrency payload)
    validateRequiredPaymentTextField "pcMethod" (pcMethod payload)
    validateRequiredPaymentTextField "pcPaidAt" (pcPaidAt payload)
    validateRequiredPaymentTextField "pcConcept" (pcConcept payload)
    referenceValue <- validateOptionalPaymentTextField "pcReference" (pcReference payload)
    periodValue <- validateOptionalPaymentTextField "pcPeriod" (pcPeriod payload)
    attachmentUrlValue <-
      validateOptionalPaymentTextField "pcAttachmentUrl" (pcAttachmentUrl payload)
    pure payload
      { pcReference = referenceValue
      , pcPeriod = periodValue
      , pcAttachmentUrl = attachmentUrlValue
      }

rejectNullOptionalPaymentFields :: Value -> Parser ()
rejectNullOptionalPaymentFields =
  withObject "PaymentCreate" $ \obj ->
    mapM_
      (rejectNullOptionalPaymentField obj)
      [ "pcOrderId"
      , "pcInvoiceId"
      , "pcReference"
      , "pcPeriod"
      , "pcAttachmentUrl"
      ]

rejectNullOptionalPaymentField :: Object -> Text -> Parser ()
rejectNullOptionalPaymentField obj fieldName =
  case AesonKeyMap.lookup (AesonKey.fromText fieldName) obj of
    Just Null ->
      fail (T.unpack fieldName <> " must be omitted instead of null")
    _ ->
      pure ()

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

validateRequiredPaymentTextField :: String -> Text -> Parser ()
validateRequiredPaymentTextField fieldName rawValue
  | T.null (T.strip rawValue) =
      fail (fieldName <> " is required")
  | T.any isUnsafePaymentCreateTextChar rawValue =
      fail
        ( fieldName
            <> " must not contain control characters or hidden formatting characters"
        )
  | otherwise =
      pure ()

validateOptionalPaymentTextField :: String -> Maybe Text -> Parser (Maybe Text)
validateOptionalPaymentTextField _ Nothing =
  pure Nothing
validateOptionalPaymentTextField fieldName (Just rawValue)
  | T.null value =
      fail (fieldName <> " must be omitted or a non-empty string")
  | T.any isUnsafePaymentCreateTextChar rawValue =
      fail
        ( fieldName
            <> " must not contain control characters or hidden formatting characters"
        )
  | otherwise =
      pure (Just value)
  where
    value = T.strip rawValue

isUnsafePaymentCreateTextChar :: Char -> Bool
isUnsafePaymentCreateTextChar ch =
  isControl ch
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

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
