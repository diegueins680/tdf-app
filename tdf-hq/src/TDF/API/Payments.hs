{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TDF.API.Payments where

import           Data.Char
  ( GeneralCategory (Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isAlpha
  , isAlphaNum
  , isAscii
  , isControl
  , isDigit
  , isSpace
  )
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, defaultTimeLocale, parseTimeM)
import           GHC.Generics (Generic)
import           Servant
import           Data.Aeson (FromJSON (parseJSON), ToJSON, Value (Null), defaultOptions, eitherDecode, genericParseJSON, rejectUnknownFields, withObject)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import           Data.Aeson.Types (Object, Parser)
import           Text.Read (readMaybe)

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
    currencyValue <- validateRequiredPaymentCurrencyField "pcCurrency" (pcCurrency payload)
    methodValue <- validateRequiredPaymentTextField "pcMethod" 64 (pcMethod payload)
    paidAtValue <- validateRequiredPaymentDateField "pcPaidAt" (pcPaidAt payload)
    conceptValue <- validateRequiredPaymentTextField "pcConcept" 240 (pcConcept payload)
    referenceValue <- validateOptionalPaymentTextField "pcReference" 160 (pcReference payload)
    periodValue <- validateOptionalPaymentPeriodField "pcPeriod" (pcPeriod payload)
    attachmentUrlValue <-
      validateOptionalPaymentAttachmentUrlField "pcAttachmentUrl" (pcAttachmentUrl payload)
    pure payload
      { pcCurrency = currencyValue
      , pcMethod = methodValue
      , pcPaidAt = paidAtValue
      , pcConcept = conceptValue
      , pcReference = referenceValue
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

validateRequiredPaymentCurrencyField :: String -> Text -> Parser Text
validateRequiredPaymentCurrencyField fieldName rawValue = do
  value <- validateRequiredPaymentTextField fieldName 3 rawValue
  let normalized = T.toUpper value
  if normalized == "USD"
    then pure normalized
    else fail (fieldName <> " must be USD because manual payments are currently USD-only")

validateRequiredPaymentTextField :: String -> Int -> Text -> Parser Text
validateRequiredPaymentTextField fieldName maxLength rawValue
  | T.null value =
      fail (fieldName <> " is required")
  | T.any isUnsafePaymentCreateTextChar rawValue =
      fail
        ( fieldName
            <> " must not contain control characters or hidden formatting characters"
        )
  | T.length value > maxLength =
      fail (fieldName <> " must be " <> show maxLength <> " characters or fewer")
  | otherwise =
      pure value
  where
    value = T.strip rawValue

validateOptionalPaymentTextField :: String -> Int -> Maybe Text -> Parser (Maybe Text)
validateOptionalPaymentTextField _ _ Nothing =
  pure Nothing
validateOptionalPaymentTextField fieldName maxLength (Just rawValue)
  | T.null value =
      fail (fieldName <> " must be omitted or a non-empty string")
  | T.any isUnsafePaymentCreateTextChar rawValue =
      fail
        ( fieldName
            <> " must not contain control characters or hidden formatting characters"
        )
  | T.length value > maxLength =
      fail (fieldName <> " must be " <> show maxLength <> " characters or fewer")
  | otherwise =
      pure (Just value)
  where
    value = T.strip rawValue

validateOptionalPaymentAttachmentUrlField :: String -> Maybe Text -> Parser (Maybe Text)
validateOptionalPaymentAttachmentUrlField fieldName rawValue = do
  mValue <- validateOptionalPaymentTextField fieldName maxPaymentAttachmentUrlChars rawValue
  case mValue of
    Nothing ->
      pure Nothing
    Just value
      | not ("https://" `T.isPrefixOf` T.toLower value) ->
          fail (paymentAttachmentUrlShapeMessage fieldName)
      | "#" `T.isInfixOf` value ->
          fail (paymentAttachmentUrlShapeMessage fieldName)
      | T.any isSpace value || T.any (== '\\') value ->
          fail (paymentAttachmentUrlShapeMessage fieldName)
      | not (hasValidPaymentAttachmentUrlAuthority value) ->
          fail (paymentAttachmentUrlShapeMessage fieldName)
      | hasAmbiguousPaymentAttachmentUrlPath value ->
          fail (fieldName <> " path must not contain empty, dot, or dot-dot segments")
      | otherwise ->
          pure (Just value)

maxPaymentAttachmentUrlChars :: Int
maxPaymentAttachmentUrlChars = 2048

paymentAttachmentUrlShapeMessage :: String -> String
paymentAttachmentUrlShapeMessage fieldName =
  fieldName <> " must be an absolute https URL without a fragment"

hasValidPaymentAttachmentUrlAuthority :: Text -> Bool
hasValidPaymentAttachmentUrlAuthority url =
  validateAuthority authority
  where
    authority =
      T.takeWhile (\ch -> ch /= '/' && ch /= '?' && ch /= '#') (T.drop 8 url)

    validateAuthority rawAuthority
      | T.null rawAuthority = False
      | T.any (== '@') rawAuthority = False
      | T.count ":" rawAuthority > 1 = False
      | otherwise =
          let (host, portSuffix) = T.breakOn ":" rawAuthority
          in validateHost host && validatePortSuffix portSuffix

    validateHost host =
      let normalizedHost = T.toLower host
          labels = T.splitOn "." normalizedHost
      in case reverse labels of
           [] ->
             False
           finalLabel : _ ->
             not (T.null normalizedHost)
               && T.any (== '.') normalizedHost
               && all isValidPaymentAttachmentHostLabel labels
               && T.length finalLabel >= 2
               && T.any isAlpha finalLabel

    validatePortSuffix suffix
      | T.null suffix = True
      | ":" `T.isPrefixOf` suffix =
          let port = T.drop 1 suffix
          in not (T.null port)
               && T.all isDigit port
               && not (T.length port > 1 && T.head port == '0')
               && maybe False (\portNumber -> portNumber >= (1 :: Int) && portNumber <= 65535)
                    (readMaybe (T.unpack port))
      | otherwise = False

isValidPaymentAttachmentHostLabel :: Text -> Bool
isValidPaymentAttachmentHostLabel label =
  not (T.null label)
    && T.all isValidPaymentAttachmentHostChar label
    && T.head label /= '-'
    && T.last label /= '-'

isValidPaymentAttachmentHostChar :: Char -> Bool
isValidPaymentAttachmentHostChar ch =
  isAscii ch && (isAlphaNum ch || ch == '-')

hasAmbiguousPaymentAttachmentUrlPath :: Text -> Bool
hasAmbiguousPaymentAttachmentUrlPath rawUrl =
  any isAmbiguousPathSegment pathSegments
  where
    noScheme = T.drop 8 rawUrl
    pathWithQueryOrFragment =
      T.dropWhile (\ch -> ch /= '/' && ch /= '?' && ch /= '#') noScheme
    path =
      case T.uncons pathWithQueryOrFragment of
        Just ('/', _) ->
          T.takeWhile (\ch -> ch /= '?' && ch /= '#') pathWithQueryOrFragment
        _ ->
          ""
    trimmedPath = T.dropWhileEnd (== '/') path
    pathSegments =
      if T.null trimmedPath
        then []
        else T.splitOn "/" (T.drop 1 trimmedPath)
    isAmbiguousPathSegment segment =
      segment == "" || decodedSegment == "." || decodedSegment == ".."
      where
        decodedSegment = decodeUrlEncodedDots (T.toLower segment)

decodeUrlEncodedDots :: Text -> Text
decodeUrlEncodedDots txt =
  case T.uncons txt of
    Nothing ->
      ""
    Just ('%', rest)
      | "2e" `T.isPrefixOf` rest ->
          "." <> decodeUrlEncodedDots (T.drop 2 rest)
      | otherwise ->
          "%" <> decodeUrlEncodedDots rest
    Just (ch, rest) ->
      T.singleton ch <> decodeUrlEncodedDots rest

validateRequiredPaymentDateField :: String -> Text -> Parser Text
validateRequiredPaymentDateField fieldName rawValue = do
  value <- validateRequiredPaymentTextField fieldName 10 rawValue
  case parsePaymentDate value of
    Just _ -> pure value
    Nothing -> fail (fieldName <> " must be a valid date in YYYY-MM-DD format")

parsePaymentDate :: Text -> Maybe Day
parsePaymentDate value
  | hasPaymentDateShape value =
      parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack value)
  | otherwise =
      Nothing

hasPaymentDateShape :: Text -> Bool
hasPaymentDateShape value =
  case T.splitOn "-" value of
    [yearPart, monthPart, dayPart] ->
      T.length yearPart == 4
        && T.length monthPart == 2
        && T.length dayPart == 2
        && T.all isDigit (yearPart <> monthPart <> dayPart)
    _ ->
      False

validateOptionalPaymentPeriodField :: String -> Maybe Text -> Parser (Maybe Text)
validateOptionalPaymentPeriodField fieldName rawValue = do
  mValue <- validateOptionalPaymentTextField fieldName 7 rawValue
  case mValue of
    Nothing -> pure Nothing
    Just value
      | hasPaymentPeriodShape value -> pure (Just value)
      | otherwise -> fail (fieldName <> " must be in YYYY-MM format")

hasPaymentPeriodShape :: Text -> Bool
hasPaymentPeriodShape value =
  case T.splitOn "-" value of
    [yearPart, monthPart]
      | T.length yearPart == 4
          && T.length monthPart == 2
          && T.all isDigit (yearPart <> monthPart) ->
              case readMaybe (T.unpack monthPart) :: Maybe Int of
                Just monthNumber -> monthNumber >= 1 && monthNumber <= 12
                Nothing -> False
    _ ->
      False

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
