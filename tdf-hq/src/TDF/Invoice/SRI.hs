{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Invoice.SRI
  ( SriScriptCustomer(..)
  , SriScriptLine(..)
  , SriScriptRequest(..)
  , decodeSriScriptOutput
  , decodeSriScriptOutputForRequest
  , formatSriScriptFailure
  , validateSriScriptRequest
  , discoverDefaultScriptPath
  , runSriInvoiceScript
  ) where

import           Prelude hiding (lines)

import           Control.Monad (when)
import           Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isAlphaNum
  , isSpace
  )
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           GHC.Generics (Generic)
import           Control.Exception (IOException, displayException, try)
import           System.Directory (doesFileExist, makeAbsolute)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.FilePath (isAbsolute, normalise, splitDirectories, takeExtension)
import           System.Process (proc, readCreateProcessWithExitCode)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL

import           TDF.DTO (SriIssueBuyerDTO(..), SriIssueResultDTO(..))

data SriScriptCustomer = SriScriptCustomer
  { ruc       :: Text
  , legalName :: Text
  , email     :: Maybe Text
  , phone     :: Maybe Text
  } deriving (Show, Generic)

instance Aeson.ToJSON SriScriptCustomer

data SriScriptLine = SriScriptLine
  { code              :: Maybe Text
  , auxiliaryCode     :: Maybe Text
  , description       :: Text
  , quantity          :: Int
  , unitCents         :: Int
  , taxBps            :: Maybe Int
  , sriAdditionalInfo :: Maybe Text
  , sriIvaCode        :: Maybe Text
  } deriving (Show, Generic)

instance Aeson.ToJSON SriScriptLine

data SriScriptRequest = SriScriptRequest
  { customer            :: SriScriptCustomer
  , lines               :: [SriScriptLine]
  , establishment       :: Text
  , emissionPoint       :: Text
  , paymentMode         :: Text
  , signAndSend         :: Bool
  , certificatePassword :: Maybe Text
  } deriving (Show, Generic)

instance Aeson.ToJSON SriScriptRequest

runSriInvoiceScript :: SriScriptRequest -> IO (Either Text SriIssueResultDTO)
runSriInvoiceScript payload = do
  case validateSriScriptRequest payload of
    Left err -> pure (Left err)
    Right validatedPayload -> do
      scriptPathResult <- resolveScriptPath
      case scriptPathResult of
        Left err -> pure (Left err)
        Right scriptPath -> do
          absolutePath <- makeAbsolute scriptPath
          let processSpec = proc "node" [absolutePath]
              stdinJson = T.unpack (TE.decodeUtf8 (BL.toStrict (Aeson.encode validatedPayload)))
          processResult <- try (readCreateProcessWithExitCode processSpec stdinJson)
          case processResult of
            Left err ->
              pure $
                Left $
                  T.pack
                    ( "SRI invoice script could not be started: "
                        <> displayException (err :: IOException)
                    )
            Right (exitCode, stdoutTxt, stderrTxt) ->
              case exitCode of
                ExitSuccess ->
                  pure (decodeSriScriptOutputForRequest validatedPayload stdoutTxt)
                ExitFailure _ ->
                  pure (Left (formatSriScriptFailure stderrTxt))

formatSriScriptFailure :: String -> Text
formatSriScriptFailure stderrTxt =
  let sanitized = T.strip (T.map sanitizeFailureChar (T.pack stderrTxt))
      redacted = redactSriScriptFailureSecrets sanitized
  in if T.null redacted
       then "SRI invoice script failed"
       else limitSriScriptFailure redacted
  where
    sanitizeFailureChar ch
      | ch == '\n' || ch == '\t' = ch
      | ch == '\DEL' || ch < ' ' = ' '
      | generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator] = ' '
      | otherwise = ch

redactSriScriptFailureSecrets :: Text -> Text
redactSriScriptFailureSecrets =
  redactSriScriptFailureFields . redactSriScriptFailureBearerTokens

redactSriScriptFailureBearerTokens :: Text -> Text
redactSriScriptFailureBearerTokens = go Nothing
  where
    go _ textValue
      | T.null textValue = ""
    go previous textValue =
      case matchSriScriptFailureBearerToken previous textValue of
        Just (prefix, rest) ->
          prefix <> "[redacted]" <> go Nothing rest
        Nothing ->
          let ch = T.head textValue
          in T.singleton ch <> go (Just ch) (T.tail textValue)

matchSriScriptFailureBearerToken :: Maybe Char -> Text -> Maybe (Text, Text)
matchSriScriptFailureBearerToken previous textValue
  | not (isSriScriptFailureSecretBoundary previous) = Nothing
  | not ("bearer" `T.isPrefixOf` T.toLower textValue) = Nothing
  | otherwise =
      let bearerText = T.take 6 textValue
          afterBearer = T.drop 6 textValue
          (between, tokenStart) = T.span isSpace afterBearer
          (openingQuote, tokenText, isValueEnd) =
            consumeSriScriptFailureValueOpeningQuote tokenStart
          (tokenValue, rest) = T.break isValueEnd tokenText
      in if T.null between || T.null tokenValue || not (T.any isSriScriptFailureSecretAtom tokenValue)
           then Nothing
           else Just (bearerText <> between <> openingQuote, rest)

redactSriScriptFailureFields :: Text -> Text
redactSriScriptFailureFields = go Nothing
  where
    go _ textValue
      | T.null textValue = ""
    go previous textValue =
      case matchSriScriptFailureField previous textValue of
        Just (prefix, rest) ->
          prefix <> "[redacted]" <> go Nothing rest
        Nothing ->
          let ch = T.head textValue
          in T.singleton ch <> go (Just ch) (T.tail textValue)

matchSriScriptFailureField :: Maybe Char -> Text -> Maybe (Text, Text)
matchSriScriptFailureField previous textValue
  | not (isSriScriptFailureSecretBoundary previous) = Nothing
  | otherwise = firstMatch sensitiveSriScriptFailureFields
  where
    lowered = T.toLower textValue

    firstMatch [] = Nothing
    firstMatch (fieldName:rest) =
      case parseSriScriptFailureField fieldName lowered textValue of
        Just match -> Just match
        Nothing -> firstMatch rest

sensitiveSriScriptFailureFields :: [Text]
sensitiveSriScriptFailureFields =
  [ "access_token"
  , "api_key"
  , "apikey"
  , "authorization"
  , "certificate_password"
  , "certificatepassword"
  , "client_secret"
  , "password"
  , "private_key"
  , "refresh_token"
  ]

parseSriScriptFailureField :: Text -> Text -> Text -> Maybe (Text, Text)
parseSriScriptFailureField fieldName lowered textValue
  | not (fieldName `T.isPrefixOf` lowered) = Nothing
  | otherwise = do
      let fieldLength = T.length fieldName
          fieldText = T.take fieldLength textValue
          afterField = T.drop fieldLength textValue
          (closingQuote, afterClosingQuote) =
            consumeSriScriptFailureOptionalQuote afterField
          (beforeSeparator, separatorCandidate) = T.span isSpace afterClosingQuote
      (separator, afterSeparator) <- T.uncons separatorCandidate
      if separator == '=' || separator == ':'
        then
          let (afterSeparatorSpace, valueStart) = T.span isSpace afterSeparator
              (openingQuote, valueText, isValueEnd) =
                consumeSriScriptFailureValueOpeningQuote valueStart
              (_, rest) = T.break isValueEnd valueText
              prefix =
                fieldText
                  <> closingQuote
                  <> beforeSeparator
                  <> T.singleton separator
                  <> afterSeparatorSpace
                  <> openingQuote
          in Just (prefix, rest)
        else Nothing

isSriScriptFailureSecretBoundary :: Maybe Char -> Bool
isSriScriptFailureSecretBoundary Nothing = True
isSriScriptFailureSecretBoundary (Just ch) =
  not (isAlphaNum ch || ch == '_' || ch == '-')

isSriScriptFailureSecretAtom :: Char -> Bool
isSriScriptFailureSecretAtom ch =
  isAlphaNum ch || ch `elem` (".-_~+/=" :: String)

consumeSriScriptFailureOptionalQuote :: Text -> (Text, Text)
consumeSriScriptFailureOptionalQuote textValue =
  case T.uncons textValue of
    Just ('"', rest) -> ("\"", rest)
    Just ('\'', rest) -> ("'", rest)
    _ -> ("", textValue)

consumeSriScriptFailureValueOpeningQuote :: Text -> (Text, Text, Char -> Bool)
consumeSriScriptFailureValueOpeningQuote textValue =
  case T.uncons textValue of
    Just ('"', rest) -> ("\"", rest, (== '"'))
    Just ('\'', rest) -> ("'", rest, (== '\''))
    _ -> ("", textValue, isUnquotedSriScriptFailureSecretValueEnd)

isUnquotedSriScriptFailureSecretValueEnd :: Char -> Bool
isUnquotedSriScriptFailureSecretValueEnd ch =
  isSpace ch || ch `elem` ("&,;}]" :: String)

limitSriScriptFailure :: Text -> Text
limitSriScriptFailure message
  | T.length message <= maxSriScriptFailureChars = message
  | otherwise =
      T.take maxSriScriptFailureChars message <> "\n[SRI script stderr truncated]"

maxSriScriptFailureChars :: Int
maxSriScriptFailureChars = 2000

decodeSriScriptOutput :: String -> Either Text SriIssueResultDTO
decodeSriScriptOutput stdoutTxt =
  case Aeson.eitherDecodeStrict' (TE.encodeUtf8 (T.pack stdoutTxt)) of
    Left err -> Left (T.pack ("Invalid SRI script JSON output: " <> err))
    Right dto -> validateSriScriptResult dto

decodeSriScriptOutputForRequest :: SriScriptRequest -> String -> Either Text SriIssueResultDTO
decodeSriScriptOutputForRequest request stdoutTxt = do
  validatedRequest <- validateSriScriptRequest request
  decodeSriScriptOutput stdoutTxt >>= validateSriScriptResultTotal validatedRequest

validateSriScriptResultTotal :: SriScriptRequest -> SriIssueResultDTO -> Either Text SriIssueResultDTO
validateSriScriptResultTotal request result =
  case sirTotal result of
    Nothing ->
      Left "SRI script JSON output total is required"
    Just totalValue ->
      let scaled = totalValue * 100
          reportedCents = round scaled :: Integer
          expectedCents = sriRequestTotalCents request
      in if abs (scaled - fromIntegral reportedCents) > sriTotalCentEpsilon
           then Left "SRI script JSON output total must use cents precision"
           else if reportedCents /= expectedCents
             then
               Left $
                 "SRI script JSON output total does not match request total: expected "
                   <> T.pack (show expectedCents)
                   <> " cents, got "
                   <> T.pack (show reportedCents)
                   <> " cents"
             else Right result

sriRequestTotalCents :: SriScriptRequest -> Integer
sriRequestTotalCents request =
  sum (map sriLineTotalCents (lines request))

sriLineTotalCents :: SriScriptLine -> Integer
sriLineTotalCents line =
  fromIntegral (quantity line) * fromIntegral (unitCents line)

sriTotalCentEpsilon :: Double
sriTotalCentEpsilon = 0.000001

validateSriScriptResult :: SriIssueResultDTO -> Either Text SriIssueResultDTO
validateSriScriptResult dto =
  let statusValue = T.strip (sirStatus dto)
      canonicalStatus = canonicalSriStatus statusValue
  in if T.null statusValue
       then Left "SRI script JSON output status is required"
       else if T.any isInvalidVisibleTextChar statusValue
         then Left "SRI script JSON output status must not contain control characters or hidden formatting characters"
         else
             validateScriptTotal dto { sirStatus = canonicalStatus }
             >>= validateIssuedResult
             >>= validateOptionalDocumentIdentifiers
             >>= validateOptionalTargetId
             >>= validateOptionalBuyer
             >>= validateOptionalBuyerEmail
  where
    validateOptionalTargetId result = do
      targetIdValue <-
        validateOptionalOutputField
          "targetId"
          validateSriTargetId
          (sirTargetId result)
      Right result { sirTargetId = targetIdValue }

    validateOptionalBuyer result = do
      buyerValue <- traverse validateSriBuyer (sirBuyer result)
      Right result { sirBuyer = buyerValue }

    validateSriBuyer buyer = do
      rucValue <- validateOutputTaxIdField "buyer.ruc" (sibRuc buyer)
      legalNameValue <- validateOutputRequiredTextField "buyer.legalName" (sibLegalName buyer)
      emailValue <- validateOutputOptionalEmailField "buyer.email" (sibEmail buyer)
      phoneValue <- validateOutputOptionalTextField "buyer.phone" (sibPhone buyer)
      Right buyer
        { sibRuc = rucValue
        , sibLegalName = legalNameValue
        , sibEmail = emailValue
        , sibPhone = phoneValue
        }

    validateOptionalBuyerEmail result = do
      buyerEmail <-
        validateOptionalOutputField
          "buyerEmail"
          validateSriBuyerEmail
          (sirBuyerEmail result)
      case (buyerEmail, sirBuyer result >>= sibEmail) of
        (Just topLevelEmail, Just nestedEmail)
          | topLevelEmail /= nestedEmail ->
              Left "SRI script JSON output buyerEmail must match buyer.email when both are present"
        _ ->
          Right result { sirBuyerEmail = buyerEmail }

    validateOptionalDocumentIdentifiers result = do
      authorizationNumber <-
        validateOptionalOutputField
          "authorizationNumber"
          validateSriAuthorizationNumber
          (sirAuthorizationNumber result)
      invoiceNumber <-
        validateOptionalOutputField
          "invoiceNumber"
          validateIssuedInvoiceNumber
          (sirInvoiceNumber result)
      Right result
        { sirAuthorizationNumber = authorizationNumber
        , sirInvoiceNumber = invoiceNumber
        }

    validateOptionalOutputField fieldName validateShape mValue =
      case T.strip <$> mValue of
        Nothing -> Right Nothing
        Just value
          | T.null value -> Right Nothing
          | T.any isInvalidVisibleTextChar value ->
              Left
                ( "SRI script JSON output "
                    <> fieldName
                    <> " must not contain control characters or hidden formatting characters"
                )
          | otherwise ->
              Just <$> validateShape value

    validateScriptTotal result =
      case sirTotal result of
        Nothing -> Right result
        Just total
          | isNaN total || isInfinite total || total < 0 ->
              Left "SRI script JSON output total must be a finite non-negative number"
          | otherwise -> Right result

    validateIssuedResult result
      | sirStatus result /= "issued" = Right result
      | not (sirOk result) =
          Left "SRI script JSON output ok must be true when status is issued"
      | otherwise = do
          authorizationNumber <-
            validateSriAuthorizationNumber
              =<< validateRequiredIssuedField "authorizationNumber" (sirAuthorizationNumber result)
          invoiceNumber <-
            validateIssuedInvoiceNumber
              =<< validateRequiredIssuedField "invoiceNumber" (sirInvoiceNumber result)
          Right result
            { sirAuthorizationNumber = Just authorizationNumber
            , sirInvoiceNumber = Just invoiceNumber
            }

    validateRequiredIssuedField fieldName mValue =
      case T.strip <$> mValue of
        Nothing ->
          Left ("SRI script JSON output " <> fieldName <> " is required when status is issued")
        Just value
          | T.null value ->
              Left ("SRI script JSON output " <> fieldName <> " is required when status is issued")
          | T.any isInvalidVisibleTextChar value ->
              Left
                ( "SRI script JSON output "
                    <> fieldName
                    <> " must not contain control characters or hidden formatting characters"
                )
          | otherwise ->
              Right value

    validateIssuedInvoiceNumber value =
      if isSriInvoiceNumber value
        then Right value
        else
          Left
            "SRI script JSON output invoiceNumber must use SRI format ###-###-#########"

    validateSriAuthorizationNumber value =
      if T.length value == 49 && T.all isAsciiDigit value
        then Right value
        else
          Left
            "SRI script JSON output authorizationNumber must contain exactly 49 ASCII digits"

    validateSriBuyerEmail value =
      let normalized = T.toLower value
      in if T.length normalized > maxSriEmailChars
           then Left "SRI script JSON output buyerEmail must be 254 characters or fewer"
           else if isValidSriEmail normalized
             then Right normalized
             else Left "SRI script JSON output buyerEmail must be a valid email address"

    validateSriTargetId value
      | T.length value > maxSriTargetIdChars =
          Left "SRI script JSON output targetId must be 128 characters or fewer"
      | not (T.all isSafeSriTargetIdChar value) =
          Left $
            "SRI script JSON output targetId must contain only ASCII letters, digits, "
              <> "dots, dashes, underscores, or colons"
      | otherwise = Right value

    isSafeSriTargetIdChar ch =
      isAsciiLower ch
        || isAsciiUpper ch
        || isAsciiDigit ch
        || ch `elem` (".-_:" :: String)

    maxSriTargetIdChars :: Int
    maxSriTargetIdChars = 128

    isSriInvoiceNumber value =
      case T.splitOn "-" value of
        [establishmentCode, emissionCode, sequential]
          | T.length establishmentCode == 3
          , T.length emissionCode == 3
          , T.length sequential == 9 ->
              T.all isAsciiDigit establishmentCode
                && T.all isAsciiDigit emissionCode
                && T.all isAsciiDigit sequential
        _ ->
          False

canonicalSriStatus :: Text -> Text
canonicalSriStatus statusValue
  | T.toLower statusValue == "issued" = "issued"
  | otherwise = statusValue

validateOutputTaxIdField :: Text -> Text -> Either Text Text
validateOutputTaxIdField fieldName raw = do
  value <- validateOutputRequiredTextField fieldName raw
  if not (T.all isAsciiDigit value)
    then Left (outputFieldMessage fieldName "must contain ASCII digits only")
    else if T.length value == 10 || T.length value == 13
      then Right value
      else Left (outputFieldMessage fieldName "must contain 10 or 13 digits")

validateOutputRequiredTextField :: Text -> Text -> Either Text Text
validateOutputRequiredTextField fieldName raw =
  let value = T.strip raw
  in if T.null value
       then Left (outputFieldMessage fieldName "is required")
       else if T.any isInvalidVisibleTextChar value
         then
           Left
             ( outputFieldMessage
                 fieldName
                 "must not contain control characters or hidden formatting characters"
             )
         else Right value

validateOutputOptionalTextField :: Text -> Maybe Text -> Either Text (Maybe Text)
validateOutputOptionalTextField _ Nothing = Right Nothing
validateOutputOptionalTextField fieldName (Just raw) =
  let value = T.strip raw
  in if T.null value
       then Right Nothing
       else if T.any isInvalidVisibleTextChar value
         then
           Left
             ( outputFieldMessage
                 fieldName
                 "must not contain control characters or hidden formatting characters"
             )
         else Right (Just value)

validateOutputOptionalEmailField :: Text -> Maybe Text -> Either Text (Maybe Text)
validateOutputOptionalEmailField fieldName raw = do
  mValue <- validateOutputOptionalTextField fieldName raw
  case mValue of
    Nothing -> Right Nothing
    Just value ->
      let normalized = T.toLower value
      in if T.length normalized > maxSriEmailChars
           then Left (outputFieldMessage fieldName "must be 254 characters or fewer")
           else if isValidSriEmail normalized
             then Right (Just normalized)
             else Left (outputFieldMessage fieldName "must be a valid email address")

outputFieldMessage :: Text -> Text -> Text
outputFieldMessage fieldName message =
  "SRI script JSON output " <> fieldName <> " " <> message

validateSriScriptRequest :: SriScriptRequest -> Either Text SriScriptRequest
validateSriScriptRequest request = do
  customerValue <- validateCustomer (customer request)
  lineValues <- validateLines (lines request)
  establishmentValue <- validateNumericField "establishment" (establishment request)
  emissionPointValue <- validateNumericField "emissionPoint" (emissionPoint request)
  paymentModeValue <- validatePaymentMode (paymentMode request)
  certificatePasswordValue <-
    validateOptionalSecretField "certificatePassword" (certificatePassword request)
  pure request
    { customer = customerValue
    , lines = lineValues
    , establishment = establishmentValue
    , emissionPoint = emissionPointValue
    , paymentMode = paymentModeValue
    , certificatePassword = certificatePasswordValue
    }

validateCustomer :: SriScriptCustomer -> Either Text SriScriptCustomer
validateCustomer value = do
  rucValue <- validateCustomerRuc (ruc value)
  legalNameValue <- validateRequiredTextField "customer.legalName" (legalName value)
  emailValue <- validateOptionalEmailField "customer.email" (email value)
  phoneValue <- validateOptionalTextField "customer.phone" (phone value)
  pure value
    { ruc = rucValue
    , legalName = legalNameValue
    , email = emailValue
    , phone = phoneValue
    }

validateCustomerRuc :: Text -> Either Text Text
validateCustomerRuc raw = do
  value <- validateRequiredTextField "customer.ruc" raw
  if not (T.all isAsciiDigit value)
    then Left (fieldMessage "customer.ruc" "must contain ASCII digits only")
    else if T.length value == 10 || T.length value == 13
      then Right value
      else Left (fieldMessage "customer.ruc" "must contain 10 or 13 digits")

validateOptionalEmailField :: Text -> Maybe Text -> Either Text (Maybe Text)
validateOptionalEmailField fieldName raw = do
  mValue <- validateOptionalTextField fieldName raw
  case mValue of
    Nothing -> Right Nothing
    Just value ->
      let normalized = T.toLower value
      in if T.length normalized > maxSriEmailChars
           then Left (fieldMessage fieldName "must be 254 characters or fewer")
           else if isValidSriEmail normalized
             then Right (Just normalized)
             else Left (fieldMessage fieldName "must be a valid email address")

maxSriEmailChars :: Int
maxSriEmailChars = 254

maxSriEmailLocalPartChars :: Int
maxSriEmailLocalPartChars = 64

maxSriEmailDomainLabelChars :: Int
maxSriEmailDomainLabelChars = 63

isValidSriEmail :: Text -> Bool
isValidSriEmail candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      isValidSriEmailLocalPart localPart
        && T.isInfixOf "." domain
        && hasValidSriEmailFinalDomainLabel domain
        && all isValidSriEmailDomainLabel (T.splitOn "." domain)
    _ ->
      False

hasValidSriEmailFinalDomainLabel :: Text -> Bool
hasValidSriEmailFinalDomainLabel domain =
  case reverse (T.splitOn "." domain) of
    finalLabel : _ ->
      T.length finalLabel >= 2 && T.any isAsciiLower finalLabel
    _ ->
      False

isValidSriEmailLocalPart :: Text -> Bool
isValidSriEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= maxSriEmailLocalPartChars
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (".." `T.isInfixOf` localPart)
    && T.all isValidSriEmailLocalChar localPart

isValidSriEmailLocalChar :: Char -> Bool
isValidSriEmailLocalChar ch =
  isAsciiLower ch || isAsciiDigit ch || ch `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidSriEmailDomainLabel :: Text -> Bool
isValidSriEmailDomainLabel label =
  not (T.null label)
    && T.length label <= maxSriEmailDomainLabelChars
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidSriEmailDomainChar label

isValidSriEmailDomainChar :: Char -> Bool
isValidSriEmailDomainChar ch =
  isAsciiLower ch || isAsciiDigit ch || ch == '-'

validateLines :: [SriScriptLine] -> Either Text [SriScriptLine]
validateLines [] =
  Left "SRI script request requires at least one invoice line"
validateLines values
  | length values > maxSriScriptLineItems =
      Left $
        "SRI script request supports at most "
          <> T.pack (show maxSriScriptLineItems)
          <> " invoice lines"
validateLines values = do
  lineValues <- traverse validateLine (zip [(1 :: Int)..] values)
  when (sum (map sriLineTotalCents lineValues) > maxSriScriptJsonInteger) $
    Left (fieldMessage "totalCents" maxSriScriptJsonIntegerMessage)
  pure lineValues

maxSriScriptLineItems :: Int
maxSriScriptLineItems = 100

maxSriScriptJsonInteger :: Integer
maxSriScriptJsonInteger = 9007199254740991

maxSriScriptJsonIntegerMessage :: Text
maxSriScriptJsonIntegerMessage =
  "must be 9007199254740991 or less"

validateLine :: (Int, SriScriptLine) -> Either Text SriScriptLine
validateLine (index, value) = do
  codeValue <- validateOptionalTextField (lineField "code") (code value)
  auxiliaryCodeValue <-
    validateOptionalTextField (lineField "auxiliaryCode") (auxiliaryCode value)
  descriptionValue <-
    validateRequiredTextField (lineField "description") (description value)
  when (quantity value <= 0) $
    Left (fieldMessage (lineField "quantity") "must be greater than zero")
  when (unitCents value < 0) $
    Left (fieldMessage (lineField "unitCents") "must be zero or greater")
  when (fromIntegral (quantity value) > maxSriScriptJsonInteger) $
    Left (fieldMessage (lineField "quantity") maxSriScriptJsonIntegerMessage)
  when (fromIntegral (unitCents value) > maxSriScriptJsonInteger) $
    Left (fieldMessage (lineField "unitCents") maxSriScriptJsonIntegerMessage)
  when (sriLineTotalCents value > maxSriScriptJsonInteger) $
    Left (fieldMessage (lineField "totalCents") maxSriScriptJsonIntegerMessage)
  taxBpsValue <- validateTaxBps (lineField "taxBps") (taxBps value)
  additionalInfoValue <-
    validateOptionalTextField (lineField "sriAdditionalInfo") (sriAdditionalInfo value)
  ivaCodeValue <- validateOptionalSriIvaCode (lineField "sriIvaCode") (sriIvaCode value)
  case (taxBpsValue, ivaCodeValue) of
    (Just bps, Nothing)
      | bps `notElem` [0, 500, 1500] ->
          Left $
            fieldMessage
              (lineField "taxBps")
              "must be 0, 500, or 1500 unless sriIvaCode is provided"
    _ ->
      pure ()
  pure value
    { code = codeValue
    , auxiliaryCode = auxiliaryCodeValue
    , description = descriptionValue
    , taxBps = taxBpsValue
    , sriAdditionalInfo = additionalInfoValue
    , sriIvaCode = ivaCodeValue
    }
  where
    lineField fieldName =
      "lines[" <> T.pack (show index) <> "]." <> fieldName

validateTaxBps :: Text -> Maybe Int -> Either Text (Maybe Int)
validateTaxBps _ Nothing = Right Nothing
validateTaxBps fieldName (Just value)
  | value < 0 = Left (fieldMessage fieldName "must be zero or greater")
  | value > 10000 = Left (fieldMessage fieldName "must be 10000 or less")
  | otherwise = Right (Just value)

validateOptionalSriIvaCode :: Text -> Maybe Text -> Either Text (Maybe Text)
validateOptionalSriIvaCode fieldName raw = do
  mValue <- validateOptionalTextField fieldName raw
  case mValue of
    Nothing -> Right Nothing
    Just value
      | not (T.all isAsciiDigit value) ->
          Left (fieldMessage fieldName "must contain ASCII digits only")
      | T.length value > 4 ->
          Left (fieldMessage fieldName "must be 4 digits or fewer")
      | otherwise -> Right (Just value)

validatePaymentMode :: Text -> Either Text Text
validatePaymentMode raw = do
  value <- validateRequiredTextField "paymentMode" raw
  let normalized = T.toLower value
  if normalized `elem` ["cash", "debit", "credit"]
    then Right normalized
    else Left "SRI script request paymentMode must be one of: cash, debit, credit"

validateNumericField :: Text -> Text -> Either Text Text
validateNumericField fieldName raw = do
  value <- validateRequiredTextField fieldName raw
  if T.all isAsciiDigit value
    then
      if T.length value == 3
        then Right value
        else Left (fieldMessage fieldName "must contain exactly 3 digits")
    else Left (fieldMessage fieldName "must contain ASCII digits only")

validateRequiredTextField :: Text -> Text -> Either Text Text
validateRequiredTextField fieldName raw =
  let value = T.strip raw
  in if T.null value
       then Left (fieldMessage fieldName "is required")
       else if T.any isInvalidVisibleTextChar value
         then Left (fieldMessage fieldName "must not contain control characters or hidden formatting characters")
         else Right value

validateOptionalTextField :: Text -> Maybe Text -> Either Text (Maybe Text)
validateOptionalTextField _ Nothing = Right Nothing
validateOptionalTextField fieldName (Just raw) =
  let value = T.strip raw
  in if T.null value
       then Right Nothing
       else if T.any isInvalidVisibleTextChar value
         then Left (fieldMessage fieldName "must not contain control characters or hidden formatting characters")
         else Right (Just value)

validateOptionalSecretField :: Text -> Maybe Text -> Either Text (Maybe Text)
validateOptionalSecretField _ Nothing = Right Nothing
validateOptionalSecretField fieldName (Just raw)
  | T.null (T.strip raw) = Right Nothing
  | T.any isInvalidVisibleTextChar raw =
      Left $
        fieldMessage
          fieldName
          "must not contain control characters or hidden formatting characters"
  | otherwise = Right (Just raw)

isInvalidVisibleTextChar :: Char -> Bool
isInvalidVisibleTextChar ch =
  isControlTextChar ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

isControlTextChar :: Char -> Bool
isControlTextChar ch = ch == '\DEL' || ch < ' '

isAsciiDigit :: Char -> Bool
isAsciiDigit ch = ch >= '0' && ch <= '9'

isAsciiLower :: Char -> Bool
isAsciiLower ch = ch >= 'a' && ch <= 'z'

isAsciiUpper :: Char -> Bool
isAsciiUpper ch = ch >= 'A' && ch <= 'Z'

fieldMessage :: Text -> Text -> Text
fieldMessage fieldName message =
  "SRI script request " <> fieldName <> " " <> message

resolveScriptPath :: IO (Either Text FilePath)
resolveScriptPath = do
  envPath <- lookupEnv "SRI_INVOICE_SCRIPT"
  case envPath of
    Just rawPath ->
      case normalizeConfiguredScriptPath rawPath of
        Right scriptPath -> validateConfiguredScriptPath scriptPath
        Left err -> pure (Left err)
    Nothing ->
      discoverDefaultScriptPath
        ["scripts/generate-sri-invoice.mjs", "../scripts/generate-sri-invoice.mjs"]

validateConfiguredScriptPath :: FilePath -> IO (Either Text FilePath)
validateConfiguredScriptPath scriptPath
  | not (isNodeScriptPath scriptPath) =
      pure (Left (invalidConfiguredScriptMessage scriptPath))
  | otherwise = do
      exists <- doesFileExist scriptPath
      pure $
        if exists
          then Right scriptPath
          else Left (missingConfiguredScriptMessage scriptPath)

isNodeScriptPath :: FilePath -> Bool
isNodeScriptPath scriptPath =
  T.toLower (T.pack (takeExtension scriptPath)) `elem` [".mjs", ".js", ".cjs"]

missingConfiguredScriptMessage :: FilePath -> Text
missingConfiguredScriptMessage scriptPath =
  "SRI_INVOICE_SCRIPT does not point to an existing file: " <> T.pack scriptPath

invalidConfiguredScriptMessage :: FilePath -> Text
invalidConfiguredScriptMessage scriptPath =
  "SRI_INVOICE_SCRIPT must point to a .mjs, .js, or .cjs Node script: "
    <> T.pack scriptPath

blankConfiguredScriptMessage :: Text
blankConfiguredScriptMessage =
  "SRI_INVOICE_SCRIPT must not be blank; unset it to use the default script discovery."

missingDefaultScriptMessage :: Text
missingDefaultScriptMessage =
  "Could not find scripts/generate-sri-invoice.mjs. Set SRI_INVOICE_SCRIPT to enable SRI emission."

ambiguousDefaultScriptMessage :: [FilePath] -> Text
ambiguousDefaultScriptMessage paths =
  "Found multiple default SRI invoice scripts: "
    <> T.intercalate ", " (map T.pack paths)
    <> ". Set SRI_INVOICE_SCRIPT to choose one explicitly."

normalizeConfiguredScriptPath :: String -> Either Text FilePath
normalizeConfiguredScriptPath raw =
  let rawText = T.pack raw
      trimmedText = T.strip rawText
      trimmed = T.unpack trimmedText
  in if null trimmed
       then Left blankConfiguredScriptMessage
       else if T.any isInvalidVisibleTextChar rawText
         then Left invalidConfiguredScriptControlMessage
       else if rawText /= trimmedText
         then Left whitespaceConfiguredScriptMessage
       else if not (isAbsolute trimmed)
         then Left relativeConfiguredScriptMessage
       else if hasAmbiguousConfiguredScriptPathSegments trimmed
         then Left nonCanonicalConfiguredScriptMessage
       else Right trimmed

invalidConfiguredScriptControlMessage :: Text
invalidConfiguredScriptControlMessage =
  "SRI_INVOICE_SCRIPT must not contain control characters or hidden formatting characters."

whitespaceConfiguredScriptMessage :: Text
whitespaceConfiguredScriptMessage =
  "SRI_INVOICE_SCRIPT must not include leading or trailing whitespace."

relativeConfiguredScriptMessage :: Text
relativeConfiguredScriptMessage =
  "SRI_INVOICE_SCRIPT must be an absolute path; unset it to use the default script discovery."

nonCanonicalConfiguredScriptMessage :: Text
nonCanonicalConfiguredScriptMessage =
  "SRI_INVOICE_SCRIPT must be a normalized absolute file path without . or .. segments."

hasAmbiguousConfiguredScriptPathSegments :: FilePath -> Bool
hasAmbiguousConfiguredScriptPathSegments scriptPath =
  normalise scriptPath /= scriptPath
    || any (`elem` [".", ".."]) (splitDirectories scriptPath)

discoverDefaultScriptPath :: [FilePath] -> IO (Either Text FilePath)
discoverDefaultScriptPath candidates = do
  case traverse validateDefaultScriptCandidate candidates of
    Left err -> pure (Left err)
    Right validCandidates -> do
      existing <- existingFiles validCandidates
      pure $
        case existing of
          [] -> Left missingDefaultScriptMessage
          [scriptPath] -> Right scriptPath
          paths -> Left (ambiguousDefaultScriptMessage paths)

validateDefaultScriptCandidate :: FilePath -> Either Text FilePath
validateDefaultScriptCandidate scriptPath
  | T.null trimmedText = Left blankDefaultScriptCandidateMessage
  | T.any isInvalidVisibleTextChar rawText =
      Left invalidDefaultScriptCandidateControlMessage
  | rawText /= trimmedText = Left whitespaceDefaultScriptCandidateMessage
  | isNodeScriptPath scriptPath = Right scriptPath
  | otherwise = Left (invalidDefaultScriptMessage scriptPath)
  where
    rawText = T.pack scriptPath
    trimmedText = T.strip rawText

invalidDefaultScriptMessage :: FilePath -> Text
invalidDefaultScriptMessage scriptPath =
  "Default SRI invoice script discovery candidate must point to a .mjs, .js, or .cjs Node script: "
    <> T.pack scriptPath

blankDefaultScriptCandidateMessage :: Text
blankDefaultScriptCandidateMessage =
  "Default SRI invoice script discovery candidate must not be blank."

invalidDefaultScriptCandidateControlMessage :: Text
invalidDefaultScriptCandidateControlMessage =
  "Default SRI invoice script discovery candidate must not contain control characters or hidden formatting characters."

whitespaceDefaultScriptCandidateMessage :: Text
whitespaceDefaultScriptCandidateMessage =
  "Default SRI invoice script discovery candidate must not include leading or trailing whitespace."

existingFiles :: [FilePath] -> IO [FilePath]
existingFiles [] = pure []
existingFiles (candidate:rest) = do
  exists <- doesFileExist candidate
  remaining <- existingFiles rest
  pure $
    if exists
      then candidate : remaining
      else remaining
