{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Invoice.SRI
  ( SriScriptCustomer(..)
  , SriScriptLine(..)
  , SriScriptRequest(..)
  , decodeSriScriptOutput
  , validateSriScriptRequest
  , runSriInvoiceScript
  ) where

import           Prelude hiding (lines)

import           Control.Monad (when)
import           Data.Char (isDigit)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           GHC.Generics (Generic)
import           Control.Exception (IOException, displayException, try)
import           System.Directory (doesFileExist, makeAbsolute)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.FilePath (takeExtension)
import           System.Process (proc, readCreateProcessWithExitCode)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL

import           TDF.DTO (SriIssueResultDTO(..))

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
                  pure (decodeSriScriptOutput stdoutTxt)
                ExitFailure _ ->
                  let trimmedErr = T.strip (T.pack stderrTxt)
                  in pure (Left (if T.null trimmedErr then "SRI invoice script failed" else trimmedErr))

decodeSriScriptOutput :: String -> Either Text SriIssueResultDTO
decodeSriScriptOutput stdoutTxt =
  case Aeson.eitherDecodeStrict' (TE.encodeUtf8 (T.pack stdoutTxt)) of
    Left err -> Left (T.pack ("Invalid SRI script JSON output: " <> err))
    Right dto -> validateSriScriptResult dto

validateSriScriptResult :: SriIssueResultDTO -> Either Text SriIssueResultDTO
validateSriScriptResult dto =
  let statusValue = T.strip (sirStatus dto)
  in if T.null statusValue
       then Left "SRI script JSON output status is required"
       else if T.any isInvalidStatusChar statusValue
         then Left "SRI script JSON output status must not contain control characters"
         else validateScriptTotal dto { sirStatus = statusValue } >>= validateIssuedResult
  where
    isInvalidStatusChar ch = ch == '\DEL' || ch < ' '

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
            validateRequiredIssuedField "authorizationNumber" (sirAuthorizationNumber result)
          invoiceNumber <-
            validateRequiredIssuedField "invoiceNumber" (sirInvoiceNumber result)
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
          | T.any isInvalidStatusChar value ->
              Left
                ( "SRI script JSON output "
                    <> fieldName
                    <> " must not contain control characters"
                )
          | otherwise ->
              Right value

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
  rucValue <- validateRequiredTextField "customer.ruc" (ruc value)
  legalNameValue <- validateRequiredTextField "customer.legalName" (legalName value)
  emailValue <- validateOptionalTextField "customer.email" (email value)
  phoneValue <- validateOptionalTextField "customer.phone" (phone value)
  pure value
    { ruc = rucValue
    , legalName = legalNameValue
    , email = emailValue
    , phone = phoneValue
    }

validateLines :: [SriScriptLine] -> Either Text [SriScriptLine]
validateLines [] =
  Left "SRI script request requires at least one invoice line"
validateLines values =
  traverse validateLine (zip [(1 :: Int)..] values)

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
  taxBpsValue <- validateTaxBps (lineField "taxBps") (taxBps value)
  additionalInfoValue <-
    validateOptionalTextField (lineField "sriAdditionalInfo") (sriAdditionalInfo value)
  ivaCodeValue <- validateOptionalTextField (lineField "sriIvaCode") (sriIvaCode value)
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
  if T.all isDigit value
    then
      if T.length value == 3
        then Right value
        else Left (fieldMessage fieldName "must contain exactly 3 digits")
    else Left (fieldMessage fieldName "must contain digits only")

validateRequiredTextField :: Text -> Text -> Either Text Text
validateRequiredTextField fieldName raw =
  let value = T.strip raw
  in if T.null value
       then Left (fieldMessage fieldName "is required")
       else if T.any isInvalidTextChar value
         then Left (fieldMessage fieldName "must not contain control characters")
         else Right value

validateOptionalTextField :: Text -> Maybe Text -> Either Text (Maybe Text)
validateOptionalTextField _ Nothing = Right Nothing
validateOptionalTextField fieldName (Just raw) =
  let value = T.strip raw
  in if T.null value
       then Right Nothing
       else if T.any isInvalidTextChar value
         then Left (fieldMessage fieldName "must not contain control characters")
         else Right (Just value)

validateOptionalSecretField :: Text -> Maybe Text -> Either Text (Maybe Text)
validateOptionalSecretField _ Nothing = Right Nothing
validateOptionalSecretField fieldName (Just raw)
  | T.null (T.strip raw) = Right Nothing
  | T.any isInvalidTextChar raw =
      Left (fieldMessage fieldName "must not contain control characters")
  | otherwise = Right (Just raw)

isInvalidTextChar :: Char -> Bool
isInvalidTextChar ch = ch == '\DEL' || ch < ' '

fieldMessage :: Text -> Text -> Text
fieldMessage fieldName message =
  "SRI script request " <> fieldName <> " " <> message

resolveScriptPath :: IO (Either Text FilePath)
resolveScriptPath = do
  envPath <- lookupEnv "SRI_INVOICE_SCRIPT"
  case envPath of
    Just rawPath ->
      case nonEmptyPath rawPath of
        Just scriptPath -> validateConfiguredScriptPath scriptPath
        Nothing -> pure (Left blankConfiguredScriptMessage)
    Nothing -> do
      mDefaultPath <-
        firstExisting ["scripts/generate-sri-invoice.mjs", "../scripts/generate-sri-invoice.mjs"]
      pure $
        maybe
          (Left missingDefaultScriptMessage)
          Right
          mDefaultPath

validateConfiguredScriptPath :: FilePath -> IO (Either Text FilePath)
validateConfiguredScriptPath scriptPath = do
  exists <- doesFileExist scriptPath
  pure $
    if not exists
      then Left (missingConfiguredScriptMessage scriptPath)
      else
        if isNodeScriptPath scriptPath
          then Right scriptPath
          else Left (invalidConfiguredScriptMessage scriptPath)

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

nonEmptyPath :: String -> Maybe FilePath
nonEmptyPath raw =
  let trimmed = T.unpack (T.strip (T.pack raw))
  in if null trimmed then Nothing else Just trimmed

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting [] = pure Nothing
firstExisting (candidate:rest) = do
  exists <- doesFileExist candidate
  if exists then pure (Just candidate) else firstExisting rest
