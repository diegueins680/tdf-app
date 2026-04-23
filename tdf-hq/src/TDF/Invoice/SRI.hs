{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Invoice.SRI
  ( SriScriptCustomer(..)
  , SriScriptLine(..)
  , SriScriptRequest(..)
  , decodeSriScriptOutput
  , runSriInvoiceScript
  ) where

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
  scriptPathResult <- resolveScriptPath
  case scriptPathResult of
    Left err -> pure (Left err)
    Right scriptPath -> do
      absolutePath <- makeAbsolute scriptPath
      let processSpec = proc "node" [absolutePath]
          stdinJson = T.unpack (TE.decodeUtf8 (BL.toStrict (Aeson.encode payload)))
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

resolveScriptPath :: IO (Either Text FilePath)
resolveScriptPath = do
  envPath <- lookupEnv "SRI_INVOICE_SCRIPT"
  case envPath >>= nonEmptyPath of
    Just scriptPath -> validateConfiguredScriptPath scriptPath
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
