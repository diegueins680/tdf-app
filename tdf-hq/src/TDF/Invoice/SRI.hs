{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Invoice.SRI
  ( SriScriptCustomer(..)
  , SriScriptLine(..)
  , SriScriptRequest(..)
  , runSriInvoiceScript
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           GHC.Generics (Generic)
import           System.Directory (doesFileExist, makeAbsolute)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.Process (proc, readCreateProcessWithExitCode)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL

import           TDF.DTO (SriIssueResultDTO)

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
          stdinJson = BS8.unpack (BL.toStrict (Aeson.encode payload))
      (exitCode, stdoutTxt, stderrTxt) <- readCreateProcessWithExitCode processSpec stdinJson
      case exitCode of
        ExitSuccess ->
          case Aeson.eitherDecodeStrict' (BS8.pack stdoutTxt) of
            Left err -> pure (Left (T.pack ("Invalid SRI script JSON output: " <> err)))
            Right dto -> pure (Right dto)
        ExitFailure _ ->
          let trimmedErr = T.strip (TE.decodeUtf8 (BS8.pack stderrTxt))
          in pure (Left (if T.null trimmedErr then "SRI invoice script failed" else trimmedErr))

resolveScriptPath :: IO (Either Text FilePath)
resolveScriptPath = do
  envPath <- lookupEnv "SRI_INVOICE_SCRIPT"
  case envPath >>= nonEmptyPath of
    Just scriptPath -> do
      exists <- doesFileExist scriptPath
      pure $
        if exists
          then Right scriptPath
          else Left (missingConfiguredScriptMessage scriptPath)
    Nothing -> do
      mDefaultPath <-
        firstExisting ["scripts/generate-sri-invoice.mjs", "../scripts/generate-sri-invoice.mjs"]
      pure $
        maybe
          (Left missingDefaultScriptMessage)
          Right
          mDefaultPath

missingConfiguredScriptMessage :: FilePath -> Text
missingConfiguredScriptMessage scriptPath =
  "SRI_INVOICE_SCRIPT does not point to an existing file: " <> T.pack scriptPath

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
