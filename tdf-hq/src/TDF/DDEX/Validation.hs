{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.Validation
  ( -- * Validation
    validateDocument
  , ValidationConfig(..)
  , defaultValidationConfig
  , runXsdValidation
  , parseXmllintOutput
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime, getCurrentTime)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import TDF.DDEX.Types

-- | Configuration for validation
data ValidationConfig = ValidationConfig
  { schemaBasePath :: FilePath       -- ^ Path to XSD schemas
  , xmllintPath    :: FilePath       -- ^ Path to xmllint binary
  , validationTimeout :: Int         -- ^ Timeout in seconds
  } deriving (Show, Eq)

-- | Default validation configuration
defaultValidationConfig :: ValidationConfig
defaultValidationConfig = ValidationConfig
  { schemaBasePath = "/opt/ddex/schemas"
  , xmllintPath = "/usr/bin/xmllint"
  , validationTimeout = 60
  }

-- | Validate a DDEX document
-- Returns validation result with issues
validateDocument :: ValidationConfig -> FilePath -> DdexFamily -> Text -> IO ValidationResult
validateDocument config filePath family version = do
  -- Check if xmllint is available
  xmllintExists <- doesFileExist (xmllintPath config)
  if not xmllintExists
    then return ValidationResult
      { validationResult = False
      , resultErrors = [ValidationIssue
          { issueSeverity = SeverityError
          , issueLayer = LayerXML
          , issueCode = Just "XSLINT_MISSING"
          , issueMessage = "xmllint not available for validation"
          , issueLine = Nothing
          , issueColumn = Nothing
          , issueXPath = Nothing
          , issueSuggestion = Just "Install libxml2-utils package"
          }]
      , resultWarnings = []
      , resultInfo = []
      }
    else do
      -- Run XSD validation
      xsdResult <- runXsdValidation config filePath family version
      return xsdResult

-- | Run XSD validation using xmllint
runXsdValidation :: ValidationConfig -> FilePath -> DdexFamily -> Text -> IO ValidationResult
runXsdValidation config xmlPath family version = do
  let schemaPath = schemaBasePath config </> schemaFileName family version
  schemaExists <- doesFileExist schemaPath
  if not schemaExists
    then return ValidationResult
      { validationResult = False
      , resultErrors = [ValidationIssue
          { issueSeverity = SeverityError
          , issueLayer = LayerXSD
          , issueCode = Just "SCHEMA_MISSING"
          , issueMessage = "Schema file not found: " <> T.pack schemaPath
          , issueLine = Nothing
          , issueColumn = Nothing
          , issueXPath = Nothing
          , issueSuggestion = Just "Download DDEX schemas to schema directory"
          }]
      , resultWarnings = []
      , resultInfo = []
      }
    else do
      -- Run xmllint with schema validation
      let args = ["--schema", "--noout", "--nonet", schemaPath, xmlPath]
      (exitCode, stdout, stderr) <- readProcessWithExitCode (xmllintPath config) args ""
      let issues = parseXmllintOutput stdout stderr
          errors = filter (\i -> issueSeverity i == SeverityError) issues
          warnings = filter (\i -> issueSeverity i == SeverityWarning) issues
          infos = filter (\i -> issueSeverity i == SeverityInfo) issues
      return ValidationResult
        { validationResult = exitCode == ExitSuccess && null errors
        , resultErrors = errors
        , resultWarnings = warnings
        , resultInfo = infos
        }

-- | Get schema filename for a DDEX family and version
schemaFileName :: DdexFamily -> Text -> String
schemaFileName family version =
  let familyStr = T.unpack (familyToText family)
      versionStr = T.unpack (T.replace "." "_" version)
  in familyStr <> "_" <> versionStr <> ".xsd"

-- | Parse xmllint output into validation issues
parseXmllintOutput :: String -> String -> [ValidationIssue]
parseXmllintOutput stdout stderr =
  let output = T.pack (stdout ++ stderr)
      lines' = T.lines output
  in concatMap parseLine lines'

-- | Parse a single line of xmllint output
parseLine :: Text -> [ValidationIssue]
parseLine line
  | T.isInfixOf "error" line || T.isInfixOf "Error" line =
    [ValidationIssue
      { issueSeverity = SeverityError
      , issueLayer = LayerXSD
      , issueCode = extractCode line
      , issueMessage = extractMessage line
      , issueLine = extractLineNumber line
      , issueColumn = extractColumnNumber line
      , issueXPath = Nothing
      , issueSuggestion = Nothing
      }]
  | T.isInfixOf "warning" line || T.isInfixOf "Warning" line =
    [ValidationIssue
      { issueSeverity = SeverityWarning
      , issueLayer = LayerXSD
      , issueCode = extractCode line
      , issueMessage = extractMessage line
      , issueLine = extractLineNumber line
      , issueColumn = extractColumnNumber line
      , issueXPath = Nothing
      , issueSuggestion = Nothing
      }]
  | otherwise = []

-- | Extract error code from xmllint output
extractCode :: Text -> Maybe Text
extractCode line =
  case T.breakOn "parser error :" line of
    (_, rest) | T.isPrefixOf "parser error :" rest ->
      let afterColon = T.drop 14 rest
      in case T.words afterColon of
        (code:_) -> Just code
        [] -> Nothing
    _ -> case T.breakOn "SAX error :" line of
      (_, rest) | T.isPrefixOf "SAX error :" rest -> Just "SAX_ERROR"
      _ -> Nothing

-- | Extract error message from xmllint output
extractMessage :: Text -> Text
extractMessage line =
  -- Message is typically after the line/column info
  let parts = T.splitOn ":" line
  in case reverse parts of
    (msg:_) -> T.strip msg
    [] -> line

-- | Extract line number from xmllint output
extractLineNumber :: Text -> Maybe Int
extractLineNumber line =
  -- Look for pattern like "file.xml:42:" or "line 42"
  case T.breakOn ":" line of
    (before, _) ->
      case T.splitOn ":" before of
        [_file, lineStr] -> safeReadInt lineStr
        _ -> case T.breakOn "line " line of
          (_, rest) | T.isPrefixOf "line " rest ->
            let afterLine = T.drop 5 rest
            in case T.takeWhile isDigit afterLine of
              "" -> Nothing
              digits -> safeReadInt digits
          _ -> Nothing
  where
    isDigit c = c >= '0' && c <= '9'

-- | Extract column number from xmllint output
extractColumnNumber :: Text -> Maybe Int
extractColumnNumber line =
  -- Look for pattern like ":123:" after line number
  case T.splitOn ":" line of
    [_file, _line, colStr] -> safeReadInt colStr
    _ -> Nothing

-- | Safe integer parsing
safeReadInt :: Text -> Maybe Int
safeReadInt text =
  case reads (T.unpack text) of
    [(n, "")] -> Just n
    _ -> Nothing
