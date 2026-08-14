{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.Detect
  ( -- * Detection
    detectDocument
  , detectFromRoot
  , detectFromNamespace
  , ProbeResult(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Maybe (fromMaybe)
import TDF.DDEX.Types

-- | Result of probing a document
data ProbeResult = ProbeResult
  { probeRoot      :: Text
  , probeNamespace :: Text
  , probeVersion   :: Maybe Text
  } deriving (Show, Eq)

-- | Detect DDEX document type from raw XML content
-- Returns Nothing if not a recognized DDEX document
detectDocument :: BL.ByteString -> Maybe DdexDetection
detectDocument content =
  let probe = probeXml content
  in case probe of
    Nothing -> Nothing
    Just ProbeResult{..} ->
      case (detectFromRoot probeRoot, detectFromNamespace probeNamespace) of
        (Just family, _) -> Just DdexDetection
          { detectionFamily = family
          , detectionVersion = fromMaybe "unknown" probeVersion
          , detectionNamespace = probeNamespace
          , detectionRoot = probeRoot
          , detectionConfidence = ConfidenceHigh
          }
        (_, Just family) -> Just DdexDetection
          { detectionFamily = family
          , detectionVersion = fromMaybe "unknown" probeVersion
          , detectionNamespace = probeNamespace
          , detectionRoot = probeRoot
          , detectionConfidence = ConfidenceMedium
          }
        _ -> Nothing

-- | Detect family from XML root element name
detectFromRoot :: Text -> Maybe DdexFamily
detectFromRoot root
  | "ernNewReleaseMessage" `T.isInfixOf` root = Just FamilyERN
  | "NewReleaseMessage" `T.isInfixOf` root     = Just FamilyERN
  | "rinReleaseMessage" `T.isInfixOf` root     = Just FamilyRIN
  | "RINReleaseMessage" `T.isInfixOf` root     = Just FamilyRIN
  | "digitalSalesReport" `T.isInfixOf` root    = Just FamilyDSR
  | "DSR" `T.isInfixOf` root                   = Just FamilyDSR
  | "meadMessage" `T.isInfixOf` root           = Just FamilyMEAD
  | otherwise = Nothing

-- | Detect family from XML namespace
detectFromNamespace :: Text -> Maybe DdexFamily
detectFromNamespace ns
  | "ddex.net/ERN" `T.isInfixOf` ns  = Just FamilyERN
  | "ddex.net/S/ERN" `T.isInfixOf` ns = Just FamilyERN
  | "ddex.net/RIN" `T.isInfixOf` ns  = Just FamilyRIN
  | "ddex.net/S/RIN" `T.isInfixOf` ns = Just FamilyRIN
  | "ddex.net/DSR" `T.isInfixOf` ns  = Just FamilyDSR
  | "ddex.net/S/DSR" `T.isInfixOf` ns = Just FamilyDSR
  | "ddex.net/MEAD" `T.isInfixOf` ns = Just FamilyMEAD
  | otherwise = Nothing

-- | Simple XML probe to extract root element and namespace
-- This is a lightweight probe; full parsing happens later
probeXml :: BL.ByteString -> Maybe ProbeResult
probeXml content =
  let text = decodeUtf8Lenient content
      -- Find the XML declaration or first element
      -- Look for root element pattern: <prefix:RootName xmlns:prefix="namespace"
      -- or <RootName xmlns="namespace"
  in extractRootAndNamespace text

-- | Extract root element name and default namespace from XML text
extractRootAndNamespace :: Text -> Maybe ProbeResult
extractRootAndNamespace xmlText =
  -- Skip XML declaration if present
  let afterDecl = skipXmlDeclaration xmlText
      -- Find first '<' that starts an element (not '<?' or '<!')
      firstElement = findFirstElement afterDecl
  in case firstElement of
    Nothing -> Nothing
    Just (rootName, rest) ->
      let namespace = extractNamespace rest
          version = extractVersion rest
      in Just ProbeResult
        { probeRoot = rootName
        , probeNamespace = namespace
        , probeVersion = version
        }

skipXmlDeclaration :: Text -> Text
skipXmlDeclaration text
  | T.isPrefixOf "<?xml" text =
    case T.findIndex (== '>') text of
      Just idx -> T.drop (idx + 1) text
      Nothing -> text
  | otherwise = text

findFirstElement :: Text -> Maybe (Text, Text)
findFirstElement text =
  case T.findIndex (== '<') text of
    Nothing -> Nothing
    Just idx ->
      let rest = T.drop (idx + 1) text
      in if T.isPrefixOf "?" rest || T.isPrefixOf "!" rest
         then findFirstElement (T.drop 1 rest)
         else extractElementName rest

extractElementName :: Text -> Maybe (Text, Text)
extractElementName text =
  let -- Element name ends at space, '>', or '/'
      nameEnd = T.findIndex (\c -> c == ' ' || c == '>' || c == '/' || c == '\t' || c == '\n') text
  in case nameEnd of
    Nothing -> Nothing
    Just idx ->
      let name = T.take idx text
          rest = T.drop idx text
      in if T.null name
         then Nothing
         else Just (name, rest)

extractNamespace :: Text -> Text
extractNamespace text =
  -- Look for xmlns="..." or xmlns:prefix="..."
  let patterns = ["xmlns=\"", "xmlns:ern=\"", "xmlns:rin=\"", "xmlns:dsr=\""]
      findNs [] = ""
      findNs (p:ps) =
        case T.breakOn p text of
          (_, rest) | T.isPrefixOf p rest ->
            let afterQuote = T.drop (T.length p) rest
            in case T.findIndex (== '"') afterQuote of
              Just end -> T.take end afterQuote
              Nothing -> findNs ps
          _ -> findNs ps
  in findNs patterns

extractVersion :: Text -> Maybe Text
extractVersion text =
  -- Look for version attribute or namespace version
  case T.breakOn "version=\"" text of
    (_, rest) | T.isPrefixOf "version=\"" rest ->
      let afterQuote = T.drop 9 rest
      in case T.findIndex (== '"') afterQuote of
        Just end -> Just (T.take end afterQuote)
        Nothing -> Nothing
    _ -> extractVersionFromNamespace text

extractVersionFromNamespace :: Text -> Maybe Text
extractVersionFromNamespace ns =
  -- Extract version from namespace like "http://ddex.net/xml/ern/432"
  let parts = T.splitOn "/" ns
      versionPart = case reverse parts of
        (x:_) | T.all isDigitOrDot x -> Just x
        _ -> Nothing
  in versionPart
  where
    isDigitOrDot c = c >= '0' && c <= '9' || c == '.'

-- Helper to decode UTF-8 leniently
decodeUtf8Lenient :: BL.ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With TEE.lenientDecode . BL.toStrict
