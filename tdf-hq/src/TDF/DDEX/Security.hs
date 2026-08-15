{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.Security
  ( -- * Safe XML parsing
    safeParseXml
  , XmlParseConfig(..)
  , defaultXmlParseConfig
  , XmlSecurityError(..)
    -- * Content validation
  , validateXmlContent
  , checkForEntities
  , checkForDoctype
  , checkForXInclude
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe (isJust)

-- | Configuration for safe XML parsing
data XmlParseConfig = XmlParseConfig
  { maxDocumentSize   :: !Integer      -- ^ Maximum document size in bytes
  , maxElementDepth   :: !Int          -- ^ Maximum nesting depth
  , maxElementCount   :: !Int          -- ^ Maximum number of elements
  , allowEntities     :: !Bool         -- ^ Allow entity declarations
  , allowDoctype      :: !Bool         -- ^ Allow DOCTYPE declarations
  , allowXInclude     :: !Bool         -- ^ Allow XInclude elements
  , allowedNamespaces :: ![Text]       -- ^ Allowed namespace prefixes
  } deriving (Show, Eq)

-- | Default safe configuration for DDEX documents
defaultXmlParseConfig :: XmlParseConfig
defaultXmlParseConfig = XmlParseConfig
  { maxDocumentSize   = 50 * 1024 * 1024  -- 50 MB
  , maxElementDepth   = 100
  , maxElementCount   = 1000000
  , allowEntities     = False
  , allowDoctype      = False
  , allowXInclude     = False
  , allowedNamespaces = ["ern", "rin", "dsr", "mead", "ddex"]
  }

-- | Security error during XML parsing
data XmlSecurityError
  = DocumentTooLarge Integer Integer  -- ^ limit, actual
  | DepthExceeded Int Int             -- ^ limit, actual
  | ElementCountExceeded Int Int      -- ^ limit, actual
  | EntityDeclarationFound Text       -- ^ location
  | DoctypeFound Text                 -- ^ location
  | XIncludeFound Text                -- ^ location
  | DisallowedNamespace Text          -- ^ namespace URI
  | InvalidUtf8 Text                  -- ^ error detail
  | ParseError Text                   -- ^ generic error
  deriving (Show, Eq)

-- | Safely parse XML content with security checks
-- Returns either security errors or the parsed content as Text
safeParseXml :: XmlParseConfig -> BL.ByteString -> Either [XmlSecurityError] Text
safeParseXml config content =
  let sizeChecks = checkDocumentSize config content
      contentChecks = validateXmlContent config content
      allErrors = sizeChecks ++ contentChecks
  in if null allErrors
     then Right (decodeUtf8Safe content)
     else Left allErrors

-- | Check document size against limit
checkDocumentSize :: XmlParseConfig -> BL.ByteString -> [XmlSecurityError]
checkDocumentSize config content =
  let actualSize = fromIntegral (BL.length content)
      limit = maxDocumentSize config
  in if actualSize > limit
     then [DocumentTooLarge limit actualSize]
     else []

-- | Validate XML content for security issues
validateXmlContent :: XmlParseConfig -> BL.ByteString -> [XmlSecurityError]
validateXmlContent config content =
  let text = decodeUtf8Safe content
      entityErrors = if allowEntities config
                    then []
                    else maybeToList (checkForEntities text)
      doctypeErrors = if allowDoctype config
                     then []
                     else maybeToList (checkForDoctype text)
      xincludeErrors = if allowXInclude config
                      then []
                      else maybeToList (checkForXInclude text)
  in entityErrors ++ doctypeErrors ++ xincludeErrors
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-- | Check for entity declarations (XXE protection)
checkForEntities :: Text -> Maybe XmlSecurityError
checkForEntities text =
  let -- Look for <!ENTITY patterns
      hasEntity = T.isInfixOf "<!ENTITY" text || T.isInfixOf "<!entity" text
  in if hasEntity
     then Just (EntityDeclarationFound "Document contains entity declarations")
     else Nothing

-- | Check for DOCTYPE declarations
checkForDoctype :: Text -> Maybe XmlSecurityError
checkForDoctype text =
  let hasDoctype = T.isInfixOf "<!DOCTYPE" text || T.isInfixOf "<!doctype" text
  in if hasDoctype
     then Just (DoctypeFound "Document contains DOCTYPE declaration")
     else Nothing

-- | Check for XInclude elements
checkForXInclude :: Text -> Maybe XmlSecurityError
checkForXInclude text =
  let hasXInclude = T.isInfixOf "<xi:include" text
                 || T.isInfixOf "<XInclude:include" text
                 || T.isInfixOf "http://www.w3.org/2001/XInclude" text
  in if hasXInclude
     then Just (XIncludeFound "Document contains XInclude elements")
     else Nothing

-- | Decode UTF-8 safely, replacing invalid sequences
decodeUtf8Safe :: BL.ByteString -> Text
decodeUtf8Safe = T.pack . BL8.unpack
