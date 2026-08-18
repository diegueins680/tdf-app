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
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Text.XML.Light (Content(..), Element(..), parseXMLDoc)

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
  case checkDocumentSize config content of
    sizeErrors@(_ : _) -> Left sizeErrors
    [] -> case TE.decodeUtf8' (BL.toStrict content) of
      Left err -> Left [InvalidUtf8 (T.pack (show err))]
      Right decoded ->
        let contentChecks = validateDecodedContent config decoded
            structureChecks = validateStructure config decoded
            allErrors = contentChecks ++ structureChecks
        in if null allErrors then Right decoded else Left allErrors

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
  case TE.decodeUtf8' (BL.toStrict content) of
    Left err -> [InvalidUtf8 (T.pack (show err))]
    Right text -> validateDecodedContent config text

validateDecodedContent :: XmlParseConfig -> Text -> [XmlSecurityError]
validateDecodedContent config text =
  let entityErrors = if allowEntities config
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

validateStructure :: XmlParseConfig -> Text -> [XmlSecurityError]
validateStructure config decoded =
  case validateTagBalance decoded of
    Left err -> [ParseError err]
    Right () ->
      case parseXMLDoc (T.unpack decoded) of
        Nothing -> [ParseError "Malformed XML document"]
        Just root ->
          let (depth, count) = elementStats root
          in [DepthExceeded (maxElementDepth config) depth | depth > maxElementDepth config]
              ++ [ElementCountExceeded (maxElementCount config) count | count > maxElementCount config]

-- xml-light intentionally recovers from some malformed input (for example an
-- unclosed root element). DDEX intake must fail closed, so perform a small,
-- conservative tag-stack pass before asking xml-light to build the tree. This
-- scanner does not expand entities or resolve any external resource.
validateTagBalance :: Text -> Either Text ()
validateTagBalance = go [] 0
  where
    go :: [Text] -> Int -> Text -> Either Text ()
    go stack roots remaining =
      case T.breakOn "<" remaining of
        (plain, rest)
          | T.null rest ->
              if T.all isXmlSpace plain && null stack && roots == 1
                then Right ()
                else Left "Malformed XML document"
          | not (null stack) || T.all isXmlSpace plain -> consumeTag stack roots rest
          | otherwise -> Left "Text is not allowed outside the root element"

    consumeTag stack roots input
      | "<!--" `T.isPrefixOf` input = skipDelimited "-->" 4 stack roots input
      | "<?" `T.isPrefixOf` input = skipDelimited "?>" 2 stack roots input
      | "<![CDATA[" `T.isPrefixOf` input =
          if null stack
            then Left "CDATA is not allowed outside the root element"
            else skipDelimited "]]>" 9 stack roots input
      | "</" `T.isPrefixOf` input = do
          (inside, after) <- takeTag 2 input
          let name = T.strip inside
          if not (validXmlName name)
            then Left "Malformed closing tag"
            else case stack of
              expected : parent
                | expected == name -> go parent roots after
                | otherwise -> Left "Mismatched closing tag"
              [] -> Left "Closing tag has no matching start tag"
      | "<!" `T.isPrefixOf` input = Left "Unsupported XML declaration"
      | otherwise = do
          (inside, after) <- takeTag 1 input
          let trimmed = T.strip inside
              selfClosing = "/" `T.isSuffixOf` trimmed
              body = T.strip (if selfClosing then T.dropEnd 1 trimmed else trimmed)
              name = T.takeWhile (not . isXmlSpace) body
              topLevel = null stack
              nextRoots = if topLevel then roots + 1 else roots
          if roots > 0 && topLevel
            then Left "XML document contains more than one root element"
            else if not (validXmlName name)
              then Left "Malformed opening tag"
              else if selfClosing
                then go stack nextRoots after
                else go (name : stack) nextRoots after

    skipDelimited delimiter prefixLength stack roots input =
      let afterPrefix = T.drop prefixLength input
          (_, suffix) = T.breakOn delimiter afterPrefix
      in if T.null suffix
          then Left "Unterminated XML construct"
          else go stack roots (T.drop (T.length delimiter) suffix)

    takeTag prefixLength input =
      scan Nothing T.empty (T.drop prefixLength input)
      where
        scan _ _ text | T.null text = Left "Unterminated XML tag"
        scan quote acc text =
          case T.uncons text of
            Nothing -> Left "Unterminated XML tag"
            Just (character, rest)
              | Just character == quote -> scan Nothing (T.snoc acc character) rest
              | quote == Nothing && (character == '\'' || character == '"') ->
                  scan (Just character) (T.snoc acc character) rest
              | quote == Nothing && character == '>' -> Right (acc, rest)
              | otherwise -> scan quote (T.snoc acc character) rest

    validXmlName name =
      not (T.null name)
        && T.all validNameCharacter name
        && not (T.any isXmlSpace name)
        && T.head name /= '/'

    validNameCharacter character =
      character == ':' || character == '_' || character == '-' || character == '.'
        || character >= '0' && character <= '9'
        || character >= 'A' && character <= 'Z'
        || character >= 'a' && character <= 'z'

    isXmlSpace character =
      character == ' ' || character == '\t' || character == '\r' || character == '\n' || character == '\xfeff'

elementStats :: Element -> (Int, Int)
elementStats element =
  let children = [child | Elem child <- elContent element]
      childStats = map elementStats children
      depth = 1 + maximum (0 : map fst childStats)
      count = 1 + sum (map snd childStats)
  in (depth, count)
