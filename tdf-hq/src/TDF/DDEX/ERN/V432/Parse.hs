{-# LANGUAGE OverloadedStrings #-}

module TDF.DDEX.ERN.V432.Parse
  ( -- * Parsing
    parseErnMessage
  , ParseError(..)
  ) where

import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import TDF.DDEX.ERN.V432.Types

-- | Parse error with location information
data ParseError = ParseError
  { peMessage :: Text
  , peElement :: Maybe Text
  , peLine    :: Maybe Int
  } deriving (Show, Eq)

-- | Parse a complete ERN 4.3.2 message from XML
-- TODO: Implement full XML parsing with xml-conduit or xml-light
-- For now, returns a stub error indicating parsing is not yet implemented
parseErnMessage :: BL.ByteString -> Either [ParseError] ErnMessage
parseErnMessage _content =
  Left [ParseError "ERN 4.3.2 parser not yet implemented" Nothing Nothing]

-- Full implementation would:
-- 1. Parse XML document
-- 2. Extract MessageHeader
-- 3. Extract PartyList
-- 4. Extract ResourceList (SoundRecording, MusicVideo, Image)
-- 5. Extract ReleaseList
-- 6. Extract ResourceGroup (tracklist ordering)
-- 7. Extract DealList
-- 8. Handle namespaces correctly (don't rely on prefix)
-- 9. Resolve internal references
