{-# LANGUAGE OverloadedStrings #-}

module TDF.EventResearch.Identity (
    normalizeResearchEntityText,
    researchEntityExternalId,
) where

import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

normalizeResearchEntityText :: Text -> Text
normalizeResearchEntityText =
    T.unwords
        . T.words
        . T.map (\ch -> if isAlphaNum ch then ch else ' ')
        . T.toCaseFold
        . T.strip

researchEntityExternalId :: Text -> [Text] -> Text
researchEntityExternalId entityType parts =
    "event-research:"
        <> entityType
        <> ":"
        <> T.take
            40
            ( sha256Text
                (BL.fromStrict (TE.encodeUtf8 (T.intercalate "|" (map normalizeResearchEntityText parts))))
            )

sha256Text :: BL.ByteString -> Text
sha256Text bytes =
    TE.decodeUtf8 (BAE.convertToBase BAE.Base16 (hash (BL.toStrict bytes) :: Digest SHA256))
