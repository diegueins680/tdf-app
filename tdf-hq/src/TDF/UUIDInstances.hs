{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.UUIDInstances where

import           Data.Text                 (pack)
import qualified Data.UUID                 as UUID
import           Data.UUID                 (UUID)
import           Database.Persist          (PersistField(..), PersistValue(..))
import           Database.Persist.Sql      (PersistFieldSql(..), SqlType(..))
import           Web.PathPieces            (PathPiece(..))

instance PathPiece UUID where
  toPathPiece   = UUID.toText
  fromPathPiece = UUID.fromText

instance PersistField UUID where
  toPersistValue = PersistLiteralEscaped . UUID.toASCIIBytes
  fromPersistValue value =
    case value of
      PersistText t       -> noteText (UUID.fromText t)
      PersistByteString b -> noteBytes (UUID.fromASCIIBytes b)
      PersistLiteral_ _ b -> noteBytes (UUID.fromASCIIBytes b)
      PersistNull         -> Left "Unexpected NULL for UUID column"
      other               -> Left ("Unable to parse UUID from " <> pack (show other))
    where
      noteText = maybe (Left "Failed to parse UUID from text value") Right
      noteBytes = maybe (Left "Failed to parse UUID from raw bytes") Right

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"
