{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.CMS.Models where

import Database.Persist.TH
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Aeson (Value(..), encode, eitherDecodeStrict')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Database.Persist
import Database.Persist.Sql (PersistFieldSql(..), SqlType(..))
import TDF.Models (PartyId)

newtype AesonValue = AesonValue { unAesonValue :: Value } deriving stock (Show, Eq)

instance PersistField AesonValue where
  toPersistValue (AesonValue v) = PersistText . decodeUtf8 . BL.toStrict $ encode v
  fromPersistValue (PersistText t) = AesonValue <$> decodeValue (encodeUtf8 t)
  fromPersistValue (PersistByteString bs) = AesonValue <$> decodeValue bs
  fromPersistValue other = Left $ "Expected JSON text, got: " <> T.pack (show other)

instance PersistFieldSql AesonValue where
  sqlType _ = SqlString

-- | Decode JSON stored as text/bytea, but gracefully fall back to wrapping
-- non-JSON payloads as a JSON string so legacy rows don't crash the API.
decodeValue :: BS.ByteString -> Either T.Text Value
decodeValue bs =
  case eitherDecodeStrict' bs of
    Right val -> Right val
    Left _ ->
      let fallback = decodeUtf8With lenientDecode bs
      in Right (String fallback)

share [mkPersist sqlSettings, mkMigrate "migrateCMS"] [persistLowerCase|
CmsContent
    slug        Text
    locale      Text
    version     Int
    status      Text          -- draft | published | archived
    title       Text Maybe
    payload     AesonValue Maybe
    createdBy   PartyId Maybe
    createdAt   UTCTime default=CURRENT_TIMESTAMP
    updatedAt   UTCTime default=CURRENT_TIMESTAMP
    publishedAt UTCTime Maybe
    UniqueCmsVersion slug locale version
    deriving Show
|]
