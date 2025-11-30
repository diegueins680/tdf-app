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
import Data.Aeson (Value, encode, eitherDecodeStrict')
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Persist
import Database.Persist.Sql (PersistFieldSql(..), SqlType(..))
import TDF.Models (PartyId)

newtype AesonValue = AesonValue { unAesonValue :: Value } deriving stock (Show, Eq)

instance PersistField AesonValue where
  toPersistValue (AesonValue v) = PersistText . decodeUtf8 . BL.toStrict $ encode v
  fromPersistValue (PersistText t) =
    case eitherDecodeStrict' (encodeUtf8 t) of
      Left err -> Left (T.pack err)
      Right val -> Right (AesonValue val)
  fromPersistValue (PersistByteString bs) =
    case eitherDecodeStrict' bs of
      Left err -> Left (T.pack err)
      Right val -> Right (AesonValue val)
  fromPersistValue other = Left $ "Expected JSON text, got: " <> T.pack (show other)

instance PersistFieldSql AesonValue where
  sqlType _ = SqlString

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
