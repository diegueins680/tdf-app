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
import Data.Aeson (Value)
import Database.Persist.TH (derivePersistFieldJSON)
import TDF.Models (PartyId)

newtype AesonValue = AesonValue { unAesonValue :: Value }
derivePersistFieldJSON "AesonValue"

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
