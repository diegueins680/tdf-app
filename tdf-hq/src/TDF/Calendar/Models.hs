{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Calendar.Models where

import           Data.Time (UTCTime)
import           Data.Text (Text)
import           Database.Persist.TH

import           TDF.Models (PartyId)
import qualified TDF.CMS.Models as CMS

share [mkPersist sqlSettings, mkMigrate "migrateCalendar"] [persistLowerCase|
GoogleCalendarConfig
    ownerId      PartyId Maybe
    calendarId   Text
    accessToken  Text Maybe
    refreshToken Text Maybe
    tokenType    Text Maybe
    tokenExpiresAt UTCTime Maybe
    syncCursor   Text Maybe
    syncedAt     UTCTime Maybe
    createdAt    UTCTime default=CURRENT_TIMESTAMP
    updatedAt    UTCTime default=CURRENT_TIMESTAMP
    UniqueCalendar calendarId
    deriving Show

GoogleCalendarEvent
    calendarId   Text
    googleId     Text
    status       Text
    summary      Text Maybe
    description  Text Maybe
    location     Text Maybe
    startAt      UTCTime Maybe
    endAt        UTCTime Maybe
    updatedAt    UTCTime Maybe
    htmlLink     Text Maybe
    attendees    CMS.AesonValue Maybe
    rawPayload   CMS.AesonValue Maybe
    createdAt    UTCTime default=CURRENT_TIMESTAMP
    updatedLocal UTCTime default=CURRENT_TIMESTAMP
    UniqueCalendarEvent calendarId googleId
    deriving Show
|]
