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

module TDF.Trials.Models where

import Database.Persist.TH
import Data.Time (UTCTime, Day)
import Data.Text (Text)

import TDF.Models (BookingId, PartyId, ResourceId)

-- NOTE: We keep this migration separate to avoid touching your main Models block.
-- Call 'runMigration migrateTrials' at startup.

share [mkPersist sqlSettings, mkMigrate "migrateTrials"] [persistLowerCase|
Subject
    name                 Text
    active               Bool              default=True
    UniqueSubjectName    name
    deriving Show Eq
TeacherSubject
    teacherId            PartyId
    subjectId            SubjectId
    levelMin             Int Maybe
    levelMax             Int Maybe
    UniqueTeacherSubject teacherId subjectId
    deriving Show Eq
SubjectRoomPreference
    subjectId            SubjectId
    roomId               ResourceId
    priority             Int              default=1
    UniqueSubjectRoom    subjectId roomId
    deriving Show Eq

TeacherAvailability
    teacherId            PartyId
    subjectId            SubjectId
    roomId               ResourceId
    startAt              UTCTime
    endAt                UTCTime
    notes                Text Maybe
    createdAt            UTCTime          default=CURRENT_TIMESTAMP
    deriving Show Eq

LeadInterest
    partyId              PartyId
    interestType         Text
    subjectId            SubjectId Maybe
    details              Text Maybe
    source               Text
    driveLink            Text Maybe
    status               Text             default='Open'
    createdAt            UTCTime          default=CURRENT_TIMESTAMP
    deriving Show

TrialRequest
    partyId              PartyId
    subjectId            SubjectId
    pref1Start           UTCTime
    pref1End             UTCTime
    pref2Start           UTCTime Maybe
    pref2End             UTCTime Maybe
    pref3Start           UTCTime Maybe
    pref3End             UTCTime Maybe
    notes                Text Maybe
    status               Text
    assignedTeacherId    PartyId Maybe
    assignedAt           UTCTime Maybe
    createdAt            UTCTime          default=CURRENT_TIMESTAMP
    deriving Show

TrialAssignment
    requestId            TrialRequestId
    teacherId            PartyId
    startAt              UTCTime
    endAt                UTCTime
    roomId               ResourceId
    bookingId            BookingId Maybe
    createdAt            UTCTime          default=CURRENT_TIMESTAMP
    UniqueTrialAssignmentRequest requestId
    deriving Show

PackageCatalog
    subjectId            SubjectId
    name                 Text
    hoursQty             Int
    priceCents           Int
    expiresDays          Int
    refundPolicy         Text
    active               Bool             default=True
    UniquePackagePerSubject subjectId name
    deriving Show

ClassPackagePurchase
    studentId            PartyId
    packageId            PackageCatalogId
    priceCents           Int
    discountCents        Int              default=0
    taxCents             Int              default=0
    totalPaidCents       Int              default=0
    purchasedAt          UTCTime          default=CURRENT_TIMESTAMP
    sellerId             PartyId Maybe
    commissionedTeacherId PartyId Maybe
    trialRequestId       TrialRequestId Maybe
    status               Text             default='Open'
    deriving Show

ClassSession
    studentId            PartyId
    teacherId            PartyId
    subjectId            SubjectId
    startAt              UTCTime
    endAt                UTCTime
    roomId               ResourceId
    bookingId            BookingId Maybe
    attended             Bool             default=False
    purchaseId           ClassPackagePurchaseId Maybe
    consumedMinutes      Int              default=0
    notes                Text Maybe
    deriving Show

Commission
    teacherId            PartyId
    purchaseId           ClassPackagePurchaseId
    basisCents           Int
    percent              Double
    amountCents          Int
    recognizedAt         UTCTime
    status               Text             default='Accrued'
    paidAt               UTCTime Maybe
    deriving Show

TrialThrottle
    partyId              PartyId
    day                  Day
    count                Int
    UniqueTrialThrottle  partyId day
    deriving Show

Course
    slug                 Text
    title                Text
    subtitle             Text Maybe
    format               Text Maybe
    duration             Text Maybe
    priceCents           Int
    currency             Text
    capacity             Int
    sessionStartHour     Int Maybe
    sessionDurationHours Int Maybe
    locationLabel        Text Maybe
    locationMapUrl       Text Maybe
    whatsappCtaUrl       Text Maybe
    landingUrl           Text Maybe
    daws                 [Text] Maybe sqltype=text[]
    includes             [Text] Maybe sqltype=text[]
    createdAt            UTCTime default=CURRENT_TIMESTAMP
    updatedAt            UTCTime default=CURRENT_TIMESTAMP
    UniqueCourseSlug     slug
    deriving Show

CourseSessionModel
    courseId             CourseId
    label                Text
    date                 Day
    order                Int Maybe
    deriving Show

CourseSyllabusItem
    courseId             CourseId
    title                Text
    topics               [Text]
    order                Int Maybe
    deriving Show Eq
|]
