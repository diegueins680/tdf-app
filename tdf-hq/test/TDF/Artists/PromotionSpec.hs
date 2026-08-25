{-# LANGUAGE OverloadedStrings #-}

module TDF.Artists.PromotionSpec (spec) where

import qualified Data.Aeson as A
import           Data.Either              (isLeft)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Time                (fromGregorian)
import           Data.Time.Clock          (UTCTime (..), secondsToDiffTime)
import           Database.Persist         (Key, insert)
import           Database.Persist.Sql     (SqlBackend, SqlPersistT, rawExecute)
import           Database.Persist.Sqlite  (runSqlite)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Logger     (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Test.Hspec

import           TDF.Artists.Promotion    ( createArtistPromoSlotRecord
                                          , listArtistPromoSlotsForDay
                                          , loadArtistPromoDayReport
                                          , renderArtistPromoDayReportLatex
                                          )
import           TDF.DTO                  ( ArtistPromoDayReportDTO(..)
                                          , ArtistPromoSlotDTO(..)
                                          , ArtistPromoSlotUpsert(..)
                                          )
import           TDF.Models               (Party(..))

spec :: Spec
spec = do
    describe "ArtistPromoSlotUpsert FromJSON" $ do
        it "accepts canonical artist promotion payloads" $
            case A.eitherDecode
                "{\"apsuDay\":\"2026-04-23\",\"apsuStartTime\":\"09:15\",\"apsuMedium\":\"Radio Quito\",\"apsuProgram\":\"Despertar\",\"apsuInterviewerHost\":\"Ana Rivera\",\"apsuBandMembers\":\"La Ruta\",\"apsuStatus\":\"confirmado\",\"apsuNotes\":\"Llegar 15 minutos antes\"}" of
                Left err ->
                    expectationFailure ("Expected canonical artist promotion payload to decode, got: " <> err)
                Right payload -> do
                    apsuStartTime payload `shouldBe` "09:15"
                    apsuMedium payload `shouldBe` "Radio Quito"
                    apsuProgram payload `shouldBe` "Despertar"

        it "rejects unexpected artist promotion keys so typoed writes fail explicitly" $ do
            (A.eitherDecode
                "{\"apsuDay\":\"2026-04-23\",\"apsuStartTime\":\"09:15\",\"apsuMedium\":\"Radio Quito\",\"unexpected\":true}"
                    :: Either String ArtistPromoSlotUpsert)
                `shouldSatisfy` isLeft

    describe "artist promotion helpers" $ do
        it "lists promotion slots ordered by time and trims optional fields" $ do
            slots <- runInMemory $ do
                let now = mkUtc 2026 4 23
                    reportDay = fromGregorian 2026 4 23
                artistId <- insertParty "La Ruta"
                createSlot artistId now
                    ArtistPromoSlotUpsert
                        { apsuDay = reportDay
                        , apsuStartTime = " 11:30 "
                        , apsuMedium = "  TV  "
                        , apsuProgram = " Magazine PM "
                        , apsuInterviewerHost = " Host Dos "
                        , apsuBandMembers = " Trío "
                        , apsuStatus = Just " confirmado "
                        , apsuNotes = Just "   "
                        }
                createSlot artistId now
                    ArtistPromoSlotUpsert
                        { apsuDay = reportDay
                        , apsuStartTime = "08:15"
                        , apsuMedium = "Radio"
                        , apsuProgram = "Despertar"
                        , apsuInterviewerHost = "Host Uno"
                        , apsuBandMembers = "Dúo"
                        , apsuStatus = Nothing
                        , apsuNotes = Just " Llegar temprano "
                        }
                listArtistPromoSlotsForDay artistId reportDay

            map apsStartTime slots `shouldBe` ["08:15", "11:30"]
            map apsMedium slots `shouldBe` ["Radio", "TV"]
            map apsStatus slots `shouldBe` [Nothing, Just "confirmado"]
            map apsNotes slots `shouldBe` [Just "Llegar temprano", Nothing]

        it "builds a report with Ecuador header data and PDF-ready columns" $ do
            report <- runInMemory $ do
                let now = mkUtc 2026 4 23
                    reportDay = fromGregorian 2026 4 23
                artistId <- insertParty "La Ruta"
                createSlot artistId now
                    ArtistPromoSlotUpsert
                        { apsuDay = reportDay
                        , apsuStartTime = "09:00"
                        , apsuMedium = "Radio Quito"
                        , apsuProgram = "La mañana en vivo"
                        , apsuInterviewerHost = "Ana Rivera"
                        , apsuBandMembers = "La Ruta completo"
                        , apsuStatus = Just "confirmado"
                        , apsuNotes = Just "Llegar 15 minutos antes"
                        }
                mReport <- loadArtistPromoDayReport artistId reportDay
                pure (maybe (error "Expected artist promotion report to exist") id mReport)

            let latex = renderArtistPromoDayReportLatex report
            apdArtistName report `shouldBe` "La Ruta"
            apdTimezone report `shouldBe` "Hora de Ecuador (America/Guayaquil)"
            apdDayHeader report `shouldSatisfy` T.isInfixOf "abril de 2026"
            latex `shouldSatisfy` T.isInfixOf "Reporte diario de promoción"
            latex `shouldSatisfy` T.isInfixOf "Hora & Medio & Programa"
            latex `shouldSatisfy` T.isInfixOf "La mañana en vivo"

runInMemory :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runInMemory action =
    runSqlite ":memory:" $ do
        initializeArtistPromotionSchema
        action

initializeArtistPromotionSchema :: (MonadIO m) => SqlPersistT m ()
initializeArtistPromotionSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"party\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"legal_name\" VARCHAR NULL,\
        \\"display_name\" VARCHAR NOT NULL,\
        \\"is_org\" BOOLEAN NOT NULL,\
        \\"tax_id\" VARCHAR NULL,\
        \\"primary_email\" VARCHAR NULL,\
        \\"primary_phone\" VARCHAR NULL,\
        \\"whatsapp\" VARCHAR NULL,\
        \\"instagram\" VARCHAR NULL,\
        \\"emergency_contact\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"artist_promo_slot\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"artist_party_id\" INTEGER NOT NULL,\
        \\"day\" DATE NOT NULL,\
        \\"start_time\" TIME NOT NULL,\
        \\"medium\" VARCHAR NOT NULL,\
        \\"program\" VARCHAR NOT NULL,\
        \\"interviewer_host\" VARCHAR NOT NULL,\
        \\"band_members\" VARCHAR NOT NULL,\
        \\"status\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL,\
        \FOREIGN KEY(\"artist_party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE\
        \)"
        []

insertParty :: (MonadIO m) => Text -> SqlPersistT m (Key Party)
insertParty name =
    insert
        Party
            { partyLegalName = Nothing
            , partyDisplayName = name
            , partyIsOrg = False
            , partyTaxId = Nothing
            , partyPrimaryEmail = Nothing
            , partyPrimaryPhone = Nothing
            , partyWhatsapp = Nothing
            , partyInstagram = Nothing
            , partyEmergencyContact = Nothing
            , partyNotes = Nothing
            , partyCreatedAt = mkUtc 2026 4 23
            }

createSlot :: (MonadIO m) => Key Party -> UTCTime -> ArtistPromoSlotUpsert -> SqlPersistT m ()
createSlot artistId now payload =
    case createArtistPromoSlotRecord artistId payload now of
        Left err ->
            error ("Expected valid artist promo payload, got: " <> show err)
        Right action -> do
            _ <- action
            pure ()

mkUtc :: Integer -> Int -> Int -> UTCTime
mkUtc yearVal monthVal dayVal =
    UTCTime (fromGregorian yearVal monthVal dayVal) (secondsToDiffTime 0)
