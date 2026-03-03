{-# LANGUAGE OverloadedStrings #-}

module TDF.Trials.PublicLeadSpec (spec) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (..), getJustEntity, selectList, (==.))
import Database.Persist.Sql (SqlPersistT, rawExecute, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import TDF.Trials.Server (createOrFetchParty, ensurePublicLeadParty)
import qualified TDF.Models as Models

spec :: Spec
spec = describe "Public trials lead party resolution" $ do
  it "creates/reuses signup party by email" $ do
    (firstId, secondId, storedName, storedPhone) <- runInMemory $ do
      now <- liftIO getCurrentTime
      firstId <- createOrFetchParty (Just "Test User") (Just "user@example.com") (Just "+593 99 123 4567") now
      secondId <- createOrFetchParty (Just "Another Name") (Just "user@example.com") Nothing now
      Entity _ party <- getJustEntity firstId
      pure (firstId, secondId, Models.partyDisplayName party, Models.partyPrimaryPhone party)

    firstId `shouldBe` secondId
    storedName `shouldBe` "Test User"
    storedPhone `shouldBe` Just "+593991234567"

  it "keeps a single fallback party for anonymous interests" $ do
    (firstId, secondId, total) <- runInMemory $ do
      now <- liftIO getCurrentTime
      firstId <- ensurePublicLeadParty now
      secondId <- ensurePublicLeadParty now
      rows <- selectList [Models.PartyPrimaryEmail ==. Just "public-interest@tdf.local"] []
      pure (firstId, secondId, length rows)

    firstId `shouldBe` secondId
    total `shouldBe` 1

runInMemory :: SqlPersistT IO a -> IO a
runInMemory action =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializePartySchema pool
    liftIO $ runSqlPool action pool

initializePartySchema :: (MonadIO m) => SqlPersistT m ()
initializePartySchema = do
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
