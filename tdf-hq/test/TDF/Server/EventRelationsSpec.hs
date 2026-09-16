{-# LANGUAGE OverloadedStrings #-}
module TDF.Server.EventRelationsSpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (Single(..), rawExecute, rawSql, runSqlPool, toSqlKey)
import System.Environment (lookupEnv)
import Test.Hspec
import TDF.DB (makePool)
import TDF.Server.SocialEventsHandlers (replaceLogisticsActivityRelations)

spec :: Spec
spec = do
  database <- runIO (lookupEnv "TDF_EVENT_RELATIONS_DATABASE_URL")
  case database of
    Nothing -> pure ()
    Just url -> describe "event-relations PostgreSQL replacement" $ do
      it "preserves existing overridden edges and rejects newly acquired incomplete edges" $ do
        pool <- makePool (BS.pack url)
        now <- getCurrentTime
        flip runSqlPool pool $ do
          rawExecute "INSERT INTO event_operation_task_policy(activity_id,requires_accountability) VALUES (100,false)" []
          rawExecute "INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (100,101)" []
          rawExecute "INSERT INTO event_operation_task_override(activity_id,activity_version,override_kind,reason,policy_reference,authorized_by_party_id) VALUES (100,1,'blocked_completion','Verified emergency','event-ops-emergency-v1',1)" []
        before <- flip runSqlPool pool $ rawSql
          "SELECT id FROM event_logistics_dependency WHERE activity_id=100" []
        flip runSqlPool pool $ do
          rawExecute "UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=100 AND version=1" []
          replaceLogisticsActivityRelations (toSqlKey 100) [] [toSqlKey 101] now
        -- A relation-only replay must keep the same stored edge, too.
        runSqlPool (replaceLogisticsActivityRelations (toSqlKey 100) [] [toSqlKey 101] now) pool
        after <- flip runSqlPool pool $ rawSql
          "SELECT id FROM event_logistics_dependency WHERE activity_id=100" []
        (after :: [Single Int64]) `shouldBe` before
        denied <- try $ runSqlPool
          (replaceLogisticsActivityRelations (toSqlKey 100) [] [toSqlKey 101,toSqlKey 103] now) pool
        case (denied :: Either SomeException ()) of
          Left err -> show err `shouldContain` "23514"
          Right _ -> expectationFailure "A historical override cannot authorize a new incomplete edge"
        final <- flip runSqlPool pool $ rawSql
          "SELECT depends_on_activity_id FROM event_logistics_dependency WHERE activity_id=100" []
        (final :: [Single Int64]) `shouldBe` [Single 101]
