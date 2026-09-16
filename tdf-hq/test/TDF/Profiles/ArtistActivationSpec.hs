{-# LANGUAGE OverloadedStrings #-}

module TDF.Profiles.ArtistActivationSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, void)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool, toSqlKey)
import System.Environment (lookupEnv)
import Test.Hspec
import TDF.DTO (ArtistProfileDTO(..))
import TDF.Profiles.Artist (activateOwnArtistProfile)

spec :: Spec
spec = describe "artist-self-service-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_ARTIST_SELF_SERVICE_TEST_DB")
  case configured of
    Nothing -> it "requires the isolated PostgreSQL integration runner" $
      pendingWith "Run scripts/test-artist-self-service.sh"
    Just connection | not ("dbname=tdf_artist_self_service_test_" `isInfixOf` connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 4)) $ do
      it "activates only Artist, creates an owned profile and completes an old pending request" $ \pool -> do
        runSqlPool (rawExecute "INSERT INTO feature_access_requests(requester_party_id,feature_id,action,role_context,module_context,status,reviewer_group,requested_at,updated_at) VALUES(1,'artist.onboarding','create','[]','[]','pending','admin',now(),now())" []) pool
        result <- activate pool 1
        fmap apArtistId result `shouldBe` Right 1
        counts pool 1 `shouldReturn` [1,1,1]
        status <- runSqlPool (rawSql "SELECT status FROM feature_access_requests WHERE requester_party_id=1" []) pool
        status `shouldBe` [Single ("approved" :: Text)]
        roles <- runSqlPool (rawSql "SELECT r.code FROM party_security_role p JOIN security_role r ON r.id=p.role_id WHERE p.party_id=1 AND p.active" []) pool
        roles `shouldBe` [Single ("artist" :: Text)]
      it "preserves existing content and does not duplicate profiles, grants or audit on retry" $ \pool -> do
        runSqlPool (rawExecute "UPDATE artist_profile SET bio='Keep my biography' WHERE artist_party_id=1" []) pool
        result <- activate pool 1
        fmap apBio result `shouldBe` Right (Just "Keep my biography")
        counts pool 1 `shouldReturn` [1,1,1]
      it "serializes simultaneous activations of the same account" $ \pool -> do
        boxes <- forM [1..4 :: Int] $ \_ -> do
          box <- newEmptyMVar
          void $ forkIO $ do
            result <- try (activate pool 2) :: IO (Either SomeException (Either Text ArtistProfileDTO))
            putMVar box result
          pure box
        results <- mapM takeMVar boxes
        map (either (const False) isRight) results `shouldBe` replicate 4 True
        counts pool 2 `shouldReturn` [1,1,1]
      it "does not reactivate a revoked artist assignment" $ \pool -> do
        runSqlPool (rawExecute "INSERT INTO party_security_role(party_id,role_id,active,revoked_at) SELECT 3,id,false,now() FROM security_role WHERE code='artist'" []) pool
        activate pool 3 >>= (`shouldSatisfy` isLeft)
        counts pool 3 `shouldReturn` [0,0,0]
      it "fails closed when the automatic policy is disabled" $ \pool -> do
        runSqlPool (rawExecute "UPDATE security_role_assignment_policy SET active=false WHERE code='artist.self-service.artist'" []) pool
        result <- activate pool 4
        runSqlPool (rawExecute "UPDATE security_role_assignment_policy SET active=true WHERE code='artist.self-service.artist'" []) pool
        result `shouldSatisfy` isLeft
        counts pool 4 `shouldReturn` [0,0,0]
      it "rejects a policy outside its effective period" $ \pool -> do
        runSqlPool (rawExecute "UPDATE security_role_assignment_policy SET effective_from=now()+interval '1 day' WHERE code='artist.self-service.artist'" []) pool
        result <- activate pool 6
        runSqlPool (rawExecute "UPDATE security_role_assignment_policy SET effective_from=NULL WHERE code='artist.self-service.artist'" []) pool
        result `shouldSatisfy` isLeft
        counts pool 6 `shouldReturn` [0,0,0]
      it "rejects inactive accounts and parties without credentials" $ \pool -> do
        activate pool 5 >>= (`shouldSatisfy` isLeft)
        activate pool 8 >>= (`shouldSatisfy` isLeft)
        counts pool 5 `shouldReturn` [0,0,0]
        counts pool 8 `shouldReturn` [0,0,0]
      it "preserves an administrator's strict role scope" $ \pool -> do
        activate pool 9 >>= (`shouldSatisfy` isRight)
        counts pool 9 `shouldReturn` [1,1,0]
        roles <- runSqlPool (rawSql "SELECT r.code FROM party_security_role p JOIN security_role r ON r.id=p.role_id WHERE p.party_id=9 AND p.active" []) pool
        roles `shouldBe` [Single ("admin" :: Text)]
      it "rolls back the role and audit if profile creation fails" $ \pool -> do
        runSqlPool (rawExecute "CREATE FUNCTION fail_test_profile() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.artist_party_id=7 THEN RAISE EXCEPTION 'injected profile write failure'; END IF; RETURN NEW; END $$" []) pool
        runSqlPool (rawExecute "CREATE TRIGGER fail_profile BEFORE INSERT ON artist_profile FOR EACH ROW EXECUTE FUNCTION fail_test_profile()" []) pool
        result <- try (activate pool 7) :: IO (Either SomeException (Either Text ArtistProfileDTO))
        result `shouldSatisfy` isLeft
        counts pool 7 `shouldReturn` [0,0,0]

activate :: ConnectionPool -> Int64 -> IO (Either Text ArtistProfileDTO)
activate pool party = do
  now <- getCurrentTime
  runSqlPool (activateOwnArtistProfile (toSqlKey party) now) pool

counts :: ConnectionPool -> Int64 -> IO [Int64]
counts pool party = do
  rows <- runSqlPool (rawSql "SELECT (SELECT count(*) FROM party_security_role WHERE party_id=? AND active), (SELECT count(*) FROM artist_profile WHERE artist_party_id=?), (SELECT count(*) FROM security_audit_event WHERE party_id=?)" (replicate 3 (PersistInt64 party))) pool
  case rows of
    [(Single grants, Single profiles, Single audits)] -> pure [grants,profiles,audits]
    _ -> fail "Expected a single verification row"
