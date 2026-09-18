{-# LANGUAGE OverloadedStrings #-}
module TDF.TrialIdentitySpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, void)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool, toSqlKey)
import Database.Persist.Postgresql (createPostgresqlPool)
import Servant (ServerError, errHTTPCode, (:<|>)(..))
import System.Environment (lookupEnv)
import Test.Hspec
import TDF.Auth (AuthedUser(..), modulesForRoles)
import TDF.Models (RoleEnum(..))
import qualified TDF.Trials.API as A
import qualified TDF.Trials.DTO as D
import TDF.Trials.Server (AppM, publicTrialsServer, privateTrialsServer)

spec :: Spec
spec = describe "trial-identity-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_TRIAL_IDENTITY_TEST_DB")
  case configured of
    Nothing -> it "requires the isolated runner" $ pendingWith "Run scripts/test-trial-identity.sh"
    Just connection | not ("dbname=tdf_trial_identity_test_" `isInfixOf` connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 6)) $ do
      it "serializes signup retries without new contacts, leads, credentials, or permissions" $ \pool -> do
        before <- snapshot pool
        boxes <- forM [1..5 :: Int] $ \_ -> do
          box <- newEmptyMVar
          void $ forkIO $ do
            result <- try (runSqlPool (signup (Just "trial-signup-concurrent") enquiry) pool) :: IO (Either SomeException A.SignupOut)
            putMVar box (either (const False) (const True) result)
          pure box
        mapM takeMVar boxes `shouldReturn` replicate 5 True
        after <- snapshot pool
        zipWith (-) after before `shouldBe` [1,1,0,1,0,0]
        void $ runSqlPool (signup (Just "trial-signup-concurrent") enquiry) pool
        snapshot pool `shouldReturn` after
      it "separates distinct people sharing details and blocks changed accepted requests" $ \pool -> do
        before <- snapshot pool
        void $ runSqlPool (signup (Just "trial-signup-another-person") enquiry) pool
        after <- snapshot pool
        zipWith (-) after before `shouldBe` [1,1,0,1,0,0]
        result <- try (runSqlPool (signup (Just "trial-signup-concurrent") (A.SignupIn "Different" "Person" "trial-identity@example.test" Nothing Nothing Nothing False)) pool) :: IO (Either ServerError A.SignupOut)
        either errHTTPCode (const 0) result `shouldBe` 409
        snapshot pool `shouldReturn` after
        scalar pool "SELECT count(*) FROM lead_interest l JOIN party p ON p.id=l.party_id WHERE p.display_name='Established synthetic account'" `shouldReturn` 0
      it "rejects missing keys and unsupported credentials before saving any request" $ \pool -> do
        before <- snapshot pool
        result <- try (runSqlPool (signup Nothing enquiry) pool) :: IO (Either ServerError A.SignupOut)
        either errHTTPCode (const 0) result `shouldBe` 400
        secret <- try (runSqlPool (signup (Just "trial-unsupported-secret") (A.SignupIn "Test" "Person" "trial-identity@example.test" Nothing (Just "not-a-real-password") Nothing False)) pool) :: IO (Either ServerError A.SignupOut)
        either errHTTPCode (const 0) secret `shouldBe` 400
        snapshot pool `shouldReturn` before
      it "replays an accepted trial after subject availability changes without creating an account" $ \pool -> do
        now <- getCurrentTime
        let request = D.TrialRequestIn Nothing 900101 [D.PreferredSlot (addUTCTime 86400 now) (addUTCTime 90000 now)] Nothing (Just "Synthetic trial") (Just "trial-identity@example.test") Nothing
        before <- snapshot pool
        result <- runSqlPool (trial (Just "trial-accepted-request") request) pool
        after <- snapshot pool
        zipWith (-) after before `shouldBe` [1,0,1,1,0,0]
        runSqlPool (rawExecute "UPDATE subject SET active=false WHERE id=900101" []) pool
        replay <- runSqlPool (trial (Just "trial-accepted-request") request) pool
        show replay `shouldBe` show result
        snapshot pool `shouldReturn` after
        failure <- try (runSqlPool (trial (Just "trial-inactive-subject") request) pool) :: IO (Either ServerError D.TrialRequestOut)
        either (const True) (const False) failure `shouldBe` True
        snapshot pool `shouldReturn` after
      it "scopes student creation to authorized actors and preserves policy provenance on replay" $ \pool -> do
        let request = D.StudentCreate "Synthetic student" "trial-identity@example.test" Nothing Nothing
        before <- scalar pool "SELECT count(*) FROM party"
        a <- runSqlPool (student (actor 900102 [Admin]) (Just "student-actor-scoped-key") request) pool
        again <- runSqlPool (student (actor 900102 [Admin]) (Just "student-actor-scoped-key") request) pool
        show again `shouldBe` show a
        scalar pool "SELECT count(*) FROM party" `shouldReturn` (before+1)
        b <- runSqlPool (student (actor 900103 [Admin]) (Just "student-actor-scoped-key") request) pool
        show b `shouldNotBe` show a
        scalar pool "SELECT count(*) FROM party" `shouldReturn` (before+2)
        denied <- try (runSqlPool (student (actor 900102 []) (Just "student-actor-scoped-key") request) pool) :: IO (Either ServerError D.StudentDTO)
        either errHTTPCode (const 0) denied `shouldBe` 403
        scalar pool "SELECT count(*) FROM security_audit_event WHERE result='success' AND operation='system-policy-assigned' AND party_id IN (SELECT party_id FROM identity_trial_request WHERE request_scope LIKE 'school-student:%')" `shouldReturn` 2
      it "rolls back the contact when the dependent lead insert fails, then permits a clean retry" $ \pool -> do
        before <- snapshot pool
        runSqlPool (rawExecute "CREATE FUNCTION trial_identity_fail() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic lead failure'; END $$; CREATE TRIGGER trial_identity_fail BEFORE INSERT ON lead_interest FOR EACH ROW EXECUTE FUNCTION trial_identity_fail()" []) pool
        result <- try (runSqlPool (signup (Just "trial-partial-failure") enquiry) pool) :: IO (Either SomeException A.SignupOut)
        either (const True) (const False) result `shouldBe` True
        runSqlPool (rawExecute "DROP TRIGGER trial_identity_fail ON lead_interest; DROP FUNCTION trial_identity_fail()" []) pool
        snapshot pool `shouldReturn` before
        void $ runSqlPool (signup (Just "trial-partial-failure") enquiry) pool
        after <- snapshot pool
        zipWith (-) after before `shouldBe` [1,1,0,1,0,0]

enquiry :: A.SignupIn
enquiry = A.SignupIn "Synthetic" "Person" "trial-identity@example.test" Nothing Nothing Nothing False
signup :: Maybe Text -> A.SignupIn -> AppM A.SignupOut
signup = let handler :<|> _ = publicTrialsServer in handler
trial :: Maybe Text -> D.TrialRequestIn -> AppM D.TrialRequestOut
trial = let _ :<|> _ :<|> handler :<|> _ = publicTrialsServer in handler
student :: AuthedUser -> Maybe Text -> D.StudentCreate -> AppM D.StudentDTO
student user =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _
        :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _
        :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> handler :<|> _ = privateTrialsServer user
  in handler
actor :: Int64 -> [RoleEnum] -> AuthedUser
actor key roles = AuthedUser (toSqlKey key) roles (modulesForRoles roles) Nothing
scalar :: ConnectionPool -> Text -> IO Int64
scalar pool query = do
  rows <- runSqlPool (rawSql query []) pool
  case rows of
    [Single value] -> pure value
    _ -> fail "Missing scalar result"
snapshot :: ConnectionPool -> IO [Int64]
snapshot pool = mapM (scalar pool) ["SELECT count(*) FROM party", "SELECT count(*) FROM lead_interest", "SELECT count(*) FROM trial_request", "SELECT count(*) FROM identity_trial_request", "SELECT count(*) FROM user_credential", "SELECT count(*) FROM party_security_role"]
