{-# LANGUAGE OverloadedStrings #-}
module TDF.CourseIdentitySpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, void)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Servant (ServerError, errHTTPCode, runHandler)
import System.Environment (lookupEnv)
import Test.Hspec
import TDF.Config (AppConfig(..), loadConfig)
import TDF.DB (Env(..))
import qualified TDF.Routes.Courses as C
import TDF.Server (createCourseRegistrationInScope)

spec :: Spec
spec = describe "course-identity-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_COURSE_IDENTITY_TEST_DB")
  case configured of
    Nothing -> it "requires the isolated runner" $ pendingWith "Run scripts/test-course-identity.sh"
    Just connection | not ("dbname=tdf_course_identity_test_" `isInfixOf` connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 6)) $ do
      it "keeps shared email and phone separate across legitimate source operations" $ \pool -> do
        a <- submit pool "public-course" "course-distinct-request-a" payload
        b <- submit pool "public-course" "course-distinct-request-b" payload
        either (const False) (> 0) a `shouldBe` True
        a `shouldNotBe` b
        scalar pool "SELECT count(DISTINCT party_id) FROM course_registration WHERE course_slug='identity-test-course'" `shouldReturn` 2
        scalar pool "SELECT count(*) FROM course_registration r JOIN party p ON p.id=r.party_id WHERE p.display_name='Established synthetic account'" `shouldReturn` 0
        scalar pool "SELECT count(*) FROM user_credential c JOIN course_registration r ON r.party_id=c.party_id WHERE r.course_slug='identity-test-course'" `shouldReturn` 0
        scalar pool "SELECT count(*) FROM party_security_role g JOIN course_registration r ON r.party_id=g.party_id WHERE r.course_slug='identity-test-course'" `shouldReturn` 0
      it "serializes retries without repeating contacts, follow-ups, or notifications" $ \pool -> do
        before <- snapshot pool
        boxes <- forM [1..5 :: Int] $ \_ -> do
          box <- newEmptyMVar
          void $ forkIO $ do
            result <- try (submit pool "public-course" "course-concurrent-request" payload) :: IO (Either SomeException (Either Int Int64))
            putMVar box result
          pure box
        outcomes <- mapM takeMVar boxes
        let ids = [rid | Right (Right rid) <- outcomes]
        length ids `shouldBe` 5
        map (== head ids) ids `shouldBe` replicate 5 True
        after <- snapshot pool
        zipWith (-) after before `shouldBe` [1,1,1,1,1]
        submit pool "public-course" "course-concurrent-request" payload `shouldReturn` Right (head ids)
        snapshot pool `shouldReturn` after
      it "blocks changed accepted payloads without adding records" $ \pool -> do
        before <- snapshot pool
        submit pool "public-course" "course-concurrent-request" (payload { C.fullName = Just "Changed" }) `shouldReturn` Left 409
        snapshot pool `shouldReturn` before
      it "separates public and verified integration operation namespaces" $ \pool -> do
        a <- submit pool "public-course" "course-same-namespace-key" payload
        b <- submit pool "whatsapp-course" "course-same-namespace-key" payload
        a `shouldNotBe` b
      it "rolls back contacts and registrations when a dependent insert fails" $ \pool -> do
        before <- snapshot pool
        runSqlPool (rawExecute "CREATE FUNCTION course_identity_fail() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic dependency failure'; END $$; CREATE TRIGGER course_identity_fail BEFORE INSERT ON course_registration_follow_up FOR EACH ROW EXECUTE FUNCTION course_identity_fail()" []) pool
        result <- try (submit pool "public-course" "course-failing-request" payload) :: IO (Either SomeException (Either Int Int64))
        either (const True) isLeft result `shouldBe` True
        runSqlPool (rawExecute "DROP TRIGGER course_identity_fail ON course_registration_follow_up; DROP FUNCTION course_identity_fail()" []) pool
        snapshot pool `shouldReturn` before
        result2 <- submit pool "public-course" "course-failing-request" payload
        either (const False) (> 0) result2 `shouldBe` True

payload :: C.CourseRegistrationRequest
payload = C.CourseRegistrationRequest (Just "Synthetic course contact") (Just "course-identity@example.test") (Just "+593990000111") "landing" Nothing Nothing (Just True)

submit :: ConnectionPool -> Text -> Text -> C.CourseRegistrationRequest -> IO (Either Int Int64)
submit pool scope key body = do
  cfg <- loadConfig
  result <- runHandler $ runReaderT (createCourseRegistrationInScope scope "identity-test-course" (Just key) body) (Env pool (cfg { emailConfig = Nothing }))
  pure $ either (Left . errHTTPCode) (Right . C.id) (result :: Either ServerError C.CourseRegistrationResponse)

scalar :: ConnectionPool -> Text -> IO Int64
scalar pool query = do
  rows <- runSqlPool (rawSql query []) pool
  case rows of
    [Single value] -> pure value
    _ -> fail "Missing scalar result"

snapshot :: ConnectionPool -> IO [Int64]
snapshot pool = mapM (scalar pool)
  [ "SELECT count(*) FROM party WHERE primary_email='course-identity@example.test'"
  , "SELECT count(*) FROM course_registration WHERE course_slug='identity-test-course'"
  , "SELECT count(*) FROM identity_course_registration_request"
  , "SELECT count(*) FROM course_registration_follow_up f JOIN course_registration r ON r.id=f.registration_id WHERE r.course_slug='identity-test-course'"
  , "SELECT count(*) FROM course_email_event WHERE course_slug='identity-test-course'"
  ]
