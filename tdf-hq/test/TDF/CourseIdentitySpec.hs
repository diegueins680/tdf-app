{-# LANGUAGE OverloadedStrings #-}
module TDF.CourseIdentitySpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, void)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool, toSqlKey, toPersistValue)
import Database.Persist.Postgresql (createPostgresqlPool)
import Servant (Handler, ServerError, errHTTPCode, runHandler)
import System.Environment (lookupEnv)
import Test.Hspec
import TDF.Config (AppConfig(..), loadConfig)
import TDF.DB (Env(..))
import qualified TDF.ModelsExtra as ME
import qualified TDF.Routes.Courses as C
import TDF.Server (createCourseRegistrationInScope, courseEmailSentWithinLast24Hours)
import qualified TDF.Server.CourseCheckout as Checkout

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
      it "allows each shared-email registration its own confirmation while suppressing its retries" $ \pool -> do
        Right firstId <- submit pool "public-course" "course-confirmation-first" payload
        Right secondId <- submit pool "public-course" "course-confirmation-second" payload
        runSqlPool (rawExecute "INSERT INTO course_email_event(course_slug,registration_id,recipient_email,event_type,status,created_at) VALUES ('identity-test-course',?,'course-identity@example.test','registration_confirmation','sent',now())" [toPersistValue (toSqlKey firstId :: ME.CourseRegistrationId)]) pool
        cfg <- loadConfig
        let recent rid = runHandler $ runReaderT
              (courseEmailSentWithinLast24Hours (toSqlKey rid) "course-identity@example.test") (Env pool cfg)
        first <- recent firstId
        second <- recent secondId
        either (const False) id first `shouldBe` True
        either (const True) id second `shouldBe` False
        before <- scalar pool "SELECT count(*) FROM course_email_event"
        submit pool "public-course" "course-confirmation-first" payload `shouldReturn` Right firstId
        scalar pool "SELECT count(*) FROM course_email_event" `shouldReturn` before
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

      it "preserves a fallback registration when checkout becomes enabled" $ \pool -> do
        setCheckoutEnabled pool False
        a <- route pool "course-cross-mode-fallback" payload
        before <- scalar pool "SELECT count(*) FROM course_registration"
        setCheckoutEnabled pool True
        b <- route pool "course-cross-mode-fallback" payload
        fmap C.registrationId b `shouldBe` fmap C.registrationId a
        fmap C.checkoutId b `shouldBe` Right Nothing
        scalar pool "SELECT count(*) FROM course_registration" `shouldReturn` before
        scalar pool "SELECT count(*) FROM course_registration_checkout_runtime" `shouldReturn` 0
      it "preserves checkout details across disabling checkout and changing policy availability" $ \pool -> do
        setCheckoutEnabled pool True
        a <- route pool "course-cross-mode-checkout" payload
        either (const False) C.checkoutAvailable a `shouldBe` True
        before <- scalar pool "SELECT count(*) FROM course_registration"
        setCheckoutEnabled pool False
        runSqlPool (rawExecute "UPDATE course_checkout_policy SET active=false WHERE policy_version='identity-test-v1'" []) pool
        b <- route pool "course-cross-mode-checkout" payload
        fmap C.registrationId b `shouldBe` fmap C.registrationId a
        fmap C.checkoutId b `shouldBe` fmap C.checkoutId a
        fmap C.lookupToken b `shouldBe` fmap C.lookupToken a
        scalar pool "SELECT count(*) FROM course_registration" `shouldReturn` before
        changed <- route pool "course-cross-mode-checkout" (payload { C.fullName = Just "Changed" })
        either id (const 0) changed `shouldBe` 409
        runSqlPool (rawExecute "UPDATE course_checkout_policy SET active=true WHERE policy_version='identity-test-v1'" []) pool
      it "permits distinct attendees sharing an email without duplicating their retries" $ \pool -> do
        setCheckoutEnabled pool True
        a <- route pool "course-shared-email-a" payload
        b <- route pool "course-shared-email-b" payload
        either (const False) C.checkoutAvailable a `shouldBe` True
        either (const False) C.checkoutAvailable b `shouldBe` True
        fmap C.registrationId a `shouldNotBe` fmap C.registrationId b
        scalar pool "SELECT count(*) FROM commerce_payment_attempt" `shouldReturn` 0
      it "converges when a pending fallback submission overlaps checkout enablement" $ \pool -> do
        setCheckoutEnabled pool False
        entered <- newEmptyMVar
        resume <- newEmptyMVar
        finished <- newEmptyMVar
        let pausedLegacy slug key body = do
              liftIO (putMVar entered ())
              liftIO (takeMVar resume)
              createCourseRegistrationInScope "public-course" slug key body
        before <- scalar pool "SELECT count(*) FROM course_registration"
        void $ forkIO $ do
          result <- try (routeWithLegacy pool "course-concurrent-mode-switch" payload pausedLegacy) :: IO (Either SomeException (Either Int C.CourseCheckoutResponse))
          putMVar finished result
        takeMVar entered
        setCheckoutEnabled pool True
        checkout <- route pool "course-concurrent-mode-switch" payload
        putMVar resume ()
        fallback <- takeMVar finished
        case fallback of
          Left err -> expectationFailure (show err)
          Right saved -> do
            fmap C.registrationId saved `shouldBe` fmap C.registrationId checkout
            fmap C.checkoutId saved `shouldBe` fmap C.checkoutId checkout
        scalar pool "SELECT count(*) FROM course_registration" `shouldReturn` (before+1)
        scalar pool "SELECT count(*) FROM commerce_payment_attempt" `shouldReturn` 0
      it "rejects unsupported key punctuation before either creation transaction" $ \pool -> do
        before <- scalar pool "SELECT count(*) FROM course_registration"
        setCheckoutEnabled pool False
        a <- route pool "course.unsupported:key" payload
        either id (const 0) a `shouldBe` 400
        setCheckoutEnabled pool True
        b <- route pool "course.unsupported:key" payload
        either id (const 0) b `shouldBe` 400
        scalar pool "SELECT count(*) FROM course_registration" `shouldReturn` before

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

setCheckoutEnabled :: ConnectionPool -> Bool -> IO ()
setCheckoutEnabled pool enabled = runSqlPool (rawExecute
  ("UPDATE revenue_feature_flag SET enabled=" <> (if enabled then "true" else "false") <> " WHERE flag_key='commerce.courses' AND environment='production'") []) pool

route :: ConnectionPool -> Text -> C.CourseRegistrationRequest -> IO (Either Int C.CourseCheckoutResponse)
route pool key body = routeWithLegacy pool key body (createCourseRegistrationInScope "public-course")

routeWithLegacy :: ConnectionPool -> Text -> C.CourseRegistrationRequest
  -> (Text -> Maybe Text -> C.CourseRegistrationRequest -> ReaderT Env Handler C.CourseRegistrationResponse)
  -> IO (Either Int C.CourseCheckoutResponse)
routeWithLegacy pool key body legacy = do
  cfg <- loadConfig
  result <- runHandler $ runReaderT
    (Checkout.createCourseCheckoutRegistration legacy "identity-paid-course" (Just key) body)
    (Env pool (cfg { emailConfig = Nothing }))
  pure (either (Left . errHTTPCode) Right result)
