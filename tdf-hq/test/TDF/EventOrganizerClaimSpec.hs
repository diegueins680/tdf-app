{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.EventOrganizerClaimSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, forM_)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistValue(..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), fromSqlKey, rawExecute, rawSql, runSqlPool, toSqlKey)
import Servant (Handler, ServerError(errHTTPCode), (:<|>)(..))
import Servant.Server.Internal.Handler (runHandler)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Hspec

import TDF.Auth (AuthedUser(..), modulesForRoles)
import TDF.Config (loadConfig)
import TDF.DB (Env(..))
import TDF.DTO.SocialEventsDTO
  ( EventOrganizerClaimCreateDTO(..), EventOrganizerClaimDTO(..), EventOrganizerClaimDecisionDTO(..)
  , EventOrganizerClaimReviewDTO(..), EventOrganizerClaimStateDTO(..) )
import TDF.Models (RoleEnum(..))
import TDF.Server.SocialEventsHandlers (socialEventsServer)

-- EVT-ORGANIZER-CLAIM-001: an event without an organizer is claimed with
-- evidence and assigned only by the approval of a strict administrator other
-- than the claimant. Runs against PostgreSQL because the single assignment
-- and the ordering of decisions rest on the event row lock.
spec :: Spec
spec = describe "event-organizer-claim-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_EVENT_ORGANIZER_CLAIM_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-event-organizer-claim.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $
        expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 10)) $
      afterAll (\pool -> removeFixtures pool >> destroyAllResources pool) $ do
        it "records a claim only on an event without an organizer and grants nothing" $ \pool -> do
          claimant <- newAccount pool
          organized <- newEvent pool (Just "7499")
          taken <- run pool (submit claimant organized evidence)
          status taken `shouldBe` Just 409
          closed <- expectRight =<< run pool (claimState claimant organized)
          (eocsClaimable closed, eocsClaim closed) `shouldBe` (False, Nothing)
          eventId <- newEvent pool Nothing
          before <- expectRight =<< run pool (claimState claimant eventId)
          (eocsClaimable before, eocsClaim before) `shouldBe` (True, Nothing)
          short <- run pool (submit claimant eventId "Es mi evento")
          status short `shouldBe` Just 400
          claim <- expectRight =<< run pool (submit claimant eventId ("  " <> evidence <> "  "))
          (eocEventId claim, eocStatus claim, eocReviewedAt claim)
            `shouldBe` (textId eventId, "submitted", Nothing)
          organizerOf pool eventId `shouldReturn` Nothing
          again <- expectRight =<< run pool (submit claimant eventId "Otra descripción de la misma solicitud.")
          eocId again `shouldBe` eocId claim
          stored <- runSqlPool (rawSql
            "SELECT evidence FROM event_organizer_claim WHERE event_id = ?" [PersistInt64 eventId]) pool
          stored `shouldBe` [Single evidence]
          after <- expectRight =<< run pool (claimState claimant eventId)
          fmap eocId (eocsClaim after) `shouldBe` Just (eocId claim)
          other <- newAccount pool
          unrelated <- expectRight =<< run pool (claimState other eventId)
          eocsClaim unrelated `shouldBe` Nothing

        it "lets only a strict administrator other than the claimant decide" $ \pool -> do
          claimant <- newAccount pool
          eventId <- newEvent pool Nothing
          claim <- expectRight =<< run pool (submit claimant eventId evidence)
          listed <- run pool (queue claimant)
          status listed `shouldBe` Just 403
          byClaimant <- run pool (decide claimant (eocId claim) "approved" notes)
          status byClaimant `shouldBe` Just 403
          selfReview <- run pool (decide (admin (partyOf claimant)) (eocId claim) "approved" notes)
          status selfReview `shouldBe` Just 403
          unknownDecision <- run pool (decide reviewer (eocId claim) "revoked" notes)
          status unknownDecision `shouldBe` Just 400
          withoutNotes <- run pool (decide reviewer (eocId claim) "approved" "ok")
          status withoutNotes `shouldBe` Just 400
          missing <- run pool (decide reviewer "999999999999" "approved" notes)
          status missing `shouldBe` Just 404
          organizerOf pool eventId `shouldReturn` Nothing
          pending <- expectRight =<< run pool (queue reviewer)
          [ (eocrStatus row, eocrClaimantPartyId row, eocrEvidence row, eocrEventHasOrganizer row)
            | row <- pending, eocrId row == eocId claim ]
            `shouldBe` [("submitted", partyText claimant, evidence, False)]

        it "assigns the claimant on approval and closes the other open claims" $ \pool -> do
          first <- newAccount pool
          second <- newAccount pool
          eventId <- newEvent pool Nothing
          winner <- expectRight =<< run pool (submit first eventId evidence)
          loser <- expectRight =<< run pool (submit second eventId evidence)
          approved <- expectRight =<< run pool (decide reviewer (eocId winner) "approved" notes)
          (eocrStatus approved, eocrReviewerPartyId approved, eocrReviewerNotes approved, eocrEventHasOrganizer approved)
            `shouldBe` ("approved", Just (partyText reviewer), Just notes, True)
          organizerOf pool eventId `shouldReturn` Just (partyText first)
          statusOf pool (eocId loser) `shouldReturn` "superseded"
          replay <- expectRight =<< run pool (decide otherReviewer (eocId winner) "approved" "Segunda lectura de la misma decisión.")
          (eocrReviewerPartyId replay, eocrReviewerNotes replay)
            `shouldBe` (Just (partyText reviewer), Just notes)
          reversed <- run pool (decide reviewer (eocId winner) "rejected" notes)
          status reversed `shouldBe` Just 409
          late <- run pool (decide reviewer (eocId loser) "approved" notes)
          status late `shouldBe` Just 409
          third <- newAccount pool
          afterwards <- run pool (submit third eventId evidence)
          status afterwards `shouldBe` Just 409
          organizerOf pool eventId `shouldReturn` Just (partyText first)

        it "leaves the event without an organizer on rejection and admits a new claim" $ \pool -> do
          claimant <- newAccount pool
          eventId <- newEvent pool Nothing
          claim <- expectRight =<< run pool (submit claimant eventId evidence)
          rejected <- expectRight =<< run pool (decide reviewer (eocId claim) "rejected" notes)
          eocrStatus rejected `shouldBe` "rejected"
          organizerOf pool eventId `shouldReturn` Nothing
          state <- expectRight =<< run pool (claimState claimant eventId)
          (eocsClaimable state, fmap eocStatus (eocsClaim state)) `shouldBe` (True, Just "rejected")
          renewed <- expectRight =<< run pool (submit claimant eventId evidence)
          eocId renewed `shouldNotBe` eocId claim
          eocStatus renewed `shouldBe` "submitted"

        it "refuses to approve once the event has an organizer or the claimant is gone" $ \pool -> do
          claimant <- newAccount pool
          eventId <- newEvent pool Nothing
          claim <- expectRight =<< run pool (submit claimant eventId evidence)
          runSqlPool (rawExecute "UPDATE social_event SET organizer_party_id = '7499' WHERE id = ?"
            [PersistInt64 eventId]) pool
          assigned <- run pool (decide reviewer (eocId claim) "approved" notes)
          status assigned `shouldBe` Just 409
          statusOf pool (eocId claim) `shouldReturn` "submitted"
          organizerOf pool eventId `shouldReturn` Just "7499"
          orphanEvent <- newEvent pool Nothing
          orphan <- expectRight =<< run pool (submit (user 749800000001) orphanEvent evidence)
          gone <- run pool (decide reviewer (eocId orphan) "approved" notes)
          status gone `shouldBe` Just 409
          organizerOf pool orphanEvent `shouldReturn` Nothing

        it "admits exactly one of several simultaneous approvals of different claims" $ \pool -> do
          eventId <- newEvent pool Nothing
          claimants <- mapM (const (newAccount pool)) [1 .. 6 :: Int]
          claims <- forM claimants $ \claimant ->
            expectRight =<< run pool (submit claimant eventId evidence)
          outcomes <- within (mapConcurrently
            (\(index, claim) -> run pool (decide (admin (7410 + index)) (eocId claim) "approved" notes))
            (zip [0 ..] claims))
          length (filter isRight outcomes) `shouldBe` 1
          [code | Left err <- outcomes, let code = errHTTPCode err] `shouldBe` replicate 5 409
          let winners = [partyText claimant | (claimant, Right _) <- zip claimants outcomes]
          organizer <- organizerOf pool eventId
          [organizer] `shouldBe` map Just winners
          counts <- runSqlPool (rawSql
            "SELECT status, count(*) FROM event_organizer_claim WHERE event_id = ? GROUP BY status ORDER BY status"
            [PersistInt64 eventId]) pool
          counts `shouldBe` [(Single ("approved" :: Text), Single (1 :: Int64)), (Single "superseded", Single 5)]

        it "bounds the claims one account keeps pending" $ \pool -> do
          claimant <- newAccount pool
          events <- mapM (const (newEvent pool Nothing)) [1 .. 6 :: Int]
          forM_ (take 5 events) $ \eventId ->
            expectRight =<< run pool (submit claimant eventId evidence)
          sixth <- run pool (submit claimant (last events) evidence)
          status sixth `shouldBe` Just 409

type App = ReaderT Env Handler

data ClaimHandlers = ClaimHandlers
  { hState :: Text -> App EventOrganizerClaimStateDTO
  , hSubmit :: Text -> EventOrganizerClaimCreateDTO -> App EventOrganizerClaimDTO
  , hQueue :: App [EventOrganizerClaimReviewDTO]
  , hDecide :: Text -> EventOrganizerClaimDecisionDTO -> App EventOrganizerClaimReviewDTO
  }

handlersFor :: AuthedUser -> ClaimHandlers
handlersFor actor =
  case socialEventsServer actor of
    _events :<|> _cities :<|> _sources :<|> _research :<|> _venues :<|> _artists :<|> _rsvps
      :<|> _invitations :<|> _moments :<|> _live :<|> _tickets :<|> _budget :<|> _finance
      :<|> _logistics :<|> _admins :<|> claims :<|> _memory ->
        case claims of
          stateH :<|> submitH :<|> queueH :<|> decideH -> ClaimHandlers stateH submitH queueH decideH

claimState :: AuthedUser -> Int64 -> App EventOrganizerClaimStateDTO
claimState actor eventId = hState (handlersFor actor) (textId eventId)

submit :: AuthedUser -> Int64 -> Text -> App EventOrganizerClaimDTO
submit actor eventId text = hSubmit (handlersFor actor) (textId eventId) (EventOrganizerClaimCreateDTO text)

queue :: AuthedUser -> App [EventOrganizerClaimReviewDTO]
queue actor = hQueue (handlersFor actor)

decide :: AuthedUser -> Text -> Text -> Text -> App EventOrganizerClaimReviewDTO
decide actor claimId decision text =
  hDecide (handlersFor actor) claimId (EventOrganizerClaimDecisionDTO decision text)

run :: ConnectionPool -> App a -> IO (Either ServerError a)
run pool action = do
  cfg <- loadConfig
  runHandler (runReaderT action (Env pool cfg))

status :: Either ServerError a -> Maybe Int
status = either (Just . errHTTPCode) (const Nothing)

expectRight :: Either ServerError a -> IO a
expectRight = either (\err -> fail ("unexpected refusal: " <> show err)) pure

withRoles :: [RoleEnum] -> Int64 -> AuthedUser
withRoles roles party = AuthedUser
  { auPartyId = toSqlKey party
  , auRoles = roles
  , auModules = modulesForRoles roles
  , auApiTokenId = Nothing
  , auSessionWitness = Nothing
  }

user, admin :: Int64 -> AuthedUser
user = withRoles [Fan]
admin = withRoles [Admin]

reviewer, otherReviewer :: AuthedUser
reviewer = admin 7401
otherReviewer = admin 7402

partyOf :: AuthedUser -> Int64
partyOf = fromSqlKey . auPartyId

partyText :: AuthedUser -> Text
partyText = textId . partyOf

textId :: Int64 -> Text
textId = T.pack . show

evidence, notes, fixtureTitle, fixtureAccount :: Text
evidence = "Organizo este evento desde 2024; el afiche oficial y la cuenta del local lo confirman."
notes = "Evidencia contrastada con el afiche oficial y el local."
fixtureTitle = "Event organizer claim fixture"
fixtureAccount = "Event organizer claim fixture account"

-- A signed-in account whose party exists, as an approval requires.
newAccount :: ConnectionPool -> IO AuthedUser
newAccount pool = do
  rows <- runSqlPool (rawSql
    "INSERT INTO party(display_name, is_org, created_at) VALUES (?, FALSE, now()) RETURNING id"
    [PersistText fixtureAccount]) pool
  case rows of
    [Single key] -> pure (user key)
    _ -> fail "Expected one party"

newEvent :: ConnectionPool -> Maybe Text -> IO Int64
newEvent pool organizer = do
  events <- runSqlPool (rawSql
    "INSERT INTO social_event(organizer_party_id,title,start_time,end_time,workflow_state_id,event_type_id,created_at,updated_at)\
    \ SELECT ?, ?, now()+interval '30 days', now()+interval '30 days 3 hours',\
    \ '00000000-0000-4000-8000-000000000231', (SELECT id FROM event_type ORDER BY id LIMIT 1), now(), now()\
    \ RETURNING id" [maybe PersistNull PersistText organizer, PersistText fixtureTitle]) pool
  case events of
    [Single key] -> pure key
    _ -> fail "Expected one event"

organizerOf :: ConnectionPool -> Int64 -> IO (Maybe Text)
organizerOf pool eventId = do
  rows <- runSqlPool (rawSql
    "SELECT organizer_party_id FROM social_event WHERE id = ?" [PersistInt64 eventId]) pool
  case rows of
    [Single organizer] -> pure organizer
    _ -> fail "claim fixture event disappeared"

statusOf :: ConnectionPool -> Text -> IO Text
statusOf pool claimId = do
  rows <- runSqlPool (rawSql
    "SELECT status FROM event_organizer_claim WHERE id::text = ?" [PersistText claimId]) pool
  case rows of
    [Single value] -> pure value
    _ -> fail "claim fixture disappeared"

-- Claims go with their events (ON DELETE CASCADE).
removeFixtures :: ConnectionPool -> IO ()
removeFixtures = runSqlPool $ do
  rawExecute "DELETE FROM social_event WHERE title = ?" [PersistText fixtureTitle]
  rawExecute "DELETE FROM party WHERE display_name = ?" [PersistText fixtureAccount]

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres@127.0.0.1:", "postgresql://postgres:postgres@postgres:5432/"]

within :: IO a -> IO a
within action = timeout 30000000 action >>= maybe (fail "Organizer claim case timed out") pure
