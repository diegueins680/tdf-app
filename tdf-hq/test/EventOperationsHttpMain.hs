{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (bracket_, throwIO)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (Value(..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (Key)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (RequestHeaders, statusCode)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai (Request)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler)
import System.Environment (getEnv)
import Test.Hspec

import TDF.Auth (AuthedUser, authContext)
import TDF.DB (Env(..))
import TDF.EventOperations.API (EventOperationsAPI)
import TDF.EventOperations.HttpTestConfig (httpTestConfig)
import TDF.EventOperations.Server (eventOperationsServer)
import qualified TDF.EventOperations.SessionFenceSpec as SessionFence
import qualified TDF.EventOperations.DatabaseBoundarySpec as Boundary
import qualified TDF.EventOperations.TypesSpec as Types

type ProtectedEvents = AuthProtect "bearer-token" :> EventOperationsAPI
type HttpResponse = HTTP.Response BL.ByteString

main :: IO ()
main = do
  dsn <- getEnv "EVENT_OPERATIONS_TEST_DSN"
  -- The wrapper sets this guard only after creating the disposable database.
  guardValue <- getEnv "EVENT_OPERATIONS_DISPOSABLE_HTTP_TEST"
  if guardValue /= "1" then fail "Disposable HTTP test guard required" else pure ()
  runNoLoggingT $ withPostgresqlPool (BS.pack dsn) 8 $ \pool -> liftIO $ do
    let env = Env pool httpTestConfig
        api = Proxy :: Proxy ProtectedEvents
        ctxProxy = Proxy :: Proxy '[AuthHandler Request AuthedUser]
        app = serveWithContext api (authContext env) $
          hoistServerWithContext api ctxProxy (flip runReaderT env) eventOperationsServer
    Warp.testWithApplicationSettings (Warp.setHost "127.0.0.1" Warp.defaultSettings) (pure app) $ \port -> do
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      hspec $ do
        Boundary.spec
        Types.spec
        httpSpec pool manager port
        SessionFence.spec pool

httpSpec :: ConnectionPool -> HTTP.Manager -> Int -> Spec
httpSpec pool manager port = describe "event operations authenticated HTTP / PostgreSQL" $ do
  it "returns only current manager options and never treats assignment as recipient eligibility" $ do
    response <- send "GET" "/80/tasks/8000/raci/context" (auth owner) Nothing
    expectStatus 200 response
    field "canManage" response `shouldBe` Just (Bool True)
    field "operationReady" response `shouldBe` Just (Bool True)
    field "eligiblePartyIds" response `shouldBe` decode "[1,2]"
    field "aggregateRevision" response `shouldBe` Just (String "4")
    lookup "Cache-Control" (HTTP.responseHeaders response) `shouldBe` Just "private, no-store"
    readOnly <- send "GET" "/80/tasks/8000/raci/context" (auth collaborator) Nothing
    expectStatus 200 readOnly
    field "canManage" readOnly `shouldBe` Just (Bool False)
    field "eligiblePartyIds" readOnly `shouldBe` decode "[]"
    field "replaceableAssignments" readOnly `shouldBe` decode "[]"
    send "GET" "/80/tasks/8000/raci/context" (auth outsider) Nothing >>= expectError 404 "not_found"
    countFor "event_operation_audit_event" 80 `shouldReturn` 0

  it "validates editor context cursor, exact target and current credentials" $ do
    page <- send "GET" "/80/tasks/8000/raci/context?afterPartyId=1" (auth owner) Nothing
    expectStatus 200 page
    field "eligiblePartyIds" page `shouldBe` decode "[2]"
    forM_ ["-1","9007199254740992","bad"] $ \cursor ->
      send "GET" ("/80/tasks/8000/raci/context?afterPartyId=" <> cursor) (auth owner) Nothing >>= expectStatus 400
    send "GET" "/81/tasks/8000/raci/context" (auth owner) Nothing >>= expectError 404 "not_found"
    send "GET" "/80/tasks/8000/raci/context" [] Nothing >>= expectStatus 401

  it "fails closed when editor context SQL is unavailable, leaving old task reads intact" $
    bracket_ (execute "ALTER FUNCTION event_operation_read_raci_editor_context(BIGINT,BIGINT,BIGINT,BIGINT) RENAME TO raci_context_http_saved")
      (execute "ALTER FUNCTION raci_context_http_saved(BIGINT,BIGINT,BIGINT,BIGINT) RENAME TO event_operation_read_raci_editor_context") $ do
        send "GET" "/80/tasks/8000/raci/context" (auth owner) Nothing >>= expectError 503 "event_operations_unavailable"
        send "GET" "/80/tasks/8000" (auth owner) Nothing >>= expectStatus 200

  it "opts into a coherent revision envelope without changing old task JSON or domain records" $ do
    before <- scalar "SELECT revision FROM event_operation_task_revision WHERE activity_id=8000"
    original <- send "GET" "/80/tasks/8000" (auth owner) Nothing
    response <- send "GET" "/80/tasks/8000/revisioned" (auth owner) Nothing
    expectStatus 200 response
    decode (HTTP.responseBody response) `shouldBe` Just (object
      ["task" .= (decode (HTTP.responseBody original) :: Maybe Value),
       "aggregateRevision" .= T.pack (show before)])
    lookup "Cache-Control" (HTTP.responseHeaders response) `shouldBe` Just "private, no-store"
    send "GET" "/80/tasks/8000/revisioned" (auth collaborator) Nothing >>= expectStatus 200
    scalar "SELECT revision FROM event_operation_task_revision WHERE activity_id=8000" `shouldReturn` before
    countFor "event_operation_audit_event" 80 `shouldReturn` 0
    countFor "event_operation_command_receipt" 80 `shouldReturn` 0

  it "applies exact scope, capture and credential checks to the opt-in representation" $ do
    missing <- send "GET" "/80/tasks/999999/revisioned" (auth collaborator) Nothing
    forM_ ["/80/tasks/8001/revisioned", "/81/tasks/8000/revisioned"] $ \path ->
      send "GET" path (auth collaborator) Nothing >>= expectOpaque missing
    send "GET" "/80/tasks/8000/revisioned?actorPartyId=1" (auth outsider) Nothing >>= expectOpaque missing
    forM_ ["/80/tasks/no-id/revisioned", "/80/tasks/0/revisioned", "/0/tasks/8000/revisioned",
           "/80/tasks/9007199254740992/revisioned"] $ \path ->
      send "GET" path (auth owner) Nothing >>= expectStatus 400
    forM_ [[], auth "unknown", auth "http-inactive-test-token", auth "http-reset-test-token"] $ \headers ->
      send "GET" "/80/tasks/8000/revisioned" headers Nothing >>= expectStatus 401

  it "transports the maximum BIGINT revision losslessly over real HTTP" $ do
    before <- scalar "SELECT revision FROM event_operation_task_revision WHERE activity_id=8000"
    bracket_ (execute "UPDATE event_operation_task_revision SET revision=9223372036854775807 WHERE activity_id=8000")
             (execute ("UPDATE event_operation_task_revision SET revision=" <> T.pack (show before) <> " WHERE activity_id=8000")) $ do
      response <- send "GET" "/80/tasks/8000/revisioned" (auth owner) Nothing
      expectStatus 200 response
      field "aggregateRevision" response `shouldBe` Just (String "9223372036854775807")

  it "fails unavailable during reader rollback and restores the opt-in route afterward" $ do
    bracket_ (execute "ALTER FUNCTION event_operation_read_task_with_revision(BIGINT,BIGINT,BIGINT) RENAME TO revisioned_read_test_saved")
             (execute "ALTER FUNCTION revisioned_read_test_saved(BIGINT,BIGINT,BIGINT) RENAME TO event_operation_read_task_with_revision") $ do
      send "GET" "/80/tasks/8000/revisioned" (auth owner) Nothing >>= expectError 503 "event_operations_unavailable"
      send "GET" "/80/tasks/8000" (auth owner) Nothing >>= expectStatus 200
    send "GET" "/80/tasks/8000/revisioned" (auth owner) Nothing >>= expectStatus 200

  it "serves an exact authorized task projection without HTTP caching" $ do
    response <- send "GET" "/80/tasks/8000" (auth owner) Nothing
    expectStatus 200 response
    decode (HTTP.responseBody response) `shouldBe` (decode
      "{\"eventId\":80,\"activityId\":8000,\"status\":\"planned\",\"version\":1,\"policy\":{\"requiresAccountability\":true,\"dependenciesGateCompletion\":true,\"version\":1},\"raci\":[{\"partyId\":1,\"role\":\"accountable\"},{\"partyId\":3,\"role\":\"responsible\"}],\"accountabilityNeedsAttention\":false}"
      :: Maybe Value)
    lookup "Cache-Control" (HTTP.responseHeaders response) `shouldBe` Just "private, no-store"

  it "allows an exact task grant without granting the parent snapshot or sibling task" $ do
    send "GET" "/80/tasks/8000" (auth collaborator) Nothing >>= expectStatus 200
    send "GET" "/80" (auth collaborator) Nothing >>= expectError 404 "not_found"
    missing <- send "GET" "/80/tasks/999999" (auth collaborator) Nothing
    send "GET" "/80/tasks/8001" (auth collaborator) Nothing >>= expectOpaque missing
    send "GET" "/81/tasks/8000" (auth collaborator) Nothing >>= expectOpaque missing
    send "GET" "/80/tasks/8100" (auth owner) Nothing >>= expectOpaque missing
    send "GET" "/80/tasks/8000" (auth outsider) Nothing >>= expectOpaque missing
    send "GET" "/80/tasks/8000?actorPartyId=1&evaluatedAt=2020-01-01" (auth outsider) Nothing
      >>= expectOpaque missing
    countFor "event_operation_audit_event" 80 `shouldReturn` 0
    countFor "event_operation_command_receipt" 80 `shouldReturn` 0

  it "validates task captures and real credentials without fabricating a successful object" $ do
    forM_ ["/80/tasks/no-id", "/80/tasks/0", "/0/tasks/8000", "/80/tasks/9007199254740992"] $ \path ->
      send "GET" path (auth owner) Nothing >>= expectStatus 400
    forM_ [[], auth "unknown", auth "http-inactive-test-token", auth "http-reset-test-token"] $ \headers ->
      send "GET" "/80/tasks/8000" headers Nothing >>= expectStatus 401
    response <- send "GET" "/80/tasks/8001" [("Cookie", "tdf_session=http-owner-test-token")] Nothing
    expectStatus 200 response
    field "status" response `shouldBe` Just (String "confirmed")
    field "policy" response `shouldBe` Nothing

  it "does not widen task authority after a downgrade to event.read" $
    bracket_ (execute "UPDATE event_operation_grant SET scope_code='event.read',resource_kind='event',resource_id=NULL WHERE event_id=80")
             (execute "UPDATE event_operation_grant SET scope_code='task.read',resource_kind='task',resource_id='8000' WHERE event_id=80") $ do
      send "GET" "/80" (auth collaborator) Nothing >>= expectStatus 200
      send "GET" "/80/tasks/8000" (auth collaborator) Nothing >>= expectError 404 "not_found"
      send "GET" "/80/tasks/8000/revisioned" (auth collaborator) Nothing >>= expectError 404 "not_found"

  it "reauthorizes task reads after expiry and revocation without changing assignments" $ do
    bracket_ (execute "UPDATE event_operation_grant SET valid_from=clock_timestamp()-interval '2 days',valid_until=clock_timestamp()-interval '1 day' WHERE event_id=80")
             (execute "UPDATE event_operation_grant SET valid_until=NULL WHERE event_id=80") $
      send "GET" "/80/tasks/8000" (auth collaborator) Nothing >>= expectError 404 "not_found"
    bracket_ (execute "UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='HTTP task test' WHERE event_id=80")
             (execute "UPDATE event_operation_grant SET revoked_at=NULL,revoked_by_party_id=NULL,revocation_reason=NULL WHERE event_id=80") $
      send "GET" "/80/tasks/8000" (auth collaborator) Nothing >>= expectError 404 "not_found"
    send "GET" "/80/tasks/8000" (auth collaborator) Nothing >>= expectStatus 200
    scalar "SELECT count(*) FROM event_operation_raci_assignment WHERE activity_id=8000" `shouldReturn` 2

  it "surfaces expired accountability explicitly without declaring the task ready" $
    bracket_ (execute "UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()-interval '2 days',valid_until=clock_timestamp()-interval '1 day' WHERE activity_id=8000 AND raci_role='responsible'")
             (execute "UPDATE event_operation_raci_assignment SET valid_until=NULL WHERE activity_id=8000") $ do
      response <- send "GET" "/80/tasks/8000" (auth owner) Nothing
      expectStatus 200 response
      field "accountabilityNeedsAttention" response `shouldBe` Just (Bool True)
      field "raci" response `shouldBe` (decode "[{\"partyId\":1,\"role\":\"accountable\"}]" :: Maybe Value)

  it "sanitizes invalid task rows and a missing projection function, then recovers" $ do
    bracket_ (execute "UPDATE event_logistics_activity SET status='private-invalid-status' WHERE id=8000")
             (execute "UPDATE event_logistics_activity SET status='planned' WHERE id=8000") $ do
      send "GET" "/80/tasks/8000" (auth owner) Nothing >>= expectError 503 "event_operations_unavailable"
      send "GET" "/80/tasks/8000/revisioned" (auth owner) Nothing >>= expectError 503 "event_operations_unavailable"
    bracket_ (execute "ALTER FUNCTION event_operation_read_task(BIGINT,BIGINT,BIGINT) RENAME TO task_read_test_saved")
             (execute "ALTER FUNCTION task_read_test_saved(BIGINT,BIGINT,BIGINT) RENAME TO event_operation_read_task") $
      send "GET" "/80/tasks/8000" (auth owner) Nothing >>= expectError 503 "event_operations_unavailable"
    send "GET" "/80/tasks/8000" (auth owner) Nothing >>= expectStatus 200

  it "rejects missing, unknown, inactive and password-reset credentials before any command" $ do
    forM_ [[], auth "unknown", auth "http-inactive-test-token", auth "http-reset-test-token"] $ \headers -> do
      send "GET" "/60" headers Nothing >>= expectStatus 401
      send "POST" "/60/transitions" (headers <> idem 1) (Just (body 1 "planning")) >>= expectStatus 401
    countFor "event_operation_audit_event" 60 `shouldReturn` 0

  it "rejects duplicate authorization and conflicting cookie credentials" $ do
    let duplicate = auth owner <> auth collaborator
        conflict = auth owner <> [("Cookie", "tdf_session=http-outsider-test-token")]
    forM_ [duplicate, conflict] $ \headers ->
      send "GET" "/60" headers Nothing >>= expectStatus 401

  it "accepts a real session cookie and returns the canonical snapshot" $ do
    response <- send "GET" "/60" [("Cookie", "tdf_session=http-owner-test-token")] Nothing
    expectStatus 200 response
    field "eventId" response `shouldBe` Just (Number 60)
    field "canonicalState" response `shouldBe` Just (String "draft")
    field "version" response `shouldBe` Just (Number 1)

  it "does not confer access through a guessed event ID" $ do
    send "GET" "/60" (auth outsider) Nothing >>= expectError 404 "not_found"
    send "GET" "/999999" (auth owner) Nothing >>= expectError 404 "not_found"
    missing <- post 999999 2 outsider (body 1 "planning")
    forM_ [body 1 "planning", body 1 "planning", body 99 "approved"] $ \payload -> do
      hidden <- post 60 2 outsider payload
      expectOpaque missing hidden
    countFor "event_operation_transition" 60 `shouldReturn` 0
    countFor "event_operation_command_receipt" 60 `shouldReturn` 1
    countFor "event_operation_audit_event" 60 `shouldReturn` 3
    scalar "SELECT count(*) FROM event_operation_command_receipt WHERE event_id=60 AND response->>'error'='forbidden'"
      `shouldReturn` 1

  it "rejects malformed captures, headers and strict JSON before durable command work" $ do
    send "GET" "/not-an-integer" (auth owner) Nothing >>= expectStatus 400
    let valid = body 1 "planning"
        withExtra = object ["expectedVersion" .= (1 :: Int), "targetState" .= ("planning" :: Text),
                            "correlationId" .= ("http-boundary" :: Text), "actorPartyId" .= (2 :: Int)]
        withNull = object ["expectedVersion" .= (1 :: Int), "targetState" .= ("planning" :: Text),
                           "correlationId" .= ("http-boundary" :: Text), "reason" .= Null]
    send "POST" "/61/transitions" (auth owner) (Just valid) >>= expectStatus 400
    send "POST" "/61/transitions" (auth owner <> [("Idempotency-Key", "invalid")]) (Just valid)
      >>= expectStatus 400
    forM_ [withExtra, withNull, body 1 "invented"] $ \payload ->
      post 61 3 owner payload >>= expectStatus 400
    countFor "event_operation_command_receipt" 61 `shouldReturn` 0
    countFor "event_operation_audit_event" 61 `shouldReturn` 0

  it "returns exact historical replay and conflicts on a changed command body" $ do
    first <- post 62 4 owner (body 1 "planning")
    expectStatus 200 first
    replay <- post 62 4 owner (body 1 "planning")
    expectStatus 200 replay
    expectReplay first replay
    post 62 4 owner (body 1 "pending_approval") >>= expectError 409 "idempotency_conflict"
    current <- send "GET" "/62" (auth owner) Nothing
    expectStatus 200 current
    field "canonicalState" current `shouldBe` field "canonicalState" first
    field "version" current `shouldBe` Just (Number 2)
    countFor "event_operation_transition" 62 `shouldReturn` 1
    countFor "event_operation_command_receipt" 62 `shouldReturn` 1
    scalar "SELECT count(*) FROM event_operation_command_receipt WHERE event_id=62 AND response->>'replayed'='false'"
      `shouldReturn` 1

  it "deduplicates simultaneous HTTP retries" $ do
    (first, second) <- parallelPair (post 63 5 owner (body 1 "planning"))
                                   (post 63 5 owner (body 1 "planning"))
    map (statusCode . HTTP.responseStatus) [first, second] `shouldBe` [200, 200]
    if field "replayed" first == Just (Bool False)
      then expectReplay first second else expectReplay second first
    countFor "event_operation_transition" 63 `shouldReturn` 1
    countFor "event_operation_command_receipt" 63 `shouldReturn` 1
    scalar "SELECT count(*) FROM event_operation_command_receipt WHERE event_id=63 AND response->>'replayed'='false'"
      `shouldReturn` 1

  it "reauthorizes receipt replay after revoking event access with the same active session" $ do
    post 64 6 collaborator (body 1 "planning") >>= expectStatus 200
    execute "UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='HTTP test' WHERE event_id=64"
    send "GET" "/64" (auth collaborator) Nothing >>= expectError 404 "not_found"
    missing <- post 999999 6 collaborator (body 1 "planning")
    post 64 6 collaborator (body 1 "planning") >>= expectOpaque missing
    post 64 7 collaborator (body 2 "pending_approval") >>= expectOpaque missing
    countFor "event_operation_transition" 64 `shouldReturn` 1
    scalar "SELECT count(*) FROM event_operation_audit_event WHERE event_id=64 AND outcome='rejected'"
      `shouldReturn` 2

  it "allows historical read-only replay but rejects a new mutation after downgrade" $ do
    first <- post 65 8 collaborator (body 1 "planning")
    expectStatus 200 first
    execute "UPDATE event_operation_grant SET scope_code='event.read' WHERE event_id=65"
    replay <- post 65 8 collaborator (body 1 "planning")
    expectStatus 200 replay
    expectReplay first replay
    post 65 9 collaborator (body 2 "pending_approval") >>= expectError 403 "forbidden"
    countFor "event_operation_transition" 65 `shouldReturn` 1
    scalar "SELECT count(*) FROM event_operation_command_receipt WHERE event_id=65 AND response->>'replayed'='false' AND outcome='accepted'"
      `shouldReturn` 1

  it "rejects expired event grants on the next HTTP request" $ do
    post 66 10 collaborator (body 1 "planning") >>= expectStatus 200
    execute "UPDATE event_operation_grant SET valid_from=clock_timestamp()-interval '2 days',valid_until=clock_timestamp()-interval '1 day' WHERE event_id=66"
    send "GET" "/66" (auth collaborator) Nothing >>= expectError 404 "not_found"
    missing <- post 999999 10 collaborator (body 1 "planning")
    post 66 10 collaborator (body 1 "planning") >>= expectOpaque missing
    countFor "event_operation_transition" 66 `shouldReturn` 1

  it "hides an occupied private command key for other actors and changed bodies" $ do
    first <- post 73 21 owner (body 1 "planning")
    expectStatus 200 first
    missing <- post 999999 21 outsider (body 1 "planning")
    post 73 21 outsider (body 1 "planning") >>= expectOpaque missing
    post 73 21 outsider (body 99 "approved") >>= expectOpaque missing
    post 73 22 outsider (body 99 "approved") >>= expectOpaque missing
    replay <- post 73 21 owner (body 1 "planning")
    expectReplay first replay
    countFor "event_operation_transition" 73 `shouldReturn` 1
    countFor "event_operation_command_receipt" 73 `shouldReturn` 2
    scalar "SELECT count(*) FROM event_operation_audit_event WHERE event_id=73 AND outcome='rejected'"
      `shouldReturn` 3

  it "hides a rejected receipt after revocation and preserves it after read restoration" $ do
    post 74 23 collaborator (body 1 "planning") >>= expectError 403 "forbidden"
    execute "UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='HTTP privacy test' WHERE event_id=74"
    missing <- post 999999 23 collaborator (body 1 "planning")
    post 74 23 collaborator (body 1 "planning") >>= expectOpaque missing
    execute "UPDATE event_operation_grant SET revoked_at=NULL,revoked_by_party_id=NULL,revocation_reason=NULL WHERE event_id=74"
    post 74 23 collaborator (body 1 "planning") >>= expectError 403 "forbidden"
    countFor "event_operation_transition" 74 `shouldReturn` 0
    countFor "event_operation_command_receipt" 74 `shouldReturn` 1
    countFor "event_operation_audit_event" 74 `shouldReturn` 2

  it "requires an independent approver and refuses unimplemented publication effects" $ do
    post 67 11 owner (body 1 "planning") >>= expectStatus 200
    post 67 12 owner (body 2 "pending_approval") >>= expectStatus 200
    post 67 13 owner (body 3 "approved") >>= expectError 409 "separation_of_duties"
    post 67 14 collaborator (body 3 "approved") >>= expectStatus 200
    post 67 15 owner (body 4 "published") >>= expectError 409 "transition_effects_not_ready"
    countFor "event_operation_transition" 67 `shouldReturn` 3

  it "returns one accepted command and one version conflict for competing HTTP writes" $ do
    (first, second) <- parallelPair (post 68 16 owner (body 1 "planning"))
                                   (post 68 17 owner (body 1 "planning"))
    sort (map (statusCode . HTTP.responseStatus) [first, second]) `shouldBe` [200, 409]
    forM_ [r | r <- [first,second], statusCode (HTTP.responseStatus r) == 409] $
      expectError 409 "version_conflict"
    countFor "event_operation_transition" 68 `shouldReturn` 1
    countFor "event_operation_command_receipt" 68 `shouldReturn` 2

  it "rejects a deactivated token on reuse rather than caching its authenticated identity" $ do
    let token = "http-revocable-test-token"
    send "GET" "/69" (auth token) Nothing >>= expectStatus 200
    execute "UPDATE api_token SET active=FALSE WHERE token='http-revocable-test-token'"
    send "GET" "/69" (auth token) Nothing >>= expectStatus 401
    post 69 18 token (body 1 "planning") >>= expectStatus 401
    countFor "event_operation_audit_event" 69 `shouldReturn` 0

  it "fails closed while the event operations feature is disabled" $
    bracket_ (setFlag False) (setFlag True) $ do
      send "GET" "/70" (auth owner) Nothing >>= expectError 404 "feature_disabled"
      send "GET" "/80/tasks/8000" (auth owner) Nothing >>= expectError 404 "feature_disabled"
      send "GET" "/80/tasks/8000/revisioned" (auth owner) Nothing >>= expectError 404 "feature_disabled"
      send "POST" raciPath (auth owner <> idem 300) (Just raciBody) >>= expectError 404 "feature_disabled"
      post 70 19 owner (body 1 "planning") >>= expectError 404 "feature_disabled"
      countFor "event_operation_audit_event" 70 `shouldReturn` 0

  it "maps a real SQL failure to a sanitized non-success response and recovers" $ do
    bracket_
      (execute "ALTER FUNCTION event_operation_read_snapshot(BIGINT,BIGINT) RENAME TO event_operation_read_snapshot_test_saved")
      (execute "ALTER FUNCTION event_operation_read_snapshot_test_saved(BIGINT,BIGINT) RENAME TO event_operation_read_snapshot") $ do
        send "GET" "/71" (auth owner) Nothing >>= expectError 503 "event_operations_unavailable"
    send "GET" "/71" (auth owner) Nothing >>= expectStatus 200

  it "rejects an inactive assigned canonical role on the next authentication attempt" $
    bracket_ (execute "UPDATE security_role SET active=FALSE")
             (execute "UPDATE security_role SET active=TRUE") $ do
      send "GET" "/72" (auth owner) Nothing >>= expectStatus 401
      post 72 20 owner (body 1 "planning") >>= expectStatus 401
      countFor "event_operation_audit_event" 72 `shouldReturn` 0
  it "rejects invalid RACI transport, stale versions and unprivileged task readers" $ do
    execute "INSERT INTO social_event(id,organizer_party_id) VALUES (82,'1'); INSERT INTO event_operation_event_state(event_id,canonical_state,version,migration_evidence) VALUES (82,'planning',1,'RACI HTTP test'); INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind) VALUES(82,1,'primary_owner'); INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES(8200,82,'planned',1); INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id) VALUES(8200,1,'accountable',1),(8200,3,'responsible',1); INSERT INTO event_operation_task_policy(activity_id) VALUES(8200); INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id) VALUES(82,2,'task.read','task','8200',1)"
    let patch key value = case raciBody of Object fields -> Object (KM.insert key value fields); _ -> Null
    forM_ [patch "expectedRevision" (Number 4), patch "expectedRevision" (String "04"),
           patch "expectedRevision" (String "9223372036854775808"), patch "actorPartyId" (Number 1),
           patch "toPartyId" (Number 3), patch "reason" (String " "), patch "correlationId" Null] $ \payload ->
      send "POST" raciPath (auth owner <> idem 300) (Just payload) >>= expectStatus 400
    send "POST" raciPath (auth owner) (Just raciBody) >>= expectStatus 400
    send "POST" raciPath (idem 300) (Just raciBody) >>= expectStatus 401
    send "POST" raciPath (auth collaborator <> idem 300) (Just raciBody) >>= expectError 403 "forbidden"
    send "POST" raciPath (auth outsider <> idem 300) (Just raciBody) >>= expectError 404 "not_found"
    send "POST" raciPath (auth owner <> idem 300) (Just (patch "expectedRevision" (String "3")))
      >>= expectError 409 "version_conflict"
    scalar "SELECT revision FROM event_operation_task_revision WHERE activity_id=8200" `shouldReturn` 4
    countFor "event_operation_command_receipt" 82 `shouldReturn` 0

  it "rolls back real SQL reassignment effects when its returned receipt is malformed or foreign" $ do
    let signature = "(BIGINT,BIGINT,BIGINT,UUID,BIGINT,TEXT,BIGINT,BIGINT,TEXT,TEXT)"
        restore = execute ("DROP FUNCTION event_operation_reassign_raci" <> signature
          <> "; ALTER FUNCTION raci_http_saved" <> signature <> " RENAME TO event_operation_reassign_raci")
    forM_ ["'{}'::jsonb", "result || '{\"activityId\":999}'::jsonb",
           "result || '{\"aggregateRevision\":\"7\"}'::jsonb", "'{\"error\":\"private diagnostic\"}'::jsonb"] $ \bad ->
      bracket_ (execute ("ALTER FUNCTION event_operation_reassign_raci" <> signature <> " RENAME TO raci_http_saved; "
        <> "CREATE FUNCTION event_operation_reassign_raci(bigint,bigint,bigint,uuid,bigint,text,bigint,bigint,text,text) RETURNS jsonb LANGUAGE plpgsql AS $$ DECLARE result jsonb; BEGIN result := raci_http_saved($1,$2,$3,$4,$5,$6,$7,$8,$9,$10); RETURN " <> bad <> "; END $$")) restore $ do
        response <- send "POST" raciPath (auth owner <> idem 300) (Just raciBody)
        expectError 503 "event_operations_unavailable" response
        lookup "Cache-Control" (HTTP.responseHeaders response) `shouldBe` Just "private, no-store"
        scalar "SELECT revision FROM event_operation_task_revision WHERE activity_id=8200" `shouldReturn` 4
        scalar "SELECT count(*) FROM event_operation_raci_assignment WHERE activity_id=8200 AND revoked_at IS NOT NULL" `shouldReturn` 0
        countFor "event_operation_audit_event" 82 `shouldReturn` 0
        countFor "event_operation_command_receipt" 82 `shouldReturn` 0

  it "never reports success when a deferred failure rejects COMMIT after receipt validation" $
    bracket_ (execute "CREATE FUNCTION raci_http_commit_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'private commit diagnostic' USING ERRCODE='ZX009'; END $$; CREATE CONSTRAINT TRIGGER raci_http_commit_failure AFTER INSERT ON event_operation_audit_event DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION raci_http_commit_failure()")
      (execute "DROP TRIGGER raci_http_commit_failure ON event_operation_audit_event; DROP FUNCTION raci_http_commit_failure()") $ do
        send "POST" raciPath (auth owner <> idem 300) (Just raciBody)
          >>= expectError 503 "event_operations_unavailable"
        scalar "SELECT revision FROM event_operation_task_revision WHERE activity_id=8200" `shouldReturn` 4
        countFor "event_operation_audit_event" 82 `shouldReturn` 0
        countFor "event_operation_command_receipt" 82 `shouldReturn` 0

  it "commits the authenticated RACI command once and replays its original bound receipt" $ do
    response <- send "POST" raciPath (auth owner <> idem 300) (Just raciBody)
    expectStatus 200 response
    field "aggregateRevision" response `shouldBe` Just (String "6")
    field "activityId" response `shouldBe` Just (Number 8200)
    field "fromPartyId" response `shouldBe` Just (Number 3)
    field "toPartyId" response `shouldBe` Just (Number 2)
    lookup "Cache-Control" (HTTP.responseHeaders response) `shouldBe` Just "private, no-store"
    retry <- send "POST" raciPath (auth owner <> idem 300) (Just raciBody)
    expectReplay response retry
    send "POST" raciPath (auth owner <> idem 301) (Just raciBody) >>= expectError 409 "version_conflict"
    countFor "event_operation_audit_event" 82 `shouldReturn` 1
    countFor "event_operation_command_receipt" 82 `shouldReturn` 1
    scalar "SELECT revision FROM event_operation_task_revision WHERE activity_id=8200" `shouldReturn` 6

  it "deduplicates concurrent RACI HTTP retries without changing the aggregate twice" $ do
    execute "INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES(8201,82,'planned',1); INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id) VALUES(8201,3,'responsible',1); INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id) VALUES(82,2,'task.read','task','8201',1)"
    let payload = case raciBody of Object fields -> Object (KM.insert "expectedRevision" (String "2") fields); _ -> Null
        request = send "POST" "/82/tasks/8201/raci/reassign" (auth owner <> idem 302) (Just payload)
    first <- newEmptyMVar
    _ <- forkFinally request (putMVar first)
    second <- request
    initial <- takeMVar first >>= either throwIO pure
    mapM_ (expectStatus 200) [initial, second]
    sort [field "replayed" initial, field "replayed" second] `shouldBe` [Just (Bool False), Just (Bool True)]
    scalar "SELECT revision FROM event_operation_task_revision WHERE activity_id=8201" `shouldReturn` 4
    scalar "SELECT count(*) FROM event_operation_command_receipt WHERE operation_code='event.task.raci.reassign/8201'" `shouldReturn` 1

  it "allows historical replay but blocks new RACI writes outside the supported lifecycle" $
    bracket_ (execute "UPDATE event_operation_event_state SET canonical_state='ready' WHERE event_id=82")
      (execute "UPDATE event_operation_event_state SET canonical_state='planning' WHERE event_id=82") $ do
        send "POST" raciPath (auth owner <> idem 300) (Just raciBody) >>= expectStatus 200
        response <- send "POST" raciPath (auth owner <> idem 310) (Just raciBody)
        expectError 409 "operation_not_ready" response
        lookup "Cache-Control" (HTTP.responseHeaders response) `shouldBe` Just "private, no-store"
        scalar "SELECT revision FROM event_operation_task_revision WHERE activity_id=8200" `shouldReturn` 6

  it "fails unavailable when the RACI function is rolled back without falling back to legacy writes" $
    bracket_ (execute "ALTER FUNCTION event_operation_reassign_raci(BIGINT,BIGINT,BIGINT,UUID,BIGINT,TEXT,BIGINT,BIGINT,TEXT,TEXT) RENAME TO raci_http_saved")
      (execute "ALTER FUNCTION raci_http_saved(BIGINT,BIGINT,BIGINT,UUID,BIGINT,TEXT,BIGINT,BIGINT,TEXT,TEXT) RENAME TO event_operation_reassign_raci") $ do
        send "POST" raciPath (auth owner <> idem 300) (Just raciBody)
          >>= expectError 503 "event_operations_unavailable"
        send "GET" "/82/tasks/8200/revisioned" (auth owner) Nothing >>= expectStatus 200
  where
    raciPath = "/82/tasks/8200/raci/reassign"
    raciBody = object ["expectedRevision" .= ("4" :: Text), "role" .= ("responsible" :: Text),
      "fromPartyId" .= (3 :: Int), "toPartyId" .= (2 :: Int),
      "reason" .= ("Synthetic RACI HTTP test" :: Text), "correlationId" .= ("raci-http" :: Text)]
    owner = "http-owner-test-token"
    collaborator = "http-collaborator-test-token"
    outsider = "http-outsider-test-token"
    auth token = [("Authorization", "Bearer " <> token)]
    idem n = [("Idempotency-Key", BS.pack (commandId n))]
    body version target = object ["expectedVersion" .= (version :: Int), "targetState" .= (target :: Text),
                                  "correlationId" .= ("http-boundary" :: Text)]
    send :: BS.ByteString -> BS.ByteString -> RequestHeaders -> Maybe Value -> IO HttpResponse
    send verb suffix headers payload = HTTP.httpLbs HTTP.defaultRequest
      { HTTP.host = "127.0.0.1", HTTP.port = port, HTTP.secure = False
      , HTTP.path = "/event-operations/events" <> fst (BS.break (== '?') suffix), HTTP.method = verb
      , HTTP.queryString = snd (BS.break (== '?') suffix)
      , HTTP.requestHeaders = ("Content-Type", "application/json") : headers
      , HTTP.requestBody = HTTP.RequestBodyLBS (maybe BL.empty encode payload)
      , HTTP.redirectCount = 0, HTTP.responseTimeout = HTTP.responseTimeoutMicro 10000000
      } manager
    post event command token payload = send "POST" (BS.pack ("/" <> show (event :: Int) <> "/transitions"))
      (auth token <> idem command) (Just payload)
    execute sql = runSqlPool (rawExecute sql []) pool
    scalar sql = do
      rows <- runSqlPool (rawSql sql []) pool :: IO [Single Int64]
      case rows of
        [Single value] -> pure value
        _ -> fail "Expected exactly one scalar database result"
    countFor table event = scalar ("SELECT count(*) FROM " <> table <> " WHERE event_id=" <> T.pack (show (event :: Int)))
    setFlag enabled = execute ("UPDATE event_operation_feature_flag SET enabled=" <> (if enabled then "TRUE" else "FALSE")
      <> ",updated_at=clock_timestamp(),updated_by_party_id=1,change_reason='HTTP test flag' WHERE feature_code='event.operations.api'")

commandId :: Int -> String
commandId n = "60000000-0000-4000-8000-" <> replicate (12 - length digits) '0' <> digits
  where digits = show n

expectStatus :: Int -> HttpResponse -> IO ()
expectStatus expected response = statusCode (HTTP.responseStatus response) `shouldBe` expected

expectError :: Int -> Text -> HttpResponse -> IO ()
expectError expected code response = do
  expectStatus expected response
  decode (HTTP.responseBody response) `shouldBe` Just (object ["code" .= code])

-- Compare the wire envelope, excluding only Warp's generic clock-based Date header.
expectOpaque :: HttpResponse -> HttpResponse -> IO ()
expectOpaque missing hidden = do
  expectError 404 "not_found" missing
  expectError 404 "not_found" hidden
  HTTP.responseStatus hidden `shouldBe` HTTP.responseStatus missing
  HTTP.responseBody hidden `shouldBe` HTTP.responseBody missing
  let headers = sort . filter ((/= "Date") . fst) . HTTP.responseHeaders
  headers hidden `shouldBe` headers missing
  lookup "Content-Type" (HTTP.responseHeaders hidden) `shouldBe` Just "application/json"

field :: Key -> HttpResponse -> Maybe Value
field key response = case decode (HTTP.responseBody response) of
  Just (Object fields) -> KM.lookup key fields
  _ -> Nothing

-- The existing contract changes only response metadata, never the stored historical result.
expectReplay :: HttpResponse -> HttpResponse -> IO ()
expectReplay original replay = do
  expectStatus 200 original
  expectStatus 200 replay
  field "replayed" original `shouldBe` Just (Bool False)
  field "replayed" replay `shouldBe` Just (Bool True)
  case decode (HTTP.responseBody original) of
    Just (Object fields) -> decode (HTTP.responseBody replay)
      `shouldBe` Just (Object (KM.insert "replayed" (Bool True) fields))
    _ -> expectationFailure "Original command response must be a JSON object"

-- Both workers cross the same start barrier; propagate failures rather than hanging silently.
parallelPair :: IO a -> IO a -> IO (a,a)
parallelPair left right = do
  start <- newEmptyMVar
  first <- newEmptyMVar
  second <- newEmptyMVar
  void $ forkFinally (readMVar start >> left) (putMVar first)
  void $ forkFinally (readMVar start >> right) (putMVar second)
  putMVar start ()
  firstResult <- takeMVar first
  secondResult <- takeMVar second
  (,) <$> either throwIO pure firstResult <*> either throwIO pure secondResult
