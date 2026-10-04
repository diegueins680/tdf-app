{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module TDF.EventOperations.SessionFenceSpec (spec) where

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Exception
  ( AsyncException(ThreadKilled), SomeException, bracket, bracket_, finally, fromException, throwIO, try )
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Database.Persist.Sql (ConnectionPool, PersistValue(..), Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool, toSqlKey)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import Network.Wai (Request)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Server.Experimental.Auth (AuthHandler)
import System.Timeout (timeout)
import Test.Hspec

import TDF.Auth (AuthedUser(..), authContext, loadAuthedUser, withCurrentAuthSession)
import TDF.DB (Env(..))
import TDF.EventOperations.HttpTestConfig (httpTestConfig)
import TDF.EventOperations.API (EventOperationsAPI)
import TDF.EventOperations.DatabaseBoundary (DatabaseFailure(..), loadSnapshot, tryDatabaseAction)
import TDF.EventOperations.Server (eventOperationsServer)
import TDF.EventOperations.Types

spec :: ConnectionPool -> Spec
spec pool = describe "event operations in-flight session fence / PostgreSQL" $ do
  it "rejects an already authenticated snapshot after token revocation" $ do
    user <- authenticated pool "http-flight-test-token"
    runSqlPool (rawExecute "UPDATE api_token SET active=FALSE WHERE token='http-flight-test-token'" []) pool
    result <- snapshot pool user 90
    case result of
      Left failure -> errHTTPCode failure `shouldBe` 401
      Right _ -> expectationFailure "Revoked in-flight session disclosed an event snapshot"

  forM_ (zip [100..] mutations) $ \(eventId, (label, mutation)) ->
    it ("rejects GET/task/new/replay for captured sessions after " <> label) $ do
      (token, user) <- seedSession pool eventId
      accepted <- transition pool user eventId eventId 1 Planning
      expectRight accepted
      taskSnapshot pool user eventId >>= expectRight
      revisionedTaskSnapshot pool user eventId >>= expectRight
      before <- receipt pool eventId
      execute pool mutation [PersistText token]
      snapshot pool user eventId >>= expect401
      taskSnapshot pool user eventId >>= expect401
      revisionedTaskSnapshot pool user eventId >>= expect401
      transition pool user eventId eventId 1 Planning >>= expect401
      transition pool user eventId (eventId+1000) 2 PendingApproval >>= expect401
      receipt pool eventId `shouldReturn` before
      eventCounts pool eventId `shouldReturn` [1,1,1,2]
      if label == "credential rotation" then do
        refreshed <- authenticated pool (token <> "-rotated")
        snapshot pool refreshed eventId >>= expectRight
      else pure ()

  it "fails closed without a witness and binds both the copied actor and token owner" $ do
    (token,user) <- seedSession pool 106
    snapshot pool (user { auSessionWitness = Nothing }) 106 >>= expect401
    taskSnapshot pool (user { auSessionWitness = Nothing }) 106 >>= expect401
    revisionedTaskSnapshot pool (user { auSessionWitness = Nothing }) 106 >>= expect401
    snapshot pool (user { auPartyId = toSqlKey 3 }) 106 >>= expect401
    taskSnapshot pool (user { auPartyId = toSqlKey 3 }) 106 >>= expect401
    revisionedTaskSnapshot pool (user { auPartyId = toSqlKey 3 }) 106 >>= expect401
    execute pool "UPDATE api_token SET party_id=3 WHERE token=?" [PersistText token]
    snapshot pool (user { auPartyId = toSqlKey 3 }) 106 >>= expect401
    eventCounts pool 106 `shouldReturn` [0,0,0,1]

  it "redacts credentials and fingerprints when showing the session witness" $ do
    (token,user) <- seedSession pool 107
    fmap show (auSessionWitness user) `shouldBe` Just "<authenticated-session>"
    show user `shouldSatisfy` (not . isInfixOf (T.unpack token))
    show user `shouldSatisfy` (not . isInfixOf (show (hash (TE.encodeUtf8 token) :: Digest SHA256)))

  it "treats explicit same-credential reactivation as reauthorization, not permanent revocation" $ do
    (token,user) <- seedSession pool 108
    execute pool "UPDATE api_token SET active=FALSE WHERE token=?" [PersistText token]
    snapshot pool user 108 >>= expect401
    execute pool "UPDATE api_token SET active=TRUE WHERE token=?" [PersistText token]
    snapshot pool user 108 >>= expectRight
    eventCounts pool 108 `shouldReturn` [0,0,0,1]

  it "sanitizes a session-guard database failure and recovers after rollback of the fault" $ do
    (_,user) <- seedSession pool 109
    bracket_ (execute pool "ALTER TABLE api_token RENAME TO api_token_test_saved" [])
             (execute pool "ALTER TABLE api_token_test_saved RENAME TO api_token" []) $ do
      result <- snapshot pool user 109
      case result of
        Left failure -> do
          errHTTPCode failure `shouldBe` 503
          errBody failure `shouldBe` "{\"code\":\"event_operations_unavailable\"}"
        Right _ -> expectationFailure "Session guard database fault was ignored"
    snapshot pool user 109 >>= expectRight
    eventCounts pool 109 `shouldReturn` [0,0,0,1]

  forM_ (zip [110..] ["read","new","replay","task","revisioned","raci","raci-replay","context",
                      "complete","complete-replay"]) $ \(eventId, mode) ->
    it ("rejects real HTTP " <> mode <> " when revoked after production authentication") $ do
      (token,user) <- seedSession pool eventId
      if mode == "replay" then transition pool user eventId eventId 1 Planning >>= expectRight else pure ()
      if mode == "raci-replay" then do
        execute pool "INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id) VALUES (?,2,'responsible',1)"
          [PersistInt64 (eventId+10000)]
        let _ :<|> _ :<|> _ :<|> _ :<|> apply :<|> _ = eventOperationsServer user eventId
            revision = maybe (error "invalid fixture revision") id (parseEventTaskAggregateRevision "2")
        runHandler (runReaderT (apply (eventId+10000) (commandKey eventId)
          (EventRaciReassignmentCommand revision RaciResponsible 2 1 "test" "test"))
          (Env pool httpTestConfig)) >>= expectRight
      else pure ()
      if mode `elem` ["complete","complete-replay"] then do
        execute pool "INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id) VALUES (?,1,'accountable',1),(?,2,'responsible',1); INSERT INTO event_operation_task_policy(activity_id,requires_accountability,dependencies_gate_completion) VALUES (?,TRUE,TRUE)"
          (replicate 3 (PersistInt64 (eventId+10000)))
        if mode == "complete-replay" then do
          let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> apply = eventOperationsServer user eventId
              revision = maybe (error "invalid fixture revision") id (parseEventTaskAggregateRevision "4")
          runHandler (runReaderT (apply (eventId+10000) (commandKey eventId)
            (EventTaskCompletionCommand revision "test" "test")) (Env pool httpTestConfig)) >>= expectRight
        else pure ()
      else pure ()
      before <- eventCounts pool eventId
      taskSnapshot pool user eventId >>= expectRight
      response <- httpAfterAuthentication pool token eventId mode $
        execute pool "UPDATE api_token SET active=FALSE WHERE token=?" [PersistText token]
      statusCode (HTTP.responseStatus response) `shouldBe` 401
      HTTP.responseBody response `shouldBe` "Invalid or inactive token"
      eventCounts pool eventId `shouldReturn` before

  forM_ (zip [120..] ["READ COMMITTED","REPEATABLE READ","SERIALIZABLE"]) $ \(eventId,isolation) ->
    it ("serializes revocation first under " <> T.unpack isolation) $
      revocationFirst pool eventId isolation

  it "holds the session fence through commit while revocation waits" $
    operationFirst pool 130 False
  it "releases the session fence on cancellation/rollback without swallowing cancellation" $
    operationFirst pool 131 True

mutations :: [(String, Text)]
mutations =
  [ ("deactivation", "UPDATE api_token SET active=FALSE WHERE token=?")
  , ("deletion", "DELETE FROM api_token WHERE token=?")
  , ("credential rotation", "UPDATE api_token SET token=token || '-rotated' WHERE token=?")
  , ("party rebinding", "UPDATE api_token SET party_id=3 WHERE token=?")
  , ("reset-purpose conversion", "UPDATE api_token SET label='  PaSsWoRd-ReSeT:test-only  ' WHERE token=?")
  ]

authenticated :: ConnectionPool -> Text -> IO AuthedUser
authenticated pool token = do
  result <- runSqlPool (loadAuthedUser token) pool
  maybe (fail "Expected a real authenticated test session") pure result

snapshot :: ConnectionPool -> AuthedUser -> Int64 -> IO (Either ServerError EventOperationSnapshotDTO)
snapshot pool user eventId =
  let getSnapshot :<|> _ = eventOperationsServer user eventId
  in runHandler (runReaderT getSnapshot (Env pool httpTestConfig))

transition :: ConnectionPool -> AuthedUser -> Int64 -> Int64 -> Int64 -> EventLifecycleState
           -> IO (Either ServerError EventTransitionOutcomeDTO)
transition pool user eventId key version target =
  let _ :<|> apply :<|> _ = eventOperationsServer user eventId
  in runHandler (runReaderT (apply (commandKey key) (command version target)) (Env pool httpTestConfig))

taskSnapshot :: ConnectionPool -> AuthedUser -> Int64
             -> IO (Either ServerError (Headers '[Header "Cache-Control" Text] EventOperationTaskDTO))
taskSnapshot pool user eventId =
  let _ :<|> _ :<|> getTask :<|> _ = eventOperationsServer user eventId
  in runHandler (runReaderT (getTask (eventId + 10000)) (Env pool httpTestConfig))

revisionedTaskSnapshot :: ConnectionPool -> AuthedUser -> Int64
  -> IO (Either ServerError (Headers '[Header "Cache-Control" Text] EventOperationTaskWithRevisionDTO))
revisionedTaskSnapshot pool user eventId =
  let _ :<|> _ :<|> _ :<|> getTask :<|> _ = eventOperationsServer user eventId
  in runHandler (runReaderT (getTask (eventId + 10000)) (Env pool httpTestConfig))

commandKey :: Int64 -> UUID.UUID
commandKey n = UUID.fromWords 0 0 0 (fromIntegral n)

command :: Int64 -> EventLifecycleState -> EventTransitionCommand
command version target = EventTransitionCommand version target Nothing "session-fence-test"

expect401 :: Either ServerError a -> IO ()
expect401 result = case result of
  Left failure -> do
    errHTTPCode failure `shouldBe` 401
    errBody failure `shouldBe` "Invalid or inactive token"
  Right _ -> expectationFailure "Stale or unbound session executed an event operation"

expectRight :: Either ServerError a -> IO ()
expectRight result = case result of
  Right _ -> pure ()
  Left failure -> expectationFailure ("Expected authorized operation, status " <> show (errHTTPCode failure))

execute :: ConnectionPool -> Text -> [PersistValue] -> IO ()
execute pool sql args = runSqlPool (rawExecute sql args) pool

seedSession :: ConnectionPool -> Int64 -> IO (Text, AuthedUser)
seedSession pool eventId = do
  let token = "http-session-fence-" <> T.pack (show eventId)
  runSqlPool (do
    rawExecute "INSERT INTO api_token(token,party_id,active) VALUES (?,1,TRUE)" [PersistText token]
    rawExecute "INSERT INTO social_event(id,organizer_party_id) VALUES (?,'1')" [PersistInt64 eventId]
    rawExecute "INSERT INTO event_operation_event_state(event_id,canonical_state,version,migration_evidence) VALUES (?,'draft',1,'session fence test')" [PersistInt64 eventId]
    rawExecute "INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind) VALUES (?,1,'primary_owner')" [PersistInt64 eventId]
    rawExecute "INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES (?,?,'planned',1)"
      [PersistInt64 (eventId + 10000), PersistInt64 eventId]) pool
  user <- authenticated pool token
  pure (token,user)

scalar :: Text -> [PersistValue] -> SqlPersistT IO Int64
scalar sql args = do
  rows <- rawSql sql args
  case rows of
    [Single value] -> pure value
    _ -> fail "Expected one scalar test result"

eventCounts :: ConnectionPool -> Int64 -> IO [Int64]
eventCounts pool eventId = runSqlPool (mapM (\sql -> scalar sql [PersistInt64 eventId])
  [ "SELECT count(*) FROM event_operation_transition WHERE event_id=?"
  , "SELECT count(*) FROM event_operation_command_receipt WHERE event_id=?"
  , "SELECT count(*) FROM event_operation_audit_event WHERE event_id=?"
  , "SELECT version FROM event_operation_event_state WHERE event_id=?"
  ]) pool

receipt :: ConnectionPool -> Int64 -> IO [Single Text]
receipt pool eventId = runSqlPool
  (rawSql "SELECT response::text FROM event_operation_command_receipt WHERE event_id=?" [PersistInt64 eventId]) pool

bounded :: String -> IO a -> IO a
bounded label action = timeout 30000000 action >>= maybe (fail ("Timeout at " <> label)) pure

-- All test threads are bracketed, including assertion-failure paths and open DB transactions.
withWorker :: IO a -> (IO a -> IO () -> IO b) -> IO b
withWorker action use = bracket spawn (killThread . fst) $ \(worker,done) ->
  use (bounded "worker completion" (takeMVar done) >>= either throwIO pure) (killThread worker)
  where
    spawn = do
      done <- newEmptyMVar
      worker <- forkFinally action (putMVar done)
      pure (worker,done)

waitBlocked :: ConnectionPool -> Int64 -> Int64 -> IO ()
waitBlocked pool holder waiter = bounded "observed PostgreSQL lock wait" loop
  where
    loop = do
      rows <- runSqlPool (rawSql "SELECT ?::int = ANY(pg_blocking_pids(?::int))"
        [PersistInt64 holder,PersistInt64 waiter]) pool :: IO [Single Bool]
      if rows == [Single True] then pure () else threadDelay 20000 >> loop

revocationFirst :: ConnectionPool -> Int64 -> Text -> IO ()
revocationFirst pool eventId isolation = do
  (token,user) <- seedSession pool eventId
  snapshotReady <- newEmptyMVar
  startGuard <- newEmptyMVar
  revokeReady <- newEmptyMVar
  finishRevoke <- newEmptyMVar
  let reader = tryDatabaseAction $ runSqlPool (do
        rawExecute ("SET TRANSACTION ISOLATION LEVEL " <> isolation) []
        _ <- scalar "SELECT count(*) FROM api_token" [] -- establish even an RR snapshot
        pid <- scalar "SELECT pg_backend_pid()::bigint" []
        liftIO (putMVar snapshotReady pid >> bounded "start guard" (takeMVar startGuard))
        withCurrentAuthSession user (loadSnapshot eventId 1)) pool
      revoker = runSqlPool (do
        executeRevoke token
        pid <- scalar "SELECT pg_backend_pid()::bigint" []
        liftIO (putMVar revokeReady pid >> bounded "finish revoke" (takeMVar finishRevoke))) pool
  withWorker reader $ \waitReader _ -> do
    readerPid <- bounded "reader snapshot" (takeMVar snapshotReady)
    withWorker revoker $ \waitRevoker _ -> do
      revokerPid <- bounded "revoker lock" (takeMVar revokeReady)
      putMVar startGuard ()
      waitBlocked pool revokerPid readerPid
      putMVar finishRevoke ()
      waitRevoker
      result <- waitReader
      if isolation == "READ COMMITTED"
        then result `shouldBe` Right Nothing
        else result `shouldBe` Left TransactionConflict
  eventCounts pool eventId `shouldReturn` [0,0,0,1]

executeRevoke :: Text -> SqlPersistT IO ()
executeRevoke token = rawExecute "UPDATE api_token SET active=FALSE WHERE token=?" [PersistText token]

operationFirst :: ConnectionPool -> Int64 -> Bool -> IO ()
operationFirst pool eventId cancel = do
  (token,user) <- seedSession pool eventId
  operationReady <- newEmptyMVar
  finishOperation <- newEmptyMVar
  revokeReady <- newEmptyMVar
  let operation = runSqlPool (withCurrentAuthSession user $ do
        pid <- scalar "SELECT pg_backend_pid()::bigint" []
        liftIO (putMVar operationReady pid >> bounded "finish operation" (takeMVar finishOperation))
        loadSnapshot eventId 1) pool
      revoker = runSqlPool (do
        pid <- scalar "SELECT pg_backend_pid()::bigint" []
        liftIO (putMVar revokeReady pid)
        executeRevoke token) pool
  withWorker operation $ \waitOperation cancelOperation -> do
    operationPid <- bounded "operation lock" (takeMVar operationReady)
    withWorker revoker $ \waitRevoker _ -> do
      revokerPid <- bounded "revoker start" (takeMVar revokeReady)
      waitBlocked pool operationPid revokerPid
      if cancel then do
        cancelOperation
        result <- try waitOperation :: IO (Either SomeException (Maybe (Maybe EventOperationSnapshotDTO)))
        case result of
          Left failure -> (fromException failure :: Maybe AsyncException) `shouldBe` Just ThreadKilled
          Right _ -> expectationFailure "Cancellation was swallowed"
      else do
        putMVar finishOperation ()
        result <- waitOperation
        result `shouldSatisfy` maybe False (maybe False (const True))
      waitRevoker
  runSqlPool (withCurrentAuthSession user (loadSnapshot eventId 1)) pool `shouldReturn` Nothing
  eventCounts pool eventId `shouldReturn` [0,0,0,1]

type ProtectedEvents = AuthProtect "bearer-token" :> EventOperationsAPI

httpAfterAuthentication :: ConnectionPool -> Text -> Int64 -> String -> IO () -> IO (HTTP.Response BL.ByteString)
httpAfterAuthentication pool token eventId mode revoke = do
  authenticatedSignal <- newEmptyMVar
  continue <- newEmptyMVar
  let env = Env pool httpTestConfig
      api = Proxy :: Proxy ProtectedEvents
      ctx = Proxy :: Proxy '[AuthHandler Request AuthedUser]
      app = serveWithContext api (authContext env) $ hoistServerWithContext api ctx
        (\action -> do
          liftIO (putMVar authenticatedSignal ())
          liftIO (bounded "HTTP transaction gate" (takeMVar continue))
          runReaderT action env) eventOperationsServer
  Warp.testWithApplicationSettings (Warp.setHost "127.0.0.1" Warp.defaultSettings) (pure app) $ \port -> do
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    let readOnly = mode `elem` ["read", "task", "revisioned", "context"]
        suffix = if mode `elem` ["task", "revisioned", "context"] then "/tasks/" <> show (eventId + 10000)
                    <> (if mode == "revisioned" then "/revisioned" else if mode == "context" then "/raci/context" else "")
                 else if mode `elem` ["raci","raci-replay"] then "/tasks/" <> show (eventId + 10000) <> "/raci/reassign"
                 else if mode `elem` ["complete","complete-replay"] then "/tasks/" <> show (eventId + 10000) <> "/complete"
                 else if readOnly then "" else "/transitions"
        request = HTTP.defaultRequest
          { HTTP.host = "127.0.0.1", HTTP.port = port, HTTP.secure = False
          , HTTP.method = if readOnly then "GET" else "POST"
          , HTTP.path = BS.pack ("/event-operations/events/" <> show eventId <> suffix)
          , HTTP.requestHeaders = [("Authorization","Bearer " <> TE.encodeUtf8 token),
              ("Content-Type","application/json"),("Idempotency-Key",TE.encodeUtf8 (UUID.toText (commandKey eventId)))]
          , HTTP.requestBody = HTTP.RequestBodyLBS (if readOnly then BL.empty
              else if mode == "raci" then "{\"expectedRevision\":\"1\",\"role\":\"responsible\",\"fromPartyId\":2,\"toPartyId\":1,\"reason\":\"test\",\"correlationId\":\"test\"}"
              else if mode == "raci-replay" then "{\"expectedRevision\":\"2\",\"role\":\"responsible\",\"fromPartyId\":2,\"toPartyId\":1,\"reason\":\"test\",\"correlationId\":\"test\"}"
              else if mode `elem` ["complete","complete-replay"] then "{\"expectedRevision\":\"4\",\"reason\":\"test\",\"correlationId\":\"test\"}"
              else encode (command 1 Planning))
          , HTTP.redirectCount = 0, HTTP.responseTimeout = HTTP.responseTimeoutMicro 30000000 }
    withWorker (HTTP.httpLbs request manager) $ \waitResponse _ -> do
      (bounded "production HTTP authentication" (takeMVar authenticatedSignal) >> revoke)
        `finally` void (tryPutMVar continue ())
      waitResponse
