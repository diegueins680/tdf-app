{-# LANGUAGE OverloadedStrings #-}

module TDF.EventOperations.DatabaseBoundarySpec (spec) where

import Control.Exception (AsyncException(..), throwIO, toException)
import Control.Monad (forM_)
import Data.Aeson (Value(..), encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.Persist.Sql (Single(..))
import Database.PostgreSQL.Simple (SqlError(..))
import Database.PostgreSQL.LibPQ (ExecStatus(FatalError))
import Test.Hspec
import Test.QuickCheck (choose, forAll, property)

import TDF.EventOperations.DatabaseBoundary
import TDF.EventOperations.Types

spec :: Spec
spec = describe "event operations database privacy boundary" $ do
  it "validates private editor context flags, strict pages and exact target" $ do
    let result = EventRaciEditorContextDTO 10 100 (revision "4") True True
          [EventRaciAssignmentDTO 1 RaciAccountable] [1,2,3] Nothing
        decodeContext = decodeRaciEditorContextRows 10 100 0 . pure . taskRow . toJSON
    decodeContext result `shouldBe` Right (Just result)
    forM_ [result { eccEventId=11 }, result { eccActivityId=101 }, result { eccCanManage=False },
      result { eccOperationReady=False }, result { eccEligiblePartyIds=[1,1] },
      result { eccEligiblePartyIds=[2,1] }, result { eccEligiblePartyIds=[0] },
      result { eccEligiblePartyIds=[1..101] }, result { eccNextAfterPartyId=Just 3 },
      result { eccReplaceableAssignments=replicate 2 (EventRaciAssignmentDTO 1 RaciAccountable) }]
      $ \bad -> decodeContext bad `shouldBe` Left SnapshotDecodeError
    decodeRaciEditorContextRows 10 100 2 [taskRow (toJSON result)] `shouldBe` Left SnapshotDecodeError
    let page = result { eccEligiblePartyIds=[1..100], eccNextAfterPartyId=Just 100 }
    decodeContext page `shouldBe` Right (Just page)
    decodeContext (page { eccNextAfterPartyId=Just 99 }) `shouldBe` Left SnapshotDecodeError
    let reader = result { eccCanManage=False, eccOperationReady=False,
          eccReplaceableAssignments=[], eccEligiblePartyIds=[] }
    decodeContext reader `shouldBe` Right (Just reader)
    forM_ [Null, object ["canManage" .= True], object ["error" .= ("private diagnostic" :: Text)]] $ \raw ->
      decodeRaciEditorContextRows 10 100 0 [taskRow raw] `shouldBe` Left SnapshotDecodeError
    decodeRaciEditorContextRows 10 100 0 [Single Nothing] `shouldBe` Right Nothing

  it "validates a RACI receipt against the exact request before transaction completion" $ do
    let command = raciCommand "4"
        result = raciResult "6"
    decodeRaciReassignmentRows 10 100 UUID.nil command [taskRow (toJSON result)]
      `shouldBe` Right (Right result)
    forM_ [result { eroEventId = 11 }, result { eroActivityId = 101 },
           result { eroFromPartyId = 3 }, result { eroToPartyId = 1 },
           result { eroRole = RaciAccountable }, result { eroCommandId = UUID.fromWords 1 0 0 0 },
           result { eroAggregateRevision = revision "7" }] $ \wrong ->
      decodeRaciReassignmentRows 10 100 UUID.nil command [taskRow (toJSON wrong)]
        `shouldBe` Left SnapshotDecodeError

  it "rejects null, extra, unknown-error and malformed command database receipts" $ do
    let decodeRows = decodeRaciReassignmentRows 10 100 UUID.nil (raciCommand "4")
    forM_ [[], [Single Nothing], [taskRow (toJSON (raciResult "6")), Single Nothing]] $ \rows ->
      decodeRows rows `shouldBe` Left SnapshotDecodeError
    forM_ [Null, object [], object ["error" .= ("private message" :: Text)],
           object ["error" .= ("forbidden" :: Text), "private" .= True],
           object ["aggregateRevision" .= (6 :: Int)]] $ \raw ->
      decodeRows [taskRow raw] `shouldBe` Left SnapshotDecodeError
    decodeRows [taskRow (object ["error" .= ("version_conflict" :: Text)])]
      `shouldBe` Right (Left "version_conflict")

  it "preserves exact generated BIGINT command/result revisions without floating point" $
    forAll (choose (1, 9223372036854775805 :: Integer)) $ \n ->
      let result = raciResult (T.pack (show (n+2)))
      in decodeRaciReassignmentRows 10 100 UUID.nil (raciCommand (T.pack (show n)))
        [taskRow (toJSON result)] == Right (Right result)

  it "decodes a strict revision envelope and preserves exact task target binding" $ do
    let raw = object ["task" .= taskFixture, "aggregateRevision" .= ("9223372036854775807" :: Text)]
        decoded = decodeTaskWithRevisionRows 10 100 [taskRow raw]
    fmap (fmap etrTask) decoded `shouldBe` Right (Just taskFixture)
    fmap (fmap toJSON) decoded `shouldBe` Right (Just raw)
    decodeTaskWithRevisionRows 11 100 [taskRow raw] `shouldBe` Left SnapshotDecodeError
    decodeTaskWithRevisionRows 10 101 [taskRow raw] `shouldBe` Left SnapshotDecodeError
    decodeTaskWithRevisionRows 10 100 [Single Nothing] `shouldBe` Right Nothing
    decodeTaskWithRevisionRows 10 100 [] `shouldBe` Left SnapshotDecodeError
    decodeTaskWithRevisionRows 10 100 [Single Nothing, Single Nothing] `shouldBe` Left SnapshotDecodeError

  it "rejects malformed revision envelopes and applies every nested task constraint" $ do
    let envelope task revision = object ["task" .= task, "aggregateRevision" .= revision]
        invalid = [envelope (toJSON taskFixture) v | v <- [Null, Number 1, String "01", String "9223372036854775808"]]
          <> [envelope (toJSON t) (String "1") | t <-
               [taskFixture { eotVersion = 0 }, taskFixture { eotRaci = eotRaci taskFixture <> eotRaci taskFixture },
                taskFixture { eotAccountabilityNeedsAttention = True }]]
          <> [toJSON taskFixture, Null, object ["task" .= taskFixture],
              object ["task" .= taskFixture, "aggregateRevision" .= ("1" :: Text), "secret" .= True]]
    map (decodeTaskWithRevisionRows 10 100 . pure . taskRow) invalid
      `shouldBe` replicate (length invalid) (Left SnapshotDecodeError)

  it "does not log arbitrary database messages, details, hints or unknown SQLSTATE bytes" $
    property $ \payload ->
      let bytes = BS.pack (payload :: String)
          exception = toException (SqlError bytes FatalError bytes bytes bytes)
          category = if bytes `elem` ["40001", "40P01"] then "transaction_conflict" else "unavailable"
      in fmap databaseFailureLog (classifyDatabaseFailure exception)
          == Just (object ["event" .= ("event_operations_database_error" :: String),
                           "category" .= (category :: String)])

  it "classifies only allowlisted transaction failures without echoing their payload" $ do
    let failure state = toException (SqlError state FatalError "secret query" "secret contract" "secret token")
    map (classifyDatabaseFailure . failure) ["40001", "40P01"]
      `shouldBe` replicate 2 (Just TransactionConflict)
    databaseFailureLog TransactionConflict `shouldBe`
      object ["event" .= ("event_operations_database_error" :: String),
              "category" .= ("transaction_conflict" :: String)]

  it "normalizes synchronous failures without returning their message" $ do
    result <- tryDatabaseAction (throwIO (userError "password=private") :: IO ())
    result `shouldBe` Left DatabaseUnavailable

  it "preserves asynchronous cancellation" $
    (tryDatabaseAction (throwIO ThreadKilled) :: IO (Either DatabaseFailure ()))
      `shouldThrow` (== ThreadKilled)

  it "decodes exactly one matching, versioned snapshot" $ do
    let snapshot = EventOperationSnapshotDTO 10 Planning 4 Nothing ["event.read"] []
        raw = TE.decodeUtf8 (BL.toStrict (encode snapshot))
    decodeSnapshotRows 10 [Single (Just raw)] `shouldBe` Right (Just snapshot)
    decodeSnapshotRows 11 [Single (Just raw)] `shouldBe` Left SnapshotDecodeError

  it "maps SQL NULL to no snapshot, not an empty successful object" $
    decodeSnapshotRows 10 [Single Nothing] `shouldBe` Right Nothing

  it "rejects malformed, unknown-field, invalid-version and noncanonical responses" $ do
    let invalid =
          [ "{}"
          , "private diagnostic text"
          , "{\"eventId\":10,\"canonicalState\":\"finished\",\"version\":1,\"capabilities\":[],\"availableTransitions\":[]}"
          , "{\"eventId\":10,\"canonicalState\":\"planning\",\"version\":0,\"capabilities\":[],\"availableTransitions\":[]}"
          , "{\"eventId\":10,\"canonicalState\":\"planning\",\"version\":1,\"capabilities\":[],\"availableTransitions\":[],\"secret\":\"private\"}"
          ]
    map (decodeSnapshotRows 10 . pure . Single . Just) invalid
      `shouldBe` replicate (length invalid) (Left SnapshotDecodeError)
    decodeSnapshotRows 10 [] `shouldBe` Left SnapshotDecodeError
    decodeSnapshotRows 10 [Single Nothing, Single Nothing] `shouldBe` Left SnapshotDecodeError

  it "round-trips safe generated task versions with an exact target binding" $
    forAll (choose (1, 9007199254740991)) $ \version ->
      let task = taskFixture { eotVersion = version }
      in decodeTaskRows 10 100 [taskRow (toJSON task)] == Right (Just task)

  it "maps only one SQL NULL to task absence and rejects wrong cardinality or targets" $ do
    decodeTaskRows 10 100 [Single Nothing] `shouldBe` Right Nothing
    decodeTaskRows 10 100 [] `shouldBe` Left SnapshotDecodeError
    decodeTaskRows 10 100 [Single Nothing, Single Nothing] `shouldBe` Left SnapshotDecodeError
    decodeTaskRows 11 100 [taskRow (toJSON taskFixture)] `shouldBe` Left SnapshotDecodeError
    decodeTaskRows 10 101 [taskRow (toJSON taskFixture)] `shouldBe` Left SnapshotDecodeError

  it "rejects unsafe/nonpositive task, party and policy versions without a payload exception" $ do
    let invalid = [ taskFixture { eotVersion = n } | n <- [0, -1, 9007199254740992] ]
          <> [taskFixture { eotEventId = 9007199254740992 }, taskFixture { eotActivityId = 0 }]
          <> [taskFixture { eotPolicy = Just (EventTaskPolicyDTO True True 0) }]
          <> [taskFixture { eotRaci = [EventRaciAssignmentDTO 9007199254740992 RaciResponsible] }]
    map (\task -> decodeTaskRows (eotEventId task) (eotActivityId task) [taskRow (toJSON task)]) invalid
      `shouldBe` replicate (length invalid) (Left SnapshotDecodeError)
    show SnapshotDecodeError `shouldBe` "SnapshotDecodeError"

  it "rejects duplicate RACI and inconsistent attention but accepts a real attention state" $ do
    let duplicate = taskFixture { eotRaci = eotRaci taskFixture <> eotRaci taskFixture }
        wrongAttention = taskFixture { eotAccountabilityNeedsAttention = True }
        attention = taskFixture { eotRaci = [], eotAccountabilityNeedsAttention = True }
    map (decodeTaskRows 10 100 . pure . taskRow . toJSON) [duplicate, wrongAttention]
      `shouldBe` replicate 2 (Left SnapshotDecodeError)
    decodeTaskRows 10 100 [taskRow (toJSON attention)] `shouldBe` Right (Just attention)
    let advisory = taskFixture { eotPolicy = Nothing, eotRaci = [] }
    decodeTaskRows 10 100 [taskRow (toJSON advisory)] `shouldBe` Right (Just advisory)

  it "rejects unknown, missing and null fields at every task projection level" $ do
    let base = case toJSON taskFixture of Object fields -> fields; _ -> KM.empty
        patch key value = Object (KM.insert key value base)
        invalid =
          [ Object (KM.delete "raci" base), patch "secret" (String "private")
          , patch "policy" Null, patch "status" (String "invented"), patch "version" (Number 1.5)
          , patch "policy" (object ["requiresAccountability" .= True, "version" .= (1 :: Int)])
          , patch "policy" (object ["requiresAccountability" .= True, "dependenciesGateCompletion" .= True,
                                    "version" .= (1 :: Int), "private" .= True])
          , patch "raci" (toJSON [object ["partyId" .= (1 :: Int), "role" .= ("owner" :: String)]])
          , patch "raci" (toJSON [object ["partyId" .= (1 :: Int), "role" .= ("accountable" :: String),
                                         "contact" .= ("private" :: String)]])
          , Null, String "private database diagnostic"
          ]
    map (decodeTaskRows 10 100 . pure . taskRow) invalid
      `shouldBe` replicate (length invalid) (Left SnapshotDecodeError)

taskFixture :: EventOperationTaskDTO
taskFixture = EventOperationTaskDTO 10 100 TaskPlanned 1
  (Just (EventTaskPolicyDTO True True 1))
  [EventRaciAssignmentDTO 1 RaciAccountable, EventRaciAssignmentDTO 2 RaciResponsible] False

taskRow :: Value -> Single (Maybe Text)
taskRow = Single . Just . TE.decodeUtf8 . BL.toStrict . encode

revision :: Text -> EventTaskAggregateRevision
revision raw = maybe (error "invalid test revision") id (parseEventTaskAggregateRevision raw)
raciCommand :: Text -> EventRaciReassignmentCommand
raciCommand raw = EventRaciReassignmentCommand (revision raw) RaciResponsible 2 3 "test" "test"
raciResult :: Text -> EventRaciReassignmentOutcomeDTO
raciResult raw = EventRaciReassignmentOutcomeDTO 10 100 UUID.nil RaciResponsible 2 3 (revision raw) False
