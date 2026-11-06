{-# LANGUAGE OverloadedStrings #-}

-- | Request-level idempotency for session invoice generation
-- (PAY-INVOICE-ISSUE-001). @POST /invoices/{sessionId}/generate@ creates an
-- invoice and, by default, issues a legal SRI document through an external
-- portal. A retried or doubled request must never issue a second document, so
-- the handler reserves @(session, idempotencyKey)@ before doing any work.
--
-- Pure decisions ('validateIssueKey', 'issueFingerprint', 'decideIssueReplay')
-- are separate from the persistence ('reserveIssue', 'completeIssue',
-- 'releaseIssue') so both can be tested on their own.
module TDF.Invoice.SessionIssue
  ( IssueReplay (..)
  , IssueReservation (..)
  , validateIssueKey
  , issueFingerprint
  , decideIssueReplay
  , reserveIssue
  , completeIssue
  , releaseIssue
  , responseSettled
  , unresolvedSiblingAttempt
  , GuardedReservation (..)
  , reserveIssueGuarded
  , issueAdvisoryLock
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Crypto.Hash (Digest, SHA256, hash)
import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Database.Persist (Entity (..), getBy, selectList, update, (==.), (!=.), (=.))
import           Database.Persist.Sql (Single (..), SqlPersistT, rawExecute, rawSql, toPersistValue, toSqlKey)

import           TDF.DTO (GenerateSessionInvoiceReq (..))
import           TDF.Models (InvoiceId)
import qualified TDF.ModelsExtra as ME

-- | What a stored reservation means for a new request with the same key.
data IssueReplay
  = IssueFresh
    -- ^ no reservation for this key: reserve it and do the work
  | IssueReplayStored Text
    -- ^ same request, already answered: return the stored response
  | IssueInProgress
    -- ^ same request, not answered yet. The first request may still be
    -- running, or it was interrupted after the SRI portal may have issued the
    -- document; issuing again could duplicate a legal invoice.
  | IssueConflict Text
    -- ^ the key was used for a different request; the text is the public reason
  deriving (Eq, Show)

data IssueReservation
  = IssueReserved ME.SessionInvoiceIssueId
  | IssueExisting IssueReplay
  deriving (Show)

-- | The key is required whenever an SRI document will be issued; without SRI
-- it is optional and, when present, still makes the request replayable.
-- Keys are 16 to 128 visible ASCII characters, the shared storefront rule.
validateIssueKey :: Bool -> Maybe Text -> Either Text (Maybe Text)
validateIssueKey issuesSri mRawKey =
  case T.strip <$> mRawKey of
    Nothing
      | issuesSri -> Left "idempotencyKey is required when issuing an SRI invoice"
      | otherwise -> Right Nothing
    Just key
      | T.length key < 16 || T.length key > 128 ->
          Left "idempotencyKey must contain 16 to 128 characters"
      | not (T.all (\c -> c >= '!' && c <= '~') key) ->
          Left "idempotencyKey must contain visible ASCII characters only"
      | otherwise -> Right (Just key)

-- | SHA-256 over the session and the decoded request, without the key itself
-- and without the certificate password (a secret that is never stored and
-- that a retry may re-enter). Decoding first means two encodings of the same
-- request (field order, whitespace) share a fingerprint.
issueFingerprint :: Text -> GenerateSessionInvoiceReq -> Text
issueFingerprint sessionId req =
  let canonical = req
        { gsiIdempotencyKey = Nothing
        , gsiCertificatePassword = Nothing
        , gsiSriPortalVerified = Nothing
        }
      material = TE.encodeUtf8 (sessionId <> "\n" <> T.pack (show canonical))
  in T.pack (show (hash material :: Digest SHA256))

-- | Decide from the stored @(fingerprint, response)@ of the reservation.
decideIssueReplay :: Text -> Maybe (Text, Maybe Text) -> IssueReplay
decideIssueReplay _ Nothing = IssueFresh
decideIssueReplay fingerprint (Just (storedFingerprint, storedResponse))
  | storedFingerprint /= fingerprint =
      IssueConflict "idempotencyKey was already used for a different invoice request"
  | otherwise = maybe IssueInProgress IssueReplayStored storedResponse

-- | Reserve the key in one statement. A concurrent request with the same key
-- observes the winner's row instead of a unique-violation error.
reserveIssue
  :: MonadIO m
  => UTCTime -> ME.SessionId -> Text -> Text -> SqlPersistT m IssueReservation
reserveIssue now sessionKey key fingerprint = do
  inserted <- rawSql
    "INSERT INTO session_invoice_issue (session_id, idempotency_key, request_sha256, created_at)\
    \ VALUES (?,?,?,?)\
    \ ON CONFLICT (session_id, idempotency_key) DO NOTHING RETURNING id"
    [ toPersistValue sessionKey
    , toPersistValue key
    , toPersistValue fingerprint
    , toPersistValue now
    ]
  case listToMaybe inserted of
    Just (Single rawId) -> pure (IssueReserved (toSqlKey rawId))
    Nothing -> do
      existing <- getBy (ME.UniqueSessionInvoiceIssue sessionKey key)
      let stored (Entity _ row) =
            (ME.sessionInvoiceIssueRequestSha256 row, ME.sessionInvoiceIssueResponse row)
      pure (IssueExisting (decideIssueReplay fingerprint (stored <$> existing)))

-- | Record the answer to replay. Only an unanswered reservation is completed.
completeIssue
  :: MonadIO m
  => UTCTime -> ME.SessionInvoiceIssueId -> Maybe InvoiceId -> Text -> SqlPersistT m ()
completeIssue now reservation invoiceKey response =
  update reservation
    [ ME.SessionInvoiceIssueInvoiceId =. invoiceKey
    , ME.SessionInvoiceIssueResponse =. Just response
    , ME.SessionInvoiceIssueCompletedAt =. Just now
    ]

-- | Drop an unanswered reservation when the request failed before any SRI
-- document could have been issued, so the caller can retry with the same key.
releaseIssue :: MonadIO m => ME.SessionInvoiceIssueId -> SqlPersistT m ()
releaseIssue reservation =
  rawExecute
    "DELETE FROM session_invoice_issue WHERE id = ? AND completed_at IS NULL"
    [toPersistValue reservation]

-- | Whether a stored response settles its attempt: SRI was not requested
-- (@sri@ is null or absent), or SRI reported the document as issued. Anything
-- else (an SRI error, another status, or an unreadable response) is unsettled,
-- because the portal may have issued the document.
responseSettled :: Text -> Bool
responseSettled stored =
  case Aeson.decodeStrict (TE.encodeUtf8 stored) of
    Just (Aeson.Object top) -> case KeyMap.lookup "sri" top of
      Nothing -> True
      Just Aeson.Null -> True
      Just (Aeson.Object sri) ->
        KeyMap.lookup "ok" sri == Just (Aeson.Bool True)
          && KeyMap.lookup "status" sri == Just (Aeson.String "issued")
      Just _ -> False
    _ -> False

-- | Another key's attempt for the same session and request fingerprint that is
-- not settled: still unanswered, or answered with an uncertain SRI outcome.
-- Issuing under a new key while one exists could duplicate a legal invoice,
-- so the handler refuses unless staff confirmed the portal.
unresolvedSiblingAttempt
  :: MonadIO m => ME.SessionId -> Text -> Text -> SqlPersistT m Bool
unresolvedSiblingAttempt sessionKey fingerprint key = do
  siblings <- selectList
    [ ME.SessionInvoiceIssueSessionId ==. sessionKey
    , ME.SessionInvoiceIssueRequestSha256 ==. fingerprint
    , ME.SessionInvoiceIssueIdempotencyKey !=. key
    ] []
  pure (any (maybe True (not . responseSettled) . ME.sessionInvoiceIssueResponse . entityVal) siblings)

data GuardedReservation
  = SiblingUnresolved
    -- ^ another key's attempt for the same request is unsettled; refuse
  | Guarded IssueReservation
  deriving (Show)

-- | The sibling check and the reservation, run in one transaction after the
-- given lock. Without a lock shared by both requests, two concurrent requests
-- with different keys could both pass the check before either reservation is
-- committed, and both would issue. Pass 'issueAdvisoryLock' on PostgreSQL;
-- the second request then waits for the first to commit its reservation, sees
-- it as an unsettled sibling, and is refused.
reserveIssueGuarded
  :: MonadIO m
  => SqlPersistT m ()  -- ^ lock on (session, fingerprint)
  -> Bool              -- ^ refuse when an unsettled sibling exists
  -> UTCTime -> ME.SessionId -> Text -> Text
  -> SqlPersistT m GuardedReservation
reserveIssueGuarded takeLock checkSiblings now sessionKey key fingerprint = do
  takeLock
  -- An existing key is a retry: resolve it (replay, in progress or conflict)
  -- before any sibling check, so retrying a newer attempt is never refused
  -- because of an older unresolved one. Only a genuinely new key is guarded.
  existing <- getBy (ME.UniqueSessionInvoiceIssue sessionKey key)
  sibling <-
    if checkSiblings && null existing
      then unresolvedSiblingAttempt sessionKey fingerprint key
      else pure False
  if sibling
    then pure SiblingUnresolved
    else Guarded <$> reserveIssue now sessionKey key fingerprint

-- | PostgreSQL transaction-scoped advisory lock on (session, fingerprint),
-- released at commit or rollback. The lock function returns a value, so it is
-- run through the query API (the same form as the other advisory locks in
-- TDF.Server); executing it as a statement fails on PostgreSQL.
issueAdvisoryLock :: ME.SessionId -> Text -> SqlPersistT IO ()
issueAdvisoryLock sessionKey fingerprint = do
  _ <- rawSql
    "SELECT 1::bigint FROM (SELECT pg_advisory_xact_lock(hashtextextended(?, 0))) locked"
    [toPersistValue (T.pack (show sessionKey) <> ":" <> fingerprint)]
    :: SqlPersistT IO [Single Int64]
  pure ()
