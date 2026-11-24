{-# LANGUAGE OverloadedStrings #-}

{- | Per-account throttling of the credential routes
(AUTH-CREDENTIAL-INTAKE-001, AUTH-PASSWORD-RECOVERY-001).

Counters live in @auth_rate_limit@, keyed by scope, a SHA-256 subject and
the start of a fixed window.

  * Password login: failed password checks are counted three ways in a
    15-minute window ('LoginCounter'): for the identifier at one client
    address (10), for the client address across identifiers (50) and for the
    identifier from every address (100). Once any of the three is used up,
    attempts it covers are refused until the window ends, whether or not the
    password is right and whether or not the account exists. Someone guessing
    from one address therefore locks the account only for that address; the
    owner, elsewhere, is locked out only after 100 failures in the window,
    which needs at least ten addresses. The client address comes from
    'TDF.Auth.ClientAddress'.
  * Signup and password-reset requests: at most 'authThrottleLimit' per e-mail
    address in a window.
-}
module TDF.Auth.Throttle
  ( AuthThrottleScope(..)
  , authThrottleScopeText
  , authThrottleLimit
  , authThrottleWindowSeconds
  , authThrottleSubject
  , authThrottleWindowStart
  , authThrottleRetryAfterSeconds
  , authThrottleLockedOut
  , authThrottleOverLimit
  , ensureAuthThrottleSchema
  , lockAuthThrottleSubject
  , readAuthThrottleCount
  , consumeAuthThrottle
  , LoginCounter(..)
  , loginCounterLimit
  , loginCounterSubject
  , loginCountersLockedOut
  , lockLoginCounter
  , readLoginCounter
  , consumeLoginCounter
  ) where

import           Crypto.Hash           (Digest, SHA256, hash)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Time             (UTCTime, addUTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Database.Persist.Sql  (Single(..), SqlPersistT, rawExecute, rawSql, toPersistValue)

data AuthThrottleScope
  = ThrottleLoginFailure
  | ThrottleSignup
  | ThrottlePasswordReset
  deriving (Eq, Show, Enum, Bounded)

authThrottleScopeText :: AuthThrottleScope -> Text
authThrottleScopeText ThrottleLoginFailure = "login-failure"
authThrottleScopeText ThrottleSignup = "signup"
authThrottleScopeText ThrottlePasswordReset = "password-reset"

authThrottleLimit :: AuthThrottleScope -> Int
authThrottleLimit ThrottleLoginFailure = 10
authThrottleLimit ThrottleSignup = 5
authThrottleLimit ThrottlePasswordReset = 5

authThrottleWindowSeconds :: AuthThrottleScope -> Integer
authThrottleWindowSeconds ThrottleLoginFailure = 15 * 60
authThrottleWindowSeconds ThrottleSignup = 60 * 60
authThrottleWindowSeconds ThrottlePasswordReset = 60 * 60

-- | Case and surrounding whitespace do not make a different subject.
authThrottleSubject :: AuthThrottleScope -> Text -> Text
authThrottleSubject scope identifier =
  T.pack . show $
    (hash (TE.encodeUtf8 ("auth-rate-limit:" <> authThrottleScopeText scope <> ":" <> T.toLower (T.strip identifier))) :: Digest SHA256)

authThrottleWindowStart :: AuthThrottleScope -> UTCTime -> UTCTime
authThrottleWindowStart scope now =
  let window = authThrottleWindowSeconds scope
      seconds = floor (utcTimeToPOSIXSeconds now) :: Integer
  in posixSecondsToUTCTime (fromInteger (seconds - seconds `mod` window))

-- | Whole seconds until the current window ends; never less than one.
authThrottleRetryAfterSeconds :: AuthThrottleScope -> UTCTime -> Integer
authThrottleRetryAfterSeconds scope now =
  let windowEnd = addUTCTime (fromInteger (authThrottleWindowSeconds scope)) (authThrottleWindowStart scope now)
  in max 1 (ceiling (utcTimeToPOSIXSeconds windowEnd - utcTimeToPOSIXSeconds now))

-- | A counter read before the attempt: the limit is already used up.
authThrottleLockedOut :: AuthThrottleScope -> Int -> Bool
authThrottleLockedOut scope recorded = recorded >= authThrottleLimit scope

-- | A counter returned by consuming one unit: this request went past the limit.
authThrottleOverLimit :: AuthThrottleScope -> Int -> Bool
authThrottleOverLimit scope consumed = consumed > authThrottleLimit scope

-- | The three login-failure counters. All are stored under the
-- @login-failure@ scope; their subjects are hashed with distinct prefixes.
data LoginCounter
  = LoginIdentifierAtAddress
  | LoginAddress
  | LoginIdentifier
  deriving (Eq, Show, Enum, Bounded)

loginCounterLimit :: LoginCounter -> Int
loginCounterLimit LoginIdentifierAtAddress = 10
loginCounterLimit LoginAddress = 50
loginCounterLimit LoginIdentifier = 100

-- | The identifier counter keeps the subject of the earlier per-identifier
-- lockout, so counts recorded before this version still apply.
loginCounterSubject :: LoginCounter -> Text -> Text -> Text
loginCounterSubject counter identifier address = case counter of
  LoginIdentifier -> authThrottleSubject ThrottleLoginFailure identifier
  LoginIdentifierAtAddress ->
    digest ("auth-rate-limit:login-failure-at-address:" <> address <> "\n" <> normalized)
  LoginAddress -> digest ("auth-rate-limit:login-failure-address:" <> address)
  where
    normalized = T.toLower (T.strip identifier)
    digest value = T.pack (show (hash (TE.encodeUtf8 value) :: Digest SHA256))

-- | Counts read before the attempt: one of them is already used up.
loginCountersLockedOut :: [(LoginCounter, Int)] -> Bool
loginCountersLockedOut = any (\(counter, recorded) -> recorded >= loginCounterLimit counter)

-- | Create the counter table when the backend migrates its own schema at
-- startup (development, test fixtures). The statements are the ones of
-- @sql/2026-11-10_auth_rate_limit.sql@ and are idempotent, so a database that
-- already received that migration is left as it is.
ensureAuthThrottleSchema :: SqlPersistT IO ()
ensureAuthThrottleSchema = do
  rawExecute
    "CREATE TABLE IF NOT EXISTS auth_rate_limit (\
    \  scope TEXT NOT NULL CHECK (scope IN ('login-failure', 'signup', 'password-reset')),\
    \  subject_hash TEXT NOT NULL CHECK (subject_hash ~ '^[0-9a-f]{64}$'),\
    \  window_started_at TIMESTAMPTZ NOT NULL,\
    \  request_count INT NOT NULL CHECK (request_count > 0),\
    \  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \  PRIMARY KEY (scope, subject_hash, window_started_at)\
    \)"
    []
  rawExecute
    "CREATE INDEX IF NOT EXISTS auth_rate_limit_window_idx ON auth_rate_limit (window_started_at)"
    []

-- | Serialize every transaction that checks and counts one subject. The
-- advisory lock is held to the end of the transaction, so concurrent attempts
-- on one identifier run one after another, whether or not a credential row
-- exists to lock.
lockAuthThrottleSubject :: AuthThrottleScope -> Text -> SqlPersistT IO ()
lockAuthThrottleSubject scope identifier = lockSubjectHash (authThrottleSubject scope identifier)

-- | Login takes the three locks in the order of 'LoginCounter' (pair,
-- address, identifier). Every login takes exactly one lock of each kind in
-- that order, so two logins cannot wait for each other in a cycle.
lockLoginCounter :: LoginCounter -> Text -> Text -> SqlPersistT IO ()
lockLoginCounter counter identifier address = lockSubjectHash (loginCounterSubject counter identifier address)

readLoginCounter :: LoginCounter -> Text -> Text -> UTCTime -> SqlPersistT IO Int
readLoginCounter counter identifier address =
  readSubjectHash ThrottleLoginFailure (loginCounterSubject counter identifier address)

consumeLoginCounter :: LoginCounter -> Text -> Text -> UTCTime -> SqlPersistT IO Int
consumeLoginCounter counter identifier address =
  consumeSubjectHash ThrottleLoginFailure (loginCounterSubject counter identifier address)

lockSubjectHash :: Text -> SqlPersistT IO ()
lockSubjectHash subject = do
  _ <- rawSql "SELECT pg_advisory_xact_lock(hashtextextended(?, 0))::text"
         [toPersistValue subject] :: SqlPersistT IO [Single Text]
  pure ()

readAuthThrottleCount :: AuthThrottleScope -> Text -> UTCTime -> SqlPersistT IO Int
readAuthThrottleCount scope identifier = readSubjectHash scope (authThrottleSubject scope identifier)

readSubjectHash :: AuthThrottleScope -> Text -> UTCTime -> SqlPersistT IO Int
readSubjectHash scope subject now = do
  rows <- rawSql
    "SELECT request_count FROM auth_rate_limit WHERE scope = ? AND subject_hash = ? AND window_started_at = ?"
    [ toPersistValue (authThrottleScopeText scope)
    , toPersistValue subject
    , toPersistValue (authThrottleWindowStart scope now)
    ]
  pure $ case rows of
    Single count : _ -> count
    [] -> 0

-- | Count one unit in the current window and return the new total. Windows
-- older than a day are dropped on the way.
consumeAuthThrottle :: AuthThrottleScope -> Text -> UTCTime -> SqlPersistT IO Int
consumeAuthThrottle scope identifier = consumeSubjectHash scope (authThrottleSubject scope identifier)

consumeSubjectHash :: AuthThrottleScope -> Text -> UTCTime -> SqlPersistT IO Int
consumeSubjectHash scope subject now = do
  rawExecute "DELETE FROM auth_rate_limit WHERE window_started_at < ?"
    [toPersistValue (addUTCTime (-86400) now)]
  rows <- rawSql
    "INSERT INTO auth_rate_limit(scope, subject_hash, window_started_at, request_count, updated_at) \
    \VALUES (?, ?, ?, 1, ?) \
    \ON CONFLICT(scope, subject_hash, window_started_at) \
    \DO UPDATE SET request_count = auth_rate_limit.request_count + 1, updated_at = EXCLUDED.updated_at \
    \RETURNING request_count"
    [ toPersistValue (authThrottleScopeText scope)
    , toPersistValue subject
    , toPersistValue (authThrottleWindowStart scope now)
    , toPersistValue now
    ]
  pure $ case rows of
    Single count : _ -> count
    [] -> 0
