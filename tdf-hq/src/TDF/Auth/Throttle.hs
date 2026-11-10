{-# LANGUAGE OverloadedStrings #-}

{- | Per-account throttling of the credential routes
(AUTH-CREDENTIAL-INTAKE-001, AUTH-PASSWORD-RECOVERY-001).

Counters live in @auth_rate_limit@, keyed by scope, a SHA-256 of the
normalized identifier and the start of a fixed window. The subject is the
account identifier only: the backend has no trusted client address (no
proxy-forwarding contract), so there is no per-address counter.

  * Password login: after 'authThrottleLimit' failures for one identifier in
    a window, further attempts are refused until the window ends, whether or
    not the password is right and whether or not the account exists.
  * Signup and password-reset requests: at most 'authThrottleLimit' per
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
  , lockAuthThrottleSubject
  , readAuthThrottleCount
  , consumeAuthThrottle
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

-- | Serialize every transaction that checks and counts one subject. The
-- advisory lock is held to the end of the transaction, so concurrent attempts
-- on one identifier run one after another, whether or not a credential row
-- exists to lock.
lockAuthThrottleSubject :: AuthThrottleScope -> Text -> SqlPersistT IO ()
lockAuthThrottleSubject scope identifier = do
  _ <- rawSql "SELECT pg_advisory_xact_lock(hashtextextended(?, 0))::text"
         [toPersistValue (authThrottleSubject scope identifier)] :: SqlPersistT IO [Single Text]
  pure ()

readAuthThrottleCount :: AuthThrottleScope -> Text -> UTCTime -> SqlPersistT IO Int
readAuthThrottleCount scope identifier now = do
  rows <- rawSql
    "SELECT request_count FROM auth_rate_limit WHERE scope = ? AND subject_hash = ? AND window_started_at = ?"
    [ toPersistValue (authThrottleScopeText scope)
    , toPersistValue (authThrottleSubject scope identifier)
    , toPersistValue (authThrottleWindowStart scope now)
    ]
  pure $ case rows of
    Single count : _ -> count
    [] -> 0

-- | Count one unit in the current window and return the new total. Windows
-- older than a day are dropped on the way.
consumeAuthThrottle :: AuthThrottleScope -> Text -> UTCTime -> SqlPersistT IO Int
consumeAuthThrottle scope identifier now = do
  rawExecute "DELETE FROM auth_rate_limit WHERE window_started_at < ?"
    [toPersistValue (addUTCTime (-86400) now)]
  rows <- rawSql
    "INSERT INTO auth_rate_limit(scope, subject_hash, window_started_at, request_count, updated_at) \
    \VALUES (?, ?, ?, 1, ?) \
    \ON CONFLICT(scope, subject_hash, window_started_at) \
    \DO UPDATE SET request_count = auth_rate_limit.request_count + 1, updated_at = EXCLUDED.updated_at \
    \RETURNING request_count"
    [ toPersistValue (authThrottleScopeText scope)
    , toPersistValue (authThrottleSubject scope identifier)
    , toPersistValue (authThrottleWindowStart scope now)
    , toPersistValue now
    ]
  pure $ case rows of
    Single count : _ -> count
    [] -> 0
