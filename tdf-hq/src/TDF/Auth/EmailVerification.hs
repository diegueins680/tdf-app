{-# LANGUAGE OverloadedStrings #-}

-- | The fact that a party proved control of its email address by completing a
-- password reset sent to it (COMMUNITY-FRIEND-SUGGESTION-001).
--
-- The only reader is the friend-suggestion eligibility query; nothing here
-- grants access. The write can never fail the authentication transaction it
-- runs in: outside PostgreSQL, before the table exists or without the insert
-- privilege it does nothing.
module TDF.Auth.EmailVerification
  ( emailVerificationWritableSql
  , recordVerifiedEmailSql
  , recordVerifiedEmail
  ) where

import           Control.Monad               (when)
import           Control.Monad.IO.Class      (MonadIO)
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Database.Persist            (PersistValue (..))
import           Database.Persist.Sql        (Single (..), SqlPersistT, rawExecute, rawSql)
import           Database.Persist.SqlBackend (getRDBMS)

-- | Whether the table exists and the current role may insert into it (no
-- parameters). Never raises.
emailVerificationWritableSql :: Text
emailVerificationWritableSql =
  "SELECT coalesce(has_table_privilege(to_regclass('party_email_verification'), 'INSERT'), false)"

-- | One parameter: the party. The first verification is kept.
recordVerifiedEmailSql :: Text
recordVerifiedEmailSql =
  "INSERT INTO party_email_verification(party_id) \
  \SELECT p.id FROM party p WHERE p.id = ? \
  \ON CONFLICT (party_id) DO NOTHING"

-- | Record that @party@ completed a password reset, in the caller's
-- transaction.
recordVerifiedEmail :: MonadIO m => Int64 -> SqlPersistT m ()
recordVerifiedEmail party = do
  backend <- T.toCaseFold <$> getRDBMS
  when (backend == "postgresql") $ do
    writable <- rawSql emailVerificationWritableSql []
    when (writable == [Single True]) $
      rawExecute recordVerifiedEmailSql [PersistInt64 party]
