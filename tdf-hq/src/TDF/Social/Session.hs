{-# LANGUAGE OverloadedStrings #-}
module TDF.Social.Session (SessionAccess(..), withSocialSession, withCurrentSession) where

import Data.Int (Int64)
import Database.Persist (PersistValue(..), get)
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawSql)
import Servant (ServerError, err401, err404)
import TDF.Auth (AuthedUser(..), isAuthenticatableApiTokenLabel)
import TDF.Models (PartyId, apiTokenActive, apiTokenLabel, apiTokenPartyId)

data SessionAccess = ReadSession | WriteSession (Maybe PartyId)

-- Token validation and the protected operation share a transaction. Reads hold
-- shared session locks; writes hold exclusive locks. Ordinary token UPDATE/DELETE
-- therefore cannot commit between validation and the operation. Account and
-- credential locks precede token locks, matching password-reset lock acquisition.
-- Domain policy still runs inside action; this function never grants social rights.
withSocialSession
  :: SessionAccess -> AuthedUser -> SqlPersistT IO a
  -> SqlPersistT IO (Either ServerError a)
withSocialSession access user action = do
  checked <- withCurrentSession access user $ do
    live <- rawSql "SELECT social_v2_live(?)" [PersistInt64 (fromSqlKey (auPartyId user))]
      :: SqlPersistT IO [Single Bool]
    if live == [Single True] then Right <$> action else pure (Left err404)
  pure (checked >>= id)

-- Current bearer authority only. Callers must independently enforce domain
-- eligibility; in particular this never authorizes an organization delegation.
withCurrentSession
  :: SessionAccess -> AuthedUser -> SqlPersistT IO a
  -> SqlPersistT IO (Either ServerError a)
withCurrentSession access user action = case auApiTokenId user of
  Nothing -> pure (Left err401)
  Just tokenKey -> do
    let actor = auPartyId user
        (other, lockMode) = case access of
          ReadSession -> (actor, " FOR SHARE")
          WriteSession target -> (maybe actor id target, " FOR UPDATE")
        parties = map (PersistInt64 . fromSqlKey) [actor, other]
        lockRows statement values = do
          _ <- rawSql (statement <> lockMode) values :: SqlPersistT IO [Single Int64]
          pure ()
    lockRows "SELECT id FROM party WHERE id IN (?,?) ORDER BY id" parties
    lockRows "SELECT id FROM user_credential WHERE party_id IN (?,?) ORDER BY id" parties
    lockRows "SELECT id FROM api_token WHERE id=?" [PersistInt64 (fromSqlKey tokenKey)]
    current <- get tokenKey
    case current of
      Just token
        | apiTokenActive token
        , apiTokenPartyId token == actor
        , isAuthenticatableApiTokenLabel (apiTokenLabel token) -> do
            Right <$> action
      _ -> pure (Left err401)
