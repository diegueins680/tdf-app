{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TDF.Social.Profiles (profileList, profileGet, validateProfileIds) where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool)
import Database.Persist.SqlBackend (getRDBMS)
import Database.PostgreSQL.Simple (SqlError(..))
import Servant
import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.DTO (SocialPartyProfileDTO(..))
import TDF.Social.Session (SessionAccess(..), withCurrentSession)

type ProfileM = ReaderT Env Handler

profileList :: AuthedUser -> ([Int64] -> ProfileM [SocialPartyProfileDTO])
  -> [Int64] -> ProfileM [SocialPartyProfileDTO]
profileList user legacy ids = do
  checked <- either throwError pure (validateProfileIds ids)
  choose (legacy checked) (readProfiles user checked)

profileGet :: AuthedUser -> (Int64 -> ProfileM SocialPartyProfileDTO)
  -> Int64 -> ProfileM SocialPartyProfileDTO
profileGet user legacy target = do
  when (target <= 0) $ throwError err400 {errBody="Invalid party id"}
  choose (legacy target) $ do
    profiles <- readProfiles user [target]
    case profiles of
      [profile] -> pure profile
      _ -> throwError err404

-- No foundation: preserve legacy installations. Partial foundation: fail closed;
-- choosing legacy on an empty pair table races the first committed block.
choose :: ProfileM a -> ProfileM a -> ProfileM a
choose legacy repaired = do
  pool <- asks envPool
  available <- liftIO $ runSqlPool availability pool
  case available of
    Just True -> repaired
    Just False -> legacy
    Nothing -> throwError err503 {errBody="Perfiles temporalmente no disponibles."}

availability :: SqlPersistT IO (Maybe Bool)
availability = do
  backend <- T.toCaseFold <$> getRDBMS
  if backend /= "postgresql" then pure (Just False) else do
    [Single present] <- rawSql
      "SELECT to_regprocedure('social_v2_profiles(bigint,bigint[])') IS NOT NULL AND to_regprocedure('social_v2_profile_eligible(bigint,bigint[])') IS NOT NULL" []
    if present then pure (Just True) else do
      [Single foundation] <- rawSql "SELECT to_regclass('social_v2_runtime') IS NOT NULL" []
      pure $ if foundation then Nothing else Just False

type ProfileRow = (Single Int64,Single Text,Single (Maybe Text),
                   Single (Maybe Text),Single (Maybe Text),Single (Maybe Text))

readProfiles :: AuthedUser -> [Int64] -> ProfileM [SocialPartyProfileDTO]
readProfiles user ids = do
  pool <- asks envPool
  outcome <- liftIO $ try $ runSqlPool (withCurrentSession ReadSession user $
    rawSql
      "SELECT * FROM social_v2_profiles(?, ARRAY(SELECT value::bigint FROM jsonb_array_elements_text(?::jsonb)))"
      [PersistInt64 (fromSqlKey (auPartyId user)),
       PersistText (TE.decodeUtf8 (BL.toStrict (encode ids)))] :: SqlPersistT IO (Either ServerError [ProfileRow])) pool
  case outcome of
    Left (err :: SqlError)
      | sqlState err `elem` ["40001","40P01"] -> throwError err503 {errHeaders=[("Retry-After","1")]}
      | otherwise -> throwError err500
    Right result -> map toProfile <$> either throwError pure result
  where
    -- Match the legacy Unicode trimming and fallback exactly, without changing
    -- avatar, bio or city values. SQL has already authorized every returned row.
    clean = (>>= (\t -> let trimmed=T.strip t in if T.null trimmed then Nothing else Just trimmed))
    toProfile (Single ident,Single name,Single preferred,Single avatar,Single bio,Single city) =
      SocialPartyProfileDTO ident (fromMaybe name (clean preferred <|> clean (Just name))) avatar bio city

validateProfileIds :: [Int64] -> Either ServerError [Int64]
validateProfileIds ids
  | any (<=0) ids = Left err400 {errBody="partyId query must contain only positive integers"}
  | length ids > 100 = Left err400 {errBody="partyId query supports at most 100 ids"}
  | length ids /= length (nub ids) = Left err400 {errBody="partyId query must not contain duplicate ids"}
  | otherwise = Right ids
