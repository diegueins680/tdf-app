{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TDF.Social.Chat (chatPolicyServer, validateLookup, validateBody, mapChatSqlError) where

import Control.Exception (try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (FromJSON, Result(..), Value(..), eitherDecodeStrict', fromJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (GeneralCategory(..), generalCategory, isControl)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist (PersistValue(..))
import Database.PostgreSQL.Simple (SqlError(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool, toSqlKey)
import Database.Persist.SqlBackend (getRDBMS)
import Servant
import TDF.API.Chat (ChatAPI)
import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.DTO (ChatSendMessageRequest(..))
import TDF.Models (PartyId, RoleEnum(..))
import TDF.Social.Session

type ChatM = ReaderT Env Handler

-- Each route selects the additive adapter independently of the process/UI flag.
-- A pause must never restore the old reader's permission behavior. Schema removal
-- is not an authorized rollback; retain these functions and the DM write guard.
chatPolicyServer :: AuthedUser -> ServerT ChatAPI ChatM -> ServerT ChatAPI ChatM
chatPolicyServer user (oldThreads :<|> oldOpen :<|> oldMessages :<|> oldSend) =
     choose oldThreads (query ReadSession (pure Nothing) "SELECT social_v2_chat_threads(?)::text" [actor])
  :<|> (\other -> choose (oldOpen other) $ do
      validId "otherPartyId" other
      when (other == fromSqlKey (auPartyId user)) $ throwError err400
        {errBody="No puedes chatear contigo mismo"}
      query (WriteSession (Just (toSqlKey other))) (pure Nothing)
        "SELECT social_v2_chat_open(?,?,?)::text" [actor,PersistInt64 other,admin])
  :<|> (\thread limit before after -> choose (oldMessages thread limit before after) $ do
      _ <- either throwError pure (validateLookup thread before after)
      let size = fromMaybe 50 limit
      unless (size >= 1 && size <= 200) $ throwError err400 {errBody="limit must be between 1 and 200"}
      query ReadSession (pure Nothing) "SELECT social_v2_chat_messages(?,?,?,?,?)::text"
        [actor,PersistInt64 thread,maybe PersistNull PersistInt64 before,
         maybe PersistNull PersistInt64 after,PersistInt64 (fromIntegral size)])
  :<|> (\thread request -> choose (oldSend thread request) $ do
      validId "threadId" thread
      body <- either throwError pure (validateBody (csmBody request))
      query (WriteSession Nothing) (peer thread) "SELECT social_v2_chat_send(?,?,?,?)::text"
        [actor,PersistInt64 thread,PersistText body,admin])
  where
    actor = PersistInt64 (fromSqlKey (auPartyId user))
    admin = PersistBool (Admin `elem` auRoles user)
    choose :: ChatM a -> ChatM a -> ChatM a
    choose legacy repaired = do
      pool <- asks envPool
      available <- liftIO $ runSqlPool policyAvailable pool
      case available of
        Just True -> repaired
        Just False -> legacy
        Nothing -> throwError err503 {errBody="Chat temporalmente no disponible."}
    peer :: Int64 -> SqlPersistT IO (Maybe PartyId)
    peer thread = do
      rows <- rawSql
        "SELECT CASE WHEN dm_party_a=? THEN dm_party_b ELSE dm_party_a END FROM chat_thread WHERE id=? AND ? IN (dm_party_a,dm_party_b)"
        [actor,PersistInt64 thread,actor] :: SqlPersistT IO [Single Int64]
      pure $ case rows of [Single other] -> Just (toSqlKey other); _ -> Nothing
    query :: FromJSON a => SessionAccess -> SqlPersistT IO (Maybe PartyId) -> Text -> [PersistValue] -> ChatM a
    query access resolvePeer statement params = do
      pool <- asks envPool
      outcome <- liftIO $ try $ runSqlPool (do
        other <- resolvePeer
        let scoped = case (access,other) of
              (WriteSession _,Just target) -> WriteSession (Just target)
              _ -> access
        withCurrentSession scoped user
          (rawSql statement params :: SqlPersistT IO [Single Text])) pool
      case outcome of
        Left (err :: SqlError) -> throwError (mapChatSqlError err)
        Right result -> do
          rows <- either throwError pure result
          case rows of
            [Single encoded] -> decodeResult encoded
            _ -> throwError err500

policyAvailable :: SqlPersistT IO (Maybe Bool)
policyAvailable = do
  backend <- T.toCaseFold <$> getRDBMS
  if backend /= "postgresql" then pure (Just False) else do
    [Single present] <- rawSql
      "SELECT to_regprocedure('social_v2_chat_threads(bigint)') IS NOT NULL AND to_regprocedure('social_v2_chat_messages(bigint,bigint,bigint,bigint,integer)') IS NOT NULL AND to_regprocedure('social_v2_chat_open(bigint,bigint,boolean)') IS NOT NULL AND to_regprocedure('social_v2_chat_send(bigint,bigint,text,boolean)') IS NOT NULL" []
    if present then pure (Just True) else do
      [Single foundation] <- rawSql "SELECT to_regclass('social_v2_runtime') IS NOT NULL" []
      -- Do not choose legacy based on an empty pair table: a first block could
      -- commit between that check and the legacy read. Install the whole adapter
      -- before serving this application version against the social foundation.
      pure $ if foundation then Nothing else Just False

decodeResult :: FromJSON a => Text -> ChatM a
decodeResult encoded = case eitherDecodeStrict' (TE.encodeUtf8 encoded) of
  Right (Object fields) -> case KM.lookup "error" fields of
    Just (String "unavailable") -> throwError err404 {errBody=BL.fromStrict (TE.encodeUtf8 "Conversación no disponible.")}
    Just (String "forbidden") -> throwError err403
      {errBody=BL.fromStrict (TE.encodeUtf8 "Esta conversación requiere una conexión aceptada y permisos vigentes.")}
    Just (String "invalid") -> throwError err400
    Just (String "before_not_found") -> throwError err404 {errBody="beforeId not found in this thread"}
    Just (String "after_not_found") -> throwError err404 {errBody="afterId not found in this thread"}
    Just _ -> throwError err500
    Nothing -> case KM.lookup "result" fields of
      Just value -> case fromJSON value of Success parsed -> pure parsed; Error _ -> throwError err500
      Nothing -> throwError err500
  _ -> throwError err500

-- Never expose SQL details/message contents to clients. Known trigger denials use
-- the same response as a pre-check; serialization/deadlock errors aborted the write.
mapChatSqlError :: SqlError -> ServerError
mapChatSqlError err
  | sqlState err == "42501" && sqlErrorMsg err == "social_dm_not_permitted" =
      err403 {errBody=BL.fromStrict (TE.encodeUtf8 "Esta conversación requiere una conexión aceptada y permisos vigentes.")}
  | sqlState err == "40001" || sqlState err == "40P01" =
      err503 {errBody=BL.fromStrict (TE.encodeUtf8 "La conversación está ocupada. Inténtalo de nuevo."),errHeaders=[("Retry-After","1")]}
  | otherwise = err500 {errBody=BL.fromStrict (TE.encodeUtf8 "No se pudo completar la operación de chat.")}

positive :: Text -> Int64 -> Either ServerError Int64
positive field value
  | value <= 0 = Left err400 {errBody=BL.fromStrict (TE.encodeUtf8 (field <> " must be a positive integer"))}
  | otherwise = Right value
validId :: Text -> Int64 -> ChatM ()
validId field value = either throwError (const (pure ())) (positive field value)

validateLookup :: Int64 -> Maybe Int64 -> Maybe Int64 -> Either ServerError (Int64,Maybe Int64,Maybe Int64)
validateLookup thread before after = do
  t <- positive "threadId" thread
  b <- traverse (positive "beforeId") before
  a <- traverse (positive "afterId") after
  when (isJust b && isJust a) $ Left err400 {errBody="Use either beforeId or afterId"}
  pure (t,b,a)

validateBody :: Text -> Either ServerError Text
validateBody raw
  | T.null body = Left err400 {errBody="Mensaje vacío"}
  | T.length body > 5000 = Left err400 {errBody="Mensaje demasiado largo (max 5000 caracteres)"}
  | T.any unsupported body = Left err400 {errBody="message must not contain control or formatting characters"}
  | otherwise = Right body
  where
    body = T.strip raw
    unsupported c = (isControl c && c /= '\n' && c /= '\r' && c /= '\t')
      || generalCategory c `elem` [Format,LineSeparator,ParagraphSeparator]
