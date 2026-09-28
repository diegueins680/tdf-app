{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module TDF.Interactions.Server (interactionsServer, publicInteractionsServer, searchMentions, legacyCommand) where

import Control.Exception (try)
import Control.Monad (unless, when)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (FromJSON, Value(..), Result(..), fromJSON, encode, eitherDecodeStrict')
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, toSqlKey, rawSql, runSqlPool)
import Database.PostgreSQL.Simple (SqlError(..))
import Servant
import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.DTO (PartySelectorPageDTO)
import TDF.Interactions.API
import TDF.Social.Session (SessionAccess(..), withCurrentSession)

type InteractionM m = (MonadReader Env m, MonadIO m, MonadError ServerError m)

interactionsServer :: InteractionM m => AuthedUser -> ServerT InteractionsAPI m
interactionsServer user = discussionReads (Just user) :<|> mutate :<|> destinationRead (Just user) :<|> blocksFor :<|> blockList :<|> reportInbox :<|> preferences :<|> moderation
  where
    mutate target (CommandRequest key payload) = do
      targetValue <- uuidValue target
      requestValue <- uuidValue key
      when (BL.length (encode payload) > 32768) $ throwError err413
      query (Just user) True "SELECT interaction_command(?,?::uuid,?::uuid,?::jsonb)::text"
        [actorValue (Just user),targetValue,requestValue,PersistText (TE.decodeUtf8 (BL.toStrict (encode payload)))]

    blocksFor peer = blockState :<|> setBlock
      where
        peerValue = maybe (throwError err400) (pure . PersistInt64) (readsPositive peer)
        blockState = do
          other <- peerValue
          query (Just user) False "SELECT interaction_block_state(?,?)::text" [actorValue (Just user),other]
        setBlock (BlockRequest key desired version) = do
          other <- peerValue
          request <- uuidValue key
          unless (version >= 0 && version <= fromIntegral (maxBound :: Int64)) $ throwError err400
          queryWith (Just user) (case other of PersistInt64 n -> WriteSession (Just (toSqlKey n)); _ -> WriteSession Nothing)
            "SELECT interaction_block(?,?,?,?::bigint,?::uuid)::text"
            [actorValue (Just user),other,PersistBool desired,PersistInt64 (fromIntegral version),request]
    blockList cursor size = do
      unless (maybe True (\n -> n >= 0 && n <= fromIntegral (maxBound :: Int64)) cursor) $ throwError err400
      limit <- checkedSize size
      query (Just user) False "SELECT interaction_block_list(?,?,?)::text"
        [actorValue (Just user),maybe PersistNull (PersistInt64 . fromIntegral) cursor,limit]
    reportInbox cursor size = do
      after <- maybe (pure PersistNull) uuidValue cursor
      limit <- checkedSize size
      query (Just user) False "SELECT interaction_report_inbox(?,?::uuid,?)::text" [actorValue (Just user),after,limit]
    preferences = readPreferences :<|> writePreferences
      where
        readPreferences = query (Just user) False "SELECT interaction_preferences(?)::text" [actorValue (Just user)]
        writePreferences settings = do
          when (BL.length (encode settings)>1024) $ throwError err413
          query (Just user) True "SELECT interaction_preferences(?,?::jsonb)::text"
            [actorValue (Just user),PersistText (TE.decodeUtf8 (BL.toStrict (encode settings)))]
    moderation target cursor size = do
      ident <- uuidValue target
      after <- maybe (pure PersistNull) uuidValue cursor
      limit <- checkedSize size
      query (Just user) False "SELECT interaction_moderation_page(?,?::uuid,?::uuid,?)::text"
        [actorValue (Just user),ident,after,limit]

publicInteractionsServer :: InteractionM m => ServerT PublicInteractionsAPI m
publicInteractionsServer = discussionReads Nothing :<|> destinationRead Nothing

destinationRead :: InteractionM m => Maybe AuthedUser -> Text -> Text -> m InteractionResponse
destinationRead user kind ident = do
  unless (kind `elem` ["target","comment"]) $ throwError err400
  value <- uuidValue ident
  query user False "SELECT interaction_destination(?,?,?::uuid)::text" [actorValue user,PersistText kind,value]

searchMentions :: InteractionM m => AuthedUser -> Text -> Text -> Maybe Int64 -> Maybe Int -> m PartySelectorPageDTO
searchMentions user target term cursor size = do
  ident <- uuidValue target
  limit <- checkedSize size
  unless (T.length (T.strip term) >= 2 && T.length term <= 120 && maybe True (>=0) cursor) $ throwError err400
  response <- query (Just user) False "SELECT interaction_mention_candidates(?,?::uuid,?,?,?)::text"
    [actorValue (Just user),ident,PersistText term,maybe PersistNull PersistInt64 cursor,limit]
  case fromJSON (getResponse response) of
    Success page -> pure page
    Error _ -> throwError err500

discussionReads :: InteractionM m => Maybe AuthedUser -> Text -> Text -> ServerT ReadDiscussion m
discussionReads user kind entity = summary :<|> comments :<|> context :<|> reactors
  where
    base = do
      unless (T.length kind <= 48 && T.length entity <= 128 && not (T.null entity)) $ throwError err400
      pure [actorValue user,PersistText kind,PersistText entity]
    summary = do
      args <- base
      query user False "SELECT interaction_summary(?,?,?)::text" args
    comments root cursor order size = do
      _ <- base
      rootValue <- maybe (pure PersistNull) uuidValue root
      cursorValue <- maybe (pure PersistNull) uuidValue cursor
      pageSize <- checkedSize size
      let orderCode = fromMaybe "relevant" order
      unless (orderCode `elem` ["relevant","newest","oldest"]) $ throwError err400
      query user False
        "SELECT interaction_comments_page(interaction_register(?,?,?),?,?::uuid,?,?::uuid,?)::text"
        [PersistText kind,PersistText entity,actorValue user,actorValue user,rootValue,PersistText orderCode,cursorValue,pageSize]
    context comment = do
      _ <- base
      commentValue <- uuidValue comment
      query user False
        "SELECT interaction_comment_context(interaction_register(?,?,?),?,?::uuid)::text"
        [PersistText kind,PersistText entity,actorValue user,actorValue user,commentValue]
    reactors cursor size = do
      _ <- base
      pageSize <- checkedSize size
      cursorValue <- case cursor of
        Nothing -> pure PersistNull
        Just value -> case readsPositive value of
          Just number -> pure (PersistInt64 number)
          Nothing -> throwError err400
      query user False "SELECT interaction_reactors(interaction_register(?,?,?),?,?,?)::text"
        [PersistText kind,PersistText entity,actorValue user,actorValue user,cursorValue,pageSize]

actorValue :: Maybe AuthedUser -> PersistValue
actorValue = maybe PersistNull (PersistInt64 . fromSqlKey . auPartyId)

uuidValue :: MonadError ServerError m => Text -> m PersistValue
uuidValue value = case UUID.fromText value of
  Just ident -> pure (PersistText (UUID.toText ident))
  Nothing -> throwError err400 {errBody="Invalid interaction identifier"}

readsPositive :: Text -> Maybe Int64
readsPositive raw = case reads (T.unpack raw) of
  [(number,"")] | number > 0 -> Just number
  _ -> Nothing

checkedSize :: MonadError ServerError m => Maybe Int -> m PersistValue
checkedSize requested = do
  let n = fromMaybe 20 requested
  unless (n >= 1 && n <= 50) $ throwError err400
  pure (PersistInt64 (fromIntegral n))

query :: InteractionM m => Maybe AuthedUser -> Bool -> Text -> [PersistValue] -> m InteractionResponse
query user mutation = queryWith user (if mutation then WriteSession Nothing else ReadSession)

queryWith :: InteractionM m => Maybe AuthedUser -> SessionAccess -> Text -> [PersistValue] -> m InteractionResponse
queryWith user access sql args = do
  pool <- asks envPool
  let action = do
        installed <- rawSql "SELECT to_regclass('interaction_runtime') IS NOT NULL" [] :: SqlPersistT IO [Single Bool]
        gate <- if installed == [Single True]
          then rawSql "SELECT enabled FROM interaction_runtime WHERE singleton" []
          else pure []
        if gate /= [Single True] then pure (Left err404) else do
          budget <- case (user,access) of
            (Just principal,WriteSession _) -> rawSql "SELECT interaction_consume_write_budget(?)"
              [PersistInt64 (fromSqlKey (auPartyId principal))]
            _ -> pure [Single True]
          if budget /= [Single True] then pure (Left err429) else do
            rows <- rawSql sql args :: SqlPersistT IO [Single Text]
            pure (Right rows)
      transaction = case user of
        Nothing -> action
        Just principal -> fmap (>>= id) (withCurrentSession access principal action)
  outcome <- liftIO $ try (runSqlPool transaction pool)
  rows <- case outcome of
    Left (err :: SqlError)
      | sqlState err `elem` ["40001","40P01","55P03"] -> throwError err503 {errHeaders=[("Retry-After","1"),("Cache-Control","no-store")]}
      | otherwise -> throwError err500 {errBody="Interaction could not be completed",errHeaders=[("Cache-Control","no-store")]}
    Right value -> either throwError pure value
  case rows of
    [Single body] -> case eitherDecodeStrict' (TE.encodeUtf8 body) of
      Right result@(Object value) -> case KM.lookup "error" value of
        Nothing -> pure (addHeader "no-store" result)
        Just (String code) -> throwError $ responseError code
        _ -> throwError err500
      _ -> throwError err500
    _ -> throwError err404
  where
    responseError code = (case code of
      "disabled" -> err404
      "unavailable" -> err404
      "forbidden" -> err403
      "comments_not_allowed" -> err403
      "rate_limited" -> err429
      "invalid" -> err400
      "invalid_reaction" -> err422
      "invalid_cursor" -> err400
      _ -> err409) {errBody=BL.fromStrict (TE.encodeUtf8 code),errHeaders=[("Cache-Control","no-store")]}

-- The adapter only accepts caller-validated legacy DTOs; actor identity still
-- comes from a currently locked session, never a request body.
legacyCommand :: (InteractionM m, FromJSON a) => AuthedUser -> Text -> Text -> Value -> m a
legacyCommand user kind entity payload = do
  response <- query (Just user) True "SELECT interaction_legacy_command(?,?,?,?::jsonb)::text"
    [actorValue (Just user),PersistText kind,PersistText entity,PersistText (TE.decodeUtf8 (BL.toStrict (encode payload)))]
  case fromJSON (getResponse response) of
    Success value -> pure value
    Error _ -> throwError err500
