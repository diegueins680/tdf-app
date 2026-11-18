{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module TDF.Server.Production (productionServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (Value(..), decodeStrict', encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool)
import Servant
import TDF.API.Production (ProductionAPI, ProductionAudio)
import TDF.Auth (AuthedUser(..), withCurrentAuthSession, withCurrentAuthorization)
import TDF.ContentStorage (readManagedFile)
import TDF.DB (Env(..))

type AppM = ReaderT Env Handler

productionServer :: (AuthedUser -> Bool) -> AuthedUser -> ServerT ProductionAPI AppM
productionServer studioAccess user = list :<|> create :<|> command :<|> audio
  where
    person = toPersistValue (auPartyId user)
    payload :: Value -> PersistValue
    payload = PersistText . TE.decodeUtf8 . BL.toStrict . encode
    transaction :: SqlPersistT IO a -> AppM a
    transaction action = do
      pool <- asks envPool
      result <- liftIO (runSqlPool (withCurrentAuthSession user action) pool)
      maybe (throwError err401) pure result
    query sql params = rawSql sql params :: SqlPersistT IO [Single Text]
    list :: AppM (Headers '[Header "Cache-Control" Text] Value)
    list = do
      value <- transaction (query "SELECT production_workspace(?)::text" [person]) >>= response
      pure (addHeader ("private, no-store" :: Text) value)
    create body = transaction (query "SELECT production_create(?,?::jsonb)::text" [person,payload body]) >>= response
    command :: UUID -> Value -> AppM Value
    command project body = do
      let run staff = query "SELECT production_command(?,?,?::uuid,?::jsonb)::text"
                        [person,PersistBool staff,toPersistValue project,payload body]
      rows <- if studioAccess user
        then do
          pool <- asks envPool
          let check current = if studioAccess current then Right () else Left err403
          checked <- liftIO (runSqlPool (withCurrentAuthorization check user (run True)) pool)
          either throwError pure checked
        else transaction (run False)
      response rows
    audio :: UUID -> UUID -> AppM ProductionAudio
    audio project version = do
      rows <- transaction (rawSql
        "SELECT u.id,u.media_type FROM production_version v JOIN production_task t ON t.id=v.task JOIN content_storage_upload u ON u.id=v.file_id WHERE t.project=?::uuid AND v.id=?::uuid AND production_visible_version(?,v.id) AND u.visibility='private' AND u.status='completed' AND u.destination='tdf'"
        [toPersistValue project,toPersistValue version,person] :: SqlPersistT IO [(Single UUID,Single Text)])
      case rows of
        [(Single file,Single mime)] -> do
          bytes <- readManagedFile file "private" mime
          pure (addHeader ("private, no-store" :: Text) (addHeader ("nosniff" :: Text) bytes))
        _ -> throwError err404

response :: [Single Text] -> AppM Value
response [Single raw] = case decodeStrict' (TE.encodeUtf8 raw) of
  Just value@(Object fields) | Just (Number code) <- KM.lookup "error" fields ->
    let failure = case code of
          400 -> err400
          403 -> err403
          404 -> err404
          409 -> err409
          _ -> err500
        message = case KM.lookup "message" fields of
          Just (String msg) -> BL.fromStrict (TE.encodeUtf8 msg)
          _ -> encode value
    in throwError failure { errBody = message }
  Just value -> pure value
  Nothing -> throwError err500
response _ = throwError err500
