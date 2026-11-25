{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.Server.DirectoryFeed (feedImage, publishFeed, editFeed) where

import Control.Monad (forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool)
import Servant
import TDF.API.Directory (FeedImageResponse, FeedPublish(..), FeedEdit(..))
import TDF.Auth (AuthedUser(..))
import TDF.ContentStorage (readFeedRaster)
import TDF.DB (Env(..))

type AppM = ReaderT Env Handler

runDB :: SqlPersistT IO a -> AppM a
runDB action = asks envPool >>= liftIO . runSqlPool action

feedImage :: Text -> UUID -> AppM FeedImageResponse
feedImage slug file = do
  rows <- runDB $ rawSql
    "SELECT f.media_type,f.byte_size,f.sha256 FROM directory_public_profile_resolution p JOIN directory_public_feed post ON post.profile_id=p.id JOIN content_storage_upload f ON f.id=ANY(post.images) WHERE p.requested_slug=? AND f.id=? AND f.destination='tdf' AND f.visibility='private' AND f.status='completed'"
    [PersistText slug,toPersistValue file]
  case rows of
    [(Single mime, Single size, Single checksum)] -> do
      bytes <- readFeedRaster file mime size checksum
      pure (addHeader ("no-store" :: Text) ("data:" <> mime <> ";base64," <> TE.decodeUtf8 (B64.encode bytes)))
    _ -> throwError err404

publishFeed :: (AuthedUser -> UUID -> Text -> AppM ()) -> AuthedUser -> UUID -> UUID -> FeedPublish -> AppM NoContent
publishFeed requireProfileCapability user profile post FeedPublish{caption,images} = do
  requireProfileCapability user profile "edit"
  requireProfileCapability user profile "publish"
  unless (not (null images) && length images <= 12 && length images == Set.size (Set.fromList images)
          && not (T.null (T.strip caption)) && T.length caption <= 2000) (throwError err400)
  forM_ images $ \file -> do
    rows <- runDB $ rawSql "SELECT media_type,byte_size,sha256 FROM content_storage_upload WHERE id=? AND owner_party_id=? AND destination='tdf' AND visibility='private' AND status='completed'"
      [toPersistValue file,toPersistValue (auPartyId user)]
    case rows of
      [(Single mime,Single size,Single checksum)] -> void (readFeedRaster file mime size checksum)
      _ -> throwError err404
  result <- runDB $ rawSql "SELECT directory_publish_feed(?,?,?,string_to_array(?,',')::uuid[],?)"
    [toPersistValue profile,toPersistValue post,PersistText caption,PersistText (T.intercalate "," (map UUID.toText images)),toPersistValue (auPartyId user)]
  unless (result == [Single True]) (throwError err409)
  pure NoContent

editFeed :: AuthedUser -> UUID -> UUID -> FeedEdit -> AppM NoContent
editFeed user profile post FeedEdit{caption,published} = do
  unless (not (T.null (T.strip caption)) && T.length caption <= 2000) (throwError err400)
  result <- runDB $ rawSql "SELECT directory_edit_feed(?,?,?,?,?)"
    [toPersistValue profile,toPersistValue post,PersistText caption,PersistBool published,toPersistValue (auPartyId user)]
  unless (result == [Single True]) (throwError err404)
  pure NoContent
