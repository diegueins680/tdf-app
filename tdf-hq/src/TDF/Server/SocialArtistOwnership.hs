{-# LANGUAGE OverloadedStrings #-}

module TDF.Server.SocialArtistOwnership (duplicateArtistOwner, ensureAvailableArtistOwner) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Int (Int64)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Database.Persist (Key, selectFirst, toPersistValue, (==.), (!=.))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawSql)
import Database.Persist.SqlBackend (getRDBMS)
import Servant (ServerError, err409, err422, errBody)
import TDF.Models (Party)
import qualified TDF.Models.SocialEventsModels as Social

duplicateArtistOwner :: ServerError
duplicateArtistOwner = err409{errBody = "An artist profile already exists for this party"}

ensureAvailableArtistOwner :: Key Party -> Maybe Social.ArtistProfileId -> ExceptT ServerError (SqlPersistT IO) ()
ensureAvailableArtistOwner owner currentProfile = do
    backend <- lift getRDBMS
    let lock = if backend == "postgresql" then " FOR UPDATE" else ""
    owners <- lift (rawSql ("SELECT id FROM party WHERE id = ?" <> lock)
        [toPersistValue owner] :: SqlPersistT IO [Single Int64])
    when (null owners) $ throwError err422{errBody = "artistPartyId references an unknown party"}
    duplicate <- lift $ selectFirst
        ([Social.ArtistProfilePartyId ==. Just (T.pack (show (fromSqlKey owner)))] ++ maybe [] (\key -> [Social.ArtistProfileId !=. key]) currentProfile) []
    when (isJust duplicate) $ throwError duplicateArtistOwner
