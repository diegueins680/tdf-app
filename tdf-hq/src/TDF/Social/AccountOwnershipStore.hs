{-# LANGUAGE OverloadedStrings #-}

-- | Persistence of Instagram account connections under AUTHORITY-094
-- (AUTH-SOCIAL-OAUTH-001). Planning reads the current owners and decides with
-- 'TDF.Social.AccountOwnership.decideConnect'; a refused rebind fails the plan
-- before any write. Applying upserts the accounts and records one audit event
-- per binding change ("connected" or "rebound"); a reconnect by the same owner
-- refreshes the token silently and a rebound account loses its artist binding
-- and delivery designation.
module TDF.Social.AccountOwnershipStore
  ( ConnectedAccountInput (..)
  , PlannedConnection (..)
  , planInstagramConnections
  , applyInstagramConnections
  ) where

import           Control.Monad (forM, forM_)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Database.Persist (Entity (..), getBy, insert_, upsert, (=.))
import           Database.Persist.Sql (SqlPersistT, fromSqlKey)

import           TDF.Models
import           TDF.Social.AccountOwnership (RebindDecision (..), decideConnect)

data ConnectedAccountInput = ConnectedAccountInput
  { caiExternalUserId :: !Text
  , caiHandle         :: !(Maybe Text)
  , caiAccessToken    :: !Text
  } deriving (Eq, Show)

data PlannedConnection = PlannedConnection
  { pcInput    :: !ConnectedAccountInput
  , pcExisting :: !(Maybe (Entity SocialSyncAccount))
  , pcDecision :: !RebindDecision
  } deriving (Show)

-- | Decide every connection, or name the first account that another party
-- connected and the caller may not rebind.
planInstagramConnections
  :: MonadIO m => Bool -> PartyId -> [ConnectedAccountInput] -> SqlPersistT m (Either Text [PlannedConnection])
planInstagramConnections isAdmin caller inputs = do
  plans <- forM inputs $ \input -> do
    existing <- getBy (UniqueSocialSyncAccount "instagram" (caiExternalUserId input))
    let owner = fmap (fmap fromSqlKey . socialSyncAccountPartyId . entityVal) existing
    pure PlannedConnection
      { pcInput = input
      , pcExisting = existing
      , pcDecision = decideConnect isAdmin (fromSqlKey caller) owner
      }
  pure $ case [ caiExternalUserId (pcInput plan) | plan <- plans, pcDecision plan == RebindRefused ] of
    refused : _ -> Left ("Instagram account " <> refused <> " is connected by another user; an administrator must rebind it")
    [] -> Right plans

-- | Apply a plan with no refused connection; returns the account keys.
applyInstagramConnections :: MonadIO m => UTCTime -> PartyId -> [PlannedConnection] -> SqlPersistT m [SocialSyncAccountId]
applyInstagramConnections now caller plans = forM plans $ \PlannedConnection{ pcInput = input, pcExisting = existing, pcDecision = decision } -> do
  let record = SocialSyncAccount
        { socialSyncAccountPartyId = Just caller
        , socialSyncAccountArtistProfileId = Nothing
        , socialSyncAccountPlatform = "instagram"
        , socialSyncAccountExternalUserId = caiExternalUserId input
        , socialSyncAccountHandle = caiHandle input
        , socialSyncAccountAccessToken = Just (caiAccessToken input)
        , socialSyncAccountTokenExpiresAt = Nothing
        , socialSyncAccountStatus = "connected"
        , socialSyncAccountLastSyncedAt = Nothing
        , socialSyncAccountCreatedAt = now
        , socialSyncAccountUpdatedAt = Just now
        , socialSyncAccountDeliveryDesignated = False
        , socialSyncAccountConnectedByPartyId = Just caller
        , socialSyncAccountDesignatedByPartyId = Nothing
        , socialSyncAccountDesignatedAt = Nothing
        }
      rebindUpdates = case decision of
        RebindAudited _ ->
          [ SocialSyncAccountPartyId =. Just caller
          , SocialSyncAccountConnectedByPartyId =. Just caller
          -- A rebound account loses its artist binding and delivery designation.
          , SocialSyncAccountArtistProfileId =. Nothing
          , SocialSyncAccountDeliveryDesignated =. False
          , SocialSyncAccountDesignatedByPartyId =. Nothing
          , SocialSyncAccountDesignatedAt =. Nothing ]
        _ -> []
  Entity accountKey _ <- upsert record $
    [ SocialSyncAccountHandle =. caiHandle input
    , SocialSyncAccountAccessToken =. Just (caiAccessToken input)
    , SocialSyncAccountTokenExpiresAt =. Nothing
    , SocialSyncAccountStatus =. "connected"
    , SocialSyncAccountUpdatedAt =. Just now
    ] ++ rebindUpdates
  let formerOwner = existing >>= socialSyncAccountPartyId . entityVal
      eventType = case decision of
        ConnectNew -> Just "connected"
        RebindAudited _ -> Just "rebound"
        _ -> Nothing
  forM_ eventType $ \kind ->
    insert_ SocialSyncAccountEvent
      { socialSyncAccountEventAccountId = accountKey
      , socialSyncAccountEventActorPartyId = Just caller
      , socialSyncAccountEventEventType = kind
      , socialSyncAccountEventFromPartyId = formerOwner
      , socialSyncAccountEventToPartyId = Just caller
      , socialSyncAccountEventCreatedAt = now
      }
  pure accountKey
