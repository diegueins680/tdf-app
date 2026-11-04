{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistence of Instagram account connections under AUTHORITY-094
-- (AUTH-SOCIAL-OAUTH-001). Planning reads the current owners and decides with
-- 'TDF.Social.AccountOwnership.decideConnect'; a refused rebind fails the plan
-- before any write. Applying upserts the accounts and records one audit event
-- per binding change ("connected" or "rebound"); a reconnect by the same owner
-- refreshes the token silently and a rebound account loses its artist binding
-- and delivery designation.
--
-- Planning and application run in separate transactions, so application only
-- writes while the plan is still true: a new account is inserted only if it is
-- still absent, and an existing account is updated only while its owner is the
-- one observed during planning. Otherwise application stops and reports the
-- plan as stale, so a concurrent rebind can never be overwritten with the
-- former owner's token; the caller rolls back the transaction.
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
import           Database.Persist (Entity (..), getBy, insertUnique, insert_, (=.), (==.))
import           Database.Persist.Sql (SqlPersistT, fromSqlKey, updateWhereCount)

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

-- | Apply a plan with no refused connection; returns the account keys, or
-- 'Left' when an account changed owner or appeared since planning. On 'Left'
-- the caller must roll back: earlier accounts of the same plan were written.
applyInstagramConnections
  :: MonadIO m => UTCTime -> PartyId -> [PlannedConnection] -> SqlPersistT m (Either Text [SocialSyncAccountId])
applyInstagramConnections now caller plans = go [] plans
  where
    go acc [] = pure (Right (reverse acc))
    go acc (plan : rest) = do
      applied <- applyOne plan
      case applied of
        Just accountKey -> go (accountKey : acc) rest
        Nothing -> pure (Left ("Instagram account " <> caiExternalUserId (pcInput plan)
          <> " changed owner while connecting; retry the connection"))

    applyOne PlannedConnection{ pcInput = input, pcExisting = existing, pcDecision = decision } = do
      let tokenUpdates =
            [ SocialSyncAccountHandle =. caiHandle input
            , SocialSyncAccountAccessToken =. Just (caiAccessToken input)
            , SocialSyncAccountTokenExpiresAt =. Nothing
            , SocialSyncAccountStatus =. "connected"
            , SocialSyncAccountUpdatedAt =. Just now
            ]
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
      mKey <- case existing of
        Nothing -> insertUnique SocialSyncAccount
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
        Just (Entity accountKey observed) -> do
          -- One conditional statement: PostgreSQL re-evaluates the owner after
          -- waiting for a concurrent writer, so a committed rebind is seen.
          changed <- updateWhereCount
            [ SocialSyncAccountId ==. accountKey
            , SocialSyncAccountPartyId ==. socialSyncAccountPartyId observed ]
            (tokenUpdates <> rebindUpdates)
          pure (if changed == 1 then Just accountKey else Nothing)
      forM_ mKey $ \accountKey -> do
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
      pure mKey
