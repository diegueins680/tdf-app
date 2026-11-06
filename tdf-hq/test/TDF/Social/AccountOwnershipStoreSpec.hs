{-# LANGUAGE OverloadedStrings #-}

-- | AUTH-SOCIAL-OAUTH-001 (AUTHORITY-094): Instagram connection persistence
-- against a real (SQLite) database: ownership, refused rebinds, audited
-- rebinds and the designation reset.
module TDF.Social.AccountOwnershipStoreSpec (spec) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Time (UTCTime (..), fromGregorian)
import           Database.Persist (Entity (..), getBy, selectList, update, (=.))
import           Database.Persist.Sql (SqlPersistT, rawExecute, toSqlKey)
import           Database.Persist.Sqlite (runSqlite)
import           Test.Hspec

import           TDF.Models
import           TDF.Social.AccountOwnership (RebindDecision (..))
import           TDF.Social.AccountOwnershipStore

type Db = SqlPersistT (NoLoggingT (ResourceT IO))

schema :: Db ()
schema = do
  rawExecute
    "CREATE TABLE \"social_sync_account\" (\
    \\"id\" INTEGER PRIMARY KEY, \"party_id\" INTEGER NULL, \"artist_profile_id\" INTEGER NULL,\
    \\"platform\" VARCHAR NOT NULL, \"external_user_id\" VARCHAR NOT NULL, \"handle\" VARCHAR NULL,\
    \\"access_token\" VARCHAR NULL, \"token_expires_at\" TIMESTAMP NULL, \"status\" VARCHAR NOT NULL,\
    \\"last_synced_at\" TIMESTAMP NULL, \"created_at\" TIMESTAMP NOT NULL, \"updated_at\" TIMESTAMP NULL,\
    \\"delivery_designated\" BOOLEAN NOT NULL DEFAULT 0, \"connected_by_party_id\" INTEGER NULL,\
    \\"designated_by_party_id\" INTEGER NULL, \"designated_at\" TIMESTAMP NULL,\
    \CONSTRAINT \"unique_social_sync_account\" UNIQUE (\"platform\", \"external_user_id\"))" []
  rawExecute
    "CREATE TABLE \"social_sync_account_event\" (\
    \\"id\" INTEGER PRIMARY KEY, \"account_id\" INTEGER NOT NULL, \"actor_party_id\" INTEGER NULL,\
    \\"event_type\" VARCHAR NOT NULL, \"from_party_id\" INTEGER NULL, \"to_party_id\" INTEGER NULL,\
    \\"created_at\" TIMESTAMP NOT NULL)" []

t0, t1 :: UTCTime
t0 = UTCTime (fromGregorian 2026 11 2) 0
t1 = UTCTime (fromGregorian 2026 11 2) 3600

alice, bob, admin :: PartyId
alice = toSqlKey 1
bob = toSqlKey 2
admin = toSqlKey 9

input :: ConnectedAccountInput
input = ConnectedAccountInput { caiExternalUserId = "ig-100", caiHandle = Just "tdf", caiAccessToken = "token-a" }

connect :: Bool -> PartyId -> UTCTime -> ConnectedAccountInput -> Db (Either String [RebindDecision])
connect isAdmin caller at account = do
  planned <- planInstagramConnections isAdmin caller [account]
  case planned of
    Left message -> pure (Left (show message))
    Right plan -> do
      applied <- applyInstagramConnections at caller plan
      pure (either (Left . show) (const (Right (map pcDecision plan))) applied)

account :: Db SocialSyncAccount
account = do
  found <- getBy (UniqueSocialSyncAccount "instagram" "ig-100")
  maybe (fail "account missing") (pure . entityVal) found

events :: Db [SocialSyncAccountEvent]
events = map entityVal <$> selectList [] []

withDb :: Db a -> IO a
withDb action = runSqlite ":memory:" (schema >> action)

spec :: Spec
spec = describe "Instagram connection persistence (AUTH-SOCIAL-OAUTH-001, database)" $ do
  it "a first connection belongs to the connecting party, is not designated, and is audited once" $ withDb $ do
    decisions <- connect False alice t0 input
    liftIO (decisions `shouldBe` Right [ConnectNew])
    stored <- account
    liftIO $ do
      socialSyncAccountPartyId stored `shouldBe` Just alice
      socialSyncAccountConnectedByPartyId stored `shouldBe` Just alice
      socialSyncAccountDeliveryDesignated stored `shouldBe` False
      socialSyncAccountAccessToken stored `shouldBe` Just "token-a"
    recorded <- events
    liftIO $ map socialSyncAccountEventEventType recorded `shouldBe` ["connected"]
    liftIO $ map socialSyncAccountEventFromPartyId recorded `shouldBe` [Nothing]

  it "the owner reconnecting refreshes the token and keeps designation without a new event" $ withDb $ do
    _ <- connect False alice t0 input
    Just (Entity key _) <- getBy (UniqueSocialSyncAccount "instagram" "ig-100")
    update key [SocialSyncAccountDeliveryDesignated =. True, SocialSyncAccountDesignatedByPartyId =. Just admin, SocialSyncAccountDesignatedAt =. Just t0]
    decisions <- connect False alice t1 input { caiAccessToken = "token-b", caiHandle = Just "tdf-new" }
    liftIO (decisions `shouldBe` Right [KeepOwner])
    stored <- account
    liftIO $ do
      socialSyncAccountAccessToken stored `shouldBe` Just "token-b"
      socialSyncAccountHandle stored `shouldBe` Just "tdf-new"
      socialSyncAccountDeliveryDesignated stored `shouldBe` True
      socialSyncAccountPartyId stored `shouldBe` Just alice
    recorded <- events
    liftIO (length recorded `shouldBe` 1)

  it "another non-admin party is refused before any row changes" $ withDb $ do
    _ <- connect False alice t0 input
    outcome <- connect False bob t1 input { caiAccessToken = "token-bob" }
    liftIO (either (const True) (const False) outcome `shouldBe` True)
    stored <- account
    liftIO $ do
      socialSyncAccountPartyId stored `shouldBe` Just alice
      socialSyncAccountAccessToken stored `shouldBe` Just "token-a"
    recorded <- events
    liftIO (length recorded `shouldBe` 1)

  it "refuses a stale reconnect plan after a concurrent admin rebind and keeps the new owner's token" $ withDb $ do
    _ <- connect False alice t0 input
    -- Alice plans a token refresh while she still owns the account ...
    stalePlan <- planInstagramConnections False alice [input { caiAccessToken = "token-alice-late" }]
    -- ... an administrator rebinds it to Bob before Alice's plan is applied ...
    _ <- connect True bob t1 input { caiAccessToken = "token-bob" }
    -- ... so applying Alice's plan must not write her token over Bob's account.
    applied <- either (fail . show) (applyInstagramConnections t1 alice) stalePlan
    stored <- account
    eventCount <- length <$> events
    liftIO $ do
      either (const True) (const False) applied `shouldBe` True
      socialSyncAccountPartyId stored `shouldBe` Just bob
      socialSyncAccountAccessToken stored `shouldBe` Just "token-bob"
      eventCount `shouldBe` 2

  it "refuses a stale new-account plan when someone else connected the account first" $ withDb $ do
    stalePlan <- planInstagramConnections False bob [input { caiAccessToken = "token-bob" }]
    _ <- connect False alice t0 input
    applied <- either (fail . show) (applyInstagramConnections t1 bob) stalePlan
    stored <- account
    liftIO $ do
      either (const True) (const False) applied `shouldBe` True
      socialSyncAccountPartyId stored `shouldBe` Just alice
      socialSyncAccountAccessToken stored `shouldBe` Just "token-a"

  it "an administrator rebinds with an audit event and the account loses artist binding and designation" $ withDb $ do
    _ <- connect False alice t0 input
    Just (Entity key _) <- getBy (UniqueSocialSyncAccount "instagram" "ig-100")
    update key [ SocialSyncAccountDeliveryDesignated =. True, SocialSyncAccountDesignatedByPartyId =. Just admin
               , SocialSyncAccountDesignatedAt =. Just t0, SocialSyncAccountArtistProfileId =. Just (toSqlKey 5) ]
    decisions <- connect True admin t1 input { caiAccessToken = "token-admin" }
    liftIO (decisions `shouldBe` Right [RebindAudited 1])
    stored <- account
    liftIO $ do
      socialSyncAccountPartyId stored `shouldBe` Just admin
      socialSyncAccountConnectedByPartyId stored `shouldBe` Just admin
      socialSyncAccountArtistProfileId stored `shouldBe` Nothing
      socialSyncAccountDeliveryDesignated stored `shouldBe` False
      socialSyncAccountDesignatedByPartyId stored `shouldBe` Nothing
      socialSyncAccountAccessToken stored `shouldBe` Just "token-admin"
    recorded <- events
    liftIO $ map socialSyncAccountEventEventType recorded `shouldBe` ["connected", "rebound"]
    liftIO $ map socialSyncAccountEventFromPartyId recorded `shouldBe` [Nothing, Just alice]
    liftIO $ map socialSyncAccountEventToPartyId recorded `shouldBe` [Just alice, Just admin]
