{-# LANGUAGE OverloadedStrings #-}

-- | PAY-TIP-001 keyed tip reservation against a real (SQLite) database.
module TDF.Commerce.ArtistTipStoreSpec (spec) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Text (Text)
import           Data.Time (UTCTime (..), fromGregorian)
import           Database.Persist (Entity (..), selectList, update, (=.))
import           Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, toSqlKey)
import           Database.Persist.Sqlite (runSqlite)
import           Test.Hspec

import           TDF.Commerce.ArtistTip (tipRequestFingerprint)
import           TDF.Commerce.ArtistTipStore
import           TDF.Models (ArtistProfileId)
import qualified TDF.ModelsExtra as ME

type Db = SqlPersistT (NoLoggingT (ResourceT IO))

schema :: Db ()
schema = rawExecute
  "CREATE TABLE \"artist_tip\" (\
  \\"id\" INTEGER PRIMARY KEY, \"artist_profile_id\" INTEGER NOT NULL, \"tipper_party_id\" INTEGER NULL,\
  \\"tipper_email\" VARCHAR NULL, \"tipper_name\" VARCHAR NULL, \"amount_cents\" INTEGER NOT NULL,\
  \\"currency\" VARCHAR NOT NULL, \"platform_fee_cents\" INTEGER NOT NULL,\
  \\"stripe_payment_intent_id\" VARCHAR NULL, \"status\" VARCHAR NOT NULL DEFAULT 'pending',\
  \\"message\" VARCHAR NULL, \"idempotency_key\" VARCHAR NULL, \"request_sha256\" VARCHAR NULL,\
  \\"created_at\" TIMESTAMP NOT NULL, \"updated_at\" TIMESTAMP NOT NULL,\
  \CONSTRAINT \"unique_artist_tip_stripe_payment_intent\" UNIQUE (\"stripe_payment_intent_id\"),\
  \CONSTRAINT \"unique_artist_tip_idempotency\" UNIQUE (\"artist_profile_id\", \"idempotency_key\"))" []

now :: UTCTime
now = UTCTime (fromGregorian 2026 11 2) 0

fingerprintFor :: Int -> Text -> Maybe Text -> Text
fingerprintFor amount currency message = tipRequestFingerprint 7 amount currency Nothing Nothing message

row :: ArtistProfileId -> Int -> Maybe Text -> Maybe Text -> ME.ArtistTip
row profile amount mKey mMessage = ME.ArtistTip
  { ME.artistTipArtistProfileId = profile
  , ME.artistTipTipperPartyId = Nothing
  , ME.artistTipTipperEmail = Nothing
  , ME.artistTipTipperName = Nothing
  , ME.artistTipAmountCents = amount
  , ME.artistTipCurrency = "usd"
  , ME.artistTipPlatformFeeCents = 0
  , ME.artistTipStripePaymentIntentId = Nothing
  , ME.artistTipStatus = "pending"
  , ME.artistTipMessage = mMessage
  , ME.artistTipIdempotencyKey = mKey
  , ME.artistTipRequestSha256 = fingerprintFor amount "usd" mMessage <$ mKey
  , ME.artistTipCreatedAt = now
  , ME.artistTipUpdatedAt = now
  }

reserve :: ArtistProfileId -> Int -> Maybe Text -> Maybe Text -> Db TipReservation
reserve profile amount mKey mMessage =
  reserveArtistTip (fingerprintFor amount "usd" mMessage) mKey (row profile amount mKey mMessage)

countRows :: Db Int
countRows = length <$> (selectList [] [] :: Db [Entity ME.ArtistTip])

withDb :: Db a -> IO a
withDb action = runSqlite ":memory:" (schema >> action)

spec :: Spec
spec = describe "Keyed artist tip reservation (PAY-TIP-001, database)" $ do
  let artist = toSqlKey 7 :: ArtistProfileId
      other = toSqlKey 8 :: ArtistProfileId

  it "reserves a new row for a keyed request and replays it once the intent is stored" $ withDb $ do
    first <- reserve artist 1500 (Just "key-one") (Just "hola")
    tipKey <- case first of
      TipReserved k -> pure k
      otherwise' -> liftIO (expectationFailure ("expected reservation, got " <> show otherwise')) >> fail "stop"
    -- Before the provider answered, the same request is refused (the stored row
    -- has no PaymentIntent to resume), never doubled.
    pending <- reserve artist 1500 (Just "key-one") (Just "hola")
    case pending of
      TipConflict _ -> pure ()
      other' -> liftIO (expectationFailure ("expected conflict while pending, got " <> show other'))
    update tipKey [ME.ArtistTipStripePaymentIntentId =. Just "pi_123"]
    replay <- reserve artist 1500 (Just "key-one") (Just "hola")
    case replay of
      TipReplay k _ intent -> liftIO $ do
        fromSqlKey k `shouldBe` fromSqlKey tipKey
        intent `shouldBe` "pi_123"
      other' -> liftIO (expectationFailure ("expected replay, got " <> show other'))
    rows <- countRows
    liftIO (rows `shouldBe` 1)

  it "rejects the same key with a different request and keeps the stored row" $ withDb $ do
    first <- reserve artist 1500 (Just "key-two") Nothing
    case first of
      TipReserved k -> update k [ME.ArtistTipStripePaymentIntentId =. Just "pi_456"]
      other' -> liftIO (expectationFailure ("expected reservation, got " <> show other'))
    conflict <- reserve artist 2500 (Just "key-two") Nothing
    case conflict of
      TipConflict _ -> pure ()
      other' -> liftIO (expectationFailure ("expected conflict, got " <> show other'))
    rows <- countRows
    liftIO (rows `shouldBe` 1)

  it "scopes keys per artist and never deduplicates unkeyed requests" $ withDb $ do
    a <- reserve artist 1000 (Just "shared-key") Nothing
    b <- reserve other 1000 (Just "shared-key") Nothing
    u1 <- reserve artist 1000 Nothing Nothing
    u2 <- reserve artist 1000 Nothing Nothing
    let reserved r = case r of TipReserved _ -> True; _ -> False
    liftIO $ map reserved [a, b, u1, u2] `shouldBe` [True, True, True, True]
    rows <- countRows
    liftIO (rows `shouldBe` 4)
