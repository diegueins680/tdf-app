{-# LANGUAGE OverloadedStrings #-}
module TDF.Server.PublicBookingNotificationsSpec (spec) where

import Test.Hspec
import TDF.Server.PublicBookingNotifications (parseAlertPartyIds, resolveAlertRecipients)

spec :: Spec
spec = describe "PUBLIC_BOOKING_ALERT_PARTY_IDS" $ do
  it "reads comma- and space-separated party ids in order without duplicates" $
    parseAlertPartyIds " 9, 43 ,57 9" `shouldBe` [9, 43, 57]
  it "ignores blank, non-numeric and non-positive entries" $
    parseAlertPartyIds "9,,abc,-4,0, 12x" `shouldBe` [9]
  it "yields no ids for an empty value so role recipients apply" $
    parseAlertPartyIds "  " `shouldBe` []
  it "accepts the largest PostgreSQL bigint and ignores overflow" $
    parseAlertPartyIds "9223372036854775807,9223372036854775808" `shouldBe` [maxBound]
  it "keeps known configured parties in order and skips missing ones" $ do
    let findParty key = pure (lookup key [(9 :: Int, 'a'), (43, 'b')])
    result <- resolveAlertRecipients findParty (fail "unexpected fallback") [43, 99, 9]
    result `shouldBe` [(43, 'b'), (9, 'a')]
  it "uses role recipients when every configured party is missing" $ do
    let findParty key = pure (lookup key [(9 :: Int, 'a')])
    result <- resolveAlertRecipients findParty (pure [9]) [98, 99]
    result `shouldBe` [(9, 'a')]
