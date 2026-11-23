{-# LANGUAGE OverloadedStrings #-}
module TDF.Server.PublicBookingNotificationsSpec (spec) where

import Test.Hspec
import TDF.Server.PublicBookingNotifications (parseAlertPartyIds)

spec :: Spec
spec = describe "PUBLIC_BOOKING_ALERT_PARTY_IDS" $ do
  it "reads comma- and space-separated party ids in order without duplicates" $
    parseAlertPartyIds " 9, 43 ,57 9" `shouldBe` [9, 43, 57]
  it "ignores blank, non-numeric and non-positive entries" $
    parseAlertPartyIds "9,,abc,-4,0, 12x" `shouldBe` [9]
  it "yields no ids for an empty value so role recipients apply" $
    parseAlertPartyIds "  " `shouldBe` []
