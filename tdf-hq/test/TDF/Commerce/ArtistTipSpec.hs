{-# LANGUAGE OverloadedStrings #-}

-- | PAY-TIP-001: pure admission and fee rules for public artist tips.
module TDF.Commerce.ArtistTipSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import TDF.API.Types (maxArtistTipAmountCents)
import TDF.Commerce.ArtistTip

spec :: Spec
spec = describe "TDF.Commerce.ArtistTip (PAY-TIP-001)" $ do
  describe "artistDirectoryVisibilityFromCounts" $ do
    it "treats an artist without directory identity as listed (legacy fallback)" $
      artistPubliclyListed (artistDirectoryVisibilityFromCounts 0 0) `shouldBe` True
    it "lets a private, draft, moderated or merged identity hide the legacy profile" $
      artistPubliclyListed (artistDirectoryVisibilityFromCounts 2 0) `shouldBe` False
    it "lists an artist with at least one public identity" $
      artistPubliclyListed (artistDirectoryVisibilityFromCounts 2 1) `shouldBe` True
    it "fails closed on impossible evidence" $ do
      artistPubliclyListed (artistDirectoryVisibilityFromCounts 1 2) `shouldBe` False
      artistPubliclyListed (artistDirectoryVisibilityFromCounts 1 (-1)) `shouldBe` False
    it "is listed exactly when there is no identity or a public one exists" $
      property $ \(NonNegative totalIds) (NonNegative publicIds) ->
        artistPubliclyListed (artistDirectoryVisibilityFromCounts totalIds publicIds)
          === (totalIds == 0 || (publicIds > 0 && publicIds <= totalIds))

  describe "admitArtistTip" $ do
    it "rejects a party without an artist profile" $
      admitArtistTip Nothing NoDirectoryIdentity `shouldBe` Left TipArtistNotFound
    it "rejects an artist the directory does not list, before payout checks" $
      admitArtistTip (Just (Just "acct_1")) (DirectoryIdentity 0)
        `shouldBe` Left TipArtistNotListed
    it "rejects a listed artist without a payout account" $ do
      admitArtistTip (Just Nothing) NoDirectoryIdentity `shouldBe` Left TipPayoutNotReady
      admitArtistTip (Just (Just "  ")) (DirectoryIdentity 1) `shouldBe` Left TipPayoutNotReady
    it "routes an admitted tip to the addressed artist's own account" $
      admitArtistTip (Just (Just " acct_9 ")) (DirectoryIdentity 1) `shouldBe` Right "acct_9"
    it "never admits a hidden artist and never invents a destination" $
      property $ \(NonNegative totalIds) (NonNegative publicIds) mAccount ->
        let visibility = artistDirectoryVisibilityFromCounts totalIds publicIds
            stored = fmap T.pack mAccount
        in case admitArtistTip (Just stored) visibility of
             Right account ->
               artistPubliclyListed visibility
                 .&&. Just account === fmap T.strip stored
                 .&&. not (T.null account)
             Left TipArtistNotListed -> property (not (artistPubliclyListed visibility))
             Left TipPayoutNotReady ->
               property (artistPubliclyListed visibility
                 && maybe True (T.null . T.strip) stored)
             Left TipArtistNotFound -> counterexample "profile was present" False

  describe "artistTipPlatformFeeCents" $ do
    it "keeps 10% rounded down" $ do
      artistTipPlatformFeeCents 1000 `shouldBe` 100
      artistTipPlatformFeeCents 999 `shouldBe` 99
      artistTipPlatformFeeCents 9 `shouldBe` 0
    it "is exact floor division and strictly below the amount for every valid amount" $
      property $ forAll (chooseInt (1, maxArtistTipAmountCents)) $ \amount ->
        let fee = artistTipPlatformFeeCents amount
        in fee >= 0
             .&&. fee < amount
             .&&. fee * 10000 <= amount * artistTipPlatformFeeBps
             .&&. amount * artistTipPlatformFeeBps < (fee + 1) * 10000
    it "does not overflow at the validated maximum" $
      artistTipPlatformFeeCents maxArtistTipAmountCents `shouldBe` 9999999
