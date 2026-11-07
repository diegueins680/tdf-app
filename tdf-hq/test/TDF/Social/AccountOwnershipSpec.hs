{-# LANGUAGE OverloadedStrings #-}

-- | AUTH-SOCIAL-OAUTH-001 / AUTHORITY-094: pure ownership and delivery rules.
module TDF.Social.AccountOwnershipSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           TDF.Social.AccountOwnership

spec :: Spec
spec = describe "Social account ownership (AUTH-SOCIAL-OAUTH-001)" $ do
  it "creates a new binding when no row exists, for any caller" $
    property $ \isAdmin (Positive caller) -> decideConnect isAdmin caller Nothing === ConnectNew
  it "lets the owner refresh its own account" $
    property $ \isAdmin (Positive caller) -> decideConnect isAdmin caller (Just (Just caller)) === KeepOwner
  it "refuses a non-administrator who is not the owner, including unowned rows" $
    property $ \(Positive caller) mOwner ->
      let owner = fmap getPositive mOwner
      in owner /= Just caller ==> decideConnect False caller (Just owner) === RebindRefused
  it "audits an administrator rebind from the former owner" $
    property $ \(Positive caller) (Positive former) ->
      former /= caller ==> decideConnect True caller (Just (Just former)) === RebindAudited former
  it "never rebinds silently: every change of owner is either audited or refused" $
    property $ \isAdmin (Positive caller) mOwner ->
      let owner = fmap getPositive mOwner
      in case decideConnect isAdmin caller (Just owner) of
           KeepOwner -> owner === Just caller
           RebindAudited _ -> property isAdmin .&&. owner =/= Just caller
           RebindRefused -> property (not isAdmin) .&&. owner =/= Just caller
           ConnectNew -> counterexample "row existed" False
  it "delivers only through designated, connected accounts with a token" $
    property $ \d c t -> deliveryEligible d c t === (d && c && t)
