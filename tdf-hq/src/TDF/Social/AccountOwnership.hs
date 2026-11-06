{-# LANGUAGE OverloadedStrings #-}

-- | Ownership and delivery-designation rules for social sync accounts
-- (AUTHORITY-094, AUTH-SOCIAL-OAUTH-001).
--
-- An account connected through OAuth belongs to the party that connected it
-- and serves only that party's synchronization. Only an administrator may
-- rebind an account another party connected, and the rebind is audited. Only
-- accounts an administrator designated (or the environment-configured account)
-- may deliver platform messages or run backfills.
--
-- Pure decisions; the handlers gather the evidence and apply the effects.
module TDF.Social.AccountOwnership
  ( RebindDecision (..)
  , decideConnect
  , deliveryEligible
  ) where

import           Data.Int (Int64)

data RebindDecision
  = ConnectNew
    -- ^ no account row yet: create it bound to the connecting party
  | KeepOwner
    -- ^ the connecting party already owns the row: refresh token and handle
  | RebindAudited Int64
    -- ^ administrator rebinds from the former owner (its party id) to the connecting party
  | RebindRefused
    -- ^ another party owns the row and the caller is not an administrator
  deriving (Eq, Show)

-- | Decide how an OAuth exchange may bind an account.
--
-- Arguments: whether the caller holds the Admin role, the connecting party,
-- and the stored owner of the existing row ('Nothing' when there is no row;
-- @Just Nothing@ when the row exists without an owner, which only an
-- administrator may claim).
decideConnect :: Bool -> Int64 -> Maybe (Maybe Int64) -> RebindDecision
decideConnect _ _ Nothing = ConnectNew
decideConnect isAdmin caller (Just owner)
  | owner == Just caller = KeepOwner
  | isAdmin = RebindAudited (maybe 0 id owner)
  | otherwise = RebindRefused

-- | Whether a stored account may be used as a platform delivery or backfill
-- channel: designated by an administrator, connected, and holding a token.
deliveryEligible :: Bool -> Bool -> Bool -> Bool
deliveryEligible designated connected hasToken = designated && connected && hasToken
