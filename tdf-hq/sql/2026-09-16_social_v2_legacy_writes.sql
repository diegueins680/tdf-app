-- Additive compatibility adapter; does not activate social V2 or rewrite history.
-- Requires foundation and the durable activation latch from the DM boundary.
BEGIN;
CREATE OR REPLACE FUNCTION social_v2_lock_legacy_write(a bigint,b bigint) RETURNS boolean
LANGUAGE plpgsql VOLATILE AS $$
BEGIN
  IF current_setting('transaction_isolation') <> 'read committed' THEN
    RAISE EXCEPTION USING ERRCODE='0A000', MESSAGE='social_legacy_requires_read_committed';
  END IF;
  -- Lock order: activation, ordered accounts, ordered credentials, then the
  -- caller locks/revalidates its bearer token in this same transaction.
  PERFORM singleton FROM social_v2_runtime WHERE singleton FOR SHARE;
  PERFORM id FROM party WHERE id IN (a,b) ORDER BY id FOR UPDATE;
  PERFORM id FROM user_credential WHERE party_id IN (a,b) ORDER BY id FOR UPDATE;
  -- A canonical pair (including an unblocked tombstone) owns its semantics.
  -- An old reciprocal add/delete cannot stand in for either party's consent.
  RETURN NOT social_v2_dm_required(a,b);
END $$;
REVOKE ALL ON FUNCTION social_v2_lock_legacy_write(bigint,bigint) FROM PUBLIC;
COMMIT;
