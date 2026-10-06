-- Additive compatibility fence. Requires social_v2_foundation and existing chat tables.
-- Do not activate: legacy reads/error mapping must be integrated first.
BEGIN;
ALTER TABLE social_v2_runtime ADD COLUMN IF NOT EXISTS activated_once boolean NOT NULL DEFAULT false;
UPDATE social_v2_runtime SET activated_once=true WHERE enabled AND NOT activated_once;
CREATE OR REPLACE FUNCTION social_v2_remember_activation() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP IN ('DELETE','TRUNCATE') THEN
    RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='social_activation_memory_required';
  END IF;
  IF TG_OP='UPDATE' THEN
    NEW.activated_once := OLD.activated_once OR NEW.activated_once OR NEW.enabled;
  ELSE
    NEW.activated_once := NEW.activated_once OR NEW.enabled;
  END IF;
  RETURN NEW;
END $$;
CREATE OR REPLACE TRIGGER social_v2_activation_memory
  BEFORE INSERT OR UPDATE OR DELETE ON social_v2_runtime
  FOR EACH ROW EXECUTE FUNCTION social_v2_remember_activation();
CREATE OR REPLACE TRIGGER social_v2_activation_memory_truncate
  BEFORE TRUNCATE ON social_v2_runtime
  FOR EACH STATEMENT EXECUTE FUNCTION social_v2_remember_activation();

-- This function is a policy predicate, not proof of thread participation or token authority.
-- Existing pair tombstones and closure always constrain legacy writers, even during pause.
CREATE OR REPLACE FUNCTION social_v2_dm_required(a bigint,b bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT coalesce((SELECT activated_once FROM social_v2_runtime WHERE singleton),true)
    OR EXISTS(SELECT 1 FROM social_v2_pair WHERE party_a=least(a,b) AND party_b=greatest(a,b))
    OR EXISTS(SELECT 1 FROM social_v2_preference WHERE party_id IN (a,b) AND closed)
$$;
CREATE OR REPLACE FUNCTION social_v2_dm_allowed(a bigint,b bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT a<>b AND social_v2_allowed(a,b) AND EXISTS(
    SELECT 1 FROM social_v2_pair WHERE party_a=least(a,b) AND party_b=greatest(a,b)
      AND consent_a AND consent_b AND NOT block_a AND NOT block_b)
$$;
CREATE OR REPLACE FUNCTION social_v2_check_dm_write() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE a bigint; b bigint;
BEGIN
  -- A stale REPEATABLE READ snapshot could otherwise miss a committed first block
  -- after waiting on the account lock. The application uses READ COMMITTED.
  IF current_setting('transaction_isolation') <> 'read committed' THEN
    RAISE EXCEPTION USING ERRCODE='0A000', MESSAGE='social_dm_requires_read_committed';
  END IF;
  -- Thread participant identities are immutable through existing APIs. A trusted
  -- database owner can bypass triggers; this is not an RLS/hostile-owner boundary.
  SELECT dm_party_a,dm_party_b INTO a,b FROM chat_thread WHERE id=NEW.thread_id;
  IF a IS NULL OR b IS NULL THEN
    RAISE EXCEPTION USING ERRCODE='23503', MESSAGE='social_dm_thread_missing';
  END IF;
  -- Lock before deciding legacy/strict mode: a first block creating the pair must
  -- serialize too. Canonical mutations/closure use these same account locks.
  PERFORM id FROM party WHERE id IN (a,b) ORDER BY id FOR UPDATE;
  PERFORM id FROM user_credential WHERE party_id IN (a,b) ORDER BY id FOR UPDATE;
  IF social_v2_dm_required(a,b) AND
    (NEW.sender_party_id NOT IN (a,b) OR NOT social_v2_dm_allowed(a,b)) THEN
    RAISE EXCEPTION USING ERRCODE='42501', MESSAGE='social_dm_not_permitted';
  END IF;
  RETURN NEW;
END $$;
-- Guard INSERT and UPDATE, including upserts and a moved/edited message. DELETE
-- remains available for existing moderation/data removal. No old message is deleted.
CREATE OR REPLACE TRIGGER social_v2_dm_write_guard
  BEFORE INSERT OR UPDATE ON chat_message
  FOR EACH ROW EXECUTE FUNCTION social_v2_check_dm_write();
COMMIT;
