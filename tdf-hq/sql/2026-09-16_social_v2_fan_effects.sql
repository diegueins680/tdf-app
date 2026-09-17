-- Retire implicit fan-club graph/profile/notification side effects at cutover.
-- Requires the durable runtime latch, profile eligibility and relationship reads.
BEGIN;
CREATE OR REPLACE FUNCTION social_v2_lock_fan_effects() RETURNS boolean
LANGUAGE plpgsql VOLATILE AS $$
BEGIN
  IF current_setting('transaction_isolation') <> 'read committed' THEN
    RAISE EXCEPTION USING ERRCODE='0A000', MESSAGE='social_fan_effects_requires_read_committed';
  END IF;
  -- Shared readers may coexist, but activation must wait for admitted legacy
  -- work to commit. Canonical public writers require enabled=true, which also
  -- permanently sets activated_once. Direct owner SQL is outside this boundary.
  PERFORM singleton FROM social_v2_runtime WHERE singleton FOR SHARE;
  RETURN social_v2_legacy_suggestions_enabled();
END $$;
REVOKE ALL ON FUNCTION social_v2_lock_fan_effects() FROM PUBLIC;
COMMIT;
