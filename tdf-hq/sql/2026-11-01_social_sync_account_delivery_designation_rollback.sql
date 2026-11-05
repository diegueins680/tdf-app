-- Rollback of 2026-11-01_social_sync_account_delivery_designation. Drops the
-- designation columns and the audit table; accounts, tokens and sync posts are
-- untouched. Delivery selection reverts to the pre-decision behaviour.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP TABLE IF EXISTS public.social_sync_account_event;
ALTER TABLE public.social_sync_account DROP CONSTRAINT IF EXISTS social_sync_account_designation_shape;
ALTER TABLE public.social_sync_account
  DROP COLUMN IF EXISTS delivery_designated,
  DROP COLUMN IF EXISTS connected_by_party_id,
  DROP COLUMN IF EXISTS designated_by_party_id,
  DROP COLUMN IF EXISTS designated_at;
COMMIT;
