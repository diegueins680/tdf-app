-- AUTHORITY-094 / AUTH-SOCIAL-OAUTH-001: only environment-configured accounts
-- and accounts an administrator explicitly designates may send platform
-- messages or run backfills. OAuth-connected accounts are user-owned and serve
-- only that user's synchronization. Rebinding an account another party
-- connected is an audited administrator action.
--
-- Existing rows are not designated: after this migration an administrator
-- designates the intended delivery account; until then delivery uses the
-- configured environment token only.
BEGIN;
SET LOCAL lock_timeout = '10s';

ALTER TABLE public.social_sync_account
  ADD COLUMN IF NOT EXISTS delivery_designated BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN IF NOT EXISTS connected_by_party_id BIGINT REFERENCES public.party(id),
  ADD COLUMN IF NOT EXISTS designated_by_party_id BIGINT REFERENCES public.party(id),
  ADD COLUMN IF NOT EXISTS designated_at TIMESTAMPTZ;

ALTER TABLE public.social_sync_account DROP CONSTRAINT IF EXISTS social_sync_account_designation_shape;
ALTER TABLE public.social_sync_account ADD CONSTRAINT social_sync_account_designation_shape CHECK (
  (NOT delivery_designated AND designated_by_party_id IS NULL AND designated_at IS NULL)
  OR (delivery_designated AND designated_by_party_id IS NOT NULL AND designated_at IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS public.social_sync_account_event (
  id BIGSERIAL PRIMARY KEY,
  account_id BIGINT NOT NULL REFERENCES public.social_sync_account(id) ON DELETE CASCADE,
  actor_party_id BIGINT REFERENCES public.party(id),
  event_type TEXT NOT NULL CHECK (event_type IN ('connected', 'rebound', 'rebind_refused', 'designated', 'undesignated')),
  from_party_id BIGINT REFERENCES public.party(id),
  to_party_id BIGINT REFERENCES public.party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_social_sync_account_event_account
  ON public.social_sync_account_event(account_id, created_at DESC);

COMMIT;
