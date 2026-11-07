-- WhatsApp double opt-in. A public consent request is recorded as pending
-- (confirmation_requested_at) and becomes consent only when the number itself
-- replies to the confirmation message. Additive; existing consent rows keep
-- their state.
BEGIN;
ALTER TABLE whats_app_consent
  ADD COLUMN IF NOT EXISTS confirmation_requested_at TIMESTAMPTZ;
COMMIT;
