-- Audited provider-event retry operations and formal inbox transitions.
BEGIN;

CREATE TABLE IF NOT EXISTS commerce_provider_event_action (
  id BIGSERIAL PRIMARY KEY,
  provider_event_id UUID NOT NULL
    REFERENCES commerce_provider_event_inbox(id) ON DELETE RESTRICT,
  action TEXT NOT NULL CHECK (action IN ('requeued')),
  from_status TEXT NOT NULL,
  to_status TEXT NOT NULL,
  actor_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  reason TEXT NOT NULL CHECK (
    char_length(btrim(reason)) BETWEEN 8 AND 500
    AND reason !~ '[[:cntrl:]]'
  ),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_commerce_provider_event_action_event
  ON commerce_provider_event_action(provider_event_id, created_at DESC, id DESC);

DROP TRIGGER IF EXISTS trg_commerce_provider_event_action_immutable
  ON commerce_provider_event_action;
CREATE TRIGGER trg_commerce_provider_event_action_immutable
  BEFORE UPDATE OR DELETE ON commerce_provider_event_action
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_immutable_mutation();

CREATE OR REPLACE FUNCTION commerce_validate_provider_event_transition()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.processing_status IS NOT DISTINCT FROM NEW.processing_status THEN
    RETURN NEW;
  END IF;

  IF (OLD.processing_status, NEW.processing_status) IN (
    ('pending', 'processing'),
    ('retry', 'processing'),
    ('processing', 'processed'),
    ('processing', 'ignored'),
    ('processing', 'retry'),
    ('processing', 'dead_letter')
  ) THEN
    RETURN NEW;
  END IF;

  IF OLD.processing_status = 'dead_letter'
     AND NEW.processing_status = 'retry'
     AND current_setting('tdf.provider_event_requeue', TRUE) = OLD.id::text THEN
    RETURN NEW;
  END IF;

  RAISE EXCEPTION 'Invalid provider event transition % -> %',
    OLD.processing_status, NEW.processing_status;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_provider_event_transition
  ON commerce_provider_event_inbox;
CREATE TRIGGER trg_commerce_provider_event_transition
  BEFORE UPDATE OF processing_status ON commerce_provider_event_inbox
  FOR EACH ROW EXECUTE FUNCTION commerce_validate_provider_event_transition();

CREATE OR REPLACE FUNCTION commerce_requeue_provider_event(
  requested_event_id UUID,
  requested_actor_party_id BIGINT,
  requested_reason TEXT,
  requested_at TIMESTAMPTZ
) RETURNS UUID LANGUAGE plpgsql AS $$
DECLARE
  current_status TEXT;
BEGIN
  IF requested_actor_party_id IS NULL OR requested_actor_party_id <= 0 THEN
    RAISE EXCEPTION 'Provider event replay requires an actor';
  END IF;
  IF requested_reason IS NULL
     OR char_length(btrim(requested_reason)) NOT BETWEEN 8 AND 500
     OR requested_reason ~ '[[:cntrl:]]' THEN
    RAISE EXCEPTION 'Provider event replay requires a reason of 8 to 500 characters';
  END IF;

  SELECT processing_status INTO current_status
  FROM commerce_provider_event_inbox
  WHERE id = requested_event_id
  FOR UPDATE;

  IF NOT FOUND THEN
    RAISE EXCEPTION 'Provider event not found';
  END IF;
  IF current_status <> 'dead_letter' THEN
    RAISE EXCEPTION 'Only dead-letter provider events can be requeued';
  END IF;

  PERFORM set_config('tdf.provider_event_requeue', requested_event_id::text, TRUE);
  UPDATE commerce_provider_event_inbox
  SET processing_status = 'retry',
      next_attempt_at = requested_at,
      processing_started_at = NULL,
      processed_at = NULL
  WHERE id = requested_event_id;
  PERFORM set_config('tdf.provider_event_requeue', '', TRUE);

  INSERT INTO commerce_provider_event_action (
    provider_event_id, action, from_status, to_status,
    actor_party_id, reason, created_at
  ) VALUES (
    requested_event_id, 'requeued', 'dead_letter', 'retry',
    requested_actor_party_id, btrim(requested_reason), requested_at
  );

  RETURN requested_event_id;
END $$;

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('checkout.provider_event_worker', TRUE, 'sandbox',
    'Sandbox worker may process only previously signature-verified inbox evidence'),
  ('checkout.provider_event_worker', FALSE, 'production',
    'Requires credentialed webhook retry evidence, alert ownership, and production authorization')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
