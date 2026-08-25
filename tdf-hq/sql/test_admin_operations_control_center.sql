\set ON_ERROR_STOP on
BEGIN;

UPDATE operations_organization
SET operations_enabled = TRUE
WHERE id = '00000000-0000-4000-8000-000000000001';

DO $$
DECLARE
  actor_id BIGINT;
  first_event UUID;
  replay_event UUID;
  followup_event UUID;
  approval_id UUID;
  audit_id BIGINT;
  source_before TEXT;
BEGIN
  SELECT id INTO actor_id FROM party ORDER BY id LIMIT 1;
  IF actor_id IS NULL THEN RAISE EXCEPTION 'fixture requires one party'; END IF;

  first_event := operations_record_event(
    'manual.acceptance', 'manual', 'acceptance-source', 'acceptance:dedup', 'test',
    'normal', 'Aceptación', 'Acceptance', 'No cambia fuente', 'Does not change source',
    jsonb_build_object('test', true, 'terminal', false),
    '2026-08-11T12:00:00Z', 'acceptance-provider-event', false
  );
  replay_event := operations_record_event(
    'manual.acceptance', 'manual', 'acceptance-source', 'acceptance:dedup', 'test',
    'normal', 'Aceptación', 'Acceptance', 'No cambia fuente', 'Does not change source',
    jsonb_build_object('test', true, 'terminal', false),
    '2026-08-11T12:00:00Z', 'acceptance-provider-event', false
  );
  IF first_event <> replay_event THEN RAISE EXCEPTION 'event replay was not idempotent'; END IF;

  followup_event := operations_record_event(
    'manual.acceptance.followup', 'manual', 'acceptance-source', 'acceptance:dedup', 'test',
    'high', 'Seguimiento', 'Follow-up', 'Mismo agregado', 'Same aggregate',
    jsonb_build_object('test', true, 'terminal', false),
    '2026-08-11T12:01:00Z', 'acceptance-provider-event-2', false
  );
  IF followup_event = first_event THEN RAISE EXCEPTION 'distinct aggregate event was collapsed'; END IF;
  IF (
    SELECT array_agg(aggregate_sequence ORDER BY aggregate_sequence)
    FROM operations_outbox
    WHERE aggregate_type = 'manual' AND aggregate_id = 'acceptance-source'
  ) <> ARRAY[1::bigint, 2::bigint] THEN
    RAISE EXCEPTION 'aggregate outbox sequence is not monotonic';
  END IF;

  PERFORM operations_process_outbox_batch(100, 'sql-acceptance');
  IF (SELECT count(*) FROM operations_work_item WHERE correlation_key = 'acceptance:dedup') <> 1 THEN
    RAISE EXCEPTION 'event replay created duplicate work-item threads';
  END IF;

  source_before := 'authoritative-source-unchanged';
  UPDATE operations_work_item SET status = 'resolved', resolved_at = now(), version = version + 1
  WHERE correlation_key = 'acceptance:dedup';
  IF source_before <> 'authoritative-source-unchanged' THEN
    RAISE EXCEPTION 'operations transition mutated source state';
  END IF;

  INSERT INTO operations_approval_request (
    organization_id, branch_id, action_type, target_entity_type, target_entity_id,
    requester_party_id, requester_role, request_reason, idempotency_key
  ) VALUES (
    '00000000-0000-4000-8000-000000000001',
    '00000000-0000-4000-8000-000000000002',
    'refund', 'payment', 'acceptance-payment', actor_id, 'Admin',
    'Acceptance self-approval guard', 'acceptance-self-approval'
  ) RETURNING id INTO approval_id;

  BEGIN
    UPDATE operations_approval_request SET approver_party_id = actor_id, decision = 'approved'
    WHERE id = approval_id;
    RAISE EXCEPTION 'self approval unexpectedly succeeded';
  EXCEPTION WHEN check_violation THEN
    NULL;
  END;

  INSERT INTO operations_admin_audit (
    organization_id, branch_id, actor_party_id, acting_role, source_client, action,
    target_entity_type, target_entity_id, request_id, correlation_id, reason
  ) VALUES (
    '00000000-0000-4000-8000-000000000001',
    '00000000-0000-4000-8000-000000000002', actor_id, 'Admin', 'sql-test',
    'acceptance', 'manual', 'acceptance-source', 'acceptance-request',
    'acceptance:dedup', 'append-only test'
  ) RETURNING id INTO audit_id;

  BEGIN
    UPDATE operations_admin_audit SET reason = 'mutated' WHERE id = audit_id;
    RAISE EXCEPTION 'audit update unexpectedly succeeded';
  EXCEPTION WHEN SQLSTATE '55000' THEN
    NULL;
  END;

  IF EXISTS (
    SELECT 1 FROM operations_work_item
    WHERE correlation_key = 'acceptance:dedup'
      AND metadata ?| ARRAY['token','secret','rawPayload','pan','cvv']
  ) THEN RAISE EXCEPTION 'restricted metadata was projected'; END IF;
END;
$$;

ROLLBACK;
