-- Privileged fixture setup in a disposable database, never a public grant API.
INSERT INTO social_event(id,organizer_party_id,workflow_state_id) VALUES
 (20,'1','00000000-0000-0000-0000-000000000001'),
 (21,'1','00000000-0000-0000-0000-000000000001'),
 (22,'2','00000000-0000-0000-0000-000000000001');
INSERT INTO event_operation_event_state(event_id,canonical_state,version,migration_evidence)
 VALUES (20,'planning',1,'replay test'),(21,'planning',1,'replay test'),(22,'planning',1,'replay test');
INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id)
 VALUES (20,2,'event.manage',1);
INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind)
 VALUES (22,2,'primary_owner');

DO $$
DECLARE
 command UUID := '20000000-0000-4000-8000-000000000001';
 command_hash TEXT := encode(digest('replay-test','sha256'),'hex');
 original JSONB;
 result JSONB;
 expires TIMESTAMPTZ;
BEGIN
 original := event_operation_apply_transition(20,2,command,1,'pending_approval',NULL,'replay-test',command_hash);
 IF original->>'canonicalState' IS DISTINCT FROM 'pending_approval' THEN
   RAISE EXCEPTION 'fixture command failed: %', original;
 END IF;
 result := event_operation_apply_transition(20,2,command,1,'pending_approval',NULL,'replay-test',command_hash);
 IF result IS DISTINCT FROM original || '{"replayed":true}'::jsonb THEN
   RAISE EXCEPTION 'authorized replay did not preserve original result';
 END IF;
 UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,
   revocation_reason='test revocation' WHERE event_id=20;
 FOR attempt IN 1..2 LOOP
   result := event_operation_apply_transition(20,2,command,1,'pending_approval',NULL,'replay-test',command_hash);
   IF result IS DISTINCT FROM '{"error":"not_found"}'::jsonb THEN
     RAISE EXCEPTION 'revoked replay disclosed a receipt: %', result;
   END IF;
 END LOOP;
 IF (SELECT count(*) FROM event_operation_command_receipt WHERE event_id=20) <> 1
   OR (SELECT count(*) FROM event_operation_transition WHERE event_id=20) <> 1
   OR (SELECT response FROM event_operation_command_receipt WHERE event_id=20) IS DISTINCT FROM original
   OR (SELECT count(*) FROM event_operation_audit_event WHERE event_id=20 AND outcome='rejected') <> 2 THEN
   RAISE EXCEPTION 'replay denial changed receipt/effects or lost audit';
 END IF;
 -- Fixture restore/downgrade: a historical read is not a new write permission.
 UPDATE event_operation_grant SET revoked_at=NULL,revoked_by_party_id=NULL,revocation_reason=NULL,
   scope_code='event.read' WHERE event_id=20;
 result := event_operation_apply_transition(20,2,command,1,'pending_approval',NULL,'replay-test',command_hash);
 IF result IS DISTINCT FROM original || '{"replayed":true}'::jsonb THEN
   RAISE EXCEPTION 'read-only historical replay rejected';
 END IF;
 result := event_operation_apply_transition(20,2,'20000000-0000-4000-8000-000000000002',2,'approved',NULL,'read-only',command_hash);
 IF result->>'error' IS DISTINCT FROM 'forbidden' THEN
   RAISE EXCEPTION 'read-only new command gained mutation permission';
 END IF;
 -- Read survives while a separate write grant expires during this transaction.
 INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id,valid_from,valid_until)
 VALUES (20,2,'event.manage',1,now()-interval '1 hour',clock_timestamp()+interval '50 milliseconds');
 PERFORM pg_sleep(0.1);
 result := event_operation_apply_transition(20,2,'20000000-0000-4000-8000-000000000003',2,'planning','Changed venue','expired-write',command_hash);
 IF result->>'error' IS DISTINCT FROM 'forbidden' THEN
   RAISE EXCEPTION 'expired write grant accepted a new command: %', result;
 END IF;
 result := event_operation_apply_transition(20,2,command,1,'pending_approval',NULL,'replay-test',repeat('b',64));
 IF result->>'error' IS DISTINCT FROM 'idempotency_conflict' THEN
   RAISE EXCEPTION 'changed hash replay accepted';
 END IF;
 result := event_operation_apply_transition(20,3,command,1,'pending_approval',NULL,'replay-test',command_hash);
 IF result IS DISTINCT FROM '{"error":"not_found"}'::jsonb THEN
   RAISE EXCEPTION 'outsider learned receipt existence/payload';
 END IF;
 result := event_operation_apply_transition(21,2,command,1,'pending_approval',NULL,'replay-test',command_hash);
 IF result IS DISTINCT FROM '{"error":"not_found"}'::jsonb THEN
   RAISE EXCEPTION 'cross-event ID leaked receipt state';
 END IF;
 -- now() deliberately remains before expiry in this transaction.
 expires := clock_timestamp() + interval '50 milliseconds';
 UPDATE event_operation_grant SET valid_from=now()-interval '1 hour', valid_until=expires WHERE event_id=20;
 PERFORM pg_sleep(0.1);
 IF now() >= expires OR clock_timestamp() < expires THEN RAISE EXCEPTION 'expiry test setup invalid'; END IF;
 result := event_operation_apply_transition(20,2,command,1,'pending_approval',NULL,'replay-test',command_hash);
 IF result IS DISTINCT FROM '{"error":"not_found"}'::jsonb THEN
   RAISE EXCEPTION 'expired permission used transaction-start clock: %', result;
 END IF;
 -- Stored rejection receipts are private too.
 result := event_operation_apply_transition(20,2,'20000000-0000-4000-8000-000000000002',2,'approved',NULL,'read-only',command_hash);
 IF result IS DISTINCT FROM '{"error":"not_found"}'::jsonb THEN
   RAISE EXCEPTION 'expired read disclosed stored rejection fields';
 END IF;
 UPDATE event_operation_grant SET valid_from=clock_timestamp()+interval '1 day',valid_until=NULL WHERE event_id=20;
 result := event_operation_apply_transition(20,2,command,1,'pending_approval',NULL,'replay-test',command_hash);
 IF result IS DISTINCT FROM '{"error":"not_found"}'::jsonb THEN RAISE EXCEPTION 'future grant replay accepted'; END IF;
END $$;

DO $$
BEGIN
 BEGIN
   UPDATE event_operation_grant SET event_id=21 WHERE event_id=20;
   RAISE EXCEPTION 'grant moved between events';
 EXCEPTION WHEN check_violation THEN NULL;
 END;
 BEGIN
   UPDATE event_operation_grant SET grantee_party_id=3 WHERE event_id=20;
   RAISE EXCEPTION 'grant recipient was replaced';
 EXCEPTION WHEN check_violation THEN NULL;
 END;
 BEGIN
   UPDATE event_operation_command_receipt SET response='{}' WHERE event_id=20;
   RAISE EXCEPTION 'receipt was mutated';
 EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL;
 END;
END $$;

DO $$
DECLARE
 command UUID := '22000000-0000-4000-8000-000000000001';
 result JSONB;
BEGIN
 result := event_operation_apply_transition(22,2,command,1,'pending_approval',NULL,'owner-replay',repeat('c',64));
 IF result->>'version' IS DISTINCT FROM '2' THEN RAISE EXCEPTION 'owner setup failed'; END IF;
 UPDATE event_operation_relationship SET relationship_kind='coproducer' WHERE event_id=22;
 result := event_operation_apply_transition(22,2,command,1,'pending_approval',NULL,'owner-replay',repeat('c',64));
 IF result IS DISTINCT FROM '{"error":"not_found"}'::jsonb THEN RAISE EXCEPTION 'coproduction expanded replay visibility'; END IF;
 UPDATE event_operation_relationship SET relationship_kind='co_owner',valid_until=clock_timestamp()+interval '50 milliseconds' WHERE event_id=22;
 PERFORM pg_sleep(0.1);
 result := event_operation_apply_transition(22,2,command,1,'pending_approval',NULL,'owner-replay',repeat('c',64));
 IF result IS DISTINCT FROM '{"error":"not_found"}'::jsonb THEN RAISE EXCEPTION 'expired owner replay accepted'; END IF;
 DELETE FROM event_operation_relationship WHERE event_id=22;
 result := event_operation_apply_transition(22,2,command,1,'pending_approval',NULL,'owner-replay',repeat('c',64));
 IF result IS DISTINCT FROM '{"error":"not_found"}'::jsonb THEN RAISE EXCEPTION 'removed owner replay accepted'; END IF;
END $$;
