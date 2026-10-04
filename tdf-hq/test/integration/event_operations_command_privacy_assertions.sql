-- Paired observations against synthetic IDs in the disposable migration database.
INSERT INTO social_event(id,organizer_party_id)
 SELECT n,'1' FROM generate_series(80,82) n;
INSERT INTO event_operation_event_state(event_id,canonical_state,version,migration_evidence)
 SELECT n,'planning',1,'command privacy regression' FROM generate_series(80,82) n;
INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id)
 VALUES (81,2,'event.manage',1),(82,2,'event.read',1);

DO $$
DECLARE
 command UUID := '80000000-0000-4000-8000-000000000001';
 original JSONB;
 result JSONB;
 missing JSONB;
 request_hash TEXT;
BEGIN
 missing := event_operation_apply_transition(999999,2,command,1,'pending_approval',NULL,'privacy',repeat('a',64));
 IF missing IS DISTINCT FROM '{"error":"not_found"}'::jsonb THEN
   RAISE EXCEPTION 'missing target envelope changed: %', missing;
 END IF;
 -- Fresh key, denied receipt replay, changed content: all exactly the absent envelope.
 FOREACH request_hash IN ARRAY ARRAY[repeat('a',64),repeat('a',64),repeat('b',64)] LOOP
   result := event_operation_apply_transition(80,2,command,1,'pending_approval',NULL,'privacy',request_hash);
   IF result IS DISTINCT FROM missing THEN
     RAISE EXCEPTION 'target existence leaked through new/replayed/conflicting denial: %', result;
   END IF;
 END LOOP;
 IF (SELECT count(*) FROM event_operation_command_receipt WHERE event_id=80) <> 1
    OR (SELECT count(*) FROM event_operation_audit_event WHERE event_id=80 AND outcome='rejected') <> 3
    OR (SELECT response->>'error' FROM event_operation_command_receipt WHERE event_id=80) IS DISTINCT FROM 'forbidden'
    OR (SELECT version FROM event_operation_event_state WHERE event_id=80) <> 1
    OR EXISTS (SELECT 1 FROM event_operation_transition WHERE event_id=80) THEN
   RAISE EXCEPTION 'opaque denial lost private diagnostics or changed state';
 END IF;

 original := event_operation_apply_transition(81,2,command,1,'pending_approval',NULL,'privacy',repeat('a',64));
 IF original->>'version' IS DISTINCT FROM '2' THEN RAISE EXCEPTION 'accepted fixture failed'; END IF;
 UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,
   revocation_reason='privacy test' WHERE event_id=81;
 FOREACH request_hash IN ARRAY ARRAY[repeat('a',64),repeat('b',64)] LOOP
   result := event_operation_apply_transition(81,2,command,1,'pending_approval',NULL,'privacy',request_hash);
   IF result IS DISTINCT FROM missing THEN RAISE EXCEPTION 'revoked accepted receipt leaked: %', result; END IF;
 END LOOP;
 result := event_operation_apply_transition(81,3,command,1,'pending_approval',NULL,'privacy',repeat('a',64));
 IF result IS DISTINCT FROM missing THEN RAISE EXCEPTION 'other-actor receipt binding leaked'; END IF;
 IF (SELECT response FROM event_operation_command_receipt WHERE event_id=81) IS DISTINCT FROM original
    OR (SELECT count(*) FROM event_operation_transition WHERE event_id=81) <> 1
    OR (SELECT count(*) FROM event_operation_command_receipt WHERE event_id=81) <> 1
    OR (SELECT count(*) FROM event_operation_audit_event WHERE event_id=81 AND outcome='rejected') <> 3 THEN
   RAISE EXCEPTION 'accepted receipt history or denial audit changed';
 END IF;

 -- A readable target without write authority is still a useful, explicit 403 at HTTP.
 original := event_operation_apply_transition(82,2,command,1,'pending_approval',NULL,'privacy',repeat('a',64));
 IF original->>'error' IS DISTINCT FROM 'forbidden' THEN RAISE EXCEPTION 'read-only mutation gained authority'; END IF;
 UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,
   revocation_reason='privacy test' WHERE event_id=82;
 result := event_operation_apply_transition(82,2,command,1,'pending_approval',NULL,'privacy',repeat('a',64));
 IF result IS DISTINCT FROM missing THEN RAISE EXCEPTION 'revoked rejection receipt leaked'; END IF;
 -- Restoring read access may expose the actor's own unchanged historical denial.
 UPDATE event_operation_grant SET revoked_at=NULL,revoked_by_party_id=NULL,
   revocation_reason=NULL WHERE event_id=82;
 result := event_operation_apply_transition(82,2,command,1,'pending_approval',NULL,'privacy',repeat('a',64));
 IF result IS DISTINCT FROM original || '{"replayed":true}'::jsonb
    OR (SELECT response FROM event_operation_command_receipt WHERE event_id=82) IS DISTINCT FROM original
    OR (SELECT count(*) FROM event_operation_command_receipt WHERE event_id=82) <> 1
    OR (SELECT count(*) FROM event_operation_audit_event WHERE event_id=82 AND outcome='rejected') <> 2
    OR EXISTS (SELECT 1 FROM event_operation_transition WHERE event_id=82) THEN
   RAISE EXCEPTION 'read restoration changed historical rejection or executed a transition';
 END IF;
END $$;
