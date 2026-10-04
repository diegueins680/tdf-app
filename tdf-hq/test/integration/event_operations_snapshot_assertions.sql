INSERT INTO social_event(id,organizer_party_id) VALUES (40,'1'),(41,'1'),(42,'1');
INSERT INTO event_operation_event_state(event_id,canonical_state,version,migration_evidence)
 VALUES (40,'planning',1,'snapshot fixture'),(41,'pending_approval',1,'missing review fixture'),
        (42,'planning',1,'expiry fixture');
INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind) VALUES (40,1,'primary_owner');
INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id)
 VALUES (40,1,'event.approve',1),(40,2,'event.approve',1),(41,2,'event.approve',1),(42,2,'event.read',1);
SELECT event_operation_apply_transition(40,1,'40000000-0000-4000-8000-000000000001',1,
 'pending_approval',NULL,'snapshot-review',repeat('a',64));

DO $$
DECLARE snapshot JSONB; expires TIMESTAMPTZ; legacy_visible BOOLEAN;
BEGIN
 snapshot := event_operation_read_snapshot(40,1);
 IF snapshot IS NULL OR snapshot->>'canonicalState' IS DISTINCT FROM 'pending_approval'
    OR snapshot->>'version' IS DISTINCT FROM '2'
    OR NOT (snapshot->'capabilities' ? 'event.manage')
    OR snapshot->'availableTransitions' ? 'approved' THEN
   RAISE EXCEPTION 'owner snapshot or separation of duties invalid: %', snapshot;
 END IF;
 snapshot := event_operation_read_snapshot(40,2);
 IF snapshot IS NULL OR NOT (snapshot->'availableTransitions' ? 'approved')
    OR snapshot->'capabilities' ? 'event.manage' THEN
   RAISE EXCEPTION 'independent approver snapshot invalid: %', snapshot;
 END IF;
 snapshot := event_operation_read_snapshot(41,2);
 IF snapshot IS NULL OR snapshot->'availableTransitions' ? 'approved' THEN
   RAISE EXCEPTION 'approval advertised without review requester';
 END IF;
 IF event_operation_read_snapshot(40,3) IS NOT NULL
    OR event_operation_read_snapshot(999999,1) IS NOT NULL THEN
   RAISE EXCEPTION 'unauthorized/missing snapshot disclosed';
 END IF;
 UPDATE event_operation_transition_capability SET write_enabled=FALSE
  WHERE from_state='pending_approval' AND to_state='approved';
 snapshot := event_operation_read_snapshot(40,2);
 IF snapshot IS NULL OR snapshot->'availableTransitions' ? 'approved' THEN
   RAISE EXCEPTION 'implementation-disabled transition advertised';
 END IF;
 UPDATE event_operation_transition_capability SET write_enabled=TRUE
  WHERE from_state='pending_approval' AND to_state='approved';

 expires := clock_timestamp()+interval '50 milliseconds';
 UPDATE event_operation_grant SET valid_until=expires WHERE event_id=42;
 PERFORM pg_sleep(0.1);
 IF now() >= expires OR clock_timestamp() < expires THEN RAISE EXCEPTION 'expiry setup invalid'; END IF;
 -- Negative control: the old WHERE/default-now predicate still authorizes an expired grant.
 SELECT EXISTS (SELECT 1 FROM event_operation_event_state state
   WHERE state.event_id=42 AND event_operation_actor_can_read(state.event_id,2)
   FOR SHARE OF state) INTO legacy_visible;
 IF NOT legacy_visible THEN RAISE EXCEPTION 'legacy stale-clock control did not reproduce'; END IF;
 IF event_operation_read_snapshot(42,2) IS NOT NULL THEN RAISE EXCEPTION 'expired grant snapshot disclosed'; END IF;
 UPDATE event_operation_grant SET valid_from=clock_timestamp()+interval '1 day',valid_until=NULL WHERE event_id=42;
 IF event_operation_read_snapshot(42,2) IS NOT NULL THEN RAISE EXCEPTION 'future grant snapshot disclosed'; END IF;
 UPDATE event_operation_grant SET valid_from=clock_timestamp()-interval '1 day',revoked_at=clock_timestamp(),
   revoked_by_party_id=1,revocation_reason='snapshot test' WHERE event_id=42;
 IF event_operation_read_snapshot(42,2) IS NOT NULL THEN RAISE EXCEPTION 'revoked grant snapshot disclosed'; END IF;
END $$;
