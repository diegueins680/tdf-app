-- Synthetic records against the unmodified, fully migrated repository schema.
BEGIN;
CREATE SCHEMA event_rehearsal;
CREATE FUNCTION event_rehearsal.check_that(ok BOOLEAN, description TEXT) RETURNS void
LANGUAGE plpgsql AS $$ BEGIN
  IF ok IS DISTINCT FROM TRUE THEN RAISE EXCEPTION 'Schema rehearsal: %', description; END IF;
END $$;

SELECT event_rehearsal.check_that(
  (SELECT count(*)=8 AND bool_and(NOT enabled AND status='disabled'
    AND contract_status='unverified' AND credential_status='absent') FROM commerce_provider_account),
  'registered canonical provider accounts remain disabled and unverified');
SELECT event_rehearsal.check_that(
  NOT EXISTS (
    SELECT 1 FROM (VALUES ('checkout.placetopay'),('checkout.payphone'),
      ('commerce.marketplace_connected_accounts'),('commerce.recurring_payments'),
      ('commerce.payment_links'),('merch.checkout'),('merch.checkout.runtime_ready')) expected(flag_key)
    LEFT JOIN revenue_feature_flag actual ON actual.flag_key=expected.flag_key AND actual.environment='production'
    WHERE actual.enabled IS DISTINCT FROM FALSE),
  'registration does not activate payment or merch production flags');

INSERT INTO party(id,display_name,is_org,created_at) VALUES
  (900001,'Synthetic event organization',TRUE,now()),
  (900002,'Synthetic event collaborator',FALSE,now());
INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,timezone,workflow_state_id,event_type_id)
SELECT event.id,event.owner,event.title,'2026-10-01 18:00:00Z','2026-10-02 03:00:00Z',
  'America/Guayaquil',state.id,event_type.id
FROM (VALUES (900010,'900001','Synthetic owned event'),
             (900011,NULL,'Synthetic unresolved owner')) AS event(id,owner,title)
CROSS JOIN workflow_state state
JOIN workflow_definition workflow ON workflow.id=state.workflow_id
CROSS JOIN (
  SELECT item.id FROM event_type item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='event-types' AND catalog.active
  JOIN workflow_state published ON published.id=item.workflow_state_id
    AND published.workflow_id=catalog.workflow_id AND published.code='published' AND published.active
  WHERE item.active AND item.deprecated_at IS NULL
    AND (item.effective_from IS NULL OR item.effective_from<=CURRENT_DATE)
    AND (item.effective_until IS NULL OR item.effective_until>=CURRENT_DATE)
  ORDER BY item.id LIMIT 1
) event_type
WHERE workflow.code='social-event-lifecycle' AND workflow.active
  AND state.code='planning' AND state.active;
SELECT event_rehearsal.check_that(
  (SELECT count(*)=2 FROM social_event WHERE id IN (900010,900011)), 'two canonical legacy events');

INSERT INTO event_logistics_activity(
  id,event_id,activity_type,title,start_time,priority,status,version,created_by_party_id
) VALUES
  (900010,900010,'task','Synthetic soundcheck','2026-10-01 18:00:00Z','normal','planned',1,'900001'),
  (900011,900010,'task','Synthetic load-in','2026-10-01 17:00:00Z','normal','planned',1,'900001');
INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (900010,900011);
INSERT INTO event_invitation(id,event_id,from_party_id,to_party_id,status)
VALUES (900010,900010,'900001','900002','pending');

-- Preserve every existing row, not just the synthetic examples. No legacy table is rewritten.
CREATE FUNCTION event_rehearsal.legacy_rows() RETURNS JSONB LANGUAGE sql AS $$
  SELECT jsonb_build_object(
    'party',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM party t),
    'event',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM social_event t),
    'invitation',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_invitation t),
    'activity',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_logistics_activity t),
    'dependency',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_logistics_dependency t),
    'ledger',(SELECT jsonb_agg(to_jsonb(t) ORDER BY migration_id) FROM tdf_schema_migration t),
    'providers',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM commerce_provider_account t),
    'revenueFlags',(SELECT jsonb_agg(to_jsonb(t) ORDER BY flag_key,environment) FROM revenue_feature_flag t),
    'paymentAttempts',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM commerce_payment_attempt t),
    'paymentIntents',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM commerce_payment_intent t),
    'refunds',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM commerce_refund t))
$$;
CREATE TABLE event_rehearsal.expected_legacy AS SELECT event_rehearsal.legacy_rows() AS snapshot;
CREATE TABLE event_rehearsal.expected_columns AS
SELECT table_name,column_name,data_type,is_nullable,column_default
FROM information_schema.columns WHERE table_schema='public';
COMMIT;
