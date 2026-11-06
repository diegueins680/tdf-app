-- Reversible before any delivery; preserve preferences, configuration and user content.
BEGIN;
SET LOCAL lock_timeout='5s';
DO $$ DECLARE prior event_memory_migration_state%ROWTYPE; BEGIN
 SELECT * INTO STRICT prior FROM event_memory_migration_state WHERE id=1;
 IF EXISTS(SELECT 1 FROM event_memory_delivery) THEN
  RAISE EXCEPTION 'Retain the installed privacy authority for existing invitations; use a forward repair';
 END IF;
 IF md5(pg_get_functiondef('interaction_notification_visible(bigint,bigint)'::regprocedure))<>prior.installed_visibility_digest
  OR md5(pg_get_viewdef('directory_public_rsvp_event'::regclass,true))<>prior.installed_view_digest THEN
  RAISE EXCEPTION 'A successor changed the installed authority; review a forward repair';
 END IF;
 UPDATE event_memory_campaign SET enabled=false;
 EXECUTE prior.visibility_definition;
 EXECUTE 'CREATE OR REPLACE VIEW directory_public_rsvp_event AS '||prior.rsvp_view_definition;
 ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
 EXECUTE format('ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK (%s)',prior.notification_expression);
END $$;
UPDATE workflow_state_capability c SET enabled=coalesce(b.prior_enabled,false),updated_at=now()
 FROM event_memory_capability_backup b WHERE c.state_id=b.state_id AND c.capability_code='public-listable';
-- New catalog rows are retained disabled; registry authority forbids hard deletion.
DROP TRIGGER IF EXISTS event_memory_publication ON event_moment;
DROP FUNCTION event_memory_dispatch_batch(integer);
-- Tables, opt-in history, and all moment rows remain intact.
COMMIT;
