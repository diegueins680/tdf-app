BEGIN;
SET LOCAL lock_timeout='5s';
-- Preserve the deployed allowlist rather than replacing other feature scopes.
DO $$ DECLARE previous text; BEGIN
 SELECT pg_get_expr(conbin,conrelid) INTO STRICT previous FROM pg_constraint
 WHERE conrelid='directory_rate_limit'::regclass AND conname='directory_rate_limit_scope_check' AND contype='c';
 IF NOT EXISTS(SELECT 1 FROM interaction_migration_state WHERE key='rate_scope_constraint') THEN
   INSERT INTO interaction_migration_state VALUES('rate_scope_constraint',to_jsonb(previous));
   ALTER TABLE directory_rate_limit DROP CONSTRAINT directory_rate_limit_scope_check;
   EXECUTE format('ALTER TABLE directory_rate_limit ADD CONSTRAINT directory_rate_limit_scope_check CHECK ((%s) OR scope IN (''party_selector:interaction_mention'',''interaction:write''))',previous);
 END IF;
END $$;
CREATE OR REPLACE FUNCTION interaction_consume_write_budget(actor bigint) RETURNS boolean LANGUAGE sql AS $$
 WITH budget AS (
   INSERT INTO directory_rate_limit(scope,subject_hash,window_started_at,count,updated_at)
   VALUES('interaction:write',encode(digest(actor::text,'sha256'),'hex'),date_trunc('minute',now()),1,now())
   ON CONFLICT(scope,subject_hash,window_started_at) DO UPDATE SET count=directory_rate_limit.count+1,updated_at=now()
   RETURNING count
 ) SELECT count<=90 FROM budget
$$;
CREATE INDEX IF NOT EXISTS interaction_reaction_catalog_reference ON interaction_reaction(reaction_type_id,target_id);
-- The existing catalog authority remains canonical. Archived legacy references
-- and active universal references both prevent unsafe option deactivation.
CREATE OR REPLACE FUNCTION interaction_protect_referenced_reaction_type() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF NEW.catalog_id IS DISTINCT FROM OLD.catalog_id OR NEW.code IS DISTINCT FROM OLD.code THEN
   RAISE EXCEPTION 'referenced content reaction type catalog and code are immutable' USING ERRCODE='23514';
 END IF;
 IF OLD.active AND NOT NEW.active AND (
   EXISTS(SELECT 1 FROM fan_club_post_reaction WHERE reaction_type_id=OLD.id)
   OR EXISTS(SELECT 1 FROM fan_club_memory_reaction WHERE reaction_type_id=OLD.id)
   OR EXISTS(SELECT 1 FROM interaction_reaction WHERE reaction_type_id=OLD.id)) THEN
   RAISE EXCEPTION 'a referenced content reaction type must be replaced before deactivation' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS interaction_reaction_type_reference_protection ON content_reaction_type;
CREATE TRIGGER interaction_reaction_type_reference_protection BEFORE UPDATE OF catalog_id,code,active ON content_reaction_type
 FOR EACH ROW EXECUTE FUNCTION interaction_protect_referenced_reaction_type();
-- Existing identity reconciliation already discovers new FK references. Install
-- its normal write guard on the new tables as well, without changing identity policy.
DO $$ DECLARE item record; BEGIN
 FOR item IN SELECT cl.oid::regclass table_name,string_agg(quote_literal(a.attname),',' ORDER BY a.attnum) columns
   FROM pg_attribute a JOIN pg_class cl ON cl.oid=a.attrelid
   WHERE cl.relnamespace='public'::regnamespace AND cl.relkind IN ('r','p') AND cl.relname LIKE 'interaction\_%' ESCAPE '\'
     AND a.attnum>0 AND NOT a.attisdropped
     AND EXISTS(SELECT 1 FROM pg_constraint fk WHERE fk.contype='f' AND fk.confrelid='party'::regclass AND fk.conrelid=cl.oid AND a.attnum=ANY(fk.conkey))
   GROUP BY cl.oid ORDER BY cl.oid LOOP
   EXECUTE format('DROP TRIGGER IF EXISTS identity_archive_reference_guard ON %s',item.table_name);
   EXECUTE format('CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON %s FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference(%s)',item.table_name,item.columns);
 END LOOP;
END $$;
-- Source deletion erases discussion bodies while preserving thread/audit identity.
-- This trigger also removes frozen source children at trigger depth two, without
-- giving ordinary legacy API writes a bypass of the canonical write fence.
CREATE OR REPLACE FUNCTION interaction_delete_moment_source() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE target uuid;
BEGIN
 IF EXISTS(SELECT 1 FROM interaction_runtime WHERE activated_once) THEN
   SELECT id INTO target FROM interaction_target WHERE entity_kind='event_moment' AND entity_key=OLD.id::text FOR UPDATE;
   IF target IS NOT NULL THEN
     INSERT INTO interaction_audit(target_id,operation,reason) VALUES(target,'target.deleted','Source event moment deleted');
     UPDATE interaction_comment SET state='removed',body='',version=version+1,updated_at=now() WHERE target_id=target AND state NOT IN ('deleted','removed');
     DELETE FROM interaction_comment_mention WHERE comment_id IN(SELECT id FROM interaction_comment WHERE target_id=target);
     DELETE FROM interaction_reaction WHERE target_id=target;
   END IF;
   DELETE FROM event_moment_reaction WHERE moment_id=OLD.id;
   DELETE FROM event_moment_comment WHERE moment_id=OLD.id;
 END IF;
 RETURN OLD;
END $$;
DROP TRIGGER IF EXISTS interaction_delete_source ON event_moment;
CREATE TRIGGER interaction_delete_source BEFORE DELETE ON event_moment FOR EACH ROW EXECUTE FUNCTION interaction_delete_moment_source();
-- Polymorphic source identity is resolved by trusted adapters; source deletion
-- permanently retires the registered target so a reused source key cannot
-- accidentally inherit someone else's discussion.
CREATE OR REPLACE FUNCTION interaction_retire_deleted_source() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE target uuid;
BEGIN
 SELECT id INTO target FROM interaction_target WHERE entity_kind=TG_ARGV[0] AND entity_key=OLD.id::text FOR UPDATE;
 IF target IS NOT NULL THEN
   UPDATE interaction_target SET retired_at=now(),version=version+1,updated_at=now() WHERE id=target;
   INSERT INTO interaction_audit(target_id,operation,reason) VALUES(target,'target.retired','Publication source deleted');
   UPDATE interaction_comment SET state='removed',body='',version=version+1,updated_at=now()
     WHERE target_id=target AND state NOT IN ('deleted','removed');
   DELETE FROM interaction_comment_mention WHERE comment_id IN(SELECT id FROM interaction_comment WHERE target_id=target);
   DELETE FROM interaction_reaction WHERE target_id=target;
 END IF;
 RETURN NULL;
END $$;
DO $$ DECLARE item record; BEGIN
 FOR item IN SELECT * FROM (VALUES
   ('fan_club_post','club_post'),('fan_club_memory','club_memory'),('social_event','event'),
   ('event_moment','event_moment'),('recording','recording'),('recording_session','recording_session'),
   ('record_release','record_release'),('artist_release','artist_release'),('classified','classified'),
   ('directory_profile','directory_profile'),('social_sync_post','artist_update')) t(source,kind) LOOP
   EXECUTE format('DROP TRIGGER IF EXISTS interaction_retire_source ON %I',item.source);
   EXECUTE format('CREATE TRIGGER interaction_retire_source AFTER DELETE ON %I FOR EACH ROW EXECUTE FUNCTION interaction_retire_deleted_source(%L)',item.source,item.kind);
 END LOOP;
END $$;
COMMIT;
