#!/bin/sh
set -eu
# PGHOST/PGPORT/PGUSER may select an isolated local PostgreSQL instance.
notification_test_db="tdf_notification_test_$$"
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
createdb "$notification_test_db"
trap 'dropdb "$notification_test_db"' EXIT INT TERM
sql() { psql -X -v ON_ERROR_STOP=1 -d "$notification_test_db" "$@"; }
sql <<'SQL'
CREATE TABLE notification(id bigserial PRIMARY KEY,recipient_party_id bigint,notif_type text,title text,body text,target_type text,target_id bigint,is_read boolean DEFAULT false,created_at timestamptz);
CREATE TABLE engagement_event(actor_party_id bigint,target_artist_id bigint,entity_type text,entity_id bigint,event_type text,created_at timestamptz);
CREATE TABLE intern_audit_notification_outbox(recipient_party_id bigint,template_key text,created_at timestamptz,delivery_mode text,plan_id uuid,report_id uuid);
CREATE TABLE directory_alert_delivery(id uuid PRIMARY KEY DEFAULT gen_random_uuid(),internal_notification_id bigint,saved_search_id uuid,result_kind text,result_id text,result_version bigint,email_status text,push_status text,UNIQUE(saved_search_id,result_kind,result_id,result_version));
INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_id,created_at,is_read) VALUES
 (5,'artist_liked','New fan','Do not parse this name','artist',5,'2026-09-16 10:00Z',true),
 (5,'artist_liked','New fan','Ambiguous','artist',5,'2026-09-16 11:00Z',false),
 (5,'artist_liked','New fan','No evidence','artist',5,'2026-09-16 12:00Z',false),
 (5,'internal_feedback_received','Feedback','Original','internal_feedback_report',NULL,'2026-09-16 13:00Z',false);
INSERT INTO engagement_event VALUES (7,5,'artist',5,'follow','2026-09-16 10:00Z'),(7,5,'artist',5,'follow','2026-09-16 11:00Z'),(8,5,'artist',5,'follow','2026-09-16 11:00Z');
INSERT INTO intern_audit_notification_outbox VALUES (5,'internal_feedback_received','2026-09-16 13:00Z','immediate',NULL,'00000000-0000-0000-0000-000000000017');
CREATE TABLE original_notifications AS SELECT * FROM notification;
ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK (notif_type IN ('artist_liked','internal_feedback_received','historical_custom')); 
SQL
sql -f "$repo_root/tdf-hq/sql/2026-09-18_notification_navigation.sql"
sql -f "$repo_root/tdf-hq/sql/2026-09-18_notification_navigation.sql"
sql <<'SQL'
DO $$ BEGIN
 IF NOT EXISTS(SELECT 1 FROM notification WHERE id=1 AND target_type='party_profile' AND target_id=7 AND is_read) THEN RAISE EXCEPTION 'follower identity/read history lost'; END IF;
 IF (SELECT count(*) FROM notification WHERE id IN (2,3) AND target_type='artist' AND target_id=5)<>2 THEN RAISE EXCEPTION 'guessed legacy actor'; END IF;
 IF (SELECT target_key FROM notification WHERE id=4)<>'00000000-0000-0000-0000-000000000017' THEN RAISE EXCEPTION 'report relationship not recovered'; END IF;
 IF (SELECT count(*) FROM notification_navigation_constraint_history)<>1 THEN RAISE EXCEPTION 'original type constraint not retained'; END IF;
 IF (SELECT count(*) FROM notification_navigation_backfill)<>2 THEN RAISE EXCEPTION 'backfill is not idempotent'; END IF;
END $$;
INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at) VALUES(5,'directory.invitation','New','Keep this new record','directory_invitation','00000000-0000-0000-0000-000000000018',true,now());
SQL
sql -f "$repo_root/tdf-hq/sql/2026-09-18_notification_navigation_rollback.sql"
sql -f "$repo_root/tdf-hq/sql/2026-09-18_notification_navigation_rollback.sql"
sql <<'SQL'
DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM original_notifications o JOIN notification n USING(id) WHERE ROW(o.recipient_party_id,o.notif_type,o.title,o.body,o.target_type,o.target_id,o.is_read,o.created_at) IS DISTINCT FROM ROW(n.recipient_party_id,n.notif_type,n.title,n.body,n.target_type,n.target_id,n.is_read,n.created_at)) THEN RAISE EXCEPTION 'historical rollback mismatch'; END IF;
 IF NOT EXISTS(SELECT 1 FROM notification_navigation_constraint_history) THEN RAISE EXCEPTION 'unsafe narrowing after new notification'; END IF;
 IF NOT EXISTS(SELECT 1 FROM notification WHERE id=5 AND target_key='00000000-0000-0000-0000-000000000018' AND is_read) THEN RAISE EXCEPTION 'rollback destroyed new notification'; END IF;
END $$;
SQL
sql -f "$repo_root/tdf-hq/sql/2026-09-18_notification_navigation.sql"
sql <<'SQL'
CREATE FUNCTION directory_normalize_text(text) RETURNS text LANGUAGE sql IMMUTABLE AS 'SELECT lower($1)';
CREATE FUNCTION directory_text_similarity(text,text) RETURNS double precision LANGUAGE sql IMMUTABLE AS 'SELECT 0::double precision';
CREATE TABLE directory_saved_search(id uuid PRIMARY KEY,account_party_id bigint,name text,canonical_query jsonb,alerts_enabled boolean,alert_frequency text,last_evaluated_at timestamptz);
CREATE TABLE directory_search_document(entity_kind text,entity_id text,source_version bigint,sponsored boolean,source_status text,visibility text,moderation_status text,expires_at timestamptz,search_vector tsvector,search_text text,city_id uuid,profession_ids uuid[],instrument_ids uuid[],genre_ids uuid[]);
CREATE TRIGGER test_alert AFTER INSERT OR UPDATE ON directory_search_document FOR EACH ROW EXECUTE FUNCTION directory_enqueue_saved_search_alerts();
INSERT INTO directory_saved_search VALUES('00000000-0000-0000-0000-000000000020',5,'My search','{}',true,'daily',NULL);
INSERT INTO directory_search_document VALUES('profile','00000000-0000-0000-0000-000000000021',1,false,'published','public','allowed',NULL,to_tsvector('profile'),'profile',NULL,'{}','{}','{}');
UPDATE directory_search_document SET source_version=1;
DO $$ BEGIN
 IF (SELECT count(*) FROM notification WHERE notif_type='directory.saved-search-match')<>1 THEN RAISE EXCEPTION 'alert delivery is not idempotent'; END IF;
 IF NOT EXISTS(SELECT 1 FROM directory_alert_delivery d JOIN notification n ON n.id=d.internal_notification_id AND n.target_key=d.id::text WHERE n.recipient_party_id=5 AND d.result_id='00000000-0000-0000-0000-000000000021') THEN RAISE EXCEPTION 'alert lacks exact result relationship'; END IF;
END $$;
SQL
echo 'Notification identity, ambiguous legacy, idempotency, rollback and history checks passed.'
