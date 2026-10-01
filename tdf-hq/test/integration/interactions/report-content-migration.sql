-- Exercise the real upgrade with nonempty prior reports, then reapply safely.
\set ON_ERROR_STOP on
DO $$ BEGIN ASSERT current_database() LIKE 'tdf_interaction_%', 'Disposable database required'; END $$;
UPDATE interaction_runtime SET enabled=true;
INSERT INTO party(id,display_name,is_org,created_at) VALUES(916100001,'Report migration owner',false,now()),(916100002,'Report migration actor',false,now());
INSERT INTO user_credential(party_id,username,password_hash,active) VALUES(916100001,'report-migration-owner','not-a-login-hash',true),(916100002,'report-migration-actor','not-a-login-hash',true);
INSERT INTO fan_club(id,artist_party_id,name) VALUES(916100001,916100001,'Report migration');
INSERT INTO fan_club_post(id,club_id,fan_party_id,content,created_at) VALUES(916100001,916100001,916100001,'Report migration',now());
INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES(916100002,916100001,now());
CREATE TEMP TABLE report_migration_before AS
 SELECT interaction_register('club_post','916100001',916100001) AS target;
INSERT INTO interaction_comment(id,target_id,author_id,root_id,body,version,content_version)
 SELECT '91610000-0000-4000-8000-000000000001',target,916100001,'91610000-0000-4000-8000-000000000001','Existing evidence',4,4 FROM report_migration_before;
INSERT INTO interaction_report(target_id,comment_id,reporter_id,reason,state,reported_version)
 SELECT target,'91610000-0000-4000-8000-000000000001',916100002,'Retained original reason','dismissed',4 FROM report_migration_before;
-- Only this isolated migration test removes the new columns to model schema 156.
ALTER TABLE interaction_report DROP COLUMN reported_version;
ALTER TABLE interaction_comment DROP COLUMN content_version;
\ir ../../../sql/2026-09-30_interaction_report_content_version.sql
DO $$ BEGIN
 ASSERT (SELECT content_version=4 FROM interaction_comment WHERE id='91610000-0000-4000-8000-000000000001');
 ASSERT (SELECT state='dismissed' AND reason='Retained original reason' AND reported_version=4 FROM interaction_report WHERE comment_id='91610000-0000-4000-8000-000000000001');
END $$;
SELECT interaction_command(916100001,target,gen_random_uuid(),'{"operation":"comment.edit","commentId":"91610000-0000-4000-8000-000000000001","expectedVersion":4,"body":"Changed after migration","mentions":[]}') FROM report_migration_before;
\ir ../../../sql/2026-09-30_interaction_report_content_version.sql
DO $$ BEGIN
 ASSERT (SELECT content_version=5 FROM interaction_comment WHERE id='91610000-0000-4000-8000-000000000001');
 ASSERT (SELECT reported_version=4 FROM interaction_report WHERE comment_id='91610000-0000-4000-8000-000000000001'), 'Reapplication cannot consume an unreported edit';
END $$;
SELECT interaction_command(916100002,target,gen_random_uuid(),'{"operation":"comment.report","commentId":"91610000-0000-4000-8000-000000000001","reason":"New content evidence"}') FROM report_migration_before;
DO $$ BEGIN
 ASSERT (SELECT state='open' AND reported_version=5 FROM interaction_report WHERE comment_id='91610000-0000-4000-8000-000000000001');
 ASSERT (SELECT count(*)=1 FROM interaction_audit WHERE comment_id='91610000-0000-4000-8000-000000000001' AND operation='comment.report.reopen' AND reason='Retained original reason');
END $$;
