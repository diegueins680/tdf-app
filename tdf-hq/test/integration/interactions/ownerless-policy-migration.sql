-- Runs only at the end of the disposable DB suite, after historical-state seeding.
\set ON_ERROR_STOP on
CREATE TEMP TABLE ownerless_policy_before AS
 SELECT id,version,(SELECT count(*) FROM interaction_audit a WHERE a.target_id=t.id AND a.operation='settings.reconcile') AS audit_count
 FROM interaction_target t WHERE entity_kind='recording' ORDER BY id LIMIT 1;
DO $$ BEGIN ASSERT (SELECT count(*)=1 FROM ownerless_policy_before); END $$;
UPDATE interaction_target SET comment_policy='followers' WHERE id IN (SELECT id FROM ownerless_policy_before);
\ir ../../../sql/2026-09-29_interaction_ownerless_policy.sql
DO $$ BEGIN
 ASSERT (SELECT t.comment_policy='off' AND t.version=b.version+1 FROM interaction_target t JOIN ownerless_policy_before b USING(id)), 'Repair preserves denied access with an explicit mode';
 ASSERT (SELECT count(*)=b.audit_count+1 FROM interaction_audit a JOIN ownerless_policy_before b ON b.id=a.target_id WHERE a.operation='settings.reconcile' GROUP BY b.audit_count), 'Repair retains a single audit';
END $$;
\ir ../../../sql/2026-09-29_interaction_ownerless_policy.sql
DO $$ BEGIN
 ASSERT (SELECT t.comment_policy='off' AND t.version=b.version+1 FROM interaction_target t JOIN ownerless_policy_before b USING(id)), 'Reapplying repair changes no revision';
 ASSERT (SELECT count(*)=b.audit_count+1 FROM interaction_audit a JOIN ownerless_policy_before b ON b.id=a.target_id WHERE a.operation='settings.reconcile' GROUP BY b.audit_count), 'Reapplying repair duplicates no audit';
END $$;
