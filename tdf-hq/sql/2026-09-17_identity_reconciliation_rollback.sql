-- Schema rollback is allowed only before use. Routine data undo uses
-- identity_rollback_merge; it preserves the audit history and later edits.
BEGIN;
DO $$
DECLARE item record;
BEGIN
  IF EXISTS(SELECT 1 FROM identity_merge_history) OR EXISTS(SELECT 1 FROM identity_contact_request)
    OR EXISTS(SELECT 1 FROM identity_reconciliation_case) THEN
    RAISE EXCEPTION 'identity reconciliation has evidence; use operation rollback and retain schema';
  END IF;
  FOR item IN SELECT tgrelid::regclass table_name FROM pg_trigger
    WHERE tgname='identity_archive_reference_guard' AND NOT tgisinternal LOOP
    EXECUTE format('DROP TRIGGER identity_archive_reference_guard ON %s',item.table_name);
  END LOOP;
END $$;
DROP TRIGGER IF EXISTS identity_archive_write_guard ON party;
DROP FUNCTION IF EXISTS identity_reject_archived_reference();
DROP FUNCTION IF EXISTS identity_reject_archived_party_write();
DROP FUNCTION IF EXISTS identity_rollback_merge(uuid);
DROP FUNCTION IF EXISTS identity_execute_merge(uuid,uuid,text);
DROP FUNCTION IF EXISTS identity_merge_plan(uuid);
DROP FUNCTION IF EXISTS identity_party_dependencies(bigint);
DROP FUNCTION IF EXISTS identity_create_contact(bigint,text,jsonb);
DROP TABLE identity_party_archive;
DROP TABLE identity_merge_history;
DROP TABLE identity_reconciliation_case;
DROP TABLE identity_contact_request;
COMMIT;
