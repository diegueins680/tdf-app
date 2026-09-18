-- Forward-only repair: the initial migration has already been exercised in staging.
-- Legacy catalog reviewer/approver columns are Party identifiers but have no FKs.
-- Preserve audit history by blocking merges, and reject new archived-party references.
-- Guard the proven catalog columns rather than unrelated entities with similar names.
BEGIN;
CREATE OR REPLACE FUNCTION identity_party_dependencies(candidate bigint)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE item record; n bigint; result jsonb:='[]'::jsonb;
BEGIN
  FOR item IN
    SELECT DISTINCT ns.nspname AS schema_name,cl.relname AS table_name,a.attname AS column_name
    FROM pg_attribute a JOIN pg_class cl ON cl.oid=a.attrelid
    JOIN pg_namespace ns ON ns.oid=cl.relnamespace
    WHERE ns.nspname='public' AND cl.relkind IN ('r','p') AND a.attnum>0 AND NOT a.attisdropped
      AND cl.relname NOT IN ('identity_contact_request','identity_reconciliation_case','identity_merge_history','identity_party_archive')
      AND (EXISTS(SELECT 1 FROM pg_constraint fk WHERE fk.contype='f'
           AND fk.confrelid='party'::regclass AND fk.conrelid=cl.oid AND a.attnum=ANY(fk.conkey))
        OR a.attname ~ '(^|_)(party_id|party_ref)$'
        OR (a.attname IN ('user_id','owner_user_id','claimant_user_id','actor_id','created_by','updated_by','reviewer_id','approver_id','reviewed_by','approved_by')
            AND cl.relname<>'party'))
    ORDER BY 1,2,3
  LOOP
    EXECUTE format('SELECT count(*) FROM %I.%I WHERE %I::text=$1',item.schema_name,item.table_name,item.column_name)
      INTO n USING candidate::text;
    IF n>0 THEN result:=result||jsonb_build_array(jsonb_build_object('table',item.table_name,'column',item.column_name,'count',n)); END IF;
  END LOOP;
  RETURN result;
END $$;

DO $$
DECLARE item record;
BEGIN
  FOR item IN
    SELECT cl.oid::regclass table_name,string_agg(quote_literal(a.attname),',' ORDER BY a.attnum) columns
    FROM pg_attribute a JOIN pg_class cl ON cl.oid=a.attrelid
    WHERE cl.relnamespace='public'::regnamespace AND cl.relkind IN ('r','p') AND a.attnum>0 AND NOT a.attisdropped
      AND cl.relname NOT IN ('identity_contact_request','identity_reconciliation_case','identity_merge_history','identity_party_archive')
      AND (EXISTS(SELECT 1 FROM pg_constraint fk WHERE fk.contype='f' AND fk.confrelid='party'::regclass
           AND fk.conrelid=cl.oid AND a.attnum=ANY(fk.conkey))
        OR a.attname ~ '(^|_)(party_id|party_ref)$'
        OR (cl.relname='catalog_revision' AND a.attname IN ('reviewed_by','approved_by'))
        OR (cl.relname='catalog_audit_event' AND a.attname IN ('reviewer_id','approver_id')))
    GROUP BY cl.oid ORDER BY cl.oid
  LOOP
    EXECUTE format('DROP TRIGGER IF EXISTS identity_archive_reference_guard ON %s',item.table_name);
    EXECUTE format('CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON %s FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference(%s)',item.table_name,item.columns);
  END LOOP;
END $$;
REVOKE ALL ON FUNCTION identity_party_dependencies(bigint) FROM PUBLIC;
COMMIT;
