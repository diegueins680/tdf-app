-- Guard the nine raw-SQL Party references introduced by event agreements.
-- The model-only registry remains unchanged; actual FKs already participate in
-- identity_party_dependencies. No identity merge or agreement mutation occurs.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$
DECLARE target record; scope text := current_schema(); columns text;
BEGIN
  IF to_regprocedure('identity_reject_archived_reference()') IS NULL THEN
    RAISE EXCEPTION 'Canonical identity archive guard is missing' USING ERRCODE='55000';
  END IF;
  FOR target IN SELECT * FROM (VALUES
    ('event_workspace_plan','recipient_party_id',true),
    ('event_workspace_plan','created_by',true),
    ('event_agreement','recipient_party_id',true),
    ('event_agreement','created_by',true),
    ('event_agreement_version','approved_by',false),
    ('event_agreement_version','created_by',true),
    ('event_agreement_acceptance','accepted_by',true),
    ('event_workspace_action','actor_party_id',true),
    ('event_workspace_command','actor_party_id',true)
  ) AS expected(table_name,column_name,required) LOOP
    IF NOT EXISTS (
      SELECT 1 FROM pg_class relation JOIN pg_namespace ns ON ns.oid=relation.relnamespace
      JOIN pg_attribute attribute ON attribute.attrelid=relation.oid
      WHERE ns.nspname=scope AND relation.relname=target.table_name AND relation.relkind IN ('r','p')
        AND attribute.attname=target.column_name AND attribute.attnum>0 AND NOT attribute.attisdropped
        AND attribute.atttypid='bigint'::regtype AND attribute.attnotnull=target.required
        AND EXISTS (SELECT 1 FROM pg_constraint fk WHERE fk.conrelid=relation.oid AND fk.contype='f'
          AND fk.confrelid='party'::regclass AND fk.conkey=ARRAY[attribute.attnum]::smallint[])
    ) THEN
      RAISE EXCEPTION 'Missing expected Party FK %.%',target.table_name,target.column_name USING ERRCODE='55000';
    END IF;
  END LOOP;
  FOR target IN SELECT relation.oid,relation.relname FROM pg_class relation
    JOIN pg_namespace ns ON ns.oid=relation.relnamespace
    WHERE ns.nspname=scope AND relation.relname IN
      ('event_workspace_plan','event_agreement','event_agreement_version','event_agreement_acceptance','event_workspace_action','event_workspace_command')
    ORDER BY relation.relname LOOP
    EXECUTE format('LOCK TABLE %I.%I IN SHARE ROW EXCLUSIVE MODE',scope,target.relname);
    -- Discover actual Party FKs on just these reviewed tables. Replaying on an
    -- expanded schema preserves additional real Party columns instead of
    -- replacing an existing guard with a narrower hard-coded argument list.
    SELECT string_agg(quote_literal(attribute.attname),',' ORDER BY attribute.attnum) INTO columns
      FROM pg_attribute attribute WHERE attribute.attrelid=target.oid AND attribute.attnum>0 AND NOT attribute.attisdropped
        AND EXISTS (SELECT 1 FROM pg_constraint fk WHERE fk.conrelid=target.oid AND fk.contype='f'
          AND fk.confrelid='party'::regclass AND attribute.attnum=ANY(fk.conkey));
    IF columns IS NULL THEN RAISE EXCEPTION 'Party FK discovery failed' USING ERRCODE='55000'; END IF;
    EXECUTE format('DROP TRIGGER IF EXISTS identity_archive_reference_guard ON %I.%I',scope,target.relname);
    EXECUTE format('CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON %I.%I FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference(%s)',scope,target.relname,columns);
  END LOOP;
END $$;
COMMIT;
