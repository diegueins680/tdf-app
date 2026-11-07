-- Add archived-Party admission to the two raw storage-security relations.
-- Existing credentials/references remain valid historical data.
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE content_storage_credential, content_storage_reference IN SHARE ROW EXCLUSIVE MODE;
DO $$
DECLARE schema_name text := current_schema(); relation_name text; column_name text;
        target regclass; owner_column smallint; party_column smallint; columns text;
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
      WHERE n.nspname=schema_name AND p.proname='identity_reject_archived_reference'
        AND p.pronargs=0 AND p.prorettype='trigger'::regtype) THEN
    RAISE EXCEPTION 'Storage identity admission requires the canonical archived-reference guard' USING ERRCODE='55000';
  END IF;
  SELECT attnum INTO party_column FROM pg_attribute
    WHERE attrelid=to_regclass(format('%I.party',schema_name)) AND attname='id' AND NOT attisdropped;
  FOR relation_name,column_name IN SELECT * FROM (VALUES
      ('content_storage_credential','owner_party_id'),
      ('content_storage_reference','bound_by')) reviewed(relation_name,column_name)
  LOOP
    target := to_regclass(format('%I.%I',schema_name,relation_name));
    IF NOT EXISTS (SELECT 1 FROM pg_class WHERE oid=target AND relkind IN ('r','p')) THEN
      RAISE EXCEPTION 'Storage identity admission requires its original real table %',relation_name USING ERRCODE='55000';
    END IF;
    SELECT attnum INTO owner_column FROM pg_attribute WHERE attrelid=target
      AND attname=column_name AND atttypid='bigint'::regtype AND attnotnull AND NOT attisdropped;
    IF owner_column IS NULL OR party_column IS NULL OR NOT EXISTS (
        SELECT 1 FROM pg_constraint WHERE conrelid=target AND contype='f'
          AND confrelid=to_regclass(format('%I.party',schema_name)) AND convalidated
          AND conkey=ARRAY[owner_column] AND confkey=ARRAY[party_column]) THEN
      RAISE EXCEPTION 'Storage identity admission requires original NOT NULL BIGINT Party FK %.%',relation_name,column_name USING ERRCODE='55000';
    END IF;
    SELECT string_agg(quote_literal(a.attname), ',' ORDER BY a.attname) INTO columns
    FROM pg_attribute a WHERE a.attrelid=target AND NOT a.attisdropped AND EXISTS (
      SELECT 1 FROM pg_constraint c WHERE c.conrelid=target AND c.contype='f'
        AND c.confrelid=to_regclass(format('%I.party',schema_name)) AND a.attnum=ANY(c.conkey));
    IF columns IS NULL THEN RAISE EXCEPTION 'Storage table % has no reviewed Party reference',relation_name USING ERRCODE='55000'; END IF;
    EXECUTE format('DROP TRIGGER IF EXISTS identity_archive_reference_guard ON %s',target);
    EXECUTE format('CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON %s FOR EACH ROW EXECUTE FUNCTION %I.identity_reject_archived_reference(%s)',target,schema_name,columns);
  END LOOP;
END $$;
COMMIT;
