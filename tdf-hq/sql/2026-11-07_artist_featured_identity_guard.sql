-- Protect the new raw featured-preference Party reference without changing the
-- model registry, preferences, media membership or any existing migration.
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE music_artist_featured_track IN SHARE ROW EXCLUSIVE MODE;
DO $$
DECLARE schema_name text := current_schema(); target regclass;
        artist_column smallint; party_column smallint; columns text;
BEGIN
  target := to_regclass(format('%I.music_artist_featured_track', schema_name));
  IF NOT EXISTS (SELECT 1 FROM pg_class WHERE oid=target AND relkind IN ('r','p')) THEN
    RAISE EXCEPTION 'Featured preference requires its original real table' USING ERRCODE='55000';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
      WHERE n.nspname=schema_name AND p.proname='identity_reject_archived_reference'
        AND p.pronargs=0 AND p.prorettype='trigger'::regtype) THEN
    RAISE EXCEPTION 'Featured preference requires the canonical archived-reference guard' USING ERRCODE='55000';
  END IF;
  SELECT attnum INTO artist_column FROM pg_attribute WHERE attrelid=target
    AND attname='artist_party_id' AND atttypid='bigint'::regtype AND attnotnull AND NOT attisdropped;
  SELECT attnum INTO party_column FROM pg_attribute
    WHERE attrelid=to_regclass(format('%I.party',schema_name)) AND attname='id' AND NOT attisdropped;
  IF artist_column IS NULL OR party_column IS NULL OR NOT EXISTS (
      SELECT 1 FROM pg_constraint WHERE conrelid=target AND contype='f'
        AND confrelid=to_regclass(format('%I.party',schema_name))
        AND conkey=ARRAY[artist_column] AND confkey=ARRAY[party_column]) THEN
    RAISE EXCEPTION 'Featured preference requires the original NOT NULL BIGINT Party FK' USING ERRCODE='55000';
  END IF;
  -- Keep every actual Party FK on this locked table protected if later reviewed
  -- schema versions append references; do not narrow discovery to a stale list.
  SELECT string_agg(quote_literal(a.attname), ',' ORDER BY a.attname) INTO columns
  FROM pg_attribute a WHERE a.attrelid=target AND NOT a.attisdropped AND EXISTS (
    SELECT 1 FROM pg_constraint c WHERE c.conrelid=target AND c.contype='f'
      AND c.confrelid=to_regclass(format('%I.party',schema_name)) AND a.attnum=ANY(c.conkey));
  IF columns IS NULL THEN RAISE EXCEPTION 'Featured preference has no reviewed Party reference' USING ERRCODE='55000'; END IF;
  EXECUTE format('DROP TRIGGER IF EXISTS identity_archive_reference_guard ON %s',target);
  EXECUTE format('CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON %s FOR EACH ROW EXECUTE FUNCTION %I.identity_reject_archived_reference(%s)',target,schema_name,columns);
END $$;
COMMIT;
