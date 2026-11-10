BEGIN;
-- A later-created service credential table must enforce the same archived-Party
-- write fence as the canonical identity tables. Keep prior migration bytes and
-- historical relationships intact; this installs no permissions or credentials.
SET LOCAL search_path = public, pg_catalog;
LOCK TABLE public.event_watch_client IN SHARE ROW EXCLUSIVE MODE;
DO $$
DECLARE actor_column smallint; item record;
BEGIN
  IF to_regprocedure('public.identity_reject_archived_reference()') IS NULL THEN
    RAISE EXCEPTION 'identity_reject_archived_reference() missing';
  END IF;
  SELECT attnum INTO actor_column FROM pg_attribute
    WHERE attrelid='public.event_watch_client'::regclass
      AND attname='actor_party_id' AND atttypid='bigint'::regtype
      AND attnotnull AND attnum>0 AND NOT attisdropped;
  IF actor_column IS NULL OR NOT EXISTS (
    SELECT 1 FROM pg_constraint fk JOIN pg_attribute target
      ON target.attrelid=fk.confrelid AND target.attnum=fk.confkey[1]
    WHERE fk.contype='f' AND fk.conrelid='public.event_watch_client'::regclass
      AND fk.confrelid='public.party'::regclass
      AND cardinality(fk.conkey)=1 AND cardinality(fk.confkey)=1
      AND fk.conkey[1]=actor_column AND target.attname='id'
  ) THEN
    RAISE EXCEPTION 'event_watch_client.actor_party_id must be a required bigint Party foreign key';
  END IF;
  -- Retain all actual Party-reference columns if a future schema projection has
  -- added another one. Never replace a broader archive guard with a narrower one.
  FOR item IN
    SELECT string_agg(quote_literal(a.attname), ',' ORDER BY a.attnum) AS columns
      FROM pg_attribute a
      WHERE a.attrelid='public.event_watch_client'::regclass
        AND a.attnum>0 AND NOT a.attisdropped
        AND (EXISTS(SELECT 1 FROM pg_constraint fk
              WHERE fk.contype='f' AND fk.confrelid='public.party'::regclass
                AND fk.conrelid=a.attrelid AND a.attnum=ANY(fk.conkey))
             OR a.attname ~ '(^|_)(party_id|party_ref)$')
  LOOP
    IF item.columns IS NULL THEN RAISE EXCEPTION 'service actor reference missing'; END IF;
    EXECUTE 'DROP TRIGGER IF EXISTS identity_archive_reference_guard ON public.event_watch_client';
    EXECUTE format('CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON public.event_watch_client FOR EACH ROW EXECUTE FUNCTION public.identity_reject_archived_reference(%s)', item.columns);
  END LOOP;
END $$;
COMMIT;
