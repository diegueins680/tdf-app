-- PostgreSQL integration test for the creator badge type cutover.
-- Run only against a disposable pre-cutover database with catalog seeds.

\set ON_ERROR_STOP on
SELECT 'creator-badge-postgres-'||gen_random_uuid() AS run_code,
  'integration-creator-badge-v1-'||gen_random_uuid() AS candidate_revision
\gset
\set safety_threshold 20
SELECT set_config('tdf.test_creator_badge_run_code',:'run_code',FALSE);
SELECT set_config('tdf.test_creator_badge_revision',:'candidate_revision',FALSE);

CREATE TEMP TABLE creator_badge_fixture (
  creator_badge_id bigint PRIMARY KEY,
  original_badge_type text NOT NULL,
  expected_code text NOT NULL
) ON COMMIT PRESERVE ROWS;

WITH parties AS (
  INSERT INTO party(display_name,is_org,created_at)
  VALUES
    ('Creator badge artist '||:'run_code',FALSE,now()),
    ('Creator badge member A '||:'run_code',FALSE,now()),
    ('Creator badge member B '||:'run_code',FALSE,now())
  RETURNING id,display_name
), artist AS (
  SELECT id FROM parties WHERE display_name LIKE 'Creator badge artist %'
), club AS (
  INSERT INTO fan_club(artist_party_id,name,created_at)
  SELECT id,'Creator badge club '||:'run_code',now() FROM artist RETURNING id
), source AS (
  SELECT member.id AS party_id,club.id AS club_id,
    CASE WHEN member.display_name LIKE '% A %' THEN 'TRENDSETTER' ELSE 'og' END AS badge_type
  FROM parties member CROSS JOIN club
  WHERE member.display_name LIKE 'Creator badge member %'
), inserted AS (
  INSERT INTO creator_badge(party_id,club_id,badge_type,awarded_at)
  SELECT party_id,club_id,badge_type,now() FROM source
  RETURNING id,badge_type
)
INSERT INTO creator_badge_fixture
SELECT id,badge_type,lower(btrim(badge_type)) FROM inserted;

\ir ../../sql/2026-08-12_creator_badge_cutover_dry_run.sql
\ir ../../sql/2026-08-12_creator_badge_cutover_apply.sql

DO $assert_first_apply$ DECLARE run_id_value uuid; BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_creator_badge_run_code')
    AND candidate_revision=current_setting('tdf.test_creator_badge_revision') AND NOT dry_run;
  IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type')
    OR NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type_id')
    OR (SELECT count(*) FROM catalog_creator_badge_cutover_source WHERE run_id=run_id_value)<>2
    OR (SELECT count(*) FROM catalog_migration_mapping WHERE run_id=run_id_value AND source_table='creator_badge')<>2
    OR (SELECT sum(usage_count) FROM creator_badge_type)<>2
    OR EXISTS (
      SELECT 1 FROM creator_badge_fixture fixture
      LEFT JOIN creator_badge badge ON badge.id=fixture.creator_badge_id
      LEFT JOIN creator_badge_type item ON item.id=badge.badge_type_id
      WHERE item.code IS DISTINCT FROM fixture.expected_code
    ) THEN RAISE EXCEPTION 'creator badge first apply did not produce exact canonical relationships'; END IF;
END $assert_first_apply$;

CREATE TEMP TABLE creator_badge_digest ON COMMIT PRESERVE ROWS AS
SELECT md5(string_agg(concat_ws(':',badge.id,badge.party_id,badge.club_id,badge.badge_type_id),'|' ORDER BY badge.id)) AS digest
FROM creator_badge badge;
\ir ../../sql/2026-08-12_creator_badge_cutover_apply.sql

DO $assert_rerun$ DECLARE expected_digest text; actual_digest text; BEGIN
  SELECT digest INTO STRICT expected_digest FROM creator_badge_digest;
  SELECT md5(string_agg(concat_ws(':',badge.id,badge.party_id,badge.club_id,badge.badge_type_id),'|' ORDER BY badge.id))
    INTO STRICT actual_digest FROM creator_badge badge;
  IF actual_digest IS DISTINCT FROM expected_digest THEN
    RAISE EXCEPTION 'creator badge rerun changed canonical identities';
  END IF;
END $assert_rerun$;

\ir ../../sql/2026-08-12_creator_badge_cutover_rollback.sql

DO $assert_rollback$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type')
    OR EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type_id')
    OR (SELECT sum(usage_count) FROM creator_badge_type)<>0
    OR EXISTS (
      SELECT 1 FROM creator_badge_fixture fixture
      LEFT JOIN creator_badge badge ON badge.id=fixture.creator_badge_id
      WHERE badge.badge_type IS DISTINCT FROM fixture.original_badge_type
    ) THEN RAISE EXCEPTION 'creator badge rollback did not restore exact source values'; END IF;
END $assert_rollback$;

\ir ../../sql/2026-08-12_creator_badge_cutover_apply.sql

DO $assert_guards$ DECLARE badge_type_id_value uuid; badge_party_id bigint; badge_club_id bigint; BEGIN
  SELECT badge.badge_type_id,badge.party_id,badge.club_id
    INTO STRICT badge_type_id_value,badge_party_id,badge_club_id
  FROM creator_badge badge JOIN creator_badge_fixture fixture ON fixture.creator_badge_id=badge.id LIMIT 1;
  BEGIN
    INSERT INTO creator_badge(party_id,club_id,badge_type_id)
    VALUES(badge_party_id,badge_club_id,'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa');
    RAISE EXCEPTION 'unknown creator badge UUID was accepted';
  EXCEPTION WHEN check_violation THEN NULL; WHEN foreign_key_violation THEN NULL; END;
  BEGIN
    UPDATE creator_badge_type SET active=FALSE WHERE id=badge_type_id_value;
    RAISE EXCEPTION 'referenced creator badge type was deactivated';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    DELETE FROM creator_badge_type WHERE id=badge_type_id_value;
    RAISE EXCEPTION 'creator badge type was hard-deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  BEGIN
    DELETE FROM catalog_creator_badge_cutover_source
    WHERE run_id=(SELECT id FROM catalog_backfill_run
      WHERE run_code=current_setting('tdf.test_creator_badge_run_code')
        AND candidate_revision=current_setting('tdf.test_creator_badge_revision') AND NOT dry_run);
    RAISE EXCEPTION 'creator badge evidence was deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
END $assert_guards$;
