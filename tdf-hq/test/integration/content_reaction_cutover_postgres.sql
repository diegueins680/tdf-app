-- PostgreSQL integration test for the Fan Club content reaction cutover.
-- Run only against a disposable pre-cutover database with catalog seeds.

\set ON_ERROR_STOP on
SELECT 'content-reaction-postgres-'||gen_random_uuid() AS run_code,
  'integration-content-reaction-v1-'||gen_random_uuid() AS candidate_revision
\gset
SELECT set_config('tdf.test_content_reaction_run_code',:'run_code',FALSE);
SELECT set_config('tdf.test_content_reaction_revision',:'candidate_revision',FALSE);
\set safety_threshold 20

CREATE TEMP TABLE content_reaction_fixture (
  target_type text NOT NULL,
  target_id bigint NOT NULL,
  reactor_party_id bigint NOT NULL,
  original_reaction text NOT NULL,
  expected_code text NOT NULL,
  PRIMARY KEY(target_type,target_id,reactor_party_id)
) ON COMMIT PRESERVE ROWS;

WITH parties AS (
  INSERT INTO party(display_name,is_org,created_at)
  VALUES
    ('Content reaction artist '||:'run_code',FALSE,now()),
    ('Content reaction reactor post '||:'run_code',FALSE,now()),
    ('Content reaction reactor memory '||:'run_code',FALSE,now())
  RETURNING id,display_name
), artist AS (
  SELECT id FROM parties WHERE display_name LIKE 'Content reaction artist %'
), post_reactor AS (
  SELECT id FROM parties WHERE display_name LIKE 'Content reaction reactor post %'
), memory_reactor AS (
  SELECT id FROM parties WHERE display_name LIKE 'Content reaction reactor memory %'
), club AS (
  INSERT INTO fan_club(artist_party_id,name,created_at)
  SELECT id,'Content reaction club '||:'run_code',now() FROM artist RETURNING id
), post AS (
  INSERT INTO fan_club_post(club_id,fan_party_id,content,created_at)
  SELECT club.id,post_reactor.id,'Fixture post',now() FROM club,post_reactor RETURNING id
), profile AS (
  INSERT INTO fan_club_member_profile(party_id,club_id,joined_at)
  SELECT memory_reactor.id,club.id,now() FROM memory_reactor,club RETURNING id
), memory AS (
  INSERT INTO fan_club_memory(member_profile_id,title,created_at)
  SELECT profile.id,'Fixture memory',now() FROM profile RETURNING id
), source AS (
  SELECT 'post'::text AS target_type,post.id AS target_id,post_reactor.id AS reactor_party_id,
    'FIRE'::text AS original_reaction,'fire'::text AS expected_code
  FROM post,post_reactor
  UNION ALL
  SELECT 'memory',memory.id,memory_reactor.id,'mic_drop','mic_drop'
  FROM memory,memory_reactor
), inserted AS (
  INSERT INTO content_reaction(target_type,target_id,reactor_party_id,reaction,created_at)
  SELECT target_type,target_id,reactor_party_id,original_reaction,now() FROM source
  RETURNING target_type,target_id,reactor_party_id
)
INSERT INTO content_reaction_fixture
SELECT source.* FROM source JOIN inserted USING(target_type,target_id,reactor_party_id);

\ir ../../sql/2026-08-12_content_reaction_cutover_dry_run.sql
\ir ../../sql/2026-08-12_content_reaction_cutover_apply.sql

DO $assert_first_apply$
DECLARE run_id_value uuid;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_content_reaction_run_code')
    AND candidate_revision=current_setting('tdf.test_content_reaction_revision') AND NOT dry_run;
  IF to_regclass('public.content_reaction') IS NOT NULL
    OR to_regclass('public.catalog_content_reaction_legacy_source') IS NULL
    OR (SELECT count(*) FROM catalog_content_reaction_cutover_source WHERE run_id=run_id_value)<>2
    OR (SELECT count(*) FROM catalog_migration_mapping WHERE run_id=run_id_value AND source_table='content_reaction')<>2
    OR (SELECT sum(usage_count) FROM content_reaction_type)<>2
    OR EXISTS (
      SELECT 1 FROM content_reaction_fixture fixture
      LEFT JOIN fan_club_post_reaction post_reaction ON fixture.target_type='post'
        AND post_reaction.post_id=fixture.target_id AND post_reaction.reactor_party_id=fixture.reactor_party_id
      LEFT JOIN fan_club_memory_reaction memory_reaction ON fixture.target_type='memory'
        AND memory_reaction.memory_id=fixture.target_id AND memory_reaction.reactor_party_id=fixture.reactor_party_id
      LEFT JOIN content_reaction_type item ON item.id=COALESCE(post_reaction.reaction_type_id,memory_reaction.reaction_type_id)
      WHERE item.code IS DISTINCT FROM fixture.expected_code
    ) THEN
    RAISE EXCEPTION 'content reaction first apply did not preserve and map both typed targets';
  END IF;
END
$assert_first_apply$;

CREATE TEMP TABLE content_reaction_digest ON COMMIT PRESERVE ROWS AS
SELECT md5(string_agg(identity,'|' ORDER BY identity)) AS digest FROM (
  SELECT concat_ws(':','post',post_id,reactor_party_id,reaction_type_id,id) AS identity FROM fan_club_post_reaction
  UNION ALL
  SELECT concat_ws(':','memory',memory_id,reactor_party_id,reaction_type_id,id) FROM fan_club_memory_reaction
) rows;

\ir ../../sql/2026-08-12_content_reaction_cutover_apply.sql

DO $assert_rerun$
DECLARE expected_digest text; actual_digest text;
BEGIN
  SELECT digest INTO STRICT expected_digest FROM content_reaction_digest;
  SELECT md5(string_agg(identity,'|' ORDER BY identity)) INTO STRICT actual_digest FROM (
    SELECT concat_ws(':','post',post_id,reactor_party_id,reaction_type_id,id) AS identity FROM fan_club_post_reaction
    UNION ALL
    SELECT concat_ws(':','memory',memory_id,reactor_party_id,reaction_type_id,id) FROM fan_club_memory_reaction
  ) rows;
  IF actual_digest IS DISTINCT FROM expected_digest THEN
    RAISE EXCEPTION 'content reaction rerun changed canonical identities';
  END IF;
END
$assert_rerun$;

\ir ../../sql/2026-08-12_content_reaction_cutover_rollback.sql

DO $assert_rollback$
BEGIN
  IF to_regclass('public.content_reaction') IS NULL
    OR to_regclass('public.catalog_content_reaction_legacy_source') IS NOT NULL
    OR to_regclass('public.fan_club_post_reaction') IS NOT NULL
    OR to_regclass('public.fan_club_memory_reaction') IS NOT NULL
    OR (SELECT sum(usage_count) FROM content_reaction_type)<>0
    OR EXISTS (
      SELECT 1 FROM content_reaction_fixture fixture
      LEFT JOIN content_reaction reaction USING(target_type,target_id,reactor_party_id)
      WHERE reaction.reaction IS DISTINCT FROM fixture.original_reaction
    ) THEN
    RAISE EXCEPTION 'content reaction rollback did not restore exact source table';
  END IF;
END
$assert_rollback$;

\ir ../../sql/2026-08-12_content_reaction_cutover_apply.sql

DO $assert_guards$
DECLARE post_id_value bigint; party_id_value bigint; reaction_type_id_value uuid;
BEGIN
  SELECT target_id,reactor_party_id INTO STRICT post_id_value,party_id_value
  FROM content_reaction_fixture WHERE target_type='post';
  SELECT reaction_type_id INTO STRICT reaction_type_id_value
  FROM fan_club_post_reaction WHERE post_id=post_id_value AND reactor_party_id=party_id_value;
  BEGIN
    INSERT INTO fan_club_post_reaction(post_id,reactor_party_id,reaction_type_id)
    VALUES(post_id_value,(SELECT id FROM party WHERE id<>party_id_value LIMIT 1),'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa');
    RAISE EXCEPTION 'unknown content reaction UUID was accepted';
  EXCEPTION WHEN check_violation THEN NULL; WHEN foreign_key_violation THEN NULL; END;
  BEGIN
    UPDATE content_reaction_type SET active=FALSE WHERE id=reaction_type_id_value;
    RAISE EXCEPTION 'referenced content reaction type was deactivated';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    DELETE FROM content_reaction_type WHERE id=reaction_type_id_value;
    RAISE EXCEPTION 'content reaction type was hard-deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  BEGIN
    DELETE FROM catalog_content_reaction_cutover_source
    WHERE run_id=(SELECT id FROM catalog_backfill_run
      WHERE run_code=current_setting('tdf.test_content_reaction_run_code')
        AND candidate_revision=current_setting('tdf.test_content_reaction_revision') AND NOT dry_run);
    RAISE EXCEPTION 'content reaction evidence was deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
END
$assert_guards$;
