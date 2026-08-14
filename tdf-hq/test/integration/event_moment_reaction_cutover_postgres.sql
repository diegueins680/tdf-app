-- PostgreSQL integration test for the event-moment reaction cutover.
-- Run only against a disposable pre-cutover database with catalog seeds.
-- The production scripts commit and preserve immutable evidence by design.

\set ON_ERROR_STOP on

SELECT 'event-moment-reaction-postgres-' || gen_random_uuid() AS run_code,
  'integration-event-moment-reaction-v1-' || gen_random_uuid() AS candidate_revision
\gset
SELECT set_config('tdf.test_event_moment_reaction_run_code', :'run_code', FALSE);
SELECT set_config('tdf.test_event_moment_reaction_revision', :'candidate_revision', FALSE);
\set safety_threshold 20

CREATE TEMP TABLE event_moment_reaction_fixture (
  moment_id bigint NOT NULL,
  reactor_party_id text NOT NULL,
  original_reaction text NOT NULL,
  expected_code text NOT NULL
) ON COMMIT PRESERVE ROWS;

WITH event_type_row AS (
  SELECT item.id
  FROM event_type item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id
  JOIN workflow_state state ON state.id=item.workflow_state_id
  WHERE catalog.code='event-types' AND catalog.active AND item.active
    AND state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
  ORDER BY item.sort_order,item.id LIMIT 1
), workflow_state_row AS (
  SELECT state.id
  FROM workflow_state state
  JOIN workflow_definition workflow ON workflow.id=state.workflow_id
  WHERE workflow.code='social-event-lifecycle' AND state.active
  ORDER BY state.sort_order,state.id LIMIT 1
), currency_row AS (
  SELECT id FROM currency_reference WHERE active ORDER BY code,id LIMIT 1
), event_row AS (
  INSERT INTO social_event (
    title,event_type_id,workflow_state_id,start_time,end_time,currency_id
  )
  SELECT 'Event moment reaction integration ' || :'run_code',event_type_row.id,
    workflow_state_row.id,now(),now()+interval '1 hour',currency_row.id
  FROM event_type_row,workflow_state_row,currency_row
  RETURNING id
), moment_row AS (
  INSERT INTO event_moment (event_id,author_name,media_url,media_type)
  SELECT id,'Integration fixture','https://example.invalid/catalog-reaction.jpg','image'
  FROM event_row RETURNING id
), source_rows AS (
  SELECT moment_row.id AS moment_id, source.reactor_party_id,
    source.original_reaction,source.expected_code
  FROM moment_row CROSS JOIN (VALUES
    ('fixture-fire','fire','fire'),
    ('fixture-heart','HEART','love'),
    ('fixture-clap','clap','applause')
  ) source(reactor_party_id,original_reaction,expected_code)
), inserted AS (
  INSERT INTO event_moment_reaction (moment_id,reaction,reactor_party_id)
  SELECT moment_id,original_reaction,reactor_party_id FROM source_rows
  RETURNING moment_id,reaction,reactor_party_id
)
INSERT INTO event_moment_reaction_fixture
SELECT source_rows.moment_id,source_rows.reactor_party_id,
  source_rows.original_reaction,source_rows.expected_code
FROM source_rows JOIN inserted USING (moment_id,reactor_party_id);

\ir ../../sql/2026-08-12_event_moment_reaction_cutover_dry_run.sql
\ir ../../sql/2026-08-12_event_moment_reaction_cutover_apply.sql

DO $assert_first_apply$
DECLARE run_id_value uuid;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_event_moment_reaction_run_code')
    AND candidate_revision=current_setting('tdf.test_event_moment_reaction_revision')
    AND NOT dry_run;
  IF (SELECT count(*) FROM catalog_event_moment_reaction_cutover_source
      WHERE run_id=run_id_value)<>3
    OR (SELECT count(*) FROM catalog_migration_mapping
      WHERE run_id=run_id_value AND source_table='event_moment_reaction')<>3 THEN
    RAISE EXCEPTION 'first reaction apply did not preserve three source mappings';
  END IF;
  IF EXISTS (
    SELECT 1 FROM event_moment_reaction_fixture fixture
    JOIN event_moment_reaction reaction
      ON reaction.moment_id=fixture.moment_id
      AND reaction.reactor_party_id=fixture.reactor_party_id
    JOIN reaction_type item ON item.id=reaction.reaction_type_id
    WHERE reaction.reaction IS NOT NULL OR item.code<>fixture.expected_code
  ) OR (SELECT count(*) FROM event_moment_reaction_fixture fixture
    JOIN event_moment_reaction reaction
      ON reaction.moment_id=fixture.moment_id
      AND reaction.reactor_party_id=fixture.reactor_party_id)<>3 THEN
    RAISE EXCEPTION 'reaction apply produced incorrect canonical identities';
  END IF;
END
$assert_first_apply$;

CREATE TEMP TABLE event_moment_reaction_digest ON COMMIT PRESERVE ROWS AS
SELECT md5(string_agg(
  concat_ws('|',reaction.id,reaction.reaction_type_id,reaction.reactor_party_id),
  ',' ORDER BY reaction.reactor_party_id
)) AS digest
FROM event_moment_reaction_fixture fixture
JOIN event_moment_reaction reaction
  ON reaction.moment_id=fixture.moment_id
  AND reaction.reactor_party_id=fixture.reactor_party_id;

\ir ../../sql/2026-08-12_event_moment_reaction_cutover_apply.sql

DO $assert_rerun$
DECLARE run_id_value uuid; expected_digest text; actual_digest text;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_event_moment_reaction_run_code')
    AND candidate_revision=current_setting('tdf.test_event_moment_reaction_revision')
    AND NOT dry_run;
  SELECT digest INTO STRICT expected_digest FROM event_moment_reaction_digest;
  SELECT md5(string_agg(
    concat_ws('|',reaction.id,reaction.reaction_type_id,reaction.reactor_party_id),
    ',' ORDER BY reaction.reactor_party_id
  )) INTO STRICT actual_digest
  FROM event_moment_reaction_fixture fixture
  JOIN event_moment_reaction reaction
    ON reaction.moment_id=fixture.moment_id
    AND reaction.reactor_party_id=fixture.reactor_party_id;
  IF actual_digest IS DISTINCT FROM expected_digest
    OR (SELECT count(*) FROM catalog_event_moment_reaction_cutover_source
      WHERE run_id=run_id_value)<>3
    OR (SELECT count(*) FROM catalog_migration_mapping
      WHERE run_id=run_id_value AND source_table='event_moment_reaction')<>3 THEN
    RAISE EXCEPTION 'reaction rerun changed identities or duplicated evidence';
  END IF;
END
$assert_rerun$;

\ir ../../sql/2026-08-12_event_moment_reaction_cutover_rollback.sql

DO $assert_rollback$
BEGIN
  IF EXISTS (
    SELECT 1 FROM event_moment_reaction_fixture fixture
    JOIN event_moment_reaction reaction
      ON reaction.moment_id=fixture.moment_id
      AND reaction.reactor_party_id=fixture.reactor_party_id
    WHERE reaction.reaction<>fixture.original_reaction OR reaction.reaction_type_id IS NOT NULL
  ) OR (SELECT count(*) FROM event_moment_reaction_fixture fixture
    JOIN event_moment_reaction reaction
      ON reaction.moment_id=fixture.moment_id
      AND reaction.reactor_party_id=fixture.reactor_party_id)<>3 THEN
    RAISE EXCEPTION 'reaction rollback did not restore exact source values';
  END IF;
END
$assert_rollback$;

\ir ../../sql/2026-08-12_event_moment_reaction_cutover_apply.sql

DO $assert_guards$
DECLARE moment_id_value bigint; reaction_type_id_value uuid;
BEGIN
  SELECT moment_id INTO STRICT moment_id_value FROM event_moment_reaction_fixture LIMIT 1;
  SELECT reaction.reaction_type_id INTO STRICT reaction_type_id_value
  FROM event_moment_reaction reaction
  JOIN event_moment_reaction_fixture fixture
    ON fixture.moment_id=reaction.moment_id
    AND fixture.reactor_party_id=reaction.reactor_party_id
  LIMIT 1;
  BEGIN
    INSERT INTO event_moment_reaction (moment_id,reaction,reactor_party_id)
    VALUES (moment_id_value,'fire','legacy-string-after-cutover');
    RAISE EXCEPTION 'legacy reaction string was accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    INSERT INTO event_moment_reaction (moment_id,reaction_type_id,reactor_party_id)
    VALUES (moment_id_value,'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa','unknown-id');
    RAISE EXCEPTION 'unknown reaction UUID was accepted';
  EXCEPTION WHEN check_violation THEN NULL; WHEN foreign_key_violation THEN NULL; END;
  BEGIN
    UPDATE reaction_type SET active=FALSE WHERE id=reaction_type_id_value;
    RAISE EXCEPTION 'referenced reaction type was deactivated';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    DELETE FROM catalog_event_moment_reaction_cutover_source
    WHERE run_id=(SELECT id FROM catalog_backfill_run
      WHERE run_code=current_setting('tdf.test_event_moment_reaction_run_code')
        AND candidate_revision=current_setting('tdf.test_event_moment_reaction_revision')
        AND NOT dry_run);
    RAISE EXCEPTION 'reaction source evidence was hard-deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  RAISE NOTICE 'Event-moment reaction dry-run/apply/rerun/rollback/reapply and negative guards passed';
END
$assert_guards$;
