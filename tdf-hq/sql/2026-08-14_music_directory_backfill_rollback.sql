-- Recovery for an unmodified backfill only. Abort instead of deleting a
-- profile that has since received user-authored content or version changes.
BEGIN;

DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM directory_profile profile
    JOIN directory_legacy_link link ON link.profile_id=profile.id
    WHERE link.backfill_run_id=directory_stable_uuid('directory-backfill','legacy-v1:apply')
      AND (profile.version<>1 OR EXISTS (SELECT 1 FROM classified WHERE author_profile_id=profile.id)
        OR EXISTS (SELECT 1 FROM directory_interaction WHERE profile_a_id=profile.id OR profile_b_id=profile.id))
  ) THEN
    RAISE EXCEPTION 'backfill rollback refused: a migrated profile has user-authored changes';
  END IF;
END
$$;

DELETE FROM directory_search_document search
USING directory_legacy_link link
WHERE link.backfill_run_id=directory_stable_uuid('directory-backfill','legacy-v1:apply')
  AND search.entity_kind='profile' AND search.entity_id=link.profile_id::text;
DELETE FROM directory_search_document WHERE entity_kind IN ('event','venue');

CREATE TEMP TABLE directory_profiles_to_reverse ON COMMIT DROP AS
SELECT DISTINCT link.profile_id
FROM directory_legacy_link link
WHERE link.backfill_run_id=directory_stable_uuid('directory-backfill','legacy-v1:apply');

DELETE FROM directory_legacy_link
WHERE backfill_run_id=directory_stable_uuid('directory-backfill','legacy-v1:apply');

-- Preserve the reconciliation ledger and its evidence while releasing the FK
-- to the additive profiles being reversed. Reapplying the backfill restores
-- the deterministic target and disposition on the same rows.
UPDATE directory_backfill_mapping
SET target_profile_id=NULL,
    disposition='reversed',
    reason_code='backfill-reversed',
    evidence=evidence || jsonb_build_object('reversedAt',now())
WHERE backfill_run_id=directory_stable_uuid('directory-backfill','legacy-v1:apply')
  AND target_profile_id IN (SELECT profile_id FROM directory_profiles_to_reverse);

DELETE FROM directory_profile profile
USING directory_profiles_to_reverse target
WHERE target.profile_id=profile.id;

UPDATE directory_backfill_run SET status='reversed',completed_at=now()
WHERE id=directory_stable_uuid('directory-backfill','legacy-v1:apply');

COMMIT;
