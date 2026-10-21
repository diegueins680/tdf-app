-- Restores the batch-wide scheduler functions and removes the live-version
-- trigger. Release rows repaired by the
-- forward migration are left as they are. Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';

DROP TRIGGER IF EXISTS trg_music_release_live_version ON music_release_version;
DROP FUNCTION IF EXISTS music_release_live_version();
DROP FUNCTION IF EXISTS music_retire_live_versions(UUID, UUID);

CREATE OR REPLACE FUNCTION music_publish_due(batch_size INTEGER DEFAULT 100)
RETURNS TABLE(release_id UUID, version_id UUID) LANGUAGE plpgsql AS $$
DECLARE
  candidate RECORD;
BEGIN
  IF batch_size < 1 OR batch_size > 1000 THEN
    RAISE EXCEPTION 'batch_size must be between 1 and 1000';
  END IF;

  FOR candidate IN
    SELECT v.id, v.release_id, v.replaces_version_id
    FROM music_release_version v
    JOIN music_release r ON r.id = v.release_id
    WHERE v.state = 'scheduled'
      AND v.release_at_utc <= NOW()
      AND (v.embargo_until_utc IS NULL OR v.embargo_until_utc <= NOW())
      AND r.withdrawn_at IS NULL
    ORDER BY v.release_at_utc, v.id
    FOR UPDATE OF v, r SKIP LOCKED
    LIMIT batch_size
  LOOP
    IF candidate.replaces_version_id IS NOT NULL THEN
      UPDATE music_release_version AS replaced
      SET state = 'replacement_pending', updated_at = NOW()
      WHERE replaced.id = candidate.replaces_version_id
        AND replaced.release_id = candidate.release_id
        AND replaced.state = 'published';

      UPDATE music_release_version AS replaced
      SET state = 'takedown_scheduled',
          takedown_at_utc = COALESCE(takedown_at_utc, NOW()),
          takedown_timezone = COALESCE(takedown_timezone, 'UTC'),
          updated_at = NOW()
      WHERE replaced.id = candidate.replaces_version_id
        AND replaced.release_id = candidate.release_id
        AND replaced.state = 'replacement_pending';

      UPDATE music_release_version AS replaced
      SET state = 'withdrawn', updated_at = NOW()
      WHERE replaced.id = candidate.replaces_version_id
        AND replaced.release_id = candidate.release_id
        AND replaced.state = 'takedown_scheduled';
    END IF;

    UPDATE music_release_version
    SET state = 'published', published_at = COALESCE(published_at, NOW()), updated_at = NOW()
    WHERE id = candidate.id AND state = 'scheduled';

    IF FOUND THEN
      UPDATE music_release
      SET published_version_id = candidate.id, updated_at = NOW()
      WHERE id = candidate.release_id AND published_version_id IS DISTINCT FROM candidate.id;
      release_id := candidate.release_id;
      version_id := candidate.id;
      RETURN NEXT;
    END IF;
  END LOOP;
END;
$$;

CREATE OR REPLACE FUNCTION music_withdraw_due(batch_size INTEGER DEFAULT 100)
RETURNS TABLE(release_id UUID, version_id UUID) LANGUAGE plpgsql AS $$
DECLARE
  candidate RECORD;
BEGIN
  IF batch_size < 1 OR batch_size > 1000 THEN
    RAISE EXCEPTION 'batch_size must be between 1 and 1000';
  END IF;

  FOR candidate IN
    SELECT version.id, version.release_id
    FROM music_release_version version
    JOIN music_release release ON release.id = version.release_id
    WHERE version.state = 'takedown_scheduled'
      AND version.takedown_at_utc <= NOW()
    ORDER BY version.takedown_at_utc, version.id
    FOR UPDATE OF version, release SKIP LOCKED
    LIMIT batch_size
  LOOP
    UPDATE music_release_version
    SET state = 'withdrawn', updated_at = NOW()
    WHERE id = candidate.id AND state = 'takedown_scheduled';

    IF FOUND THEN
      UPDATE music_release
      SET published_version_id = CASE WHEN published_version_id = candidate.id THEN NULL ELSE published_version_id END,
          withdrawn_at = CASE WHEN published_version_id = candidate.id THEN COALESCE(withdrawn_at, NOW()) ELSE withdrawn_at END,
          updated_at = NOW()
      WHERE id = candidate.release_id;
      release_id := candidate.release_id;
      version_id := candidate.id;
      RETURN NEXT;
    END IF;
  END LOOP;
END;
$$;

COMMIT;
