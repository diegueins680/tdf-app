-- One bad scheduled version stopped every music publication and withdrawal:
-- music_publish_due ran all candidates in one transaction, so a candidate that
-- failed at publication (a second published version of the same release, a
-- submission rule that no longer holds) aborted the whole batch on every run.
-- A release whose live version had been withdrawn also kept withdrawn_at
-- forever, so a scheduled correction stayed 'scheduled' silently, and a
-- withdrawal done through the API left the release pointing at the withdrawn
-- version. These definitions isolate each candidate, retire every other live
-- version when one publishes, and keep the release pointer from one trigger.
-- Safe to apply again: functions and triggers are replaced, repairs find nothing.
BEGIN;
SET LOCAL lock_timeout = '10s';

-- The release points at its live version: set when a version is published,
-- cleared when that version leaves the live states (published, suspended,
-- replacement_pending, takedown_scheduled), whichever path moved it.
CREATE OR REPLACE FUNCTION music_release_live_version()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.state = 'published' THEN
    UPDATE music_release
    SET published_version_id = NEW.id, withdrawn_at = NULL, updated_at = NOW()
    WHERE id = NEW.release_id
      AND (published_version_id IS DISTINCT FROM NEW.id OR withdrawn_at IS NOT NULL);
  ELSIF NEW.state NOT IN ('replacement_pending','takedown_scheduled','suspended') THEN
    UPDATE music_release
    SET published_version_id = NULL, withdrawn_at = COALESCE(withdrawn_at, NOW()), updated_at = NOW()
    WHERE id = NEW.release_id AND published_version_id = NEW.id;
  END IF;
  RETURN NEW;
END;
$$;

CREATE OR REPLACE TRIGGER trg_music_release_live_version
  AFTER UPDATE OF state ON music_release_version
  FOR EACH ROW
  WHEN (OLD.state IS DISTINCT FROM NEW.state)
  EXECUTE FUNCTION music_release_live_version();

-- Every other version of the release that was published and is still
-- published, replacement_pending or takedown_scheduled leaves through the
-- lifecycle (published -> replacement_pending -> takedown_scheduled ->
-- withdrawn). Versions that were never published and suspended versions stay.
CREATE OR REPLACE FUNCTION music_retire_live_versions(target_release_id UUID, keep_version_id UUID)
RETURNS void LANGUAGE plpgsql AS $$
BEGIN
  UPDATE music_release_version
  SET state = 'replacement_pending', updated_at = NOW()
  WHERE release_id = target_release_id AND id <> keep_version_id
    AND state = 'published' AND published_at IS NOT NULL;

  UPDATE music_release_version
  SET state = 'takedown_scheduled',
      takedown_at_utc = COALESCE(takedown_at_utc, NOW()),
      takedown_timezone = COALESCE(takedown_timezone, 'UTC'),
      updated_at = NOW()
  WHERE release_id = target_release_id AND id <> keep_version_id
    AND state = 'replacement_pending' AND published_at IS NOT NULL;

  UPDATE music_release_version
  SET state = 'withdrawn', updated_at = NOW()
  WHERE release_id = target_release_id AND id <> keep_version_id
    AND state = 'takedown_scheduled' AND published_at IS NOT NULL;
END;
$$;

-- Each candidate publishes or fails on its own. A candidate that fails for a
-- transient reason (deadlock, serialization failure, lock timeout: the API
-- handler locks the version row before the release row, the scheduler the
-- other way round) stays scheduled for the next run. Any other failure
-- suspends the candidate with the error in its audit trail, so the next run
-- does not retry it; the other candidates still publish either way.
CREATE OR REPLACE FUNCTION music_publish_due(batch_size INTEGER DEFAULT 100)
RETURNS TABLE(release_id UUID, version_id UUID) LANGUAGE plpgsql AS $$
DECLARE
  candidate RECORD;
  failure_state TEXT;
  failure_message TEXT;
BEGIN
  IF batch_size < 1 OR batch_size > 1000 THEN
    RAISE EXCEPTION 'batch_size must be between 1 and 1000';
  END IF;

  FOR candidate IN
    SELECT v.id, v.release_id
    FROM music_release_version v
    JOIN music_release r ON r.id = v.release_id
    WHERE v.state = 'scheduled'
      AND v.release_at_utc <= NOW()
      AND (v.embargo_until_utc IS NULL OR v.embargo_until_utc <= NOW())
    ORDER BY v.release_at_utc, v.id
    FOR UPDATE OF v, r SKIP LOCKED
    LIMIT batch_size
  LOOP
    BEGIN
      PERFORM music_retire_live_versions(candidate.release_id, candidate.id);
      UPDATE music_release_version
      SET state = 'published', published_at = COALESCE(published_at, NOW()), updated_at = NOW()
      WHERE id = candidate.id AND state = 'scheduled';
      IF FOUND THEN
        release_id := candidate.release_id;
        version_id := candidate.id;
        RETURN NEXT;
      END IF;
    EXCEPTION WHEN OTHERS THEN
      GET STACKED DIAGNOSTICS failure_state = RETURNED_SQLSTATE, failure_message = MESSAGE_TEXT;
      IF failure_state LIKE '40%' OR failure_state = '55P03' THEN
        RAISE WARNING 'music publication of version % postponed (%): %', candidate.id, failure_state, failure_message;
        CONTINUE;
      END IF;
      BEGIN
        UPDATE music_release_version
        SET state = 'suspended', updated_at = NOW()
        WHERE id = candidate.id AND state = 'scheduled';
        INSERT INTO music_release_audit_event(
          release_id, release_version_id, actor_party_id, event_type, prior_state, next_state, data
        ) VALUES (
          candidate.release_id, candidate.id, NULL, 'publication_failed', 'scheduled', 'suspended',
          jsonb_build_object('sqlstate', failure_state, 'message', failure_message)
        );
      EXCEPTION WHEN OTHERS THEN
        RAISE WARNING 'music publication of version % failed (%) and could not be suspended: %',
          candidate.id, failure_state, SQLSTATE;
      END;
    END;
  END LOOP;
END;
$$;

-- A failed withdrawal moves behind the other eligible versions using updated_at
-- as its last-attempt marker. Even a one-row batch therefore makes progress past
-- a permanently failing version. Non-transient failures are still recorded once.
CREATE OR REPLACE FUNCTION music_withdraw_due(batch_size INTEGER DEFAULT 100)
RETURNS TABLE(release_id UUID, version_id UUID) LANGUAGE plpgsql AS $$
DECLARE
  candidate RECORD;
  failure_state TEXT;
  failure_message TEXT;
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
    ORDER BY version.updated_at, version.takedown_at_utc, version.id
    FOR UPDATE OF version, release SKIP LOCKED
    LIMIT batch_size
  LOOP
    BEGIN
      UPDATE music_release_version
      SET state = 'withdrawn', updated_at = NOW()
      WHERE id = candidate.id AND state = 'takedown_scheduled';
      IF FOUND THEN
        release_id := candidate.release_id;
        version_id := candidate.id;
        RETURN NEXT;
      END IF;
    EXCEPTION WHEN OTHERS THEN
      GET STACKED DIAGNOSTICS failure_state = RETURNED_SQLSTATE, failure_message = MESSAGE_TEXT;
      -- This update is outside the audit subtransaction: an audit-write failure
      -- must not restore the poison row's old scheduling priority.
      UPDATE music_release_version
      SET updated_at = clock_timestamp()
      WHERE id = candidate.id AND state = 'takedown_scheduled';
      IF failure_state LIKE '40%' OR failure_state = '55P03' THEN
        RAISE WARNING 'music withdrawal of version % postponed (%): %', candidate.id, failure_state, failure_message;
        CONTINUE;
      END IF;
      BEGIN
        INSERT INTO music_release_audit_event(
          release_id, release_version_id, actor_party_id, event_type, prior_state, next_state, data
        )
        SELECT candidate.release_id, candidate.id, NULL, 'withdrawal_failed', 'takedown_scheduled', 'takedown_scheduled',
          jsonb_build_object('sqlstate', failure_state, 'message', failure_message)
        WHERE NOT EXISTS (
          SELECT 1 FROM music_release_audit_event latest
          WHERE latest.release_id = candidate.release_id AND latest.release_version_id = candidate.id
          ORDER BY latest.occurred_at DESC, latest.id DESC
          LIMIT 1
        ) OR (
          SELECT latest.event_type <> 'withdrawal_failed' OR latest.data->>'sqlstate' IS DISTINCT FROM failure_state
          FROM music_release_audit_event latest
          WHERE latest.release_id = candidate.release_id AND latest.release_version_id = candidate.id
          ORDER BY latest.occurred_at DESC, latest.id DESC
          LIMIT 1
        );
      EXCEPTION WHEN OTHERS THEN
        RAISE WARNING 'music withdrawal of version % failed (%) and could not be recorded: %',
          candidate.id, failure_state, SQLSTATE;
      END;
    END;
  END LOOP;
END;
$$;

-- Repairs of rows written before these definitions.
-- 1. The release points at a version that is no longer live.
UPDATE music_release release
SET published_version_id = NULL, withdrawn_at = COALESCE(withdrawn_at, NOW()), updated_at = NOW()
WHERE published_version_id IS NOT NULL
  AND EXISTS (
    SELECT 1 FROM music_release_version version
    WHERE version.id = release.published_version_id
      AND version.state NOT IN ('published','replacement_pending','takedown_scheduled','suspended')
  );

-- 2. A version is published but the release does not point at it, or the
--    release is still marked withdrawn although it has a published version.
UPDATE music_release release
SET published_version_id = version.id, withdrawn_at = NULL, updated_at = NOW()
FROM music_release_version version
WHERE version.release_id = release.id
  AND version.state = 'published'
  AND (release.published_version_id IS DISTINCT FROM version.id OR release.withdrawn_at IS NOT NULL);

COMMIT;
