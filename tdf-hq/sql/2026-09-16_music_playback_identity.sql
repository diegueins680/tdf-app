BEGIN;

-- Preserve the prior implementation for exact rollback, without rewriting events.
DO $$ BEGIN
  IF to_regprocedure('music_record_playback_event_unbound_v1(uuid,uuid,integer,bigint,text,uuid,uuid,text,bigint,bigint,text,text,timestamp with time zone,jsonb)') IS NULL THEN
    ALTER FUNCTION music_record_playback_event(UUID,UUID,INTEGER,BIGINT,TEXT,UUID,UUID,TEXT,BIGINT,BIGINT,TEXT,TEXT,TIMESTAMPTZ,JSONB)
      RENAME TO music_record_playback_event_unbound_v1;
  END IF;
END $$;

CREATE OR REPLACE FUNCTION music_record_playback_event(
  p_event_id UUID,p_session_id UUID,p_sequence_number INTEGER,p_party_id BIGINT,
  p_anonymous_id_hash TEXT,p_release_version_id UUID,p_recording_id UUID,
  p_event_type TEXT,p_position_ms BIGINT,p_listened_delta_ms BIGINT,
  p_quality TEXT,p_territory_code TEXT,p_occurred_at TIMESTAMPTZ,p_metadata JSONB
) RETURNS TEXT LANGUAGE plpgsql AS $$
DECLARE prior music_playback_event%ROWTYPE;
BEGIN
  IF (p_party_id IS NOT NULL)::INTEGER + (p_anonymous_id_hash IS NOT NULL)::INTEGER <> 1 THEN
    RETURN 'conflict';
  END IF;
  -- Bind global event IDs and whole sessions, not merely session/recording.
  PERFORM pg_advisory_xact_lock(hashtextextended('music-event:' || p_event_id::TEXT,0));
  PERFORM pg_advisory_xact_lock(hashtextextended('music-session:' || p_session_id::TEXT,0));
  SELECT * INTO prior FROM music_playback_event WHERE event_id=p_event_id;
  IF FOUND THEN
    IF ROW(prior.session_id,prior.sequence_number,prior.party_id,prior.anonymous_id_hash,
      prior.release_version_id,prior.recording_id,prior.event_type,prior.position_ms,
      prior.listened_delta_ms,prior.quality,prior.territory_code,prior.occurred_at,prior.metadata)
      IS NOT DISTINCT FROM ROW(p_session_id,p_sequence_number,p_party_id,p_anonymous_id_hash,
      p_release_version_id,p_recording_id,p_event_type,p_position_ms,p_listened_delta_ms,
      p_quality,p_territory_code,p_occurred_at,COALESCE(p_metadata,'{}'::JSONB)) THEN
      RETURN 'duplicate';
    END IF;
    RETURN 'conflict';
  END IF;
  IF EXISTS (SELECT 1 FROM music_playback_event e WHERE e.session_id=p_session_id
      AND (e.party_id IS DISTINCT FROM p_party_id
        OR e.anonymous_id_hash IS DISTINCT FROM p_anonymous_id_hash
        OR e.sequence_number=p_sequence_number)) THEN
    RETURN 'conflict';
  END IF;
  RETURN music_record_playback_event_unbound_v1(p_event_id,p_session_id,p_sequence_number,
    p_party_id,p_anonymous_id_hash,p_release_version_id,p_recording_id,p_event_type,
    p_position_ms,p_listened_delta_ms,p_quality,p_territory_code,p_occurred_at,p_metadata);
END;
$$;

-- Administrative diagnosis only: legacy mixed sessions remain immutable.
CREATE OR REPLACE VIEW music_playback_session_sanitation AS
SELECT session_id,count(*) AS event_count,MIN(received_at) AS first_received_at,
  MAX(received_at) AS last_received_at
FROM music_playback_event
GROUP BY session_id
HAVING count(DISTINCT ROW(party_id,anonymous_id_hash)) > 1;
COMMIT;
