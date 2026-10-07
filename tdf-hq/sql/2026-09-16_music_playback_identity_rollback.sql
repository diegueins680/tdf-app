BEGIN;
DROP VIEW IF EXISTS music_playback_session_sanitation;
DO $$ BEGIN
  IF to_regprocedure('music_record_playback_event_unbound_v1(uuid,uuid,integer,bigint,text,uuid,uuid,text,bigint,bigint,text,text,timestamp with time zone,jsonb)') IS NOT NULL THEN
    DROP FUNCTION music_record_playback_event(UUID,UUID,INTEGER,BIGINT,TEXT,UUID,UUID,TEXT,BIGINT,BIGINT,TEXT,TEXT,TIMESTAMPTZ,JSONB);
    ALTER FUNCTION music_record_playback_event_unbound_v1(UUID,UUID,INTEGER,BIGINT,TEXT,UUID,UUID,TEXT,BIGINT,BIGINT,TEXT,TEXT,TIMESTAMPTZ,JSONB)
      RENAME TO music_record_playback_event;
  END IF;
END $$;
COMMIT;
