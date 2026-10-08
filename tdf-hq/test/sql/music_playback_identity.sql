BEGIN;
SELECT set_config('test.playback_version', :'version_id', true);
SELECT set_config('test.playback_recording', :'recording_id', true);
SELECT set_config('test.playback_actor', :'actor_id', true);
SELECT set_config('test.playback_other', :'other_actor_id', true);
DO $$
DECLARE
  version_id UUID := current_setting('test.playback_version')::UUID;
  recording_id UUID := current_setting('test.playback_recording')::UUID;
  actor BIGINT := current_setting('test.playback_actor')::BIGINT;
  other_actor BIGINT := current_setting('test.playback_other')::BIGINT;
  event_id UUID := gen_random_uuid(); session_id UUID := gen_random_uuid();
  mixed_session UUID := gen_random_uuid(); anon_session UUID := gen_random_uuid();
  received TIMESTAMPTZ := NOW(); before_count BIGINT;
BEGIN
  IF music_record_playback_event(event_id,session_id,0,actor,NULL,version_id,recording_id,
    'play_start',0,0,NULL,NULL,received,'{}') <> 'inserted' THEN RAISE EXCEPTION 'Insert rejected'; END IF;
  IF music_record_playback_event(event_id,session_id,0,actor,NULL,version_id,recording_id,
    'play_start',0,0,NULL,NULL,received,'{}') <> 'duplicate' THEN RAISE EXCEPTION 'Exact replay rejected'; END IF;
  SELECT count(*) INTO before_count FROM music_playback_event;
  IF music_record_playback_event(event_id,session_id,0,other_actor,NULL,version_id,recording_id,
    'play_start',0,0,NULL,NULL,received,'{}') <> 'conflict' THEN RAISE EXCEPTION 'Cross-owner event replay accepted'; END IF;
  IF music_record_playback_event(event_id,session_id,0,actor,NULL,version_id,recording_id,
    'play_start',1,0,NULL,NULL,received,'{}') <> 'conflict' THEN RAISE EXCEPTION 'Changed payload accepted'; END IF;
  IF music_record_playback_event(gen_random_uuid(),session_id,0,actor,NULL,version_id,recording_id,
    'pause',0,0,NULL,NULL,received,'{}') <> 'conflict' THEN RAISE EXCEPTION 'Sequence collision accepted'; END IF;
  IF music_record_playback_event(gen_random_uuid(),session_id,1,other_actor,NULL,version_id,recording_id,
    'progress',30000,30000,NULL,NULL,received,'{}') <> 'conflict' THEN RAISE EXCEPTION 'Session switched users'; END IF;
  IF music_record_playback_event(gen_random_uuid(),session_id,1,NULL,'synthetic-anonymous',version_id,recording_id,
    'progress',30000,30000,NULL,NULL,received,'{}') <> 'conflict' THEN RAISE EXCEPTION 'Session switched to anonymous'; END IF;
  IF (SELECT count(*) FROM music_playback_event) <> before_count THEN RAISE EXCEPTION 'Rejected event wrote data'; END IF;
  IF music_record_playback_event(gen_random_uuid(),anon_session,0,NULL,'synthetic-anonymous',version_id,recording_id,
    'play_start',0,0,NULL,NULL,received,'{}') <> 'inserted' THEN RAISE EXCEPTION 'Anonymous start rejected'; END IF;
  IF music_record_playback_event(gen_random_uuid(),anon_session,1,NULL,'another-anonymous',version_id,recording_id,
    'pause',0,0,NULL,NULL,received,'{}') <> 'conflict' THEN RAISE EXCEPTION 'Anonymous session switched owner'; END IF;
  IF music_record_playback_event(gen_random_uuid(),anon_session,1,actor,NULL,version_id,recording_id,
    'pause',0,0,NULL,NULL,received,'{}') <> 'conflict' THEN RAISE EXCEPTION 'Anonymous session adopted by user'; END IF;
  -- Explicit legacy fixture via preserved old writer; no UPDATE/DELETE of evidence.
  PERFORM music_record_playback_event_unbound_v1(gen_random_uuid(),mixed_session,0,actor,NULL,
    version_id,recording_id,'play_start',0,0,NULL,NULL,received,'{}');
  PERFORM music_record_playback_event_unbound_v1(gen_random_uuid(),mixed_session,1,other_actor,NULL,
    version_id,recording_id,'pause',0,0,NULL,NULL,received,'{}');
  IF NOT EXISTS (SELECT 1 FROM music_playback_session_sanitation s WHERE s.session_id=mixed_session)
    THEN RAISE EXCEPTION 'Mixed legacy session absent from diagnosis'; END IF;
  IF music_record_playback_event(gen_random_uuid(),mixed_session,2,actor,NULL,version_id,recording_id,
    'progress',30000,30000,NULL,NULL,received,'{}') <> 'conflict' THEN RAISE EXCEPTION 'Mixed legacy session extended'; END IF;
END $$;
ROLLBACK;
