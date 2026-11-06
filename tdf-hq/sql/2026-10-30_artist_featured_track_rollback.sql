BEGIN;
-- Preserve artist selections during binary rollback. Only readers/writers are
-- retired; reapplying restores them without deleting preferences or provenance.
DROP FUNCTION IF EXISTS music_set_artist_featured_track(BIGINT,UUID,BIGINT,TEXT);
DROP FUNCTION IF EXISTS music_artist_featured_track_eligible(BIGINT,UUID,BIGINT,TEXT);
COMMIT;
