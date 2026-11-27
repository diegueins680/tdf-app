-- COMMUNITY-ARTIST-SIMILARITY-001: fixture for scripts/test-artist-similarity-postgres.sh.
-- Only the columns the similarity queries and the listing query read, with the
-- migrated column types. tdf-hq/sql/2026-11-21_artist_similarity.sql is applied
-- on top of it unchanged.
\set ON_ERROR_STOP on
BEGIN;
CREATE TABLE party (id BIGINT PRIMARY KEY);
INSERT INTO party (id) SELECT g FROM generate_series(1, 9) g;
INSERT INTO party (id) SELECT g FROM generate_series(100, 106) g;

-- Artists 1..7. Artist 7 has one directory identity and it is a draft: hidden.
CREATE TABLE artist_profile (id BIGSERIAL PRIMARY KEY, artist_party_id BIGINT NOT NULL UNIQUE REFERENCES party(id));
CREATE TABLE directory_profile (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  subject_party_id BIGINT NOT NULL REFERENCES party(id),
  profile_status TEXT NOT NULL, visibility TEXT NOT NULL, moderation_status TEXT NOT NULL,
  canonical_profile_id UUID REFERENCES directory_profile(id));
CREATE TABLE directory_legacy_link (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  legacy_kind TEXT NOT NULL, legacy_id TEXT NOT NULL);
INSERT INTO artist_profile (id, artist_party_id) SELECT 500 + g, g FROM generate_series(1, 7) g;
INSERT INTO directory_profile (subject_party_id, profile_status, visibility, moderation_status)
  VALUES (7, 'draft', 'public', 'allowed');

CREATE TABLE fan_follow (
  id BIGSERIAL PRIMARY KEY, fan_party_id BIGINT NOT NULL, artist_party_id BIGINT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE (fan_party_id, artist_party_id));
-- Six fans follow artist 1; 101 and 102 also follow 2; 105 follows only 4.
-- 100, 101 and 102 follow the hidden artist 7.
INSERT INTO fan_follow (fan_party_id, artist_party_id) VALUES
  (100,1),(101,1),(102,1),(103,1),(104,1),(106,1),(101,2),(102,2),(105,4),(100,7),(101,7),(102,7);

CREATE TABLE social_v2_preference (
  party_id BIGINT PRIMARY KEY REFERENCES party(id),
  personalized BOOLEAN NOT NULL DEFAULT true);
-- Fan 106 switched personalisation off; fan 105 has a row with the default.
INSERT INTO social_v2_preference (party_id, personalized) VALUES (106, false), (105, true);

CREATE TABLE music_release (id UUID PRIMARY KEY, artist_party_id BIGINT NOT NULL REFERENCES party(id));
CREATE TABLE music_release_version (id UUID PRIMARY KEY, release_id UUID NOT NULL REFERENCES music_release(id));
CREATE TABLE music_recording (id UUID PRIMARY KEY);
CREATE TABLE music_release_track (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id),
  recording_id UUID NOT NULL REFERENCES music_recording(id));
CREATE TABLE music_favorite (
  party_id BIGINT NOT NULL REFERENCES party(id), recording_id UUID NOT NULL REFERENCES music_recording(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(), PRIMARY KEY (party_id, recording_id));
CREATE TABLE music_playback_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  party_id BIGINT REFERENCES party(id), anonymous_id_hash TEXT,
  release_version_id UUID NOT NULL REFERENCES music_release_version(id),
  recording_id UUID NOT NULL REFERENCES music_recording(id),
  occurred_at TIMESTAMPTZ NOT NULL, eligible_play BOOLEAN NOT NULL DEFAULT FALSE,
  CHECK ((party_id IS NOT NULL)::INT + (anonymous_id_hash IS NOT NULL)::INT = 1));
-- Artist 3 released two recordings (the second one on two versions); artist 6 one.
INSERT INTO music_release VALUES
  ('00000000-0000-0000-0000-000000000003', 3), ('00000000-0000-0000-0000-000000000006', 6);
INSERT INTO music_release_version VALUES
  ('00000000-0000-0000-0001-000000000003', '00000000-0000-0000-0000-000000000003'),
  ('00000000-0000-0000-0002-000000000003', '00000000-0000-0000-0000-000000000003'),
  ('00000000-0000-0000-0001-000000000006', '00000000-0000-0000-0000-000000000006');
INSERT INTO music_recording VALUES
  ('00000000-0000-0000-0000-000000000301'), ('00000000-0000-0000-0000-000000000302'),
  ('00000000-0000-0000-0000-000000000601');
INSERT INTO music_release_track (release_version_id, recording_id) VALUES
  ('00000000-0000-0000-0001-000000000003', '00000000-0000-0000-0000-000000000301'),
  ('00000000-0000-0000-0001-000000000003', '00000000-0000-0000-0000-000000000302'),
  ('00000000-0000-0000-0002-000000000003', '00000000-0000-0000-0000-000000000302'),
  ('00000000-0000-0000-0001-000000000006', '00000000-0000-0000-0000-000000000601');
-- 103 and 104 favourite artist 3 (104 twice: once). 106 too, but opted out.
INSERT INTO music_favorite (party_id, recording_id) VALUES
  (103, '00000000-0000-0000-0000-000000000301'),
  (104, '00000000-0000-0000-0000-000000000301'), (104, '00000000-0000-0000-0000-000000000302'),
  (106, '00000000-0000-0000-0000-000000000302');
-- Artist 6: 101 has 3 eligible plays (2 points), 102 has 12 (3 points). Not
-- evidence: an ineligible play by 103, an eligible play by 104 from 200 days
-- ago, and an anonymous eligible play.
INSERT INTO music_playback_event (party_id, release_version_id, recording_id, occurred_at, eligible_play)
  SELECT 101, '00000000-0000-0000-0001-000000000006', '00000000-0000-0000-0000-000000000601',
         now() - make_interval(days => g), TRUE FROM generate_series(1, 3) g;
INSERT INTO music_playback_event (party_id, release_version_id, recording_id, occurred_at, eligible_play)
  SELECT 102, '00000000-0000-0000-0001-000000000006', '00000000-0000-0000-0000-000000000601',
         now() - make_interval(days => g), TRUE FROM generate_series(1, 12) g;
INSERT INTO music_playback_event (party_id, release_version_id, recording_id, occurred_at, eligible_play) VALUES
  (103, '00000000-0000-0000-0001-000000000006', '00000000-0000-0000-0000-000000000601', now(), FALSE),
  (104, '00000000-0000-0000-0001-000000000006', '00000000-0000-0000-0000-000000000601', now() - interval '200 days', TRUE);
INSERT INTO music_playback_event (anonymous_id_hash, release_version_id, recording_id, occurred_at, eligible_play) VALUES
  ('anonymous', '00000000-0000-0000-0001-000000000006', '00000000-0000-0000-0000-000000000601', now(), TRUE);

CREATE TABLE social_artist_profile (id BIGSERIAL PRIMARY KEY, party_id TEXT, name TEXT NOT NULL);
CREATE TABLE social_event (id BIGSERIAL PRIMARY KEY, title TEXT NOT NULL, is_public BOOLEAN NOT NULL);
-- Stand-in for the migrated view: the public events a person may answer.
CREATE VIEW directory_public_rsvp_event AS SELECT id FROM social_event WHERE is_public;
CREATE TABLE event_artist (
  event_id BIGINT NOT NULL REFERENCES social_event(id),
  artist_id BIGINT NOT NULL REFERENCES social_artist_profile(id), role TEXT,
  PRIMARY KEY (event_id, artist_id));
CREATE TABLE event_rsvp (
  id BIGSERIAL PRIMARY KEY, event_id BIGINT NOT NULL REFERENCES social_event(id),
  party_id TEXT NOT NULL, status TEXT NOT NULL, UNIQUE (event_id, party_id));
INSERT INTO social_artist_profile (id, party_id, name) VALUES
  (901, '5', 'Artist five'), (902, NULL, 'No party'), (903, 'five', 'Not a number');
INSERT INTO social_event (id, title, is_public) VALUES (1, 'Public', TRUE), (2, 'Private', FALSE);
INSERT INTO event_artist (event_id, artist_id) VALUES (1, 901), (1, 902), (1, 903), (2, 901);
-- 103 and 104 accepted the public event of artist 5. Not evidence: a maybe, an
-- acceptance of the private event, and a party id that is not a number.
INSERT INTO event_rsvp (event_id, party_id, status) VALUES
  (1, '103', 'accepted'), (1, '104', 'accepted'), (1, '105', 'maybe'),
  (2, '100', 'accepted'), (1, 'not-a-party', 'accepted');
COMMIT;
