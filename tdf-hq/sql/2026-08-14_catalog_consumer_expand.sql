-- Expand the production-shaped schema with canonical catalog references and
-- typed consumers before any legacy string/slug cutover runs.
\set ON_ERROR_STOP on

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-catalog-consumer-expand-v1', 0));

-- Canonical foreign-key slots are nullable during the expand phase. The
-- reviewed backfills populate them before legacy writers are disabled.
ALTER TABLE party ADD COLUMN IF NOT EXISTS country_id uuid;
ALTER TABLE artist_profile ADD COLUMN IF NOT EXISTS country_id uuid;
ALTER TABLE user_locale_preferences ADD COLUMN IF NOT EXISTS locale_id uuid;
ALTER TABLE user_locale_preferences ADD COLUMN IF NOT EXISTS currency_id uuid;
ALTER TABLE user_locale_preferences ADD COLUMN IF NOT EXISTS country_id uuid;
ALTER TABLE service_order ADD COLUMN IF NOT EXISTS service_offering_id uuid;
ALTER TABLE booking ADD COLUMN IF NOT EXISTS service_offering_id uuid;
ALTER TABLE booking ADD COLUMN IF NOT EXISTS booking_type_id uuid;
ALTER TABLE booking ADD COLUMN IF NOT EXISTS workflow_state_id uuid;
ALTER TABLE radio_stream ADD COLUMN IF NOT EXISTS country_id uuid;
ALTER TABLE radio_stream ADD COLUMN IF NOT EXISTS genre_id uuid;
ALTER TABLE cms_content ADD COLUMN IF NOT EXISTS authored_content_id uuid;

ALTER TABLE pipeline_card ADD COLUMN IF NOT EXISTS service_offering_id uuid;
ALTER TABLE pipeline_card ADD COLUMN IF NOT EXISTS workflow_state_id uuid;
ALTER TABLE live_session_intake ADD COLUMN IF NOT EXISTS primary_genre_id uuid;
ALTER TABLE live_session_musician ADD COLUMN IF NOT EXISTS instrument_id uuid;
ALTER TABLE feedback ADD COLUMN IF NOT EXISTS category_id uuid;
ALTER TABLE feedback ADD COLUMN IF NOT EXISTS severity_id uuid;
ALTER TABLE input_row ADD COLUMN IF NOT EXISTS instrument_id uuid;

ALTER TABLE social_artist_profile ADD COLUMN IF NOT EXISTS country_id uuid;
ALTER TABLE venue ADD COLUMN IF NOT EXISTS country_id uuid;
ALTER TABLE venue ADD COLUMN IF NOT EXISTS city_id uuid;
ALTER TABLE social_event ADD COLUMN IF NOT EXISTS event_type_id uuid;
ALTER TABLE social_event ADD COLUMN IF NOT EXISTS workflow_state_id uuid;
ALTER TABLE social_event ADD COLUMN IF NOT EXISTS currency_id uuid;
ALTER TABLE artist_genre ADD COLUMN IF NOT EXISTS genre_id uuid;
ALTER TABLE event_ticket_tier ADD COLUMN IF NOT EXISTS currency_id uuid;
ALTER TABLE event_moment_reaction ADD COLUMN IF NOT EXISTS id uuid;
ALTER TABLE event_moment_reaction ADD COLUMN IF NOT EXISTS reaction_type_id uuid;
ALTER TABLE event_moment_reaction ALTER COLUMN id SET DEFAULT gen_random_uuid();
UPDATE event_moment_reaction
SET id = md5(moment_id::text || ':' || COALESCE(reaction, reaction_type_id::text, '') || ':' || reactor_party_id)::uuid
WHERE id IS NULL;
ALTER TABLE event_moment_reaction ALTER COLUMN id SET NOT NULL;
CREATE UNIQUE INDEX IF NOT EXISTS uq_event_moment_reaction_id
  ON event_moment_reaction (id);

CREATE TABLE IF NOT EXISTS artist_profile_genre_membership (
  artist_party_id bigint NOT NULL REFERENCES party(id),
  genre_id uuid NOT NULL,
  sort_order integer NOT NULL DEFAULT 0,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (artist_party_id, genre_id)
);

CREATE TABLE IF NOT EXISTS fan_profile_genre_membership (
  fan_party_id bigint NOT NULL REFERENCES party(id),
  genre_id uuid NOT NULL,
  sort_order integer NOT NULL DEFAULT 0,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (fan_party_id, genre_id)
);

CREATE TABLE IF NOT EXISTS artist_genre_membership (
  artist_id bigint NOT NULL REFERENCES social_artist_profile(id),
  genre_id uuid NOT NULL,
  sort_order integer NOT NULL DEFAULT 0,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (artist_id, genre_id)
);

CREATE TABLE IF NOT EXISTS radio_stream_genre_observation (
  id bigserial PRIMARY KEY,
  stream_id bigint NOT NULL REFERENCES radio_stream(id),
  original_value text NOT NULL,
  normalized_value text NOT NULL,
  genre_id uuid,
  status text NOT NULL,
  source text NOT NULL,
  first_observed_at timestamptz NOT NULL DEFAULT now(),
  last_observed_at timestamptz NOT NULL DEFAULT now(),
  observation_count bigint NOT NULL DEFAULT 1,
  UNIQUE (stream_id, normalized_value, source)
);

CREATE TABLE IF NOT EXISTS radio_stream_genre_observation_candidate (
  observation_id bigint NOT NULL REFERENCES radio_stream_genre_observation(id),
  genre_id uuid NOT NULL,
  active boolean NOT NULL DEFAULT TRUE,
  first_matched_at timestamptz NOT NULL DEFAULT now(),
  last_matched_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (observation_id, genre_id)
);

CREATE TABLE IF NOT EXISTS radio_stream_country_observation (
  id bigserial PRIMARY KEY,
  stream_id bigint NOT NULL REFERENCES radio_stream(id),
  original_value text NOT NULL,
  normalized_value text NOT NULL,
  country_id uuid,
  status text NOT NULL,
  source text NOT NULL,
  first_observed_at timestamptz NOT NULL DEFAULT now(),
  last_observed_at timestamptz NOT NULL DEFAULT now(),
  observation_count bigint NOT NULL DEFAULT 1,
  UNIQUE (stream_id, normalized_value, source)
);

CREATE TABLE IF NOT EXISTS radio_stream_country_observation_candidate (
  observation_id bigint NOT NULL REFERENCES radio_stream_country_observation(id),
  country_id uuid NOT NULL,
  active boolean NOT NULL DEFAULT TRUE,
  first_matched_at timestamptz NOT NULL DEFAULT now(),
  last_matched_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (observation_id, country_id)
);

CREATE TABLE IF NOT EXISTS engagement_event (
  id bigserial PRIMARY KEY,
  actor_party_id bigint REFERENCES party(id),
  target_artist_id bigint REFERENCES party(id),
  entity_type text NOT NULL,
  entity_id bigint,
  event_type text NOT NULL,
  metadata text,
  created_at timestamptz NOT NULL
);
CREATE INDEX IF NOT EXISTS ix_engagement_artist_created
  ON engagement_event (target_artist_id, created_at);
CREATE INDEX IF NOT EXISTS ix_engagement_actor_created
  ON engagement_event (actor_party_id, created_at);

ALTER TABLE fan_club_post ADD COLUMN IF NOT EXISTS media_urls text;

CREATE TABLE IF NOT EXISTS fan_club_inbox_message (
  id bigserial PRIMARY KEY,
  club_id bigint NOT NULL REFERENCES fan_club(id),
  fan_party_id bigint NOT NULL REFERENCES party(id),
  subject text,
  body text NOT NULL,
  status text NOT NULL DEFAULT 'unread',
  officer_party_id bigint REFERENCES party(id),
  reply_body text,
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz
);

CREATE TABLE IF NOT EXISTS fan_club_post_reaction (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  post_id bigint NOT NULL REFERENCES fan_club_post(id),
  reactor_party_id bigint NOT NULL REFERENCES party(id),
  reaction_type_id uuid NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (post_id, reactor_party_id)
);

CREATE TABLE IF NOT EXISTS fan_club_memory_reaction (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  memory_id bigint NOT NULL REFERENCES fan_club_memory(id),
  reactor_party_id bigint NOT NULL REFERENCES party(id),
  reaction_type_id uuid NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (memory_id, reactor_party_id)
);

CREATE TABLE IF NOT EXISTS creator_badge (
  id bigserial PRIMARY KEY,
  party_id bigint NOT NULL REFERENCES party(id),
  club_id bigint NOT NULL REFERENCES fan_club(id),
  badge_type_id uuid NOT NULL,
  awarded_at timestamptz NOT NULL DEFAULT now(),
  expires_at timestamptz,
  UNIQUE (party_id, club_id, badge_type_id)
);

CREATE TABLE IF NOT EXISTS boosted_content (
  id bigserial PRIMARY KEY,
  target_type text NOT NULL,
  target_id bigint NOT NULL,
  club_id bigint NOT NULL REFERENCES fan_club(id),
  total_reactions bigint NOT NULL,
  boosted_at timestamptz NOT NULL DEFAULT now(),
  surfaced_to_artist boolean NOT NULL DEFAULT FALSE,
  UNIQUE (target_type, target_id)
);

CREATE TABLE IF NOT EXISTS event_live_broadcast (
  id bigserial PRIMARY KEY,
  event_id bigint NOT NULL REFERENCES social_event(id),
  artist_id bigint NOT NULL REFERENCES social_artist_profile(id),
  broadcaster_party_id text NOT NULL,
  broadcaster_name text NOT NULL,
  title text NOT NULL,
  description text,
  status text NOT NULL,
  playback_url text,
  ingest_url text,
  whip_url text,
  stream_key text,
  viewer_count bigint NOT NULL,
  started_at timestamptz NOT NULL,
  ended_at timestamptz,
  last_heartbeat_at timestamptz NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS label_project_note (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  text text NOT NULL,
  completed boolean NOT NULL DEFAULT FALSE,
  active boolean NOT NULL DEFAULT TRUE,
  created_by bigint,
  updated_by bigint,
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now(),
  version bigint NOT NULL DEFAULT 1,
  source_cms_content_id bigint,
  source_item_id text,
  UNIQUE (source_cms_content_id, source_item_id)
);

-- DDEX expands first; reviewed cutovers move legacy values and only then make
-- canonical references mandatory or remove copied arrays/strings.
CREATE TABLE IF NOT EXISTS ddex_job_operation (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(), code text NOT NULL UNIQUE,
  name_es text NOT NULL, name_en text NOT NULL, description_es text,
  description_en text, active boolean NOT NULL DEFAULT TRUE,
  sort_order integer NOT NULL DEFAULT 0, version integer NOT NULL DEFAULT 1
);
CREATE TABLE IF NOT EXISTS ddex_import_operation (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(), code text NOT NULL UNIQUE,
  name_es text NOT NULL, name_en text NOT NULL, description_es text,
  description_en text, active boolean NOT NULL DEFAULT TRUE,
  sort_order integer NOT NULL DEFAULT 0, version integer NOT NULL DEFAULT 1
);
CREATE TABLE IF NOT EXISTS ddex_validation_result (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(), code text NOT NULL UNIQUE,
  name_es text NOT NULL, name_en text NOT NULL, description_es text,
  description_en text, active boolean NOT NULL DEFAULT TRUE,
  sort_order integer NOT NULL DEFAULT 0, version integer NOT NULL DEFAULT 1
);
CREATE TABLE IF NOT EXISTS ddex_validation_severity (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(), code text NOT NULL UNIQUE,
  name_es text NOT NULL, name_en text NOT NULL, description_es text,
  description_en text, active boolean NOT NULL DEFAULT TRUE,
  sort_order integer NOT NULL DEFAULT 0, version integer NOT NULL DEFAULT 1
);
CREATE TABLE IF NOT EXISTS ddex_validation_layer (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(), code text NOT NULL UNIQUE,
  name_es text NOT NULL, name_en text NOT NULL, description_es text,
  description_en text, active boolean NOT NULL DEFAULT TRUE,
  sort_order integer NOT NULL DEFAULT 0, version integer NOT NULL DEFAULT 1
);

ALTER TABLE ddex_document ADD COLUMN IF NOT EXISTS standard_version_id uuid;
ALTER TABLE ddex_document ADD COLUMN IF NOT EXISTS message_type_id uuid;
ALTER TABLE ddex_document ADD COLUMN IF NOT EXISTS workflow_state_id uuid;
ALTER TABLE ddex_validation_run ADD COLUMN IF NOT EXISTS workflow_state_id uuid;
ALTER TABLE ddex_validation_run ADD COLUMN IF NOT EXISTS result_id uuid;
ALTER TABLE ddex_validation_issue ADD COLUMN IF NOT EXISTS severity_id uuid;
ALTER TABLE ddex_validation_issue ADD COLUMN IF NOT EXISTS layer_id uuid;
ALTER TABLE ddex_import_plan ADD COLUMN IF NOT EXISTS workflow_state_id uuid;
ALTER TABLE ddex_import_run ADD COLUMN IF NOT EXISTS workflow_state_id uuid;
ALTER TABLE ddex_import_change ADD COLUMN IF NOT EXISTS operation_id uuid;
ALTER TABLE ddex_export ADD COLUMN IF NOT EXISTS standard_version_id uuid;
ALTER TABLE ddex_export ADD COLUMN IF NOT EXISTS workflow_state_id uuid;
ALTER TABLE ddex_export ADD COLUMN IF NOT EXISTS validation_result_id uuid;
ALTER TABLE ddex_job ADD COLUMN IF NOT EXISTS operation_id uuid;
ALTER TABLE ddex_job ADD COLUMN IF NOT EXISTS workflow_state_id uuid;

CREATE TABLE IF NOT EXISTS ddex_partner_standard_version (
  id bigserial PRIMARY KEY,
  partner_id bigint NOT NULL REFERENCES ddex_partner(id),
  standard_version_id uuid NOT NULL,
  sort_order integer NOT NULL DEFAULT 0,
  active boolean NOT NULL DEFAULT TRUE,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (partner_id, standard_version_id)
);

COMMIT;
