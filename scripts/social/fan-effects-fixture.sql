-- Extend only the private HTTP fixture with columns needed by the real handler.
ALTER TABLE fan_follow ADD COLUMN id bigserial UNIQUE;
ALTER TABLE fan_follow ADD COLUMN created_at timestamptz NOT NULL DEFAULT now();
CREATE TABLE artist_profile(id bigserial PRIMARY KEY,artist_party_id bigint UNIQUE REFERENCES party(id),
  hero_image_url text,spotify_url text,youtube_url text);
CREATE TABLE fan_club_member_profile(id bigserial PRIMARY KEY,party_id bigint REFERENCES party(id),
  club_id bigint REFERENCES fan_club(id),handle text,bio text,avatar_url text,joined_at timestamptz NOT NULL,
  UNIQUE(party_id,club_id));
CREATE TABLE engagement_event(id bigserial PRIMARY KEY,actor_party_id bigint,target_artist_id bigint,
  entity_type text,entity_id integer,event_type text,metadata text,created_at timestamptz);
CREATE TABLE notification(id bigserial PRIMARY KEY,recipient_party_id bigint,notif_type text,title text,
  body text,target_type text,target_id integer,target_key text,is_read boolean,created_at timestamptz);
ALTER TABLE security_role
  ADD COLUMN code text, ADD COLUMN name_es text, ADD COLUMN name_en text,
  ADD COLUMN description_es text, ADD COLUMN description_en text, ADD COLUMN sort_order integer DEFAULT 0,
  ADD COLUMN system_role boolean DEFAULT false, ADD COLUMN emergency_administrator boolean DEFAULT false,
  ADD COLUMN self_assignable boolean DEFAULT false, ADD COLUMN automatic_assignable boolean DEFAULT false,
  ADD COLUMN workflow_state_id uuid, ADD COLUMN created_by bigint, ADD COLUMN updated_by bigint,
  ADD COLUMN approved_by bigint, ADD COLUMN created_at timestamptz DEFAULT now(),
  ADD COLUMN updated_at timestamptz DEFAULT now(), ADD COLUMN published_revision integer DEFAULT 1,
  ADD COLUMN version integer DEFAULT 1;
