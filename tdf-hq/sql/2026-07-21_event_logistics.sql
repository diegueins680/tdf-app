BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

CREATE TABLE IF NOT EXISTS public.event_logistics_plan (
  id BIGSERIAL PRIMARY KEY,
  event_id BIGINT NOT NULL REFERENCES public.social_event(id) ON DELETE CASCADE,
  timezone TEXT NOT NULL DEFAULT 'America/Guayaquil',
  default_travel_mode TEXT NOT NULL DEFAULT 'drive',
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT unique_event_logistics_plan UNIQUE (event_id),
  CONSTRAINT event_logistics_plan_mode_check CHECK (default_travel_mode IN ('drive', 'walk', 'bicycle', 'two_wheeler', 'transit'))
);

CREATE TABLE IF NOT EXISTS public.event_logistics_member (
  id BIGSERIAL PRIMARY KEY,
  event_id BIGINT NOT NULL REFERENCES public.social_event(id) ON DELETE CASCADE,
  party_id TEXT NOT NULL,
  member_role TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT unique_event_logistics_member UNIQUE (event_id, party_id),
  CONSTRAINT event_logistics_member_role_check CHECK (member_role IN ('viewer', 'editor'))
);

CREATE TABLE IF NOT EXISTS public.event_logistics_place (
  id BIGSERIAL PRIMARY KEY,
  event_id BIGINT NOT NULL REFERENCES public.social_event(id) ON DELETE CASCADE,
  venue_id BIGINT NULL REFERENCES public.venue(id) ON DELETE SET NULL,
  label TEXT NOT NULL,
  place_type TEXT NOT NULL,
  address TEXT NULL,
  google_place_id TEXT NULL,
  latitude DOUBLE PRECISION NOT NULL,
  longitude DOUBLE PRECISION NOT NULL,
  instructions TEXT NULL,
  contact_name TEXT NULL,
  contact_phone TEXT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT event_logistics_place_type_check CHECK (place_type IN ('venue', 'hotel', 'airport', 'pickup', 'custom')),
  CONSTRAINT event_logistics_place_latitude_check CHECK (latitude BETWEEN -90 AND 90),
  CONSTRAINT event_logistics_place_longitude_check CHECK (longitude BETWEEN -180 AND 180)
);

CREATE TABLE IF NOT EXISTS public.event_logistics_activity (
  id BIGSERIAL PRIMARY KEY,
  event_id BIGINT NOT NULL REFERENCES public.social_event(id) ON DELETE CASCADE,
  activity_type TEXT NOT NULL,
  title TEXT NOT NULL,
  notes TEXT NULL,
  start_time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ NULL,
  place_id BIGINT NULL REFERENCES public.event_logistics_place(id) ON DELETE NO ACTION,
  origin_place_id BIGINT NULL REFERENCES public.event_logistics_place(id) ON DELETE NO ACTION,
  destination_place_id BIGINT NULL REFERENCES public.event_logistics_place(id) ON DELETE NO ACTION,
  travel_mode TEXT NULL,
  buffer_minutes INTEGER NULL,
  priority TEXT NOT NULL,
  status TEXT NOT NULL,
  version INTEGER NOT NULL DEFAULT 1,
  created_by_party_id TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT event_logistics_activity_type_check CHECK (activity_type IN ('task', 'milestone', 'wait', 'travel')),
  CONSTRAINT event_logistics_activity_mode_check CHECK (travel_mode IS NULL OR travel_mode IN ('drive', 'walk', 'bicycle', 'two_wheeler', 'transit')),
  CONSTRAINT event_logistics_activity_priority_check CHECK (priority IN ('low', 'normal', 'high', 'critical')),
  CONSTRAINT event_logistics_activity_status_check CHECK (status IN ('planned', 'confirmed', 'in_progress', 'completed', 'cancelled')),
  CONSTRAINT event_logistics_activity_buffer_check CHECK (buffer_minutes IS NULL OR buffer_minutes BETWEEN 0 AND 1440),
  CONSTRAINT event_logistics_activity_version_check CHECK (version > 0)
);

CREATE TABLE IF NOT EXISTS public.event_logistics_assignment (
  id BIGSERIAL PRIMARY KEY,
  activity_id BIGINT NOT NULL REFERENCES public.event_logistics_activity(id) ON DELETE CASCADE,
  party_id TEXT NULL,
  external_name TEXT NULL,
  external_phone TEXT NULL,
  external_email TEXT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT event_logistics_assignment_identity_check CHECK ((party_id IS NOT NULL) <> (external_name IS NOT NULL))
);

CREATE TABLE IF NOT EXISTS public.event_logistics_dependency (
  id BIGSERIAL PRIMARY KEY,
  activity_id BIGINT NOT NULL REFERENCES public.event_logistics_activity(id) ON DELETE CASCADE,
  depends_on_activity_id BIGINT NOT NULL REFERENCES public.event_logistics_activity(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT unique_event_logistics_dependency UNIQUE (activity_id, depends_on_activity_id),
  CONSTRAINT event_logistics_dependency_not_self CHECK (activity_id <> depends_on_activity_id)
);

CREATE TABLE IF NOT EXISTS public.event_route_verification (
  id BIGSERIAL PRIMARY KEY,
  activity_id BIGINT NOT NULL REFERENCES public.event_logistics_activity(id) ON DELETE CASCADE,
  activity_version INTEGER NOT NULL,
  provider TEXT NOT NULL,
  travel_mode TEXT NOT NULL,
  departure_time TIMESTAMPTZ NOT NULL,
  duration_seconds INTEGER NULL,
  static_duration_seconds INTEGER NULL,
  distance_meters INTEGER NULL,
  buffer_seconds INTEGER NOT NULL,
  allocated_seconds INTEGER NOT NULL,
  verdict TEXT NOT NULL,
  encoded_polyline TEXT NULL,
  error_message TEXT NULL,
  checkpoint TEXT NULL,
  verified_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT event_route_verification_verdict_check CHECK (verdict IN ('feasible', 'tight', 'infeasible', 'provisional', 'unavailable', 'stale')),
  CONSTRAINT event_route_verification_checkpoint_check CHECK (checkpoint IS NULL OR checkpoint IN ('24h', '2h'))
);

CREATE TABLE IF NOT EXISTS public.event_logistics_alert_delivery (
  id BIGSERIAL PRIMARY KEY,
  activity_id BIGINT NOT NULL REFERENCES public.event_logistics_activity(id) ON DELETE CASCADE,
  activity_version INTEGER NOT NULL,
  checkpoint TEXT NOT NULL,
  recipient_party_id TEXT NOT NULL,
  channel TEXT NOT NULL,
  delivered_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT unique_event_logistics_alert UNIQUE (activity_id, activity_version, checkpoint, recipient_party_id, channel)
);

CREATE INDEX IF NOT EXISTS idx_event_logistics_activity_event_start ON public.event_logistics_activity (event_id, start_time);
CREATE INDEX IF NOT EXISTS idx_event_logistics_activity_recheck ON public.event_logistics_activity (activity_type, status, start_time);
CREATE INDEX IF NOT EXISTS idx_event_logistics_member_event ON public.event_logistics_member (event_id);
CREATE INDEX IF NOT EXISTS idx_event_logistics_place_event ON public.event_logistics_place (event_id);
CREATE INDEX IF NOT EXISTS idx_event_logistics_assignment_activity ON public.event_logistics_assignment (activity_id);
CREATE INDEX IF NOT EXISTS idx_event_logistics_dependency_activity ON public.event_logistics_dependency (activity_id);
CREATE INDEX IF NOT EXISTS idx_event_logistics_dependency_parent ON public.event_logistics_dependency (depends_on_activity_id);
CREATE INDEX IF NOT EXISTS idx_event_route_verification_activity ON public.event_route_verification (activity_id, verified_at DESC);
CREATE UNIQUE INDEX IF NOT EXISTS idx_event_route_verification_checkpoint_once
  ON public.event_route_verification (activity_id, activity_version, checkpoint)
  WHERE checkpoint IS NOT NULL;

COMMIT;
