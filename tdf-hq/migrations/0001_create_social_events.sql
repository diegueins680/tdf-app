-- Migration: Create tables for Social Events feature
-- Run this against PostgreSQL during initial backend implementation.

-- Artists
CREATE TABLE IF NOT EXISTS artist_profiles (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  party_id UUID REFERENCES parties(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  bio TEXT,
  avatar_url TEXT,
  genres TEXT[],
  social_links JSONB,
  created_at TIMESTAMPTZ DEFAULT now(),
  updated_at TIMESTAMPTZ DEFAULT now()
);

-- Venues
CREATE TABLE IF NOT EXISTS venues (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name TEXT NOT NULL,
  address TEXT,
  city TEXT,
  country TEXT,
  latitude DOUBLE PRECISION,
  longitude DOUBLE PRECISION,
  capacity INTEGER,
  contact JSONB,
  created_at TIMESTAMPTZ DEFAULT now(),
  updated_at TIMESTAMPTZ DEFAULT now()
);

-- Events
CREATE TABLE IF NOT EXISTS social_events (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organizer_party_id UUID REFERENCES parties(id) ON DELETE SET NULL,
  title TEXT NOT NULL,
  description TEXT,
  venue_id UUID REFERENCES venues(id) ON DELETE SET NULL,
  start_time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ NOT NULL,
  price_cents BIGINT DEFAULT 0,
  capacity INTEGER,
  metadata JSONB,
  created_at TIMESTAMPTZ DEFAULT now(),
  updated_at TIMESTAMPTZ DEFAULT now()
);

-- Junction: event_artists
CREATE TABLE IF NOT EXISTS event_artists (
  event_id UUID REFERENCES social_events(id) ON DELETE CASCADE,
  artist_id UUID REFERENCES artist_profiles(id) ON DELETE CASCADE,
  role TEXT,
  PRIMARY KEY (event_id, artist_id)
);

-- RSVPs
CREATE TABLE IF NOT EXISTS event_rsvps (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id UUID REFERENCES social_events(id) ON DELETE CASCADE,
  party_id UUID REFERENCES parties(id) ON DELETE CASCADE,
  status TEXT NOT NULL,
  metadata JSONB,
  created_at TIMESTAMPTZ DEFAULT now(),
  updated_at TIMESTAMPTZ DEFAULT now()
);

-- Invitations
CREATE TABLE IF NOT EXISTS event_invitations (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id UUID REFERENCES social_events(id) ON DELETE CASCADE,
  from_party_id UUID REFERENCES parties(id) ON DELETE SET NULL,
  to_party_id UUID REFERENCES parties(id) ON DELETE SET NULL,
  status TEXT DEFAULT 'pending',
  message TEXT,
  created_at TIMESTAMPTZ DEFAULT now(),
  updated_at TIMESTAMPTZ DEFAULT now()
);

-- Artist Genres (optional normalized table)
CREATE TABLE IF NOT EXISTS artist_genres (
  artist_id UUID REFERENCES artist_profiles(id) ON DELETE CASCADE,
  genre TEXT,
  PRIMARY KEY (artist_id, genre)
);

-- Indexes for geo/filters
CREATE INDEX IF NOT EXISTS idx_venues_lat_lng ON venues(latitude, longitude);
CREATE INDEX IF NOT EXISTS idx_events_start_time ON social_events(start_time);
