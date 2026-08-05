CREATE TABLE IF NOT EXISTS public.artist_promo_slot (
  id BIGSERIAL PRIMARY KEY,
  artist_party_id BIGINT NOT NULL
    REFERENCES public.party(id) ON DELETE CASCADE,
  day DATE NOT NULL,
  start_time TIME WITHOUT TIME ZONE NOT NULL,
  medium TEXT NOT NULL,
  program TEXT NOT NULL,
  interviewer_host TEXT NOT NULL,
  band_members TEXT NOT NULL,
  status TEXT,
  notes TEXT,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX IF NOT EXISTS artist_promo_slot_artist_day_time_idx
  ON public.artist_promo_slot (artist_party_id, day, start_time, id);
