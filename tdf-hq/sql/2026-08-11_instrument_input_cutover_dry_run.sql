\set ON_ERROR_STOP on

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5min';
SET LOCAL lock_timeout = '1s';

SELECT jsonb_build_object(
  'report', 'instrument-input-schema-readiness',
  'inputRowInstrumentIdPresent', EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema='public' AND table_name='input_row' AND column_name='instrument_id'
  ),
  'liveSessionPrimaryGenreIdPresent', EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema='public' AND table_name='live_session_intake' AND column_name='primary_genre_id'
  ),
  'liveSessionInstrumentIdPresent', EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema='public' AND table_name='live_session_musician' AND column_name='instrument_id'
  )
);

WITH reviewed(track_name, microphone_observation, instrument_code, asset_name, evidence) AS (
  VALUES
    ('Kick In','AKG D112','drums','AKG D112','reviewed kick input plus exact asset name'),
    ('Snare Up','Shure SM57','drums','Shure SM57','reviewed snare input plus exact asset name'),
    ('Snare Down','Shure SM57','drums','Shure SM57','reviewed snare input plus exact asset name'),
    ('Hi-Hat','Sennheiser MKE600','drums','Sennheiser MKE600','reviewed hi-hat input plus exact asset name'),
    ('Tom 1','Sennheiser MD421','drums','Sennheiser MD421','reviewed tom input plus exact asset name'),
    ('Tom Floor','Sennheiser MD421','drums','Sennheiser MD421','reviewed floor-tom input plus exact asset name'),
    ('OH L','AKG C414 (HC)','drums','AKG C414','reviewed overhead channel; HC is placement metadata'),
    ('OH R','AKG C414 (HC)','drums','AKG C414','reviewed overhead channel; HC is placement metadata'),
    ('Bass DI (post)','Neve RNDI','bass-guitar','Neve RNDI','reviewed bass DI plus exact active DI asset'),
    ('Bass Mic 1 (cab)','AKG D112','bass-guitar','AKG D112','reviewed bass cabinet input plus exact asset'),
    ('Bass Mic 2 (ataque)','Neumann KM184','bass-guitar','Neumann KM184','reviewed bass attack input plus exact asset'),
    ('Gtr 1','Sennheiser e906','electric-guitar','Sennheiser e906','reviewed electric-guitar input plus exact asset'),
    ('Gtr 1 Ribbon','Royer R121','electric-guitar','Royer R121','reviewed electric-guitar ribbon input plus exact asset'),
    ('Gtr 2','Sennheiser e906','electric-guitar','Sennheiser e906','reviewed electric-guitar input plus exact asset'),
    ('Gtr 2 Ribbon','Royer R121','electric-guitar','Royer R121','reviewed electric-guitar ribbon input plus exact asset'),
    ('Vox 1','Electro-Voice RE20','voice','Electro-Voice RE20','reviewed voice input plus exact asset'),
    ('Vox 2','Sennheiser e835','voice','Sennheiser e835','reviewed voice input plus exact asset'),
    ('Vox 3','Shure SM58','voice','Shure SM58','reviewed voice input plus exact asset'),
    ('Vox 4','Shure SM58','voice','Shure SM58','reviewed voice input plus exact asset'),
    ('KU-100 L','Neumann KU-100 L','voice','Neumann KU-100','reviewed left channel of one binaural microphone asset'),
    ('KU-100 R','Neumann KU-100 R','voice','Neumann KU-100','reviewed right channel of one binaural microphone asset')
), resolved AS (
  SELECT row.id, row.channel_number, row.track_name, row.instrument AS original_value,
    reviewed.instrument_code, reviewed.asset_name, reviewed.evidence,
    (SELECT count(*) FROM instrument item JOIN workflow_state state ON state.id=item.workflow_state_id
      WHERE item.code=reviewed.instrument_code AND item.active AND item.deprecated_at IS NULL
        AND state.code='published' AND state.active) AS instrument_candidates,
    (SELECT count(*) FROM asset item WHERE item.name=reviewed.asset_name AND item.status='Active') AS asset_candidates
  FROM input_row row
  LEFT JOIN reviewed ON reviewed.track_name=btrim(row.track_name)
    AND reviewed.microphone_observation=btrim(row.instrument)
  WHERE row.instrument IS NOT NULL
)
SELECT jsonb_build_object(
  'report', 'input-row-instrument-and-asset-map',
  'sourceRows', count(*),
  'mapped', count(*) FILTER (WHERE instrument_candidates=1 AND asset_candidates=1),
  'unresolved', count(*) FILTER (WHERE instrument_candidates=0 OR asset_candidates=0),
  'ambiguous', count(*) FILTER (WHERE instrument_candidates>1 OR asset_candidates>1),
  'rows', jsonb_agg(jsonb_build_object(
    'id', id, 'channel', channel_number, 'trackName', track_name,
    'originalValue', original_value, 'instrumentCode', instrument_code,
    'assetName', asset_name, 'instrumentCandidates', instrument_candidates,
    'assetCandidates', asset_candidates, 'evidence', evidence
  ) ORDER BY channel_number)
) FROM resolved;

SELECT jsonb_build_object(
  'report', 'live-session-catalog-reference-map',
  'legacyGenreRows', (SELECT count(*) FROM live_session_intake WHERE primary_genre IS NOT NULL),
  'legacyInstrumentRows', (SELECT count(*) FROM live_session_musician WHERE instrument IS NOT NULL OR role IS NOT NULL)
);

ROLLBACK;
