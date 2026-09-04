-- Atomic, idempotent save for a viewer's private preference profile.
-- It is intentionally unrelated to reputation_public_aggregate.
\set ON_ERROR_STOP on
BEGIN;

CREATE TABLE IF NOT EXISTS reputation_preference_idempotency (
  owner_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  idempotency_key TEXT NOT NULL CHECK (length(idempotency_key) BETWEEN 8 AND 160),
  request_fingerprint TEXT NOT NULL,
  response JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  expires_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(owner_party_id, idempotency_key)
);

CREATE OR REPLACE FUNCTION reputation_personal_preference_response(preference_uuid UUID)
RETURNS JSONB LANGUAGE sql STABLE AS $$
  SELECT jsonb_build_object(
    'contextKind', preference.context_kind,
    'status', preference.status,
    'revision', preference.revision,
    'formulaVersion', preference.preference_formula_version_id,
    'categories', coalesce(
      jsonb_agg(jsonb_build_object(
        'categoryId', category.id,
        'slug', category.slug,
        'position', item.position,
        'weight', item.weight,
        'notApplicable', item.not_applicable
      ) ORDER BY item.position) FILTER (WHERE item.category_id IS NOT NULL),
      '[]'::jsonb
    )
  )
  FROM reputation_personal_preference preference
  LEFT JOIN reputation_personal_preference_category item ON item.preference_id=preference.id
  LEFT JOIN reputation_category category ON category.id=item.category_id
  WHERE preference.id=preference_uuid
  GROUP BY preference.id
$$;

CREATE OR REPLACE FUNCTION reputation_save_personal_preference(
  requested_owner_party_id BIGINT,
  requested_context_kind TEXT,
  expected_revision INTEGER,
  requested_active BOOLEAN,
  requested_categories JSONB,
  requested_idempotency_key TEXT,
  requested_fingerprint TEXT
) RETURNS JSONB
LANGUAGE plpgsql AS $$
DECLARE
  preference_uuid UUID;
  current_revision INTEGER;
  created_profile BOOLEAN := FALSE;
  stored_fingerprint TEXT;
  stored_response JSONB;
  saved_response JSONB;
BEGIN
  IF requested_owner_party_id IS NULL OR expected_revision < 0
    OR requested_context_kind IS NULL OR length(btrim(requested_context_kind)) NOT BETWEEN 1 AND 80
    OR requested_idempotency_key IS NULL OR length(btrim(requested_idempotency_key)) NOT BETWEEN 8 AND 160
    OR requested_fingerprint IS NULL OR length(btrim(requested_fingerprint)) = 0
    OR jsonb_typeof(requested_categories) <> 'array'
    OR jsonb_array_length(requested_categories) > 10 THEN
    RAISE EXCEPTION 'Invalid private preference save request' USING ERRCODE='23514';
  END IF;

  PERFORM pg_advisory_xact_lock(hashtextextended(
    'reputation-personal-preference:' || requested_owner_party_id::text || ':' || btrim(requested_context_kind), 0
  ));

  SELECT request_fingerprint, response
  INTO stored_fingerprint, stored_response
  FROM reputation_preference_idempotency
  WHERE owner_party_id=requested_owner_party_id
    AND idempotency_key=btrim(requested_idempotency_key)
    AND expires_at > now()
  FOR UPDATE;

  IF FOUND THEN
    IF stored_fingerprint <> requested_fingerprint THEN
      RAISE EXCEPTION 'Idempotency key was already used with a different request' USING ERRCODE='23505';
    END IF;
    RETURN stored_response;
  END IF;

  DELETE FROM reputation_preference_idempotency
  WHERE owner_party_id=requested_owner_party_id
    AND idempotency_key=btrim(requested_idempotency_key)
    AND expires_at <= now();

  SELECT id, revision INTO preference_uuid, current_revision
  FROM reputation_personal_preference
  WHERE owner_party_id=requested_owner_party_id
    AND context_kind=btrim(requested_context_kind)
  FOR UPDATE;

  IF NOT FOUND THEN
    IF expected_revision <> 0 THEN
      RAISE EXCEPTION 'Preference revision conflict' USING ERRCODE='40001';
    END IF;
    INSERT INTO reputation_personal_preference(
      owner_party_id, context_kind, status, revision, preference_formula_version_id
    ) VALUES (
      requested_owner_party_id, btrim(requested_context_kind), 'draft', 1, 'public-bayes-roc-v1'
    ) RETURNING id, revision INTO preference_uuid, current_revision;
    created_profile := TRUE;
  ELSIF current_revision <> expected_revision THEN
    RAISE EXCEPTION 'Preference revision conflict' USING ERRCODE='40001';
  ELSE
    -- Items are mutable only in drafts. Moving an active profile back to draft
    -- is internal to this atomic save and is not visible as an intermediate state.
    UPDATE reputation_personal_preference
    SET status='draft', updated_at=now()
    WHERE id=preference_uuid AND status <> 'draft';
  END IF;

  DELETE FROM reputation_personal_preference_category WHERE preference_id=preference_uuid;

  INSERT INTO reputation_personal_preference_category(
    preference_id, category_id, position, weight, not_applicable
  )
  SELECT
    preference_uuid,
    (item.value->>'categoryId')::uuid,
    (item.value->>'position')::smallint,
    round((item.value->>'weight')::numeric, 4),
    coalesce((item.value->>'notApplicable')::boolean, false)
  FROM jsonb_array_elements(requested_categories) AS item(value);

  UPDATE reputation_personal_preference
  SET revision=CASE WHEN created_profile THEN current_revision ELSE current_revision + 1 END,
      status=CASE WHEN requested_active THEN 'active' ELSE 'draft' END,
      updated_at=now()
  WHERE id=preference_uuid;

  SELECT reputation_personal_preference_response(preference_uuid) INTO saved_response;
  INSERT INTO reputation_preference_idempotency(
    owner_party_id, idempotency_key, request_fingerprint, response, expires_at
  ) VALUES (
    requested_owner_party_id, btrim(requested_idempotency_key), requested_fingerprint,
    saved_response, now()+interval '24 hours'
  );

  RETURN saved_response;
END $$;

COMMENT ON FUNCTION reputation_save_personal_preference(BIGINT,TEXT,INTEGER,BOOLEAN,JSONB,TEXT,TEXT) IS
  'Owner-only, optimistic-concurrency and idempotent private preference save. It never writes public reputation aggregates.';

COMMIT;
