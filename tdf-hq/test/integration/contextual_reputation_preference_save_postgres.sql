\set ON_ERROR_STOP on

DO $$
DECLARE
  owner_id BIGINT;
  categories JSONB;
  invalid_categories JSONB;
  saved JSONB;
  saved_total NUMERIC(10,4);
  invalid_profile_count INTEGER;
BEGIN
  SELECT min(id) INTO owner_id FROM party;
  IF owner_id IS NULL THEN
    RAISE EXCEPTION 'Contextual reputation preference test requires a party fixture';
  END IF;

  WITH selected_categories AS (
    SELECT id, row_number() OVER (ORDER BY default_position, slug) AS position
    FROM reputation_category
    WHERE status = 'active'
    ORDER BY default_position, slug
    LIMIT 8
  ), centroid_weights(position, weight) AS (
    VALUES
      (1, 33.973214285714285::numeric),
      (2, 21.473214285714285::numeric),
      (3, 15.223214285714285::numeric),
      (4, 11.056547619047619::numeric),
      (5, 7.931547619047619::numeric),
      (6, 5.4315476190476195::numeric),
      (7, 3.3482142857142856::numeric),
      (8, 1.5624999999999858::numeric)
  )
  SELECT jsonb_agg(jsonb_build_object(
    'categoryId', selected_categories.id,
    'position', selected_categories.position,
    'weight', centroid_weights.weight,
    'notApplicable', false
  ) ORDER BY selected_categories.position)
  INTO categories
  FROM selected_categories
  JOIN centroid_weights ON centroid_weights.position = selected_categories.position;

  IF jsonb_array_length(categories) <> 8 THEN
    RAISE EXCEPTION 'Contextual reputation preference test requires eight active categories';
  END IF;

  saved := reputation_save_personal_preference(
    owner_id,
    'rounding-regression',
    0,
    true,
    categories,
    'rounding-regression-valid',
    'rounding-regression-valid-v1'
  );

  IF saved->>'status' IS DISTINCT FROM 'active' THEN
    RAISE EXCEPTION 'Canonical rank-order centroid preference was not activated: %', saved;
  END IF;

  SELECT sum(item.weight)
  INTO saved_total
  FROM reputation_personal_preference preference
  JOIN reputation_personal_preference_category item ON item.preference_id = preference.id
  WHERE preference.owner_party_id = owner_id
    AND preference.context_kind = 'rounding-regression'
    AND NOT item.not_applicable;

  IF saved_total <> 100.0000 THEN
    RAISE EXCEPTION 'Canonical rank-order centroid preference total was %, expected 100.0000', saved_total;
  END IF;

  SELECT jsonb_agg(item.value || jsonb_build_object('weight', 30))
  INTO invalid_categories
  FROM jsonb_array_elements(categories) WITH ORDINALITY AS item(value, ordinal)
  WHERE item.ordinal <= 3;

  BEGIN
    PERFORM reputation_save_personal_preference(
      owner_id,
      'invalid-total-regression',
      0,
      true,
      invalid_categories,
      'rounding-regression-invalid',
      'rounding-regression-invalid-v1'
    );
    RAISE EXCEPTION 'Invalid 90-point preference total was accepted';
  EXCEPTION WHEN check_violation THEN
    NULL;
  END;

  SELECT count(*)
  INTO invalid_profile_count
  FROM reputation_personal_preference
  WHERE owner_party_id = owner_id
    AND context_kind = 'invalid-total-regression';

  IF invalid_profile_count <> 0 THEN
    RAISE EXCEPTION 'Rejected invalid preference left % profile rows behind', invalid_profile_count;
  END IF;
END $$;
