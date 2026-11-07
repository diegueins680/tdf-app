-- PAY-MUSIC-MONEY-WIRE-001. Preserve legacy JSON numbers only inside the
-- interoperable integer domain; every other BIGINT travels as decimal text.
-- No financial rows or historical snapshots are changed.
CREATE OR REPLACE FUNCTION public.music_minor_units_json(amount BIGINT)
RETURNS JSONB
LANGUAGE SQL IMMUTABLE STRICT PARALLEL SAFE
AS $$
  SELECT CASE
    WHEN amount BETWEEN -9007199254740991 AND 9007199254740991
      THEN to_jsonb(amount)
    ELSE to_jsonb(amount::TEXT)
  END
$$;
