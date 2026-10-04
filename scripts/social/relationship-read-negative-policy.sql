BEGIN;
CREATE OR REPLACE FUNCTION social_v2_profile_eligible(actor bigint,requested bigint[])
RETURNS TABLE(profile_id bigint,ordinal bigint) LANGUAGE sql STABLE AS $$
 SELECT r.id,r.ordinal FROM unnest(requested) WITH ORDINALITY AS r(id,ordinal)
$$;
