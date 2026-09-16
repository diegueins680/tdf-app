BEGIN;
CREATE OR REPLACE FUNCTION social_v2_legacy_suggestions_enabled()
RETURNS boolean LANGUAGE sql STABLE AS $$ SELECT true $$;
