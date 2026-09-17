-- Deliberately unsafe membership-only projection, within a throwaway transaction.
-- This is a regression control for generated cases, not a historical server run.
BEGIN;
CREATE OR REPLACE FUNCTION social_v2_chat_threads(actor bigint) RETURNS jsonb
LANGUAGE sql STABLE AS $$
  SELECT jsonb_build_object('result',coalesce(jsonb_agg(jsonb_build_object('ctThreadId',id)),'[]'::jsonb))
  FROM chat_thread WHERE actor IN (dm_party_a,dm_party_b)
$$;
\ir dm-read-model-cases.sql
ROLLBACK;
