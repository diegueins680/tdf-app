-- Rollback of 2026-11-21_directory_semantic_search.sql
-- (DIR-SEMANTIC-SEARCH-001). Set DIRECTORY_SEMANTIC_SEARCH_ENABLED=false
-- and restart the API first. With the flag on and the tables gone search
-- keeps today's ranking and the sweep does nothing. Embeddings and cached
-- query vectors are derived data and are dropped.
BEGIN;
SET LOCAL lock_timeout = '10s';

DO $$
BEGIN
  IF to_regclass('public.directory_search_document') IS NOT NULL THEN
    DROP TRIGGER IF EXISTS directory_search_embedding_guard_trigger ON public.directory_search_document;
  END IF;
END $$;
DROP FUNCTION IF EXISTS public.directory_search_embedding_guard();
DROP TABLE IF EXISTS public.directory_search_embedding;
DROP TABLE IF EXISTS public.directory_query_embedding;
DROP TABLE IF EXISTS public.directory_query_embedding_budget;

COMMIT;
