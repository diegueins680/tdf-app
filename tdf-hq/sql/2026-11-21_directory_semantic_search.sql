-- DIR-SEMANTIC-SEARCH-001: embeddings for directory search.
--
-- One vector per public profile or classified search document, a cache of
-- query vectors keyed by a hash (the query text is not stored), and the
-- per-minute count of provider calls made by search requests. Nothing reads
-- these tables unless DIRECTORY_SEMANTIC_SEARCH_ENABLED is set.
--
-- An embedding is deleted in the transaction that hides its document or
-- changes its revision (title, subtitle, summary, search text or source
-- version). The application writes one only while the document is public,
-- effective and not expired (formal/system/directory-semantic-search.md).
--
-- Requires the pgvector extension already installed; this file does not
-- create it. Safe to apply more than once.
BEGIN;
SET LOCAL lock_timeout = '10s';

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') THEN
    RAISE EXCEPTION 'The vector extension is not installed; directory semantic search refused';
  END IF;
  IF to_regclass('public.directory_search_document') IS NULL THEN
    RAISE EXCEPTION 'directory_search_document is missing; directory semantic search refused';
  END IF;
END $$;

CREATE TABLE IF NOT EXISTS public.directory_search_embedding (
  entity_kind TEXT NOT NULL,
  entity_id TEXT NOT NULL,
  content_hash TEXT NOT NULL,
  provider TEXT NOT NULL,
  model TEXT NOT NULL,
  embedding vector(1536) NOT NULL,
  embedded_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (entity_kind, entity_id),
  FOREIGN KEY (entity_kind, entity_id)
    REFERENCES public.directory_search_document (entity_kind, entity_id) ON DELETE CASCADE,
  CONSTRAINT directory_search_embedding_kind_check CHECK (entity_kind IN ('profile','classified')),
  CONSTRAINT directory_search_embedding_hash_check CHECK (content_hash ~ '^[0-9a-f]{64}$')
);

CREATE TABLE IF NOT EXISTS public.directory_query_embedding (
  query_hash TEXT PRIMARY KEY,
  provider TEXT NOT NULL,
  model TEXT NOT NULL,
  embedding vector(1536) NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT directory_query_embedding_hash_check CHECK (query_hash ~ '^[0-9a-f]{64}$')
);
CREATE INDEX IF NOT EXISTS directory_query_embedding_created_idx
  ON public.directory_query_embedding (created_at DESC, query_hash);

CREATE TABLE IF NOT EXISTS public.directory_query_embedding_budget (
  window_started_at TIMESTAMPTZ PRIMARY KEY,
  calls INTEGER NOT NULL,
  CONSTRAINT directory_query_embedding_budget_calls_check CHECK (calls > 0)
);

CREATE OR REPLACE FUNCTION public.directory_search_embedding_guard()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  -- Not public any more (the predicate of directory_public_search_document
  -- for profiles and classifieds), or another revision of the embedded text.
  IF NOT (NEW.source_status = 'published' AND NEW.visibility = 'public'
          AND NEW.moderation_status = 'allowed'
          AND (NEW.effective_at IS NULL OR NEW.effective_at <= now())
          AND (NEW.expires_at IS NULL OR NEW.expires_at > now()))
     OR NEW.title IS DISTINCT FROM OLD.title
     OR NEW.subtitle IS DISTINCT FROM OLD.subtitle
     OR NEW.summary IS DISTINCT FROM OLD.summary
     OR NEW.search_text IS DISTINCT FROM OLD.search_text
     OR NEW.source_version IS DISTINCT FROM OLD.source_version THEN
    DELETE FROM public.directory_search_embedding
    WHERE entity_kind = NEW.entity_kind AND entity_id = NEW.entity_id;
  END IF;
  RETURN NULL;
END $$;
DROP TRIGGER IF EXISTS directory_search_embedding_guard_trigger ON public.directory_search_document;
CREATE TRIGGER directory_search_embedding_guard_trigger
  AFTER UPDATE ON public.directory_search_document
  FOR EACH ROW EXECUTE FUNCTION public.directory_search_embedding_guard();

-- An embedding left by an earlier application of this file must still belong
-- to a public document.
DELETE FROM public.directory_search_embedding stale
WHERE NOT EXISTS (
  SELECT 1 FROM public.directory_search_document document
  WHERE document.entity_kind = stale.entity_kind AND document.entity_id = stale.entity_id
    AND document.source_status = 'published' AND document.visibility = 'public'
    AND document.moderation_status = 'allowed'
    AND (document.effective_at IS NULL OR document.effective_at <= now())
    AND (document.expires_at IS NULL OR document.expires_at > now())
    AND encode(sha256(convert_to(concat_ws(chr(31), document.title, coalesce(document.subtitle, ''),
          coalesce(document.summary, ''), document.search_text, document.source_version::text), 'UTF8')), 'hex')
        = stale.content_hash);

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_trigger
    WHERE tgrelid = 'public.directory_search_document'::regclass
      AND tgname = 'directory_search_embedding_guard_trigger'
      AND NOT tgisinternal AND tgenabled <> 'D'
  ) THEN
    RAISE EXCEPTION 'directory_search_embedding_guard_trigger is missing or disabled';
  END IF;
  IF (SELECT format_type(atttypid, atttypmod) FROM pg_attribute
      WHERE attrelid = 'public.directory_search_embedding'::regclass AND attname = 'embedding')
       IS DISTINCT FROM 'vector(1536)'
     OR (SELECT format_type(atttypid, atttypmod) FROM pg_attribute
         WHERE attrelid = 'public.directory_query_embedding'::regclass AND attname = 'embedding')
       IS DISTINCT FROM 'vector(1536)' THEN
    RAISE EXCEPTION 'Directory embedding columns are not vector(1536)';
  END IF;
END $$;

COMMIT;
