-- Removes the alias resync. Event documents are left as they are; run
-- SELECT directory_refresh_legacy_event_search(); after later alias edits.
-- Safe to apply again.
BEGIN;
DROP TRIGGER IF EXISTS trg_directory_sync_alias_events_search ON catalog_search_alias;
DROP FUNCTION IF EXISTS directory_sync_alias_events_search_trigger();
COMMIT;
