BEGIN;

-- Rollback of 2026-11-21_location_genre_filters (TAX-FILTER-001,
-- TAX-GENRE-SUGGESTION-001).
-- Genres created from approved suggestions are catalog rows and stay (catalog
-- integrity forbids hard deletes); only the suggestion records are removed.
-- Venue city/country ids written by the backfill stay: they are valid
-- references and the previous code ignores them for filtering.
-- The seeded provinces and cities stay for the same reason; deleting reference
-- rows that profiles may already point to is never safe.

DROP TRIGGER IF EXISTS genre_suggestion_transition_trigger ON genre_suggestion;
DROP FUNCTION IF EXISTS genre_suggestion_guard_transition();
DROP TABLE IF EXISTS genre_suggestion;
DROP TABLE IF EXISTS taxonomy_backfill_review;
DROP INDEX IF EXISTS ix_venue_taxonomy_location;
DROP FUNCTION IF EXISTS taxonomy_city_from_text(TEXT, UUID);
DROP FUNCTION IF EXISTS taxonomy_location_matches(UUID, UUID, UUID[], UUID[], UUID[]);
DROP FUNCTION IF EXISTS taxonomy_genres_match(UUID[], UUID[]);
DROP FUNCTION IF EXISTS taxonomy_genre_ancestors(UUID[]);
DROP FUNCTION IF EXISTS taxonomy_genre_closure(UUID[]);

COMMIT;
