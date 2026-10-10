# Event and venue search sync

`/buscar` reads `directory_public_search_document`. Until 2026-10-09, event and venue documents were written only by the one-off August backfill (`directory_refresh_legacy_event_search`), so events created later never appeared.

Migration `2026-10-09_directory_event_search_sync` makes the documents follow their sources:

- **Triggers.** Inserting, updating or deleting a `social_event`, updating or deleting a `venue`, and changing an `external_event_ref` (provider suppression) upsert the affected event and venue documents. Deleting the source removes its document. An event that becomes private or suppressed keeps its cached document, and the public search view filters it out at read time, as required by the event privacy composition contract.
- **Same projection.** The documents come from the privacy-reviewed `directory_public_event` and `directory_public_venue` views. The manual full refresh reuses the same functions, so a rebuild can never disagree with the incremental path. Search still re-checks those views when reading, so a stale row cannot expose a private event.
- **Upcoming events.** For events, `effective_at` holds the start time, which date filters use. The public search view previously hid documents whose `effective_at` was in the future, so upcoming events were invisible. Events are now exempt from that check, and their visibility stays governed by `directory_public_event`.
- **City.** A venue whose city is only free text gets the `city_reference` id when exactly one city matches by normalized name. This applies on insert and update, plus a one-time backfill, so city filters such as "Quito" match. When a venue's city text changes and its id agreed with the previous text, the id follows the new text (the venue API writes only the text), so the venue and its events move to the new city's results. An id that was chosen independently of the text is kept.
- **Venues of late-published events.** Every event insert or update also syncs its venue, because publishing an event (a workflow-state change) is what makes its venue listable.
- **Stable versions.** An upsert that changes nothing keeps `source_version`. Saved-search alerts fire on that column, so a rebuild or a re-applied migration does not notify again.
- **Concurrent edits.** Both sync functions take one transaction-scoped advisory lock before reading their sources, so a venue edit and an edit to one of its events cannot overwrite each other's projection with an older snapshot. They lock no source rows, so the two paths cannot deadlock. Deleting an event or venue takes the same lock before removing its document, so an overlapping sync cannot reinsert it.
- **Kind words.** Event documents include "evento eventos" and venue documents include "venue venues local locales". A search for "Eventos" therefore returns events.
- **Images.** Only HTTPS `imageUrl` values from valid event metadata become the result image.

The rollback removes the triggers and helpers and restores the previous view and refresh. Derived documents and resolved city ids are kept.

Verification: `tdf-hq/test/integration/directory_event_search_sync.sql`, which runs from `scripts/test-music-directory-migration.sh` (apply twice, verify, roll back, reapply, verify).
