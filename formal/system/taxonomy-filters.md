# Shared location and genre filters (TAX-FILTER-001) and artist genre suggestions (TAX-GENRE-SUGGESTION-001)

Status: specified and implemented on 2026-11-21; local PostgreSQL, Hspec and
React evidence only. Hosted CI, browser and production verification pending.

Authority: user request 2026-11-21 — "Users should be able to filter by city,
region, musical genre, etc. That applies to community search results, radio
stations, music in the player, etc.", refined the same day: curated taxonomy,
one shared filter set overridable per surface, OR within a dimension and AND
across, a parent includes its children, items without a value are hidden when
the dimension is filtered, "near me" from the profile city (device location
only after permission and only without a profile city), per-option counts,
shareable URLs, Spanish and English names, artist genre suggestions with
casing/spelling/duplicate checks, all four surfaces shipped together.

## Taxonomy

Countries (`country_reference`), regions (`subdivision_reference`), cities
(`city_reference`) and genres (`genre`, `parent_id` for subgenres) are the
existing persisted catalogs. `2026-11-21_location_genre_filters.sql` adds the
24 provinces of Ecuador, 32 Ecuadorian cities (capitals and main music
cities) and 16 bilingual subgenres. Rows are skipped when the code, or for
genres an equal normalized name in either language, already exists.
`GET /taxonomy/filter-options` lists every active option with both names.

## Filter semantics

Every surface accepts the repeatable parameters `countryIds`, `regionIds`,
`cityIds` and `genreIds` (at most 50 values each, deduplicated). One set of
SQL functions defines the rules for all of them:

| Function | Rule |
|---|---|
| `taxonomy_genre_closure(uuid[])` | selected genres plus every active descendant |
| `taxonomy_genres_match(item, closure)` | empty selection matches all; otherwise the item must carry a genre in the closure |
| `taxonomy_location_matches(city, country, countries, regions, cities)` | each non-empty dimension must match; region and (when absent) country derive from the item's city; a missing value never matches |
| `taxonomy_genre_ancestors(uuid[])` | genres plus ancestors, used so a subgenre item counts for its parent |
| `taxonomy_city_from_text(text, country)` | free-text city resolved only when exactly one active reference matches |

| Surface | Location of an item | Genres of an item |
|---|---|---|
| Community/global search (`/directory/search`) | document city; country from its country code | document genre ids |
| Radio (`/radio/streams`) | station country; a selected region or city narrows to its country, intersected with selected countries (empty intersection returns nothing) | station genre |
| Music (`/music/releases`, `/music/search`) | the artist profile's city text (resolved) and country | release primary/secondary genre, else the artist's genres |
| Events (`/social-events/events`) | venue city id, else its city text resolved; venue country | canonical genres of the lineup |

Directory saved-search alerts keep the exact single `cityId`/`genreId`
semantics of DIR-SAVED-SEARCH-001; the shared filters are separate parameters
and do not change alert parity.

Facet counts (`facets.taxonomy` on directory search; `/radio/streams/facets`,
`/music/facets/releases`, `/social-events/events/facets`) apply every other
active filter but not the counted dimension's own selection. Event counts
cover the first 500 events the list would return.

## Shared selection and overrides (web)

One selection is kept per browser (`localStorage`). A URL-backed surface uses
the URL's `f.country`/`f.region`/`f.city`/`f.genre` values when present,
otherwise the shared set (mirrored into the URL so a copied link reproduces
the view). A link whose values differ from the shared set, or that carries
`f.scope=local`, is a per-surface override and is announced with "use these
everywhere" and "use the general filters". Changing filters updates the
shared set unless "only in this section" is on. The radio overlay keeps its
override in memory, not in the URL of whatever page it floats over. Old
`genreId`/`cityId` community links are converted to the shared keys.
"Near me" pre-selects the profile city once per browser and never overrides
an existing choice; device location is requested only on an explicit tap.

## Genre suggestions

`TDF.Taxonomy.GenreSuggestion` cleans each name (2–60 characters; letters,
digits, spaces and `- & ' . / +`), cases it (Spanish sentence case, English
title case with lowercase joining words, short acronyms kept, letter
prefixes such as "K-pop" kept, each language's established hyphen casing),
and compares an accent-, case-, space- and punctuation-insensitive key with
every existing genre in both languages:

* equal key → `duplicate` (refused with the existing genre);
* edit distance within the threshold → `similar` (refused unless the artist
  confirms it is different, which sends it to review);
* every word known from existing genre names → `approve` (published at once,
  added to the artist's profile);
* otherwise → `review` (pending, shown on the artist's profile as pending).

`genre_suggestion` stores suggestions. `pending` is the only open state; the
`genre_suggestion_guard_transition` trigger allows only pending → approved,
rejected or merged and keeps authorship immutable. A partial unique index
allows one pending suggestion per normalized key (a second artist sees the
existing one). Artists are limited to 10 pending and 20 suggestions a day.
Strict administrators approve (names re-checked for duplicates), reject (a
note of 1–500 characters is required) or merge into an existing genre from
`/admin/generos`.

## Rollback

`2026-11-21_location_genre_filters_rollback.sql` drops the suggestion table,
its trigger, the review list and the functions. Seeded references and
genres created from suggestions stay (catalog integrity forbids deleting
referenced rows); the previous code ignores them for filtering.

## Evidence

* `scripts/test-community-search.py` — directory SQL on migrated PostgreSQL:
  closure, OR/AND, region from city, missing values hidden, facet counts.
* `tdf-hq/test/TDF/Taxonomy/FilterSpec.hs`, `GenreSuggestionSpec.hs`.
* `tdf-hq-ui/src/components/taxonomyFilters/taxonomyFilterModel.test.ts`,
  `CommunityDiscovery.test.tsx`, radio widget tests.
* `e2e/web/taxonomy-filters.spec.mjs` (Spanish and English); screenshots at
  desktop and 360 px phone width in
  `artifacts/discovery-playwright/taxonomy-filters/`.
* Mobile: `tdf-mobile/__tests__/taxonomyFilterModel.test.ts`,
  `DirectoryTaxonomyFilters.test.tsx`, `taxonomyFilterStore.test.tsx`,
  `GenreSuggestionModal.test.tsx`, `DeepLinks.test.ts`.

## Open questions

* Which further regions and cities outside Ecuador should be curated.
* Marketplace listings and studio storefront packages carry no location or
  genre; they are reached through directory classifieds and services only.
