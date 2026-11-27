# Venue search in SQL (EVT-VENUE-SEARCH-001)

`GET /social-events/venues?city=&near=&q=&limit=&offset=` lists venues. When `q` or `near`
was present, `listVenues` read the first 2,000 venues by name and filtered them in Haskell,
so venues after the 2,000th were never found. Behind the runtime flag
`social_venues.sql_search` the same filter now runs in PostgreSQL without a cap. The route,
its parameters, its response and the OpenAPI contract are unchanged.

The owner approved this rule and its constants on 2026-11-21: the exact fold function, the
1 mm tolerance, ordering by name then id, and sizing for fewer than 2,000 venues today and
fewer than 10,000 in two years.

## Inputs (unchanged)

| Input | Rule |
|---|---|
| `city` | trimmed; at most 120 characters; no control or format characters; exact, case-sensitive equality with `venue.city` |
| `q` | same validation, then `Data.Text.toCaseFold`; empty means no text filter |
| `near` | `lat,lng` or `lat,lng,radiusKm`; finite; latitude −90..90; longitude −180..180; radius in (0, 1000] km, default 25 |
| `limit` | 1..500, default 200 |
| `offset` | 0..10,000, default 0 |

Invalid input is refused with the same `400` bodies as before, before the search runs.

## Specification

`venueSearchSpec` in `tdf-hq/src/TDF/Venues/Search.hs` is the specification: the filter the
handler always applied, moved unchanged into a pure, total function. Over rows ordered by
name and then id it keeps a row when

- **city:** `venue.city` equals the requested city;
- **text:** the folded `q` is a substring of `toCaseFold name`, `toCaseFold city` or
  `toCaseFold address` (an absent city or address never matches);
- **distance:** both coordinates are present and
  `haversineDistanceKm lat lng venueLat venueLng <= radiusKm` (Earth radius 6371 km);

then drops `offset` rows and takes `limit`. With the flag off the handler still calls this
function on the first 2,000 rows, so the specification is the code that runs.

**Rule.** With the flag on, the answer is the specification applied to every venue, except
as stated under Tolerance. On a table of at most 2,000 venues that is
the former answer, with one refinement: venues with the same name are now ordered by id.

## SQL

`loadVenueSearch` sends one read-only statement built from fixed fragments
(`venueSearchSelectSql`, `…CityClauseSql`, `…TextClauseSql`, `…NearClauseSql`, `…PageSql`).

- **Text.** `venue_search_fold(column) LIKE '%needle%' ESCAPE '!'` on name, city and address,
  with `!`, `%` and `_` escaped in the needle.
- **Fold.** PostgreSQL 17 has no case-fold function and `lower()` is not `toCaseFold`
  (`q=STRASSE` matches "Straße"). `venue_search_fold(text)` is generated from
  `Data.Text.toCaseFold` by `tdf-hq/src/TDF/Venues/SearchFold.hs`; a test requires the
  migration to carry that text unchanged. `toCaseFold` maps each character on its own, so
  agreement on every scalar value is agreement on every string. A `text` upgrade that changes
  the fold fails the test and needs a new migration that rebuilds the expression indexes.
- **Distance.** The bounding box, then the haversine formula with the operations of the
  Haskell function in the same order. Every stored coordinate is in range (see Stored
  coordinates), so the box applies to every row.
- **Order and page.** `ORDER BY venue.name, venue.id LIMIT ? OFFSET ?`.
- No PostGIS and no new extension. `pg_trgm` is used when installed.

### Bounding box

`venueBoundingBox`, with `d = (radiusKm + 0.000001) / 6371` radians, so that the box also
holds every venue inside the tolerance band:

- latitude within `d` of the query;
- longitude within `asin(sin d / cos lat)` of the query, tested on the venue longitude and
  on that longitude plus and minus 360, which is what covers the 180° meridian;
- when `sin d / cos lat` reaches 0.9999, which includes every circle that touches a pole,
  all longitudes are kept (below that the arcsine is well conditioned);
- each half-width is widened by a relative `1e-9` plus `1e-9` degrees.

**Soundness (critical):** the box never excludes a venue the haversine test accepts. The
argument needs the venue's latitude in −90..90, which the coordinate constraint guarantees.

### Tolerance

PostgreSQL and the Haskell runtime call their own C mathematics libraries, which may differ
in the last bit. The SQL answer and the specification are required to agree on every venue
whose specified distance differs from the radius by more than **0.000001 km (1 mm)**
(`venueSearchToleranceKm`). Inside that band either answer is allowed.

PostgreSQL raises on floating-point underflow where Haskell returns zero or a subnormal. The
SQL therefore takes a coordinate difference below `1e-290` degrees as zero (the value Haskell
computes) and drops a squared sine below `1e-240` (a change far inside the tolerance).

### Stored coordinates

The constraint `venue_coordinates_in_range` (migration
`2026-11-24_venue_coordinate_constraint.sql`) keeps each coordinate NULL or within
−90..90 / −180..180. `NaN` and both infinities fail it. Every writer already refused such
values (the venue create and update handler, event research materialisation and event
discovery), and on 2026-11-23 production held 45 venues, none out of range. The migration
refuses, changing nothing, if any stored row is out of range.

- No coordinates, or only one: never matches.
- `venueSqlSearchEnabled` requires the validated constraint on the venue table the
  statement reads, so a database without it keeps the in-memory path.

The first version of the SQL had a separate branch and a partial index for out-of-range
rows, with two stated deviations (values above `1e300` never matched; no argued tolerance for
out-of-range rows). The constraint removes those rows, the branch, the index and both
deviations.

## Failure

Arithmetic the specification evaluates to `NaN` is a non-match in SQL, guarded by `CASE`, not
an error. A database error is a `500`, as before. The statement never writes.

## Privacy, authorization, concurrency

- Venues carry no per-user data and the list is not personalised: every caller receives the
  same answer for the same query, so there is no opt-out state and no fallback to define.
  Nothing offers a party to follow or contact, so `social_v2_profile_eligible` does not apply.
- Venues have no hidden, private or unlisted state. The route's authorization is unchanged.
- One statement, one snapshot at the pool's default isolation.

## Rollout

- Flag `social_venues.sql_search` in `revenue_feature_flag`, **disabled** for sandbox and
  production by the migration. `venueSqlSearchEnabled` also requires the flag table and the
  fold function to exist, so a database without the migration keeps the in-memory path.
- Off: the former path, including the 2,000 cap and ordering by name only. On: the SQL path
  for every venue list, filtered or not. Disabling the flag restores the former path without
  a deploy. Reading the flag adds two small queries to each venue list request.
- Migration `tdf-hq/sql/2026-11-21_venue_search_sql.sql` (rollback beside it): the fold
  function; `venue (latitude, longitude)`; a partial index on out-of-range rows; three
  trigram expression indexes when `pg_trgm` is installed. Migration
  `tdf-hq/sql/2026-11-24_venue_coordinate_constraint.sql` (rollback beside it) adds the
  coordinate constraint and drops the partial index.
- In production since release 10 (2026-11-24): flag enabled, `pg_trgm` installed. At the expected size (under 10,000
  venues) a scan is already fast; the indexes are sized for that and no more. A needle
  shorter than three characters cannot use a trigram index.
- Web only. No OpenAPI change, no regenerated clients, no mobile pin.

## Properties and evidence

| Property | Evidence | Level |
|---|---|---|
| SQL answer equals the specification outside the tolerance (membership, order, page) | QuickCheck against PostgreSQL through `loadVenueSearch`, 1,500 generated tables of up to 30 venues weighted to the radius edge, poles, meridian and extreme values; exact comparison on 2,000 and 2,500 venues | tested |
| Bounding box is sound in real arithmetic, poles and meridian included | Z3, `formal/social/venue_search_bounding_box.py`: four lemmas, two witnesses, three negative controls | proved for the algebraic model |
| Bounding box is sound in floating point | QuickCheck, 20,000 cases on the real function, and boundary examples | tested |
| `venue_search_fold` equals `toCaseFold` | every Unicode scalar value (1,112,063) on PostgreSQL, through each of the three branches | tested exhaustively |
| Filtering every venue refines the capped path; the box needs the wrap and the pole guard | TLC, `formal/event-operations/VenueSearch.tla`, with three negative controls | bounded model |
| Flag off is the former path; flag on lifts the cap; missing function or constraint keeps the former path | handler and flag tests on PostgreSQL | tested |
| Out-of-range and non-finite coordinates cannot be stored | PostgreSQL test of eight refused values and the in-range edges; migration refuses a table holding one | tested |

The Z3 result is about the mathematics of the box, with `sin` and `cos` replaced by algebraic
values. It does not cover IEEE rounding, the compiled SQL or PostgreSQL's planner; the
widening margin and the tests stand between the proof and the code. The equivalence of the
SQL with the specification is tested, not proved.

## Open for the owner

- Nothing is open. Merged with a merge commit, so `introducedBy` in
  `scripts/production-migrations.json` stays in the release history.
