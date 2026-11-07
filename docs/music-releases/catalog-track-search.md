# Authorized recording-title search

Baseline: protected main `5114fe95ac8e328b58162a8dcae5c9015a47ce09`.
Scope: a focused continuation of universal audio P1, originally prepared
separately from playback PR #620. Recording search is now combined with catalog
pagination on the playback candidate; see
[the discovery delivery increment](catalog-discovery-delivery-2026-10-25.md).
This change does not establish complete universal search or deployment.

## User-visible behavior and authorization

The existing music catalog (`/musica`) now accepts a recording title even when
the album/release title differs. Its search field says “Buscar por pista,
lanzamiento o artista”. Results remain release cards with stable release URLs;
opening a card retains the existing per-track global-player actions.

`TDF.Server.MusicRelease.listPublicReleases` still starts from the real
`music_public_release` view and retains the existing availability territory/time
filter, artist filter, limit/offset bounds and `published_at DESC, id` ordering.
The added recording-title branch uses `EXISTS`, so multiple matching renditions
and release contexts do not multiply release rows. A recording reused in two
authorized releases produces one card for each release, not an invented merged
release identity.

A title match must have a stream/preview asset for the same recording and
release version for which `music_public_asset_accessible(asset, territory)` is
currently true. Checking only release visibility is insufficient: a release
may be visible in Ecuador through one track while another is US-only. The
protected title must not make that release appear in an Ecuadorian search.
The query returns existing release metadata and introduces no track-title or
credential fields into the response.

Search reuses the existing `directory_normalize_text` function for Latin
accents, case, whitespace and partial matching. A literal case-insensitive
branch retains matching for titles with non-Latin characters that the directory
normalizer does not retain. Punctuation-only queries do not turn into an empty
normalized term that matches everything; percent/underscore are treated as text.
The dependency is the existing August 14 directory migration, already in the
current production migration manifest. No extension, index, migration, new
search service or background job is introduced.

## Reproduction and executable verification

Run the checked-in PostgreSQL regression against a loopback disposable test
server (or the CI postgres service):

```sh
TDF_MUSIC_WORKER_TEST_DATABASE_URL=postgresql://USER@127.0.0.1:PORT/tdf_music_search_test \
  node scripts/test-music-catalog-search.mjs
```

The URL only selects the server. The runner refuses non-disposable URL routing,
creates a new random database through the existing ownership/cleanup helper,
applies the real music schema and relevant publication migrations, and drops
only its own database. It extracts the actual SQL literal from the shipped
Haskell handler, the normalizer from its authoritative SQL migration, and the
existing submission-valid publication fixture. Fixture adaptations set distinct
recording titles, multiple renditions, a shared recording across releases and
two tracks with different territories. Publication uses normal transitions and
scheduler functions; no immutable or publication trigger is disabled.

On the unmodified baseline, the release-title positive control passed, followed
by the expected failure:

```text
AssertionError: Published recording title differing from release title must be searchable
actual: undefined; expected: the synthetic published release UUID
```

The corrected source passed four runtime groups:

1. Actual handler SQL finds recording titles while preserving release/artist
   matches, accents/case/partial matching, publication and territory isolation.
2. Several renditions and two release contexts sharing a recording produce
   unique release cards, stable pagination, artist filtering and safe browsing.
3. An EC-visible release does not match the title of its US-only recording.
   Replacing only `music_public_asset_accessible` with true makes this hidden
   title match; the explicit negative control detects that mutation.
4. Literal non-Latin titles match without turning an unnormalizable unrelated
   query into a match-all result.

The final runtime run on October 26 UTC / October 25 Ecuador passed on the
owned PostgreSQL 16 cluster at loopback port 55571. Its disposable database was
removed and the cluster stopped. Original search-run source identities
(historical; the combined paginated catalog page has subsequently changed):

| File | SHA-256 |
|---|---|
| `tdf-hq/src/TDF/Server/MusicRelease.hs` | `fb6257e60b9c506e856ef7827946fbeb166ba9b4f0d5227fa9c0fa91c0bb17fc` |
| `scripts/test-music-catalog-search.mjs` | `070bc9f4770b80966328b164bbbd29a7ecc623eb0f4f2c0612e9c24542abd6a2` |
| `tdf-hq-ui/src/pages/MusicCatalogPage.tsx` | `1820ce1be669dd74d8987b26b2b305271eeaf5c0ac52cf72e757f4e0eabe17b1` |

Initial sandbox restrictions prevented PostgreSQL shared-memory initialization
and loopback access; approved isolated execution produced the baseline and
candidate results above. These setup failures are not product failures.

CI now runs this regression in the existing backend job after the worker
runtime suite. A configured CI invocation is not evidence of a hosted pass.
The complete Stack build, HTTP route behavior, target-environment playback,
large-catalog execution plans, p95 search SLO, screen-reader behavior and
production deployment are not established by this local SQL test.

## Traceability and remaining scope

`AC-003 / MEDIA-002 → listPublicReleases and existing public/delivery predicates
→ test-music-catalog-search.mjs → dedicated backend CI step → pending release`.
Status is `TESTED` for the named finite PostgreSQL controls, not mathematical
proof and not a completed universal audio catalog. Baseline failure and final
runtime logs are retained locally as `/private/tmp/tdf-audio-search-baseline.log`
and `/private/tmp/tdf-audio-search-candidate.log`.

Frontend catalog pagination is now integrated in the combined P1 candidate.
Universal `/buscar` projection, typo tolerance, credits/genre/aliases search,
track-level result cards, historical ingestion and non-release audio contexts
remain separate work. This increment adds no query
ranking model; existing chronological ordering is preserved. Index changes
require representative `EXPLAIN` and latency evidence before being justified.

Rollback is the prior application code and search-field label; there is no data
or schema rollback. Publication/rights gates, original masters and existing
audio source authorization remain unchanged.
